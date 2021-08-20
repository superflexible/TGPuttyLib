#include "putty.h"

#ifdef TGDLL
#define winselcli_event (curlibctx->winselcli_event)
#endif


void cli_main_loop(cliloop_pre_t pre, cliloop_post_t post, void *ctx)
{
    SOCKET *sklist = NULL;
    size_t skcount = 0, sksize = 0;
    // TG: removed unsigned long now, next, then;
    // TG: removed now = GETTICKCOUNT();

    while (true) {
        int nhandles;
        HANDLE *handles;
        DWORD n;
        DWORD ticks;

        const HANDLE *extra_handles = NULL;
        size_t n_extra_handles = 0;
        if (!pre(ctx, &extra_handles, &n_extra_handles))
            break;

        if (toplevel_callback_pending()) {
            ticks = 0;
            // TG removed: next = now;
        } 
		else 
		{
         unsigned long next, then; // TG
         unsigned long now = GETTICKCOUNT(); // TG
		 if (run_timers(now, &next)) 
		 {
            then = now;
            now = GETTICKCOUNT();
            if (now>next) // TG
                ticks = 0;
            else
            {
              ticks = next - now; // TG
              if (ticks>1000)
                 ticks = 1000; // TG 2019: never hang for more than one second
            }
         }
         else // TG
         {
            // TG 2019: never hang for more than a second, need to be able to cancel job etc.
            // we also observed rare infinite hangs here after an Internet disconnection
            ticks = 1000;
         }
        }

        handles = handle_get_events(&nhandles);
        size_t winselcli_index = -(size_t)1;
        size_t extra_base = nhandles;
        if (winselcli_event != INVALID_HANDLE_VALUE) {
            winselcli_index = extra_base++;
            handles = sresize(handles, extra_base, HANDLE);
            handles[winselcli_index] = winselcli_event;
        }
        size_t total_handles = extra_base + n_extra_handles;
        handles = sresize(handles, total_handles, HANDLE);
        for (size_t i = 0; i < n_extra_handles; i++)
            handles[extra_base + i] = extra_handles[i];
        // printf("Calling WaitForMultipleObjects with ticks: %ld\n",ticks); // TG - for debugging
        n = WaitForMultipleObjects(total_handles, handles, false, ticks);

        size_t extra_handle_index = n_extra_handles;

        if ((unsigned)(n - WAIT_OBJECT_0) < (unsigned)nhandles) {
            handle_got_event(handles[n - WAIT_OBJECT_0]);
        } else if (winselcli_event != INVALID_HANDLE_VALUE &&
                   n == WAIT_OBJECT_0 + winselcli_index) {
            WSANETWORKEVENTS things;
            SOCKET socket;
            int i, socketstate;

            /*
             * We must not call select_result() for any socket
             * until we have finished enumerating within the tree.
             * This is because select_result() may close the socket
             * and modify the tree.
             */
            /* Count the active sockets. */
            i = 0;
            for (socket = first_socket(&socketstate);
                 socket != INVALID_SOCKET;
                 socket = next_socket(&socketstate)) i++;

            /* Expand the buffer if necessary. */
            sgrowarray(sklist, sksize, i);

            /* Retrieve the sockets into sklist. */
            skcount = 0;
            for (socket = first_socket(&socketstate);
                 socket != INVALID_SOCKET;
                 socket = next_socket(&socketstate)) {
                sklist[skcount++] = socket;
            }

            /* Now we're done enumerating; go through the list. */
            for (i = 0; i < skcount; i++) {
                WPARAM wp;
                socket = sklist[i];
                wp = (WPARAM) socket;
                if (!p_WSAEnumNetworkEvents(socket, NULL, &things)) {
                    static const struct { int bit, mask; } eventtypes[] = {
                        {FD_CONNECT_BIT, FD_CONNECT},
                        {FD_READ_BIT, FD_READ},
                        {FD_CLOSE_BIT, FD_CLOSE},
                        {FD_OOB_BIT, FD_OOB},
                        {FD_WRITE_BIT, FD_WRITE},
                        {FD_ACCEPT_BIT, FD_ACCEPT},
                    };
                    int e;

                    noise_ultralight(NOISE_SOURCE_IOID, (unsigned long) socket); // TG

                    for (e = 0; e < lenof(eventtypes); e++)
                        if (things.lNetworkEvents & eventtypes[e].mask) {
                            LPARAM lp;
                            int err = things.iErrorCode[eventtypes[e].bit];
                            lp = WSAMAKESELECTREPLY(eventtypes[e].mask, err);
                            select_result(wp, lp);
                        }
                }
            }
        } else if (n >= WAIT_OBJECT_0 + extra_base &&
                   n < WAIT_OBJECT_0 + extra_base + n_extra_handles) {
            extra_handle_index = n - (WAIT_OBJECT_0 + extra_base);
        }

        run_toplevel_callbacks();

/* TG
        if (n == WAIT_TIMEOUT) {
            now = next;
        } else {
            now = GETTICKCOUNT();
        }
*/

        sfree(handles);

        if (!post(ctx, extra_handle_index))
            break;
    }

    sfree(sklist);
}

bool cliloop_null_pre(void *vctx, const HANDLE **eh, size_t *neh)
{ return true; }
bool cliloop_null_post(void *vctx, size_t ehi) { return true; }
