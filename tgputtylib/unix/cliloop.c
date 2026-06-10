#include <errno.h>

#include "putty.h"

void cli_main_loop(cliloop_pw_setup_t pw_setup,
                   cliloop_pw_check_t pw_check,
                   cliloop_continue_t cont, void *ctx)
{
    unsigned long now = GETTICKCOUNT();

    int *fdlist = NULL;
    size_t fdsize = 0;

    pollwrapper *pw = pollwrap_new();

    while (true) {
        int rwx;
        int ret;
        int fdstate;
        unsigned long next;

        pollwrap_clear(pw);

        if (!pw_setup(ctx, pw))
            break; /* our client signalled emergency exit */

        /* Count the currently active fds. */
        size_t nfds = 0;
        for (int fd = first_fd(&fdstate, &rwx); fd >= 0;
             fd = next_fd(&fdstate, &rwx))
            nfds++;

        /* Expand the fdlist buffer if necessary. */
        sgrowarray(fdlist, fdsize, nfds);

        /*
         * Add all currently open uxsel fds to pw, and store them in
         * fdlist as well.
         */
        size_t fdcount = 0;
        for (int fd = first_fd(&fdstate, &rwx); fd >= 0;
             fd = next_fd(&fdstate, &rwx)) {
            fdlist[fdcount++] = fd;
            pollwrap_add_fd_rwx(pw, fd, rwx);
        }

        if (toplevel_callback_pending()) {
            ret = pollwrap_poll_instant(pw);
        } else if (run_timers(now, &next)) {
            do {
                unsigned long then;
                long ticks;

                then = now;
                now = GETTICKCOUNT();
                if (now - then > next - then)
                    ticks = 0;
                else
                    ticks = next - now;

                bool overflow = false;
                /* TG: never sleep for more than one second, mirroring the
                 * 2019 fix in windows/cliloop.c. During a connection
                 * attempt to an unreachable host, the only pending timer
                 * can be tens of seconds away (e.g. the noise-collection
                 * timer), and a full-length poll here starves the callers'
                 * abort/timeout checks (psftp_connect, sftp_recvdata):
                 * connectiontimeoutticks=10s could take 30-75s to trigger.
                 * Capping the poll also makes curlibctx->aborted reactive
                 * within a second. (Counts as 'overflow' so the timer
                 * bookkeeping below doesn't assume we reached 'next'.) */
                if (ticks > 1000) {
                    ticks = 1000;
                    overflow = true;
                }

                ret = pollwrap_poll_timeout(pw, ticks);
                if (ret == 0 && !overflow)
                    now = next;
                else
                    now = GETTICKCOUNT();
            } while (ret < 0 && errno == EINTR);
        } else {
            /* TG: was pollwrap_poll_endless - with no timers pending that
             * could block forever, never returning control to the loops
             * that check for aborts and timeouts. Wake at least once per
             * second instead. */
            ret = pollwrap_poll_timeout(pw, 1000);
            now = GETTICKCOUNT();
        }

        if (ret < 0 && errno == EINTR)
            continue;

        if (ret < 0) {
            perror("poll");
            exit(1);
        }

        bool found_fd = (ret > 0);

        for (size_t i = 0; i < fdcount; i++) {
            int fd = fdlist[i];
            int rwx = pollwrap_get_fd_rwx(pw, fd);
            /*
             * We must process exceptional notifications before
             * ordinary readability ones, or we may go straight
             * past the urgent marker.
             */
            if (rwx & SELECT_X)
                select_result(fd, SELECT_X);
            if (rwx & SELECT_R)
                select_result(fd, SELECT_R);
            if (rwx & SELECT_W)
                select_result(fd, SELECT_W);
        }

        pw_check(ctx, pw);

        bool ran_callback = run_toplevel_callbacks();

        if (!cont(ctx, found_fd, ran_callback))
            break;
    }

    pollwrap_free(pw);
    sfree(fdlist);
}

bool cliloop_no_pw_setup(void *ctx, pollwrapper *pw) { return true; }
void cliloop_no_pw_check(void *ctx, pollwrapper *pw) {}
bool cliloop_always_continue(void *ctx, bool fd, bool cb) { return true; }

/*
 * Any application using this main loop doesn't need to do anything
 * when uxsel adds or removes an fd, because we synchronously re-check
 * the current list every time we go round the main loop above.
 */
uxsel_id *uxsel_input_add(int fd, int rwx) { return NULL; }
void uxsel_input_remove(uxsel_id *id) { }
