/*
 * PuTTY's memory allocation wrappers.
 */

#include <assert.h>
#include <stdlib.h>
#include <limits.h>

#include "defs.h"
#include "puttymem.h"
#include "misc.h"


#ifdef DEBUG_MALLOC // TG
#undef malloc
void *safemalloc(size_t factor1, size_t factor2, size_t addend,const char *filename,const int line)
#else
void *safemalloc(size_t factor1, size_t factor2, size_t addend)
#endif
{
    if (factor1 > SIZE_MAX / factor2)
        goto fail;
    size_t product = factor1 * factor2;

    if (addend > SIZE_MAX)
        goto fail;
    if (product > SIZE_MAX - addend)
        goto fail;
    size_t size = product + addend;

    if (size == 0)
        size = 1;

    void *p;
#ifdef MINEFIELD
    p = minefield_c_malloc(size);
#else
#ifdef DEBUG_MALLOC // TG
    p = tgdlldebugmalloc(size,filename,line);
#else
    p = malloc(size);
#endif
#endif

    if (!p)
        goto fail;

    return p;

  fail:
    out_of_memory();
    return NULL; // TG
}

#ifdef DEBUG_MALLOC // TG
#undef realloc
void *saferealloc(void *ptr, size_t n, size_t size,const char *filename,const int line)
#else
void *saferealloc(void *ptr, size_t n, size_t size)
#endif
{
    void *p;

    if (n > INT_MAX / size) {
        p = NULL;
    } else {
        size *= n;
        if (!ptr) {
#ifdef MINEFIELD
            p = minefield_c_malloc(size);
#else
#ifdef DEBUG_MALLOC // TG
            p = tgdlldebugmalloc(size,filename,line);
#else
            p = malloc(size);
#endif
#endif
        } else {
#ifdef MINEFIELD
            p = minefield_c_realloc(ptr, size);
#else
#ifdef DEBUG_MALLOC // TG
            p = tgdlldebugrealloc(ptr,size,filename,line);
#else
            p = realloc(ptr, size);
#endif
#endif
        }
    }

    if (!p)
        out_of_memory();

    return p;
}

#ifdef DEBUG_MALLOC // TG
#undef free
void safefree(void *ptr,const char *filename,const int line)
#else
void safefree(void *ptr)
#endif
{
    if (ptr) {
#ifdef MINEFIELD
        minefield_c_free(ptr);
#else
#ifdef DEBUG_MALLOC // TG
        tgdlldebugfree(ptr,filename,line);
#else
        free(ptr);
#endif
#endif
    }
}

#ifdef DEBUG_MALLOC // TG
void *safegrowarray(void *ptr, size_t *allocated, size_t eltsize,
                    size_t oldlen, size_t extralen, bool secret,
                    const char *filename,const int line)
#else
void *safegrowarray(void *ptr, size_t *allocated, size_t eltsize,
                    size_t oldlen, size_t extralen, bool secret)
#endif
{
    /* The largest value we can safely multiply by eltsize */
    assert(eltsize > 0);
    size_t maxsize = (~(size_t)0) / eltsize;

    size_t oldsize = *allocated;

    /* Range-check the input values */
    assert(oldsize <= maxsize);
    assert(oldlen <= maxsize);
    assert(extralen <= maxsize - oldlen);

    /* If the size is already enough, don't bother doing anything! */
    if (oldsize > oldlen + extralen)
        return ptr;

    /* Find out how much we need to grow the array by. */
    size_t increment = (oldlen + extralen) - oldsize;

    /* Invent a new size. We want to grow the array by at least
     * 'increment' elements; by at least a fixed number of bytes (to
     * get things started when sizes are small); and by some constant
     * factor of its old size (to avoid repeated calls to this
     * function taking quadratic time overall). */
    if (increment < 256 / eltsize)
        increment = 256 / eltsize;
    if (increment < oldsize / 16)
        increment = oldsize / 16;

    /* But we also can't grow beyond maxsize. */
    size_t maxincr = maxsize - oldsize;
    if (increment > maxincr)
        increment = maxincr;

    size_t newsize = oldsize + increment;
    void *toret;
    if (secret) {
        toret = safemalloc(newsize, eltsize, 0
#ifdef DEBUG_MALLOC // TG
        , filename, line
#endif
        );
        memcpy(toret, ptr, oldsize * eltsize);
        smemclr(ptr, oldsize * eltsize);
        sfree(ptr);
    } else {
        toret = saferealloc(ptr, newsize, eltsize
#ifdef DEBUG_MALLOC // TG
          , filename, line
#endif
          );
    }
    *allocated = newsize;
    return toret;
}
