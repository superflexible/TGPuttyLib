#ifndef __TGMEM__H__
#define __TGMEM__H__

/*
#define CALLBACK_MALLOC
#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC
#endif
*/

#if defined(CALLBACK_MALLOC) || defined(DEBUG_MALLOC)
// make sure these are included before redefining malloc and free
#include <stdio.h>                     /* for FILE * */
#include <stdarg.h>                    /* for va_list */
#include <stdlib.h>                    /* for abort */
#include <time.h>                      /* for struct tm */
#include <limits.h>                    /* for INT_MAX/MIN */
#include <assert.h>                    /* for assert (obviously) */
#endif

#ifdef DEBUG_MALLOC
#define malloc(x) tgdlldebugmalloc(x,__FILE__,__LINE__)
#define free(x) tgdlldebugfree(x,__FILE__,__LINE__)
#define realloc(x,y) tgdlldebugrealloc(x,y,__FILE__,__LINE__)
#define calloc DONOTUSEERROR

void *tgdlldebugmalloc(size_t size,const char *filename,const int line);
void tgdlldebugfree(void *ptr,const char *filename,const int line);
void *tgdlldebugrealloc(void *ptr, size_t new_size,const char *filename,const int line);

#else

#ifdef CALLBACK_MALLOC
#define malloc tgdllmalloc
#define free tgdllfree
#define realloc tgdllrealloc
#define calloc DONOTUSEERROR

void *tgdllmalloc(size_t size);
void tgdllfree(void *ptr);
void *tgdllrealloc(void *ptr, size_t new_size);
#endif

#endif


#endif
