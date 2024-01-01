/*

TG

On some architectures, we need to specify which memcpy version to use (from which glibc)
because we need to ensure binary compatibility with various Linux distributions 
(including some NAS operating systems).

To list available versions (example for an i386 Ubuntu machine):
objdump -TC /lib/i386-linux-gnu/libc.so.6 | grep memcpy

To see gcc compiler predefines:
echo | gcc -dM -E -

*/
#ifndef tglibcver___H
#define tglibcver___H
#if __GNUC__ > 5
#ifdef i386
__asm__(".symver memcpy,memcpy@GLIBC_2.0");
#endif

#ifdef __amd64
__asm__(".symver memcpy,memcpy@GLIBC_2.2.5");
#endif

#else

// older GNU compiler (__GNUC__ <= 5)
#define AVOID_CLOCK_GETTIME

#endif
#endif
