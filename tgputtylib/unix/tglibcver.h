#if __GNUC__ > 5
#ifndef __PPC__
__asm__(".symver memcpy,memcpy@GLIBC_2.2.5");
#endif
#else
#define AVOID_CLOCK_GETTIME
#endif
