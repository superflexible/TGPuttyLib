/*
 * Noise generation for PuTTY's cryptographic random number
 * generator.
 */

/* TG: for RTLD_DEFAULT in <dlfcn.h>; -std=gnu99 does not define it. */
#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "putty.h"
#include "ssh.h"
#include "storage.h"

/*
 * TG: getentropy(3) reaches the kernel CSPRNG without a file descriptor and
 * without a child process. It is glibc 2.25+ and macOS 10.12+ only, so it is
 * a bonus, never a requirement - read_dev_urandom() below is the floor, and
 * /dev/urandom has been there since Mac OS X 10.0 and forever on Linux.
 */
#if defined(__APPLE__)
#include <Availability.h>
/*
 * We still support macOS 10.10, and getentropy() is annotated
 * __API_AVAILABLE(macos(10.12)). Referencing it below that deployment target
 * either fails to compile (an SDK that old has no <sys/random.h>) or, on a
 * modern SDK, makes clang weak-link it: the dylib then loads fine on Yosemite
 * but the symbol resolves to NULL and the first call jumps to address 0.
 * So key this off the deployment target, not off __APPLE__. It switches
 * itself on if the minimum is ever raised to 10.12.
 */
#if defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && __MAC_OS_X_VERSION_MIN_REQUIRED >= 101200
#include <sys/random.h>
#define TG_HAVE_GETENTROPY 1
#endif
#elif defined(__linux__)
/*
 * On Linux, getentropy() is looked up at run time instead of linked, because
 * linking it puts a GLIBC_2.25 version requirement into the library, and NAS
 * devices with an older glibc then refuse to load it at all. dlsym finds it
 * on glibc 2.25+ (and on musl); anywhere else the pointer stays NULL and
 * read_dev_urandom() below takes over. The library links -ldl already.
 */
#include <dlfcn.h>
#define TG_DYNAMIC_GETENTROPY 1

typedef int (*tg_getentropy_fn)(void *buf, size_t len);

/* Resolved once; racing threads all store the same value, so relaxed
 * atomics are enough. (void *)1 marks "looked up, not found". */
static void *tg_getentropy_ptr;

static tg_getentropy_fn tg_getentropy(void)
{
    void *p = __atomic_load_n(&tg_getentropy_ptr, __ATOMIC_RELAXED);
    if (!p) {
        p = dlsym(RTLD_DEFAULT, "getentropy");
        if (!p)
            p = (void *)1;
        __atomic_store_n(&tg_getentropy_ptr, p, __ATOMIC_RELAXED);
    }
    return p == (void *)1 ? NULL : (tg_getentropy_fn)p;
}
#endif

static bool read_dev_urandom(char *buf, int len)
{
    int fd;
    int ngot, ret;

    fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0)
        return false;

    ngot = 0;
    while (ngot < len) {
        ret = read(fd, buf+ngot, len-ngot);
        if (ret < 0) {
            close(fd);
            return false;
        }
        ngot += ret;
    }

    close(fd);

    return true;
}

/* TG: kernel CSPRNG, no child process and (with getentropy) no file
 * descriptor either. */
static bool read_kernel_random(char *buf, int len)
{
#ifdef TG_HAVE_GETENTROPY
    /* getentropy() takes at most 256 bytes per call, far more than we ask
     * for. It cannot fail on a booted system, but fall through if it does. */
    if (len <= 256 && getentropy(buf, len) == 0)
        return true;
#endif
#ifdef TG_DYNAMIC_GETENTROPY
    tg_getentropy_fn ge = tg_getentropy();
    if (ge && len <= 256 && ge(buf, len) == 0)
        return true;
#endif
    return read_dev_urandom(buf, len);
}

/*
 * This function is called once per PRNG creation. It reads 32 bytes out of
 * the kernel CSPRNG and loads the saved random seed from disk. Only if the
 * kernel gives us nothing at all does it fall back to the silly things
 * upstream does unconditionally - fetching an entire process listing and
 * scanning /tmp.
 *
 * TG: that fallback is gated because we are a shared library, and upstream's
 * version is wrong for us twice over. It is two fork()s plus two /bin/sh
 * execs for EVERY connection - global_prng lives in curlibctx, so
 * random_create() runs again for each context - and each exiting child
 * raises SIGCHLD on an arbitrary thread of the HOST process, where it
 * clobbers errno and, without SA_RESTART, can hand a spurious EINTR to
 * whatever that thread was blocked in.
 *
 * Nothing is lost cryptographically: 32 bytes from the kernel CSPRNG is a
 * full 256-bit seed, and the process listing is public, highly predictable
 * data that upstream keeps only for systems with no /dev/urandom at all.
 * That case still behaves exactly as before, including the exit(1) rather
 * than continuing with an unseeded PRNG.
 */

void noise_get_heavy(void (*func) (void *, int))
{
    char buf[512];
    FILE *fp;
    int ret;
    bool got_kernel_random = false;

    if (read_kernel_random(buf, 32)) {
        got_kernel_random = true;
        func(buf, 32);
    }

    if (!got_kernel_random) {
        fp = popen("ps -axu 2>/dev/null", "r");
        if (fp) {
            while ( (ret = fread(buf, 1, sizeof(buf), fp)) > 0)
                func(buf, ret);
            pclose(fp);
        } else {
            fprintf(stderr, "popen: %s\n"
                    "Unable to access fallback entropy source\n",
                    strerror(errno));
            exit(1);
        }

        fp = popen("ls -al /tmp 2>/dev/null", "r");
        if (fp) {
            while ( (ret = fread(buf, 1, sizeof(buf), fp)) > 0)
                func(buf, ret);
            pclose(fp);
        } else {
            fprintf(stderr, "popen: %s\n"
                    "Unable to access fallback entropy source\n",
                    strerror(errno));
            exit(1);
        }
    }

    read_random_seed(func);
}

/*
 * This function is called on a timer, and grabs as much changeable
 * system data as it can quickly get its hands on.
 */
void noise_regular(void)
{
    int fd;
    int ret;
    char buf[512];
    struct rusage rusage;

    if ((fd = open("/proc/meminfo", O_RDONLY)) >= 0) {
        while ( (ret = read(fd, buf, sizeof(buf))) > 0)
            random_add_noise(NOISE_SOURCE_MEMINFO, buf, ret);
        close(fd);
    }
    if ((fd = open("/proc/stat", O_RDONLY)) >= 0) {
        while ( (ret = read(fd, buf, sizeof(buf))) > 0)
            random_add_noise(NOISE_SOURCE_STAT, buf, ret);
        close(fd);
    }
    getrusage(RUSAGE_SELF, &rusage);
    random_add_noise(NOISE_SOURCE_RUSAGE, &rusage, sizeof(rusage));
}

/*
 * This function is called on every keypress or mouse move, and
 * will add the current time to the noise pool. It gets the scan
 * code or mouse position passed in, and adds that too.
 */
void noise_ultralight(NoiseSourceId id, unsigned long data)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    random_add_noise(NOISE_SOURCE_TIME, &tv, sizeof(tv));
    random_add_noise(id, &data, sizeof(data));
}

uint64_t prng_reseed_time_ms(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}
