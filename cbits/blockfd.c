// NULL
#include <stddef.h>

// select, fd_set, FD_ZERO, FD_SET
#ifdef _WIN32
#include <Winsock2.h>
#else
#include <sys/select.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

int thread_wait_read(int fd);

#ifdef __cplusplus
}
#endif

/* Block until a file descriptor is readable. */
int thread_wait_read(int fd) {
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);

    return select(fd + 1, &fds, NULL, NULL, NULL);
}
