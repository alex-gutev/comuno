/*
 * Copyright (C) 2020  Alexander Gutev <alex.gutev@gmail.bg>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "file_stream.h"

#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/stat.h>

#ifdef __APPLE__
#include <sys/time.h>
#endif // __APPLE_

int ada_to_unix_flags(int flags) {
    int uflags = 0;

    if (flags & FILE_FLAG_EXCL)
        uflags |= O_EXCL;
}

/* Input Stream */

/**
 * Allocate and initialize a file_instream.
 *
 * @param fd Open file descriptor.
 * @param buf_size Read buffer size.
 *
 * @return Pointer to the file_instream struct, NULL if the allocation
 *   failed.
 */
static void *create_instream_handle(int fd, size_t buf_size);


void *file_instream_create(const char *path, size_t buf_size) {
    int fd = open(path, O_RDONLY | O_CLOEXEC);

    if (fd < 0) return NULL;

    void *handle = create_instream_handle(fd, buf_size);

    if (!handle) {
        close(fd);
    }

    return handle;
}

void *file_instream_create_at(int dir_fd, const char *path, size_t buf_size) {
    int fd = openat(dir_fd, path, O_RDONLY | O_CLOEXEC);

    if (fd < 0) return NULL;

    void *handle = create_instream_handle(fd, buf_size);

    if (!handle) {
        close(fd);
    }

    return handle;
};

void *create_instream_handle(int fd, size_t buf_size) {
    struct file_instream *handle = malloc(sizeof(struct file_instream));

    if (!handle) return NULL;

    handle->fd = fd;

    if (!(handle->buf = malloc(buf_size)))
        goto free_handle;

    handle->buf_size = buf_size;

    return (void *)handle;

free_handle:
    free(handle);

    return NULL;
}


void file_instream_close(void *handle) {
    struct file_instream *stream = handle;

    close(stream->fd);

    free(stream->buf);
    free(stream);
}

ssize_t file_instream_read(void *handle, char **buf) {
    struct file_instream *stream = handle;

    ssize_t n_read = read(stream->fd, stream->buf, stream->buf_size);
    *buf = n_read > 0 ? stream->buf : NULL;

    return n_read;
}


/* Output Stream */

static void *create_outstream_handle(int fd);

void *file_outstream_create(const char *path, int flags, int perms) {
    flags = ada_to_unix_flags(flags);
    int fd = open(path, flags | O_WRONLY | O_CLOEXEC | O_CREAT | O_TRUNC, perms);

    if (fd < 0)
        return NULL;

    void *handle = create_outstream_handle(fd);

    if (!handle) goto close_fd;

    return handle;

close_fd:
    close(fd);
    return NULL;

}

void *file_outstream_create_at(int dir_fd, const char *path, int flags, int perms) {
    flags = ada_to_unix_flags(flags);
    int fd = openat(dir_fd, path, flags | O_WRONLY | O_CLOEXEC | O_CREAT | O_TRUNC, perms);

    if (fd < 0)
        return NULL;

    void *handle = create_outstream_handle(fd);

    if (!handle) goto close_fd;

    return handle;

close_fd:
    close(fd);
    return NULL;

}


void *create_outstream_handle(int fd) {
    struct file_outstream *handle = malloc(sizeof(struct file_outstream));

    if (!handle) return NULL;

    handle->fd = fd;
    return handle;
}


int file_outstream_close(void *handle) {
    struct file_outstream *stream = handle;

    int err = close(stream->fd);
    free(stream);

    return err;
};

int file_outstream_set_times(void *handle, uint64_t mod, uint64_t access) {
    struct file_outstream *stream = handle;

#ifdef __APPLE__
    struct timeval times[2];

    times[0].tv_sec = access;
    times[0].tv_usec = access;

    times[1].tv_sec = mod;
    times[1].tv_usec = 0;

    return futimes(stream->fd, times);

#else
    struct timespec times[2];

    times[0].tv_sec = access;
    times[0].tv_nsec = 0;

    times[1].tv_sec = mod;
    times[1].tv_nsec = 0;

    return futimens(fd, times);

#endif
}

int file_outstream_set_mode(void *handle, uint64_t mode) {
    struct file_outstream *stream = handle;

    return fchmod(stream->fd, mode);
}
int file_outstream_set_owner(void *handle, const struct file_attributes *attr) {
    struct file_outstream *stream = handle;

    return fchown(stream->fd, attr->user, attr->group);
}

int file_outstream_seek(void *handle, size_t offset) {
    struct file_outstream *stream = handle;

    return lseek(stream->fd, offset, SEEK_CUR);
}

ssize_t file_outstream_write(void *handle, const char *buf, size_t n) {
    struct file_outstream *stream = handle;

    return write(stream->fd, buf, n);
}
