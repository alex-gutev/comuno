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

/**
 * File input and output streams.
 */

#ifndef COMUNO_FILE_STREAM_H
#define COMUNO_FILE_STREAM_H

#include <stdlib.h>
#include <sys/types.h>

#include "types.h"

#define FILE_FLAG_EXCL 1

/**
 * Convert Ada file creation flag constants to the equivalent
 * constants accepted by the open(2) system call.
 *
 * @param flags Ada file creation flags.
 *
 * @return Unix file creation flags
 */
int ada_to_unix_flags(int flags);


/* Input Stream */

/**
 * File input stream
 */
struct file_instream {
    int fd;

    char *buf;
    size_t buf_size;
};

/**
 * Create a file input stream.
 *
 * @param path Path to the file to open for reading.
 *
 * @param buf_size Size of the buffer to use for reading.
 *
 * @return Pointer to the file input stream if opened
 *   successfully. NULL otherwise.
 */
void *file_instream_create(const char *path, size_t buf_size);

/**
 * Create a file input stream, for a file relative to an open
 * directory.
 *
 * @param fd File descriptor of the open directory.
 *
 * @param path Path to the file to open for reading, relative to the
 *   directory.
 *
 * @param buf_size Size of the buffer to use for reading.
 *
 * @return Pointer to the file input stream if opened
 *   successfully. NULL otherwise.
 */
void *file_instream_create_at(int fd, const char *path, size_t buf_size);

/**
 * Close a file input stream.
 *
 * @param Pointer to the file input stream.
 */
void file_instream_close(void *handle);

/**
 * Read a block of data from an input stream.
 *
 * @param handle Pointer to the file input stream.
 *
 * @param buf Pointer to a variable which receives a pointer to the
 *   buffer, storing the block of data which was read from the file.
 *
 * @return Number of bytes read, or -1 if an error occurred.
 */
ssize_t file_instream_read(void *handle, char **buf);


/* Output Stream */

/**
 * File output stream
 */
struct file_outstream {
    int fd;
};

/**
 * Create a file output stream.
 *
 * @param path Path to the file to open for writing.
 *
 * @param flags File creation flags
 *
 * @param perms File permissions.
 *
 * @return Pointer to the file output stream if opened
 *   successfully. NULL otherwise.
 */
void *file_outstream_create(const char *path, int flags, int perms);

/**
 * Create a file output stream, for a file relative to an open
 * directory.
 *
 * @param fd File descriptor of the open directory.
 *
 * @param path Path to the file to open for writing, relative to the
 *   directory.
 *
 * @param flags File creation flags
 *
 * @param perms File permissions.
 *
 * @return Pointer to the file output stream if opened
 *   successfully. NULL otherwise.
 */
void *file_outstream_create_at(int fd, const char *path, int flags, int perms);

/**
 * Close a file output stream.
 *
 * @param handle Pointer to the output stream.
 *
 * @return Zero if the stream was closed successfully, all data
 *   written was committed to the storage medium, non-zero otherwise.
 */
int file_outstream_close(void *handle);

/**
 * Set the access and modification times of the underlying file.
 *
 * @param handle Pointer to the output stream.
 * @param mod Modification timestamp
 * @param access Access timestamp
 *
 * @return 0 if successful, non-zero on error.
 */
int file_outstream_set_times(void *handle, uint64_t mod, uint64_t access);

/**
 * Set the permissions (mode) of the underlying file.
 *
 * @param handle Pointer to the output stream.
 * @param mode New file permissions (equivalent to stat.st_mode)
 *
 * @return 0 if successful, non-zero on error.
 */
int file_outstream_set_mode(void *handle, uint64_t mode);

/**
 * Set the owner and group of the underlying file.
 *
 * @param handle Pointer to the output stream.
 *
 * @param attr file_attributes structure containing the new file owner
 *   and new group.
 *
 * @return 0 if successful, non-zero on error.
 */
int file_outstream_set_owner(void *handle, const struct file_attributes *attr);

/**
 * Seek to a particular location within the file output stream.
 *
 * @param handle File output stream.
 *
 * @param offset Offset from the current stream position, to seek to.
 *
 * @return Zero if the seek was successful, non-zero otherwise.
 */
int file_outstream_seek(void *handle, size_t offset);

/**
 * Write a block of data to a file output stream.
 *
 * @param handle File output stream.
 *
 * @param buf Block of data to write.
 *
 * @param n Number of bytes to write.
 *
 * @return Number of bytes written, or -1 if an error occurred.
 */
ssize_t file_outstream_write(void *handle, const char *buf, size_t n);

#endif /* COMUNO_FILE_STREAM_H */
