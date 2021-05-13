/*
 * Copyright (C) 2019  Alexander Gutev <alex.gutev@gmail.bg>
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
 * Interfaces for listing the contents of a regular directory,
 * accessible via the POSIX dirent API.
 */

#include "types.h"

/**
 * Open a directory for reading.
 *
 * @param path Path to the directory.
 *
 * @return Handle to the directory or NULL if the directory could not
 *   be opened.
 */
void *open_dir(const char *path);

/**
 * Release all resources held by a handle to a directory.
 *
 * @param handle Handle to the directory.
 */
void close_dir(void *handle);

/**
 * Retrieve the metadata of the next entry in a directory.
 *
 * @param handle Handle to the directory.
 *
 * @param ent Pointer to dir_entry struct which is populated with the
 *   metadata of the next entry.
 *
 * @return 1 if the metadata of the next entry was actually read
 *         0 if there are no more entries in the directory
 *        -1 if there was an error reading the directory
 */
int get_entry(void *handle, struct dir_entry *ent);

/**
 * Retrieve the full file attributes of a directory entry.
 *
 * @param handle Handle to the directory.
 *
 * @param name Name of the entry.
 *
 * @param attrs Pointer to file_attributes structure which is populated
 *   with the file attributes.
 *
 * @return 1 if the file attributes were obtained successfully, 0 if
 *   there was an error.
 */
int get_attributes(void *handle, const char *name, struct file_attributes *attrs);


/* Writer Interface */

/**
 * Open a directory, in order to modify its contents.
 *
 * @param path Path to the directory.
 *
 * @return File descriptor of the directory (>= 0), or < 0 on error.
 */
int dir_writer_open(const char *path);

/**
 * Close a directory.
 *
 * @param dir File descriptor of the directory.
 *
 * @return 0 if successful, non-zero if an error occurred.
 */
int dir_writer_close(int dir);

/**
 * Create a directory.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath at which to create the directory relative
 *    to the the parent directory.
 *
 * @return 0 if successful, non-zero on error.
 */
int dir_writer_make_dir(int dir, const char *path);

/**
 * Create a symbolic link.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath at which to create the symbolic link,
 *   relative to the parent directory.
 *
 * @param target Path to the file the link points to.
 *
 * @param attr New attributes of the symbolic link.
 *
 * @return 0 if successful, non-zero on error.
 */
int dir_writer_symlink(int dir, const char *path, const char *target, struct file_attributes *attr);

/**
 * Set the permissions (mode) of a file.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath of the file of which to change the
 *   permissions, relative to the parent directory.
 *
 * @param mode New file mode, equivalent to the stat.st_mode
 *   field. Only permission bits should be set.
 *
 * @return 0 if successful, non-zero on error.
 */
int dir_writer_set_mode(int dir, const char *path, uint64_t mode);

/**
 * Set the owner and group of a file.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath of the file of which to change the
 *   owner, relative to the parent directory.
 *
 * @param attr file_attributes structure containing the new value for
 *   the file owner and group.
 *
 * @return 0 if successful, non-zero on error.
 */
int dir_writer_set_owner(int dir, const char *path, struct file_attributes *attr);

/**
 * Set the access and modification time of a file.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath of the file of which to change the access
 *   and modification times, relative to the parent directory.
 *
 * @param attr file_attributes structure containing the new value for
 *   the access and modification time.
 *
 * @return 0 if successful, non-zero on error.
 */
int dir_writer_set_times(int dir, const char *path, struct file_attributes *attr);

/**
 * Check if a file exists in a directory.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath to the file to check for existence,
 *   relative to the parent directory.
 *
 * @return 0 if the file exists, non-zero otherwise.
 */
int dir_writer_check(int dir, const char *path);

/**
 * Rename a file within a directory.
 *
 * Does not check if a file exists at the destination path.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param src Name/subpath of the file to rename, relative to the
 *   parent directory.
 *
 * @param dest Name/subpath to which the file should be renamed,
 *   relative to the parent directory.
 *
 * @return 0 if the file exists, non-zero otherwise.
 */
int dir_writer_rename(int dir, const char *src, const char *dest);

/**
 * Remove a file/directory.
 *
 * @param dir File descriptor of the parent directory.
 *
 * @param path Name/subpath of the file to remove, relative to the
 *   parent directory.
 *
 * @return 0 if the file exists, non-zero otherwise.
 */
int dir_writer_remove(int dir, const char *src);
