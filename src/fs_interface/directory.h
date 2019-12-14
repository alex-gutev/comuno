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
 * @return 1 if the metadata of the next entry was actually read, 0 if
 *   there are no more entries in the directory.
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
