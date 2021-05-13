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
 * Types for interfacing with Ada code, corresponding to the types in
 * the C_Types package.
 */

#include <stdint.h>

#include <sys/stat.h>

#ifdef __APPLE__
#include <sys/time.h>
#endif // __APPLE__

/**
 * Flag indicating that existing files should not be overwritten.
 */
#define CREAT_FLAG_EXCLUSIVE 1

/**
 * File type constants
 */
enum file_type {
	file_type_unknown = 0,
	file_type_regular = 1,
	file_type_directory = 2,
	file_type_link = 3,
	file_type_fifo = 4,
	file_type_character = 5,
	file_type_block = 6,
	file_type_socket = 7,
	file_type_whiteout = 8
};

/**
 * Directory entry metadata
 */
struct dir_entry {
	char *name;
	int kind; /* File Type */
};

/**
 * File attributes structure
 */
struct file_attributes {
	uint64_t device;
	uint64_t inode;
	uint64_t mode;
	uint64_t kind;
	uint64_t num_links;
	uint64_t user;
	uint64_t group;
	uint64_t size;
	uint64_t block_size;

	uint64_t modification_time;
	uint64_t access_time;
};

/* Platform Time Types */

/**
 * Platform dependent access and modification time type.
 */

#ifdef __APPLE__
typedef struct timeval time_type;

#else
typedef struct timespec time_type;

#endif
