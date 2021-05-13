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

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/param.h>

#include "types.h"

/**
 * Convert a dirent DT_x file type constant to a file_type constant.
 *
 * @param type The DT_x file type constant.
 *
 * @return The corresponding file_type enum
 *   constant. file_type_unknown is returned for invalid or unknown
 *   values.
 */
static enum file_type dt_to_ft(int type);

/**
 * Retrieve the access and modification times from a file_attributes
 * structure, in the correct type for the platform.
 *
 * @param attr File attributes
 *
 * @param times Output array of size 2 in which the access and
 *   modification times are placed.
 */
static void stat_times(const struct file_attributes *attr, time_type times[]);


/* Lister Interface */

void *open_dir(const char * path) {
	return opendir(path);
}

void close_dir(void *handle) {
	DIR *dp = handle;
	closedir(handle);
}


int get_entry(void *handle, struct dir_entry * out_ent) {
	DIR *dp = handle;

	errno = 0;
	struct dirent *ent = readdir(dp);

	if (ent) {
		out_ent->name = ent->d_name;
		out_ent->kind = dt_to_ft(ent->d_type);

		return 1;
	}
	else {
		return errno ? -1 : 0;
	}
}

enum file_type dt_to_ft(int type) {
	switch (type) {
	case DT_UNKNOWN:
		return file_type_unknown;

	case DT_FIFO:
		return file_type_fifo;

	case DT_CHR:
		return file_type_character;

	case DT_BLK:
		return file_type_block;

	case DT_DIR:
		return file_type_directory;

	case DT_REG:
		return file_type_regular;

	case DT_LNK:
		return file_type_link;

	case DT_SOCK:
		return file_type_socket;

	case DT_WHT:
		return file_type_whiteout;

	default:
		return file_type_unknown;
	}
}


int get_attributes(void *handle, const char *name, struct file_attributes *attrs) {
	DIR *dp = handle;

	struct stat st;

	if (fstatat(dirfd(dp), name, &st, 0))
		return 0;

	attrs->device = st.st_dev;
	attrs->inode = st.st_ino;
	attrs->mode = st.st_mode;
	attrs->kind = dt_to_ft(IFTODT(st.st_mode));
	attrs->num_links = st.st_nlink;
	attrs->user = st.st_uid;
	attrs->group = st.st_gid;
	attrs->size = st.st_size;
	attrs->block_size = st.st_blksize;

	attrs->modification_time = st.st_mtime;
	attrs->access_time = st.st_atime;

	return 1;
}

void stat_times(const struct file_attributes *attr, time_type times[]) {
#ifdef __APPLE__
	times[0].tv_sec = attr->access_time;
	times[0].tv_usec = 0;

	times[1].tv_sec = attr->modification_time;
	times[1].tv_usec = 0;

#else
	times[0].tv_sec = attr->access_time;
	times[0].tv_nsec = 0;

	times[1].tv_sec = attr->modification_time;
	times[1].tv_nsec = 0;

#endif // __APPLE__
}


/* Writer Interface */

int dir_writer_open(const char *path) {
	return open(path, O_DIRECTORY);
}

int dir_writer_close(int dir) {
	return close(dir);
}

int dir_writer_make_dir(int dir, const char *path) {
	return mkdirat(dir, path, 0);
}

int dir_writer_symlink(int dir, const char *path, const char *target, struct file_attributes *attr) {
	return symlinkat(target, dir, path);
}

int dir_writer_set_mode(int dir, const char *path, struct file_attributes *attr) {
	if (attr->kind != file_type_link) {
		return fchmodat(dir, path, attr->mode, 0);
	}

	return 0;
}

int dir_writer_set_owner(int dir, const char *path, struct file_attributes *attr) {
	return fchownat(dir, path, attr->user, attr->group, AT_SYMLINK_NOFOLLOW);
}

int dir_writer_set_times(int dir, const char *path, struct file_attributes *attr) {
	time_type times[2];

	stat_times(attr, times);

#ifdef __APPLE__

	char dir_path[MAXPATHLEN + 1 + strlen(path) + 1];

	if (!fcntl(dir, F_GETPATH, dir_path)) {
		size_t len = strlen(dir_path);

		dir_path[len] = '/';
		strcpy(dir_path + len + 1, path);

		return lutimes(dir_path, times);
	}

	return -1;

#else
	return utimensat(dir, path, time, AT_SYMLINK_NOFOLLOW);

#endif
}

int dir_writer_check(int dir, const char *path) {
	return faccessat(dir, path, F_OK, AT_SYMLINK_NOFOLLOW);
}

int dir_writer_rename(int dir, const char *src, const char *dest) {
	return renameat(dir, src, dir, dest);
}

int dir_writer_remove(int dir, const char *src) {
	if (unlinkat(dir, src, AT_REMOVEDIR)) {
		if (errno == ENOTDIR) {
			return unlinkat(dir, src, 0);
		}
	}

	return 0;
}
