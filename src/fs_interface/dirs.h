/*
 * Copyright (C) 2020  Alexander Gutev <alex.gutev@mail.bg>
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

#ifndef COMUNO_DIRS_H
#define COMUNO_DIRS_H

/**
 * Functions for retrieving the paths to various standard directories.
 */

/**
 * Retrieve the path to the current user's home directory.
 *
 * @return Absolute path to the home directory or NULL if it could not
 * be retrieved.
 */
char * home_dir(void);

/**
 * Retrieve the path to the home directory corresponding to a given
 * user name.
 *
 * @param user The username.
 *
 * @return Absolute path to the home directory or NULL if it could not
 * be retrieved.
 */
char * user_home_dir(const char * user);

#endif /* COMUNO_DIRS_H */
