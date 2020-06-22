--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body Directory_Writers is

   package C renames Interfaces.C;

   use type Access_Mask;

   function Ada_To_Unix (Perms : File_Permissions) return Access_Mask is
     ((if Perms.Other.Read then 1 else 0) or
        (if Perms.Other.Write then 2 else 0) or
        (if Perms.Other.Execute then 4 else 0) or

        (if Perms.Group.Read then 8#10# else 0) or
        (if Perms.Group.Write then 8#20# else 0) or
        (if Perms.Group.Execute then 8#40# else 0) or

        (if Perms.User.Read then 8#100# else 0) or
        (if Perms.User.Write then 8#200# else 0) or
        (if Perms.User.Execute then 8#400# else 0) or

        (if Perms.Set_Uid then 8#4000# else 0) or
        (if Perms.Set_Gid then 8#2000# else 0) or
        (if Perms.Sticky then 8#1000# else 0));

end Directory_Writers;
