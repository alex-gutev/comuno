--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with File_System;
with Regular_Directory_Types;

package body Directory_Types is

   function Get_Type (Path : Paths.Path) return Directory_Type'Class is
   begin
      return Regular_Directory_Types.Create(Path);
   end Get_Type;

   function Get_Type (Path : Paths.Path; File : Directory_Entries.Directory_Entry) return Directory_Type'Class is
      use Directory_Entries;

      File_Path : Paths.Path renames Paths.Path(Subpath(File));

   begin

      case Kind(File) is
         when File_System.Directory =>
            return Regular_Directory_Types.Create(Path.Append(File_Path));

         when others =>
            raise Not_Directory;
      end case;

   end Get_Type;

end Directory_Types;
