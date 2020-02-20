--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

--
-- Purpose:
--
--  Column descriptor for the file extension column.
--
package File_Columns.Extension_Columns is

   --
   -- Extension_Column
   --
   --  Extension Column Descriptor
   --
   type Extension_Column is new Column with null record;

   -- Operations --

   function Column_Type (Col : Extension_Column) return Glib.Gtype;

   function Create (This : Extension_Column) return Tree_View_Column;

   function Get_Sort_Function (This : Extension_Column; Order : Gtk.Enums.Gtk_Sort_Type)
                              return Sort_Function;

end File_Columns.Extension_Columns;
