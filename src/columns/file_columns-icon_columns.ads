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
--  Column descriptor for the file icon column.
--
package File_Columns.Icon_Columns is

   --
   -- Icon_Column
   --
   --  Icon Column Descriptor
   --
   type Icon_Column is new Column with null record;


   -- Operations --

   overriding function Column_Type (Col : Icon_Column) return Glib.Gtype;

   overriding function Create (This : Icon_Column) return Tree_View_Column;

   overriding function Get_Sort_Function (This : Icon_Column; Order : Gtk.Enums.Gtk_Sort_Type)
                                         return Sort_Function;

   overriding procedure Set_Data (This      : Icon_Column;
                                  Model     : Gtk.List_Store.Gtk_List_Store;
                                  Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
                                  Dir_Entry : Directory_Entries.Directory_Entry);

end File_Columns.Icon_Columns;
