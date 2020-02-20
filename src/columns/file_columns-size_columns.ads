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
--  Column descriptor for the file size column.
--
package File_Columns.Size_Columns is

   --
   -- Size_Column
   --
   --  Size Column Descriptor
   --
   type Size_Column is new Column with null record;


   -- Operations --

   function Column_Type (Col : Size_Column) return Glib.Gtype;

   function Create (This : Size_Column) return Tree_View_Column;

   function Get_Sort_Function (This : Size_Column; Order : Gtk.Enums.Gtk_Sort_Type)
                              return Sort_Function;

   procedure Set_Row_Data (This  : Size_Column;
                           Model : Gtk.List_Store.Gtk_List_Store;
                           Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
                           Ent   : Directory_Entries.Directory_Entry);

end File_Columns.Size_Columns;
