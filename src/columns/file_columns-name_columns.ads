--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Gtk.Enums;
with Gtk.Tree_Sortable;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;

--
-- Purpose:
--
--  Column descriptor definition for the file name column.
--
package File_Columns.Name_Columns is
   --
   -- Name_Column
   --
   --  Name Column Descriptor
   --
   type Name_Column is new Column with null record;


   -- Operations --

   function Column_Type (Col : Name_Column) return Glib.Gtype;

   function Create (This : Name_Column) return Tree_View_Column;

   function Get_Sort_Function (This : Name_Column; Order : Gtk.Enums.Gtk_Sort_Type)
                              return Sort_Function;

   procedure Set_Row_Data (This  : Name_Column;
                           Model : Gtk.List_Store.Gtk_List_Store;
                           Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
                           Ent   : Directory_Entries.Directory_Entry);

end File_Columns.Name_Columns;
