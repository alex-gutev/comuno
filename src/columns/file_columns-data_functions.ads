--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib;

with Gtk.Cell_Layout;
with Gtk.Cell_Renderer;
with Gtk.Tree_Model;

with Directory_Entries;

--
-- Purpose:
--
--  Provides functions which can be used as Tree View Column Cell data
--  functions.
--
package File_Columns.Data_Functions is

   -- GTK subtypes --

   type Data_Function is
     access function (Dir_Entry : Directory_Entries.Directory_Entry) return String;

   --
   -- Memoized_Data_Function
   --
   --  Cell data function which formats the displayed string, using
   --  the Format function, only the first time it is called, and
   --  stores the formatted string at field 'Index' of the row's entry
   --  data.
   --
   --  This function assumes the following:
   --
   --   - The cell is GTK Cell Renderer Text to display a string
   --     value.
   --
   --  Notes:
   --
   --   This function also sets the text color of the cell based on
   --   whether the row is marked or unmarked.
   --
   generic
      --
      -- Function which should return a formatted string of the entry
      -- field which is displayed in the column.
      --
      with function Format (Ent : Directory_Entries.Directory_Entry) return String;

   procedure Memoized_Data_Function
     (Layout :                 Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row    :                 Gtk.Tree_Model.Gtk_Tree_Iter;
      Index  :                 Glib.Gint);

end File_Columns.Data_Functions;
