--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Pango.Layout;
with Gtk.Cell_Renderer_Text;

--
-- Purpose:
--
--  Utilities for creating GTK Tree View Columns.
--
package File_Columns.Util is

   --
   -- Create_Column
   --
   --  Create a new GTK Tree View Column with a given heading title.
   --
   function Create_Column (Title : String) return Tree_View_Column;

   --
   -- Set_Ellipsize_Mode
   --
   --  Set the ellipsize mode of a Cell Renderer Text.
   --
   procedure Set_Ellipsize_Mode
     (Cell : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Mode : Pango.Layout.Pango_Ellipsize_Mode);

end File_Columns.Util;
