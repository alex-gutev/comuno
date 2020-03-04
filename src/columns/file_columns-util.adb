--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body File_Columns.Util is

   function Create_Column (Title : String) return Tree_View_Column is
      Col : Tree_View_Column;

   begin
      Gtk.Tree_View_Column.Gtk_New(Col);
      Col.Set_Title(Title);
      Col.Set_Resizable(True);

      return Col;

   end Create_Column;

   procedure Set_Ellipsize_Mode
     (Cell : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Mode : Pango.Layout.Pango_Ellipsize_Mode) is

   begin
      Pango.Layout.Pango_Ellipsize_Mode_Properties.Set_Property
        (Cell,
         Pango.Layout.Pango_Ellipsize_Mode_Properties.Property
           (Gtk.Cell_Renderer_Text.Ellipsize_Property),
         Mode);

   end Set_Ellipsize_Mode;


end File_Columns.Util;
