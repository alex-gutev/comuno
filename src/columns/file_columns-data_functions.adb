--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib.Properties;
with Gtk.Cell_Renderer_Text;
with Gtk.List_Store;
with Gdk.Rgba;

with File_Model_Columns;

package body File_Columns.Data_Functions is

   --
   -- Marked Row Color
   --
   Marked_Color : constant Gdk.Rgba.Gdk_Rgba :=
     (Red   => 1.0,
      Green => 0.0,
      Blue  => 0.0,
      Alpha => 1.0);

   --
   -- Non marked row color
   --
   Unmarked_Color : Gdk.Rgba.Gdk_Rgba := Gdk.Rgba.Black_Rgba;

   --
   -- Set_Text_Color
   --
   --  Set the text color attribute of a cell, based on whether the
   --  row is marked or unmarked.
   --
   procedure Set_Text_Color
     (Cell  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row   :                 Gtk.Tree_Model.Gtk_Tree_Iter);


   -- Data Function --

   procedure Memoized_Data_Function
     (Layout :                 Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row    :                 Gtk.Tree_Model.Gtk_Tree_Iter;
      Index  :                 Glib.Gint) is

      Ent : Directory_Entries.Directory_Entry :=
        File_Model_Columns.Get_Entry(Model, Row);

   begin

      if not File_Model_Columns.Has_Field(Model, Row, Index) then
         declare
            Formatted : String := Format(Ent);

         begin
            File_Model_Columns.Set_Field(Model, Row, Index, Formatted);

            Glib.Properties.Set_Property
              (Cell,
               Gtk.Cell_Renderer_Text.Text_Property,
               Formatted);
         end;

      else
         Glib.Properties.Set_Property
           (Cell,
            Gtk.Cell_Renderer_Text.Text_Property,
            File_Model_Columns.Get_Field(Model, Row, Index));

      end if;

      Set_Text_Color(Cell, Model, Row);

   end Memoized_Data_Function;

   procedure Set_Text_Color
     (Cell  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row   :                 Gtk.Tree_Model.Gtk_Tree_Iter) is

   begin
      Gdk.Rgba.Set_Property
        (Cell,
         Gtk.Cell_Renderer_Text.Foreground_Rgba_Property,
         (if File_Model_Columns.Is_Marked(Model, Row) then
             Marked_Color else
             Unmarked_Color));

   end Set_Text_Color;

end File_Columns.Data_Functions;
