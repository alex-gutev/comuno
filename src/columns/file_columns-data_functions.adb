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

with File_Model_Columns;

package body File_Columns.Data_Functions is

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

   end Memoized_Data_Function;

end File_Columns.Data_Functions;
