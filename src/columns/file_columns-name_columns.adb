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
with Glib.Properties;

with Gtk.Cell_Layout;
with Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;
with Gtk.List_Store;

with Sort_Functions;
with File_Model_Columns;

package body File_Columns.Name_Columns is
   use type Gtk.Enums.Gtk_Sort_Type;

   subtype Cell_Renderer_Text is Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;


   -- Data Function --

   package Set_Data_Func is new Gtk.Tree_View_Column.Set_Cell_Data_Func_User_Data
     (Glib.Gint);

   procedure Data_Function
     (Layout :                 Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row    :                 Gtk.Tree_Model.Gtk_Tree_Iter;
      Index  :                 Glib.Gint);


   -- Column Creation --

   function Create (This : Name_Column) return Tree_View_Column is
      Col  : Tree_View_Column;
      Cell : Cell_Renderer_Text;

   begin
      -- Make Column --
      Gtk.Tree_View_Column.Gtk_New(Col);
      Col.Set_Title("Name");

      -- Make Cell Renderer --
      Gtk.Cell_Renderer_Text.Gtk_New(Cell);

      -- Add cell to column --
      Col.Pack_Start(Cell, True);

      -- Set Cell Data Function --
      Set_Data_Func.Set_Cell_Data_Func(Col, Cell, Data_Function'Access, This.Get_Index);

      Col.Set_Expand(True);
      Col.Set_Sort_Column_Id(This.Index);

      return Col;

   end Create;


   -- Column Type --

   function Column_Type (Col : Name_Column) return Glib.Gtype is
      (Glib.Gtype_String);


   -- Sorting --

   function Sort_Type_Inverted is new
     Sort_Functions.Sort_Inverted(Sort_Functions.Sort_Entry_Type);

   function Sort_Inverted is new Sort_Functions.Sort_Combined
     (Sort_Type_Inverted,
      Sort_Functions.Sort_Name);

   function Sort is new Sort_Functions.Sort_Combined
     (Sort_Functions.Sort_Entry_Type,
      Sort_Functions.Sort_Name);


   function Get_Sort_Function (This : Name_Column; Order : Gtk.Enums.Gtk_Sort_Type) return Sort_Function is
   begin
      if Order = Gtk.Enums.Sort_Ascending then
         return Sort'Access;
      else
         return Sort_Inverted'Access;
      end if;
   end Get_Sort_Function;


   -- Data Function --

   procedure Data_Function
     (Layout :                 Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row    :                 Gtk.Tree_Model.Gtk_Tree_Iter;
      Index  :                 Glib.Gint) is

      Ent : Directory_Entries.Directory_Entry :=
        File_Model_Columns.Get_Entry(Model, Row);

   begin

      Glib.Properties.Set_Property
        (Cell,
         Gtk.Cell_Renderer_Text.Text_Property,
         Directory_Entries.Subpath(Ent).Filename);

   end Data_Function;


   -- Setting Row Data --

   procedure Set_Row_Data (This  : Name_Column;
                           Model : Gtk.List_Store.Gtk_List_Store;
                           Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
                           Ent   : Directory_Entries.Directory_Entry) is
   begin
      Model.Set(Row, This.Get_Index, Directory_Entries.Subpath(Ent).Filename);
   end Set_Row_Data;

end File_Columns.Name_Columns;
