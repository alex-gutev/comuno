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
with Glib.Values;

with Gtk.Cell_Layout;
with Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf;

with Gdk.Pixbuf;

with File_Model_Columns;
with Boxed_Refcounted;
with Icon_Loader;

package body File_Columns.Icon_Columns is

   use type Gtk.Enums.Gtk_Sort_Type;

   subtype Pixbuf is Gdk.Pixbuf.Gdk_Pixbuf;
   subtype Cell_Renderer is Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;


   -- Column Type --

   type Cached_Icon is record
      Is_Set : Boolean;
      Icon   : Pixbuf;
   end record;

   package Boxed is new Boxed_Refcounted (Cached_Icon, "Cached_Icon");


   -- Data Function --

   package Set_Data_Func is new
     Gtk.Tree_View_Column.Set_Cell_Data_Func_User_Data (Glib.Gint);

   procedure Data_Function
     (Layout :                 Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row    :                 Gtk.Tree_Model.Gtk_Tree_Iter;
      Index  :                 Glib.Gint);


   -- Column Creation --

   function Create (This : Icon_Column) return Tree_View_Column is
      Col : Tree_View_Column;
      Cell : Cell_Renderer;

   begin
      -- Make Column --
      Gtk.Tree_View_Column.Gtk_New(Col);

      -- Make Cell Renderer --
      Gtk.Cell_Renderer_Pixbuf.Gtk_New(Cell);

      -- Add cell to column --
      Col.Pack_Start(Cell, True);

      -- Set Cell Data Function --
      Set_Data_Func.Set_Cell_Data_Func(Col, Cell, Data_Function'Access, This.Get_Index);

      -- Set Properties --
      Col.Set_Expand(False);

      return Col;

   end Create;


   -- Column Type --

   function Column_Type (Col : Icon_Column) return Glib.Gtype is
     (Boxed.Boxed_Type);


   -- Data Function --

   procedure Data_Function
     (Layout :                 Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  :                 Gtk.Tree_Model.Gtk_Tree_Model;
      Row    :                 Gtk.Tree_Model.Gtk_Tree_Iter;
      Index  :                 Glib.Gint) is

      Ent : Directory_Entries.Directory_Entry :=
        File_Model_Columns.Get_Entry(Model, Row);

      Ref : Boxed.Reference := Boxed.Get_Data(Model, Row, Index);

   begin
      if not Ref.Is_Set then
         Ref.Is_Set := True;
         Ref.Icon   := Icon_Loader.Load_Icon(Ent);
      end if;

      Glib.Properties.Set_Property
        (Cell,
         Gtk.Cell_Renderer_Pixbuf.Pixbuf_Property,
         Ref.Icon);

   end Data_Function;


   -- Sorting --

   function Get_Sort_Function (This : Icon_Column; Order : Gtk.Enums.Gtk_Sort_Type)
                              return Sort_Function is
     (null);


   procedure Set_Data (This      : Icon_Column;
                       Model     : Gtk.List_Store.Gtk_List_Store;
                       Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
                       Dir_Entry : Directory_Entries.Directory_Entry) is

      Value : Glib.Values.Gvalue;

   begin
      Boxed.Box((Is_Set => False, others => <>), Value);

      Model.Set_Value(Row, This.Get_Index, Value);
      Glib.Values.Unset(Value);

   end Set_Data;

end File_Columns.Icon_Columns;
