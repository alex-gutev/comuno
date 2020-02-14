--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Glib;
with Gtk.Cell_Renderer_Text;

package body File_Columns is

   -- GTK Widget Subtypes --

   subtype Cell_Renderer_Text is Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;


   -- Builtin Column Map --

   package Column_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Column'Class,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   --
   -- Map from column identifiers to Column descriptor objects.
   --
   Column_Map : Column_Maps.Map;


   function Get_Column (Name : String) return Column_Ptr is
      Cursor : Column_Maps.Cursor := Column_Map.Find(Name);

   begin
      return (if Column_Maps.Has_Element(Cursor) then
                 Column_Map.Reference(Cursor).Element else
                 null);
   end Get_Column;

   procedure Append_Column (View : Tree_View; Col : Tree_View_Column) is
      Dummy : Glib.Gint;

   begin
      Dummy := View.Append_Column(Col);
   end Append_Column;


   -- Builtin Columns --

   --
   -- Contains Column descriptor definition for the file name column.
   --
   package Name_Columns is

      -- Column Index
      Name_Index : Glib.Gint := 2;

      --
      -- Name_Column
      --
      --  Name Column Descriptor
      --
      type Name_Column is new Column with null record;

      function Create (This : Name_Column) return Tree_View_Column;

   end Name_Columns;

   package body Name_Columns is

      function Create (This : Name_Column) return Tree_View_Column is
         Col : Tree_View_Column;
         Cell : Cell_Renderer_Text;

      begin
         -- Make Column --
         Gtk.Tree_View_Column.Gtk_New(Col);
         Col.Set_Title("Name");

         -- Make Cell Renderer --
         Gtk.Cell_Renderer_Text.Gtk_New(Cell);

         -- Add cell to column --
         Col.Pack_Start(Cell, True);

         -- Bind Tree View Column to Model --
         Col.Add_Attribute
           (Cell,
            Glib.Property_Name(Glib.Property(Gtk.Cell_Renderer_Text.Text_Property)),
            Name_Index);

         return Col;

      end Create;

   end Name_Columns;

begin

   -- Add Builtin Columns --

Add_Name_Column:
   declare
      Col : Name_Columns.Name_Column;
   begin
      Column_Map.Insert("name", Col);
   end Add_Name_Column;

end File_Columns;
