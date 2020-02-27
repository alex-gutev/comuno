--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers.Vectors;

with Glib;

with Gtk.Enums;
with Gtk.Tree_Sortable;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Tree_Model;
with Gtk.List_Store;

with Directory_Entries;

--
-- Purpose:
--
--  Provides a mapping from column identifiers to column descriptors.
--
--  A column descriptor is an object which is responsible for creating
--  the actual GTK Tree View Column.
--
package File_Columns is

   -- GTK Widget Subtypes --

   subtype Tree_View is Gtk.Tree_View.Gtk_Tree_View;
   subtype Tree_View_Column is Gtk.Tree_View_Column.Gtk_Tree_View_Column;

   subtype Sort_Function is Gtk.Tree_Sortable.Gtk_Tree_Iter_Compare_Func;


   -- Column Descriptors --

   --
   -- Column
   --
   --  Column descriptor type.
   --
   type Column is abstract tagged private;
   type Column_Ptr is access all Column'Class;

   package Column_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Column_Ptr);

   --
   -- Create
   --
   --  Create a new GTK Tree View Column.
   --
   function Create (Col : Column) return Tree_View_Column is abstract;

   --
   -- Column_Type
   --
   --  Return the GType of the data stored inside the tree model.
   --
   function Column_Type (Col : Column) return Glib.Gtype is abstract;

   --
   -- Get_Sort_Function
   --
   --  Returns the sort function for the column given the sort order.
   --
   function Get_Sort_Function (Col : Column; Order : Gtk.Enums.Gtk_Sort_Type) return Sort_Function is abstract;

   --
   -- Set_Data
   --
   --  Set the data for a the column, of a given row, in a tree model.
   --
   --  Dir_Entry: The directory entry corresponding to the row.
   --
   procedure Set_Data (Col       : Column;
                       Model     : Gtk.List_Store.Gtk_List_Store;
                       Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
                       Dir_Entry : Directory_Entries.Directory_Entry)
     is abstract;

   --
   -- Get_Index
   --
   --  Returns the column's index in the tree model.
   --
   function Get_Index (Col : Column'Class) return Glib.Gint;


   -- Retrieving Column Descriptors --

   --
   -- Get_Column
   --
   --  Returns the Column descriptor object for the column with
   --  identifier Name. If there is no column with identifier Name,
   --  null is returned.
   --
   function Get_Column (Name : String) return Column_Ptr;

   --
   -- Get_Column
   --
   --  Returns the Column descriptor object for the column at index
   --  Index within the Tree Model.
   --
   --  If Index is greater than the number of columns, null is
   --  returned.
   --
   function Get_Column (Index : Natural) return Column_Ptr;

   --
   -- Num_Columns
   --
   --  Returns the number of columns.
   --
   function Num_Columns return Natural;

   --
   -- All_Columns
   --
   --  Return a vector containing all the columns in the order they
   --  are added to the Tree Model.
   --
   function All_Columns return Column_Vectors.Vector;

   --
   -- Column_Types
   --
   --  Return an array containing the GType of each column.
   --
   function Column_Types return Glib.Gtype_Array;


   -- Utilities --

   --
   -- Append_Column
   --
   --  Add a column to a GTK Tree View.
   --
   procedure Append_Column (View : Tree_View; Col : Tree_View_Column);

private

   type Column is abstract tagged record
      Index : Glib.Gint;
   end record;

end File_Columns;
