--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Gtk.Tree_View;
with Gtk.Tree_View_Column;

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


   -- Column Descriptors --

   --
   -- Column
   --
   --  Column descriptor type.
   --
   type Column is abstract tagged private;
   type Column_Ptr is access all Column'Class;

   --
   -- Create
   --
   --  Create a new GTK Tree View Column.
   --
   function Create (Col : Column) return Tree_View_Column is abstract;


   -- Retrieving Column Descriptors --

   --
   -- Get_Column
   --
   --  Returns the Column descriptor object for the column with
   --  identifier Name. If there is no column with identifier Name,
   --  null is returned.
   --
   function Get_Column (Name : String) return Column_Ptr;


   -- Utilities --

   --
   -- Append_Column
   --
   --  Add a column to a GTK Tree View.
   --
   procedure Append_Column (View : Tree_View; Col : Tree_View_Column);

private

   type Column is abstract tagged null record;

end File_Columns;
