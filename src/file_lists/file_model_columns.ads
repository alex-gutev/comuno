--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization;

with Glib;
with Glib.Values;

with Gtk.Tree_Model;
with Gtk.List_Store;

with Directory_Entries;

--
-- Purpose:
--
--  Provides functions for setting the values of the Gtk.Tree_Model
--  columns.
--
package File_Model_Columns is

   subtype Tree_Model is Gtk.List_Store.Gtk_List_Store;
   subtype List_Store is Gtk.List_Store.Gtk_List_Store;
   subtype Row_Iter is Gtk.Tree_Model.Gtk_Tree_Iter;


   --
   -- Index of the first column that is displayed inside the tree
   -- view.
   --
   Column_Start : constant Glib.Gint;

   -- Reference Counting --

   --
   -- Model_Ref
   --
   --  Reference to a Gtk_List_Store which automatically increments
   --  the Reference count (by Ref) when copied and decrements the
   --  reference count (by Unref) when deallocated.
   --
   type Model_Ref is new Ada.Finalization.Controlled with private;

   overriding procedure Adjust (Ref : in out Model_Ref);
   overriding procedure Finalize (Ref : in out Model_Ref);

   --
   -- Set
   --
   --  Set the List_Store to which the reference refers to.
   --
   --  NOTE: The reference count of Model is not incremented, as a
   --  List_Store model is created with a reference count of 1.
   --
   procedure Set (Ref : in out Model_Ref; Model : in List_Store);

   --
   -- Get
   --
   --  Returns the access to the Gtk_List_Store model.
   --
   function Get (Ref : in Model_Ref) return List_Store;

   --
   -- Entry_Ref
   --
   --  Reference to a row's directory entry.
   --
   type Entry_Ref (Ent : not null access Directory_Entries.Directory_Entry) is limited private
   with Implicit_Dereference => Ent;


   -- List Store Model Columns --

   --
   -- Column_Types
   --
   --  Returns the types of the columns inside the Tree Model.
   --
   function Column_Types return Glib.Gtype_Array;

   --
   -- Set_Values
   --
   --  Set the columns values of a Row inside Model, to the values of
   --  the properites of the Directory_Entry Dir_Entry.
   --
   procedure Set_Values (Model     : in Gtk.List_Store.Gtk_List_Store;
                         Row       : in Gtk.Tree_Model.Gtk_Tree_Iter;
                         Dir_Entry : in Directory_Entries.Directory_Entry);

   --
   -- Get_Entry
   --
   --  Retrieve the Directory_Entry displayed in Row of Model.
   --
   function Get_Entry (Model : Tree_Model;
                       Row   : Row_Iter)
                      return Entry_Ref;

   function Get_Entry (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row   : Row_Iter)
                      return Entry_Ref;


   -- Cached Formatted String Values --

   --
   -- Cached_String_Type
   --
   --  Return the Glib Type which stores a cached formatted string.
   --
   --  This type allows a formatted string, created in a row data
   --  function, to be stored inside a tree model such that future
   --  invocations of the data function may retrieve the cached string
   --  rather than recreating a formatted string.
   --
   function Cached_String_Type return Glib.Gtype;

   --
   -- Set_Cached_String
   --
   --  Set the column value, of a given row, in a list store tree
   --  model to an empty cached formatted string.
   --
   procedure Set_Cached_String (Model : List_Store;
                                Row   : Row_Iter;
                                Index : Glib.Gint);


   --
   -- The following functions are used to retrieve formatted string
   -- values which are displayed to the user in columns.
   --

   --
   -- Has_Field
   --
   --  Check whether the row has a cached formatted string value for
   --  the column at Index.
   --
   function Has_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row   : Row_Iter;
                       Index : Glib.gint)
                      return Boolean;

   --
   -- Get_Field
   --
   --  Retrieve the formatted string value for the column Index of a
   --  given row.
   --
   function Get_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row   : Row_Iter;
                       Index : Glib.gint)
                      return String;

   --
   -- Set_Field
   --
   --  Set the formatted string value for column Index of a given row.
   --
   procedure Set_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                        Row   : Row_Iter;
                        Index : Glib.Gint;
                        Value : String);


   -- Marked State --

   --
   -- Is_Marked
   --
   --  Returns true if the row in the model is marked.
   --
   function Is_Marked (Model : in Gtk.Tree_Model.Gtk_Tree_Model; Row : in Row_Iter) return Boolean;

   --
   -- Set_Marked
   --
   --  Set the marked state of Row in Model to Marked.
   --
   procedure Set_Marked (Model : in Tree_Model; Row : in Row_Iter; Marked : in Boolean);


   -- Utilities --

   --
   -- Get_Row_Index
   --
   --  Return the row index corresponding to a particularly Gtk Tree
   --  Iter.
   --
   function Get_Row_Index (Model : in Gtk.Tree_Model.Gtk_Tree_Model; Row : in Row_Iter) return Glib.Gint;

private

   Column_Start : constant Glib.Gint := 2;

   type Entry_Ref (Ent : not null access Directory_Entries.Directory_Entry) is null record;

   type Model_Ref is new Ada.Finalization.Controlled with record
      Model : List_Store;
   end record;

end File_Model_Columns;
