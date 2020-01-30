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

with Gnatcoll.Refcount;

with Glib;
with Glib.Values;
with Glib.Types;

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
   --  NOTE: The reference count of Model is not increment, as a
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
   -- Reference Counted Directory_Entry Pointer
   --
   package Entry_Pointers is new Gnatcoll.Refcount.Shared_Pointers
     (Directory_Entries.Directory_Entry);

   subtype Entry_Ref is Entry_Pointers.Reference_Type;


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
   function Get_Entry (Model : in Gtk.List_Store.Gtk_List_Store;
                       Row   : in Gtk.Tree_Model.Gtk_Tree_Iter)
                      return Entry_Ref;


   -- Marked State --

   --
   -- Is_Marked
   --
   --  Returns true if the row in the model is marked.
   --
   function Is_Marked (Model : in Tree_Model; Row : in Row_Iter) return Boolean;

   --
   -- Set_Marked
   --
   --  Set the marked state of Row in Model to Marked.
   --
   procedure Set_Marked (Model : in Tree_Model; Row : in Row_Iter; Marked : in Boolean);

private

   type Model_Ref is new Ada.Finalization.Controlled with record
      Model : List_Store;
   end record;

end File_Model_Columns;
