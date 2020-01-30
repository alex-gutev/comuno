--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

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
   --
   -- Reference Counted Directory_Entry Pointer
   --
   package Entry_Pointers is new Gnatcoll.Refcount.Shared_Pointers
     (Directory_Entries.Directory_Entry);

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
   procedure Set_Values (Model : in Gtk.List_Store.Gtk_List_Store;
                         Row : in Gtk.Tree_Model.Gtk_Tree_Iter;
                         Dir_Entry : in Directory_Entries.Directory_Entry);

   --
   -- Get_Entry
   --
   --  Retrieve the Directory_Entry displayed in Row of Model.
   --
   function Get_Entry (Model : in Gtk.List_Store.Gtk_List_Store;
                       Row : in Gtk.Tree_Model.Gtk_Tree_Iter)
                      return Entry_Pointers.Reference_Type;

end File_Model_Columns;
