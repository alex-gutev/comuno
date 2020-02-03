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
with Ada.Containers.Indefinite_Holders;

with Gtk.Tree_Model;
with Gtk.List_Store;

with Directory_Entries;

--
-- Purpose:
--
--  Provides an abstract interface for populating a Gtk Tree_Model
--  with directory entries, obtained by reading a directory.
--
package File_Lists is

   use type Directory_Entries.Directory_Entry;


   -- Callback Interfaces --

   --
   -- Listener
   --
   --  Interface through which a 'listener' object is informed of
   --  changes to the file list.
   --
   type Listener is interface;

   --
   -- Change_Model
   --
   --  Called when the Tree Model, currently displayed to the user,
   --  should be changed.
   --
   --  Model: The new model.
   --
   procedure Change_Model (Callback : in out Listener;
                           Model    : in Gtk.List_Store.Gtk_List_Store)
     is abstract;

   --
   -- Select_Row
   --
   --  Called when the selected row, of the tree model, should be
   --  changed.
   --
   --  Row: The row which should become the new selection.
   --
   procedure Select_Row (Callback : in out Listener;
                         Row      : in Gtk.Tree_Model.Gtk_Tree_Iter)
     is abstract;


   -- Listener Holder Package --

   package Listener_Holders is new
     Ada.Containers.Indefinite_Holders(Listener'Class);


   -- File List Type --

   --
   -- File_List
   --
   --  Abstract interface for a type which is reponsible for obtaining
   --  and updating the contents of a Gtk Tree_Model.
   --
   --  The type is also responsible for keeping track of the selected
   --  entry and the set of marked entries.
   --
   type File_List is interface;


   -- Listener Object Accessors --

   --
   -- Set_Listener
   --
   --  Set file list's listener object, which is informed of changes
   --  to the Tree Model and selection.
   --
   procedure Set_Listener (List : in out File_List; Object : in Listener'Class)
     is abstract;

   --
   -- Get_Listener
   --
   --  Returns the file list's listener object.
   --
   function Get_Listener (List : in out File_List) return Listener_Holders.Reference_Type
     is abstract;


   -- Retrieving the Model --

   --
   -- Get_List
   --
   --  Returns the Tree Model.
   --
   function Get_List (List : in File_List) return Gtk.List_Store.Gtk_List_Store
     is abstract;


   -- Selection --

   --
   -- Selected_Row
   --
   --  Returns the selected row.
   --
   function Selected_Row (List : in File_List) return Gtk.Tree_Model.Gtk_Tree_Iter
     is abstract;


   -- Marking --

   package Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Directory_Entries.Directory_Entry);

   --
   -- Marked_Entries
   --
   --  Returns a vector of the marked entries.
   --
   function Marked_Entries (List : in File_List) return Entry_Vectors.Vector
     is abstract;

   --
   -- Mark_Row
   --
   --  Toggles the marked state of a row. If the row is unmarked it is
   --  marked, otherwise it is marked.
   --
   procedure Mark_Row (List : in out File_List; Row : in Gtk.Tree_Model.Gtk_Tree_Iter)
     is abstract;


   -- Tree View Events --

   --
   -- Selection_Changed
   --
   --  Procedure which should be called, by the user of a file list
   --  object, whenever the selection is changed by the user or some
   --  other event, not originating from the File_List, itself.
   --
   procedure Selection_Changed (List : in out File_List; Row : in Gtk.Tree_Model.Gtk_Tree_Iter)
     is abstract;

end File_Lists;
