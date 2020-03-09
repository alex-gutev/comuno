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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Gtk.Tree_Model;
with Gtk.List_Store;

with Paths;
with Directory_Entries;
with File_Lists;
with Sort_Functions;

private with Gnatcoll.Refcount;

private with Virtual_Hierarchies;
private with File_Model_Columns;

--
-- Purpose:
--
--  Implementation of the File_List interface which populates the Tree
--  Model with the full list of entries in a given directory.
--
package Full_File_Lists is

   -- Packages --

   package Entry_Vectors renames File_Lists.Entry_Vectors;
   package Listener_Holders renames File_Lists.Listener_Holders;


   --
   -- File_List
   --
   --  Implementation of the File_List interface.
   --
   --  Reads a list of entries from a directory and populates a Gtk
   --  Tree Model with the full list.
   --
   --  NOTE: This is a reference to a shared file list. Copying an
   --        object of this type copies the reference, not the
   --        underlying list. The shared list is deallocated when the
   --        last reference is deallocated.
   --
   type File_List is new Ada.Finalization.Controlled and File_Lists.File_List with private;


   -- Initialization --

   overriding procedure Initialize (This : in out File_List);


   -- Listener Object Accessors --

   overriding procedure Set_Listener (This : in out File_List; Object : in File_Lists.Listener'Class);
   overriding function Get_Listener (This : in out File_List) return Listener_Holders.Reference_Type;


   -- Path Accessors --

   --
   -- Path
   --
   --  Returns the path of the directory, of which, the contents are
   --  stored in the File List.
   --
   function Path (This : File_List) return Paths.Path;


   -- Changing the Path --

   --
   -- Change_Path
   --
   --  Begin reading a new directory and build a Tree Model with its
   --  contents.
   --
   --  Path: Path to the directory to read
   --
   --  Move_To_Old: If true the selection is set to the row
   --               corresponding to the entry with the same name, as
   --               the previous directory
   --
   procedure Change_Path (This        : File_List;
                          Path        : Paths.Path;
                          Move_To_Old : Boolean := False);

   --
   -- Descend
   --
   --  Descend into the directory corresponding to an entry in the
   --  file list.
   --
   --  Ent: The entry to descend into. Must be an entry obtained from
   --       the current file list.
   --
   --  Returns true if the entry is a directory into which it can be
   --  descended.
   --
   function Descend (This : File_List;
                     Ent  : Directory_Entries.Directory_Entry)
                    return Boolean;


   -- Tree Model Accessors --

   overriding function Get_List (This : in File_List) return Gtk.List_Store.Gtk_List_Store;


   -- Selection --

   overriding function Selected_Row (This : in File_List) return Gtk.Tree_Model.Gtk_Tree_Iter;
   overriding function Marked_Entries (This : in File_List) return Entry_Vectors.Vector;
   overriding procedure Mark_Row (This : in out File_List; Row : in Gtk.Tree_Model.Gtk_Tree_Iter);


   -- Tree View Events --

   overriding procedure Selection_Changed (This : in out File_List; Row : in Gtk.Tree_Model.Gtk_Tree_Iter);


private

   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   subtype Virtual_Hierarchy is Virtual_Hierarchies.Virtual_Hierarchy;


   --
   -- Map Type storing marked entries
   --
   package Entry_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Paths.Path_String,
        Element_Type    => Gtk.Tree_Model.Gtk_Tree_Iter,
        Equivalent_Keys => "=",
        Hash            => Ada.Strings.Hash);

   --
   -- File_List_Data
   --
   --  Shared file list object.
   --
   type File_List_Data is record
      Path       : Paths.Path;                    -- Path to the directory
      Hierarchy  : Virtual_Hierarchy;             -- Virtual directory hierarchy

      Empty_List : File_Model_Columns.Model_Ref; -- Empty Tree Model, displayed while reading
      List       : File_Model_Columns.Model_Ref; -- Tree Model storing actual directory contents

      Selection  : Gtk.Tree_Model.Gtk_Tree_Iter;  -- Selected Row
      Marked_Set : Entry_Maps.Map;                -- Set of marked entries

      Listener   : Listener_Holders.Holder;       -- Listener object

      Reading    : Boolean;                       -- Flag for whether currently reading directory
   end record;


   -- Reference counting pointer to shared list object --

   package Data_Pointers is new Gnatcoll.Refcount.Shared_Pointers
     (Element_Type => File_List_Data);

   subtype Data_Ptr is Data_Pointers.Ref;
   subtype Data_Weak_Ptr is Data_Pointers.Weak_Ref;

   subtype Data_Ref is Data_Pointers.Reference_Type;


   -- File_List Reference Type --

   type File_List is new Ada.Finalization.Controlled and File_Lists.File_List with record
      Data : Data_Ptr;
   end record;

end Full_File_Lists;
