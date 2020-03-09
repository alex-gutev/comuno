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
with Ada.Exceptions;

with Paths;
with Directory_Entries;

with Background_Tasks;

private with Ada.Containers.Indefinite_Holders;

private with Gnatcoll.Refcount;

private with Directory_Types;
private with Directory_Trees;
private with Task_States;

--
-- Purpose:
--
--  Provides functionality for maintaining the state of a virtual
--  directory hierarchy.
--
--  The type Virtual_Hierarchy provides procedures for reading a
--  directory, after which the virtual directory hierarchy is
--  constructed and automatically updated in response to changes at
--  the file system level.
--
package Virtual_Hierarchies is

   -- Directory_Tree Holder Package --

   package Tree_Holders renames Background_Tasks.Tree_Holders;


   -- Operation Callback Interface --

   --
   -- Operation_Callback
   --
   --  Interface by which the Virtual_Hierarchy object communicates
   --  the status of background read/update tasks.
   --
   type Operation_Callback is interface;

   --
   -- Begin_Operation
   --
   --  Called before beginning the background operation.
   --
   procedure Begin_Operation (This : in Operation_Callback)
     is abstract;

   --
   -- New_Entry
   --
   --  Called when a new entry is read. This procedure is only called
   --  for entries which are in the current subdirectory of the
   --  virtual directory hierarchy.
   --
   procedure New_Entry (This      : in Operation_Callback;
                        Dir_Entry : in Directory_Entries.Directory_entry)
     is abstract;

   --
   -- Finish_Operation
   --
   --  Called after the operation has completed successfully.
   --
   procedure Finish_Operation (This : in Operation_Callback)
     is abstract;

   --
   -- Operation_Error
   --
   --  Called when an error occurs during a background operation. This
   --  includes cancellation.
   --
   procedure Operation_Error (This  : in Operation_Callback;
                              Error : in Ada.Exceptions.Exception_Occurrence)
     is abstract;


   -- Listener Callback Interfaces

   --
   -- Directory_Changed_Listener
   --
   --  Interface for a 'listener' object which is informed whenever
   --  the state of the hierarchy changes.
   --
   type Directory_Changed_Listener is interface;

   --
   -- Directory_Changed
   --
   --  Called when the state of the underlying directory changes.
   --
   --  Should return an object, of which, the procedures of the
   --  Operation_Callback interface will be called when updating the
   --  virtual hierarchy.
   --
   function Directory_Changed (Listener : Directory_Changed_Listener) return Operation_Callback'Class is abstract;


   --
   -- Directory_Deleted_Listener
   --
   --  Interface for a 'listener' object which is informed when the
   --  underlying directory has been deleted.
   --
   type Directory_Deleted_Listener is interface;

   --
   -- Directory_Deleted
   --
   --  Called when the underlying directory has been deleted.
   --
   procedure Directory_Deleted (Listener : Directory_Deleted_Listener; Old_Path : Paths.Path) is abstract;


   -- Virtual File System Object --

   --
   -- Virtual_Hierarchy
   --
   --  Maintains a virtual directory hierarchy.
   --
   --  This object is a reference to the actual virtual
   --  hierarchy. Copying the object only creates a new reference to
   --  the shared object rather than copying the hierarchy. The shared
   --  object is deallocated when the last reference to it is
   --  deallocated.
   --
   type Virtual_Hierarchy is new Ada.Finalization.Controlled with private;


   -- Initialization --

   overriding procedure Initialize (Ref : in out Virtual_Hierarchy);


   -- Accessors --

   --
   -- Path
   --
   --  Returns the logical path to the directory.
   --
   --  The logical path includes the path to the actual directory and
   --  the subpath within the virtual hierarchy.
   --
   function Path (Ref : Virtual_Hierarchy) return Paths.Path;


   -- Background Tasks --

   --
   -- Read
   --
   --  Begin reading a new directory, and construct the new virtual
   --  hierarchy.
   --
   --  Path: The path to the directory.
   --
   --  Callback: The callback object with which the status of the
   --            operation, and the entries read are communicated.
   --
   procedure Read (Ref      : in out Virtual_Hierarchy;
                   Path     : in Paths.Path;
                   Callback : in Operation_Callback'Class);

   --
   -- Descend
   --
   --  Descend into a directory which is an entry within the
   --  hierarchy.
   --
   --  Ent: The entry to descend into. Must be an entry within the
   --  hierarchy.
   --
   --  Returns true if the entry is a directory into which it can be
   --  descended.
   --
   function Descend (Ref       : in out Virtual_Hierarchy;
                     Dir_Entry : in Directory_Entries.Directory_Entry;
                     Callback  : in Operation_Callback'Class)
                    return Boolean;

private

   use type Directory_Types.Directory_Type;
   use type Directory_Trees.Directory_Tree;


   -- Holder Packages --

   package Type_Holders is new Ada.Containers.Indefinite_Holders (Directory_Types.Directory_Type'Class);

   package Changed_Listener_Holders is new Ada.Containers.Indefinite_Holders (Directory_Changed_Listener'Class);
   package Deleted_Listener_Holders is new Ada.Containers.Indefinite_Holders (Directory_Deleted_Listener'Class);


   --
   -- Virtual_Hierarchy_Data
   --
   --  Shared virtual hierarchy object.
   --
   type Virtual_Hierarchy_Data is tagged record
      Task_State       : Task_States.Cancellation_State;  -- Background Task Cancellation State
      Directory_Type   : Type_Holders.Holder;             -- Type of the actual directory
      Current_Tree     : Tree_Holders.Holder;             -- Directory tree storing the hierarchy
      Changed_Listener : Changed_Listener_Holders.Holder; -- Changed listener object
      Deleted_Listener : Deleted_Listener_Holders.Holder; -- Deleted Listener object
   end record;


   package Pointers is
     new Gnatcoll.Refcount.Shared_Pointers (Virtual_Hierarchy_Data);

   subtype Data_Ptr is Pointers.Ref;
   subtype Data_Weak_Ptr is Pointers.Weak_Ref;
   subtype Data_Ref is Pointers.Reference_Type;

   -- Virtual Hierarchy Reference --

   type Virtual_Hierarchy is new Ada.Finalization.Controlled with record
      Data : Data_Ptr;
   end record;

end Virtual_Hierarchies;
