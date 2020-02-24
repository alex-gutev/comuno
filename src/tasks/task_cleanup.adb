--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Ada.Strings.Hash;

with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

package body Task_Cleanup is

   -- Task Map --

   function Hash (Key : Ada.Task_Identification.Task_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(Ada.Task_Identification.Image(Key));
   end Hash;

   package Task_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Task_Identification.Task_Id,
      Element_Type    => Tracked_Task_Ptr,
      Hash            => Hash,
      Equivalent_Keys => Ada.Task_Identification."=");


   procedure Free_Task is new Ada.Unchecked_Deallocation
     (Tracked_Task'Class, Tracked_Task_Ptr);


   -- Task Track Object --

   protected Tracker is
      procedure Track (Ptr : in Tracked_Task_Ptr);

   private
      Tasks : Task_Maps.Map;

   end Tracker;

   protected body Tracker is

      --
      -- Cleanup_Task
      --
      --  Deallocate a task and remove it from the task map.
      --
      procedure Cleanup_Task (C : Ada.Task_Termination.Cause_Of_Termination;
                              T : Ada.Task_Identification.Task_Id;
                              X : Ada.Exceptions.Exception_Occurrence) is

         Ptr : Tracked_Task_Ptr := Tasks(T);

      begin
         Tasks.Delete(T);
         Free_Task(Ptr);

      end Cleanup_Task;

      --
      -- Track
      --
      --  Add a task to the task map and set its termination handler.
      --
      procedure Track (Ptr : Tracked_Task_Ptr) is
         Key : constant Ada.Task_Identification.Task_Id := Ptr.all'Identity;

      begin
         Tasks.Insert(Key, Ptr);
         Ada.Task_Termination.Set_Specific_Handler(Key, Cleanup_Task'Access);

      end Track;

   end Tracker;

   procedure Track (Ptr : in Tracked_Task_Ptr) is
   begin
      Tracker.Track(Ptr);
   end Track;

end Task_Cleanup;
