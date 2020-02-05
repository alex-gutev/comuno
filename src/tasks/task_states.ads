--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization; use Ada.Finalization;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Unchecked_Deallocation;

--
-- Purpose:
--
--  Provides a background task state abstraction, which allows a task
--  to be asynchronously cancelled.
--
--  The task can be in one of three states:
--
--   - Background
--   - Foreground
--   - Cancelled
--
--  When the task is performing work in the background it is said to
--  be in the 'Background' state. When in this state it should not
--  communicate, at all, with any foreground task. During this state,
--  the task can be cancelled at any moment.
--
--  To communicate with a foreground task (which could be the task
--  that initiated the background task), the task should switch to the
--  'Foreground' state. The task cannot be cancelled during this
--  state, thus any work done during this state should be limited to
--  short non-blocking operations.
--
--  The task can be 'Cancelled', by a foreground task, during which
--  its state switches to Cancelled. Cancellation does not terminate
--  the background task immediately, however once successfully
--  switching to the 'Cancelled', it can no longer communicate with
--  the foreground task and is thus effectively cancelled.
--
--  For cancellation to result in the eventual termination of the
--  background task, it should periodically check whether it has been
--  cancelled, and then exit.
--
--  The Task_State object is a reference to the shared task state. The
--  memory for the shared task state is managed by reference counting
--  with each Task_State object being counted as a reference. The
--  shared task state is automatically deallocated once there are no
--  more Task_State objects refering to it.
--
-- Usage:
--
--  - When initiating a new background task, a Task_State object
--    should be created for it using the Create function.
--
--  - The background task should obtain a Background_State object,
--    from the Task_State object, using Get_Background_State.
--
--    - Enter_Foreground should be called (to switch to the foreground
--      state), by the background task, prior to communicating results
--      with a foreground task. Note: The actual communication may
--      occur on the background task.
--
--    - Exit_Foreground should be called, by the background task,
--      after communicating the results, after which the task may
--      resume its work in the background.
--
--    - The task should check whether it is Cancelled using
--      Is_Cancelled or Test_Cancelled.
--
--
--  - The foreground task, which initiated the background task, should
--    obtain a Cancellation_State object, from the Task_State object,
--    using Get_Cancellation_State.
--
--    - The foreground task can cancel the background task, using
--      Cancel.
--
package Task_States is
   pragma Assertion_Policy (Pre => Check);

   --
   -- Task_Cancelled
   --
   --  Exception indicating that the task has been cancelled.
   --
   Task_Cancelled : exception;


   -- Continuing after Cancellation --

   --
   -- Continuation
   --
   --  An interface to a continuation procedure which is called once a
   --  task has been successfully cancelled.
   --
   --  This should be used to launch new tasks, which can only begin
   --  safely once the current task has been cancelled.
   --
   --  NOTE: The background task will no longer communicate with the
   --  foreground after the continuation procedure has been called.
   --
   type Continuation is interface;

   --
   -- Continue
   --
   --  Invokes the continuation procedure.
   --
   --  Cancelled: If True the operation was cancelled, otherwise the
   --  operation completed succesfully.
   --
   procedure Continue (C : Continuation; Cancelled : Boolean) is abstract;


   -- Task State Reference --

   --
   -- Task_State_Ref
   --
   --  References a shared task state object.
   --
   --  NOTE: Does not reference any Task_State object unless it is
   --  created by Create.
   --
   type Task_State_Ref is new Controlled with private;

   --
   -- Create
   --
   --  Create a shared task state object and return a Task_State_Ref,
   --  which references it.
   --
   function Create return Task_State_Ref;

   procedure Finalize (State : in out Task_State_Ref);
   procedure Adjust (State : in out Task_State_Ref);

   --
   -- Is_Empty
   --
   --  Returns true if the Task_State_Ref does not point to any shared
   --  task state.
   --
   function Is_Empty (State : Task_State_Ref) return Boolean;


   -- Background Task State --

   --
   -- Task_State
   --
   --  Provides the procedures for changing the state, of a shared
   --  task state object, which should only be called by the
   --  background task.
   --
   --  NOTE: Does not reference any Task_State object unless it is
   --  obtained by Get_Background_State
   --
   type Background_State is new Task_State_Ref with private;


   --
   -- Get_Background_State
   --
   --  Obtains a reference to a shared task state, which can be used
   --  by the background task.
   --
   function Get_Background_State (State : Task_State_Ref'Class) return Background_State;

   --
   -- Enter_Foreground
   --
   --  Enter the 'Foreground' state, to communicate results with the
   --  foreground process.
   --
   --  While in the 'Foreground' state, that is until Exit_Foreground,
   --  is called, the background task cannot be cancelled.
   --
   -- Exceptions:
   --
   --  If the task has been cancelled prior to calling this procedure,
   --  a Task_Cancelled exception is raised.
   --
   procedure Enter_Foreground (State : in Background_State)
   with Pre => not State.Is_Empty;

   --
   -- Exit_Foreground
   --
   --  Exit the 'Foreground' state, indicating that the current
   --  communication with foreground process has finished, and the
   --  task will be resuming its work in the background.
   --
   --  After this procedure is called, the task may be cancelled.
   --
   -- Exceptions:
   --
   --  If an attempt was made to cancel the task, prior to this
   --  procedure being called, the task is cancelled and a
   --  Task_Cancelled exception is raised.
   --
   procedure Exit_Foreground (State : in Background_State)
   with Pre => not State.Is_Empty;


   --
   -- Test_Cancelled
   --
   --  Check whether the task was cancelled. If the task was cancelled
   --  a Task_Cancelled exception is raised.
   --
   procedure Test_Cancelled (State : in Background_State)
   with Pre => not State.Is_Empty;

   --
   -- Is_Cancelled
   --
   --  Returns true if the task was cancelled.
   --
   function Is_Cancelled (State : in Background_State) return Boolean
   with Pre => not State.Is_Empty;


   -- Cancellation State --

   --
   -- Cancellation_State
   --
   --  Provides the procedures for cancelling a background task, using
   --  a shared task state object, from a foreground task.
   --
   --  NOTE: Does not reference any Task_State object unless it is
   --  obtained by Get_Cancellation_State
   --
   type Cancellation_State is new Task_State_Ref with private;

   --
   -- Get_Cancellation_State
   --
   --  Obtains a reference to a shared task state, which can be used
   --  by the foreground task to cancel a background task.
   --
   function Get_Cancellation_State (State : Task_State_Ref'Class) return Cancellation_State;

   --
   -- Cancel
   --
   --  Cancel the task.
   --
   --  If the task was in the background state, it is cancelled
   --  immediately, otherwise if it is in the foreground state
   --  cancellation is deferred to when it returns to the background
   --  state.
   --
   --  NOTE: With this procedure there is no way to know whether the
   --  task was cancelled immediately or cancellation was
   --  deferred. Use the other Cancel procedure which takes a
   --  Continuation procedure as an argument.
   --
   procedure Cancel (State : in Cancellation_State)
   with Pre => not State.Is_Empty;

   --
   -- Cancel
   --
   --  Cancel the task.
   --
   --  If the task was in the background state, it is cancelled
   --  immediately, otherwise if it is in the foreground state
   --  cancellation is deferred to when it returns to the background
   --  state.
   --
   --  When the task is cancelled, either immediately or deferred, the
   --  Continue procedure of the Continuation 'C' is called.
   --
   procedure Cancel (State : in Cancellation_State; C : in Continuation'Class)
   with Pre => not State.Is_Empty;

   --
   -- Finish
   --
   --  Set the task's state to finished.
   --
   --  This calls the continue procedure, if any, with cancelled equal
   --  to false.
   --
   --  This procedure should be called by the foreground task once it
   --  knows that the task will no longer be communicating with it.
   --
   procedure Finish (State : in Cancellation_State)
   with Pre => not State.Is_Empty;

private

   package Continuation_Holders is new Ada.Containers.Indefinite_Holders
     (Continuation'Class);

   --
   -- Refcount
   --
   --  Type used to store the reference counts.
   --
   --  NOTE: Limited to 32 bits as it is unlikely that more references
   --  to task state objects will be needed.
   --
   type Refcount is range 0 .. 2**32;

   --
   -- Task_State
   --
   --  Shared task state object.
   --
   --  NOTE: This object does its own reference counting, rather than
   --  using GNATCOLL.Refcount, since that would require:
   --
   --  1. A wrapper over the protected object. This wrapper has to be
   --  derived from Ada.Finalization.Controlled, and store an Access
   --  to the protected object, as a type derived from
   --  Gnatcoll.Refcount.Refcounted cannot be limited.
   --
   --  2. A wrapper over the previous wrapper which is derived from
   --  Refcounted in order to add reference counting.
   --
   --  3. The reference counting pointer which stores an access to the
   --  wrapper.
   --
   --  All in all this is three wrappers over the actual task state
   --  object verses the one wrapper currently used. Since reference
   --  counting isn't too difficult to implement, especially when weak
   --  pointers are not needed, it was decided that the code is
   --  cleaner if the protected task state object does its own
   --  reference counting.
   --
   protected type Task_State is

      -- Reference Counting --

      --
      -- Acquire
      --
      --  Increment the reference count.
      --
      procedure Acquire;

      --
      -- Release
      --
      --  Decrement the reference count, with the new reference count
      --  returned in 'Count'.
      --
      --  NOTE: The object is not free if Count is decremented to
      --  zero. It is the responsibility of the caller to check
      --  whether Count = 0 and free the object.
      --
      procedure Release (Count : out Refcount);

      -- Enter/Exit Foreground procedures --

      procedure Enter_Foreground;
      procedure Exit_Foreground;

      -- Cancellation Testing --

      procedure Test_Cancelled;
      function Is_Cancelled return Boolean;

      -- Cancellation --

      procedure Cancel;
      procedure Cancel (C : Continuation'Class);

      -- Finalization --

      procedure Finish;

   private

      Foreground     : Boolean  := False;           -- Flag: True if in the 'Foreground' state
      Cancelled      : Boolean  := False;           -- Flag: True if marked for cancellation
      Finished       : Boolean  := False;           -- Flag: True if task has finished
      After_Cancel   : Continuation_Holders.Holder; -- Continuation procedure to call after cancellation
      Num_References : Refcount := 1;               -- Reference Count

   end Task_State;

   type Task_State_Ptr is access Task_State;

   procedure Free is new Ada.Unchecked_Deallocation (Task_State, Task_State_Ptr);


   -- Task State References --

   type Task_State_Ref is new Controlled with record
      Object : Task_State_Ptr;
   end record;

   type Background_State is new Task_State_Ref with null record;

   type Cancellation_State is new Task_State_Ref with null record;

end Task_States;
