--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers.Indefinite_Holders;
with Directory_Trees;

--
-- Purpose:
--
--  Provides a common interface to background read directory tasks.
--
package Background_Tasks is

   -- Directory_Tree Holder Package --

   package Tree_Holders is new Ada.Containers.Indefinite_Holders
     (Directory_Trees.Directory_Tree'Class,
      Directory_Trees."=");

   --
   -- Background_Task
   --
   --  Background Task interface type.
   --
   type Background_Task is task interface;
   type Background_Task_Ptr is access all Background_Task'Class;

   --
   -- Finish
   --
   --  When called the Directory_Tree, into which the entries were
   --  read, should be moved into Tree.
   --
   --  The task should terminate after accepting this entry.
   --
   procedure Finish (T : in out Background_Task; Tree : in out Tree_Holders.Holder)
     is abstract;

end Background_Tasks;
