--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib;
with Gtk.Tree_Model;
with Gtk.Tree_Sortable;

--
-- Purpose:
--
--  Provides functions for sorting rows, representing directory
--  entries, in a GTK Tree Model.
--
package Sort_Functions is

   use type Glib.Gint;

   subtype Iter is Gtk.Tree_Model.Gtk_Tree_Iter;
   subtype Tree_Model is Gtk.Tree_Model.Gtk_Tree_Model;
   subtype Order is Glib.Gint range -1 .. 1;


   -- Sort Order Constants --

   Order_Less    : constant Order := -1;
   Order_Equal   : constant Order := 0;
   Order_Greater : constant Order := 1;


   -- Sort Functions --

   --
   -- Sort_Entry_Type
   --
   --  Sort by the type of entry.
   --
   --  Orders the parent entry first, followed by directories,
   --  followed by the remaining entries.
   --
   function Sort_Entry_Type (Model : Tree_Model; A, B : Iter) return Order;

   --
   -- Sort_Name
   --
   --  Sort by the basenames of the entries (case insensitive).
   --
   function Sort_Name (Model : Tree_Model; A, B : Iter) return Order;

   --
   -- Sort_Size
   --
   --  Sort by file size.
   --
   function Sort_Size (Model : Tree_Model; A, B : Iter) return Order;

   --
   -- Sort_Modified_Time
   --
   --  Sort by the time of the last modification of the entries.
   --
   function Sort_Modified_Time (Model : Tree_Model; A, B : Iter) return Order;

   --
   -- Sort_Extension
   --
   --  Sort by file extension
   --
   function Sort_Extension (Model : Tree_Model; A, B : Iter) return Order;


   --
   -- Sort_Combined
   --
   --  Sort by the order determined by two sort functions.
   --
   --  First sorts A and B by Sort1. If Sort1 does not return
   --  Order_Equal then returns the resulting sort order, otherwise
   --  returns the sort order returned by Sort2.
   --
   generic
      with function Sort1 (Model : Tree_Model; A, B : Iter) return Order;
      with function Sort2 (Model : Tree_Model; A, B : Iter) return Order;
   function Sort_Combined (Model : Tree_Model; A, B : Iter) return Order;

   --
   -- Sort_Combined
   --
   --  Sort by the inverted sort order of a function.
   --
   --  Inverts the sort order returned by Sort, that is if Sort
   --  returns Order_Less, returns Order_Greater, and if Sort returns
   --  Order_Greater, returns Order_Less.
   --
   generic
      with function Sort (Model : Tree_Model; A, B : Iter) return Order;
   function Sort_Inverted (Model : Tree_Model; A, B : Iter) return Order;

end Sort_Functions;
