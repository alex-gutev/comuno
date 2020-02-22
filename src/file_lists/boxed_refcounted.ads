--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with System.Address_To_Access_Conversions;

with Gnatcoll.Refcount;

with Glib;
with Glib.Values;
with Glib.Types;

with Gtk.Tree_Model;

--
-- Purpose:
--
--  Provides functions for storing an Ada type in a Glib GValue.
--
--  A single reference counted instance of the data is stored with
--  each GValue counting as a reference.
--
generic
   --
   -- The Ada data type to box.
   --
   type Data_Type (<>) is private;

   --
   -- A Glib type identifier for the boxed type
   --
   Name : in String;

package Boxed_Refcounted is

   -- Reference Counted Shared Pointer Package --

   package Pointers is new Gnatcoll.Refcount.Shared_Pointers (Data_Type);

   subtype Reference is Pointers.Reference_Type;

   --
   -- Glib type identifier for boxed type.
   --
   Boxed_Type : Glib.Gtype;

   --
   -- Box
   --
   --  Store the value Data in a Glib GValue.
   --
   --  A copy of the value is stored in a new reference counted
   --  pointer. Copying the GValue then creates a new reference rather
   --  than a new copy of the data.
   --
   procedure Box (Data : in Data_Type; Value : in out Glib.Values.Gvalue);

   --
   -- Get_Data
   --
   --  Retrieve the boxed data type from the Glib GValue.
   --
   function Get_Data (Value : Glib.Values.Gvalue) return Reference;

   --
   -- Get_Data
   --
   --  Retrieve the boxed data type from column Index of a row in a
   --  Tree Model.
   --
   --  The column must be of type Boxed_Type.
   --
   function Get_Data (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
                      Index : Glib.Gint)
                          return Reference;

private

   package Conversions is new System.Address_To_Access_Conversions
     (Pointers.Ref);

   function Copy_Data (Src : System.Address) return System.Address;
   pragma Convention (C, Copy_Data);

   procedure Free_Data (Ptr : System.Address);
   pragma Convention (C, Free_Data);


   Copy_Data_Access : constant Glib.Boxed_Copy := Copy_Data'Access;
   Free_Data_Access : constant Glib.Boxed_Free := Free_Data'Access;

end Boxed_Refcounted;
