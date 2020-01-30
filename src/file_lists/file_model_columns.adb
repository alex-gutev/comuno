--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Glib.Values;

package body File_Model_Columns is

   -- Packages --

   package Conversions is new System.Address_To_Access_Conversions
     (Entry_Pointers.Ref);

   procedure Free is new Ada.Unchecked_Deallocation
     (Entry_Pointers.Ref, Conversions.Object_Pointer);


   -- Column Types --

   --
   -- Boxed Directory_Entry Type
   --
   Entry_Type : Glib.Gtype;

   function Column_Types return Glib.Gtype_Array is
      Types : Glib.Gtype_Array :=
        (0 => Entry_Type,
         1 => Glib.Gtype_String);
   begin
      return Types;
   end Column_Types;


   -- Boxed Entry Type --

   --
   -- Copy_Entry
   --
   --  Copy a boxed shared pointer to a Directory_Entry.
   --
   function Copy_Entry (Src : System.Address) return System.Address;
   pragma Convention (C, Copy_Entry);

   function Copy_Entry (Src : System.Address) return System.Address is
      Dest_Ref : Conversions.Object_Pointer := new Entry_Pointers.Ref;
      Src_Ref  : Conversions.Object_Pointer := Conversions.To_Pointer(Src);

   begin
      Dest_Ref.all := Src_Ref.all;
      return Conversions.To_Address(Dest_Ref);

   end Copy_Entry;

   --
   -- Free_Entry
   --
   --  Release a shared pointer to a Directory_Entry, thus
   --  decrementing the reference count.
   --
   procedure Free_Entry (Ptr : System.Address);
   pragma Convention (C, Free_Entry);

   procedure Free_Entry (Ptr : System.Address) is
      Ref : Conversions.Object_Pointer :=
        Conversions.To_Pointer(Ptr);

   begin
      Free(Ref);
   end Free_Entry;

   --
   -- Box_Entry
   --
   --  Copy a Directory_Entry to the heap and create a boxed shared
   --  pointer to it.
   --
   procedure Box_Entry (Dir_Entry : in Directory_Entries.Directory_Entry; Value : in out Glib.Values.Gvalue) is
      Ref : Entry_Pointers.Ref;

   begin
      Ref.Set(Dir_Entry);

      Glib.Values.Init(Value, Entry_Type);
      Glib.Values.Set_Boxed(Value, Ref'Address);

   end Box_Entry;


   -- Setting Row Values --

   procedure Set_Values (Model : in Gtk.List_Store.Gtk_List_Store;
                         Row : in Gtk.Tree_Model.Gtk_Tree_Iter;
                         Dir_Entry : in Directory_Entries.Directory_Entry) is

      Value : Glib.Values.Gvalue;

   begin
      Box_Entry(Dir_Entry, Value);

      Model.Set_Value(Row, 0, Value);
      Model.Set(Row, 1, Directory_Entries.Subpath(Dir_Entry).Basename);

      Glib.Values.Unset(Value);

   end Set_Values;


   -- Getting Row Values --

   function Get_Entry (Model : in Gtk.List_Store.Gtk_List_Store;
                       Row : in Gtk.Tree_Model.Gtk_Tree_Iter)
                      return Entry_Pointers.Reference_Type is

      Value : Glib.Values.Gvalue;
      Ref : Entry_Pointers.Ref;

   begin
      Model.Get_Value(Row, 0, Value);
      Ref := Conversions.To_Pointer(Glib.Values.Get_Boxed(Value)).all;

      Glib.Values.Unset(Value);

      return Ref.Get;

   end Get_Entry;

begin

   -- Initialize boxed Directory_Entry GType --

   Entry_Type := Glib.Boxed_Type_Register_Static
     ("Directory_entry", Copy_Entry'Access, Free_Entry'Access);

end File_Model_Columns;
