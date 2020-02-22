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
with Ada.Containers;
with Ada.Strings.Unbounded;
with System.Address_To_Access_Conversions;

with Glib.Values;

with File_Columns;

package body File_Model_Columns is

   use type Glib.Guint;
   use type Gtk.List_Store.Gtk_List_Store;

   -- Index of the column storing the full entry
   Column_Entry  : constant Glib.Gint := 0;

   -- Index of the column storing the marked flag
   Column_Marked : constant Glib.Gint := 1;


   -- Packages --

   package Entry_Pointers is new Gnatcoll.Refcount.Shared_Pointers
     (Directory_Entries.Directory_Entry);

   package Conversions is new System.Address_To_Access_Conversions
     (Entry_Pointers.Ref);

   procedure Free is new Ada.Unchecked_Deallocation
     (Entry_Pointers.Ref, Conversions.Object_Pointer);


   -- List Store Model Reference --

   procedure Adjust (Ref : in out Model_Ref) is
   begin
      if Ref.Model /= null then
         Ref.Model.Ref;
      end if;
   end Adjust;

   procedure Finalize (Ref : in out Model_Ref) is
   begin
      if Ref.Model /= null then
         Ref.Model.Unref;
      end if;
   end Finalize;

   procedure Set (Ref : in out Model_Ref; Model : in List_Store) is
   begin
      Ref := (Ada.Finalization.Controlled with Model => Model);
   end Set;

   function Get (Ref : in Model_Ref) return List_Store is
     (Ref.Model);


   -- Column Types --

   --
   -- Boxed Directory_Entry Type
   --
   Entry_Type : Glib.Gtype;

   String_Ref_Type : Glib.Gtype;

   function Column_Types return Glib.Gtype_Array is
      Col_Types : Glib.Gtype_Array := File_Columns.Column_Types;
      Types     : Glib.Gtype_Array (0 .. Glib.Guint(Column_Start) + Col_Types'Last);

   begin
      Types(Glib.Guint(Column_Entry))  := Entry_Type;
      Types(Glib.Guint(Column_Marked)) := Glib.Gtype_Boolean;

      Types(Glib.Guint(Column_Start) .. Types'Last) := (others => String_Ref_Type);

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


   -- Cached Formatted Strings --

   --
   -- Cached_String
   --
   --  Stores a formatted string which is displayed to the user in a
   --  column.
   --
   type Cached_String is record
      Is_Set : Boolean;                                -- Flag if the value is set
      Value  : Ada.Strings.Unbounded.Unbounded_String; -- Formatted String Value
   end record;

   package String_Pointers is new Gnatcoll.Refcount.Shared_Pointers (Cached_String);

   package String_Conversions is new System.Address_To_Access_Conversions
     (String_Pointers.Ref);

   procedure Free is new Ada.Unchecked_Deallocation
     (String_Pointers.Ref, String_Conversions.Object_Pointer);


   --
   -- Copy_String_ref
   --
   --  Copy a boxed Cached_String shared pointer.
   --
   function Copy_String_Ref (Src : System.Address) return System.Address;
   pragma Convention (C, Copy_String_Ref);

   function Copy_String_Ref (Src : System.Address) return System.Address is
      Dest_Ref : String_Conversions.Object_Pointer := new String_Pointers.Ref;
      Src_Ref  : String_Conversions.Object_Pointer := String_Conversions.To_Pointer(Src);

   begin
      Dest_Ref.all := Src_Ref.all;
      return String_Conversions.To_Address(Dest_Ref);

   end Copy_String_Ref;

   --
   -- Free_Entry
   --
   --  Release a shared pointer to a Cached_String, thus
   --  decrementing the reference count.
   --
   procedure Free_String_Ref (Ptr : System.Address);
   pragma Convention (C, Free_String_Ref);

   procedure Free_String_Ref (Ptr : System.Address) is
      Ref : String_Conversions.Object_Pointer :=
        String_Conversions.To_Pointer(Ptr);

   begin
      Free(Ref);
   end Free_String_Ref;

   --
   -- Box_String_Ref
   --
   --  Copy a Cached_String to the heap and create a boxed shared
   --  pointer to it.
   --
   procedure Box_String_Ref (Value : in out Glib.Values.Gvalue) is
      Str : Cached_String := (Is_Set => False, others => <>);
      Ref : String_Pointers.Ref;

   begin
      Ref.Set(Str);

      Glib.Values.Init(Value, String_Ref_Type);
      Glib.Values.Set_Boxed(Value, Ref'Address);

   end Box_String_Ref;


   -- Setting Row Values --

   procedure Set_Values (Model     : in Gtk.List_Store.Gtk_List_Store;
                         Row       : in Gtk.Tree_Model.Gtk_Tree_Iter;
                         Dir_Entry : in Directory_Entries.Directory_Entry) is

      use type Glib.Gint;

      Value : Glib.Values.Gvalue;

   begin
      Box_Entry(Dir_Entry, Value);

      Model.Set_Value(Row, Column_Entry, Value);
      Model.Set(Row, Column_Marked, False);

      Glib.Values.Unset(Value);

      for I in 0 .. File_Columns.Num_Columns - 1 loop
         declare
            Value : Glib.Values.Gvalue;

         begin
            Box_String_Ref(Value);

            Model.Set_Value(Row, Column_Start + Glib.Gint(I), Value);
            Glib.Values.Unset(Value);
            null;
         end;
      end loop;


   end Set_Values;


   -- Getting Row Values --

   function Get_Entry (Model : Tree_Model;
                       Row   : Row_Iter)
                      return Entry_Ref is

      Value : Glib.Values.Gvalue;
      Ref   : Entry_Pointers.Ref;

   begin
      Model.Get_Value(Row, Column_Entry, Value);
      Ref := Conversions.To_Pointer(Glib.Values.Get_Boxed(Value)).all;

      Glib.Values.Unset(Value);

      return (Ent => Ref.Get.Element);

   end Get_Entry;

   function Get_Entry (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter)
                      return Entry_Ref is
     (Get_Entry(-Model, Row));


   -- Formatted Column Strings --

   --
   -- Get_Cached_String
   --
   --   Retrieve a boxed cached string from column Index of a row in a
   --   tree model.
   --
   function Get_Cached_String (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                               Row : Row_Iter;
                               Index : Glib.Gint)
                              return String_Pointers.Reference_Type is
      Value : Glib.Values.Gvalue;
      Ref : String_Pointers.Ref;

   begin
      Gtk.Tree_Model.Get_Value(Model, Row, Index, Value);
      Ref := String_Conversions.To_Pointer(Glib.Values.Get_Boxed(Value)).all;

      Glib.Values.Unset(Value);

      return Ref.Get;
   end Get_Cached_String;


   function Has_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter;
                       Index : Glib.gint)
                      return Boolean is
      Ref : String_Pointers.Reference_Type :=
        Get_Cached_String(Model, Row, Index);

   begin
      return Ref.Is_Set;

   end Has_Field;


   function Get_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter;
                       Index : Glib.gint)
                      return String is
      Ref : String_Pointers.Reference_Type :=
        Get_Cached_String(Model, Row, Index);

      pragma Assert (Ref.Is_Set);
   begin
      return Ada.Strings.Unbounded.To_String(Ref.Value);

   end Get_Field;


   procedure Set_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                        Row : Row_Iter;
                        Index : Glib.Gint;
                        Value : String) is
      Ref : String_Pointers.Reference_Type :=
        Get_Cached_String(Model, Row, Index);

   begin
      Ref.Is_Set := True;
      Ref.Value := Ada.Strings.Unbounded.To_Unbounded_String(Value);
   end Set_Field;



   -- Marking --

   function Is_Marked (Model : in Tree_Model; Row : in Row_Iter) return Boolean is
   begin
      return Model.Get_Boolean(Row, Column_Marked);
   end Is_Marked;


   procedure Set_Marked (Model : in Tree_Model; Row : in Row_Iter; Marked : in Boolean) is
   begin
      Model.Set(Row, Column_Marked, Marked);
   end Set_Marked;


begin

   -- Initialize boxed Directory_Entry GType --

   Entry_Type := Glib.Boxed_Type_Register_Static
     ("Directory_entry", Copy_Entry'Access, Free_Entry'Access);

   String_Ref_Type := Glib.Boxed_Type_Register_Static
     ("Col_String_Reference", Copy_String_Ref'Access, Free_String_Ref'Access);

end File_Model_Columns;
