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
with Ada.Containers.Indefinite_Hashed_Maps;
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


   function Hash (Int : Glib.Gint) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(Int));

   package Field_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Glib.Gint,
      Element_Type => String,
      Hash => Hash,
      Equivalent_Keys => Glib."=");


   --
   -- Entry_Data
   --
   --  Data associated with a tree model row.
   --
   --  Stores the directory entry and a map storing cached formatted
   --  strings, which are displayed to the user.
   --
   type Entry_Data is record
      Dir_Entry : aliased Directory_Entries.Directory_Entry; -- Directory Entry
      Fields    : Field_Maps.Map;                            -- Cached column formatted strings
   end record;


   -- Packages --

   package Entry_Pointers is new Gnatcoll.Refcount.Shared_Pointers (Entry_Data);

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

   function Column_Types return Glib.Gtype_Array is
      Col_Types : Glib.Gtype_Array := File_Columns.Column_Types;
      Types     : Glib.Gtype_Array (0 .. Glib.Guint(Column_Start) + Col_Types'Last);

   begin
      Types(Glib.Guint(Column_Entry))  := Entry_Type;
      Types(Glib.Guint(Column_Marked)) := Glib.Gtype_Boolean;

      Types(Glib.Guint(Column_Start) .. Types'Last) := Col_Types;

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
      Box : Entry_Data := (Dir_Entry => Dir_Entry, others => <>);
      Ref : Entry_Pointers.Ref;

   begin
      Ref.Set(Box);

      Glib.Values.Init(Value, Entry_Type);
      Glib.Values.Set_Boxed(Value, Ref'Address);

   end Box_Entry;


   -- Setting Row Values --

   procedure Set_Values (Model     : in Gtk.List_Store.Gtk_List_Store;
                         Row       : in Gtk.Tree_Model.Gtk_Tree_Iter;
                         Dir_Entry : in Directory_Entries.Directory_Entry) is

      Value : Glib.Values.Gvalue;

   begin
      Box_Entry(Dir_Entry, Value);

      Model.Set_Value(Row, Column_Entry, Value);
      Model.Set(Row, Column_Marked, False);

      Glib.Values.Unset(Value);

   end Set_Values;


   -- Getting Row Values --

   --
   -- Get_Entry_Data
   --
   --  Retrieve the Entry_Data record associated with a given row.
   --
   function Get_Entry_Data (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                           Row : Row_Iter)
                          return Entry_Pointers.Reference_Type is

      Value : Glib.Values.Gvalue;
      Ref   : Entry_Pointers.Ref;

   begin
      Gtk.Tree_Model.Get_Value(Model, Row, Column_Entry, Value);
      Ref := Conversions.To_Pointer(Glib.Values.Get_Boxed(Value)).all;

      Glib.Values.Unset(Value);

      return Ref.Get;
   end Get_Entry_Data;


   function Get_Entry (Model : Tree_Model;
                       Row   : Row_Iter)
                      return Entry_Ref is

      Ref   : Entry_Pointers.Reference_Type :=
        Get_Entry_Data(Gtk.List_Store."+"(Model), Row);

   begin
      return (Ent => Ref.Element.Dir_Entry'Access);

   end Get_Entry;

   function Get_Entry (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter)
                      return Entry_Ref is
     (Get_Entry(-Model, Row));


   -- Formatted Column Strings --

   function Has_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter;
                       Index : Glib.gint)
                      return Boolean is
      Ref : Entry_Pointers.Reference_Type :=
        Get_Entry_Data(Model, Row);

   begin
      return Field_Maps.Has_Element(Ref.Fields.Find(Index));

   end Has_Field;


   function Get_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter;
                       Index : Glib.gint)
                      return String is
      Ref : Entry_Pointers.Reference_Type :=
        Get_Entry_Data(Model, Row);

   begin
      return Ref.Fields.Element(Index);

   end Get_Field;


   procedure Set_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                        Row : Row_Iter;
                        Index : Glib.Gint;
                        Value : String) is
      Ref : Entry_Pointers.Reference_Type := Get_Entry_Data(Model, Row);

   begin
      Ref.Fields.Insert(Index, Value);
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

end File_Model_Columns;
