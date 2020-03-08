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
with Ada.Strings.Unbounded;

with File_Columns;
with Boxed_Refcounted;

package body File_Model_Columns is

   use type Glib.Guint;
   use type Gtk.List_Store.Gtk_List_Store;

   -- Index of the column storing the full entry
   Column_Entry  : constant Glib.Gint := 0;

   -- Index of the column storing the marked flag
   Column_Marked : constant Glib.Gint := 1;


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


   -- Packages --

   package Boxed_Entry is new Boxed_Refcounted
     (Directory_Entries.Directory_Entry,
      "Directory_Entry");

   package Boxed_Cached_String is new Boxed_Refcounted
     (Cached_String,
      "Cached_Formatted_String");

   function Cached_String_Type return Glib.Gtype is
     (Boxed_Cached_String.Boxed_Type);


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

   function Column_Types return Glib.Gtype_Array is
      Col_Types : Glib.Gtype_Array := File_Columns.Column_Types;
      Types     : Glib.Gtype_Array (0 .. Glib.Guint(Column_Start) + Col_Types'Last);

   begin
      Types(Glib.Guint(Column_Entry))  := Boxed_Entry.Boxed_Type;
      Types(Glib.Guint(Column_Marked)) := Glib.Gtype_Boolean;

      Types(Glib.Guint(Column_Start) .. Types'Last) := Col_Types;

      return Types;
   end Column_Types;


   -- Setting Row Values --

   procedure Set_Values (Model     : in Gtk.List_Store.Gtk_List_Store;
                         Row       : in Gtk.Tree_Model.Gtk_Tree_Iter;
                         Dir_Entry : in Directory_Entries.Directory_Entry) is

      use type Glib.Gint;

      Value : Glib.Values.Gvalue;

   begin
      Boxed_Entry.Box(Dir_Entry, Value);

      Model.Set_Value(Row, Column_Entry, Value);
      Model.Set(Row, Column_Marked, False);

      Glib.Values.Unset(Value);

      for Column of File_Columns.All_Columns loop
         Column.Set_Data(Model, Row, Dir_Entry);
      end loop;

   end Set_Values;


   -- Getting Row Values --

   function Get_Entry (Model : Tree_Model;
                       Row   : Row_Iter)
                      return Entry_Ref is

     (Get_Entry(Gtk.List_Store."+"(Model), Row));

   function Get_Entry (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row : Row_Iter)
                      return Entry_Ref is

     (Ent => Boxed_Entry.Get_Data(Model, Row, Column_Entry).Element);



   -- Formatted Column Strings --

   procedure Set_Cached_String (Model : List_Store;
                                Row   : Row_Iter;
                                Index : Glib.Gint) is

      Value : Glib.Values.Gvalue;

   begin
      Boxed_Cached_String.Box((Is_Set => False, others => <>), Value);

      Model.Set_Value(Row, Index, Value);
      Glib.Values.Unset(Value);

   end Set_Cached_String;



   function Has_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row   : Row_Iter;
                       Index : Glib.gint)
                      return Boolean is

      Ref : Boxed_Cached_String.Reference :=
        Boxed_Cached_String.Get_Data(Model, Row, Index);

   begin
      return Ref.Is_Set;

   end Has_Field;


   function Get_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                       Row   : Row_Iter;
                       Index : Glib.gint)
                      return String is

      Ref : Boxed_Cached_String.Reference :=
        Boxed_Cached_String.Get_Data(Model, Row, Index);

      pragma Assert (Ref.Is_Set);
   begin
      return Ada.Strings.Unbounded.To_String(Ref.Value);

   end Get_Field;


   procedure Set_Field (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                        Row   : Row_Iter;
                        Index : Glib.Gint;
                        Value : String) is

      Ref : Boxed_Cached_String.Reference :=
        Boxed_Cached_String.Get_Data(Model, Row, Index);

   begin
      Ref.Is_Set := True;
      Ref.Value := Ada.Strings.Unbounded.To_Unbounded_String(Value);
   end Set_Field;



   -- Marking --

   function Is_Marked (Model : in Gtk.Tree_Model.Gtk_Tree_Model; Row : in Row_Iter) return Boolean is
   begin
      return Gtk.Tree_Model.Get_Boolean(Model, Row, Column_Marked);
   end Is_Marked;


   procedure Set_Marked (Model : in Tree_Model; Row : in Row_Iter; Marked : in Boolean) is
   begin
      Model.Set(Row, Column_Marked, Marked);
   end Set_Marked;


   -- Utilities --

   function Get_Row_Index (Model : in Gtk.Tree_Model.Gtk_Tree_Model; Row : in Row_Iter) return Glib.Gint is
      Path  : Gtk.Tree_Model.Gtk_Tree_Path := Gtk.Tree_Model.Get_Path(Model, Row);
      Index : Glib.Gint                    := Path.Get_Indices(0);

   begin
      Gtk.Tree_Model.Path_Free(Path);

      return Index;
   end Get_Row_Index;

end File_Model_Columns;
