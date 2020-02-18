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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with File_Model_Columns;
with File_Columns.Name_Columns;

package body File_Columns is

   use type Ada.Containers.Count_Type;
   use type Glib.Gint;


   -- Builtin Column Map --

   package Column_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Column'Class,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   --
   -- Map from column identifiers to Column descriptor objects.
   --
   Column_Map : Column_Maps.Map;

   --
   -- Vector storing all columns in the order in which they are added
   -- to the Tree Model.
   --
   Column_Vector : Column_Vectors.Vector;


   function Get_Column (Name : String) return Column_Ptr is
      Cursor : Column_Maps.Cursor := Column_Map.Find(Name);

   begin
      return (if Column_Maps.Has_Element(Cursor) then
                 Column_Map.Reference(Cursor).Element else
                 null);
   end Get_Column;

   function Get_Column (Index : Natural) return Column_Ptr is
     (if Ada.Containers.Count_Type(Index) < Column_Vector.Length then
         Column_Vector(Index)
      else null);


   function All_Columns return Column_Vectors.Vector is
     (Column_Vector);

   function Num_Columns return Natural is
     (Natural(Column_Vector.Length));

   function Column_Types return Glib.Gtype_Array is
      Types : Glib.Gtype_Array(0 .. Glib.Guint(Column_Vector.Last_Index));

   begin
      for I in Types'Range loop
         declare
            Col : Column_Ptr := Column_Vector(Natural(I));

         begin
            Types(I) := Col.Column_Type;

         end;
      end loop;

      return Types;

   end Column_Types;



   procedure Append_Column (View : Tree_View; Col : Tree_View_Column) is
      Dummy : Glib.Gint;

   begin
      Dummy := View.Append_Column(Col);
   end Append_Column;

   function Get_Index (Col : Column'Class) return Glib.Gint is
      (Col.Index);


   -- Adding Columns --

   --
   -- Add_Column
   --
   --  Add a column with a given identifier to Column_Map and
   --  Column_Vector.
   --
   procedure Add_Column (Name : in String; Col : in Column'Class) is
      Pos : Column_Maps.Cursor;
      Dummy : Boolean;
   begin
      Column_Map.Insert(Name, Col, Pos, Dummy);
      Column_Vector.Append(Column_Map.Reference(Pos).Element);
   end Add_Column;


begin

   -- Add Builtin Columns --

Add_Name_Column:
   declare
      Col : Name_Columns.Name_Column;

   begin
      Column(Col).Index := File_Model_Columns.Column_Start + Glib.Gint(Column_Map.Length);
      Add_Column("name", Col);
   end Add_Name_Column;

end File_Columns;
