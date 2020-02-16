--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Calendar;
with Ada.Strings.Utf_Encoding;
with Ada.Strings.Utf_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Characters.Handling;

with File_Model_Columns;
with Directory_Entries; use Directory_Entries;
with File_System;

package body Sort_Functions is

   use type Ada.Calendar.Time;
   use type File_System.File_Type;
   use type File_System.Size_Type;

   function String_Compare (A, B : String) return Order is
      use Ada.Strings.Utf_Encoding;
      use Ada.Strings.Utf_Encoding.Wide_Wide_Strings;
      use Ada.Wide_Wide_Characters.Handling;

      Wide_A : Wide_Wide_String := Decode(A, Utf_8);
      Wide_B : Wide_Wide_String := Decode(B, Utf_8);

      -- Lower bound of strings is documented to be one

   begin
      for I in 1 .. Positive'Min(Wide_A'Last, Wide_B'Last) loop
         declare
            Char_A : Wide_Wide_Character := To_Lower(Wide_A(I));
            Char_B : Wide_Wide_Character := To_Lower(Wide_B(I));

         begin
            if Char_A < Char_B then
               return Order_Less;

            elsif Char_A > Char_B then
               return Order_Greater;

            end if;
         end;
      end loop;

      return (if Wide_A'Length < Wide_B'Length then Order_Less
              elsif Wide_A'Length > Wide_B'Length then Order_Greater
              else Order_Equal);

   end String_Compare;


   -- Sort Functions --

   function Sort_Entry_Type (Model : Tree_Model; A, B : Iter) return Order is
      Ent_A : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, A);

      Ent_B : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, B);

      Type_A : File_System.File_Type := Kind(Ent_A);
      Type_B : File_System.File_Type := Kind(Ent_B);

   begin

      if Type_A = File_System.Parent then
         return Order_Less;

      elsif Type_B = File_System.Parent then
         return Order_Greater;

      elsif Type_A = File_System.Directory and
        Type_B /= File_System.Directory then
         return Order_Less;

      elsif Type_B = File_System.Directory and
        Type_A /= File_System.Directory then
         return Order_Greater;

      else
         return Order_Equal;

      end if;

   end Sort_Entry_Type;


   function Sort_Name (Model : Tree_Model; A, B : Iter) return Order is
      Ent_A : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, A);

      Ent_B : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, B);

   begin
      return String_Compare(Subpath(Ent_A).Basename,
                            Subpath(Ent_B).Basename);
   end Sort_Name;


   function Sort_Size (Model : Tree_Model; A, B : Iter) return Order is
      Ent_A : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, A);

      Ent_B : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, B);

      Size_A : File_System.Size_Type :=
        Attributes(Ent_A).Size;

      Size_B : File_System.Size_Type :=
        Attributes(Ent_B).Size;

   begin
      return (if Size_A > Size_B then Order_Greater
              elsif Size_A < Size_B then Order_Less
              else Order_Equal);

   end Sort_Size;


   function Sort_Modified_Time (Model : Tree_Model; A, B : Iter) return Order is
      Ent_A : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, A);

      Ent_B : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, B);

      Time_A : Ada.Calendar.Time :=
        Attributes(Ent_A).Modification_Time;

      Time_B : Ada.Calendar.Time :=
        Attributes(Ent_B).Modification_Time;

   begin
      return (if Time_A > Time_B then Order_Greater
              elsif Time_A < Time_B then Order_Less
              else Order_Equal);

   end Sort_Modified_Time;


   function Sort_Extension (Model : Tree_Model; A, B : Iter) return Order is
      Ent_A : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, A);

      Ent_B : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(Model, B);

   begin
      return String_Compare(Subpath(Ent_A).Extension,
                            Subpath(Ent_B).Extension);

   end Sort_Extension;


   function Sort_Combined (Model : Tree_Model; A, B : Iter) return Order is
      Order1 : Order := Sort1(Model, A, B);

   begin
      return (if Order1 /= Order_Equal then Order1
              else Sort2(Model, A, B));

   end Sort_Combined;

   function Sort_Inverted (Model : Tree_Model; A, B : Iter) return Order is
     (Sort(Model, B, A));

end Sort_Functions;
