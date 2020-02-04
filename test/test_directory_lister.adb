--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Unbounded;

with Aunit.Assertions; use Aunit.Assertions;
with Aunit.Test_Caller;
with System.Assertions;

with Test_Util;

with File_System;
with Listers;
with Directory_Listers;

package body Test_Directory_Lister is

   -- Test Routines --

   --
   -- Test_Simple
   --
   --  Test listing a simple directory with no exceptional conditions.
   --
   --  Checks that the correct files are listed and that the entry
   --  types match.
   --
   procedure Test_Simple (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      use Ada.Strings.Unbounded;

      use type File_System.File_Type;

      procedure Assert_Equal_Type is
        new Test_Util.Assert_Equal (File_System.File_Type, File_System.File_Type'Image);

      function U (S : String) return Unbounded_String renames
        To_Unbounded_String;

      Lister : Directory_Listers.Directory_Lister;

      Entries : array (Positive range <>) of Listers.Dir_Entry :=
        ((Name => U("a-folder"), Kind => File_System.Directory),
         (Name => U("a-file.txt"), Kind => File_System.Regular),
         (Name => U("a-script.sh"), Kind => File_System.Regular));

      Visited : array (Positive range Entries'Range) of Boolean := (others => False);

      function Entry_Key (Ent : Listers.Dir_Entry) return Positive is
      begin
         for Index in Entries'Range loop
            if Ent.Name = Entries(Index).Name then
               return Index;
            end if;
         end loop;

         Assert(False, "Read unexpected entry: '" & To_String(Ent.Name) & "'");
      end Entry_Key;

   begin

      Lister.Open("test/inputs/directory_lister/test1");

      declare
         Ent   : Listers.Dir_Entry;
         Index : Positive;

      begin
         while Lister.Read_Entry(Ent) loop
            Index := Entry_Key(Ent);

            Assert_Equal_Type
              (Ent.Kind, Entries(Index).Kind,
               "Kind('" & To_String(Ent.Name) & "')");

            Visited(Index) := True;
         end loop;
      end;

      for Index in Entries'Range loop
         Test_Util.Assert
           (Visited(Index),
            "Entry: '" & To_String(Entries(Index).Name) & "' not visited");
      end loop;

   end Test_Simple;

   --
   -- Test_Non_Existent_Dir
   --
   --  Test that attempting to listing a non-existent directory
   --  results in an exception being raised.
   --
   procedure Test_Non_Existent_Dir (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Lister : Directory_Listers.Directory_Lister;

   begin

      begin
         Lister.Open("test/inputs/directory_lister/nonexistent");
         Assert(False, "Exception not raised when attempting to open nonexistent directory");

      exception
         when Listers.Open_Directory_Error => null;
      end;

   end Test_Non_Existent_Dir;

   --
   -- Test_List_File
   --
   --  Test that attempting to open a regular file for listing results
   --  in an exception being raised.
   --
   procedure Test_List_File (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Lister : Directory_Listers.Directory_Lister;

   begin

      begin
         Lister.Open("test/inputs/directory_lister/test1/a-file.txt");
         Assert(False, "Exception not raised when attempting to list a regular file");

      exception
         when Listers.Open_Directory_Error => null;
      end;

   end Test_List_File;


   -- Test Registration and Suit Boilerplate

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin

      Register_Routine (T, Test_Simple'Access, "Test Simple Listing");
      Register_Routine (T, Test_Non_Existent_Dir'Access, "Test Opening Non-Existent Directory");
      Register_Routine (T, Test_List_File'Access, "Test Listing Regular File");

   end Register_Tests;

   function Name (T : Test) return Message_String is
   begin
      return Format ("Directory_Lister Tests");
   end Name;

   function Suite return Aunit.Test_Suites.Access_Test_Suite is
      Ret : AUnit.Test_Suites.Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Ret.Add_Test(new Test);

      return Ret;
   end Suite;

end Test_Directory_Lister;
