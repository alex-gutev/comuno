--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paths.Path_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Test_Util;
with Paths.String_Paths;

--  begin read only
--  end read only
package body Paths.Path_Type_Test_Data.Path_Type_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Components (Gnattest_T : in out Test_Path_Type);
   procedure Test_Components_d49c4e (Gnattest_T : in out Test_Path_Type) renames Test_1_Components;
--  id:2.2/d49c4ec9b667b813/Components/1/0/
   procedure Test_1_Components (Gnattest_T : in out Test_Path_Type) is
   --  paths.ads:107:4:Components
--  end read only

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test (Path : String; Directory : Boolean; Result : Component_Array) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path, Directory);

         Test_Util.Assert
           (Gnattest_T.Fixture.Components = Result, "Components not equal.");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test
        ("/foo/bar/baz", False,
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo/bar/baz/", True,
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo/bar/baz//", True,
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo//bar/baz//", True,
           (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("//foo/bar/baz/", True,
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("foo/bar/baz/", True,
         (U("foo"), U("bar"), U("baz")));


      Test("foo", False, (1 => U("foo")));

      Test("", False, Empty);

      Test("/", True, (1 => U("/")));

      Test("///", True, (1 => U("/")));

--  begin read only
   end Test_1_Components;
--  end read only


--  begin read only
   procedure Test_Filename (Gnattest_T : in out Test_Path_Type);
   procedure Test_Filename_b0bb2c (Gnattest_T : in out Test_Path_Type) renames Test_Filename;
--  id:2.2/b0bb2c8ae69ed161/Filename/1/0/
   procedure Test_Filename (Gnattest_T : in out Test_Path_Type) is
   --  paths.ads:129:4:Filename
--  end read only

      procedure Test (Path : String; Directory : Boolean; Expected : String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path, Directory);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Filename, expected,
            "Filename(""" & Path & """)");
      end Test;

   begin

      Test("hello.txt", False, "hello");
      Test("dir.ext/hello.txt", False, "hello");
      Test("no_extension", False, "no_extension");
      Test(".config", False, ".config");
      Test("/dir/.config", False, ".config");
      Test("file.", False, "file.");
      Test("hello.txt.gz", False, "hello.txt");

      Test("/foo/bar/baz/", True, "");
      Test("foo/", True, "");
      Test("/foo", False, "foo");
      Test("///foo", False, "foo");
      Test("/", True, "");
      Test("//", True, "");

      Test("", False, "");

--  begin read only
   end Test_Filename;
--  end read only


--  begin read only
   procedure Test_Extension (Gnattest_T : in out Test_Path_Type);
   procedure Test_Extension_62444f (Gnattest_T : in out Test_Path_Type) renames Test_Extension;
--  id:2.2/62444f115271250c/Extension/1/0/
   procedure Test_Extension (Gnattest_T : in out Test_Path_Type) is
   --  paths.ads:139:4:Extension
--  end read only

      procedure Test (Path : String; Directory : Boolean; Expected : String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path, Directory);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Extension, expected,
            "Extension(""" & Path & """)");
      end Test;

   begin

      Test("/foo/bar/baz.txt", False, "txt");
      Test("/foo/bar/baz", False, "");
      Test("/foo/bar/baz/", True, "");
      Test("hello.txt", False, "txt");
      Test("dir.ext/hello.txt", False, "txt");
      Test("no_extension", False, "");
      Test(".config", False, "");
      Test("/dir/.config", False, "");
      Test("file.", False, "");
      Test("hello.txt.gz", False, "gz");
      Test("", False, "");

      Test("/foo/bar/baz.x/", True, "");
      Test("foo.x/", True, "");
      Test("/foo.x", False, "x");
      Test("///foo.x", False, "x");
      Test("/", True, "");
      Test("//", True, "");

      Test("", False, "");

--  begin read only
   end Test_Extension;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Paths.Path_Type_Test_Data.Path_Type_Tests;
