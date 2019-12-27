--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paths.Path_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Test_Util;

--  begin read only
--  end read only
package body Paths.Path_Test_Data.Path_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Make_Path (Gnattest_T : in out Test_Path);
   procedure Test_Make_Path_ef9d4a (Gnattest_T : in out Test_Path) renames Test_1_Make_Path;
--  id:2.2/ef9d4a6d2cc4d874/Make_Path/1/0/
   procedure Test_1_Make_Path (Gnattest_T : in out Test_Path) is
   --  paths.ads:74:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Test_Util.Assert_Equal_String
        (To_String(Make_Path("/usr/local/foo")), "/usr/local/foo",
         "Make_Path(""/usr/local/foo"")");

--  begin read only
   end Test_1_Make_Path;
--  end read only


--  begin read only
   procedure Test_2_Make_Path (Gnattest_T : in out Test_Path);
   procedure Test_Make_Path_356523 (Gnattest_T : in out Test_Path) renames Test_2_Make_Path;
--  id:2.2/356523d7d0a11021/Make_Path/0/0/
   procedure Test_2_Make_Path (Gnattest_T : in out Test_Path) is
   --  paths.ads:75:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      P: Path := Make_Path(Ada.Strings.Unbounded.To_Unbounded_String("/usr/local/foo"));

   begin

      Test_Util.Assert_Equal_String
        (To_String(P), "/usr/local/foo",
         "Make_Path(To_Unbounded_string(""/usr/local/foo""))");

--  begin read only
   end Test_2_Make_Path;
--  end read only


--  begin read only
   procedure Test_3_Make_Path (Gnattest_T : in out Test_Path);
   procedure Test_Make_Path_31762a (Gnattest_T : in out Test_Path) renames Test_3_Make_Path;
--  id:2.2/31762a936940a635/Make_Path/0/0/
   procedure Test_3_Make_Path (Gnattest_T : in out Test_Path) is
   --  paths.ads:95:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path : String; Directory : Boolean; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (To_String(Make_Path(Path, Directory)), Expected,
            "Make_Path(""" & Path & ", " & Directory'Image & ")");
      end Test;

   begin

      Test("/usr/local/bin", True, "/usr/local/bin/");
      Test("/usr/local/bin", False, "/usr/local/bin");
      Test("/usr/local/bin/", True, "/usr/local/bin/");
      Test("/usr/local/bin/", False, "/usr/local/bin");

      Test("/", True, "/");
      Test("/", False, "/");

      Test("", True, "");
      Test("", False, "");

--  begin read only
   end Test_3_Make_Path;
--  end read only


--  begin read only
   procedure Test_4_Make_Path (Gnattest_T : in out Test_Path);
   procedure Test_Make_Path_ecaa4f (Gnattest_T : in out Test_Path) renames Test_4_Make_Path;
--  id:2.2/ecaa4f41d70dc5e1/Make_Path/0/0/
   procedure Test_4_Make_Path (Gnattest_T : in out Test_Path) is
   --  paths.ads:96:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      package Unbounded renames Ada.Strings.Unbounded;

      procedure Test (Path : String; Directory : Boolean; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (To_String(Make_Path(Unbounded.To_Unbounded_String(Path), Directory)), Expected,
            "Make_Path(""" & Path & ", " & Directory'Image & ")");
      end Test;

   begin

      Test("/usr/local/bin", True, "/usr/local/bin/");
      Test("/usr/local/bin", False, "/usr/local/bin");
      Test("/usr/local/bin/", True, "/usr/local/bin/");
      Test("/usr/local/bin/", False, "/usr/local/bin");

      Test("/", True, "/");
      Test("/", False, "/");

      Test("", True, "");
      Test("", False, "");

--  begin read only
   end Test_4_Make_Path;
--  end read only


--  begin read only
   procedure Test_5_Make_Path (Gnattest_T : in out Test_Path);
   procedure Test_Make_Path_f46040 (Gnattest_T : in out Test_Path) renames Test_5_Make_Path;
--  id:2.2/f46040cce201e5d6/Make_Path/0/0/
   procedure Test_5_Make_Path (Gnattest_T : in out Test_Path) is
   --  paths.ads:107:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      function Unbounded (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      Components1 : Component_Array :=
        (Unbounded("foo"),
         Unbounded("bar"),
         Unbounded("baz"));

      Components2 : Component_Array :=
        (Unbounded("/"),
         Unbounded("foo"),
         Unbounded("bar"),
         Unbounded("baz"));

      Empty_Components : Component_Array(1 .. 0);

   begin

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Components1, False)), "foo/bar/baz",
         "Make_Path((""foo"", ""bar"", ""baz""), False)");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Components1, True)), "foo/bar/baz/",
         "Make_Path((""foo"", ""bar"", ""baz""), True)");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Components2)), "/foo/bar/baz",
         "Make_Path((""/"", ""foo"", ""bar"", ""baz""))");


      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Empty_Components)), "",
         "Make_Path(())");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Empty_Components, True)), "",
         "Make_Path(())");


      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Component_Array'(1 => Unbounded("/")))), "/",
         "Make_Path((/))");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Component_Array'(1 => Unbounded("/")), True)), "/",
         "Make_Path((/))");

--  begin read only
   end Test_5_Make_Path;
--  end read only


--  begin read only
   procedure Test_6_Make_Path (Gnattest_T : in out Test_Path);
   procedure Test_Make_Path_8e2b50 (Gnattest_T : in out Test_Path) renames Test_6_Make_Path;
--  id:2.2/8e2b504bee2a9cfc/Make_Path/0/0/
   procedure Test_6_Make_Path (Gnattest_T : in out Test_Path) is
   --  paths.ads:108:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Component_Vectors;

      function Unbounded (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      Components1 : Component_Vector :=
        Component_Vector'(Unbounded("foo") & Unbounded("bar")) & Unbounded("baz");

      Components2 : Component_Vector :=
        Component_Vector'(Unbounded("/") & Unbounded("foo")) &
        Unbounded("bar") & Unbounded("baz");

      Root_Components : Component_Vector := To_Vector(Unbounded("/"), 1);
      Empty_Components : Component_Vector;

   begin

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Components1, False)), "foo/bar/baz",
         "Make_Path((""foo"", ""bar"", ""baz""), False)");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Components1, True)), "foo/bar/baz/",
         "Make_Path((""foo"", ""bar"", ""baz""), True)");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Components2)), "/foo/bar/baz",
         "Make_Path((""/"", ""foo"", ""bar"", ""baz""))");


      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Empty_Components)), "",
         "Make_Path(())");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Empty_Components, True)), "",
         "Make_Path(())");


      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Root_Components)), "/",
         "Make_Path((/))");

      Test_Util.Assert_Equal_String
        (To_String(Make_Path(Root_Components, True)), "/",
         "Make_Path((/))");

--  begin read only
   end Test_6_Make_Path;
--  end read only


--  begin read only
   procedure Test_To_String (Gnattest_T : in out Test_Path);
   procedure Test_To_String_05fc01 (Gnattest_T : in out Test_Path) renames Test_To_String;
--  id:2.2/05fc01cb376fbe8b/To_String/1/0/
   procedure Test_To_String (Gnattest_T : in out Test_Path) is
   --  paths.ads:118:4:To_String
--  end read only

      procedure Test (Path : String; Result : String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path, False);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.To_String, Result,
            "To_String(" & Path & ")");
      end Test;

   begin

      Test("/usr/local", "/usr/local");

--  begin read only
   end Test_To_String;
--  end read only


--  begin read only
   procedure Test_1_Components (Gnattest_T : in out Test_Path);
   procedure Test_Components_8df3c0 (Gnattest_T : in out Test_Path) renames Test_1_Components;
--  id:2.2/8df3c0a9419c0d2b/Components/1/0/
   procedure Test_1_Components (Gnattest_T : in out Test_Path) is
   --  paths.ads:129:4:Components
--  end read only

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      use Component_Vectors;

      procedure Test (Path : String; Result : Component_Array) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           (Gnattest_T.Fixture.Components = Vector(Result),
            "Components not equal.");
      end Test;

      Empty : Component_Array(1..0);

   begin

      Test
        ("/foo/bar/baz",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo/bar/baz/",
         (U("/"), U("foo"), U("bar"), U("baz")));


      Test
        ("/foo/bar/baz//",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo//bar/baz//",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("//foo/bar/baz/",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("foo/bar/baz/",
         (U("foo"), U("bar"), U("baz")));

      Test("foo", (1 => U("foo")));

      Test("", Empty);

      Test("/", (1 => U("/")));

      Test("///", (1 => U("/")));

--  begin read only
   end Test_1_Components;
--  end read only


--  begin read only
   procedure Test_2_Components (Gnattest_T : in out Test_Path);
   procedure Test_Components_a82b3f (Gnattest_T : in out Test_Path) renames Test_2_Components;
--  id:2.2/a82b3fab634fac00/Components/0/0/
   procedure Test_2_Components (Gnattest_T : in out Test_Path) is
   --  paths.ads:130:4:Components
--  end read only

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test (Path : String; Result : Component_Array) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           (Gnattest_T.Fixture.Components = Result,
            "Components not equal.");
      end Test;

      Empty : Component_Array(1..0);

   begin

      Test
        ("/foo/bar/baz",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo/bar/baz/",
         (U("/"), U("foo"), U("bar"), U("baz")));


      Test
        ("/foo/bar/baz//",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("/foo//bar/baz//",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("//foo/bar/baz/",
         (U("/"), U("foo"), U("bar"), U("baz")));

      Test
        ("foo/bar/baz/",
         (U("foo"), U("bar"), U("baz")));

      Test("foo", (1 => U("foo")));

      Test("", Empty);

      Test("/", (1 => U("/")));

      Test("///", (1 => U("/")));

--  begin read only
   end Test_2_Components;
--  end read only


--  begin read only
   procedure Test_Basename (Gnattest_T : in out Test_Path);
   procedure Test_Basename_2e0a84 (Gnattest_T : in out Test_Path) renames Test_Basename;
--  id:2.2/2e0a845d0ccf930c/Basename/1/0/
   procedure Test_Basename (Gnattest_T : in out Test_Path) is
   --  paths.ads:141:4:Basename
--  end read only

      procedure Test (Path : String; Expected : String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Basename, expected,
            "Basename(""" & Path & """)");
      end Test;

   begin

      Test("/foo/bar/baz.txt", "baz.txt");
      Test("/foo/bar/baz", "baz");
      Test("foo", "foo");
      Test("/foo", "foo");
      Test("///foo", "foo");

      Test("/foo/bar/baz/", "");
      Test("foo/", "");
      Test("/", "");
      Test("//", "");

      Test("", "");

--  begin read only
   end Test_Basename;
--  end read only


--  begin read only
   procedure Test_Filename (Gnattest_T : in out Test_Path);
   procedure Test_Filename_3b3684 (Gnattest_T : in out Test_Path) renames Test_Filename;
--  id:2.2/3b3684528618fec7/Filename/1/0/
   procedure Test_Filename (Gnattest_T : in out Test_Path) is
   --  paths.ads:152:4:Filename
--  end read only

      procedure Test (Path : String; Expected : String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Filename, expected,
            "Filename(""" & Path & """)");
      end Test;

   begin

      Test("hello.txt", "hello");
      Test("dir.ext/hello.txt", "hello");
      Test("no_extension", "no_extension");
      Test(".config", ".config");
      Test("/dir/.config", ".config");
      Test("file.", "file.");
      Test("hello.txt.gz", "hello.txt");

      Test("/foo/bar/baz/", "");
      Test("foo/", "");
      Test("/foo", "foo");
      Test("///foo", "foo");
      Test("/", "");
      Test("//", "");

      Test("", "");

--  begin read only
   end Test_Filename;
--  end read only


--  begin read only
   procedure Test_Extension (Gnattest_T : in out Test_Path);
   procedure Test_Extension_6a3bad (Gnattest_T : in out Test_Path) renames Test_Extension;
--  id:2.2/6a3badfb41fb22a8/Extension/1/0/
   procedure Test_Extension (Gnattest_T : in out Test_Path) is
   --  paths.ads:162:4:Extension
--  end read only

      procedure Test (Path : String; Expected : String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Extension, expected,
            "Extension(""" & Path & """)");
      end Test;

   begin

      Test("/foo/bar/baz.txt", "txt");
      Test("/foo/bar/baz", "");
      Test("/foo/bar/baz/", "");
      Test("hello.txt", "txt");
      Test("dir.ext/hello.txt", "txt");
      Test("no_extension", "");
      Test(".config", "");
      Test("/dir/.config", "");
      Test("file.", "");
      Test("hello.txt.gz", "gz");
      Test("", "");

      Test("/foo/bar/baz.x/", "");
      Test("foo.x/", "");
      Test("/foo.x", "x");
      Test("///foo.x", "x");
      Test("/", "");
      Test("//", "");

      Test("", "");

--  begin read only
   end Test_Extension;
--  end read only


--  begin read only
   procedure Test_Is_Directory (Gnattest_T : in out Test_Path);
   procedure Test_Is_Directory_a100ba (Gnattest_T : in out Test_Path) renames Test_Is_Directory;
--  id:2.2/a100ba26a093e9f1/Is_Directory/1/0/
   procedure Test_Is_Directory (Gnattest_T : in out Test_Path) is
   --  paths.ads:180:4:Is_Directory
--  end read only

      procedure Test (Path : Path_String; Expected : Boolean) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           ((Gnattest_T.Fixture.Is_Directory = Expected),
            "Is_Directory(""" & Path & """) /= " & Expected'Image);
      end Test;


   begin

      Test("/foo/bar/baz", False);
      Test("/foo/bar/baz/", True);
      Test("/", True);
      Test("", False);

--  begin read only
   end Test_Is_Directory;
--  end read only


--  begin read only
   procedure Test_Has_Directories (Gnattest_T : in out Test_Path);
   procedure Test_Has_Directories_c13acb (Gnattest_T : in out Test_Path) renames Test_Has_Directories;
--  id:2.2/c13acb12181614b2/Has_Directories/1/0/
   procedure Test_Has_Directories (Gnattest_T : in out Test_Path) is
   --  paths.ads:193:4:Has_Directories
--  end read only

      procedure Test (Path : Path_String; Expected : Boolean) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           ((Gnattest_T.Fixture.Has_Directories = Expected),
            "Has_Directories(""" & Path & """) /= " & Expected'Image);
      end Test;

   begin

      Test("/foo/bar", True);
      Test("foo.txt", False);
      Test("foo/", True);
      Test("/", True);
--      Test(".", True);
      Test("..", True);
      Test("~", True);
      Test("~user", True);
      Test("", False);

--  begin read only
   end Test_Has_Directories;
--  end read only


--  begin read only
   procedure Test_Is_Empty (Gnattest_T : in out Test_Path);
   procedure Test_Is_Empty_c8072f (Gnattest_T : in out Test_Path) renames Test_Is_Empty;
--  id:2.2/c8072fb86563d37d/Is_Empty/1/0/
   procedure Test_Is_Empty (Gnattest_T : in out Test_Path) is
   --  paths.ads:201:4:Is_Empty
--  end read only

      procedure Test (Path : Path_String; Expected : Boolean) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           ((Gnattest_T.Fixture.Is_Empty = Expected),
            "Is_Empty(""" & Path & """) /= " & Expected'Image);
      end Test;

   begin

      Test("", True);
      Test("foo", False);

--  begin read only
   end Test_Is_Empty;
--  end read only


--  begin read only
   procedure Test_Is_Root (Gnattest_T : in out Test_Path);
   procedure Test_Is_Root_a731bb (Gnattest_T : in out Test_Path) renames Test_Is_Root;
--  id:2.2/a731bbb089198278/Is_Root/1/0/
   procedure Test_Is_Root (Gnattest_T : in out Test_Path) is
   --  paths.ads:214:4:Is_Root
--  end read only

      procedure Test (Path : Path_String; Expected : Boolean) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           ((Gnattest_T.Fixture.Is_Root = Expected),
            "Is_Root(""" & Path & """) /= " & Expected'Image);
      end Test;

   begin

      Test("/", True);
      Test("//", True);
      Test("/foo", False);
      Test("foo", False);
      Test("", False);
      Test(".", False);
      Test("./", False);
      Test("../", False);
      Test("~", False);

--  begin read only
   end Test_Is_Root;
--  end read only


--  begin read only
   procedure Test_Is_Relative (Gnattest_T : in out Test_Path);
   procedure Test_Is_Relative_5525cf (Gnattest_T : in out Test_Path) renames Test_Is_Relative;
--  id:2.2/5525cfe63f263748/Is_Relative/1/0/
   procedure Test_Is_Relative (Gnattest_T : in out Test_Path) is
   --  paths.ads:225:4:Is_Relative
--  end read only

      procedure Test (Path : Path_String; Expected : Boolean) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert
           ((Gnattest_T.Fixture.Is_Relative = Expected),
            "Is_Relative(""" & Path & """) /= " & Expected'Image);
      end Test;

   begin

      Test("foo/bar", True);
      Test("foo", True);
      Test("", True);
      Test(".", True);
      Test("..", True);
      Test("./", True);
      Test("../", True);
      Test("/foo/bar", False);
      Test("/", False);
      Test("~", False);
      Test("~user", False);

--  begin read only
   end Test_Is_Relative;
--  end read only


--  begin read only
   procedure Test_1_Ensure_Directory (Gnattest_T : in out Test_Path);
   procedure Test_Ensure_Directory_29d6a7 (Gnattest_T : in out Test_Path) renames Test_1_Ensure_Directory;
--  id:2.2/29d6a79023041e60/Ensure_Directory/1/0/
   procedure Test_1_Ensure_Directory (Gnattest_T : in out Test_Path) is
   --  paths.ads:243:4:Ensure_Directory
--  end read only

      procedure Test (Path : Path_String; Directory : Boolean; Expected : Path_String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Ensure_Directory(Directory).To_String,
            Expected,
            "Ensure_Directory(""" & Path & """, " & Directory'Image & ")");
      end Test;

   begin

      Test("/foo/bar", True, "/foo/bar/");
      Test("/foo/bar", False, "/foo/bar");

      Test("/foo/bar/", True, "/foo/bar/");
      Test("/foo/bar/", False, "/foo/bar");

      Test("/", True, "/");
      Test("/", False, "/");

      Test("", True, "");
      Test("", False, "");

--      Test(".", True, "./");
--      Test(".", False, ".");

--  begin read only
   end Test_1_Ensure_Directory;
--  end read only


--  begin read only
   procedure Test_2_Ensure_Directory (Gnattest_T : in out Test_Path);
   procedure Test_Ensure_Directory_2010e8 (Gnattest_T : in out Test_Path) renames Test_2_Ensure_Directory;
--  id:2.2/2010e8038024919d/Ensure_Directory/0/0/
   procedure Test_2_Ensure_Directory (Gnattest_T : in out Test_Path) is
   --  paths.ads:244:4:Ensure_Directory
--  end read only

      procedure Test (Path : Path_String; Directory : Boolean; Expected : Path_String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Gnattest_T.Fixture.Ensure_Directory(Directory);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.To_String, Expected,
            "Ensure_Directory(""" & Path & """, " & Directory'Image & ")");
      end Test;

   begin

      Test("/foo/bar", True, "/foo/bar/");
      Test("/foo/bar", False, "/foo/bar");

      Test("/foo/bar/", True, "/foo/bar/");
      Test("/foo/bar/", False, "/foo/bar");

      Test("/", True, "/");
      Test("/", False, "/");

      Test("", True, "");
      Test("", False, "");

--      Test(".", True, "./");
--      Test(".", False, ".");

--  begin read only
   end Test_2_Ensure_Directory;
--  end read only


--  begin read only
   procedure Test_1_Append (Gnattest_T : in out Test_Path);
   procedure Test_Append_26bf90 (Gnattest_T : in out Test_Path) renames Test_1_Append;
--  id:2.2/26bf9008adf6766c/Append/1/0/
   procedure Test_1_Append (Gnattest_T : in out Test_Path) is
   --  paths.ads:255:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         Path1 : Path := Make_Path(P1);
         Path2 : Path := Make_Path(P2);

      begin
         Test_Util.Assert_Equal_String
           (Path1.Append(Path2).To_String, Expected,
            "Append(""" & P1 & """, """ & P2 & """)");
      end Test;

   begin

      Test("/foo/bar", "baz", "/foo/bar/baz");
      Test("/foo", "bar/baz", "/foo/bar/baz");
      Test("/foo/", "bar/baz", "/foo/bar/baz");
      Test(".", "foo", "./foo");
      Test("..", "foo", "../foo");
      Test("~", "foo", "~/foo");
      Test("/", "foo", "/foo");

      Test("foo", "", "foo/");
      Test("", "foo", "foo");

--  begin read only
   end Test_1_Append;
--  end read only


--  begin read only
   procedure Test_2_Append (Gnattest_T : in out Test_Path);
   procedure Test_Append_c30aa7 (Gnattest_T : in out Test_Path) renames Test_2_Append;
--  id:2.2/c30aa79582e32e23/Append/0/0/
   procedure Test_2_Append (Gnattest_T : in out Test_Path) is
   --  paths.ads:256:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         Path1 : Path := Make_Path(P1);
         Path2 : Path := Make_Path(P2);

      begin
         Path1.Append(Path2);

         Test_Util.Assert_Equal_String
           (Path1.To_String, Expected,
            "Append(""" & P1 & """, """ & P2 & """)");
      end Test;

   begin

      Test("/foo/bar", "baz", "/foo/bar/baz");
      Test("/foo", "bar/baz", "/foo/bar/baz");
      Test("/foo/", "bar/baz", "/foo/bar/baz");
      Test(".", "foo", "./foo");
      Test("..", "foo", "../foo");
      Test("~", "foo", "~/foo");
      Test("/", "foo", "/foo");

      Test("foo", "", "foo/");
      Test("", "foo", "foo");

--  begin read only
   end Test_2_Append;
--  end read only


--  begin read only
   procedure Test_1_Remove_Last_Component (Gnattest_T : in out Test_Path);
   procedure Test_Remove_Last_Component_0b58ad (Gnattest_T : in out Test_Path) renames Test_1_Remove_Last_Component;
--  id:2.2/0b58ad5b8e8c8a01/Remove_Last_Component/1/0/
   procedure Test_1_Remove_Last_Component (Gnattest_T : in out Test_Path) is
   --  paths.ads:272:4:Remove_Last_Component
--  end read only

      procedure Test (Path : Path_String; Expected : Path_String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.Remove_Last_Component.To_String, Expected,
            "Remove_Last_Component(""" & Path & """)");
      end Test;

   begin

      Test("/foo/bar/baz", "/foo/bar");
      Test("/foo/bar/baz/", "/foo/bar/");
      Test("/foo", "/");
      Test("/foo/", "/");

      Test("/", "");
      Test("foo/", "");
      Test("", "");

--  begin read only
   end Test_1_Remove_Last_Component;
--  end read only


--  begin read only
   procedure Test_2_Remove_Last_Component (Gnattest_T : in out Test_Path);
   procedure Test_Remove_Last_Component_9aa454 (Gnattest_T : in out Test_Path) renames Test_2_Remove_Last_Component;
--  id:2.2/9aa45453e65d9f53/Remove_Last_Component/0/0/
   procedure Test_2_Remove_Last_Component (Gnattest_T : in out Test_Path) is
   --  paths.ads:273:4:Remove_Last_Component
--  end read only

      procedure Test (Path : Path_String; Expected : Path_String) is
      begin
         Set_Path(Gnattest_T.Fixture.all, Path);

         Gnattest_T.Fixture.Remove_Last_Component;

         Test_Util.Assert_Equal_String
           (Gnattest_T.Fixture.To_String, Expected,
            "Remove_Last_Component(""" & Path & """)");
      end Test;

   begin

      Test("/foo/bar/baz", "/foo/bar");
      Test("/foo/bar/baz/", "/foo/bar/");
      Test("/foo", "/");
      Test("/foo/", "/");

      Test("/", "");
      Test("foo/", "");
      Test("", "");

--  begin read only
   end Test_2_Remove_Last_Component;
--  end read only


--  begin read only
   procedure Test_1_Merge (Gnattest_T : in out Test_Path);
   procedure Test_Merge_51c348 (Gnattest_T : in out Test_Path) renames Test_1_Merge;
--  id:2.2/51c348cb40ce9d90/Merge/1/0/
   procedure Test_1_Merge (Gnattest_T : in out Test_Path) is
   --  paths.ads:286:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         Path1 : Path := Make_Path(P1);
         Path2 : Path := Make_Path(P2);

      begin
         Test_Util.Assert_Equal_String
           (Path1.Merge(Path2).To_String, Expected,
            "Merge(""" & P1 & """, """ & P2 & """)");
      end Test;

   begin

      Test("/foo/bar", "baz.txt", "/foo/baz.txt");
      Test("/foo/bar/", "baz.txt", "/foo/bar/baz.txt");
      Test("/foo/bar/", "/baz/file.txt", "/baz/file.txt");
      Test("/foo/bar/", "~/baz/file.txt", "~/baz/file.txt");
      Test("foo/bar", "baz", "foo/baz");
      Test("foo/bar", "baz/", "foo/baz/");

      Test("/foo/bar", "", "/foo/");
      Test("/foo/bar/", "", "/foo/bar/");
      Test("", "foo/bar", "foo/bar");
      Test("", "", "");

--  begin read only
   end Test_1_Merge;
--  end read only


--  begin read only
   procedure Test_2_Merge (Gnattest_T : in out Test_Path);
   procedure Test_Merge_230f40 (Gnattest_T : in out Test_Path) renames Test_2_Merge;
--  id:2.2/230f409c6fbd7ad5/Merge/0/0/
   procedure Test_2_Merge (Gnattest_T : in out Test_Path) is
   --  paths.ads:287:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         Path1 : Path := Make_Path(P1);
         Path2 : Path := Make_Path(P2);

      begin
         Path1.Merge(Path2);

         Test_Util.Assert_Equal_String
           (Path1.To_String, Expected,
            "Merge(""" & P1 & """, """ & P2 & """)");
      end Test;

   begin

      Test("/foo/bar", "baz.txt", "/foo/baz.txt");
      Test("/foo/bar/", "baz.txt", "/foo/bar/baz.txt");
      Test("/foo/bar/", "/baz/file.txt", "/baz/file.txt");
      Test("/foo/bar/", "~/baz/file.txt", "~/baz/file.txt");
      Test("foo/bar", "baz", "foo/baz");
      Test("foo/bar", "baz/", "foo/baz/");

      Test("/foo/bar", "", "/foo/");
      Test("/foo/bar/", "", "/foo/bar/");
      Test("", "foo/bar", "foo/bar");
      Test("", "", "");

--  begin read only
   end Test_2_Merge;
--  end read only


--  begin read only
   procedure Test_Equal (Gnattest_T : in out Test_Path);
   procedure Test_Equal_1a15d1 (Gnattest_T : in out Test_Path) renames Test_Equal;
--  id:2.2/1a15d1dbd895fce7/Equal/1/0/
   procedure Test_Equal (Gnattest_T : in out Test_Path) is
   --  paths.ads:298:4:"="
--  end read only

      procedure Test (P1, P2 : Path_String; Expected : Boolean := True) is
      begin
         Set_Path(Gnattest_T.Fixture.all, P1);

         declare
            Path1 : Path'Class := Gnattest_T.Fixture.all;
         begin
            Set_Path(Gnattest_T.Fixture.all, P2);

            Test_Util.Assert
              ((Path1 = Gnattest_T.Fixture.all) = Expected,
               """" & P1 & """ = """ & P2 & """ /= " & Expected'Image);
         end;
      end Test;

   begin

      Test("/foo/bar/baz", "/foo/bar/baz");
      Test("", "");

      Test("/foo/bar", "/foo/baz", False);

--  begin read only
   end Test_Equal;
--  end read only


--  begin read only
   procedure Test_Is_Subpath (Gnattest_T : in out Test_Path);
   procedure Test_Is_Subpath_ff4b23 (Gnattest_T : in out Test_Path) renames Test_Is_Subpath;
--  id:2.2/ff4b23da36058932/Is_Subpath/1/0/
   procedure Test_Is_Subpath (Gnattest_T : in out Test_Path) is
   --  paths.ads:313:4:Is_Subpath
--  end read only

      procedure Test (P1, P2 : Path_String; Expected : Boolean := True) is
      begin
         Set_Path(Gnattest_T.Fixture.all, P1);

         declare
            Path1 : Path'Class := Gnattest_T.Fixture.all;
         begin
            Set_Path(Gnattest_T.Fixture.all, P2);

            Test_Util.Assert
              (Path1.Is_Subpath(Gnattest_T.Fixture.all) = Expected,
               "Is_Subpath(""" & P1 & """, """ & P2 & """) /= " & Expected'Image);
         end;
      end Test;

   begin

      Test("/foo/bar/baz/file.txt", "/foo/bar", True);
      Test("/foo/baz/file.txt", "/foo/bar", False);
      Test("/foo/bar", "/foo/bar/", False);
      Test("/foo/bar", "/foo/bar", False);
      Test("/foo/bar.txt", "/foo/bar", False);
      Test("foo", "", True);

      Test("", "foo", False);
      Test("", "", False);

--  begin read only
   end Test_Is_Subpath;
--  end read only


--  begin read only
   procedure Test_Is_Child (Gnattest_T : in out Test_Path);
   procedure Test_Is_Child_a8b575 (Gnattest_T : in out Test_Path) renames Test_Is_Child;
--  id:2.2/a8b57585827a5e48/Is_Child/1/0/
   procedure Test_Is_Child (Gnattest_T : in out Test_Path) is
   --  paths.ads:326:4:Is_Child
--  end read only

      procedure Test (P1, P2 : Path_String; Expected : Boolean := True) is
      begin
         Set_Path(Gnattest_T.Fixture.all, P1);

         declare
            Path1 : Path'Class := Gnattest_T.Fixture.all;
         begin
            Set_Path(Gnattest_T.Fixture.all, P2);

            Test_Util.Assert
              (Path1.Is_Child(Gnattest_T.Fixture.all) = Expected,
               "Is_Child(""" & P1 & """, """ & P2 & """) /= " & Expected'Image);
         end;
      end Test;

   begin

      Test("/foo/bar/file.txt", "/foo/bar", True);
      Test("/foo/bar/baz/file.txt", "/foo/bar", False);
      Test("/foo/baz/file.txt", "/foo/bar", False);
      Test("/foo/bar", "/foo/bar/", False);
      Test("/foo/bar", "/foo/bar", False);
      Test("foo", "", True);
      Test("", "foo", False);
      Test("", "", False);

--  begin read only
   end Test_Is_Child;
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
end Paths.Path_Test_Data.Path_Tests;
