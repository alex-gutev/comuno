--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paths.String_Paths.String_Path_Test_Data.

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
package body Paths.String_Paths.String_Path_Test_Data.String_Path_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Make_Path (Gnattest_T : in out Test_String_Path);
   procedure Test_Make_Path_0bb025 (Gnattest_T : in out Test_String_Path) renames Test_1_Make_Path;
--  id:2.2/0bb0256f17024c96/Make_Path/1/0/
   procedure Test_1_Make_Path (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:24:4:Make_Path
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
   procedure Test_2_Make_Path (Gnattest_T : in out Test_String_Path);
   procedure Test_Make_Path_9a7f48 (Gnattest_T : in out Test_String_Path) renames Test_2_Make_Path;
--  id:2.2/9a7f48393879fa5c/Make_Path/0/0/
   procedure Test_2_Make_Path (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:25:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      P: String_Path := Make_Path(Ada.Strings.Unbounded.To_Unbounded_String("/usr/local/foo"));

   begin

      Test_Util.Assert_Equal_String
        (To_String(P), "/usr/local/foo",
         "Make_Path(To_Unbounded_string(""/usr/local/foo""))");

--  begin read only
   end Test_2_Make_Path;
--  end read only


--  begin read only
   procedure Test_3_Make_Path (Gnattest_T : in out Test_String_Path);
   procedure Test_Make_Path_4e9adf (Gnattest_T : in out Test_String_Path) renames Test_3_Make_Path;
--  id:2.2/4e9adfb9ccfc979d/Make_Path/0/0/
   procedure Test_3_Make_Path (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:45:4:Make_Path
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
   procedure Test_4_Make_Path (Gnattest_T : in out Test_String_Path);
   procedure Test_Make_Path_a1875f (Gnattest_T : in out Test_String_Path) renames Test_4_Make_Path;
--  id:2.2/a1875f1d3ed772e0/Make_Path/0/0/
   procedure Test_4_Make_Path (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:46:4:Make_Path
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
   procedure Test_5_Make_Path (Gnattest_T : in out Test_String_Path);
   procedure Test_Make_Path_64b12a (Gnattest_T : in out Test_String_Path) renames Test_5_Make_Path;
--  id:2.2/64b12a31918c7bce/Make_Path/0/0/
   procedure Test_5_Make_Path (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:57:4:Make_Path
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
   procedure Test_6_Make_Path (Gnattest_T : in out Test_String_Path);
   procedure Test_Make_Path_791aa2 (Gnattest_T : in out Test_String_Path) renames Test_6_Make_Path;
--  id:2.2/791aa219793fee32/Make_Path/0/0/
   procedure Test_6_Make_Path (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:58:4:Make_Path
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

      -- Tests --

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
   procedure Test_To_String (Gnattest_T : in out Test_String_Path);
   procedure Test_To_String_32f526 (Gnattest_T : in out Test_String_Path) renames Test_To_String;
--  id:2.2/32f526100ee3ad2a/To_String/1/0/
   procedure Test_To_String (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:63:4:To_String
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Test_Util.Assert_Equal_String
        (To_String(Make_Path("/usr/local/foo")), "/usr/local/foo",
         "Make_Path(To_Unbounded_string(""/usr/local/foo""))");

--  begin read only
   end Test_To_String;
--  end read only


--  begin read only
   procedure Test_Components (Gnattest_T : in out Test_String_Path);
   procedure Test_Components_7bfffb (Gnattest_T : in out Test_String_Path) renames Test_Components;
--  id:2.2/7bfffb15ab480b0a/Components/1/0/
   procedure Test_Components (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:68:4:Components
--  end read only

      pragma Unreferenced (Gnattest_T);

      function Unbounded (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      use Component_Vectors;

      procedure Test (Path : String; Result : Component_Vector) is
      begin
         Test_Util.Assert
           (Make_Path(Path).Components = Result,
            "Components not equal.");
      end Test;


   begin

      Test
        ("/foo/bar/baz",
         (Component_Vector'(Unbounded("/") & Unbounded("foo")) &
            Unbounded("bar") & Unbounded("baz")));

      Test
        ("/foo/bar/baz/",
         Component_Vector'(Unbounded("/") & Unbounded("foo")) &
           Unbounded("bar")& Unbounded("baz"));


      Test
        ("/foo/bar/baz//",
         Component_Vector'(Unbounded("/") & Unbounded("foo")) &
           Unbounded("bar") & Unbounded("baz"));

      Test
        ("/foo//bar/baz//",
         Component_Vector'(Unbounded("/") & Unbounded("foo")) &
           Unbounded("bar") & Unbounded("baz"));

      Test
        ("//foo/bar/baz/",
         Component_Vector'(Unbounded("/") & Unbounded("foo")) &
           Unbounded("bar") & Unbounded("baz"));

      Test
        ("foo/bar/baz/",
         Component_Vector'(Unbounded("foo") & Unbounded("bar")) &
           Unbounded("baz"));


      Test("foo", To_Vector(Unbounded("foo"), 1));

      Test("", Empty_Vector);

      Test("/", To_Vector(Unbounded("/"), 1));

      Test("///", To_Vector(Unbounded("/"), 1));

--  begin read only
   end Test_Components;
--  end read only


--  begin read only
   procedure Test_Basename (Gnattest_T : in out Test_String_Path);
   procedure Test_Basename_9acd9f (Gnattest_T : in out Test_String_Path) renames Test_Basename;
--  id:2.2/9acd9ff9735caeac/Basename/1/0/
   procedure Test_Basename (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:69:4:Basename
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path : String; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (Basename(Make_Path(Path)), expected,
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
   procedure Test_Is_Directory (Gnattest_T : in out Test_String_Path);
   procedure Test_Is_Directory_209e9d (Gnattest_T : in out Test_String_Path) renames Test_Is_Directory;
--  id:2.2/209e9d0604dd0956/Is_Directory/1/0/
   procedure Test_Is_Directory (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:74:4:Is_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Expected : Boolean) is
         P : Path := Make_Path(Path_String);
      begin

         Test_Util.Assert
           ((if Expected then
                Is_Directory(P) else
                not Is_Directory(P)),
            "Is_Directory(""" & Path_String & """) not " & Expected'Image);

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
   procedure Test_Has_Directories (Gnattest_T : in out Test_String_Path);
   procedure Test_Has_Directories_647c02 (Gnattest_T : in out Test_String_Path) renames Test_Has_Directories;
--  id:2.2/647c02ab8250a472/Has_Directories/1/0/
   procedure Test_Has_Directories (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:75:4:Has_Directories
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Expected : Boolean) is
         P : Path := Make_Path(Path_String);
      begin

         Test_Util.Assert
           ((if Expected then
                Has_Directories(P) else
                not Has_Directories(P)),
            "Has_Directories(""" & Path_String & """) not " & Expected'Image);

      end Test;

   begin

      Test("/foo/bar", True);
      Test("foo.txt", False);
      Test("foo/", True);
      Test("/", True);
      Test(".", True);
      Test("..", True);
      Test("~", True);
      Test("~user", True);
      Test("", False);

--  begin read only
   end Test_Has_Directories;
--  end read only


--  begin read only
   procedure Test_Is_Empty (Gnattest_T : in out Test_String_Path);
   procedure Test_Is_Empty_d6ecc0 (Gnattest_T : in out Test_String_Path) renames Test_Is_Empty;
--  id:2.2/d6ecc0d5a237aed5/Is_Empty/1/0/
   procedure Test_Is_Empty (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:77:4:Is_Empty
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Test_Util.Assert
        (Is_Empty(Make_Path("")),
         "Is_Empty("""") not True.");

      Test_Util.Assert
        (not Is_Empty(Make_Path("foo")),
         "Is_Empty(""foo"") not False.");

--  begin read only
   end Test_Is_Empty;
--  end read only


--  begin read only
   procedure Test_Is_Root (Gnattest_T : in out Test_String_Path);
   procedure Test_Is_Root_c192af (Gnattest_T : in out Test_String_Path) renames Test_Is_Root;
--  id:2.2/c192af02773d551a/Is_Root/1/0/
   procedure Test_Is_Root (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:78:4:Is_Root
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Expected : Boolean) is
         P : Path := Make_Path(Path_String);
      begin

         Test_Util.Assert
           ((if Expected then
                Is_Root(P) else
                not Is_Root(P)),
            "Is_Root(""" & Path_String & """) not " & Expected'Image);

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
   procedure Test_Is_Relative (Gnattest_T : in out Test_String_Path);
   procedure Test_Is_Relative_3c6500 (Gnattest_T : in out Test_String_Path) renames Test_Is_Relative;
--  id:2.2/3c650005ac520c6d/Is_Relative/1/0/
   procedure Test_Is_Relative (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:79:4:Is_Relative
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Expected : Boolean) is
         P : Path := Make_Path(Path_String);
      begin

         Test_Util.Assert
           ((if Expected then
                Is_Relative(P) else
                not Is_Relative(P)),
            "Is_Relative(""" & Path_String & """) not " & Expected'Image);

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
   procedure Test_1_Ensure_Directory (Gnattest_T : in out Test_String_Path);
   procedure Test_Ensure_Directory_b6bd5e (Gnattest_T : in out Test_String_Path) renames Test_1_Ensure_Directory;
--  id:2.2/b6bd5ee21b20626c/Ensure_Directory/1/0/
   procedure Test_1_Ensure_Directory (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:84:4:Ensure_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path : String; Directory : Boolean; Expected : String) is
      begin

         Test_Util.Assert_Equal_String
           (To_String(Ensure_Directory(Make_Path(Path), Directory)),
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

      Test(".", True, "./");
      Test(".", False, ".");

--  begin read only
   end Test_1_Ensure_Directory;
--  end read only


--  begin read only
   procedure Test_2_Ensure_Directory (Gnattest_T : in out Test_String_Path);
   procedure Test_Ensure_Directory_bc6761 (Gnattest_T : in out Test_String_Path) renames Test_2_Ensure_Directory;
--  id:2.2/bc676145ba82b583/Ensure_Directory/0/0/
   procedure Test_2_Ensure_Directory (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:85:4:Ensure_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Directory : Boolean; Expected : String) is
         P : Path := Make_Path(Path_String);
      begin
         Ensure_Directory(P, Directory);

         Test_Util.Assert_Equal_String
           (To_String(P), Expected,
            "Ensure_Directory(""" & Path_String & """, " & Directory'Image & ")");

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

      Test(".", True, "./");
      Test(".", False, ".");

--  begin read only
   end Test_2_Ensure_Directory;
--  end read only


--  begin read only
   procedure Test_1_Append (Gnattest_T : in out Test_String_Path);
   procedure Test_Append_0ba638 (Gnattest_T : in out Test_String_Path) renames Test_1_Append;
--  id:2.2/0ba638a857b1c0dc/Append/1/0/
   procedure Test_1_Append (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:87:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test(Path1, Path2 : String; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (To_String(Append(Make_Path(Path1), Make_Path(Path2))), Expected,
            "Append(""" & Path1 & """, """ & Path2 & """)");
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
   procedure Test_2_Append (Gnattest_T : in out Test_String_Path);
   procedure Test_Append_6d672e (Gnattest_T : in out Test_String_Path) renames Test_2_Append;
--  id:2.2/6d672eb0611f9ca1/Append/0/0/
   procedure Test_2_Append (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:88:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path1, Path2 : String; Expected : String) is
         P1 : Path := Make_Path(Path1);
         P2 : Path := Make_Path(Path2);
      begin
         Append(P1, P2);

         Test_Util.Assert_Equal_String
           (To_String(P1), Expected,
            "Append(""" & Path1 & """, """ & Path2 & """)");

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
   procedure Test_1_Remove_Last_Component (Gnattest_T : in out Test_String_Path);
   procedure Test_Remove_Last_Component_99f98e (Gnattest_T : in out Test_String_Path) renames Test_1_Remove_Last_Component;
--  id:2.2/99f98e79c5e37fe5/Remove_Last_Component/1/0/
   procedure Test_1_Remove_Last_Component (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:90:4:Remove_Last_Component
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (To_String(Remove_Last_Component(Make_Path(Path_String))),
            Expected,
            "Remove_Last_Component(""" & Path_String & """)");
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
   procedure Test_2_Remove_Last_Component (Gnattest_T : in out Test_String_Path);
   procedure Test_Remove_Last_Component_ae165c (Gnattest_T : in out Test_String_Path) renames Test_2_Remove_Last_Component;
--  id:2.2/ae165cce76ab46a1/Remove_Last_Component/0/0/
   procedure Test_2_Remove_Last_Component (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:91:4:Remove_Last_Component
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Expected : String) is
         P : Path := Make_Path(Path_String);
      begin
         Remove_Last_Component(P);

         Test_Util.Assert_Equal_String
           (To_String(P), Expected,
            "Remove_Last_Component(""" & Path_String & """)");
      end Test;

   begin

      Test("/foo/bar/baz", "/foo/bar");
      Test("/foo/bar/baz/", "/foo/bar/");
      Test("/foo", "/");
      Test("/foo/", "/");

      Test("/", "");
      Test("foo", "");
      Test("", "");

--  begin read only
   end Test_2_Remove_Last_Component;
--  end read only


--  begin read only
   procedure Test_1_Merge (Gnattest_T : in out Test_String_Path);
   procedure Test_Merge_eb4978 (Gnattest_T : in out Test_String_Path) renames Test_1_Merge;
--  id:2.2/eb497801d939a02a/Merge/1/0/
   procedure Test_1_Merge (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:93:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path1, Path2 : String; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (To_String(Merge(Make_Path(Path1), Make_Path(Path2))),
            Expected,
            "Merge(""" & Path1 & """, """ & Path2 & """)");
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
   procedure Test_2_Merge (Gnattest_T : in out Test_String_Path);
   procedure Test_Merge_f7b327 (Gnattest_T : in out Test_String_Path) renames Test_2_Merge;
--  id:2.2/f7b327a7a3b4f869/Merge/0/0/
   procedure Test_2_Merge (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:94:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path1, Path2 : String; Expected : String) is
         P1 : Path := Make_Path(Path1);
         P2 : Path := Make_Path(Path2);

      begin
         Merge(P1, P2);

         Test_Util.Assert_Equal_String
           (To_String(P1), Expected,
            "Merge(""" & Path1 & """, """ & Path2 & """)");
      end Test;

   begin

      Test("/foo/bar", "baz.txt", "/foo/baz.txt");
      Test("/foo/bar/", "baz.txt", "/foo/bar/baz.txt");
      Test("/foo/bar/", "/baz/file.txt", "/baz/file.txt");
      Test("/foo/bar/", "~/baz/file.txt", "~/baz/file.txt");
      Test("foo/bar", "baz", "foo/baz");

      Test("/foo/bar", "", "/foo/");
      Test("/foo/bar/", "", "/foo/bar/");
      Test("", "foo/bar", "foo/bar");
      Test("", "", "");

--  begin read only
   end Test_2_Merge;
--  end read only


--  begin read only
   procedure Test_Canonicalize (Gnattest_T : in out Test_String_Path);
   procedure Test_Canonicalize_818747 (Gnattest_T : in out Test_String_Path) renames Test_Canonicalize;
--  id:2.2/81874761ca3ca7e3/Canonicalize/1/0/
   procedure Test_Canonicalize (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:96:4:Canonicalize
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path_String : String; Directory : Boolean; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (To_String(Canonicalize(Make_Path(Path_String), Directory)),
            Expected,
            "Canonicalize(""" & Path_String & """, " & Directory'Image & ")");
      end Test;

   begin

      Test("a/relative/../path/./", False, "a/path");
      Test("a/relative/../path/./", True, "a/path/");

      Test("a/relative/../../../path/./dir", False, "../path/dir");
      Test("a/relative/../../../path/./dir", True, "../path/dir/");

      Test(".././../a///bad/path/", False, "../../a/bad/path");
      Test(".././../a///bad/path/", True, "../../a/bad/path/");

      Test("/../dir/file", False, "dir/file");
      Test("/", False, "/");

      Test("", True, "");
      Test("", False, "");

--  begin read only
   end Test_Canonicalize;
--  end read only


--  begin read only
   procedure Test_Equal (Gnattest_T : in out Test_String_Path);
   procedure Test_Equal_d8ea50 (Gnattest_T : in out Test_String_Path) renames Test_Equal;
--  id:2.2/d8ea50df0618d7b3/Equal/1/0/
   procedure Test_Equal (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:101:4:"="
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path1, Path2 : String; Equal : Boolean := True) is
         P1 : Path := Make_Path(Path1);
         P2 : Path := Make_Path(Path2);

      begin
         Test_Util.Assert
           ((if Equal then P1 = P2 else P1 /= P2),
            Path1 & " = " & Path2 & " not " & Equal'Image);
      end Test;

   begin

      Test("/foo/bar/baz", "/foo/bar/baz");
      Test("", "");

      Test("/foo/bar", "/foo/baz", False);

--  begin read only
   end Test_Equal;
--  end read only


--  begin read only
   procedure Test_Is_Subpath (Gnattest_T : in out Test_String_Path);
   procedure Test_Is_Subpath_452b56 (Gnattest_T : in out Test_String_Path) renames Test_Is_Subpath;
--  id:2.2/452b56cc18b4abbe/Is_Subpath/1/0/
   procedure Test_Is_Subpath (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:103:4:Is_Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Child, Parent : String; Expected : Boolean) is
         P_Child : Path := Make_Path(Child);
         P_Parent : Path := Make_Path(Parent);
      begin

         Test_Util.Assert
           ((if Expected then
                Is_Subpath(P_Child, P_Parent) else
                not Is_Subpath(P_Child, P_Parent)),
            "Is_Subpath(""" & Child & """, """ & Parent & """) not " & Expected'Image);

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
   procedure Test_Is_Child (Gnattest_T : in out Test_String_Path);
   procedure Test_Is_Child_d6492a (Gnattest_T : in out Test_String_Path) renames Test_Is_Child;
--  id:2.2/d6492a6d1678c3a6/Is_Child/1/0/
   procedure Test_Is_Child (Gnattest_T : in out Test_String_Path) is
   --  paths-string_paths.ads:105:4:Is_Child
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Child, Parent : String; Expected : Boolean) is
         P_Child : Path := Make_Path(Child);
         P_Parent : Path := Make_Path(Parent);
      begin

         Test_Util.Assert
           ((if Expected then
                Is_Child(P_Child, P_Parent) else
                not Is_Child(P_Child, P_Parent)),
            "Is_Child(""" & Child & """, """ & Parent & """) not " & Expected'Image);

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
end Paths.String_Paths.String_Path_Test_Data.String_Path_Tests;
