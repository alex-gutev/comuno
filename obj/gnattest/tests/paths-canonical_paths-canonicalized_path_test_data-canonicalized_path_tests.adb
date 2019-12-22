--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paths.Canonical_Paths.Canonicalized_Path_Test_Data.

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
package body Paths.Canonical_Paths.Canonicalized_Path_Test_Data.Canonicalized_Path_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Make_Path (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Make_Path_0a8271 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Make_Path;
--  id:2.2/0a8271e658df70a4/Make_Path/1/0/
   procedure Test_Make_Path (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:28:4:Make_Path
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Component_Vectors;

      function Unbounded (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

   begin

      Test_Util.Assert_Equal_String
        (Make_Path
           (Component_Vector'(Unbounded("foo") & Unbounded("bar")), False).To_String,
         "foo/bar",
         "Make_Path((""foo"", ""bar""))");

--  begin read only
   end Test_Make_Path;
--  end read only


--  begin read only
   procedure Test_To_String (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_To_String_d56938 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_To_String;
--  id:2.2/d569385a54d255b8/To_String/1/0/
   procedure Test_To_String (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:33:4:To_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Result : Path_String) is
      begin
         Test_Util.Assert_Equal_String
           (Make_Path(Components, Directory).To_String, Result,
            "To_String(" & To_String(Components) & ")");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("foo"), U("bar"), U("baz")), False, "foo/bar/baz");
      Test((U("foo"), U("bar"), U("baz")), True, "foo/bar/baz/");
      Test((U("/"), U("foo"), U("bar"), U("baz")), False, "/foo/bar/baz");
      Test((U("/"), U("foo"), U("bar"), U("baz")), True, "/foo/bar/baz/");

      Test((1 => U("/")), False, "/");
      Test((1 => U("/")), True, "/");

      Test(Empty, False, "");
      Test(Empty, True, "");

--  begin read only
   end Test_To_String;
--  end read only


--  begin read only
   procedure Test_Components (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Components_ffa5fc (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Components;
--  id:2.2/ffa5fc971f4bd03a/Components/1/0/
   procedure Test_Components (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:38:4:Components
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Component_Vector;

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean) is
      begin
         Test_Util.Assert
           (Make_Path(Components, Directory).Components = Vector(Components),
            "Components not " & To_String(Components));
      end Test;

   begin

      Test((U("foo"), U("bar"), U("baz")), False);
      Test((U("foo"), U("bar"), U("baz")), True);

      Test((U("/"), U("foo"), U("bar"), U("baz")), False);
      Test((U("/"), U("foo"), U("bar"), U("baz")), True);

      Test((1 => U("/")), False);
      Test((1 => U("/")), True);

--  begin read only
   end Test_Components;
--  end read only


--  begin read only
   procedure Test_Basename (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Basename_aa3c3c (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Basename;
--  id:2.2/aa3c3cde45d984d7/Basename/1/0/
   procedure Test_Basename (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:39:4:Basename
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Result : Path_String) is
      begin
         Test_Util.Assert_Equal_String
           (Make_Path(Components, Directory).Basename, Result,
            "Basename(" & To_String(Components) & ")");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("foo"), U("bar"), U("baz.txt")), False, "baz.txt");
      Test((U("foo"), U("bar"), U("baz.txt")), True, "");

      Test((U("/"), U("foo"), U("bar"), U("baz")), False, "baz");
      Test((U("/"), U("foo"), U("bar"), U("baz")), True, "");

      Test((1 => U("foo")), False, "foo");
      Test((1 => U("foo")), True, "");

      Test((1 => U("/")), False, "");
      Test((1 => U("/")), True, "");

      Test(Empty, False, "");
      Test(Empty, True, "");

--  begin read only
   end Test_Basename;
--  end read only


--  begin read only
   procedure Test_Is_Directory (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Is_Directory_e7304c (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Is_Directory;
--  id:2.2/e7304c2af21f0df1/Is_Directory/1/0/
   procedure Test_Is_Directory (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:44:4:Is_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Make_Path(Components, Directory).Is_Directory = Expected,
            "Is_Directory(" & To_String(Components) & ", " &
              Directory'Image & "): /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("bar"), U("baz")), False, Expected => False);
      Test((U("/"), U("bar"), U("baz")), True, Expected => True);

      Test((1 => U("/")), False, Expected => True);
      Test((1 => U("/")), True, Expected => True);

      Test(Empty, False, Expected => False);
      Test(Empty, True, Expected => False);

--  begin read only
   end Test_Is_Directory;
--  end read only


--  begin read only
   procedure Test_Has_Directories (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Has_Directories_5b9147 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Has_Directories;
--  id:2.2/5b9147578f3335ba/Has_Directories/1/0/
   procedure Test_Has_Directories (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:45:4:Has_Directories
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Make_Path(Components, Directory).Has_Directories = Expected,
            "Has_Directories(" & To_String(Components) & ", " &
              Directory'Image & "): /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("foo"), U("bar")), False, Expected => True);
      Test((1 => U("foo.txt")), False, Expected => False);
      Test((1 => U("foo")), True, Expected => True);

      Test((1 => U("/")), False, Expected => True);
      Test((1 => U("/")), True, Expected => True);

      Test((1 => U(".")), False, Expected => True);
      Test((1 => U(".")), True, Expected => True);

      Test((1 => U("..")), False, Expected => True);
      Test((1 => U("..")), True, Expected => True);

      Test((1 => U("~")), False, Expected => True);
      Test((1 => U("~")), True, Expected => True);

      Test((1 => U("~user")), False, Expected => True);
      Test((1 => U("~user")), True, Expected => True);

      Test(Empty, False, Expected => False);
      Test(Empty, True, Expected => False);

--  begin read only
   end Test_Has_Directories;
--  end read only


--  begin read only
   procedure Test_Is_Empty (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Is_Empty_e0cbfa (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Is_Empty;
--  id:2.2/e0cbfa1a15549bd4/Is_Empty/1/0/
   procedure Test_Is_Empty (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:47:4:Is_Empty
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Make_Path(Components, Directory).Is_Empty = Expected,
            "Is_Empty(" & To_String(Components) & ", " &
              Directory'Image & "): /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Empty, False, Expected => True);
      Test(Empty, True, Expected => True);

      Test((1 => U("foo")), False, Expected => False);
      Test((1 => U("foo")), True, Expected => False);

--  begin read only
   end Test_Is_Empty;
--  end read only


--  begin read only
   procedure Test_Is_Root (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Is_Root_672679 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Is_Root;
--  id:2.2/672679d805815e91/Is_Root/1/0/
   procedure Test_Is_Root (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:48:4:Is_Root
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Make_Path(Components, Directory).Is_Root = Expected,
            "Is_Root(" & To_String(Components) & ", " &
              Directory'Image & "): /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((1 => U("/")), False, Expected => True);
      Test((1 => U("/")), True, Expected => True);

      Test((1 => U("foo")), False, Expected => False);
      Test((1 => U("foo")), True, Expected => False);

      Test((1 => U(".")), False, Expected => False);
      Test((1 => U(".")), True, Expected => False);

      Test((1 => U("..")), False, Expected => False);
      Test((1 => U("..")), True, Expected => False);

      Test((1 => U("~")), False, Expected => False);
      Test((1 => U("~")), True, Expected => False);

      Test((U("/"), U("foo")), False, Expected => False);
      Test((U("/"), U("foo")), True, Expected => False);

      Test(Empty, False, Expected => False);
      Test(Empty, True, Expected => False);

--  begin read only
   end Test_Is_Root;
--  end read only


--  begin read only
   procedure Test_Is_Relative (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Is_Relative_d31aa3 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Is_Relative;
--  id:2.2/d31aa35813a92b2e/Is_Relative/1/0/
   procedure Test_Is_Relative (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:49:4:Is_Relative
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Make_Path(Components, Directory).Is_Relative = Expected,
            "Is_Relative(" & To_String(Components) & ", " &
              Directory'Image & "): /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("foo"), U("bar")), False, Expected => True);
      Test((U("foo"), U("bar")), True, Expected => True);

      Test((U("/"), U("foo"), U("bar")), False, Expected => False);
      Test((U("/"), U("foo"), U("bar")), True, Expected => False);

      Test((1 => U("/")), False, Expected => False);
      Test((1 => U("/")), True, Expected => False);

      Test((1 => U("~")), False, Expected => False);
      Test((1 => U("~")), True, Expected => False);
      Test((1 => U("~user")), False, Expected => False);
      Test((1 => U("~user")), True, Expected => False);

      Test((1 => U(".")), False, Expected => True);
      Test((1 => U(".")), True, Expected => True);
      Test((1 => U("..")), False, Expected => True);
      Test((1 => U("..")), True, Expected => True);

      Test(Empty, False, Expected => True);
      Test(Empty, True, Expected => True);

--  begin read only
   end Test_Is_Relative;
--  end read only


--  begin read only
   procedure Test_1_Ensure_Directory (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Ensure_Directory_e0a191 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_1_Ensure_Directory;
--  id:2.2/e0a19112612009aa/Ensure_Directory/1/0/
   procedure Test_1_Ensure_Directory (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:54:4:Ensure_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory, Make_Directory : Boolean; Expected : String) is
         Path : Canonicalized_Path := Make_Path(Components, Directory).Ensure_Directory(Make_Directory);
      begin
         Test_Util.Assert_Equal_String
           (Path.To_String, Expected,
            "Ensure_Directory((" & To_String(Components) & ", " &
              Directory'Image & "), " & Make_Directory'Image & ")");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("foo"), U("bar")),
           Directory => False, Make_Directory => True,
           Expected => "/foo/bar/");

      Test((U("/"), U("foo"), U("bar")),
           Directory => False, Make_Directory => False,
           Expected => "/foo/bar");


      Test((U("/"), U("foo"), U("bar")),
           Directory => True, Make_Directory => True,
           Expected => "/foo/bar/");

      Test((U("/"), U("foo"), U("bar")),
           Directory => True, Make_Directory => False,
           Expected => "/foo/bar");


      Test((1 => U("/")),
           Directory => False, Make_Directory => True,
           Expected => "/");

      Test((1 => U("/")),
           Directory => False, Make_Directory => False,
           Expected => "/");


      Test((1 => U(".")),
           Directory => False, Make_Directory => True,
           Expected => "./");

      Test((1 => U(".")),
           Directory => True, Make_Directory => False,
           Expected => ".");


      Test(Empty,
           Directory => False, Make_Directory => True,
           Expected => "");

      Test(Empty,
           Directory => True, Make_Directory => False,
           Expected => "");

--  begin read only
   end Test_1_Ensure_Directory;
--  end read only


--  begin read only
   procedure Test_2_Ensure_Directory (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Ensure_Directory_aad401 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_2_Ensure_Directory;
--  id:2.2/aad401aa42ecdf09/Ensure_Directory/0/0/
   procedure Test_2_Ensure_Directory (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:55:4:Ensure_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory, Make_Directory : Boolean; Expected : String) is
         Path : Canonicalized_Path := Make_Path(Components, Directory);

      begin
         Path.Ensure_Directory(Make_Directory);

         Test_Util.Assert_Equal_String
           (Path.To_String, Expected,
            "Ensure_Directory((" & To_String(Components) & ", " &
              Directory'Image & "), " & Make_Directory'Image & ")");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("foo"), U("bar")),
           Directory => False, Make_Directory => True,
           Expected => "/foo/bar/");

      Test((U("/"), U("foo"), U("bar")),
           Directory => False, Make_Directory => False,
           Expected => "/foo/bar");


      Test((U("/"), U("foo"), U("bar")),
           Directory => True, Make_Directory => True,
           Expected => "/foo/bar/");

      Test((U("/"), U("foo"), U("bar")),
           Directory => True, Make_Directory => False,
           Expected => "/foo/bar");


      Test((1 => U("/")),
           Directory => False, Make_Directory => True,
           Expected => "/");

      Test((1 => U("/")),
           Directory => False, Make_Directory => False,
           Expected => "/");


      Test((1 => U(".")),
           Directory => False, Make_Directory => True,
           Expected => "./");

      Test((1 => U(".")),
           Directory => True, Make_Directory => False,
           Expected => ".");


      Test(Empty,
           Directory => False, Make_Directory => True,
           Expected => "");

      Test(Empty,
           Directory => True, Make_Directory => False,
           Expected => "");

--  begin read only
   end Test_2_Ensure_Directory;
--  end read only


--  begin read only
   procedure Test_1_Append (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Append_5c0f8e (Gnattest_T : in out Test_Canonicalized_Path) renames Test_1_Append;
--  id:2.2/5c0f8eeddb99965f/Append/1/0/
   procedure Test_1_Append (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:57:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : String) is
      begin

         Test_Util.Assert_Equal_String
           (P1.Append(P2).To_String, Expected,
            "Append(""" & P1.To_String & """, """ & P2.To_String & """)");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar"))),
           Make_Path((1 => U("baz"))),
           "/foo/bar/baz");

      Test(Make_Path((U("/"), U("foo"))),
           Make_Path((U("bar"), U("baz"))),
           "/foo/bar/baz");

      Test(Make_Path((U("/"), U("foo")), True),
           Make_Path((U("bar"), U("baz"))),
           "/foo/bar/baz");


      Test(Make_Path((1 => U("."))),
           Make_Path((1 => U("foo"))),
           "./foo");

      Test(Make_Path((1 => U(".."))),
           Make_Path((1 => U("foo"))),
           "../foo");

      Test(Make_Path((1 => U("~"))),
           Make_Path((1 => U("foo"))),
           "~/foo");


      Test(Make_Path((1 => U("/"))),
           Make_Path((1 => U("foo"))),
           "/foo");

      Test(Make_Path((1 => U("/")), True),
           Make_Path((1 => U("foo"))),
           "/foo");


      Test(Make_Path((1 => U("foo"))),
           Make_Path(Empty),
           "foo/");

      Test(Make_Path(Empty),
           Make_Path((1 => U("foo"))),
           "foo");

--  begin read only
   end Test_1_Append;
--  end read only


--  begin read only
   procedure Test_2_Append (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Append_02b90f (Gnattest_T : in out Test_Canonicalized_Path) renames Test_2_Append;
--  id:2.2/02b90f224e1e831e/Append/0/0/
   procedure Test_2_Append (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:58:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : String) is
         Path : Canonicalized_Path := P1;
      begin

         Path.Append(P2);

         Test_Util.Assert_Equal_String
           (Path.To_String, Expected,
            "Append(""" & P1.To_String & """, """ & P2.To_String & """)");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar"))),
           Make_Path((1 => U("baz"))),
           "/foo/bar/baz");

      Test(Make_Path((U("/"), U("foo"))),
           Make_Path((U("bar"), U("baz"))),
           "/foo/bar/baz");

      Test(Make_Path((U("/"), U("foo")), True),
           Make_Path((U("bar"), U("baz"))),
           "/foo/bar/baz");


      Test(Make_Path((1 => U("."))),
           Make_Path((1 => U("foo"))),
           "./foo");

      Test(Make_Path((1 => U(".."))),
           Make_Path((1 => U("foo"))),
           "../foo");

      Test(Make_Path((1 => U("~"))),
           Make_Path((1 => U("foo"))),
           "~/foo");


      Test(Make_Path((1 => U("/"))),
           Make_Path((1 => U("foo"))),
           "/foo");

      Test(Make_Path((1 => U("/")), True),
           Make_Path((1 => U("foo"))),
           "/foo");


      Test(Make_Path((1 => U("foo"))),
           Make_Path(Empty),
           "foo/");

      Test(Make_Path(Empty),
           Make_Path((1 => U("foo"))),
           "foo");

--  begin read only
   end Test_2_Append;
--  end read only


--  begin read only
   procedure Test_1_Remove_Last_Component (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Remove_Last_Component_8755fa (Gnattest_T : in out Test_Canonicalized_Path) renames Test_1_Remove_Last_Component;
--  id:2.2/8755fa28753877e8/Remove_Last_Component/1/0/
   procedure Test_1_Remove_Last_Component (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:60:4:Remove_Last_Component
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : String) is
         Path : Canonicalized_Path := Make_Path(Components, Directory);
      begin

         Test_Util.Assert_Equal_String
           (Path.Remove_Last_Component.To_String, Expected,
            "Remove_Last_Component((" & To_String(Components) & ", " & Directory'Image & "))");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("foo"), U("bar"), U("baz")), False, "/foo/bar");
      Test((U("/"), U("foo"), U("bar"), U("baz")), True, "/foo/bar/");

      Test((U("/"), U("foo")), False, "/");
      Test((U("/"), U("foo")), True, "/");

      Test((1 => U("/")), False, "");
      Test((1 => U("/")), True, "");

      Test((1 => U("foo")), False, "");
      Test((1 => U("foo")), True, "");

      Test(Empty, False, "");
      Test(Empty, True, "");

--  begin read only
   end Test_1_Remove_Last_Component;
--  end read only


--  begin read only
   procedure Test_2_Remove_Last_Component (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Remove_Last_Component_3a18f6 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_2_Remove_Last_Component;
--  id:2.2/3a18f64e096fb9f1/Remove_Last_Component/0/0/
   procedure Test_2_Remove_Last_Component (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:61:4:Remove_Last_Component
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : String) is
         Path : Canonicalized_Path := Make_Path(Components, Directory);
      begin

         Path.Remove_Last_Component;

         Test_Util.Assert_Equal_String
           (Path.To_String, Expected,
            "Remove_Last_Component((" & To_String(Components) & ", " & Directory'Image & "))");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("foo"), U("bar"), U("baz")), False, "/foo/bar");
      Test((U("/"), U("foo"), U("bar"), U("baz")), True, "/foo/bar/");

      Test((U("/"), U("foo")), False, "/");
      Test((U("/"), U("foo")), True, "/");

      Test((1 => U("/")), False, "");
      Test((1 => U("/")), True, "");

      Test((1 => U("foo")), False, "");
      Test((1 => U("foo")), True, "");

      Test(Empty, False, "");
      Test(Empty, True, "");

--  begin read only
   end Test_2_Remove_Last_Component;
--  end read only


--  begin read only
   procedure Test_1_Merge (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Merge_b109b3 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_1_Merge;
--  id:2.2/b109b38e512952d6/Merge/1/0/
   procedure Test_1_Merge (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:63:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : String) is
      begin
         Test_Util.Assert_Equal_String
           (P1.Merge(P2).To_String, Expected,
            "Merge(""" & P1.To_String & """, """ & P2.To_String & """)");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path((1 => U("baz.txt")), False),
           "/foo/baz.txt");

      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path((1 => U("baz.txt")), False),
           "/foo/bar/baz.txt");


      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path((U("/"), U("baz"), U("file.txt")), False),
           "/baz/file.txt");

      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path((U("~"), U("baz"), U("file.txt")), False),
           "~/baz/file.txt");


      Test(Make_Path((U("foo"), U("bar")), False),
           Make_Path((1 => U("baz")), False),
           "foo/baz");

      Test(Make_Path((U("foo"), U("bar")), False),
           Make_Path((1 => U("baz")), True),
           "foo/baz/");


      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path(Empty, False),
           "/foo/");

      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path(Empty, False),
           "/foo/bar/");


      Test(Make_Path(Empty, False),
           Make_Path((U("foo"), U("bar")), True),
           "foo/bar/");


      Test(Make_Path(Empty, False),
           Make_Path(Empty, False),
           "");

      Test(Make_Path(Empty, True),
           Make_Path(Empty, False),
           "");

      Test(Make_Path(Empty, True),
           Make_Path(Empty, True),
           "");

--  begin read only
   end Test_1_Merge;
--  end read only


--  begin read only
   procedure Test_2_Merge (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Merge_dc3298 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_2_Merge;
--  id:2.2/dc3298eee850a82b/Merge/0/0/
   procedure Test_2_Merge (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:64:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : String) is
         Path : Canonicalized_Path := P1;
      begin
         Path.Merge(P2);

         Test_Util.Assert_Equal_String
           (Path.To_String, Expected,
            "Merge(""" & P1.To_String & """, """ & P2.To_String & """)");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path((1 => U("baz.txt")), False),
           "/foo/baz.txt");

      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path((1 => U("baz.txt")), False),
           "/foo/bar/baz.txt");


      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path((U("/"), U("baz"), U("file.txt")), False),
           "/baz/file.txt");

      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path((U("~"), U("baz"), U("file.txt")), False),
           "~/baz/file.txt");


      Test(Make_Path((U("foo"), U("bar")), False),
           Make_Path((1 => U("baz")), False),
           "foo/baz");

      Test(Make_Path((U("foo"), U("bar")), False),
           Make_Path((1 => U("baz")), True),
           "foo/baz/");


      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path(Empty, False),
           "/foo/");

      Test(Make_Path((U("/"), U("foo"), U("bar")), True),
           Make_Path(Empty, False),
           "/foo/bar/");


      Test(Make_Path(Empty, False),
           Make_Path((U("foo"), U("bar")), True),
           "foo/bar/");


      Test(Make_Path(Empty, False),
           Make_Path(Empty, False),
           "");

      Test(Make_Path(Empty, True),
           Make_Path(Empty, False),
           "");

      Test(Make_Path(Empty, True),
           Make_Path(Empty, True),
           "");

--  begin read only
   end Test_2_Merge;
--  end read only


--  begin read only
   procedure Test_Canonicalize (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Canonicalize_89f408 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Canonicalize;
--  id:2.2/89f408d5a6dade54/Canonicalize/1/0/
   procedure Test_Canonicalize (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:66:4:Canonicalize
--  end read only

      pragma Unreferenced (Gnattest_T);


      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(Components : Component_Array; Directory : Boolean; Expected : String) is
         Path : Canonicalized_Path := Make_Path(Components);
      begin

         Test_Util.Assert_Equal_String
           (Path.Canonicalize(Directory).To_String, Expected,
            "Canonicalize((" & To_String(Components) & ", " & Directory'Image & "))");
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test((U("/"), U("foo"), U("bar")), False, "/foo/bar");
      Test((U("/"), U("foo"), U("bar")), True, "/foo/bar/");

      Test((1 => U("/")), False, "/");
      Test((1 => U("/")), True, "/");


--  begin read only
   end Test_Canonicalize;
--  end read only


--  begin read only
   procedure Test_Equal (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Equal_2b071f (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Equal;
--  id:2.2/2b071f55763f4276/Equal/1/0/
   procedure Test_Equal (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:71:4:"="
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : Boolean) is
      begin
         Test_Util.Assert
           ((P1 = P2) = Expected,
            """" & P1.To_String & """ = """ & P2.To_String & """ /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("baz")), False),
           Make_Path((U("/"), U("foo"), U("bar"), U("baz")), False),
           True);

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("baz")), True),
           Make_Path((U("/"), U("foo"), U("bar"), U("baz")), True),
           True);

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("baz")), True),
           Make_Path((U("/"), U("foo"), U("bar"), U("baz")), False),
           False);

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("baz")), False),
           Make_Path((U("/"), U("foo"), U("bar"), U("baz")), True),
           False);

      Test(Make_Path((1 => U("/")), False),
           Make_Path((1 => U("/")), False),
           True);

      Test(Make_Path(Empty, False),
           Make_Path(Empty, False),
           True);


      Test(Make_Path((U("foo"), U("bar")), False),
           Make_Path((U("foo"), U("baz")), False),
           False);

      Test(Make_Path((U("foos"), U("bar")), False),
           Make_Path((U("foo"), U("bar")), False),
           False);

--  begin read only
   end Test_Equal;
--  end read only


--  begin read only
   procedure Test_Is_Subpath (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Is_Subpath_1aeff2 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Is_Subpath;
--  id:2.2/1aeff26eb9b3a550/Is_Subpath/1/0/
   procedure Test_Is_Subpath (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:73:4:Is_Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : Boolean) is
      begin
         Test_Util.Assert
           ((P1.Is_Subpath(P2)) = Expected,
            "Is_Subpath(""" & P1.To_String & """, """ & P2.To_String & """) /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("baz"), U("file.txt")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           True);

      Test(Make_Path((U("/"), U("foo"), U("baz"), U("file.txt")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           False);


      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path((U("/"), U("foo"), U("bar")), True),
           False);

      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           False);

      Test(Make_Path((U("/"), U("foo"), U("bar.txt")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           False);


      Test(Make_Path((1 => U("foo")), False),
           Make_Path(Empty, False),
           True);

      Test(Make_Path(Empty, False),
           Make_Path((1 => U("foo")), False),
           False);

      Test(Make_Path(Empty, False),
           Make_Path(Empty, False),
           False);

--  begin read only
   end Test_Is_Subpath;
--  end read only


--  begin read only
   procedure Test_Is_Child (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Test_Is_Child_6742e3 (Gnattest_T : in out Test_Canonicalized_Path) renames Test_Is_Child;
--  id:2.2/6742e3f8d31e1762/Is_Child/1/0/
   procedure Test_Is_Child (Gnattest_T : in out Test_Canonicalized_Path) is
   --  paths-canonical_paths.ads:75:4:Is_Child
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      procedure Test(P1, P2 : Canonicalized_Path; Expected : Boolean) is
      begin
         Test_Util.Assert
           ((P1.Is_Child(P2)) = Expected,
            "Is_Child(""" & P1.To_String & """, """ & P2.To_String & """) /= " & Expected'Image);
      end Test;

      Empty : Component_Array(1 .. 0);

   begin

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("file.txt")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           True);

      Test(Make_Path((U("/"), U("foo"), U("bar"), U("baz"), U("file.txt")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           False);

      Test(Make_Path((U("/"), U("foo"), U("baz"), U("file.txt")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           False);


      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path((U("/"), U("foo"), U("bar")), True),
           False);

      Test(Make_Path((U("/"), U("foo"), U("bar")), False),
           Make_Path((U("/"), U("foo"), U("bar")), False),
           False);


      Test(Make_Path((1 => U("foo")), False),
           Make_Path(Empty, False),
           True);

      Test(Make_Path(Empty, False),
           Make_Path((1 => U("foo")), False),
           False);

      Test(Make_Path(Empty, False),
           Make_Path(Empty, False),
           False);

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
end Paths.Canonical_Paths.Canonicalized_Path_Test_Data.Canonicalized_Path_Tests;
