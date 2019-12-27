--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paths.Canonical_Paths.Canonical_Path_Test_Data.

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
package body Paths.Canonical_Paths.Canonical_Path_Test_Data.Canonical_Path_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Canonicalize (Gnattest_T : in out Test_Canonical_Path);
   procedure Test_Canonicalize_020f3e (Gnattest_T : in out Test_Canonical_Path) renames Test_Canonicalize;
--  id:2.2/020f3ec0719b9935/Canonicalize/1/0/
   procedure Test_Canonicalize (Gnattest_T : in out Test_Canonical_Path) is
   --  paths-canonical_paths.ads:35:4:Canonicalize
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path : Path_String; Directory : Boolean; Expected : Path_String) is
         String_Path : Paths.Path := Make_Path(Path);
         P : Canonical_Path := Canonicalize(String_Path, Directory);

      begin
         Test_Util.Assert_Equal_String
           (P.To_String, Expected,
            "Canonicalize(""" & Path & """, " & Directory'Image & ")");
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
   procedure Test_1_Append (Gnattest_T : in out Test_Canonical_Path);
   procedure Test_Append_26bf90 (Gnattest_T : in out Test_Canonical_Path) renames Test_1_Append;
--  id:2.2/26bf9008adf6766c/Append/1/0/
   procedure Test_1_Append (Gnattest_T : in out Test_Canonical_Path) is
   --  paths-canonical_paths.ads:40:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         String_Path1 : Path := Make_Path(P1);
         String_Path2 : Path := Make_Path(P2);

         Path1 : Canonical_Path := Canonicalize(String_Path1, String_Path1.Is_Directory);
         Path2 : Canonical_Path := Canonicalize(String_Path2, String_Path2.Is_Directory);

      begin
         Test_Util.Assert_Equal_String
           (Path1.Append(Path2).To_String, Expected,
            "Append(""" & P1 & """, """ & P2 & """)");
      end Test;

   begin

      Test("/foo/bar", "baz", "/foo/bar/baz");
      Test("/foo", "bar/baz", "/foo/bar/baz");
      Test("/foo/", "bar/baz", "/foo/bar/baz");
      Test(".", "foo", "foo");
      Test("..", "foo", "../foo");
      Test("~", "foo", "~/foo");
      Test("/", "foo", "/foo");

      Test("foo", "", "foo/");
      Test("", "foo", "foo");

      -- Test mainitaining of canonical representation --

      Test("/foo/bar", "../baz", "/foo/baz");
      Test("/foo/bar/", "../baz", "/foo/baz");
      Test("/foo/bar", "../../", "/");
      Test("/foo/bar", "../../baz", "/baz");

--  begin read only
   end Test_1_Append;
--  end read only


--  begin read only
   procedure Test_2_Append (Gnattest_T : in out Test_Canonical_Path);
   procedure Test_Append_c30aa7 (Gnattest_T : in out Test_Canonical_Path) renames Test_2_Append;
--  id:2.2/c30aa79582e32e23/Append/0/0/
   procedure Test_2_Append (Gnattest_T : in out Test_Canonical_Path) is
   --  paths-canonical_paths.ads:41:4:Append
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         String_Path1 : Path := Make_Path(P1);
         String_Path2 : Path := Make_Path(P2);

         Path1 : Canonical_Path := Canonicalize(String_Path1, String_Path1.Is_Directory);
         Path2 : Canonical_Path := Canonicalize(String_Path2, String_Path2.Is_Directory);

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
      Test(".", "foo", "foo");
      Test("..", "foo", "../foo");
      Test("~", "foo", "~/foo");
      Test("/", "foo", "/foo");

      Test("foo", "", "foo/");
      Test("", "foo", "foo");

      -- Test mainitaining of canonical representation --

      Test("/foo/bar", "../baz", "/foo/baz");
      Test("/foo/bar/", "../baz", "/foo/baz");
      Test("/foo/bar", "../../", "/");
      Test("/foo/bar", "../../baz", "/baz");

--  begin read only
   end Test_2_Append;
--  end read only


--  begin read only
   procedure Test_1_Merge (Gnattest_T : in out Test_Canonical_Path);
   procedure Test_Merge_51c348 (Gnattest_T : in out Test_Canonical_Path) renames Test_1_Merge;
--  id:2.2/51c348cb40ce9d90/Merge/1/0/
   procedure Test_1_Merge (Gnattest_T : in out Test_Canonical_Path) is
   --  paths-canonical_paths.ads:43:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         String_Path1 : Path := Make_Path(P1);
         String_Path2 : Path := Make_Path(P2);

         Path1 : Canonical_Path := Canonicalize(String_Path1, String_Path1.Is_Directory);
         Path2 : Canonical_Path := Canonicalize(String_Path2, String_Path2.Is_Directory);

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

      -- Test mainitaining of canonical representation --

      Test("/foo/bar", "../baz", "/baz");
      Test("/foo/bar/", "../baz", "/foo/baz");
      Test("/foo/bar", "../../", "");
      Test("/foo/bar/", "../../", "/");
      Test("/foo/bar", "../../baz", "baz");

--  begin read only
   end Test_1_Merge;
--  end read only


--  begin read only
   procedure Test_2_Merge (Gnattest_T : in out Test_Canonical_Path);
   procedure Test_Merge_230f40 (Gnattest_T : in out Test_Canonical_Path) renames Test_2_Merge;
--  id:2.2/230f409c6fbd7ad5/Merge/0/0/
   procedure Test_2_Merge (Gnattest_T : in out Test_Canonical_Path) is
   --  paths-canonical_paths.ads:44:4:Merge
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (P1, P2 : Path_String; Expected : Path_String) is
         String_Path1 : Path := Make_Path(P1);
         String_Path2 : Path := Make_Path(P2);

         Path1 : Canonical_Path := Canonicalize(String_Path1, String_Path1.Is_Directory);
         Path2 : Canonical_Path := Canonicalize(String_Path2, String_Path2.Is_Directory);

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

      -- Test mainitaining of canonical representation --

      Test("/foo/bar", "../baz", "/baz");
      Test("/foo/bar/", "../baz", "/foo/baz");
      Test("/foo/bar", "../../", "");
      Test("/foo/bar/", "../../", "/");
      Test("/foo/bar", "../../baz", "baz");

--  begin read only
   end Test_2_Merge;
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
end Paths.Canonical_Paths.Canonical_Path_Test_Data.Canonical_Path_Tests;
