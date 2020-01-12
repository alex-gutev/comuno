--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Hierarchical_Directory_Trees.Directory_Tree_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Strings.Unbounded;
with Test_Util;
with File_System;

--  begin read only
--  end read only
package body Hierarchical_Directory_Trees.Directory_Tree_Test_Data.Directory_Tree_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

   use type File_System.File_Type;
   use type File_System.Size_Type;

   function Unbounded (S : String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Assert_Equal_Type is
     new Test_Util.Assert_Equal (File_System.File_Type, File_System.File_Type'Image);

   procedure Assert_Equal_Size is
     new Test_Util.Assert_Equal (File_System.Size_Type, File_System.Size_Type'Image);

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Add_Entry_63afff (Gnattest_T : in out Test_Directory_Tree) renames Test_Add_Entry;
--  id:2.2/63afff00f846b632/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:39:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;

      Tree : Directory_Tree;

   begin

      -- TODO: Add duplicate entry tests

      -- Add File Entry

      Tree.Add_Entry
        (Make_Entry
           ((Name => Unbounded("foo.txt"),
             Kind => File_System.Link),
            (Kind => File_System.Regular,
             Size => 120,
             others => <>)));

      -- Add entry with directory component

      Tree.Add_Entry
        (Make_Entry
           ((Name => Unbounded("foo/bar.zip"),
             Kind => File_System.Regular),
            (Kind => File_System.Regular,
             Size => 564,
             others => <>)));

      -- Add Directory Entry

      Tree.Add_Entry
        (Make_Entry
           ((Name => Unbounded("foo/bar/./"),
             Kind => File_System.Directory),
            (Kind => File_System.Directory,
             Num_Links => 2,
             others => <>)));

      Tree.Add_Entry
        (Make_Entry
           ((Name => Unbounded("foo/./foobar/.././bar/baz"),
             Kind => File_System.Regular),
            (Kind => File_System.Regular,
             Size => 1024,
             others => <>)));


      -- Test File Entry

      declare
         Ent   : Directory_Entry        := Tree.Get_Entry(Paths.Make_Path("foo.txt"));
         Attrs : File_System.Attributes := Attributes(Ent);

      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, "foo.txt",
            "Subpath");

         Assert_Equal_Type(Entry_Type(Ent), File_System.Link, "Entry_Type");
         Assert_Equal_Type(Attrs.Kind, File_System.Regular, "Attributes.Kind");
         Assert_Equal_Size(Attrs.Size, 120, "Attributes.Size");
      end;

      -- Test File Entry in Subdirectory

      declare
         Ent   : Directory_Entry        := Tree.Get_Entry(Paths.Make_Path("foo/bar.zip"));
         Attrs : File_System.Attributes := Attributes(Ent);

      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, "foo/bar.zip",
            "Subpath");

         Assert_Equal_Type(Entry_Type(Ent), File_System.Regular, "Entry_Type");
         Assert_Equal_Type(Attrs.Kind, File_System.Regular, "Attributes.Kind");
         Assert_Equal_Size(Attrs.Size, 564, "Attributes.Size");
      end;

      -- Test 'foo' Directory Component Entry

      declare
         Ent : Directory_Entry := Tree.Get_Entry(Paths.Make_Path("foo/"));

      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, "foo",
            "Subpath");

         Assert_Equal_Type(Kind(Ent), File_System.Directory, "Kind");
      end;

      -- Test 'bar' Directory Entry

      declare
         Ent : Directory_Entry := Tree.Get_Entry(Paths.Make_Path("foo/bar/"));
         Attrs : File_System.Attributes := Attributes(Ent);

      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, "foo/bar",
            "Subpath");

         Test_Util.Assert_Equal_String
           (Original_Subpath(Ent).To_String, "foo/bar/./",
            "Original_Subpath");

         Assert_Equal_Type(Entry_Type(Ent), File_System.Directory, "Entry_Type");
         Assert_Equal_Type(Kind(Ent), File_System.Directory, "Kind");
         Assert_Equal_Size(Attrs.Num_Links, 2, "Attributes.Num_Links");
      end;

      -- Test 'foo/bar/baz' Entry

      declare
         Ent   : Directory_Entry        := Tree.Get_Entry(Paths.Make_Path("foo/bar/baz"));
         Attrs : File_System.Attributes := Attributes(Ent);

      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, "foo/bar/baz",
            "Subpath");

         Test_Util.Assert_Equal_String
           (Original_Subpath(Ent).To_String, "foo/./foobar/.././bar/baz",
            "Original_Subpath");

         Assert_Equal_Type(Entry_Type(Ent), File_System.Regular, "Entry_Type");
         Assert_Equal_Type(Attrs.Kind, File_System.Regular, "Attributes.Kind");
         Assert_Equal_Size(Attrs.Size, 1024, "Attributes.Size");
      end;


--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Subpath (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Subpath_274e69 (Gnattest_T : in out Test_Directory_Tree) renames Test_Subpath;
--  id:2.2/274e69df42372750/Subpath/1/0/
   procedure Test_Subpath (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:42:4:Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tree : Directory_Tree;

   begin

      Test_Util.Assert_Equal_String(Tree.Subpath.To_String, "");

--  begin read only
   end Test_Subpath;
--  end read only


--  begin read only
   procedure Test_Set_Subpath (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Set_Subpath_e2dd9e (Gnattest_T : in out Test_Directory_Tree) renames Test_Set_Subpath;
--  id:2.2/e2dd9e0e715f4d99/Set_Subpath/1/0/
   procedure Test_Set_Subpath (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:44:4:Set_Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;

      Tree : Directory_Tree;

   begin

      -- Add Entries --

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar.txt"), File_System.Regular));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar/baz"), File_System.Directory));


      -- Perform Tests --

      Tree.Set_Subpath(Paths.Make_Path("foo"));
      Test_Util.Assert_Equal_String
        (Tree.Subpath.To_String, "foo");

      Tree.Set_Subpath(Paths.Make_Path("foo/bar"));
      Test_Util.Assert_Equal_String
        (Tree.Subpath.To_String, "foo/bar");

      Tree.Set_Subpath(Paths.Make_Path("foo/bar/baz"));
      Test_Util.Assert_Equal_String
        (Tree.Subpath.To_String, "foo/bar/baz");


      -- Test Non-Existent Directory

      begin
         Tree.Set_Subpath(Paths.Make_Path("not/a/dir"));
         Test_Util.Assert
           (False,
            "Exception not raised when setting tree subdir to non-existent directory");

      exception
         when Directory_Trees.Not_Subdirectory => null;
      end;

--  begin read only
   end Test_Set_Subpath;
--  end read only


--  begin read only
   procedure Test_Is_Subdir (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Is_Subdir_2bcc8d (Gnattest_T : in out Test_Directory_Tree) renames Test_Is_Subdir;
--  id:2.2/2bcc8d88bb7f1b30/Is_Subdir/1/0/
   procedure Test_Is_Subdir (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:47:4:Is_Subdir
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;

      Tree : Directory_Tree;

      procedure Test (Path : String; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Tree.Is_Subdir(Tree.Get_Entry(Paths.Make_Path(Path))) = Expected,
            "Is_Subdir(Get_Entry(""" & Path & """)) /= " & Expected'Image);
      end Test;

   begin

      -- Add Entries --

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar.txt"), File_System.Regular));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar/baz"), File_System.Directory));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar/baz"), File_System.Regular));


      -- Perform Tests --

      Test("foo/", True);
      Test("foo/bar/", True);
      Test("foo/bar/baz/", True);
      Test("foo/bar/baz", False);
      Test("foo/bar.txt", False);

--  begin read only
   end Test_Is_Subdir;
--  end read only


--  begin read only
   procedure Test_At_Basedir (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_At_Basedir_878d17 (Gnattest_T : in out Test_Directory_Tree) renames Test_At_Basedir;
--  id:2.2/878d17ebe0f3f2d6/At_Basedir/1/0/
   procedure Test_At_Basedir (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:49:4:At_Basedir
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tree : Directory_Tree;

   begin

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar.txt"), File_System.Regular));

      Test_Util.Assert
        (Tree.At_Basedir, "At_Basedir("""") not True");

      Tree.Set_Subpath(Paths.Make_Path("foo"));

      Test_Util.Assert
        (not Tree.At_Basedir, "At_Basedir(""foo"") not False");

--  begin read only
   end Test_At_Basedir;
--  end read only


--  begin read only
   procedure Test_Has_Entry (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Has_Entry_e73fef (Gnattest_T : in out Test_Directory_Tree) renames Test_Has_Entry;
--  id:2.2/e73fefe8f46383d9/Has_Entry/1/0/
   procedure Test_Has_Entry (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:52:4:Has_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;

      Tree : Directory_Tree;

      procedure Test (Path : String; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Tree.Has_Entry(Paths.Make_Path(Path)) = Expected,
            "Has_Entry(""" & Path & """) /= " & Expected'Image);
      end Test;

   begin

      -- TODO: Add duplicate entry tests

      -- Add Entries --

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar.txt"), File_System.Regular));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo/bar/baz"), File_System.Directory));

      -- Perform Tests

      Test("foo/bar.txt", True);
      Test("foo/bar/baz/", True);
      Test("foo/", True);
      Test("foo/bar/", True);
      Test("dir", False);
      Test("dir/file", False);

--  begin read only
   end Test_Has_Entry;
--  end read only


--  begin read only
   procedure Test_Get_Entry (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Get_Entry_0cf739 (Gnattest_T : in out Test_Directory_Tree) renames Test_Get_Entry;
--  id:2.2/0cf73985b33f0f35/Get_Entry/1/0/
   procedure Test_Get_Entry (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:54:4:Get_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;

      Tree : Directory_Tree;

      procedure Test (Path : String; Entry_Type : File_System.File_Type) is
         Ent : Directory_Entry := Tree.Get_Entry(Paths.Make_Path(Path));

      begin
         Assert_Equal_Type
           (Kind(Ent), Entry_Type,
            "Kind(Get_Entry(""" & Path & """)");
      end Test;


   begin

      -- Add Entries to Tree --

      -- Duplicate Entries --

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo.txt"), File_System.Regular));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo.txt"), File_System.Link));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("foo.txt"), File_System.Directory));


      -- Irregular Paths --

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("/"), File_System.Directory));

      Tree.Add_Entry
        (Make_Entry(Paths.Make_Path("../bar"), File_System.Regular));


      -- Perform Tests --

      Test("foo.txt", File_System.Regular);
      Test("foo.txt/", File_System.Directory);

      Test("/", File_System.Directory);
      Test("../bar", File_System.Regular);


      declare
         Dummy : Directory_Entry;

      begin
         Dummy := Tree.Get_Entry(Paths.Make_Path("foobar.txt"));
         Test_Util.Assert
           (False, "Exception not raised when retrieving entry not in tree");

      exception
         when Directory_Trees.Entry_Not_In_Tree => null;
      end;

--  begin read only
   end Test_Get_Entry;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test_Directory_Tree);
   procedure Test_Iterate_554263 (Gnattest_T : in out Test_Directory_Tree) renames Test_Iterate;
--  id:2.2/554263ce76d46b8e/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test_Directory_Tree) is
   --  hierarchical_directory_trees.ads:57:4:Iterate
--  end read only

      pragma Unreferenced (Gnattest_T);

      pragma Assertion_Policy (Check);

      use Directory_Entries;
      use type File_System.Attributes;

      Tree : Directory_Tree;

      type Entry_Array is array (Positive range <>) of Directory_Entry;
      type Visited_Array is array (Positive range <>) of Boolean;


      -- Entries in Base Directory

      Base_Entries : Entry_Array :=
        (Make_Entry((Name => Unbounded("foo.txt"), Kind => File_System.Link),
                    (Kind => File_System.Regular, Size => 56, others => <>)),

         Make_Entry((Name => Unbounded("bar"), Kind => File_System.Directory),
                    (Kind => File_System.Directory,
                     Num_Links => 10,
                     others => <>)),

         Make_Entry((Name => Unbounded("baz.zip"), Kind => File_System.Regular),
                    (Kind => File_System.Unknown,
                     Inode => 134,
                     others => <>)));

      Base_Visited : Visited_Array := (False, False, False);


      -- Entries in bar Directory

      Bar_Entries : Entry_Array :=
        (Make_Entry((Name => Unbounded("bar/file1.txt"), Kind => File_System.Regular),
                    (Kind => File_System.Regular, Size => 1024, others => <>)),

         Make_Entry((Name => Unbounded("bar/dir1"), Kind => File_System.Directory),
                    (Kind => File_System.Directory, Num_Links => 10, others => <>)));

      Bar_Visited : Visited_Array := (False, False, False);


      -- Iterate Test Functions --

      function Get_Entry_Index (Name : String; Entries : Entry_Array) return Positive is
      begin
         for Index in Entries'Range loop
            if Name = Subpath(Entries(Index)).To_String then
               return Index;
            end if;
         end loop;

         pragma Assert(False, "Iterated over entry " & Name & " which was not added to tree");
      end Get_Entry_Index;


      procedure Test_Base (Ent : Directory_Entry) is
         Index : Positive := Get_Entry_Index(Subpath(Ent).To_String, Base_Entries);
         Original_Entry : Directory_Entry := Base_Entries(Index);

      begin

         -- Ensure that entry is only iterated over once --

         Test_Util.Assert
           (not Base_Visited(Index),
            "Iterating over entry " & Subpath(Ent).To_String & " Twice",
            Fail => true);

         Base_Visited(Index) := True;


         -- Test Attributes --

         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, Subpath(Original_Entry).To_String,
            "Subpath");

         Assert_Equal_Type
           (Entry_Type(Ent), Entry_Type(Original_Entry),
            "Entry_Type");

         Test_Util.Assert
           (Attributes(Ent) = Attributes(Original_Entry),
            "Attributes not equal");

      end Test_Base;

      procedure Test_Bar (Ent : Directory_Entry) is
         Index : Positive := Get_Entry_Index(Subpath(Ent).To_String, Bar_Entries);
         Original_Entry : Directory_Entry := Bar_Entries(Index);

      begin

         -- Ensure that entry is only iterated over once --

         Test_Util.Assert
           (not Bar_Visited(Index),
            "Iterating over entry " & Subpath(Ent).To_String & " Twice",
            Fail => true);

         Bar_Visited(Index) := True;


         -- Test Attributes --

         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, Subpath(Original_Entry).To_String,
            "Subpath");

         Assert_Equal_Type
           (Entry_Type(Ent), Entry_Type(Original_Entry),
            "Entry_Type");

         Test_Util.Assert
           (Attributes(Ent) = Attributes(Original_Entry),
            "Attributes not equal");

      end Test_Bar;

   begin

      -- TODO: Add duplicate entry tests

      -- Initialize Tree --

      for Ent of Base_Entries loop
         Tree.Add_Entry(Ent);
      end loop;

      for Ent of Bar_Entries loop
         Tree.Add_Entry(Ent);
      end loop;


      -- Test Base Directory --

      Tree.Iterate(Test_Base'Access);

      -- Check that all Entries in Base were visited --

      for Index in Base_Entries'Range loop
         Test_Util.Assert
           (Base_Visited(Index),
            Subpath(Base_Entries(Index)).To_String & " not visited");
      end loop;


      -- Test Bar Directory --

      Tree.Set_Subpath(Paths.Make_Path("bar"));
      Tree.Iterate(Test_Bar'Access);

      --  -- Check that all Entries in Bar were visited --

      for Index in Bar_Entries'Range loop
         Test_Util.Assert
           (Bar_Visited(Index),
            Subpath(Bar_Entries(Index)).To_String & " not visited");
      end loop;

--  begin read only
   end Test_Iterate;
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
end Hierarchical_Directory_Trees.Directory_Tree_Test_Data.Directory_Tree_Tests;
