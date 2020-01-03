--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Flat_Directory_Trees.Flat_Directory_Tree_Test_Data.

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
package body Flat_Directory_Trees.Flat_Directory_Tree_Test_Data.Flat_Directory_Tree_Tests is

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
   procedure Test_Add_Entry (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Add_Entry_63afff (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Add_Entry;
--  id:2.2/63afff00f846b632/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:42:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;

      Tree : Flat_Directory_Tree;

   begin

      Tree.Add_Entry
        (Make_Entry
           ((Name   => Unbounded("foo.txt"),
             Kind   => File_System.Link),
            (Kind   => File_System.Regular,
             Size   => 120,
             others => <>)));

      Tree.Add_Entry
        (Make_Entry
           ((Name   => Unbounded("bar.zip"),
             Kind   => File_System.Regular),
            (Kind   => File_System.Regular,
             Size   => 564,
             others => <>)));

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

      declare
         Ent   : Directory_Entry        := Tree.Get_Entry(Paths.Make_Path("bar.zip"));
         Attrs : File_System.Attributes := Attributes(Ent);

      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, "bar.zip",
            "Subpath");

         Assert_Equal_Type(Entry_Type(Ent), File_System.Regular, "Entry_Type");
         Assert_Equal_Type(Attrs.Kind, File_System.Regular, "Attributes.Kind");
         Assert_Equal_Size(Attrs.Size, 564, "Attributes.Size");
      end;

--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Subpath (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Subpath_274e69 (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Subpath;
--  id:2.2/274e69df42372750/Subpath/1/0/
   procedure Test_Subpath (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:45:4:Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tree : Flat_Directory_Tree;

   begin

      Test_Util.Assert_Equal_String
        (Tree.Subpath.To_String, "");

--  begin read only
   end Test_Subpath;
--  end read only


--  begin read only
   procedure Test_Set_Subpath (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Set_Subpath_e2dd9e (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Set_Subpath;
--  id:2.2/e2dd9e0e715f4d99/Set_Subpath/1/0/
   procedure Test_Set_Subpath (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:47:4:Set_Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      -- Nothing to test as Set_Subpath operation should not be called
      -- on a Flat_Directory_Tree.

      null;

--  begin read only
   end Test_Set_Subpath;
--  end read only


--  begin read only
   procedure Test_Is_Subdir (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Is_Subdir_2bcc8d (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Is_Subdir;
--  id:2.2/2bcc8d88bb7f1b30/Is_Subdir/1/0/
   procedure Test_Is_Subdir (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:50:4:Is_Subdir
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tree : Flat_Directory_Tree;

      Ent1 : Directory_Entry :=
        Make_Entry
          ((Name => Unbounded("dir"), Kind => File_System.Directory),
           (Kind => File_System.Directory, others => <>));

      Ent2 : Directory_Entry :=
        Make_Entry
          ((Name => Unbounded("file"), Kind => File_System.Regular),
           (Kind => File_System.Regular, others => <>));

   begin
      Tree.Add_Entry(Ent1);

      Test_Util.Assert(not Tree.Is_Subdir(Ent1), "Is_Subdir(Existing Entry) not False");
      Test_Util.Assert(not Tree.Is_Subdir(Ent2), "Is_Subdir(Non-existing Entry) not False");

--  begin read only
   end Test_Is_Subdir;
--  end read only


--  begin read only
   procedure Test_At_Basedir (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_At_Basedir_878d17 (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_At_Basedir;
--  id:2.2/878d17ebe0f3f2d6/At_Basedir/1/0/
   procedure Test_At_Basedir (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:52:4:At_Basedir
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tree : Flat_Directory_Tree;

   begin

      Assert(Tree.At_Basedir, "At_Basedir not True");

--  begin read only
   end Test_At_Basedir;
--  end read only


--  begin read only
   procedure Test_Has_Entry (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Has_Entry_e73fef (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Has_Entry;
--  id:2.2/e73fefe8f46383d9/Has_Entry/1/0/
   procedure Test_Has_Entry (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:55:4:Has_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Tree : Flat_Directory_Tree;

      procedure Test (Name : String; Expected : Boolean) is
      begin
         Test_Util.Assert
           (Tree.Has_Entry(Paths.Make_Path(Name)) = Expected,
            "Has_Entry(""" & Name & """) /= " & Expected'Image);
      end Test;

   begin

      -- Add Entries to Tree --

      Tree.Add_Entry
        (Make_Entry
           ((Name   => Unbounded("file-a.txt"),
             Kind   => File_System.Link),
            (Kind   => File_System.Regular,
             others => <>)));

      Tree.Add_Entry
        (Make_Entry
           ((Name   => Unbounded("folder"),
             Kind   => File_System.Directory),
            (Kind   => File_System.Directory,
             others => <>)));

      -- Perform Tests --

      Test("file-a.txt", True);
      Test("folder", True);
      Test("foo", False);

--  begin read only
   end Test_Has_Entry;
--  end read only


--  begin read only
   procedure Test_Get_Entry (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Get_Entry_0cf739 (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Get_Entry;
--  id:2.2/0cf73985b33f0f35/Get_Entry/1/0/
   procedure Test_Get_Entry (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:57:4:Get_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Directory_Entries;
      use type File_System.Attributes;

      Tree : Flat_Directory_Tree;

      procedure Test (Name : String;
                      Kind : File_System.File_Type;
                      Attributes : File_System.Attributes) is

         Ent : Directory_Entry := Tree.Get_Entry(Paths.Make_Path(Name));

      begin

         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, Name, "Subpath");

         Assert_Equal_Type(Entry_Type(Ent), Kind, "Entry_Type");

         Test_Util.Assert
           (Directory_Entries.Attributes(Ent) = Attributes,
            "Attributes not equal");

      end Test;

   begin

      -- Add Entries to Tree --

      Tree.Add_Entry
        (Make_Entry
           ((Name   => Unbounded("file-a.txt"),
             Kind   => File_System.Link),
            (Kind   => File_System.Regular,
             Size   => 120,
             others => <>)));

      Tree.Add_Entry
        (Make_Entry
           ((Name   => Unbounded("folder"),
             Kind   => File_System.Directory),
            (Kind   => File_System.Directory,
             Inode  => 876,
             others => <>)));

      -- Perform Tests --

      Test("file-a.txt", File_System.Link,
           (Kind   => File_System.Regular,
            Size   => 120,
            others => <>));

      Test("folder", File_System.Directory,
           (Kind   => File_System.Directory,
            Inode  => 876,
            others => <>));


      -- Test that an exception is raised when retrieving an entry
      -- which is not in the tree.

      declare
         Dummy : Directory_Entry;

      begin
         Dummy := Tree.Get_Entry(Paths.Make_Path("not-in-tree.txt"));

         Test_Util.Assert
           (False,
            "Exception not raised when retrieving entry not in tree.");

      exception
         when Directory_Trees.Entry_Not_In_Tree => null;

      end;

--  begin read only
   end Test_Get_Entry;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test_Flat_Directory_Tree);
   procedure Test_Iterate_554263 (Gnattest_T : in out Test_Flat_Directory_Tree) renames Test_Iterate;
--  id:2.2/554263ce76d46b8e/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test_Flat_Directory_Tree) is
   --  flat_directory_trees.ads:60:4:Iterate
--  end read only

      pragma Unreferenced (Gnattest_T);

      pragma Assertion_Policy (Check);

      use Directory_Entries;
      use type File_System.Attributes;

      Tree : Flat_Directory_Tree;

      type Entry_Key is (Foo, Bar, Baz);

      Entries : array (Entry_Key) of Directory_Entry :=
        (Foo =>
           Make_Entry((Name => Unbounded("foo.txt"), Kind => File_System.Link),
                      (Kind => File_System.Regular, Size => 56, others => <>)),

         Bar =>
           Make_Entry((Name      => Unbounded("bar"), Kind => File_System.Directory),
                      (Kind      => File_System.Directory,
                       Size      => 0,
                       Num_Links => 10,
                       others    => <>)),

         Baz =>
           Make_Entry((Name => Unbounded("baz.zip"), Kind => File_System.Regular),
                      (Kind => File_System.Unknown, Inode => 10, others => <>)));

      Visited : array (Entry_Key) of Boolean := (False, False, False);

      function Get_Entry_Key (Name : String) return Entry_Key is
      begin
         for Key in Entries'Range loop
            if Name = Subpath(Entries(Key)).To_String then
               return Key;
            end if;
         end loop;

         pragma Assert(False, "Iterated over entry " & Name & " which was not added to tree");
      end Get_Entry_Key;

      procedure Test (Ent : Directory_Entry) is
         Key              : Entry_Key       := Get_Entry_Key(Subpath(Ent).To_String);
         Original_Entry   : Directory_Entry := Entries(Key);

      begin

         -- Ensure that entry is only iterated over once --

         Test_Util.Assert
           (not Visited(Key),
            "Iterating over entry" & Subpath(Ent).To_String & " Twice",
            Fail => True);

         Visited(Key) := True;

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

      end Test;

   begin

      -- Initialize Tree --

      for Ent of Entries loop
         Tree.Add_Entry(Ent);
      end loop;


      -- Perform Tests --

      Tree.Iterate(Test'Access);

      -- Check that all Entries were visited --

      for Ent in Entries'Range loop
         Test_Util.Assert
           (Visited(Ent),
            Subpath(Entries(Ent)).To_String & " not visited");
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
end Flat_Directory_Trees.Flat_Directory_Tree_Test_Data.Flat_Directory_Tree_Tests;
