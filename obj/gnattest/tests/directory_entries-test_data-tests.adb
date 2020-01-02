--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Directory_Entries.Test_Data.

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

--  begin read only
--  end read only
package body Directory_Entries.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

   function Unbounded (S : String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   use type File_System.File_Type;
   use type File_System.Device_Id;
   use type File_System.Inode_Id;
   use type File_System.Size_Type;

   procedure Assert_Equal_Type is
     new Test_Util.Assert_Equal (File_System.File_Type, File_System.File_Type'Image);

   procedure Assert_Equal_Device is
     new Test_Util.Assert_Equal (File_System.Device_Id, File_System.Device_Id'Image);

   procedure Assert_Equal_Inode is
     new Test_Util.Assert_Equal (File_System.Inode_Id, File_System.Inode_Id'Image);

   procedure Assert_Equal_Size is
      new Test_Util.Assert_Equal (File_System.Size_Type, File_System.Size_Type'Image);

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Make_Entry (Gnattest_T : in out Test);
   procedure Test_Make_Entry_158947 (Gnattest_T : in out Test) renames Test_1_Make_Entry;
--  id:2.2/158947be67bc41b6/Make_Entry/1/0/
   procedure Test_1_Make_Entry (Gnattest_T : in out Test) is
   --  directory_entries.ads:33:4:Make_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ent : Directory_Entry :=
        Make_Entry((Name   => Unbounded("foo.txt"), Kind => File_System.Regular),
                   (Device => 1, Inode => 2,
                    Kind   => File_System.Regular,
                    others => <>));

   begin

      Test_Util.Assert_Equal_String
        (Original_Subpath(Ent).To_String, "foo.txt",
         "Original_Subpath");

      Assert_Equal_Type(Kind(Ent), File_System.Regular, "Kind");

      Assert_Equal_Device(Attributes(Ent).Device, 1, "Attributes.Device");
      Assert_Equal_Inode(Attributes(Ent).Inode, 2, "Attributes.Inode");

--  begin read only
   end Test_1_Make_Entry;
--  end read only


--  begin read only
   procedure Test_2_Make_Entry (Gnattest_T : in out Test);
   procedure Test_Make_Entry_f6b582 (Gnattest_T : in out Test) renames Test_2_Make_Entry;
--  id:2.2/f6b582abb00e1fdb/Make_Entry/0/0/
   procedure Test_2_Make_Entry (Gnattest_T : in out Test) is
   --  directory_entries.ads:45:4:Make_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ent : Directory_Entry :=
        Make_Entry
          (Paths.Make_Path("/usr/local/bin"), File_System.Directory);

   begin

      Test_Util.Assert_Equal_String
        (Original_Subpath(Ent).To_String, "/usr/local/bin",
         "Original_Subpath");

      Assert_Equal_Type(Kind(Ent), File_System.Directory, "Kind");

--  begin read only
   end Test_2_Make_Entry;
--  end read only


--  begin read only
   procedure Test_Subpath (Gnattest_T : in out Test);
   procedure Test_Subpath_dd66b3 (Gnattest_T : in out Test) renames Test_Subpath;
--  id:2.2/dd66b37067a4872d/Subpath/1/0/
   procedure Test_Subpath (Gnattest_T : in out Test) is
   --  directory_entries.ads:53:4:Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path : String; Kind : File_System.File_Type; Expected : String) is
         Ent : Directory_Entry := Make_Entry(Paths.Make_Path(Path), Kind);
      begin
         Test_Util.Assert_Equal_String
           (Subpath(Ent).To_String, Expected, "Entry(""" & Path & """)");
      end Test;

   begin

      Test("./path/../bar/./baz/file.txt", File_System.Regular, "bar/baz/file.txt");
      Test("/foo/bar/../baz/./", File_System.Directory, "/foo/baz");

--  begin read only
   end Test_Subpath;
--  end read only


--  begin read only
   procedure Test_Original_Subpath (Gnattest_T : in out Test);
   procedure Test_Original_Subpath_8aa4f9 (Gnattest_T : in out Test) renames Test_Original_Subpath;
--  id:2.2/8aa4f9fc317f22b8/Original_Subpath/1/0/
   procedure Test_Original_Subpath (Gnattest_T : in out Test) is
   --  directory_entries.ads:61:4:Original_Subpath
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Path : String; Kind : File_System.File_Type) is
         Ent : Directory_Entry := Make_Entry(Paths.Make_Path(Path), Kind);
      begin
         Test_Util.Assert_Equal_String
           (Original_Subpath(Ent).To_String, Path, "Entry(""" & Path & """)");
      end Test;

   begin

      Test("./path/../bar/./baz/file.txt", File_System.Regular);
      Test("/foo/bar/../baz/./", File_System.Directory);

--  begin read only
   end Test_Original_Subpath;
--  end read only


--  begin read only
   procedure Test_Entry_Type (Gnattest_T : in out Test);
   procedure Test_Entry_Type_addfd0 (Gnattest_T : in out Test) renames Test_Entry_Type;
--  id:2.2/addfd03c39bbd222/Entry_Type/1/0/
   procedure Test_Entry_Type (Gnattest_T : in out Test) is
   --  directory_entries.ads:69:4:Entry_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert_Equal_Type
        (Entry_Type(Make_Entry(Paths.Make_Path("foo"), File_System.Link)),
         File_System.Link,
        "Make_Entry(""foo"", Link)");

      Assert_Equal_Type
        (Entry_Type
           (Make_Entry
              ((Name => Unbounded("foo"), Kind => File_System.Link),
               (Kind => File_System.Regular, others => <>))),
         File_System.Link,
         "Make_Entry(Entry(""foo"", Link), Attributes(Kind => Regular))");


--  begin read only
   end Test_Entry_Type;
--  end read only


--  begin read only
   procedure Test_Kind (Gnattest_T : in out Test);
   procedure Test_Kind_0934a2 (Gnattest_T : in out Test) renames Test_Kind;
--  id:2.2/0934a29b0eceb0ff/Kind/1/0/
   procedure Test_Kind (Gnattest_T : in out Test) is
   --  directory_entries.ads:78:4:Kind
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert_Equal_Type
        (Kind(Make_Entry(Paths.Make_Path("foo"), File_System.Link)),
         File_System.Link,
        "Make_Entry(""foo"", Link)");

      Assert_Equal_Type
        (Kind
           (Make_Entry
              ((Name => Unbounded("foo"), Kind => File_System.Link),
               (Kind => File_System.Regular, others => <>))),
         File_System.Regular,
         "Make_Entry(Entry(""foo"", Link), Attributes(Kind => Regular))");

      Assert_Equal_Type
        (Kind
           (Make_Entry
              ((Name => Unbounded("foo"), Kind => File_System.Link),
               (Kind => File_System.Unknown, others => <>))),
         File_System.Link,
         "Make_Entry(Entry(""foo"", Link), Attributes(Kind => Unknown))");

--  begin read only
   end Test_Kind;
--  end read only


--  begin read only
   procedure Test_Attributes (Gnattest_T : in out Test);
   procedure Test_Attributes_791776 (Gnattest_T : in out Test) renames Test_Attributes;
--  id:2.2/791776c38cc4235a/Attributes/1/0/
   procedure Test_Attributes (Gnattest_T : in out Test) is
   --  directory_entries.ads:85:4:Attributes
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ent : Directory_Entry :=
        Make_Entry((Name      => Unbounded("foo.txt"), Kind => File_System.Link),
                   (Kind      => File_System.Regular,
                    Device    => 100,
                    Inode     => 283,
                    Num_Links => 2,
                    Size      => 256,
                    others    => <>));

      Attrs : File_System.Attributes := Attributes(Ent);

   begin

      Assert_Equal_Type (Attrs.Kind, File_System.Regular, "Kind");
      Assert_Equal_Device (Attrs.Device, 100, "Device");
      Assert_Equal_Inode (Attrs.Inode, 283, "Inode");
      Assert_Equal_Size (Attrs.Num_Links, 2, "Num_Links");
      Assert_Equal_Size (Attrs.Size, 256, "Size");

--  begin read only
   end Test_Attributes;
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
end Directory_Entries.Test_Data.Tests;
