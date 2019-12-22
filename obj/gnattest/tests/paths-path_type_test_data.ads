--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

with Paths.String_Paths;
with Paths.Canonical_Paths;

package Paths.Path_Type_Test_Data is

   type Path_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Paths.Path_Type'Class;

--  begin read only
   type Test_Path_Type is abstract new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Path_Type_Access;
   end record;


   -- Utility Functions --

   --
   -- Set_Path
   --
   --  Set the contents of a Path object.
   --
   --  If Path is a String_Path, assigns Path to the result of
   --  Make_Path(Name, Directory).
   --
   --  If Path is a Canonicalized_Path, assigns Path to the result of
   --  Make_Path(Name).Canonicalize(Directory).
   --
   procedure Set_Path (Path : in out Path_Type'Class; Name : in Path_String; Directory : in Boolean);

end Paths.Path_Type_Test_Data;
