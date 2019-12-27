--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Paths.Path_Test_Data is

   type Path_Access is access all GNATtest_Generated.GNATtest_Standard.Paths.Path'Class;

--  begin read only
   type Test_Path is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Path_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Path);
   procedure Tear_Down (Gnattest_T : in out Test_Path);


   -- Utility Functions --

   --
   -- Set_Path
   --
   --  Set the contents of a Path object.
   --
   --  If Object is of type Path, assigns Object to the result of
   --  Make_Path(Name, Directory).
   --
   --  If Object is a Canonical_Path, assigns Object to the result of
   --  Make_Path(Name).Canonicalize(Directory).
   --
   procedure Set_Path (Object : in out Path'Class; Name : in Path_String);
   procedure Set_Path (Object : in out Path'Class; Name : in Path_String; Directory : in Boolean);

   function Vector (Components : Component_Array) return Component_Vector;
   function To_String (Components : Component_Array) return String;

end Paths.Path_Test_Data;
