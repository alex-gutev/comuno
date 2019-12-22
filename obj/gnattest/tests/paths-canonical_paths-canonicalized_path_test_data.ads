--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Paths.Canonical_Path_Type_Test_Data.Canonical_Path_Type_Tests;

with GNATtest_Generated;

package Paths.Canonical_Paths.Canonicalized_Path_Test_Data is

--  begin read only
   type Test_Canonicalized_Path is new
     GNATtest_Generated.GNATtest_Standard.Paths.Canonical_Path_Type_Test_Data.Canonical_Path_Type_Tests.Test_Canonical_Path_Type
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test_Canonicalized_Path);
   procedure Tear_Down (Gnattest_T : in out Test_Canonicalized_Path);


   -- Utility Functions --

   --
   -- Vector
   --
   --  Create a vector from an array of path components.
   --
   function Vector (Components : Component_Array) return Component_Vector;

   --
   -- Make_Path
   --
   --  Create a Canonicalized_Path from an array of path components.
   --
   function Make_Path (Components : Component_Array; Directory : Boolean := False) return Canonicalized_Path;

   --
   -- To_String
   --
   --  Return a string displaying the contents of an array of path
   --  components.
   --
   function To_String (Components : Component_Array) return String;

end Paths.Canonical_Paths.Canonicalized_Path_Test_Data;
