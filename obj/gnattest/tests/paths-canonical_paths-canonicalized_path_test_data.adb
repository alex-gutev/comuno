--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Paths.Canonical_Paths.Canonicalized_Path_Test_Data is

   Local_Canonicalized_Path : aliased GNATtest_Generated.GNATtest_Standard.Paths.Canonical_Paths.Canonicalized_Path;
   procedure Set_Up (Gnattest_T : in out Test_Canonicalized_Path) is
   begin
      GNATtest_Generated.GNATtest_Standard.Paths.Canonical_Path_Type_Test_Data.Canonical_Path_Type_Tests.Set_Up
        (GNATtest_Generated.GNATtest_Standard.Paths.Canonical_Path_Type_Test_Data.Canonical_Path_Type_Tests.Test_Canonical_Path_Type (Gnattest_T));
      Gnattest_T.Fixture := Local_Canonicalized_Path'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Canonicalized_Path) is
   begin
      GNATtest_Generated.GNATtest_Standard.Paths.Canonical_Path_Type_Test_Data.Canonical_Path_Type_Tests.Tear_Down
        (GNATtest_Generated.GNATtest_Standard.Paths.Canonical_Path_Type_Test_Data.Canonical_Path_Type_Tests.Test_Canonical_Path_Type (Gnattest_T));
   end Tear_Down;


   -- Utility Functions --

   function Vector(Components : Component_Array) return Component_Vector is
      Vec : Component_Vector;
   begin
      for I in Components'Range loop
         Vec.Append(Components(I));
      end loop;

      return Vec;
   end Vector;

   function Make_Path (Components : Component_Array; Directory : Boolean := False) return Canonicalized_Path is
   begin
      return Make_Path(Vector(Components), Directory);
   end Make_Path;

   function To_String (Components : Component_Array) return String is
      package U renames Ada.Strings.Unbounded;

      Result : U.Unbounded_String;
   begin

      U.Append(Result, "(");

      if Components'Length > 0 then
         U.Append(Result, Components(Components'First));

         for I in Components'First + 1 .. Components'Last loop
            U.Append(Result, ", ");
            U.Append(Result, Components(I));
         end loop;
      end if;

      U.Append(Result, ")");
      return U.To_String(Result);
   end To_String;

end Paths.Canonical_Paths.Canonicalized_Path_Test_Data;
