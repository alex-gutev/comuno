--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Tags;
with Paths.Canonical_Paths;

package body Paths.Path_Test_Data is

   Local_Path : aliased GNATtest_Generated.GNATtest_Standard.Paths.Path;
   procedure Set_Up (Gnattest_T : in out Test_Path) is
   begin
      Gnattest_T.Fixture := Local_Path'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Path) is
   begin
      null;
   end Tear_Down;


   -- Utilities --

   procedure Set_Path (Object : in out Path'Class; Name : in Path_String) is
   begin
      Set_Path(Object, Name,
               (if Name'Length > 0 then
                   Name(Name'Last) = Separator else
                   False));
   end Set_Path;


   procedure Set_Path (Object : in out Path'Class; Name : in Path_String; Directory : in Boolean) is
      use type Ada.Tags.Tag;

   begin
      if Object'Tag = Path'Tag then
         Object := Path'Class(Make_Path(Name, Directory));

      elsif Object'Tag = Canonical_Paths.Canonical_Path'Tag then
         Object := Path'Class(Canonical_Paths.Canonicalize(Make_Path(Name), Directory));

      end if;
   end Set_Path;

   function Vector(Components : Component_Array) return Component_Vector is
      Vec : Component_Vector;
   begin
      for I in Components'Range loop
         Vec.Append(Components(I));
      end loop;

      return Vec;
   end Vector;

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

end Paths.Path_Test_Data;
