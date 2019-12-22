with Paths.String_Paths;
with Paths.Canonical_Paths;

package body Paths.Path_Type_Test_Data is

   procedure Set_Path (Path : in out Path_Type'Class; Name : in Path_String; Directory : in Boolean) is
   begin

      if Path in String_Paths.String_Path'Class then
         Path := Path_Type'Class(String_Paths.Make_Path(Name, Directory));

      elsif Path in Canonical_Paths.Canonicalized_Path'Class then
         Path := Path_Type'Class(String_Paths.Make_Path(Name).Canonicalize(Directory));

      end if;
   end Set_Path;

end Paths.Path_Type_Test_Data;
