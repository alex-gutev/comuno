--  Copyright (C) 2019 Alexander Gutev
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings;

package body Dir_Lister is
   package C renames Interfaces.C;

   -- Imported C Functions --

   function Open_Dir (Path : C.Char_Array) return C_Types.Handle_Ptr;
   pragma Import(C, Open_Dir);

   procedure Close_Dir (Handle : in C_Types.Handle_Ptr);
   pragma Import(C, Close_Dir);

   function Get_Entry (Handle : in C_Types.Handle_Ptr; Ent : out C_Types.Dir_Entry) return C.Int;
   pragma Import(C, Get_Entry);

   function Get_Attributes (Handle : in C_Types.Handle_Ptr; Name : in C.Char_Array; Attrs : out C_Types.Attributes)
			   return C.Int;
   pragma Import(C, Get_Attributes);


   -- Creation and Finalization --

   use type C_Types.Handle_Ptr;
   procedure Open (This : out Dir_Lister; Path : in Path_String) is
      Handle : C_Types.Handle_Ptr;
   begin
      Handle := Open_Dir(C.To_C(Path));

      if Handle = C.Strings.Null_Ptr then
	 raise Open_Dir_Error;
      end if;

      This.Handle := Handle;
   end Open;

   procedure Finalize (This : in out Dir_Lister) is
   begin
      Close_Dir(This.Handle);
   end Finalize;


   -- Lister Operations --

   use type C.Int;
   function Read_Entry (This : in out Dir_Lister; Ent : out Dir_Entry) return Boolean is
      C_Ent : C_Types.Dir_Entry;
   begin
      if Get_Entry(This.Handle, C_Ent) /= 0 then
	 Ent.Name := To_Unbounded_String(C.Strings.Value(C_Ent.Name));
	 Ent.Kind := File_Type'Val(C_Ent.Kind);

	 This.Last_Entry := Ent;
	 return True;
      end if;

      return False;
   end Read_Entry;

   function Entry_Attributes (This : in out Dir_Lister) return Attributes is
      C_Attrs : C_Types.Attributes;
   begin
      if Get_Attributes(This.Handle, C.To_C(To_String(This.Last_Entry.Name)), C_Attrs) = 0
      then
	 raise Get_Attributes_Error;
      end if;

      return C_Types.Convert(C_Attrs);
   end Entry_Attributes;
end Dir_Lister;
