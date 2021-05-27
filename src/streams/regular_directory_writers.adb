--  Copyright (C) 2021 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Interfaces.C.Strings;

with Error_Handling;
with File_Streams;

package body Regular_Directory_Writers is

   package C renames Interfaces.C;

   use type C.Int;

   subtype Limited_Controlled is Ada.Finalization.Limited_Controlled;

   procedure Try_Open (Op : access procedure) renames
     Error_Handling.Try_Op;


   --- Imported Functions ---

   -- Opening and Closing --

   function Dir_Writer_Open (Path : C.Char_Array) return C.Int;
   pragma Import (C, Dir_Writer_Open);

   function Dir_Writer_Close (Dir : C.Int) return C.Int;
   pragma Import (C, Dir_Writer_Close);


   -- Creating Files and Directories --

   function Dir_Writer_Make_Dir (Dir : C.Int; Path : C.Char_Array)
                                return C.Int;
   pragma Import (C, Dir_Writer_Make_Dir);

   function Dir_Writer_Symlink
     (Dir        : C.Int;
      Path       : C.Char_Array;
      Target     : C.Char_Array;
      Attributes : C_Types.Attributes)
     return C.Int;
   pragma Import (C, Dir_Writer_Symlink);


   -- Renaming and Deleting Files --

   function Dir_Writer_Check (Dir : C.Int; Path : C.Char_Array) return C.Int;
   pragma Import(C, Dir_Writer_Check);

   function Dir_Writer_Rename (Dir : C.Int; Src : C.Char_Array; Dest : C.Char_Array)
                              return C.Int;
   pragma Import (C, Dir_Writer_Rename);

   function Dir_Writer_Remove (Dir : C.Int; Src : C.Char_Array)
                              return C.Int;
   pragma Import (C, Dir_Writer_Remove);


   -- Setting Attributes --

   function Dir_Writer_Set_Mode (Dir : C.Int; Path : C.Char_Array; Mode : C_Types.Attribute)
                                return C.Int;
   pragma Import (C, Dir_Writer_Set_Mode);

   function Dir_Writer_Set_Owner (Dir        : C.Int;
                                  Path       : C.Char_Array;
                                  Attributes : C_Types.Attributes)
                                 return C.Int;
   pragma Import (C, Dir_Writer_Set_Owner);

   function Dir_Writer_Set_Times (Dir        : C.Int;
                                  Path       : C.Char_Array;
                                  Attributes : C_Types.Attributes)
                                 return C.Int;
   pragma Import (C, Dir_Writer_Set_Times);


   -- Creation --

   function Open_Dir (Path : Paths.Path) return Directory_Writer is
      C_Path : C.Char_Array := C.To_C(Path.To_String);
      Handle : C.Int;

      procedure Open is
      begin
         Handle := Dir_Writer_Open(C_Path);

         if Handle < 0 then
            raise Directory_Writers.Open_Error;
         end if;

      end Open;

   begin
      Try_Open(Open'Access);

      return (Limited_Controlled with Handle => Handle);

   end Open_Dir;


   -- Finalization --

   procedure Close (This : in out Directory_Writer) is
   begin
      if Dir_Writer_Close(This.Handle) /= 0 then
         raise Directory_Writers.Close_Error;
      end if;

      This.Handle := -1;
   end Close;

   procedure Finalize (This : in out Directory_Writer) is
   begin
      if This.Handle >= 0 then
         Dir_Writer_Close(This.Handle);
      end if;
   end Finalize;


   -- File Creation --

   overriding function Create_File (This       : in out Directory_Writer;
                                    Path       : in     Paths.Path;
                                    Attributes : in     File_System.Attributes;
                                    Flags      : in     File_Create_Flags)
                                   return Streams.Outstream is

      -- TODO: Add overwrite restart
      -- TODO: Set exclusive Flag

      Stream : Streams.Outstream :=
        File_Streams.Create(This.Handle, Path, Flags);

   begin

      Stream.Set_Times(Attributes);
      Stream.Set_Attributes(Attributes);

      return Stream;

   end Create_File;


   overriding procedure Make_Directory (This  : in out Directory_Writer;
                                        Path  : in     Paths.Path;
                                        Defer : in     Boolean := True) is
      C_Path : C.Char_Array := C.To_C(Path.To_String);

      procedure Make_Dir is
      begin
         if Dir_Writer_Make_Dir(This.Handle, C_Path) /= 0 then
            raise Directory_Writers.Make_Directory_Error;
         end if;
      end Make_Dir;

   begin
      Try_Op(Make_Dir'Access);

   end Make_Directory;

   overriding procedure Make_Symlink (This       : in out Directory_Writer;
                                      Path       : in     Paths.Path;
                                      Target     : in     Paths.Path;
                                      Attributes : in     File_System.Attributes) is

      C_Path   : C.Char_Array := C.To_C(Path.To_String);
      C_Target : C.Char_Array := C.To_C(Target.To_String);

      procedure Symlink is
      begin
         if Dir_Writer_Symlink(This.Handle, C_Path, C_Target, Attributes) /= 0 then
            raise Directory_Writers.Symlink_Error;
         end if;
      end Symlink;


   begin
      Try_Op(Symlink'Access);

      This.Set_Attributes(Path, Attributes);

   end Make_Symlink;


   -- Setting Attributes --

   overriding procedure Set_Attributes (This       : in out Directory_Writer;
                                        Path       : in     Paths.Path;
                                        Attributes : in     File_System.Attributes) is

      C_Path : C.Char_Array := C.To_C(Path.To_String);
      C_Attr : C_Types.Attributes := C_Types.To_C(Attributes);

      procedure Set_Mode is
      begin
         if Dir_Writer_Set_Mode(This.Handle, C_Path, C_Attr.Mode) /= 0 then
            raise Directory_Writers.Set_Attributes_Error;
         end if;
      end Set_Mode;

      procedure Set_Owner is
      begin
         if Dir_Writer_Set_Owner(This.Handle, C_Path, C_Attr) /= 0 then
            raise Directory_Writers.Set_Attributes_Error;
         end if;
      end Set_Owner;

      procedure Set_Times is
      begin
         if Dir_Writer_Set_Times(This.Handle, C_Path, C_Attr) /= 0 then
            raise Directory_Writers.Set_Attributes_Error;
         end if;
      end Set_Times;

   begin
      Try_Op(Set_Mode'Access);
      Try_Op(Set_Owner'Access);
      Try_Op(Set_Times'Access);

   end Set_Attributes;


   -- Renaming Files --

   procedure Rename (This        : in out Directory_Writer;
                     Source      : in     Paths.Path;
                     Destination : in     Paths.Path) is

      C_Src  : C.Char_Array := C.To_C(Source.To_String);
      C_Dest : C.Char_Array := C.To_C(Destination.To_String);

      procedure Mv is
      begin
         if Dir_Writer_Check(This.Handle, C_Dest) = 0 then
            raise Directory_Writers.Rename_Error;
         end if;

         if Dir_Writer_Rename(This.Handle, C_Src, C_Dest) /= 0 then
            raise Directory_Writers.Rename_Error;
         end if;
      end Mv;

   begin
      Try_Op(Mv'Access);

   end Rename;


   -- Deleting Files ---

   procedure Remove (This     : in out Directory_Writer;
                     Path     : in     Paths.Path;
                     Relative :        Boolean := True) is

      C_Path : C.Char_Array := C.To_C(Path.To_String);

      procedure Del is
      begin
         if Dir_Writer_Remove(This.Handle, C_Path) /= 0 then
            raise Directory_Writers.Remove_Error;
         end if;
      end Del;

   begin
      Try_Op(Del'Access);

   end Remove;

end Regular_Directory_Writers;
