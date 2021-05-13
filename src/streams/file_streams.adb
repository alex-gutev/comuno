--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
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

package body File_Streams is

   use type C_Types.Handle_Ptr;
   use type C.Int;
   use type Size_Type;
   use type Block_Ptr;

   subtype Limited_Controlled is Ada.Finalization.Limited_Controlled;

   procedure Try_Op (Op : access procedure) renames
     Error_Handling.Try_Op;

   --
   -- Size of the buffer to use when reading a block of data from the
   -- file.
   --
   Buffer_Size : constant Size_Type := 131072;


   --- Input Stream ---

   -- Imported Functions --

   function File_Instream_Create (Path     : C.Char_Array;
                                  Buf_Size : Size_Type)
                                 return Handle_Ptr;
   pragma Import (C, File_Instream_Create);

   function File_Instream_Create_At (Dir_Fd   : C.Int;
                                     Path     : C.Char_Array;
                                     Buf_Size : Size_Type)
                                    return Handle_Ptr;
   pragma Import (C, File_Instream_Create_At);

   procedure File_Instream_Close (Handle : Handle_Ptr);
   pragma Import (C, File_Instream_Close);

   function File_Instream_Read (Handle : Handle_Ptr; Block : out Block_Ptr)
                               return C.Int;
   pragma Import (C, File_Instream_Read);


   -- Creation --

   function Create (Path : Paths.Path) return File_Instream is
      C_Path : C.Char_Array := C.To_C(Path.To_String);
      Handle : Handle_Ptr;

      procedure Open is
      begin
         Handle := File_Instream_Create(C_Path, Buffer_Size);

         if Handle = C.Strings.Null_Ptr then
            raise Streams.Open_Error;
         end if;

      end Open;

   begin
      Try_Op(Open'Access);

      return (Limited_Controlled with Handle => Handle);

   end Create;

   function Create (Fd : Cint; Name : Paths.Path) return File_Instream is
      C_Name : C.Char_Array := C.To_C(Name.To_String);
      Handle : Handle_Ptr;

      procedure Open is
      begin
         Handle := File_Instream_Create_At(Fd, C_Name, Buffer_Size);

         if Handle = C.Strings.Null_Ptr then
            raise Streams.Open_Error;
         end if;

      end Open;

   begin
      Try_Op(Open'Access);

      return (Limited_Controlled with Handle => Handle);

   end Create;


   -- Finalization --

   procedure Finalize (This : in out File_Instream) is
   begin
      if This.Handle /= C.Strings.Null_Ptr then
         File_Instream_Close(This.Handle);
      end if;
   end Finalize;


   -- Reading Data --

   overriding function Read (This   : in out File_Instream;
                             Size   :    out Size_Type;
                             Offset :    out Size_Type)
                            return Block_Ptr is

      Block : Block_Ptr;

      procedure Read_Block is
         Bytes_Read : C.Int := File_Instream_Read(This.Handle, Block);

      begin
         if Bytes_Read >= 0 then
            Size   := Size_Type(Bytes_Read);
            Offset := 0;

         else
            raise Streams.Read_Error;

         end if;

      end Read_Block;

   begin
      Try_Op(Read_Block'Access);

      return Block;

   end Read;


   --- Output Stream ---

   function File_Outstream_Create (Path : C.Char_Array; Flags, Permissions : C.Int)
                                  return Handle_Ptr;
   pragma Import (C, File_Outstream_Create);

   function File_Outstream_Create_At (Dir_Fd : C.Int;
                                      Path : C.Char_Array;
                                      Flags, Permissions : C.Int)
                                  return Handle_Ptr;
   pragma Import (C, File_Outstream_Create_At);

   function File_Outstream_Close (Handle : Handle_Ptr) return C.Int;
   pragma Import (C, File_Outstream_Close);

   function File_Outstream_Seek (Handle : Handle_Ptr; Offset : Size_Type) return C.Int;
   pragma Import (C, File_Outstream_Seek);

   function File_Outstream_Write (Handle : Handle_Ptr; Block : Block_Ptr; Size : Size_Type)
                                 return C.Int;
   pragma Import (C, File_Outstream_Write);

   function File_Outstream_Set_Times (Handle      : Handle_Ptr;
                                      Mod_Time    : C_Types.Timestamp;
                                      Access_Time : C_Types.Timestamp)
                                     return C.Int;
   pragma Import (C, File_Outstream_Set_Times);

   function File_Outstream_Set_Mode(Handle : Handle_Ptr; Mode : C_Types.Attribute) return C.Int;
   pragma Import (C, File_Outstream_Set_Mode);

   function File_Outstream_Set_Owner(Handle : Handle_Ptr; Attributes : C_Types.Attributes) return C.Int;
   pragma Import (C, File_Outstream_Set_Owner);


   --
   -- Update_Times
   --
   --  Update the access and modification time of the underlying file
   --  to the values given in the Access_Time and Mod_Time fields.
   --
   procedure Update_Times(This : in File_Outstream);

   -- Initialization --

   function Create (Path        : Paths.Path;
                    Flags       : File_Create_Flags;
                    Permissions : File_Permissions := Directory_Writers.Permission_User_Rwx)
                   return File_Outstream is

      C_Path : C.Char_Array := C.To_C(Path.To_String);
      Handle : Handle_Ptr;

      procedure Open is
      begin
         Handle := File_Outstream_Create
           (C_Path,
            C.Int(Flags),
            C.Int(Directory_Writers.Ada_To_Unix(Permissions)));

         if Handle = C.Strings.Null_Ptr then
            raise Streams.Open_Error;
         end if;

      end Open;

   begin
      Try_Op(Open'Access);

      return (Limited_Controlled with Handle => Handle,
              others => <>);

   end Create;

   function Create (Fd          : Cint;
                    Path        : Paths.Path;
                    Flags       : File_Create_Flags;
                    Permissions : File_Permissions := Directory_Writers.Permission_User_Rwx)
                   return File_Outstream is

      C_Path : C.Char_Array := C.To_C(Path.To_String);
      Handle : Handle_Ptr;

      procedure Open is
      begin
         Handle := File_Outstream_Create_At
           (Fd, C_Path,
            C.Int(Flags),
            C.Int(Directory_Writers.Ada_To_Unix(Permissions)));

         if Handle = C.Strings.Null_Ptr then
            raise Streams.Open_Error;
         end if;

      end Open;

   begin
      Try_Op(Open'Access);

      return (Limited_Controlled with Handle => Handle, others => <>);

   end Create;


   -- Finalization --

   function Close_Outstream (This : in out File_Outstream) return Boolean is
   begin
      if This.Handle /= C.Strings.Null_Ptr then
         return File_Outstream_Close(This.Handle) = 0;
      end if;

      return True;
   end Close_Outstream;


   procedure Finalize (This : in out File_Outstream) is
      Dummy : Boolean;

   begin
      Dummy := Close_Outstream(This);

   end Finalize;


   overriding procedure Close (This : in out File_Outstream) is
   begin
      if This.Set_Times then
         Update_Times(This);
      end if;

      if not Close_Outstream(This) then
         raise Streams.Close_Error;
      end if;
   end Close;

   -- Setting Times --

   procedure Set_Times (This : in out File_Outstream; Attributes : File_System.Attributes) is
   begin
      This.Set_Times   := True;
      This.Mod_Time    := Attributes.Modification_Time;
      This.Access_Time := Attributes.Access_Time;

   end Set_Times;

   procedure Update_Times(This : in File_Outstream) is

      procedure Update is
      begin
         if File_Outstream_Set_Times
           (This.Handle,
            C_Types.To_Unix_Time(This.Mod_Time),
            C_Types.To_Unix_Time(This.Access_Time)) /= 0 then

            raise Directory_Writers.Set_Attributes_Error;

         end if;
      end Update;

   begin

      Try_Op(Update'Access);

   end Update_Times;

   -- Setting Attributes --

   procedure Set_Attributes (This : in out File_Outstream; Attributes : File_System.Attributes) is

      C_Attrs : C_Types.Attributes := C_Types.To_C(Attributes);

      procedure Set_Mode is
      begin
         if File_Outstream_Set_Mode(This.Handle, C_Attrs.Mode) /= 0 then
            raise Directory_Writers.Set_Attributes_Error;
         end if;
      end Set_Mode;

      procedure Set_Owner is
      begin
         if File_Outstream_Set_Owner(This.Handle, C_Attrs) /= 0 then
            raise Directory_Writers.Set_Attributes_Error;
         end if;
      end Set_Owner;

   begin

      Try_Op(Set_Mode'Access);
      Try_Op(Set_Owner'Access);

   end Set_Attributes;



   -- Writing Data --

   overriding procedure Write (This   : in out File_Outstream;
                               Block  : in     Block_Ptr;
                               Size   : in     Size_Type;
                               Offset : in     Size_Type) is

      N   : Size_Type := Size;
      Buf : Block_Ptr := Block;

      procedure Seek is
      begin
         if Offset > 0 and File_Outstream_Seek(This.Handle, Offset) /= 0 then
            raise Streams.Write_Error;
         end if;
      end Seek;

      procedure Write_Block is
      begin
         while N > 0 loop
            declare
               Bytes_Written : C.Int :=
                 File_Outstream_Write(This.Handle, Buf, N);

            begin
               if Bytes_Written < 0 then
                  raise Streams.Write_Error;
               end if;

               N   := N - Size_Type(Bytes_Written);
               Buf := Buf + C.Ptrdiff_T(Bytes_Written);
            end;
         end loop;
      end Write_Block;

   begin
      Try_Op(Seek'Access);
      Try_Op(Write_Block'Access);

   end Write;

end File_Streams;
