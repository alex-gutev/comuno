--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization;
with Interfaces.C;

with File_System;
with Streams;
with Directory_Writers;
with C_Types;
with Paths;

--
-- Purpose:
--
--  Provides an implementation of the abstract stream interfaces for
--  regular files.
--
package File_Streams is

   subtype Handle_Ptr is C_Types.Handle_Ptr;
   subtype Cint is Interfaces.C.Int;

   subtype Size_Type is Streams.Size_Type;
   subtype Block_Ptr is Streams.Block_Ptr;

   subtype File_Create_Flags is Directory_Writers.File_Create_Flags;
   subtype File_Permissions is Directory_Writers.File_Permissions;


   -- Input Stream --

   --
   -- File_Instream
   --
   --  Regular file input stream.
   --
   --  Automatically closed when the stream object is deallocated.
   --
   type File_Instream is new Ada.Finalization.Limited_Controlled
     and Streams.Instream with private;

   --
   -- Create
   --
   --  Create an input stream for a file at a given path.
   --
   function Create (Path : Paths.Path) return File_Instream;

   --
   -- Create
   --
   --  Create an input stream for a file at a given path, relative to
   --  an open directory.
   --
   --   Fd: File descriptor of the directory
   --
   function Create (Fd : Cint; Name : Paths.Path) return File_Instream;

   overriding procedure Finalize (This : in out File_Instream);

   overriding function Read (This   : in out File_Instream;
                             Size   :    out Size_Type;
                             Offset :    out Size_Type)
                            return Block_Ptr;


   -- Output Stream --

   --
   -- File_Instream
   --
   --  Regular file input stream.
   --
   --  Automatically closed when the stream object is deallocated.
   --
   type File_Outstream is new Ada.Finalization.Limited_Controlled
     and Streams.Outstream with private;

   --
   -- Create
   --
   --  Create an output stream for a file at a given path.
   --
   --  If the file does not exist and the Flag_Exclusive flag is not
   --  set, in Flags, a new file is created.
   --
   function Create (Path        : Paths.Path;
                    Flags       : File_Create_Flags;
                    Permissions : File_Permissions := Directory_Writers.Permission_User_Rwx)
                   return File_Outstream;

   --
   -- Create
   --
   --  Create an output stream for a file at a given path, relative to
   --  an open directory.
   --
   --  If the file does not exist and the Flag_Exclusive flag is not
   --  set, in Flags, a new file is created.
   --
   --   Fd: File descriptor of the directory.
   --
   function Create (Fd          : Cint;
                    Path        : Paths.Path;
                    Flags       : File_Create_Flags;
                    Permissions : File_Permissions := Directory_Writers.Permission_User_Rwx)
                   return File_Outstream;

   --
   -- Set_Times
   --
   --  Set the access and modification times of the underlying file.
   --
   --  Parameters:
   --
   --   This: File output stream
   --
   --   Attributes: Attributes record containing the new access and
   --               modification times.
   --
   --  Exceptions:
   --
   --   A Directory_Writers.Set_Attributes_Error is raised when
   --   failing to set the times.
   --
   procedure Set_Times (This : in out File_Outstream; Attributes : File_System.Attributes);

   --
   -- Set_Times
   --
   --  Set the permission and owner attributes of the underlying file.
   --
   --  Parameters:
   --
   --   This: File output stream
   --
   --   Attributes: Attributes record containing the new permissions,
   --               owner and group.
   --
   --  Exceptions:
   --
   --   A Directory_Writers.Set_Attributes_Error is raised when
   --   failing to set the attributes.
   --
   procedure Set_Attributes (This : in out File_Outstream; Attributes : File_System.Attributes);


   -- Outstream Operations

   overriding procedure Finalize (This : in out File_Outstream);

   overriding procedure Close (This : in out File_Outstream);

   overriding procedure Write (This   : in out File_Outstream;
                               Block  : in     Block_Ptr;
                               Size   : in     Size_type;
                               Offset : in     Size_Type);


private

   package C renames Interfaces.C;

   type File_Instream is new Ada.Finalization.Limited_Controlled
      and Streams.Instream with record
         Handle : Handle_Ptr;
      end record;

   type File_Outstream is new Ada.Finalization.Limited_Controlled
      and Streams.Outstream with record
         Handle : Handle_Ptr;

         -- Flag for whether the access and modification times should
         -- be updated, to Access_Time and Mod_Time respectively,
         -- before closing the file.
         Set_Times : Boolean := False;

         Mod_Time    : File_System.Time;
         Access_Time : File_System.Time;
      end record;

end File_Streams;
