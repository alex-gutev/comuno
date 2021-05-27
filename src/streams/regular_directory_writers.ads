--  Copyright (C) 2021 Alexander Gutev
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization.Limited_Controlled;
with Interfaces.C;

with Directory_Writers;
with Streams;
with File_System;
with C_Types;
with Path;

--
-- Purpose:
--
--  Implements the directory writer interface for regular directories,
--  accessible via the file system.
--
package Regular_Directory_Writers is

   subtype Limited_Controlled is Ada.Finalization.Limited_Controlled;

   subtype File_Create_Flags is Directory_Writer.File_Create_Flags;


   --
   -- Directory_Writer
   --
   --  Directory writer for regular directories
   --
   type Directory_Writer is new Ada.Finalization.Limited_Controlled and
     Directory_Writers.Directory_Writer with private;


   -- Creation --

   --
   -- Open_Dir
   --
   --  Open a directory for writing and return a directory writer
   --  object.
   --
   --  Path: Path to the directory
   --
   function Open_Dir (Path : Paths.Path) return Directory_Writer;


   -- Finalization --

   overriding procedure Close (This : in out Directory_Writer);

   overriding procedure Finalize (This : in out Directory_Writer);


   -- Directory_Writer Operations --

   overriding function Create_File (This       : in out Directory_Writer;
                                    Path       : in     Paths.Path;
                                    Attributes : in     File_System.Attributes;
                                    Flags      : in     File_Create_Flags)
                                   return Streams.Outstream;

   overriding procedure Make_Directory (This  : in out Directory_Writer;
                                        Path  : in     Paths.Path;
                                        Defer : in     Boolean := True);


   overriding procedure Make_Symlink (This       : in out Directory_Writer;
                                      Path       : in     Paths.Path;
                                      Target     : in     Paths.Path;
                                      Attributes : in     File_System.Attributes);

   overriding procedure Set_Attributes (This       : in out Directory_Writer;
                                        Path       : in     Paths.Path;
                                        Attributes : in     File_System.Attributes);

   overriding procedure Rename (This        : in out Directory_Writer;
                                Source      : in     Paths.Path;
                                Destination : in     Paths.Path);

   overriding procedure Remove (This     : in out Directory_Writer;
                                Path     : in     Paths.Path;
                                Relative :        Boolean := True);

   overriding function Get_File_Id (This : in Directory_Writer;
                                    Path : in Paths.Path)
                                   return File_System.File_Id;

private

   subtype Cint is Interfaces.C.Int;

   type Directory_Writer is new Ada.Finalization.Limited_Controlled and
      Directory_Writers.Directory_Writer with record
         Handle : Cint;
      end record;

end Regular_Directory_Writers;
