--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Interfaces.C;

with Paths;
with File_System;

with Streams;

--
-- Purpose:
--
--  Provides an abstract interface for modifying the contents of
--  directories, which includes the following operations:
--
--  - Creating Files
--  - Deleting Files
--  - Renaming Files
--
package Directory_Writers is

   subtype Access_Mask is Interfaces.Unsigned_16;


   -- File Creation Flags --

   type File_Create_Flags is mod 2**8;

   Flag_Exclusive : constant File_Create_Flags := 1;


   -- File Permissions --

   type Permissions is record
      Read    : Boolean;
      Write   : Boolean;
      Execute : Boolean;
   end record;

   type File_Permissions is record
      User    : Permissions;
      Group   : Permissions;
      Other   : Permissions;

      Set_Uid : Boolean;
      Set_Gid : Boolean;
      Sticky  : Boolean;
   end record;


   Permission_User_Rwx : constant File_Permissions :=
     (User   => (others => True),
      Group  => (others => False),
      Other  => (others => False),
      others => False);

   function Ada_To_Unix (Perms : File_Permissions) return Access_Mask;


   -- Directory Writer Interface --

   --
   -- Directory_Writer
   --
   --  Directory writer interface type.
   --
   type Directory_Writer is limited interface;

   --
   -- Close
   --
   --  Release all file system resources held by the Directory_Writer
   --  object.
   --
   --  Should raise a Close_Error exception if there was an error
   --  committing the changes to the directory, to the underlying
   --  storage medium.
   --
   procedure Close (This : in out Directory_Writer) is abstract;


   --
   -- Create_File
   --
   --  Create a new file within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, at which to create
   --          the file.
   --
   --    Attributes: File attributes
   --
   --    Flags: File creation flags
   --
   function Create_File (This       : in out Directory_Writer;
                         Path       : in     Paths.Path;
                         Attributes : in     File_System.Attributes;
                         Flags      : in     File_Create_Flags)
                        return Streams.Outstream
     is abstract;

   --
   -- Make_Directory
   --
   --  Create a directory within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, at which to create
   --          the directory.
   --
   --    Defer: If true, the creation of the directory may be deferred
   --           until a file is created in it or its attributes are
   --           set.
   --
   procedure Make_Directory (This  : in out Directory_Writer;
                             Path  : in Paths.Path;
                             Defer : in Boolean := True)
     is abstract;

   --
   -- Make_Symlink
   --
   --  Create a symbolic link within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, at which to create
   --          the symbolic link.
   --
   --    Target: Path to the target of the link
   --
   --    Attributes: Link file attributes
   --
   procedure Make_Symlink (This       : in out Directory_Writer;
                           Path       : in Paths.Path;
                           Target     : in Paths.Path;
                           Attributes : in File_System.Attributes)
     is abstract;


   --
   -- Set_Attributes
   --
   --  Set the attributes of a file within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, to the file.
   --
   --    Attributes: New file attributes
   --
   --
   procedure Set_Attributes (This : in out Directory_Writer;
                             Path : in Paths.Path;
                             Attributes : in File_System.Attributes)
     is abstract;

   --
   -- Rename
   --
   --  Rename a file within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, to the file to
   --          rename.
   --
   --    Destination: Destination path, relative to the directory, to
   --                 which, the file is renamed.
   --
   procedure Rename (This        : in out Directory_Writer;
                     Source      : in     Paths.Path;
                     Destination : in     Paths.Path)
     is abstract;

   --
   -- Remove
   --
   --  Delete a file within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, to the file to
   --          delete.
   --
   --    Relative: If true, the path is interpreted relative to the
   --              current subpath of the write object, otherwise it
   --              is interpreted relative to the base path of the
   --              writer.
   --
   procedure Remove (This     : in out Directory_Writer;
                     Path     : in     Paths.Path;
                     Relative :        Boolean := True)
     is abstract;

   --
   -- Get_File_Id
   --
   --  Retrieve a File_Id object, which uniquely identifies the file,
   --  for a file within the directory.
   --
   --    This: Directory_Writer object
   --
   --    Path: Subpath, relative to the directory, to the file.
   --
   function Get_File_Id (This : in Directory_Writer;
                         Path : in Paths.Path)
                        return File_System.File_Id
     is abstract;


   -- Exceptions --

   --
   -- Exception representing an error when opening a directory
   --
   Open_Error  : exception;

   --
   -- Exception representing an error in committing the changes to a
   -- directory, while closing.
   --
   Close_Error : exception;

   --
   -- Exception representing an error when creating a new directory.
   --
   Make_Directory_Error : exception;

   --
   -- Exception representing an error when creating a symbolic link
   --
   Symlink_Error        : exception;

   --
   -- Exception representing an error in setting the attributes of a
   -- file.
   --
   Set_Attributes_Error : exception;

   --
   -- Exception representing an error when deleting a file.
   --
   Remove_Error : exception;

   --
   -- Exception representing an error when renaming a file.
   --
   Rename_Error : exception;

end Directory_Writers;
