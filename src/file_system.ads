--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Calendar;
with Ada.Strings.Unbounded;

--
--  This package contains types describing various file system
--  entities such as file types and attributes.
--
package File_System is
   type File_Type is
     (Unknown,
      Regular,
      Directory,
      Link,
      FIFO,
      Character_Device,
      Block_Device,
      Socket,
      Whiteout,
      Parent -- Parent Directory Pseudo Entry Type
     );

   --
   -- Base type representing file attribute values.
   --
   type Attribute is mod 2**64;

   --
   -- File attribute value types
   --
   type Device_ID is new Attribute;
   type Inode_ID is new Attribute;
   type File_Mode is new Attribute;
   type User_ID is new Attribute;
   type Group_ID is new Attribute;
   type Size_Type is new Attribute;

   subtype Time is Ada.Calendar.Time;

   --
   -- Record storing file attributes
   --
   type Attributes is record
      Device            : Device_ID; -- ID of the device on which the file is located
      Inode             : Inode_ID;  -- File Inode Number
      Mode              : File_Mode; -- File Mode storing file permissions
      Kind              : File_Type; -- Type of file
      Num_Links         : Size_Type; -- Number of links to the file
      User              : User_ID;   -- File User ID
      Group             : Group_ID;  -- File Group ID
      Size              : Size_Type; -- File Size
      Block_Size        : Size_Type; -- Optimal Block Size for File IO
      Modification_Time : Time;      -- Modification Time
      Access_Time       : Time;      -- Acces Time
   end record;

   --
   -- Unique file identifier
   --
   type File_Id is record
      Device : Device_Id;
      Inode  : Inode_Id;
   end record;

end File_System;
