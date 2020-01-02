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
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

with File_System;

--
-- Purpose:
--
--  This package provides a set of types for interfacing with C code,
--  which implements the underlying file system operations.
--
-- Usage:
--
--  A value of a type, provided by this package, can either be passed
--  to or returned from a C procedure/function. The value should
--  immediately be converted to the corresponding type provided by the
--  File_System package.
--
package C_Types is
   package C renames Interfaces.C;

   --
   -- Type representing an opaque handle to a file system resource.
   --
   subtype Handle_Ptr is C.Strings.Chars_Ptr;

   --
   -- Record storing metadata about a directory entry.
   --
   type Dir_Entry is record
      Name : C.Strings.Chars_Ptr;
      Kind : C.Int;
   end record;
   pragma Convention(C, Dir_Entry);

   --
   -- Type representing a file attribute.
   --
   subtype Attribute is Interfaces.Unsigned_64;

   --
   -- Unix Timestamp
   --
   -- Number of seconds since Epoch (1 Jan 1970).
   --
   subtype Timestamp is Interfaces.Unsigned_64;

   --
   -- Record storing file attributes.
   --
   type Attributes  is record
      Device            : Attribute;
      Inode             : Attribute;
      Mode              : Attribute;
      Kind              : Attribute;
      Num_Links         : Attribute;
      User              : Attribute;
      Group             : Attribute;
      Size              : Attribute;
      Block_Size        : Attribute;
      Modification_Time : Timestamp;
      Access_Time       : Timestamp;
   end record;
   pragma Convention(C, Attributes);


   -- Converting C Types to Ada Types --

   --
   -- Converts a Unix timestamp to Ada Time type.
   --
   function To_Ada_Time (Seconds : Timestamp) return Ada.Calendar.Time;

   --
   -- Converts an Attributes record (obtained from C) to a
   -- File_System.Attributes record
   --
   function Convert (Attrs : Attributes) return File_System.Attributes;
end C_Types;
