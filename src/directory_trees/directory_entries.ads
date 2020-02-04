--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with File_System;
with Listing;

with Paths;
with Paths.Canonical_Paths;

--
-- Purpose:
--
--  This package provides a type Directory_Entry which stores the
--  metadata of an entry read from a directory.
--
package Directory_Entries is

   type Directory_Entry is private;

   --
   -- Entry representing the parent directory.
   --
   Parent_Entry : constant Directory_Entry;

   --
   -- Make_Entry
   --
   --  Create a Directory_Entry from a Listing.Dir_Entry object,
   --  obtained from listing a directory, with given Attributes.
   --
   function Make_Entry (Ent : Listing.Dir_Entry; Attributes : File_System.Attributes)
                       return Directory_Entry;

   --
   -- Make_Entry
   --
   --  Create a Directory_Entry with a given Path and File Type.
   --
   --  The Entry_Type and Kind component of the Attributes are
   --  initialized to Kind. The remaining components of Attributes are
   --  initialized to their default values.
   --
   function Make_Entry (Path : Paths.Path; Kind : File_System.File_Type) return Directory_Entry;

   --
   -- Subpath
   --
   --  Returns the name of the entry, interpreted as a path, in a
   --  canonical representation.
   --
   function Subpath (Ent : Directory_Entry) return Paths.Canonical_Paths.Canonical_Path;

   --
   -- Original_Subpath
   --
   --  Returns the original name of the entry, with which the object
   --  was created, unchanged.
   --
   function Original_Subpath (Ent : Directory_Entry) return Paths.Path;

   --
   -- Entry_Type
   --
   --  Returns the type of the entry itself. This may differ from the
   --  type of the underlying file if the entry is a link.
   --
   function Entry_Type (Ent : Directory_Entry) return File_System.File_Type;

   --
   -- Kind
   --
   --  Returns the type of the underlying file, obtained from the Kind
   --  component of the entry's Attributes. If the Kind component is
   --  Unknown the Entry_Type is returned.
   --
   function Kind (Ent : Directory_Entry) return File_System.File_Type;

   --
   -- Attributes
   --
   --  Returns the entry's attributes.
   --
   function Attributes (Ent : Directory_Entry) return File_System.Attributes;

private

   type Directory_Entry is record
      Original_Path  : Paths.Path;
      Canonical_Path : Paths.Canonical_Paths.Canonical_Path;
      Attributes     : File_System.Attributes;
      Kind           : File_System.File_Type;
   end record;

   Parent_Entry : constant Directory_Entry :=
     (Original_Path => Paths.Make_Path(".."),
      Canonical_Path => Paths.Canonical_Paths.Canonicalize(Paths.Make_Path("..")),
      Kind => File_System.Directory,
      Attributes => (Kind => File_System.Directory, others => <>));

end Directory_Entries;
