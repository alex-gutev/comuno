--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package Paths.Canonical_Paths is
   --
   -- Canonical_Path
   --
   --  Represents a path in a canonical representation.
   --
   type Canonical_Path is new Path with private;


   -- Constructors --

   --
   -- Canonicalize
   --
   --  Change a path to a canonical representation.
   --
   --  Self components (.), empty components and non-leading parent
   --  (..)  components are removed.
   --
   --  If Directory is True the resulting path represents a directory,
   --  otherwise if Directory is False, the resulting path represents
   --  a file.
   --
   function Canonicalize (P : Path'Class; Directory : Boolean := False) return Canonical_Path;


   -- Operations on Paths --

   function Append (P1, P2 : Canonical_Path) return Canonical_Path;
   procedure Append (P1 : in out Canonical_Path; P2 : in Canonical_Path);

   function Merge (P1, P2 : Canonical_Path) return Canonical_Path;
   procedure Merge (P1 : in out Canonical_Path; P2 : in Canonical_Path);

private

   type Canonical_Path is new Path with null record;

end Paths.Canonical_Paths;
