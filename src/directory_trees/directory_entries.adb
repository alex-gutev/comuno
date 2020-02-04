--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body Directory_Entries is
   package Fs renames File_System;

   use type File_System.File_Type;


   -- Constructors --

   function Make_Entry (Ent : Listers.Dir_Entry; Attributes : File_System.Attributes)
                       return Directory_Entry is
      Ent_Path : Paths.Path := Paths.Make_Path(Ent.Name);

   begin
      return (Original_Path  => Ent_Path,
              Canonical_Path => Paths.Canonical_Paths.Canonicalize(Ent_Path),
              Attributes     => Attributes,
              Kind           => Ent.Kind);

   end Make_Entry;

   function Make_Entry (Path : Paths.Path; Kind : Fs.File_Type) return Directory_Entry is
     (Original_Path  => Path,
      Canonical_Path => Paths.Canonical_Paths.Canonicalize(Path),
      Kind           => Kind,
      Attributes     => (Kind => Kind, others => <>));


   -- Path Accessors --

   function Subpath (Ent : Directory_Entry) return Paths.Canonical_Paths.Canonical_Path is
     (Ent.Canonical_Path);

   function Original_Subpath (Ent : Directory_Entry) return Paths.Path is
     (Ent.Original_Path);


   -- Attribute Accessors --

   function Entry_Type (Ent : Directory_Entry) return Fs.File_Type is
     (Ent.Kind);

   function Kind (Ent : Directory_Entry) return Fs.File_Type is
     (if Ent.Attributes.Kind /= Fs.Unknown then
         Ent.Attributes.Kind else
         Ent.Kind);

   function Attributes (Ent : Directory_Entry) return Fs.Attributes is
      (Ent.Attributes);

end Directory_Entries;
