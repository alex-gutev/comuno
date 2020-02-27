--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization;

with Glib;
with Glib.G_Icon;

with Gdk.Pixbuf;

with Directory_Entries;

--
-- Purpose:
--
--  Provides functionality for loading the icon corresponding to a
--  directory entry.
--
-- Usage:
--
--  Call Load_Icon to retrieve the icon for a particular directory
--  entry.
--
package Icon_Loader is

   -- Glib Subtypes --

   subtype G_Icon is Glib.G_Icon.G_Icon;


   -- Controlled Type Wrappers --

   --
   -- G_Icon_Ref
   --
   --  Glib.G_Icon.G_Icon wrapper which automatically calls Ref and
   --  Unref when copied and deallocated.
   --
   type G_Icon_Ref is new Ada.Finalization.Controlled with private;

   --
   -- Get
   --
   --  Returns the G_Icon wrapped by the wrapper.
   --
   function Get (Ref : G_Icon_Ref) return G_Icon;

   overriding procedure Adjust (Ref : in out G_Icon_Ref);
   overriding procedure Finalize (Ref : in out G_Icon_Ref);


   -- File Content Type --

   --
   -- Content_Type_Guess
   --
   --  Determine the Mime type of a file from its filename only.
   --
   function Content_Type_Guess (Filename : String) return String;

   --
   -- Get_Icon
   --
   --  Retrieve information about the icon corresponding to a file
   --  type.
   --
   function Get_Icon (File_Type : String) return G_Icon_Ref;


   -- Loading Icons --

   --
   -- Load_Icon
   --
   --  Load the icon corresponding to an entry.
   --
   function Load_Icon (Dir_Entry : Directory_Entries.Directory_Entry)
                      return Gdk.Pixbuf.Gdk_Pixbuf;

private

   type G_Icon_Ref is new Ada.Finalization.Controlled with record
      Gicon : G_Icon;
   end record;

end Icon_Loader;
