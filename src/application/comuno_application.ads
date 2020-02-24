--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Gtk.Application;

--
-- Purpose:
--
--  Contains the GTK Application singleton instance.
--
package Comuno_Application is

   --
   -- Application Identifier
   --
   Application_Id : String := "org.agware.comuno";

   --
   -- Application Singleton Instance
   --
   Application : Gtk.Application.Gtk_Application;

   --
   -- Run
   --
   --  Run the GTK application
   --
   procedure Run;

end Comuno_Application;
