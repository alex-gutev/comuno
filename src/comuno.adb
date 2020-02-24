--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib;
with Gdk.Threads;
with Gtk.Main;

with Comuno_Application;
with GNAT.Exception_Traces;

procedure Comuno is
begin
   -- Initialize Thread Runtime --
   Gdk.Threads.G_Init;
   Gdk.Threads.Init;

   -- Initialize GtkAda
   Gtk.Main.Init;

   -- Run Application
   Comuno_Application.Run;
end Comuno;
