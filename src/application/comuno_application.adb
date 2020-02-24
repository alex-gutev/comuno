--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;

with Gnat.Strings;

with Glib.Application;
with Glib.Object;
with Glib.Menu_Model;

with Gtk.Handlers;
with Gtk.Builder;
with Gtk.Window;
with Gtk.Application_Window;

with Comuno_Window;

package body Comuno_Application is

   procedure Run is
      Status : Glib.Gint := Application.Run;

   begin
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Exit_Status(Status));

   end Run;


   -- Initialization Application

   procedure Init_Menu is
      Builder : Gtk.Builder.Gtk_Builder :=
        Gtk.Builder.Gtk_Builder_New_From_Resource("/org/agware/comuno/main_menu.ui");

      Menu_Object : Glib.Object.Gobject :=
        Builder.Get_Object("appmenu");

   begin
      Application.Set_App_Menu(Glib.Menu_Model.Gmenu_Model(Menu_Object));

   end Init_Menu;


   -- Adding Windows --

   function New_Window return Gtk.Window.Gtk_Window is
      Window : Gtk.Application_Window.Gtk_Application_Window :=
        Comuno_Window.Create.Get_Window;

   begin
      Application.Add_Window(Window);

      return Gtk.Window.Gtk_Window(Window);

   end New_Window;


   -- Signal Handlers --

   procedure On_Startup (Application : access Glib.Application.Gapplication_Record'Class) is
   begin
      Init_Menu;
   end On_Startup;

   procedure On_Activate (Application : access Glib.Application.Gapplication_Record'Class) is
      Window : Gtk.Window.Gtk_Window := New_Window;

   begin
      Window.Present;
      Window.Show_All;
   end On_Activate;


begin
   Gtk.Application.Gtk_New
     (Application,
      Application_Id,
      Glib.Application.G_Application_Flags_None);

   Application.On_Startup(On_Startup'Access);
   Application.On_Activate(On_Activate'Access);

end Comuno_Application;
