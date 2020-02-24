--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Gtk.Application;
with Gtk.Application_Window;

private with Gnatcoll.Refcount;
private with Gtk.Paned;

private with File_List_Views;
private with Full_File_Lists;

--
-- Purpose:
--
--  Provides subprograms for creating the main application window.
--
--  The Controller type handles the events originating from the
--  window.
--
package Comuno_Window is

   -- GTK Subtypes --

   subtype Application_Window is Gtk.Application_Window.Gtk_Application_Window;

   --
   -- Controller
   --
   --  Responsible for handling events originating from the window.
   --
   --  This type is only a reference to a shared reference counted
   --  controller object. Copying an object of this type only creates
   --  a new reference not a new controller and window.
   --
   type Controller is tagged private;

   --
   -- Create
   --
   --  Create a new window and associated Controller object.
   --
   function Create return Controller;

   --
   -- Get_Window
   --
   --  Returns the GTK Application Window associated with the
   --  controller object.
   --
   function Get_Window (This : Controller) return Application_Window;

private

   type Controller_Record is record
      Window     : Application_Window;         -- Application Window

      Left_View  : File_List_Views.Controller; -- Left file list view
      Right_View : File_List_Views.Controller; -- Right file list view

      Left_List  : Full_File_Lists.File_List;  -- File list shown in left view
      Right_List : Full_File_Lists.File_List;  -- File list showin in right view
   end record;


   package Pointers is new Gnatcoll.Refcount.Shared_Pointers(Controller_Record);

   type Controller is new Pointers.Ref with null record;

end Comuno_Window;
