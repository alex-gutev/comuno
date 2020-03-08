--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib;

with Gtk.Frame;
with Gtk.Gentry;
with Gtk.Tree_View;
with Gtk.Scrolled_Window;
with Gtk.Widget;
with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.List_Store;

with File_Lists;

private with Ada.Containers.Indefinite_Holders;
private with Gnatcoll.Refcount;

--
-- Purpose:
--
--  Provides subprograms for creating a view, which displays a list of
--  entries, stored in a File_List object, in a GTK Tree View.
--
--  The provided Controller type handles the events originating from
--  the views, and synchronizes the state of the view with the state
--  of a File_List object.
--
package File_List_Views is

   -- GTK View Subtypes --

   subtype File_List_View_Record is Gtk.Frame.Gtk_Frame_Record;
   subtype File_List_View is Gtk.Frame.Gtk_Frame;

   --
   -- Controller
   --
   --  Responsible for handling events originating from the view and
   --  synchronizing the state of the view with the state of a
   --  File_List object.
   --
   type Controller is new File_Lists.Listener with private;

   --
   -- Create
   --
   --  Creates a new view and associated Controller object.
   --
   function Create return Controller;


   -- Accessors --

   --
   -- Get_View
   --
   --  Returns the underlying GTK view, associated with the
   --  controller.
   --
   function Get_View (This : Controller) return File_List_View;

   --
   -- Get_File_List
   --
   --  Returns the File_List object, the contents of which are
   --  displayed in the view.
   --
   function Get_File_List (This : Controller) return File_Lists.File_List'Class;

   --
   -- Set_File_List
   --
   --  Set the File_List object associated with the Controller.
   --
   --  The listener of 'List' is replaced with the Controller object
   --  'This', and the Tree Model, which is displayed in the view, is
   --  replaced with the Tree Model of 'List'.
   --
   procedure Set_File_List (This : in out Controller; List : File_Lists.File_List'Class);


   --
   -- Get_Path
   --
   --  Return the path which is currently displayed in the path entry.
   --
   function Get_Path (This : Controller) return Glib.Utf8_String;


   -- File List Listener Interface Operations --

   procedure Change_Model (This  : in out Controller;
                           Model : in Gtk.List_Store.Gtk_List_Store);

   procedure Select_Row (This : in out Controller;
                         Row  : in Gtk.Tree_Model.Gtk_Tree_Iter);


   -- Event Handling --

   --
   -- Purpose:
   --
   --  Provides a number of convenience functions for connecting to
   --  various events originating from the view associated with a
   --  Controller.
   --
   generic
      --
      -- User_Type
      --
      --  Type of the user data parameter passed to each signal
      --  handler.
      --
      type User_Type (<>) is private;

   package Callbacks is

      -- GTK Handler Packages --

      package Return_Handlers is new Gtk.Handlers.User_Return_Callback
        (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
         Return_Type => Boolean,
         User_Type => User_type);

      package Void_Handlers is new Gtk.Handlers.User_Callback
        (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
         User_Type => User_Type);


      -- Connect Procedures --

      --
      -- Connect_Keypress
      --
      --  Connect a signal handler to the view's key press event
      --  signal.
      --
      procedure Connect_Keypress
        (This      : Controller;
         Cb        : Return_Handlers.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);

      --
      -- Connect_Row_Activate
      --
      --  Connect a signal handler to the event emitted when a row in
      --  the view is clicked.
      --
      procedure Connect_Row_Activate
        (This      : Controller;
         Cb        : Void_Handlers.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);

      --
      -- Connect_Path_Activate
      --
      --  Connect a signal handler to the activate event of the path
      --  entry.
      --
      procedure Connect_Path_Activate
        (This      : Controller;
         Cb        : Void_Handlers.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);

   end Callbacks;

private

   package File_List_Holders is new Ada.Containers.Indefinite_Holders
     (File_Lists.File_List'Class,
      File_Lists."=");

   type Controller_Record is record
      View          : File_List_View;

      Path_Entry    : Gtk.Gentry.Gtk_Entry;
      List_View     : Gtk.Tree_View.Gtk_Tree_View;
      Scroll_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Filter_Entry  : Gtk.Gentry.Gtk_Entry;

      File_List     : File_List_Holders.Holder;

      -- Selection Events --

      -- Flag for whether the rows, between the current and previous
      -- selection, should be marked on the next selection change
      -- event
      Mark_Rows       : Boolean := False;

      -- Offset, from the selected row, of the last row which is to be
      -- marked in the next selection change event.
      Mark_End_Offset : Glib.Gint;

   end record;

   package Pointers is new Gnatcoll.Refcount.Shared_Pointers(Controller_Record);

   type Controller is new Pointers.Ref and File_Lists.Listener with null record;

end File_List_Views;
