--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib.Object;

with Gtk.Builder;
with Gtk.Widget;
with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.List_Store;
with Gtk.Tree_View_Column;

with Gdk.Event;

with Paths;
with Directory_Entries;
with File_Model_Columns;

package body Comuno_Window is

   subtype Controller_Ref is Pointers.Reference_Type;


   -- Paned View Initialization --

   function Get_Paned (Builder : Gtk.Builder.Gtk_Builder) return Gtk.Paned.Gtk_Paned;

   procedure Init_Pane_View (This : Controller; Pane_View : Gtk.Paned.Gtk_Paned);


   -- File View Initialization --

   procedure Init_File_View_Events (This : Controller;
                                    View : File_List_Views.Controller;
                                    List : Full_File_Lists.File_list);


   -- Signal Handlers --

   type Event_Data is record
      Window : Controller;
      View   : File_List_Views.Controller;
      List   : Full_File_Lists.File_List;
   end record;

   package File_View_Events is new File_List_Views.Callbacks (Event_Data);

   function On_Keypress
     (View  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event :        Gdk.Event.Gdk_Event;
      Data  :        Event_Data)
     return Boolean

   is begin
      return False;
   end On_Keypress;

   procedure On_Path_Activate
     (View : access Gtk.Widget.Gtk_Widget_Record'Class;
      Data : Event_Data) is

   begin
      Data.List.Change_Path
        (Data.List.Path.Ensure_Directory.Merge(Paths.Make_Path(Data.View.Get_Path)));
   end On_Path_Activate;

   --
   -- Row Activate Signal Handler.
   --
   procedure On_Row_Activate
     (View   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path   :        Gtk.Tree_Model.Gtk_Tree_Path;
      Column :        Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Data   :        Event_data);


   --- Constructors ---

   function Create return Controller is
      Builder : Gtk.Builder.Gtk_Builder :=
        Gtk.Builder.Gtk_Builder_New_From_Resource("/org/agware/comuno/window.ui");

      Object : Glib.Object.Gobject :=
        Builder.Get_Object("commander_window");

      Window : Application_Window :=
        Application_Window(Object);

      Data : Controller_Record :=
        (Window     => Window,
         Left_View  => File_List_Views.Create,
         Right_View => File_List_Views.Create,
         others     => <>);

      Ptr : Controller;

   begin
      Ptr.Set(Data);

      Init_Pane_View(Ptr, Get_Paned(Builder));

      Init_File_View_Events(Ptr, Data.Left_View, Data.Left_List);
      Init_File_View_Events(Ptr, Data.Right_View, Data.Right_List);

      Data.Left_View.Set_File_List(Data.Left_List);
      Data.Left_List.Change_Path(Paths.Make_Path("/"));

      Data.Right_View.Set_File_List(Data.Right_List);
      Data.Right_List.Change_Path(Paths.Make_Path("/"));

      return Ptr;
   end Create;


   -- Paned View Initialization --

   function Get_Paned (Builder : Gtk.Builder.Gtk_Builder) return Gtk.Paned.Gtk_Paned is
      Object : Glib.Object.Gobject :=
        Builder.Get_Object("pane_view");

   begin
      return Gtk.Paned.Gtk_Paned(Object);
   end Get_Paned;

   procedure Init_Pane_View (This : Controller; Pane_View : Gtk.Paned.Gtk_Paned) is
      package Glist renames Gtk.Widget.Widget_List;

      Data   : Controller_Ref     := This.Get;
      Window : Application_Window := Data.Window;

   begin

   Set_Focus_Order:
      declare
         List : Glist.Glist;

      begin
         Glist.Append(List, Gtk.Widget.Gtk_Widget(Pane_View));
         Window.Set_Focus_Chain(List);
      end Set_Focus_Order;

   Add_File_Views:
      declare
         List : Glist.Glist;

         Left : File_List_Views.File_List_View :=
           Data.Left_View.Get_View;

         Right : File_List_Views.File_List_View :=
           Data.Right_View.Get_View;

      begin
         Pane_View.Pack1(Left, True, True);
         Pane_View.Pack2(Right, True, True);

         Glist.Append(List, Gtk.Widget.Gtk_Widget(Left));
         Glist.Append(List, Gtk.Widget.Gtk_Widget(Right));

         Pane_View.Set_Focus_Chain(List);

      end Add_File_Views;

   end Init_Pane_View;


   -- File View Initialization --

   --
   -- File list path changed callback type.
   --
   --  Updates the contents of a view's path entry.
   --
   type Path_Changed_Callback is new Full_File_Lists.Path_Changed_Callback with record
      View : File_List_Views.Controller;
   end record;

   overriding procedure Path_Changed (This : Path_Changed_Callback; Path : Paths.Path);

   procedure Init_File_View_Events (This : Controller;
                                    View : File_List_Views.Controller;
                                    List : Full_File_Lists.File_List) is
   begin
      File_View_Events.Connect_Keypress
        (View,
         File_View_Events.Return_Handlers.To_Marshaller (On_Keypress'Access),
         (Window => This,
          View   => View,
          List   => list));

      File_View_Events.Connect_Path_Activate
        (View,
         File_View_Events.Void_Handlers.To_Marshaller (On_Path_Activate'Access),
         (Window => This,
          View   => View,
          List   => List));

      File_View_Events.Connect_Row_Activate
        (View,
         File_View_Events.Void_Handlers.To_Marshaller (On_Row_Activate'Access),
         (Window => This,
          View   => View,
          List   => List));

      List.Set_Path_Changed_Callback(Path_Changed_Callback'(View => View));

   end Init_File_View_Events;


   -- Path Changed Callback --

   procedure Path_Changed (This : Path_Changed_Callback; Path : Paths.Path) is
   begin
      This.View.Set_Path(Path.To_String);
   end Path_Changed;


   -- Signal Handlers --

   procedure On_Row_Activate
     (View   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path   :        Gtk.Tree_Model.Gtk_Tree_Path;
      Column :        Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Data   :        Event_data) is

      Model : Gtk.List_Store.Gtk_List_Store := Data.List.Get_List;

      Row : Gtk.Tree_Model.Gtk_Tree_Iter :=
        Data.List.Get_List.Get_Iter(Path);

      Ent : Directory_Entries.Directory_Entry :=
        File_Model_Columns.Get_Entry(Model, Row);

   begin
      if not Data.List.Descend(Ent) then
         -- TODO: Open File
         null;
      end if;

   end On_Row_Activate;


   --- Accessors ---

   function Get_Window (This : Controller) return Application_Window is
      Data : Controller_Ref := This.Get;

   begin
      return Data.Window;
   end Get_Window;

end Comuno_Window;
