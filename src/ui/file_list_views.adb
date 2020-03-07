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
with Gtk.Adjustment;
with Gtk.Tree_View_Column;
with Gtk.Tree_Selection;
with Gtk.Accel_Group;

with Gdk.Event;
with Gdk.Types;
with Gdk.Types.Keysyms;

with Paths;
with File_Columns;

package body File_List_Views is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use type Gdk.Types.Gdk_Modifier_Type;

   -- Gtk Widget Subtypes --

   subtype Gtk_Entry is Gtk.Gentry.Gtk_Entry;
   subtype Gtk_Tree_View is Gtk.Tree_View.Gtk_Tree_View;
   subtype Gtk_Scrolled_Window is Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   subtype Gtk_Adjustment is Gtk.Adjustment.Gtk_Adjustment;


   -- Controller Shared Pointer Reference Type --

   subtype Controller_Ref is Pointers.Reference_Type;


   --- Initialization Helpers ---

   --
   -- Init_List_View
   --
   --  Initialize the GTK Tree View in which the file list is displayed.
   --
   procedure Init_List_View (This : Controller);

   --
   -- Init_Path_Entry
   --
   --  Initialize the GTK entry in which the path to the directory is
   --  entered.
   --
   procedure Init_Path_Entry (This : Controller);


   --- Signal Handlers ---

   package Selection_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Tree_Selection.Gtk_Tree_Selection_Record,
      User_Type   => Controller);

   package Keypress_Callback is new Gtk.Handlers.User_Return_Callback
     (Widget_Type => Gtk.Tree_View.Gtk_Tree_View_Record,
      Return_Type => Boolean,
      User_Type   => Controller);

   package Entry_Activate_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Gentry.Gtk_Entry_Record,
      User_Type   => Controller);

   --
   -- On_Selection_Change
   --
   --  Tree view selection changed signal handler.
   --
   procedure On_Selection_Change
     (Selection : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
      This      :        Controller);

   --
   -- On_Keypress
   --
   --  Keypress event signal handler.
   --
   function On_Keypress
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event :        Gdk.Event.Gdk_Event;
      This  :        Controller)
     return Boolean;

   --
   -- On_Path_Activate
   --
   --  Path Entry activate signal handler.
   --
   procedure On_Path_Activate
     (View : access Gtk.Gentry.Gtk_Entry_Record'Class;
      This :        Controller);


   --
   -- Mark_Current_Row
   --
   --  Toggle the marked state of the currently selected row.
   --
   procedure Mark_Current_Row (This : Controller);


   --- Constructors ---

   function Create return Controller is
      package Glist renames Gtk.Widget.Widget_List;

      Builder : Gtk.Builder.Gtk_Builder :=
        Gtk.Builder.Gtk_Builder_New_From_Resource("/org/agware/comuno/fileview.ui");

      View : File_List_View :=
        File_List_View(Builder.Get_Object("file_view"));

      Path_Entry : Gtk_Entry :=
        Gtk_Entry(Builder.Get_Object("path_entry"));

      List_View : Gtk_Tree_View :=
        Gtk_Tree_View(Builder.Get_Object("file_list"));

      Scroll_Window : Gtk_Scrolled_Window :=
        Gtk_Scrolled_Window(Builder.Get_Object("scroll_window"));

      Filter_Entry : Gtk_Entry :=
        Gtk_Entry(Builder.Get_Object("filter_entry"));


      C : Controller_Record :=
        (View          => View,
         Path_Entry    => Path_Entry,
         List_View     => List_View,
         Scroll_Window => Scroll_Window,
         Filter_Entry  => Filter_Entry,
         others        => <>);

      Ptr : Controller;

   begin
      Ptr.Set(C);

   Set_Focus_Order:
      declare
         List : Glist.GList;

      begin
         Glist.Append(List, Gtk.Widget.Gtk_Widget(List_View));
         View.Set_Focus_Chain(List);

      end Set_Focus_Order;

      Ptr.Init_List_View;
      Ptr.Init_Path_Entry;

      List_View.Add_Events(Gdk.Event.Key_Press_Mask);

      return Ptr;
   end Create;


   --- Initializing File List View ---

   --
   -- Init_Columns
   --
   --  Set the tree view's columns.
   --
   procedure Init_Columns (View : Gtk_Tree_View);

   --
   -- Init_Scroll_Adjustments
   --
   --  Configure the scroll adjustments of the scrolled window, in
   --  which the tree view is contained, to disable "smooth
   --  scrolling".
   --
   procedure Init_Scroll_Adjustments (This : Controller);

   --
   -- Init_List_View_Events
   --
   --  Setup handlers for the events originating from the tree view.
   --
   procedure Init_List_View_Events (This : Controller);


   procedure Init_List_View (This : Controller) is
      Data : Controller_Ref := This.Get;

   begin
      Init_Columns(Data.List_View);
      Init_Scroll_Adjustments(This);
      Init_List_View_Events(This);

   end Init_List_View;

   procedure Init_Columns (View : Gtk_Tree_View) is
      Icon : File_Columns.Column_Ptr := File_Columns.Get_Column("icon");
      Name : File_Columns.Column_Ptr := File_Columns.Get_Column("name");
      Size : File_Columns.Column_Ptr := File_Columns.Get_Column("size");
      Ext  : File_Columns.Column_Ptr := File_Columns.Get_Column("extension");
      Date : File_Columns.Column_Ptr := File_Columns.Get_Column("modified-date");

   begin
      File_Columns.Append_Column(View, Icon.Create);
      File_Columns.Append_Column(View, Name.Create);
      File_Columns.Append_Column(View, Ext.Create);
      File_Columns.Append_Column(View, Size.Create);
      File_Columns.Append_Column(View, Date.Create);
   end Init_Columns;


   -- Initializing Scroll Adjustments --

   --
   -- Adjustment_Data
   --
   --  User data record passed to the adjustment signal handlers.
   --
   type Adjustment_Data is record
      Source        : Gtk_Adjustment;      -- Source adjustment to read from
      Target        : Gtk_Adjustment;      -- Target adjustment to set
      List_View     : Gtk_Tree_View;       -- File list tree view
      Scroll_Window : Gtk_Scrolled_Window; -- Container scrolled window
   end record;

   package Adjustment_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Adjustment.Gtk_Adjustment_Record,
      User_Type   => Adjustment_Data);

   --
   -- Adjustment_Changed
   --
   --  Adjustment changed signal handler.
   --
   --  Updates the values of the scrolled window's vertical adjustment
   --  to match the values of the tree view's vertical adjustment.
   --
   procedure Adjustment_Changed
     (Adj  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Data :        Adjustment_Data) is

      Adjustment : Gtk_Adjustment := Data.List_View.Get_Vadjustment;

   begin
      Data.Scroll_Window.Get_Vadjustment.Configure
        (Adjustment.Get_Value,
         Adjustment.Get_Lower,
         Adjustment.Get_Upper,
         Adjustment.Get_Step_Increment,
         Adjustment.Get_Page_Increment,
         Adjustment.Get_Page_Size);

   end Adjustment_Changed;

   --
   -- Adjustment_Value_Changed
   --
   --  Adjustment value changed signal handler.
   --
   --  Updates the value of the target adjustment (Data.Target) to the
   --  value of the source adjustment (Data.Source).
   --
   procedure Adjustment_Value_Changed
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'class;
      Data : Adjustment_Data) is

   begin
      Data.Target.Set_Value(Data.Source.Get_Value);

   end Adjustment_Value_Changed;


   procedure Init_Scroll_Adjustments (This : Controller) is
      Data : Controller_Ref := This.Get;

      Adjustment : Gtk_Adjustment;

   begin
      Gtk.Adjustment.Gtk_New(Adjustment, 0.0, 0.0, 0.0, 1.0, 10.0);

      Adjustment_Callback.Connect
        (Adjustment,
         Gtk.Adjustment.Signal_Changed,
         Adjustment_Callback.To_Marshaller(Adjustment_Changed'Access),

         (List_View     => Data.List_View,
          Scroll_Window => Data.Scroll_Window,
          others        => <>));

      Data.List_View.Set_Vadjustment(Adjustment);

      Adjustment_Callback.Connect
        (Adjustment,
         Gtk.Adjustment.Signal_Value_Changed,
         Adjustment_Callback.To_Marshaller(Adjustment_Value_Changed'Access),

         (Source => Data.List_View.Get_Vadjustment,
          Target => Data.Scroll_Window.Get_Vadjustment,
          others => <>));

      Adjustment_Callback.Connect
        (Data.Scroll_Window.Get_Vadjustment,
         Gtk.Adjustment.Signal_Value_Changed,
         Adjustment_Callback.To_Marshaller(Adjustment_Value_Changed'Access),

         (Source => Data.Scroll_Window.Get_Vadjustment,
          Target => Data.List_View.Get_Vadjustment,
          others => <>));

   end Init_Scroll_Adjustments;


   -- File List View Events --

   package Activate_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Tree_View.Gtk_Tree_View_Record,
      User_Type   => Controller);

   -- Used to emit row activate signals --
   procedure Emit_By_Name is
     new Activate_Callback.Tree_Path_Tree_Column_Marshaller.Emit_By_Name_Generic
       (Gtk.Handlers.To_Address, Gtk.Tree_View_Column.Convert);


   procedure Init_List_View_Events (This : Controller) is
      Data : Controller_Ref := This.Get;

      use type Gdk.Event.Gdk_Event_Mask;

   begin
      -- Set Event Mask --

      Data.List_View.Add_Events(Gdk.Event.Key_Press_Mask or Gdk.Event.Focus_Change_Mask);

      -- Connect Selection Event Handler --

      Selection_Callback.Connect
        (Data.List_View.Get_Selection,
         Gtk.Tree_Selection.Signal_Changed,
         Selection_Callback.To_Marshaller(On_Selection_Change'Access),
         This);

      -- Connect Keypress Event Handler --

      Keypress_Callback.Connect
        (Data.List_View,
         Gtk.Widget.Signal_Key_Press_Event,
         Keypress_Callback.To_Marshaller(On_Keypress'Access),
         This,
         False);

   end Init_List_View_Events;


   --- Initialize Path Entry ---

   procedure Init_Path_Entry (This : Controller) is
      Data : Controller_Ref := This.Get;

   begin
      Entry_Activate_Callback.Connect
        (Data.Path_Entry,
         Gtk.Gentry.Signal_Activate,
         Entry_Activate_Callback.To_Marshaller(On_Path_Activate'Access),
         This);
   end Init_Path_Entry;


   --- Accessors ---

   function Get_View (This : Controller) return File_List_View is
      Data : Controller_Ref := This.Get;

   begin
      return Data.View;
   end Get_View;

   function Get_Path (This : Controller) return Glib.Utf8_String is
      Data : Controller_Ref := This.Get;

   begin
      return Data.Path_Entry.Get_Text;
   end Get_Path;


   --- Changing File List ---

   function Get_File_List (This : Controller) return File_Lists.File_List'Class is
      Data : Controller_Ref := This.Get;

   begin
      return Data.File_List.Element;
   end Get_File_List;

   procedure Set_File_List (This : in out Controller; List : File_Lists.File_List'Class) is
      Data  : Controller_Ref := This.Get;
      Empty : Controller;

   begin
      if not Data.File_List.Is_Empty then
         Data.File_List.Reference.Set_Listener(Empty);
      end if;

      Data.File_List.Clear;

      This.Change_Model(List.Get_List);
      This.Select_Row(List.Selected_Row);

      Data.File_List.Replace_Element(List);
      Data.File_List.Reference.Set_Listener(This);

   end Set_File_List;


   --- File List Listener Callbacks ---

   procedure Change_Model (This  : in out Controller;
                           Model : in Gtk.List_Store.Gtk_List_Store) is

      Data : Controller_Ref := This.Get;

   begin
      -- Increment the reference count as it appears the GTK Tree View
      -- does not increment it, when setting the model, but does
      -- decrement it when the model is changed or the Tree View is
      -- destroyed.

      Model.Ref;

      Data.List_View.Set_Model(Gtk.List_Store."+"(Model));
   end Change_Model;

   procedure Select_Row (This : in out Controller;
                         Row  : in Gtk.Tree_Model.Gtk_Tree_Iter) is

      Data : Controller_Ref := This.Get;

   begin
      if Row /= Gtk.Tree_Model.Null_Iter then
         Data.List_View.Get_Selection.Select_Iter(Row);

      end if;
   end Select_Row;


   --- Signals ---

   package body Callbacks is
      procedure Connect_Keypress
        (This      : Controller;
         Cb        : Return_Handlers.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) is

         Data : Controller_Ref := This.Get;

      begin
         Return_Handlers.Connect
           (Gtk.Widget.Gtk_Widget(Data.View),
            Gtk.Widget.Signal_Key_Press_Event,
            Cb,
            User_Data,
            After);
      end Connect_Keypress;

      procedure Connect_Row_Activate
        (This      : Controller;
         Cb        : Void_Handlers.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) is

         Data : Controller_Ref := This.Get;

      begin
         Void_Handlers.Connect
           (Gtk.Widget.Gtk_Widget(Data.List_View),
            Gtk.Tree_View.Signal_Row_Activated,
            Cb,
            User_Data,
            After);
      end Connect_Row_Activate;

      procedure Connect_Path_Activate
        (This      : Controller;
         Cb        : Void_Handlers.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) is

         Data : Controller_Ref := This.Get;

      begin
         Void_Handlers.Connect
           (Gtk.Widget.Gtk_Widget(Data.Path_Entry),
            Gtk.Gentry.Signal_Activate,
            Cb,
            User_Data,
            After);
      end Connect_Path_Activate;

   end Callbacks;


   -- Signal Handlers --

   procedure On_Selection_Change
     (Selection : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
      This : Controller) is

      Data  : Controller_Ref := This.Get;

      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Selection.Get_Selected(Model, Row);

      if Row /= Gtk.Tree_Model.Null_Iter then
         Data.File_List.Reference.Selection_Changed(Row);
      end if;

   end On_Selection_Change;

   function On_Keypress
     (View : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      This : Controller)
     return Boolean is

      package Keys renames Gdk.Types.Keysyms;

      Data : Controller_Ref := This.Get;

   begin
      case Event.Key.Keyval  is
         when Keys.Gdk_Return =>
            declare
               Model : Gtk.Tree_Model.Gtk_Tree_Model;
               Row : Gtk.Tree_Model.Gtk_Tree_Iter;
               Path : Gtk.Tree_Model.Gtk_Tree_Path;

               Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column :=
                 Data.List_View.Get_Column(0);

            begin
               Data.List_View.Get_Selection.Get_Selected(Model, Row);

               if Row /= Gtk.Tree_Model.Null_Iter then
                  Path := Gtk.Tree_Model.Get_Path(Model, Row);

                  -- Emit row activate signal. This should be emitted
                  -- automatically by the widget however the signal is
                  -- not emitted if the selection was changed
                  -- programmatically.

                  Emit_By_Name
                    (Data.List_View,
                     Gtk.Tree_View.Signal_Row_Activated,
                     Path,
                     Column);

                  return True;

               end if;
            end;

         when Keys.Gdk_Up | Keys.Gdk_Down =>
            if (Event.Key.State and Gtk.Accel_Group.Get_Default_Mod_Mask) = Gdk.Types.Shift_Mask then
               Mark_Current_Row(This);
            end if;

         when Keys.Gdk_Home | Keys.Gdk_End =>
            null;

         when Keys.Gdk_Page_Up | Keys.Gdk_Page_Down =>
            null;

         when others =>
            null;

      end case;

      return False;
   end On_Keypress;

   procedure Mark_Current_Row (This : Controller) is
      Data  : Controller_Ref := This.Get;
      Dummy : Gtk.Tree_Model.Gtk_Tree_Model;
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Data.List_View.Get_Selection.Get_Selected(Dummy, Row);

      if Row /= Gtk.Tree_Model.Null_Iter then
         Data.File_List.Reference.Mark_Row(Row);
      end if;

   end Mark_Current_Row;


   procedure On_Path_Activate
     (View : access Gtk.Gentry.Gtk_Entry_Record'Class;
      This : Controller) is

      Data : Controller_Ref := This.Get;

   begin
      Data.List_View.Grab_Focus;
   end On_Path_Activate;

end File_List_Views;
