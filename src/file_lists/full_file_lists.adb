--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Exceptions;

with Glib;
with Glib.Object;

with Gtk.Tree_Sortable;
with Gtk.Enums;

with File_System;
with File_Columns;

package body Full_File_Lists is

   -- Types --

   use type Glib.Gint;
   use type File_System.File_Type;
   use type File_Columns.Column_Ptr;

   subtype List_Store is Gtk.List_Store.Gtk_List_Store;
   subtype Model_Ref is File_Model_Columns.Model_Ref;


   -- Read Operation Callback --

   --
   -- Read_Callback
   --
   --  Callback user data for the read task callback.
   --
   type Read_Callback is new Virtual_Hierarchies.Operation_Callback with record
      Data        : Data_Weak_Ptr;
      List        : Model_Ref;
      Move_To_Old : Boolean;
   end record;

   overriding procedure Begin_Operation (This : in Read_Callback) is null;

   overriding procedure New_Entry (This      : in Read_Callback;
                                   Dir_Entry : in Directory_Entries.Directory_Entry);

   overriding procedure Finish_Operation (This : in Read_Callback);

   overriding procedure Operation_Error (This  : in Read_Callback;
                                         Error : in Ada.Exceptions.Exception_Occurrence);


   -- Initiating Read Tasks --

   --
   -- Prepare_Read
   --
   --  Perform necessary operations, prior to beginning a background
   --  read operation, such as changing the model to the emtpy list.
   --
   procedure Prepare_Read (This : in out File_List_Data);

   --
   -- Set_Empty_List
   --
   --  Call the Change_Model callback, of the listener object,
   --  changing the model to the empty list.
   --
   procedure Set_Empty_List (This : in out File_List_Data);


   -- Finishing Read Operation --

   --
   -- Finish_Read
   --
   --  Finalize a succesful read operation:
   --
   --  - Change the tree model
   --  - Update the selection
   --  - Update the marked set
   --
   procedure Finish_Read (This        : in out File_List_Data;
                          List        : in Model_Ref;
                          Move_To_Old : in Boolean);

   --
   -- Set_New_List
   --
   --  Set the tree model, of the file list, to a new tree model
   --  containing the entries read in the last background read task.
   --
   procedure Set_New_List (This         : in out File_List_Data;
                           List         : in Model_Ref;
                           Clear_Marked : in Boolean);


   -- Handling Read Errors --

   --
   -- Reset_List
   --
   --  Reset the tree model, and selection, back to the current
   --  non-empty tree model.
   --
   procedure Reset_List (This : in out File_List_Data);


   -- Changing Selection --

   --
   -- Select_Row
   --
   --  Change the selection to a given row.
   --
   --  Calls the Select_Row procedure of the listener.
   --
   procedure Select_Row (This : in out File_List_Data; Row : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure Select_Row (This : in out File_List_Data; Index : Glib.Gint);

   --
   -- Select_Named
   --
   --  Find the first row corresponding to the entry with name Name
   --  and change the selection to it. If there is no such entry
   --  change the selection to the row at Index.
   --
   --  Calls the Select_Row procedure of the listener.
   --
   procedure Select_Named (This : in out File_List_Data; Name : Paths.Path_String; Index : Glib.Gint);

   --
   -- Select_Old
   --
   --  Change the selection to the entry with the same name as the
   --  Basename of the current directory.
   --
   procedure Select_Old (This : in out File_List_Data);

   --
   -- Update_Selection
   --
   --  Update the selection after a successful read operation.
   --
   --  Move_To_Old: If true select the row corresponding to the entry
   --               with the same name as the Basename of the current
   --               directory.
   --
   procedure Update_Selection (This : in out File_List_Data; Move_To_Old : in Boolean);


   -- Sorting --

   --
   -- Set_Sort_Column
   --
   --  Set the sort column of New_List to the sort column of Old_List.
   --
   procedure Set_Sort_Column (Old_List : Gtk.List_Store.Gtk_List_Store; New_List : Gtk.List_Store.Gtk_List_Store);

   --
   -- On_Sort_Column_Changed
   --
   --  Tree Model sort column changed signal handler.
   --
   procedure On_Sort_Column_Changed (Model : Gtk.Tree_Sortable.Gtk_Tree_Sortable);


   --- Implementation ---


   -- Creating List Store Model --

   --
   -- Make_List_Store
   --
   --  Create a new Gtk List Store tree model.
   --
   function Make_List_Store return Model_Ref is
      Model : Gtk.List_Store.Gtk_List_Store;
      Sortable : Gtk.Tree_Sortable.Gtk_Tree_Sortable;

   begin
      Gtk.List_Store.Gtk_New(Model, File_Model_Columns.Column_Types);
      Sortable := Gtk.List_Store."+"(Model);

      Gtk.Tree_Sortable.On_Sort_Column_Changed(Sortable, On_Sort_Column_Changed'Access);

      return Ref : Model_Ref do
           Ref.Set(Model);
      end return;
   end Make_List_Store;

   --
   -- Init_List_Store
   --
   --  Initialize a list store model.
   --
   --  Sets the sort function of each column.
   --
   procedure Init_List_Store (Model : Gtk.List_Store.Gtk_List_Store) is
      use type File_Columns.Sort_Function;

   begin
      for Column of File_Columns.All_Columns loop
         declare
            Func : File_Columns.Sort_Function :=
              Column.Get_Sort_Function(Gtk.Enums.Sort_Ascending);

         begin
            if Func /= null then
               Model.Set_Sort_Func
                 (Column.Get_Index + File_Model_Columns.Column_Start,
                  Gtk.List_Store.Gtk_Tree_Iter_Compare_Func(Func));
            end if;
         end;
      end loop;
   end Init_List_Store;


   --
   -- Make_List_Store
   --
   --  Create and initialize a new Gtk List Store tree model.
   --
   function Make_Tree_Model return Model_Ref is
   begin
      return Model : Model_Ref := Make_List_Store do
         Init_List_Store(Model.Get);
      end return;
   end Make_Tree_Model;


   -- Initialization and Finalization --

   procedure Initialize (This : in out File_List) is
      Data : File_List_Data :=
        (List       => Make_Tree_Model,
         Empty_List => Make_Tree_Model,
         others     => <>);

   begin
      This.Data.Set(Data);
   end Initialize;


   -- Listener Object Accessors --

   procedure Set_Listener (This : in out File_List; Object : in File_Lists.Listener'Class) is
      Data : Data_Ref := This.Data.Get;

   begin
      Data.Listener.Replace_Element(Object);

   end Set_Listener;

   function Get_Listener (This : in out File_List) return Listener_Holders.Reference_Type is
      Data : Data_Ref := This.Data.Get;

   begin
      return Data.Listener.Reference;
   end Get_Listener;


   -- Accessors --

   function Get_List (This : in File_List) return Gtk.List_Store.Gtk_List_Store is
      Data : Data_Ref := This.Data.Get;

   begin
      return Data.List.Get;
   end Get_List;

   function Path (This : File_List) return Paths.Path is
      Data : Data_Ref := This.Data.Get;

   begin
      return Data.Hierarchy.Path;
   end Path;


   -- Reading Directory --

   procedure Change_Path (This        : File_List;
                          Path        : Paths.Path;
                          Move_To_Old : Boolean := False) is

      Data : Data_Ref := This.Data.Get;

      Callback : Read_Callback :=
        (Data        => This.Data.Weak,
         List        => Make_List_Store,
         Move_To_Old => Move_To_old);

   begin
      Prepare_Read(Data);
      Data.Hierarchy.Read(Path, Callback);

   end Change_Path;

   procedure Descend (This : in out File_List;
                      Ent : in Directory_Entries.Directory_Entry)
   is begin
      null;
   end Descend;


   procedure Prepare_Read (This : in out File_List_Data) is
   begin
      Set_Empty_List(This);
   end Prepare_Read;

   procedure Set_Empty_List (This : in out File_List_Data) is
   begin
      This.Listener.Reference.Change_Model(This.Empty_List.Get);
   end Set_Empty_List;


   -- Read Operation Callback Implementation --

   procedure New_Entry (This : in Read_Callback;
                        Dir_Entry : in Directory_Entries.Directory_entry) is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      This.List.Get.Append(Row);
      File_Model_Columns.Set_Values(This.List.Get, Row, Dir_Entry);
   end New_Entry;

   procedure Finish_Operation (This : in Read_Callback) is
      Ptr : Data_Ptr;

   begin
      Ptr.Set(This.Data);

      if not Ptr.Is_Null then
         Finish_Read(Ptr.Get, This.List, This.Move_To_Old);
      end if;

   end Finish_Operation;


   -- Finishing Read Operation --

   procedure Finish_Read (This : in out File_List_Data;
                          List : in Model_Ref;
                          Move_To_Old : in Boolean) is
   begin

      Set_New_List(This, List, True);
      Update_Selection(This, Move_To_Old);

      This.Path := This.Hierarchy.Path;

   end Finish_Read;


   procedure Add_Parent_Entry (List : in List_Store; Path : in Paths.Path) is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if not Path.Is_Root then
         List.Append(Row);
         File_Model_Columns.Set_Values(List, Row, Directory_Entries.Parent_Entry);
      end if;
   end Add_Parent_Entry;


   procedure Set_New_List (This : in out File_List_Data;
                           List : in Model_Ref;
                           Clear_Marked : in Boolean) is
   begin
      if Clear_Marked then
         This.Marked_Set.Clear;
      end if;

      Add_Parent_Entry (List.Get, This.Hierarchy.Path);

      -- TODO: Load Icons

      -- Set sort column of new list to sort column of previous list
      Set_Sort_Column(This.List.Get, List.Get);

      This.List := List;

      -- Inform listener that the model has changed
      This.Listener.Reference.Change_Model(This.List.Get);

   end Set_New_List;


   -- Handling Read Directory Errors --

   procedure Operation_Error (This  : in Read_Callback;
                              Error : in Ada.Exceptions.Exception_Occurrence) is
      Ptr : Data_Ptr;

   begin
      Ptr.Set(This.Data);

      if not Ptr.Is_Null then
         Reset_List(Ptr.Get);
      end if;

   end Operation_Error;

   procedure Reset_List (This : in out File_List_Data) is
   begin
      -- TODO: Reset Path Back to previous path

      This.Listener.Reference.Change_Model(This.List.Get);
      Select_Row(This, This.Selection);

   end Reset_List;


   -- Changing Selection --

   procedure Select_Row (This : in out File_List_Data; Row : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      This.Selection := Row;
      This.Listener.Reference.Select_Row(Row);
   end Select_Row;


   procedure Select_Row (This : in out File_List_Data; Index : Glib.Gint) is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter :=
        This.List.Get.Nth_Child(Gtk.Tree_Model.Null_Iter, Index);

   begin
      Select_Row(This, Row);
   end Select_Row;

   procedure Select_Named (This : in out File_List_Data; Name : Paths.Path_String; Index : Glib.Gint) is
      List : List_Store := This.List.Get;
      Row : Gtk.Tree_Model.Gtk_Tree_Iter := List.Get_Iter_First;

   begin
      while Row /= Gtk.Tree_Model.Null_Iter loop
         if Directory_Entries.Subpath
           (File_Model_Columns.Get_Entry(List, Row)).Basename =
           Name
         then
            Select_Row(This, Row);
            return;
         end if;

         List.Next(Row);
      end loop;

      Select_Row(This, Glib.Gint'Min(Index, List.N_Children - 1));

   end Select_Named;

   procedure Select_Old (This : in out File_List_Data) is
   begin
      Select_Named(This, This.Path.Basename, 0);
   end Select_Old;

   procedure Update_Selection (This : in out File_List_Data; Move_To_Old : in Boolean) is
   begin
      if Move_To_Old then
         Select_Old(This);
      else
         Select_Row(This, 0);
      end if;
   end Update_Selection;


   -- Querying Selection --

   function Selected_Row (This : in File_List) return Gtk.Tree_Model.Gtk_Tree_Iter is
      Data : Data_Ref := This.Data.Get;

   begin
      return Data.Selection;
   end Selected_Row;


   function Marked_Entries (This : in File_List) return Entry_Vectors.Vector is
      Data : Data_Ref := This.Data.Get;
      Vector : Entry_Vectors.Vector;

   begin
      if Data.Marked_Set.Is_Empty then
         if Data.Selection /= Gtk.Tree_Model.Null_Iter then
            declare
               Ent : File_Model_Columns.Entry_Ref :=
                 File_Model_Columns.Get_Entry(Data.List.Get, Data.Selection);

            begin
               Vector.Append(Ent);
            end;
         end if;

      else
         for Row of Data.Marked_Set loop
            declare
               Ent : File_Model_Columns.Entry_Ref :=
                 File_Model_Columns.Get_Entry(Data.List.Get, Row);

            begin
               Vector.Append(Ent);
            end;
         end loop;

      end if;

      return Vector;

   end Marked_Entries;


   -- Marking Rows --

   procedure Mark_Row (This : in out File_List; Row : in Gtk.Tree_Model.Gtk_Tree_Iter) is
      Data : Data_Ref := This.Data.Get;

      List : List_Store := Data.List.Get;

      Ent : File_Model_Columns.Entry_Ref :=
        File_Model_Columns.Get_Entry(List, Row);

      Name : Paths.Path_String renames
        Directory_Entries.Subpath(Ent).Basename;

      Mark : Boolean;

   begin
      if Directory_Entries.Kind(Ent) /= File_System.Parent then
         Mark := not File_Model_Columns.Is_Marked(List, Row);

         if Mark then
            Data.Marked_Set.Include(name, Row);

         else
            Data.Marked_Set.Exclude(Name);

         end if;

         File_Model_Columns.Set_Marked(List, Row, Mark);
      end if;

   end Mark_Row;


   -- Selection Change Events --

   procedure Selection_Changed (This : in out File_List; Row : in Gtk.Tree_Model.Gtk_Tree_Iter) is
      Data : Data_Ref := This.Data.Get;

   begin
      if not Data.Reading then
         Data.Selection := Row;
      end if;
   end Selection_Changed;


   -- Sorting --

   procedure Set_Sort_Column (Old_List : Gtk.List_Store.Gtk_List_Store; New_List : Gtk.List_Store.Gtk_List_Store) is
      Id    : Glib.Gint;
      Order : Gtk.Enums.Gtk_Sort_Type;

   begin
      Old_List.Get_Sort_Column_Id(Id, Order);
      New_List.Set_Sort_Column_Id(Id, Order);
   end Set_Sort_Column;


   procedure On_Sort_Column_Changed (Model : Gtk.Tree_Sortable.Gtk_Tree_Sortable) is
      Id    : Glib.Gint;
      Order : Gtk.Enums.Gtk_Sort_Type;

      Index : Glib.Gint;

   begin
      Gtk.Tree_Sortable.Get_Sort_Column_Id(Model, Id, Order);

      Index := Id - File_Model_Columns.Column_Start;

      if Index >= 0 then
         declare
            Column : File_Columns.Column_Ptr :=
              File_Columns.Get_Column(Natural(Index));

         begin
            if Column /= null then
               Gtk.Tree_Sortable.Set_Sort_Func(Model, Id, Column.Get_Sort_Function(Order));
            end if;

         end;
      end if;

   end On_Sort_Column_Changed;

end Full_File_Lists;
