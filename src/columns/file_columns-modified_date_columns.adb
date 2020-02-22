--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Calendar;

with Glib;
with Glib.Properties;
with Gtk.Cell_Renderer_Text;

with Formatting;
with File_System;
with Sort_Functions;
with File_Model_Columns;

with File_Columns.Data_Functions;

package body File_Columns.Modified_Date_Columns is
   use type Gtk.Enums.Gtk_Sort_Type;

   subtype Cell_Renderer_Text is Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;


   -- Data Function --

   package Set_Data_Func is new Gtk.Tree_View_Column.Set_Cell_Data_Func_User_Data
     (Glib.Gint);

   function Format_Date (Ent : Directory_Entries.Directory_Entry) return String;

   procedure Data_Function is new Data_Functions.Memoized_Data_Function (Format_Date);


   -- Column Creation --

   function Create (This : Modified_Date_Column) return Tree_View_Column is
      Col  : Tree_View_Column;
      Cell : Cell_Renderer_Text;

   begin
      -- Make Column --
      Gtk.Tree_View_Column.Gtk_New(Col);
      Col.Set_Title("Date Modified");

      -- Make Cell Renderer --
      Gtk.Cell_Renderer_Text.Gtk_New(Cell);

      -- Add cell to column --
      Col.Pack_Start(Cell, True);

      -- Set Cell Data Function --
      Set_Data_Func.Set_Cell_Data_Func(Col, Cell, Data_Function'Access, This.Get_Index);

      Col.Set_Expand(False);
      Col.Set_Sort_Column_Id(This.Index);

      return Col;

   end Create;


   -- Column Type --

   function Column_Type (Col : Modified_Date_Column) return Glib.Gtype is
     (Glib.Gtype_String);


   -- Sorting --

   function Sort_Type_Inverted is new
     Sort_Functions.Sort_Inverted (Sort_Functions.Sort_Entry_Type);

   function Sort_Name_Inverted is new
     Sort_Functions.Sort_Inverted (Sort_Functions.Sort_Name);


   function Sort_Date_Name is new Sort_Functions.Sort_Combined
     (Sort_Functions.Sort_Modified_Time,
      Sort_Functions.Sort_Name);

   function Sort_Date_Name_Inverted is new Sort_Functions.Sort_Combined
     (Sort_Functions.Sort_Modified_Time,
      Sort_Name_Inverted);

   function Sort is new Sort_Functions.Sort_Combined
     (Sort_Functions.Sort_Entry_Type, Sort_Date_Name);

   function Sort_Inverted is new Sort_Functions.Sort_Combined
     (Sort_Type_Inverted, Sort_Date_Name_Inverted);

   function Get_Sort_Function (This : Modified_Date_Column; Order : Gtk.Enums.Gtk_Sort_Type)
                              return Sort_Function is
   begin
      if Order = Gtk.Enums.Sort_Ascending then
         return Sort'Access;
      else
         return Sort_Inverted'Access;
      end if;
   end Get_Sort_Function;


   -- Formatting Size --

   function Format_Year is new Formatting.Format_Int (Ada.Calendar.Year_Number);
   function Format_Month is new Formatting.Format_Int (Ada.Calendar.Month_Number);
   function Format_Day is new Formatting.Format_Int (Ada.Calendar.Day_Number);

   function Format_Date (Ent : Directory_Entries.Directory_Entry) return String is
      use Directory_Entries;
      use type File_System.Size_Type;

      package Calendar renames Ada.Calendar;

      Time  : Calendar.Time         := Attributes(Ent).Modification_Time;

      Year  : Calendar.Year_Number  := Calendar.Year(Time);
      Month : Calendar.Month_Number := Calendar.Month(Time);
      Day   : Calendar.Day_Number   := Calendar.Day(Time);

   begin
      return  Format_Day(Day, 2) & "/" & Format_Month(Month, 2) & "/" & Format_Year(Year, 4);

   end Format_Date;

end File_Columns.Modified_Date_Columns;
