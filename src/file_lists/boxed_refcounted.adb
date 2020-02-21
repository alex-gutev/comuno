--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Unchecked_Deallocation;

package body Boxed_Refcounted is

   procedure Free is new Ada.Unchecked_Deallocation
     (Pointers.Ref, Conversions.Object_Pointer);


   -- Copying --

   function Copy_Data (Src : System.Address) return System.Address is
      Dest_Ref : Conversions.Object_Pointer := new Pointers.Ref;
      Src_Ref  : Conversions.Object_Pointer := Conversions.To_Pointer(Src);

   begin
      Dest_Ref.all := Src_Ref.all;
      return Conversions.To_Address(Dest_Ref);

   end Copy_Data;


   -- Deallocation --

   procedure Free_Data (Ptr : System.Address) is
      Ref : Conversions.Object_Pointer :=
        Conversions.To_Pointer(Ptr);

   begin
      Free(Ref);
   end Free_Data;


   -- Creating Boxed Values --

   procedure Box (Data : in Data_Type; Value : in out Glib.Values.Gvalue) is
      Ref : Pointers.Ref;

   begin
      Ref.Set(Data);

      Glib.Values.Init(Value, Boxed_Type);
      Glib.Values.Set_Boxed(Value, Ref'Address);

   end Box;


   -- Retrieving Boxed Values --

   function Get_Data (Value : Glib.Values.Gvalue) return Reference is
      Ref : Pointers.Ref;

   begin
      Ref := Conversions.To_Pointer(Glib.Values.Get_Boxed(Value)).all;

      return Ref.Get;

   end Get_Data;

   function Get_Data (Model : Gtk.Tree_Model.Gtk_Tree_Model;
                      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
                      Index : Glib.Gint)
                     return Reference is
      Value : Glib.Values.Gvalue;

   begin
      Gtk.Tree_Model.Get_Value(Model, Row, Index, Value);

      return R : Reference := Get_Data(Value) do
         Glib.Values.Unset(Value);
      end return;

   end Get_Data;


begin

   Boxed_Type := Glib.Boxed_Type_Register_Static
     (Name, Copy_Data_Access, Free_Data_Access);

end Boxed_Refcounted;
