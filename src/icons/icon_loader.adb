--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with System;

with Glib.String;
with Glib.Object;
with Glib.Types;

with Gtk.Icon_Theme;

with Gtkada.Types;
with Gtkada.C;

with File_System;

package body Icon_Loader is

   use type Glib.G_Icon.G_Icon;
   use type Gtk.Icon_Theme.Gtk_Icon_Info;

   --- C Interface Types ---

   subtype Chars_Ptr is Gtkada.Types.Chars_Ptr;
   subtype Gsize is Glib.Gsize;
   subtype Gboolean is Glib.Gboolean;


   --- Imported C Functions ---

   function G_Content_Type_Guess (Filename  :     Chars_Ptr;
                                  Data      :     Chars_Ptr;
                                  Size      :     Gsize;
                                  Uncertain : out Gboolean)
                                 return Chars_Ptr;
   pragma Import (C, G_Content_Type_Guess);

   function G_Content_Type_Get_Icon (Ctype : Chars_Ptr) return Glib.C_Proxy;
   pragma Import (C, G_Content_Type_Get_Icon);


   --- Controlled Type Wrappers ---

   -- G_Icon --

   function Get (Ref : G_Icon_Ref) return G_Icon is
     (Ref.Gicon);

   procedure Adjust (Ref : in out G_Icon_Ref) is
   begin
      if Ref.Gicon /= Glib.G_Icon.Null_G_Icon then
         Glib.Types.To_Object(Glib.Types.Gtype_Interface(Ref.Gicon)).Ref;
      end if;
   end Adjust;

   procedure Finalize (Ref : in out G_Icon_Ref) is
   begin
      if Ref.Gicon /= Glib.G_Icon.Null_G_Icon then
         Glib.Types.To_Object(Glib.Types.Gtype_Interface(Ref.Gicon)).Unref;
      end if;
   end Finalize;

   function Wrap (Gicon : G_Icon) return G_Icon_Ref is
     (Ada.Finalization.Controlled with Gicon => Gicon);


   -- Icon_Info --

   type Icon_Info_Ref is new Ada.Finalization.Controlled with record
      Info : Gtk.Icon_Theme.Gtk_Icon_Info;
   end record;

   overriding procedure Adjust (Ref : in out Icon_Info_Ref);
   overriding procedure Finalize (Ref : in out Icon_Info_Ref);

   procedure Adjust (Ref : in out Icon_Info_Ref) is
   begin
      if Ref.Info /= null then
         Ref.Info.Ref;
      end if;
   end Adjust;

   procedure Finalize (Ref : in out Icon_Info_Ref) is
   begin
      if Ref.Info /= null then
         Ref.Info.Unref;
      end if;
   end Finalize;

   function Wrap (Info : Gtk.Icon_Theme.Gtk_Icon_Info) return Icon_Info_Ref is
      (Ada.Finalization.Controlled with Info => Info);


   --- File Content Type ---

   function Content_Type_Guess (Filename : String) return String is
      Dummy     : Gboolean;
      C_Name    : Chars_Ptr := Gtkada.Types.New_String(Filename);
      C_Type    : Chars_Ptr := G_Content_Type_Guess(C_Name, Gtkada.Types.Null_Ptr, 0, Dummy);

      Mime_Type : String    := Gtkada.Types.Value(C_Type);

   begin
      Gtkada.Types.G_Free(C_Name);
      Gtkada.Types.G_Free(C_Type);

      return Mime_Type;

   end Content_Type_Guess;

   function Get_Icon (File_Type : String) return G_Icon_Ref is
      C_Icon : Glib.C_Proxy := G_Content_Type_Get_Icon
        (Gtkada.Types.New_String(File_Type));

   begin
      return (Ada.Finalization.Controlled with Gicon => G_Icon(Glib.To_Address(C_Icon)));

   end Get_Icon;

   --
   -- Icon_Name
   --
   --  Returns the icon name for a given file type.
   --
   function Icon_Name (Kind : File_System.File_Type) return String is
     (case Kind is
         when File_System.Parent           => "go-up",
         when File_System.Directory        => "folder",
         when File_System.Fifo             => "inode-fifo",
         when File_System.Block_Device     => "inode-blockdevice",
         when File_System.Character_Device => "inode-chardevice",
         when File_System.Socket           => "inode-socket",
         when others                       => "inode-x-generic");


   --- Public Interface ---

   function Load_Icon (Dir_Entry : Directory_Entries.Directory_Entry)
                      return Gdk.Pixbuf.Gdk_Pixbuf is

      use type File_System.File_Type;
      use type Gdk.Pixbuf.Gdk_Pixbuf;

      Theme : Gtk.Icon_Theme.Gtk_Icon_Theme :=
        Gtk.Icon_Theme.Get_Default;

      Kind : File_System.File_Type :=
        Directory_Entries.Kind(Dir_Entry);

      Info : Icon_Info_Ref;

      Icon : Gdk.Pixbuf.Gdk_Pixbuf;

   begin
      if Kind /= File_System.Regular then
         Info := Wrap(Theme.Lookup_Icon
                        (Icon_Name(Kind),
                         16,
                         Gtk.Icon_Theme.Icon_Lookup_Force_Size));

         if Info.Info /= null then
            Icon := Info.Info.Load_Icon;
         end if;

      else
         declare
            Ref : G_Icon_Ref := Get_Icon
              (Content_Type_Guess(Directory_Entries.Subpath(Dir_Entry).Basename));

         begin
            if Ref.Gicon /= Glib.G_Icon.Null_G_Icon then
               Info := Wrap(Theme.Lookup_By_Gicon
                              (Ref.Gicon,
                               16,
                               Gtk.Icon_Theme.Icon_Lookup_Force_Size));

               if Info.Info /= null then
                  Icon := Info.Info.Load_Icon;
               end if;
            end if;
         end;
      end if;

      if Icon = null then
         Info := Wrap(Theme.Lookup_Icon("gtk-file", 16, Gtk.Icon_Theme.Icon_Lookup_Force_Size));

         if Info.Info /= null then
            Icon := Info.Info.Load_Icon;
         end if;
      end if;

      return Icon;

   end Load_Icon;

end Icon_Loader;
