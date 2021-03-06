--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Calendar.Formatting;

package body C_Types is
   use type Ada.Calendar.Time;
   function To_Ada_Time (Seconds : Timestamp) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Time_Of
        (Year => 1970, Month => 1, Day => 1, Time_Zone => 0) +
        Duration(Seconds);
   end To_Ada_Time;

   function To_Unix_Time (Time : Ada.Calendar.Time) return Timestamp is
     (Timestamp
        (Time -
           Ada.Calendar.Formatting.Time_Of
             (Year => 1970, Month => 1, Day => 1, Time_Zone => 0)));


   function Convert (Attrs : Attributes) return File_System.Attributes is
      package Fs renames File_System;

   begin
      return (Device            => Fs.Device_Id(Attrs.Device),
              Inode             => Fs.Inode_Id(Attrs.Inode),
              Mode              => Fs.File_Mode(Attrs.Mode),
              Kind              => Fs.File_Type'Val(Attrs.Kind),
              Num_Links         => Fs.Size_Type(Attrs.Num_Links),
              User              => Fs.User_Id(Attrs.User),
              Group             => Fs.Group_Id(Attrs.Group),
              Size              => Fs.Size_Type(Attrs.Size),
              Block_Size        => Fs.Size_Type(Attrs.Block_Size),
              Modification_Time => To_Ada_Time(Attrs.Modification_Time),
              Access_Time       => To_Ada_Time(Attrs.Access_Time));
   end Convert;

   function To_C (Attrs : File_System.Attributes) return Attributes is
     (Device            => Attribute(Attrs.Device),
      Inode             => Attribute(Attrs.Inode),
      Mode              => Attribute(Attrs.Mode),
      Kind              => Attribute(File_System.File_Type'Pos(Attrs.Kind)),
      Num_Links         => Attribute(Attrs.Num_Links),
      User              => Attribute(Attrs.User),
      Group             => Attribute(Attrs.Group),
      Size              => Attribute(Attrs.Size),
      Block_Size        => Attribute(Attrs.Block_Size),
      Modification_Time => To_Unix_Time(Attrs.Modification_Time),
      Access_Time       => To_Unix_Time(Attrs.Modification_Time));

end C_Types;
