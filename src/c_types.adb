--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body C_Types is
   use type Ada.Calendar.Time;
   function To_Ada_Time (Seconds : Timestamp) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Time_Of(Year => 1970, Month => 1, Day => 1) +
	Duration(Seconds);
   end To_Ada_Time;

   function Convert (Attrs : Attributes) return File_System.Attributes is
      package Fs renames File_System;

      Fs_Attrs : Fs.Attributes;
   begin
      Fs_Attrs.Device := Fs.Device_Id(Attrs.Device);
      Fs_Attrs.Inode := Fs.Inode_Id(Attrs.Inode);
      Fs_Attrs.Mode := Fs.File_Mode(Attrs.Mode);
      Fs_Attrs.Num_Links := Fs.Size_Type(Attrs.Num_Links);
      Fs_Attrs.User := Fs.User_Id(Attrs.User);
      Fs_Attrs.Group := Fs.Group_Id(Attrs.Group);
      Fs_Attrs.Size := Fs.Size_Type(Attrs.Size);
      Fs_Attrs.Block_Size := Fs.Size_Type(Attrs.Block_Size);

      Fs_Attrs.Modification_Time := To_Ada_Time(Attrs.Modification_Time);
      Fs_Attrs.Access_Time := To_Ada_Time(Attrs.Access_Time);

      return Fs_Attrs;
   end Convert;
end C_Types;
