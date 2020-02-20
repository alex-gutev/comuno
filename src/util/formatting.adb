--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Fixed;

package body Formatting is

   function Format_Int (Num : Int; Width : Natural := 0) return String is
      use Ada.Strings.Fixed;

      Image : String    := Num'Image;
      Sign  : Character := Image(1);
      Value : String    := Image(2 .. Image'Last);

      Padded : String :=
        (if Value'Length < Width then
            Tail(Value, Width, '0')
         else Value);

   begin
      return (if Sign = ' ' then Padded else Sign & Padded);

   end Format_Int;


end Formatting;
