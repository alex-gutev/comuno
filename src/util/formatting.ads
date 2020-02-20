--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

--
-- Formatting
--
--  Provides functions for formatting strings.
--
package Formatting is

   --
   -- Format_Int
   --
   --  Format an integer to a string.
   --
   --  Width: Minimum integer width. If the integer has fewer digits,
   --         it is padded to the the left with '0'.
   --
   generic
      type Int is range <>;
   function Format_Int (Num : Int; Width : Natural := 0) return String;

end Formatting;
