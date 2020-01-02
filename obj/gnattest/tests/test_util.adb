--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Assertions;

package body Test_Util is

   procedure Assert (Condition : Boolean; Message : String; Fail : Boolean := False) is
      --
      -- Dummy variable used to invoke the function version of the
      -- Assert subprogram.
      --
      Dummy : Boolean;
   begin
      if Fail then
         Aunit.Assertions.Assert(Condition, Message);
      else
         Dummy := Aunit.Assertions.Assert(Condition, Message);
      end if;
   end Assert;


   procedure Assert_Equal (Actual, Expected : Test_Type; Message : String := ""; Fail : Boolean := False) is
   begin
      Assert(Actual = Expected,
             Message & ": " & Image(Actual) & " /= " & Image(Expected),
             Fail);
   end Assert_Equal;

   procedure Assert_Equal_String (Actual, Expected : String; Message : String := ""; Fail : Boolean := False)
   is begin
      Assert(Actual = Expected,
             Message & ": " & Actual & " /= " & Expected,
             Fail);
   end Assert_Equal_String;

end Test_Util;
