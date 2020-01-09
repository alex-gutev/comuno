--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

--
-- Purpose:
--
--  Utility functions used in testing.
--
package Test_Util is

   -- Assertions --

   --
   -- Assert
   --
   --  Assert that Condition is True, using
   --  AUnit.Assertions.Assert. If Condition is False, displays
   --  Message.
   --
   --  If Fail is true, the current test function is aborted. If Fail
   --  is false, the current test function is not aborted and the
   --  remaining tests are performed.
   --
   procedure Assert (Condition : Boolean; Message : String; Fail : Boolean := False);

   --
   -- Assert_Equal_String
   --
   --  Assert that Actual = Expected, using the provided "="
   --  function. If Actual /= Expected, Message is printed followed by
   --  the value of Actual and Expected, which are converted to
   --  strings using the Image function.
   --
   --  If Fail is true, the current test function is aborted. If Fail
   --  is false, the current test function is not aborted and the
   --  remaining tests are performed.
   --
   generic
      type Test_Type(<>) is private;
      with function Image (Value : Test_Type) return String;
      with function "="(Actual, Expected : Test_Type) return Boolean is <>;
   procedure Assert_Equal (Actual, Expected : Test_Type; Message : String := ""; Fail : Boolean := False);

   --
   -- Assert_Equal_String
   --
   --  Assert that Actual = Expected. If Actual /= Expected, Message
   --  is printed followed by the value of Actual and Expected.
   --
   --  If Fail is true, the current test function is aborted. If Fail
   --  is false, the current test function is not aborted and the
   --  remaining tests are performed.
   --
   procedure Assert_Equal_String (Actual, Expected : String; Message : String := ""; Fail : Boolean := False);

end Test_Util;
