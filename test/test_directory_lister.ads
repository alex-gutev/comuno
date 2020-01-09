--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Aunit; use Aunit;
with Aunit.Test_Cases; use Aunit.Test_Cases;
with AUnit.Test_Suites;

--
-- Purpose:
--
--  Contains the tests for Directory_Lister.
--
package Test_Directory_Lister is
   type Test is new Aunit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test);

   function Name (T : Test) return Message_String;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Test_Directory_Lister;
