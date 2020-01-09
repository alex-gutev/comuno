--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;

with AUnit;
with AUnit.Run;
with AUnit.Reporter.Text;

with Test_Directory_Lister;

procedure Test_Comuno is
   use type AUnit.Status;

   function Runner is new Aunit.Run.Test_Runner_With_Status (Test_Directory_Lister.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   if Runner (Reporter) /= Aunit.Success then
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;
end Test_Comuno;
