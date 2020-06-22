--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Exceptions;

package body Error_Handling is

   procedure Try_Op (Op : access procedure) is
   begin
      loop
         begin
            Op.all;

         exception
            when E : others =>
               declare
                  Global_Handler : Handler_Holders.Holder :=
                    Task_Error_Handler.Value;

                  Err : Error;

               begin
                  if Global_Handler.Is_Empty then
                     raise;
                  end if;

                  Err.Error.Set(E);
                  Global_Handler.Reference.Handle(Err);

               end;
         end;
      end loop;
   end Try_Op;

end Error_Handling;
