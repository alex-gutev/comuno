--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Unchecked_Deallocation;

package body Exception_Holders is

   use type Ada.Exceptions.Exception_Occurrence_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Ada.Exceptions.Exception_Occurrence,
      Ada.Exceptions.Exception_Occurrence_Access);

   --
   -- Release_Error
   --
   --  Free the memory held by the Holder's Exception_Occurrence
   --  Object.
   --
   procedure Release_Error (This : in out Holder) is
   begin
      if This.Error /= null then
         Free(This.Error);
      end if;
   end Release_Error;


   overriding procedure Adjust (This : in out Holder) is
      Ptr : Ada.Exceptions.Exception_Occurrence_Access := This.Error;

   begin
      if Ptr /= null then
         This.Error := new Ada.Exceptions.Exception_Occurrence;

         Ada.Exceptions.Save_Occurrence(This.Error.all, Ptr.all);
      end if;

   end Adjust;

   overriding procedure Finalize (This : in out Holder) is
   begin
      This.Release_Error;
   end Finalize;


   procedure Set (This : in out Holder; Error : in Ada.Exceptions.Exception_Occurrence) is
   begin
      This.Release_Error;

      This.Error := new Ada.Exceptions.Exception_Occurrence;
      Ada.Exceptions.Save_Occurrence(This.Error.all, Error);
   end Set;

   function Get (This : Holder) return Ada.Exceptions.Exception_Occurrence is
   begin
      return Error : Ada.Exceptions.Exception_Occurrence do
         Ada.Exceptions.Save_Occurrence(Error, This.Error.all);
      end return;
   end Get;

end Exception_Holders;
