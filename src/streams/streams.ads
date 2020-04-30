--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Interfaces.C;
with Interfaces.C.Pointers;

with File_System;

--
-- Purpose:
--
--  Provides an abstract stream interface for reading from and writing
--  to files.
--
package Streams is

   package C renames Interfaces.C;

   -- Primitive Types --

   subtype Byte is C.Unsigned_Char;

   subtype Size_Type is C.Size_T;

   type Byte_Array is array (Natural range <>) of aliased Byte;


   -- C Byte Array Types --

   package Pointers is new C.Pointers (Natural, Byte, Byte_Array, 0);

   subtype Block_Ptr is Pointers.Pointer;


   -- Stream Interfaces --

   --
   -- Instream
   --
   --  Input stream interface.
   --
   --  The resources associated with the stream object should be
   --  released when the object is deallocated.
   --
   type Instream is limited interface;

   --
   -- Read
   --
   --  Read a block of data from the stream.
   --
   --  Output Parameters:
   --
   --   Size: Size of the block
   --
   --   Offset: Offset to the start of the current block from the end
   --   of the previous block.
   --
   function Read (Stream : in out Instream;
                  Size   :    out Size_Type;
                  Offset :    out Size_Type)
                 return Block_Ptr
     is abstract;


   --
   -- Outstream
   --
   --  Output stream interface.
   --
   --  The resources associated with the stream object should be
   --  released when the object is deallocated.
   --
   type Outstream is limited interface;

   --
   -- Close
   --
   --  Close the output stream, releasing all resource associated with
   --  the object.
   --
   --  This procedure should raise an exception if there was an error
   --  committing the data to disk.
   --
   procedure Close (Stream : in out Outstream) is abstract;

   --
   -- Write
   --
   --  Write a block of data to the stream.
   --
   --  Block:  The block to write
   --
   --  Size:   The size of the block
   --
   --  Offset: Offset to the start of the block from the end of the
   --          previous block.
   --
   procedure Write (Stream : in out Outstream;
                    Block  :        Block_Ptr;
                    Size   :        Size_type;
                    Offset :        Size_Type)
     is abstract;


   -- Exceptions --

   Open_Error  : exception;
   Read_Error  : exception;
   Write_Error : exception;
   Close_Error : exception;

end Streams;
