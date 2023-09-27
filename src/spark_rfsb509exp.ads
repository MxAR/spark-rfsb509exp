with Interfaces; use Interfaces;

package SPARK_RFSB509EXP
  with Pure
is
   --------------------------------------------------------
   --  General type definition(s)
   --------------------------------------------------------

   subtype Byte     is Unsigned_8;

   subtype I64      is Integer_64;
   subtype U64      is Unsigned_64;

   subtype I32      is Integer_32;
   subtype N32      is I32 range 0 .. I32'Last;
   subtype U32      is Unsigned_32;

   subtype Index_4  is U32 range 0 .. 3;
   subtype Index_8  is U32 range 0 .. 7;
   subtype Index_16 is U32 range 0 .. 15;
   subtype Index_48 is U32 range 0 .. 47;
   subtype Index_64 is U32 range 0 .. 63;

   --  If this type is used as the index type of an array, then the length of
   --  the resulting array can be always represented using an U32 integer.
   subtype U32_Index is U32 range 0 .. (U32'Last - 1);

   type Byte_Seq is array (U32_Index range <>) of Byte;

   subtype Bytes_8  is Byte_Seq (Index_8);
   subtype Bytes_16 is Byte_Seq (Index_16);
   subtype Bytes_48 is Byte_Seq (Index_48);
   subtype Bytes_64 is Byte_Seq (Index_64);

   --------------------------------------------------------
   --  Constant definition(s)
   --------------------------------------------------------

   --  The RFSB-509 compression function is defined by three numbers:
   --    w: number of input chunks i.e weight of the sum (positive integer)
   --    b: length of the input chunks in bits (positive integer)
   --    r: length of the output in bits (odd prime number)
   --  Dervied from those parameters is:
   --    s: length of the input in bits (w * b)
   --  In order for RFSB-509 to be a "compression" function s > r.
   RFSB509_W : constant I32 := 112;
   RFSB509_B : constant I32 := 8;
   RFSB509_R : constant I32 := 509;
   RFSB509_S : constant I32 := RFSB509_W * RFSB509_B;

   pragma Assert (RFSB509_S > RFSB509_R);

   --------------------------------------------------------
   --  RFSB509 specific type definition(s)
   --------------------------------------------------------

   subtype Matrix_Column is Bytes_64
     with Dynamic_Predicate =>
       (Matrix_Column (Matrix_Column'Last) and 2#1110_00_00#) = 0;

   subtype Data_Block    is Bytes_48;
   pragma Assert (Data_Block'Length = ((RFSB509_S - RFSB509_R) / RFSB509_B));

   subtype Digest        is Bytes_64;
   pragma Assert (Digest'Length = 64);

end SPARK_RFSB509EXP;
