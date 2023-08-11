with Interfaces; use Interfaces;

package SPARK_RFSB509EXP
  with Pure,
       SPARK_Mode => On
is
   subtype Byte     is Unsigned_8;

   subtype I64      is Integer_64;
   subtype U64      is Unsigned_64;

   subtype I32      is Integer_32;
   subtype N32      is I32 range 0 .. I32'Last;

   subtype Index_4  is I32 range 0 .. 3;
   subtype Index_8  is I32 range 0 .. 7;
   subtype Index_16 is I32 range 0 .. 15;
   subtype Index_48 is I32 range 0 .. 47;
   subtype Index_64 is I32 range 0 .. 64;

   type Byte_Seq is array (N32 range <>) of Byte;

   subtype Bytes_8  is Byte_Seq (Index_8);
   subtype Bytes_16 is Byte_Seq (Index_16);
   subtype Bytes_48 is Byte_Seq (Index_48);
   subtype Bytes_64 is Byte_Seq (Index_64);

end SPARK_RFSB509EXP;
