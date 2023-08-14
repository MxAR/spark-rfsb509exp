package SPARK_RFSB509EXP.Utilities
  with Pure,
       SPARK_Mode => On
is
   function Little_Endian_Get_Byte (Input : U64;
                                    Index : Index_8) return Byte
     with Inline,
          Global => null,
          Post   => Little_Endian_Get_Byte'Result =
            Byte (Shift_Right (Input, Integer (Index) * 8) mod 256);

   procedure Little_Endian_Unpack (Output :    out Bytes_8;
                                   Input  : in     U64)
     with Inline,
          Relaxed_Initialization => Output,
          Global => null,
          Post   => Output'Initialized and then (for all I in Output'Range =>
           Output (I) = Little_Endian_Get_Byte (Input, I - Output'First));

end SPARK_RFSB509EXP.Utilities;
