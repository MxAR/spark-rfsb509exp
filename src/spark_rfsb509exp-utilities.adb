package body SPARK_RFSB509EXP.Utilities
  with Pure,
       SPARK_Mode => On
is
   function Little_Endian_Get_Byte (Input : in U64;
                                    Index : in Index_8) return Byte
   is
      Shift_Amount : constant Integer := Integer (Index) * Byte'Size;
      Return_Value : constant Byte :=
        Byte (Shift_Right (Input, Shift_Amount) mod 256);
   begin
      return Return_Value;
   end Little_Endian_Get_Byte;

   procedure Little_Endian_Unpack (Output :    out Bytes_8;
                                   Input  : in     U64)
   is
   begin
      for I in Output'Range loop
         Output (I) := Little_Endian_Get_Byte (Input, I - Output'First);

         pragma Loop_Invariant (Output (Output'First .. I)'Initialized and then
           (for all J in Output'First .. I => Output (J) =
             Little_Endian_Get_Byte (Input, J - Output'First)));
      end loop;
   end Little_Endian_Unpack;

end SPARK_RFSB509EXP.Utilities;
