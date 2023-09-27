package body SPARK_RFSB509EXP.Utilities
  with Pure
is
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

   procedure Convert_From_SPARKNaCl_Bytes (Output :    out Byte_Seq;
                                           Input  : in     SPARKNaCl.Byte_Seq)
   is
   begin
      for I in Input'Range loop
         Output (Output'First + U32 (I - Input'First)) := Input (I);

         pragma Loop_Invariant (
           (Output (Output'First ..
             Output'First + U32 (I - Input'First))'Initialized) and then
           (for all J in Input'First .. I => Input (J) =
             Output (Output'First + U32 (J - Input'First))));
      end loop;
   end Convert_From_SPARKNaCl_Bytes;

   function Little_Endian_Get_Byte (Input : in U64;
                                    Index : in Index_8) return Byte
   is
      Shift_Amount : constant Integer := Integer (Index) * Byte'Size;
      Return_Value : constant Byte :=
        Byte (Shift_Right (Input, Shift_Amount) mod 256);
   begin
      return Return_Value;
   end Little_Endian_Get_Byte;

   function Little_Endian_Unpack (Input : in U64) return Bytes_8
   is
      Result : Bytes_8;
   begin
      Little_Endian_Unpack (Result, Input);

      return Result;
   end Little_Endian_Unpack;

   function Convert_To_SPARKNaCl_Bytes (Input : in Byte_Seq)
     return SPARKNaCl.Byte_Seq
   is
      Output : SPARKNaCl.Byte_Seq (N32 (Input'First) .. N32 (Input'Last))
        with Relaxed_Initialization;
   begin
      for I in Input'Range loop
         Output (N32 (I)) := Input (I);

         pragma Loop_Invariant (
           (Output (N32 (Input'First) .. N32 (I))'Initialized) and then
           (for all J in Input'First .. I => Input (J) = Output (N32 (J))));
      end loop;

      return Output;
   end Convert_To_SPARKNaCl_Bytes;

   function Broadcast_Byte (Input : in Byte) return U32
   is
      Factor : constant U32 := 16#01_01_01_01#;
      Result : constant U32 := U32 (Input) * Factor;
   begin
      return Result;
   end Broadcast_Byte;

   function Floor_Log2 (Input : in U32) return Integer
   is
      Power_Of_Two : U32     := 1;
      Exponent     : Integer := 0;
      Result       : Integer;
   begin
      while Power_Of_Two < Input loop
         pragma Loop_Variant (
           Increases => Power_Of_Two,
           Increases => Exponent);
         pragma Loop_Invariant (
           0 <= Exponent and then
           (Power_Of_Two = 2 ** Exponent) and then
           Exponent <= 31);

         exit when Power_Of_Two >= (U32'Last / 2);
         pragma Assert (Power_Of_Two < (U32'Last / 2));

         Power_Of_Two := Double (Power_Of_Two);
         Exponent     := Integer'Succ (Exponent);
      end loop;

      Result := Exponent;
      if Power_Of_Two > Input then
         Result := Result - 1;
      end if;

      return Result;
   end Floor_Log2;

   function Divides (X : in U32;
                     Y : in U32) return Boolean
   is
      Result : constant Boolean := (Y mod X) = 0;
   begin
      return Result;
   end Divides;

   function Is_POT (Input : in U32) return Boolean
   is
      Power_Of_Two : U32     := 1;
      Exponent     : Integer := 0 with Ghost;
   begin
      while Power_Of_Two < Input loop
         pragma Loop_Variant (
           Increases => Power_Of_Two,
           Increases => Exponent);
         pragma Loop_Invariant (
           0 <= Exponent and then
           Power_Of_Two = 2 ** Exponent and then
           Exponent <= 31);

         exit when Power_Of_Two > (U32'Last / 2);
         pragma Assert (Power_Of_Two <= (U32'Last / 2));

         Power_Of_Two := Double (Power_Of_Two);
         Exponent     := Integer'Succ (Exponent);
      end loop;

      pragma Assert (Power_Of_Two = 2 ** Exponent);

      declare
         Result : Boolean;
      begin
         if Input = Power_Of_Two then
            Result := True;
         else
            Result := False;
         end if;

         return Result;
      end;
   end Is_POT;

   function Double (Input : in U32) return U32
   is
      Result : constant U32 := 2 * Input;
   begin
      return Result;
   end Double;

   function Is_Positive (Input : in U32) return Boolean
   is
      Result : constant Boolean := Input > 0;
   begin
      return Result;
   end Is_Positive;

   function Is_Non_Negative (Input : in Integer) return Boolean
   is
      Result : constant Boolean := Input >= 0;
   begin
      return Result;
   end Is_Non_Negative;

end SPARK_RFSB509EXP.Utilities;
