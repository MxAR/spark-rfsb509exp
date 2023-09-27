with SPARKNaCl;

package SPARK_RFSB509EXP.Utilities
  with Pure
is

   ----------------------------------------------------------------------------
   --  Procedual interfaces
   ----------------------------------------------------------------------------

   procedure Little_Endian_Unpack (Output :    out Bytes_8;
                                   Input  : in     U64)
     with Inline,
          Relaxed_Initialization => Output,
          Global => null,
          Post   => Output'Initialized and then (for all I in Output'Range =>
            Output (I) = Little_Endian_Get_Byte (Input, I - Output'First));

   procedure Convert_From_SPARKNaCl_Bytes (Output :    out Byte_Seq;
                                           Input  : in     SPARKNaCl.Byte_Seq)
     with Relaxed_Initialization => Output,
          Global => null,
          Pre    => U32 (Input'Length) = Output'Length,
          Post   => Output'Initialized and then (for all I in Input'Range =>
            Input (I) = Output (U32 (I - Input'First) + Output'First));

   ----------------------------------------------------------------------------
   --  Functional interfaces
   ----------------------------------------------------------------------------

   function Little_Endian_Get_Byte (Input : U64;
                                    Index : Index_8) return Byte
     with Inline,
          Global => null,
          Post   => Little_Endian_Get_Byte'Result =
            Byte (Shift_Right (Input, Integer (Index) * 8) mod 256);

   function Little_Endian_Unpack (Input : in U64) return Bytes_8
     with Inline,
          Global => null,
          Post   =>
            (for all I in Little_Endian_Unpack'Result'Range =>
            Little_Endian_Unpack'Result (I) =
            Little_Endian_Get_Byte
              (Input, I - Little_Endian_Unpack'Result'First));

   function Convert_To_SPARKNaCl_Bytes (Input : in Byte_Seq)
   return SPARKNaCl.Byte_Seq
     with Global => null,
          Pre    => (
            (Input'First <= Input'Last) and then
            (Input'Last <= U32 (SPARKNaCl.N32'Last))),
          Post   => (
            (Input'Length = Convert_To_SPARKNaCl_Bytes'Result'Length) and then
            (for all I in Input'Range => Input (I) =
              Convert_To_SPARKNaCl_Bytes'Result (SPARKNaCl.N32 (I))));

   function Broadcast_Byte (Input : in Byte) return U32
     with Inline,
          Global => null,
          Post   => (Broadcast_Byte'Result mod 16#01_01_01_01#) = 0;

   function Floor_Log2 (Input : in U32) return Integer
     with Global         => null,
          Pre            => Is_Positive (Input),
          Post           => (
            Is_Non_Negative (Floor_Log2'Result) and then
            (Floor_Log2'Result <= 31) and then
            ((2 ** Floor_Log2'Result) <= Input)),
          Contract_Cases => (
            Input < U32'Last => True,
            Input = U32'Last => Floor_Log2'Result = 31);

   function Divides (X : in U32;
                     Y : in U32) return Boolean
     with Global         => null,
          Pre            => Is_Positive (X),
          Post           => (
            (if Divides'Result = True  then Y mod X  = 0) and then
            (if Divides'Result = False then Y mod X /= 0)),
          Contract_Cases => (
            Y mod X  = 0 => Divides'Result = True,
            Y mod X /= 0 => Divides'Result = False);

   function Is_POT (Input : in U32) return Boolean
     with Global         => null,
          Post           => (
            (if Is_POT'Result = True  then
              (for some I in 0 .. 31 => Input  = 2 ** I)) and then
            (if Is_POT'Result = False then
              (for  all I in 0 .. 31 => Input /= 2 ** I))),
          Contract_Cases => (
            (for some I in 0 .. 31 => Input  = 2 ** I) =>
              Is_POT'Result = True,
            (for  all I in 0 .. 31 => Input /= 2 ** I) =>
              Is_POT'Result = False);

   function Double (Input : in U32) return U32
     with Inline,
          Global         => null,
          Pre            => Input <= (U32'Last / 2),
          Post           =>
            (Double'Result = 2 * Input) and then
            (Double'Result <= U32'Last),
          Contract_Cases => (
            Input = 0 => Double'Result = 0,
            Input > 0 => Double'Result > Input);

   function Is_Positive (Input : in U32) return Boolean
     with Inline,
          Global         => null,
          Post           => (
            (if Is_Positive'Result = True  then 0 < Input) and then
            (if Is_Positive'Result = False then Input = 0)),
          Contract_Cases => (
            0 < Input => Is_Positive'Result = True,
            Input = 0 => Is_Positive'Result = False);

   function Is_Non_Negative (Input : in Integer) return Boolean
     with Inline,
          Global         => null,
          Post           => (
            (if Is_Non_Negative'Result = True  then 0 <= Input) and then
            (if Is_Non_Negative'Result = False then Input < 0)),
          Contract_Cases => (
            0 <= Input => Is_Non_Negative'Result = True,
            Input < 0  => Is_Non_Negative'Result = False);

end SPARK_RFSB509EXP.Utilities;
