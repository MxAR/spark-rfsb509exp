with SPARK_RFSB509EXP.Utilities; use SPARK_RFSB509EXP.Utilities;

with SPARKNaCl.Stream;         use SPARKNaCl.Stream;
with SPARKNaCl.Hashing.SHA512; use SPARKNaCl.Hashing.SHA512;

package body SPARK_RFSB509EXP.ChaCha20_SHA512
is

   ----------------------------------------------------------------------------
   --  Local subprogram declaration(s)
   ----------------------------------------------------------------------------

   procedure Calculate_Column (Column :    out Bytes_64;
                               Word   : in     Byte;
                               Key    : in     ChaCha20_Key)
     with Global => null,
          Post   => ((Column (Column'Last) and 2#1110_00_00#) = 0);

   procedure Mul128 (A : in out Matrix_Column)
     with Global => null;

   procedure Add (Sum     : in out Matrix_Column;
                  Summand : in     Matrix_Column)
     with Global => null;

   procedure Compress (Chain_Value : in out Matrix_Column;
                       Block       : in     Data_Block;
                       Key         : in     ChaCha20_Key)
     with Global => null;

   procedure Hash_Blocks (Output          : in out Matrix_Column;
                          Bytes_Remaining :    out Index_48;
                          Input           : in     Byte_Seq;
                          Key             : in     ChaCha20_Key)
     with Global => null,
          Post   => I64 (Bytes_Remaining) <= Input'Length;

   procedure Hash_Local (Output :    out Digest;
                         Input  : in     Byte_Seq;
                         Key    : in     ChaCha20_Key)
     with Global => null;

   ----------------------------------------------------------------------------
   --  Local subprogram bodies
   ----------------------------------------------------------------------------

   procedure Calculate_Column (Column :    out Bytes_64;
                               Word   : in     Byte;
                               Key    : in     ChaCha20_Key)
   is
      Nonce : constant ChaCha20_Nonce :=
        (ChaCha20_Nonce'Last => Word, others => 0);

      Five_LSB_Mask         : constant Byte := 2#0001_1111#;
      First_Byte, Last_Byte : Byte;
   begin
      declare
         SPARKNaCl_Output : SPARKNaCl.Bytes_64;
      begin
         ChaCha20 (SPARKNaCl_Output, Nonce, Key, 0);
         Convert_From_SPARKNaCl_Bytes (Column, SPARKNaCl_Output);
      end;

      First_Byte := Column (Column'First);
      Last_Byte  := Column (Column'Last);

      Column (Column'First) := First_Byte xor Shift_Right (Last_Byte, 5);
      Column (Column'Last)  := Last_Byte and Five_LSB_Mask;
   end Calculate_Column;

   procedure Mul128 (A : in out Matrix_Column)
   is
      Three_MSB_Mask : constant Byte := 2#1110_0000#;

      Exponent     : constant Index_64 := (128 / Byte'Size);
      Shift_Amount : constant Integer := 5;

      Product : Bytes_64 with Relaxed_Initialization;

      Factor_Pivot  : constant Index_64 := A'Last - Exponent;
      Product_Pivot : constant Index_64 := Product'First + Exponent;

      Factor_Index  : Index_64 := Factor_Pivot;
      Product_Index : Index_64 := Product'First;

      Current_Byte : Byte := A (Factor_Index);
      Lower_Part   : Byte := Shift_Right (Current_Byte, Shift_Amount);
      Upper_Part   : Byte;
   begin
      Product (Product_Index) := Lower_Part;

      Factor_Index  := Index_64'Succ (Factor_Index);

      while (Product_Index < Product_Pivot) loop
         pragma Loop_Variant
           (Increases => Factor_Index, Increases => Product_Index);
         pragma Loop_Invariant
           ((Factor_Index - Product_Index = A'Last - Product_Pivot + 1) and
             (Product (Product'First .. Product_Index)'Initialized) and
             ((Product (Product_Index) and
               (not Shift_Right (Three_MSB_Mask, Shift_Amount))) = 0));

         Current_Byte := A (Factor_Index);

         Lower_Part := Shift_Right (Current_Byte, Shift_Amount);
         Upper_Part := Shift_Left (Current_Byte, Byte'Size - Shift_Amount);

         Product (Product_Index) := Product (Product_Index) or Upper_Part;
         Product_Index           := Index_64'Succ (Product_Index);
         Product (Product_Index) := Lower_Part;

         exit when Product_Index = Product_Pivot;
         Factor_Index := Index_64'Succ (Factor_Index);
      end loop;

      pragma Assert (Product_Index = Product_Pivot);
      pragma Assert (Factor_Index = A'Last);

      Product (Product_Index) := Product (Product_Index) xor A (A'First);
      Product_Index           := Index_64'Succ (Product_Index);

      Product (Product_Index .. Product'Last) :=
        A (A'First + 1 .. Factor_Pivot);

      Product (Product'Last) :=
        Product (Product'Last) and (not Three_MSB_Mask);

      A := Product;
   end Mul128;

   procedure Add (Sum     : in out Matrix_Column;
                  Summand : in     Matrix_Column)
   is
   begin
      for I in Sum'Range loop
         Sum (I) := Sum (I) xor Summand (I);
      end loop;
   end Add;

   procedure Compress (Chain_Value : in out Matrix_Column;
                       Block       : in     Data_Block;
                       Key         : in     ChaCha20_Key)
   is
      Sum    : Matrix_Column;
      Column : Matrix_Column;
   begin
      Calculate_Column (Sum, Chain_Value (Digest'First), Key);

      for I in Chain_Value'First + 1 .. Chain_Value'Last loop
         Mul128 (Sum);
         Calculate_Column (Column, Chain_Value (I), Key);
         Add (Sum, Column);
      end loop;

      for I in Block'Range loop
         Mul128 (Sum);
         Calculate_Column (Column, Block (I), Key);
         Add (Sum, Column);
      end loop;

      Chain_Value := Sum;
   end Compress;

   procedure Hash_Blocks (Output          : in out Matrix_Column;
                          Bytes_Remaining :    out Index_48;
                          Input           : in     Byte_Seq;
                          Key             : in     ChaCha20_Key)
   is
      Remainder    : U32 := Input'Length;
      Current_Byte : U32 := Input'First;

      Block : Data_Block;
   begin
      while Remainder >= Block'Length loop
         pragma Loop_Variant (
           Increases => Current_Byte,
           Decreases => Remainder);
         pragma Loop_Invariant (
           (U64 (Remainder) + U64 (Current_Byte) = U64 (Input'Last) + 1)
           and then (Remainder in Block'Length .. Input'Length) and then
           (Current_Byte in Input'First .. ((Input'Last - Block'Length) + 1)));

         Block := Input (Current_Byte .. Current_Byte + (Block'Length - 1));
         Compress (Output, Block, Key);

         pragma Assert (Remainder >= Block'Length);
         Remainder := Remainder - Block'Length;

         exit when Remainder < Block'Length;
         Current_Byte := Current_Byte + Block'Length;
      end loop;

      Bytes_Remaining := Index_48 (Remainder);
   end Hash_Blocks;

   procedure Hash_Local (Output :    out Digest;
                         Input  : in     Byte_Seq;
                         Key    : in     ChaCha20_Key)
   is
      Hash  : Matrix_Column := (others => 0);
      Block : Data_Block with Relaxed_Initialization;

      Bytes_Remaining, Block_Index : Index_48;
   begin
      Hash_Blocks (Hash, Bytes_Remaining, Input, Key);
      Block_Index := Block'First + Bytes_Remaining;

      if Bytes_Remaining > 0 then
         Block (Block'First .. Block_Index - 1) :=
           Input ((Input'Last - Bytes_Remaining) + 1 .. Input'Last);

         if Block_Index > (Block'Last - 7) then
            Block (Block_Index .. Block'Last) := (others => 0);
            Compress (Hash, Block, Key);

            Block_Index := Block'First;
         end if;
      end if;

      Block (Block_Index .. Block'Last - 8) := (others => 0);
      Little_Endian_Unpack (
        Block (Block'Last - 7 .. Block'Last), U64 (Input'Length));

      Compress (Hash, Block, Key);

      declare
         SPARKNaCl_Output : SPARKNaCl.Bytes_64;
      begin
         SPARKNaCl.Hashing.SHA512.Hash (SPARKNaCl_Output,
                                        Convert_To_SPARKNaCl_Bytes (Hash));
         Convert_From_SPARKNaCl_Bytes (Output, SPARKNaCl_Output);
      end;
   end Hash_Local;

   ----------------------------------------------------------------------------
   --  Global subprogram bodies
   ----------------------------------------------------------------------------

   procedure Hash (Output :    out Digest;
                   Input  : in     Byte_Seq;
                   Key    : in     ChaCha20_Key)
   is
   begin
      Hash_Local (Output, Input, Key);
   end Hash;

   function Hash (Input : in Byte_Seq;
                  Key   : in ChaCha20_Key) return Digest
   is
      Output : Digest;
   begin
      Hash_Local (Output, Input, Key);
      return Output;
   end Hash;

end SPARK_RFSB509EXP.ChaCha20_SHA512;
