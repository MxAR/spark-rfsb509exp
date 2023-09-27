with SPARK_RFSB509EXP.ChaCha20_SHA512; use SPARK_RFSB509EXP.ChaCha20_SHA512;

package body SPARK_RFSB509EXP.ChaCha20_SHA512_Parallel
is

   ----------------------------------------------------------------------------
   --  Local subprogram declarations
   ----------------------------------------------------------------------------

   function Is_Valid_Padding_Length (Length : in U32) return Boolean
     with Ghost,
          Global         => null,
          Pre            => Is_POT (Digest'Length),
          Post           => (
            (if Is_Valid_Padding_Length'Result = True then (
              Is_Positive (Length) and then
              Divides (Digest'Length, Length) and then
              Is_POT (Length)))),
          Contract_Cases => (
            Is_Positive (Length) and then
            Divides (Digest'Length, Length) and then
            Is_POT (Length) =>
              Is_Valid_Padding_Length'Result = True,
            not Is_Positive (Length) or else
            not Divides (Digest'Length, Length) or else
            not Is_POT (Length) =>
              Is_Valid_Padding_Length'Result = False);

   function Padding_Length (Length : in U32) return U32
     with Global => null,
          Pre    => (
            Is_POT (Digest'Length) and then
            Length <= Maximum_Input_Length),
          Post   => (
            Length <= Padding_Length'Result and then
            Is_Valid_Padding_Length (Padding_Length'Result));

   procedure Hash_Blocks (Output :    out Digest;
                          Input  : in     Byte_Seq;
                          Key    : in     ChaCha20_Key)
     with Global => (
            In_Out => (
              Hash_Tree.Left_Tree,
              Hash_Tree.Right_Tree,
              Hash_Tree.Tree_Top_Hash)),
          Pre    => (
            Is_Valid_Padding_Length (Input'Length) and then
            Input'Length >= 2 * Digest'Length);

   procedure Hash_Local (Output :    out Digest;
                         Input  : in     Byte_Seq;
                         Key    : in     ChaCha20_Key)
     with Global => (
            In_Out => (
              Hash_Tree.Left_Tree,
              Hash_Tree.Right_Tree,
              Hash_Tree.Tree_Top_Hash)),
          Pre    => Input'Length <= Maximum_Input_Length;


   ----------------------------------------------------------------------------
   --  Local subprogram bodies
   ----------------------------------------------------------------------------

   function Is_Valid_Padding_Length (Length : in U32) return Boolean
   is
      A : constant Boolean := Is_Positive (Length);
      B : constant Boolean := Divides (Digest'Length, Length);
      C : constant Boolean := Is_POT (Length / Digest'Length);

      Result : constant Boolean := A and B and C;
   begin
      return Result;
   end Is_Valid_Padding_Length;

   function Padding_Length (Length : in U32) return U32
   is
      Padded_Length   : U32 := Digest'Length;
      Iteration_Count : Natural := 0 with Ghost;
   begin
      pragma Assert (Length <= (U32'Last / 2) + 1);

      while Padded_Length < Length loop
         pragma Loop_Variant (Increases => Padded_Length);
         pragma Loop_Invariant (
           Padded_Length <= (U32'Last / 2) and then
           Padded_Length = Digest'Length * (2 ** Iteration_Count) and then
           Divides (Digest'Length, Padded_Length) and then
           --  This predicate is sufficient as Digest'Length is a POT
           Is_POT (Padded_Length) and then
           Is_Valid_Padding_Length (Padded_Length));

         Padded_Length   := Double (Padded_Length);
         Iteration_Count := Natural'Succ (Iteration_Count);
      end loop;

      pragma Assert (Padded_Length = Digest'Length * (2 ** Iteration_Count));
      pragma Assert (Is_Valid_Padding_Length (Padded_Length));

      return Padded_Length;
   end Padding_Length;

   procedure Compress (Output :    out Digest;
                       A      : in     Digest;
                       B      : in     Digest;
                       Key    : in     ChaCha20_Key)
   is
      Buffer_Length : constant U32 := A'Length + B'Length;

      subtype Buffer_Index is U32 range 1 .. Buffer_Length;
      subtype Buffer_Array is Byte_Seq (Buffer_Index);

      Buffer : Buffer_Array with Relaxed_Initialization;
   begin
      Buffer (Buffer_Index'First .. Buffer_Index'First + A'Length - 1) := A;
      Buffer (Buffer_Index'First + A'Length .. Buffer_Index'Last) := B;

      ChaCha20_SHA512.Hash (Output, Buffer, Key);
   end Compress;

   procedure Hash_Blocks (Output :     out Digest;
                          Input  : in      Byte_Seq;
                          Key    : in      ChaCha20_Key)
   is
      Digest_Count      : constant U32 := Input'Length / Digest'Length;
      Digest_Count_Half : constant U32 := Digest_Count / 2;
      pragma Assert (Is_Positive (Digest_Count_Half));

      subtype Buffer_Array_Index is U32 range 1 .. Digest_Count_Half;
      subtype Buffer_Array is Hash_Tree.Digest_Array (Buffer_Array_Index);

      Left_Buffer, Right_Buffer : Buffer_Array;

      Input_lower_idx : U32 := Input'First;
      Input_upper_idx : U32 := Input'First + Digest'Length - 1;
   begin
      pragma Assert (Input'Length >= Digest'Length);
      pragma Assert (Left_Buffer'Length = Right_Buffer'Length);
      pragma Assert (Left_Buffer'Length + Right_Buffer'Length = Digest_Count);

      for I in Left_Buffer'Range loop
         pragma Loop_Variant (
           Increases => Input_lower_idx,
           Increases => Input_upper_idx);
         pragma Loop_Invariant (
            Input_upper_idx >= Input_lower_idx and then
            (Input_upper_idx - Input_lower_idx) = Digest'Length - 1 and then
            Input_upper_idx in Input'Range);

         Left_Buffer (I) := Input (Input_lower_idx .. Input_upper_idx);
         Input_lower_idx := Input_lower_idx + Digest'Length;
         Input_upper_idx := Input_upper_idx + Digest'Length;
      end loop;

      for I in Right_Buffer'Range loop
         pragma Loop_Variant (
           Increases => Input_lower_idx,
           Increases => Input_upper_idx);
         pragma Loop_Invariant (
            Input_upper_idx >= Input_lower_idx and then
            (Input_upper_idx - Input_lower_idx) = Digest'Length - 1 and then
            Input_upper_idx in Input'Range);

         Right_Buffer (I) := Input (Input_lower_idx .. Input_upper_idx);
         exit when I = Right_Buffer'Last;

         Input_lower_idx := Input_lower_idx + Digest'Length;
         Input_upper_idx := Input_upper_idx + Digest'Length;
      end loop;

      Hash_Tree.Left_Tree.Setup (Left_Buffer, Key);
      Hash_Tree.Right_Tree.Setup (Right_Buffer, Key);

      Hash_Tree.Tree_Top_Hash.Get (Output, Key);

      Hash_Tree.Left_Tree.Sanitize;
      Hash_Tree.Right_Tree.Sanitize;
   end Hash_Blocks;

   procedure Hash_Local (Output :    out Digest;
                         Input  : in     Byte_Seq;
                         Key    : in     ChaCha20_Key)
   is
      Padded_Length : constant U32 := Padding_Length (Input'Length);

      subtype Buffer_Index is U32 range 1 .. Padded_Length;
      subtype Buffer_Array is Byte_Seq (Buffer_Index);

      Input_Buffer : Buffer_Array with Relaxed_Initialization;

      Input_Length_Buffer : constant Bytes_8 := Little_Endian_Unpack (
         U64 (Input'Length));

      Input_Hash, Input_Length_Hash : Digest;
   begin
      if Input'Length > 0 then
         Input_Buffer (
           Input_Buffer'First ..
           Input_Buffer'First + (Input'Length - 1)) := Input;

         Input_Buffer (Input_Buffer'First + Input'Length .. Input_Buffer'Last)
            := (others => 0);
      else
         Input_Buffer := (others => 0);
      end if;

      if Input_Buffer'Length = Digest'Length then
         Input_Hash := Input_Buffer;
      else
         pragma Assert (Divides (Digest'Length, Input_Buffer'Length));
         pragma Assert (Input_Buffer'Length > Digest'Length);
         pragma Assert (Input_Buffer'Length >= 2 * Digest'Length);

         Hash_Blocks (Input_Hash, Input_Buffer, Key);
      end if;

      ChaCha20_SHA512.Hash (Input_Length_Hash, Input_Length_Buffer, Key);
      Compress (Output, Input_Hash, Input_Length_Hash, Key);
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

   function Maximum_Input_Length return U32
   is
      Result : constant U32 := 2 ** Floor_Log2 (U32'Last);
   begin
      return Result;
   end Maximum_Input_Length;

end SPARK_RFSB509EXP.ChaCha20_SHA512_Parallel;
