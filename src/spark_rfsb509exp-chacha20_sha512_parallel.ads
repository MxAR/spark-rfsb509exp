with SPARK_RFSB509EXP.Utilities;          use SPARK_RFSB509EXP.Utilities;
with SPARK_RFSB509EXP.Parallel_Hash_Tree;

with SPARKNaCl.Core; use SPARKNaCl.Core;

package SPARK_RFSB509EXP.ChaCha20_SHA512_Parallel
is

   procedure Compress (Output :    out Digest;
                       A      : in     Digest;
                       B      : in     Digest;
                       Key    : in     ChaCha20_Key)
     with Global => null;

   ----------------------------------------------------------------------------
   --  Instantiation of generics
   ----------------------------------------------------------------------------

   --  These constants are providing default values for the types used in the
   --  parallel hash tree instatiation
   Digest_Default_Value  : constant Digest             := (others => 0);
   Raw_Key_Default_Value : constant SPARKNaCl.Bytes_32 := (others => 0);

   package Hash_Tree is new Parallel_Hash_Tree (
     Digest_Type  => Digest,
     Key_Type     => ChaCha20_Key,
     Raw_Key_Type => SPARKNaCl.Bytes_32,
     Digest_Type_Default  => Digest_Default_Value,
     Raw_Key_Type_Default => Raw_Key_Default_Value,
     Construct_Key        => SPARKNaCl.Core.Construct,
     Serialize_Key        => SPARKNaCl.Core.Serialize,
     Sanitize_Key         => SPARKNaCl.Core.Sanitize,
     Compression_Function => Compress);

   ----------------------------------------------------------------------------
   --  Procedural interface. Faster assuming Output is passed by reference
   ----------------------------------------------------------------------------

   procedure Hash (Output :    out Digest;
                   Input  : in     Byte_Seq;
                   Key    : in     ChaCha20_Key)
     with Global => (
            In_Out => (
              Hash_Tree.Left_Tree,
              Hash_Tree.Right_Tree,
              Hash_Tree.Tree_Top_Hash)),
          Pre    => Input'Length <= Maximum_Input_Length;

   ----------------------------------------------------------------------------
   --  Functional interfaces
   ----------------------------------------------------------------------------

   --  TODO move to spark_rfsb509exp.ads after successful prove
   pragma Assert ((2 ** 6) = Digest'Length);

   --  This function returns the largest element of U32 that has the form
   --  Digest'Length * (2**I) where I is a non-negative Integer. The body of
   --  this function is as simple as it is because Digest'Length is a POT.
   function Maximum_Input_Length return U32
     with Pure_Function,
          Global => null,
          Pre    => Is_POT (Digest'Length),
          Post   => Maximum_Input_Length'Result <= (U32'Last / 2) + 1;

end SPARK_RFSB509EXP.ChaCha20_SHA512_Parallel;
