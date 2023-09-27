with SPARKNaCl.Core;    use SPARKNaCl.Core;

package SPARK_RFSB509EXP.ChaCha20_SHA512
  with Pure
is

   --------------------------------------------------------
   --  Procedural interface. Faster assuming Output is
   --  passed by reference
   --------------------------------------------------------

   procedure Hash (Output :    out Digest;
                   Input  : in     Byte_Seq;
                   Key    : in     ChaCha20_Key)
     with Global => null;

   --------------------------------------------------------
   --  Functional interfaces
   --------------------------------------------------------

   function Hash (Input : in Byte_Seq;
                  Key   : in ChaCha20_Key) return Digest
     with Global => null;

end SPARK_RFSB509EXP.ChaCha20_SHA512;
