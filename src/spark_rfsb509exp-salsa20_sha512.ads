with SPARKNaCl.Core;    use SPARKNaCl.Core;
with SPARKNaCl.Hashing; use SPARKNaCl.Hashing;

package SPARK_RFSB509EXP.Salsa20_SHA512
  with Pure,
       SPARK_Mode => On
is

   --------------------------------------------------------
   --  Procedural interface. Faster assuming Output is
   --  passed by reference
   --------------------------------------------------------

   procedure Hash (Output :    out Digest;
                   Input  : in     Byte_Seq;
                   Key    : in     Salsa20_Key)
     with Global => null;

   --------------------------------------------------------
   --  Functional interfaces
   --------------------------------------------------------

   function Hash (Input : in Byte_Seq;
                  Key   : in Salsa20_Key) return Digest
     with Global => null;

end SPARK_RFSB509EXP.Salsa20_SHA512;
