with SPARK_RFSB509EXP.Utilities; use SPARK_RFSB509EXP.Utilities;

generic
   type Digest_Type  is private;
   type Key_Type     is limited private;
   type Raw_Key_Type is private;
   Digest_Type_Default  : in Digest_Type;
   Raw_Key_Type_Default : in Raw_Key_Type;
   with procedure Construct_Key (Key     :    out Key_Type;
                                 Raw_Key : in     Raw_Key_Type);
   with function Serialize_Key (Key : in Key_Type) return Raw_Key_Type;
   with procedure Sanitize_Key (Key : out Key_Type);
   with procedure Compression_Function (Output       :    out Digest_Type;
                                        First_Input  : in     Digest_Type;
                                        Second_Input : in     Digest_Type;
                                        Key          : in     Key_Type);
package SPARK_RFSB509EXP.Parallel_Hash_Tree
is
   type Digest_Array is array (U32_Index range <>) of Digest_Type
      with Dynamic_Predicate => Is_POT (Digest_Array'Length);
   type Digest_Array_Ptr is access Digest_Array;

   protected Tree_Top_Hash
   is
      entry Get (Hash            :    out Digest_Type;
                 Compression_Key : in     Key_Type)
        with Global => null;
      procedure Set_Left_Tree_Top_Hash (Hash : in Digest_Type)
        with Global => null;
      procedure Set_Right_Tree_Top_Hash (Hash : in Digest_Type)
        with Global => null;
   private
      Left_Tree_Hash, Right_Tree_Hash : Digest_Type := Digest_Type_Default;
      Left_Tree_Ready, Right_Tree_Ready, Both_Trees_Ready : Boolean := False;
   end Tree_Top_Hash;

   protected type Tree
   is
      entry Start (Tree_Leafs : out Digest_Array_Ptr;
                   Key        : out Key_Type)
        with Global => null,
             Post   => Tree_Leafs /= null;
      procedure Setup (Input : in Digest_Array;
                       Key   : in Key_Type)
        with Global => null;
      procedure Sanitize
        with Global => null;
   private
      Leafs   : Digest_Array_Ptr := null;
      Raw_Key : Raw_Key_Type     := Raw_Key_Type_Default;

      Ready : Boolean := False;
   end Tree;

   Left_Tree, Right_Tree : Tree;

private
   task Left_Tree_Worker
     with Global => (
       In_Out => Left_Tree,
       Output => Tree_Top_Hash);
   task Right_Tree_Worker
     with Global => (
       In_Out => Right_Tree,
       Output => Tree_Top_Hash);
end SPARK_RFSB509EXP.Parallel_Hash_Tree;
