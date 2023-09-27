package body SPARK_RFSB509EXP.Parallel_Hash_Tree
is
   procedure Calculate_Top_Hash (Hash  :    out Digest_Type;
                                 Leafs :        access constant Digest_Array;
                                 Key   : in     Key_Type)
     with Global => null,
          Pre    => Leafs /= null;

   procedure Calculate_Top_Hash (Hash  :    out Digest_Type;
                                 Leafs :        access constant Digest_Array;
                                 Key   : in     Key_Type)
   is
      Stride           : U32 := 2 ** 1;
      Sibling_Distance : U32 := 2 ** 0;
      Child_Index      : U32;

      Compression_Result : Digest_Type;

      pragma Assert (Is_POT (Leafs.all'Length));
      pragma Assert (Is_Positive (Leafs.all'Length)); --  -> from prev.
      Buffer : Digest_Array (0 .. (Leafs.all'Length -  1)) := Leafs.all;
   begin
      while Stride < Buffer'Length loop
         pragma Loop_Variant (
           Increases => Stride,
           Increases => Sibling_Distance);
         pragma Loop_Invariant (
           Stride = 2 * Sibling_Distance and then
           Is_POT (Sibling_Distance) and then
           Is_POT (Stride));

         Child_Index := Buffer'First;
         loop
            pragma Loop_Variant (Increases => Child_Index);
            pragma Loop_Invariant (
              Child_Index in Buffer'First .. (Buffer'Last - Sibling_Distance));

            Compression_Function (Compression_Result,
                                  Buffer (Child_Index),
                                  Buffer (Child_Index + Sibling_Distance),
                                  Key);
            Buffer (Child_Index) := Compression_Result;

            exit when I64 (Buffer'Last) <
              (I64 (Child_Index) + I64 (Stride)) + I64 (Sibling_Distance);

            Child_Index := Child_Index + Stride;
         end loop;

         exit when Stride > (U32'Last / 2);
         Stride           := Double (Stride);
         Sibling_Distance := Double (Sibling_Distance);
      end loop;

      Hash := Buffer (Buffer'First);
   end Calculate_Top_Hash;

   protected body Tree_Top_Hash
   is
      entry Get (Hash            :    out Digest_Type;
                 Compression_Key : in     Key_Type)
        when Both_Trees_Ready
      is
      begin
         Compression_Function (Hash,
                               Left_Tree_Hash,
                               Right_Tree_Hash,
                               Compression_Key);

         Left_Tree_Ready  := False;
         Right_Tree_Ready := False;
         Both_Trees_Ready := False;
      end Get;

      procedure Set_Left_Tree_Top_Hash (Hash : in Digest_Type)
      is
      begin
         Left_Tree_Hash  := Hash;
         Left_Tree_Ready := True;

         Both_Trees_Ready := Left_Tree_Ready and Right_Tree_Ready;
      end Set_Left_Tree_Top_Hash;

      procedure Set_Right_Tree_Top_Hash (Hash : in Digest_Type)
      is
      begin
         Right_Tree_Hash  := Hash;
         Right_Tree_Ready := True;

         Both_Trees_Ready := Left_Tree_Ready and Right_Tree_Ready;
      end Set_Right_Tree_Top_Hash;
   end Tree_Top_Hash;

   protected body Tree
   is
      entry Start (Tree_Leafs : out Digest_Array_Ptr;
                   Key        : out Key_Type)
        when Ready
      is
      begin
         pragma Assert (Leafs /= null);

         Tree_Leafs := Leafs;
         Leafs      := null;

         Construct_Key (Key, Raw_Key);
         Ready := False;
      end Start;

      procedure Setup (Input : in Digest_Array;
                       Key   : in Key_Type)
      is
      begin
         pragma Warnings (Off,
           "violation of restriction " & Character'Val (34) &
           "No_Allocators" & Character'Val (34));
         Leafs := new Digest_Array'(
           0 .. (Input'Length - 1) => Digest_Type_Default);
         pragma Warnings (On,
           "violation of restriction " & Character'Val (34) &
           "No_Allocators" & Character'Val (34));
         pragma Assert (Leafs.all'Length = Input'Length);

         Leafs.all := Input;

         Raw_Key := Serialize_Key (Key);
         Ready   := True;

         pragma Assert (Leafs /= null);
      end Setup;

      procedure Sanitize
      is
      begin
         Raw_Key := Raw_Key_Type_Default;
         pragma Inspection_Point (Raw_Key); --  See RM H3.2 (9)

         Ready := False;

         pragma Assert (Leafs /= null);
      end Sanitize;
   end Tree;

   task body Left_Tree_Worker
   is
      Leafs : Digest_Array_Ptr;
      Key   : Key_Type;

      Hash : Digest_Type;
   begin
      loop
         Left_Tree.Start (Leafs, Key);
         Calculate_Top_Hash (Hash, Leafs, Key);

         Sanitize_Key (Key);

         Tree_Top_Hash.Set_Left_Tree_Top_Hash (Hash);
         Leafs := null;
      end loop;
   end Left_Tree_Worker;

   task body Right_Tree_Worker
   is
      Leafs : Digest_Array_Ptr;
      Key   : Key_Type;

      Hash : Digest_Type;
   begin
      loop
         Right_Tree.Start (Leafs, Key);
         Calculate_Top_Hash (Hash, Leafs, Key);

         Sanitize_Key (Key);

         Tree_Top_Hash.Set_Right_Tree_Top_Hash (Hash);
         Leafs := null;
      end loop;
   end Right_Tree_Worker;

end SPARK_RFSB509EXP.Parallel_Hash_Tree;
