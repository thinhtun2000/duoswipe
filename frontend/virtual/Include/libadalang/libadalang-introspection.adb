with Libadalang.Implementation;    use Libadalang.Implementation;
with Libadalang.Introspection_Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Introspection is

   package Impl renames Introspection_Implementation;

   --  TODO: move implementation of functions dealing with values (Satisfies,
   --  Eval_Property, ...) to Impl. This is not not done yet as substantial
   --  work is required in order to convert back and forth public values
   --  (structures, symbols) to their internal representations.

   function Allocate (Kind : Value_Kind) return Value_Type;
   --  Allocate a polymorphic value of the given kind

   pragma Warnings (Off, "is not referenced");
   function To_Internal_Value
     (Value : Any_Value_Type) return Impl.Internal_Value;
   function From_Internal_Value
     (Value : Impl.Internal_Value) return Any_Value_Type;
   pragma Warnings (On, "is not referenced");

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Value_Access_Wrapper) is
   begin
      if Self.Value = null then
         return;
      end if;

      declare
         Rec : Value_Record renames Self.Value.all;
      begin
         Rec.Ref_Count := Rec.Ref_Count + 1;
      end;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Value_Access_Wrapper) is
   begin
      if Self.Value = null then
         return;
      end if;

      --  If Self is non-null, decrement the reference count of the referenced
      --  value.

      declare
         Rec : Value_Record renames Self.Value.all;
      begin
         Rec.Ref_Count := Rec.Ref_Count - 1;

         if Rec.Ref_Count = 0 then
            --  Reference count dropped to 0: time to free the value and what
            --  is inside.

            case Rec.Kind is
               when Text_Type_Value =>
                  Free (Rec.Text_Type_Value);
               when Completion_Item_Array_Value =>
                  Free (Rec.Completion_Item_Array_Value);
               when Doc_Annotation_Array_Value =>
                  Free (Rec.Doc_Annotation_Array_Value);
               when Ada_Node_Array_Value =>
                  Free (Rec.Ada_Node_Array_Value);
               when Base_Formal_Param_Decl_Array_Value =>
                  Free (Rec.Base_Formal_Param_Decl_Array_Value);
               when Base_Type_Decl_Array_Value =>
                  Free (Rec.Base_Type_Decl_Array_Value);
               when Basic_Decl_Array_Value =>
                  Free (Rec.Basic_Decl_Array_Value);
               when Compilation_Unit_Array_Value =>
                  Free (Rec.Compilation_Unit_Array_Value);
               when Defining_Name_Array_Value =>
                  Free (Rec.Defining_Name_Array_Value);
               when Generic_Instantiation_Array_Value =>
                  Free (Rec.Generic_Instantiation_Array_Value);
               when Param_Spec_Array_Value =>
                  Free (Rec.Param_Spec_Array_Value);
               when Type_Decl_Array_Value =>
                  Free (Rec.Type_Decl_Array_Value);
               when Param_Actual_Array_Value =>
                  Free (Rec.Param_Actual_Array_Value);
               when Ref_Result_Array_Value =>
                  Free (Rec.Ref_Result_Array_Value);
               when Substitution_Array_Value =>
                  Free (Rec.Substitution_Array_Value);
               when Analysis_Unit_Array_Value =>
                  Free (Rec.Analysis_Unit_Array_Value);
               when Unbounded_Text_Type_Array_Value =>
                  Free (Rec.Unbounded_Text_Type_Array_Value);
               when others =>
                  null;
            end case;

            Free (Self.Value);
         end if;
      end;
   end Finalize;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Value_Type) return Value_Kind is
   begin
      return Self.Value.Value.Kind;
   end Kind;

   --------------
   -- Allocate --
   --------------

   function Allocate (Kind : Value_Kind) return Value_Type is
      Result : Any_Value_Type;
   begin
      Result.Value.Value           := new Value_Record (Kind);
      Result.Value.Value.Ref_Count := 1;
      return Result;
   end Allocate;

   -----------------------
   -- To_Internal_Value --
   -----------------------

   function To_Internal_Value
     (Value : Any_Value_Type) return Impl.Internal_Value
   is
   begin
      if Value = No_Value then
         return Impl.No_Internal_Value;
      end if;

      case Kind (Value) is
         when Boolean_Value =>
            return Impl.Create_Boolean (As_Boolean (Value));

         when Integer_Value =>
            return Impl.Create_Integer (As_Integer (Value));

         when Character_Value =>
            return Impl.Create_Character (As_Character (Value));

         when Node_Value =>
            return Impl.Create_Node (Unwrap_Entity (As_Node (Value)));

         when others =>
            --  For now we use this only to handle default values, so this
            --  should be unreachable.
            raise Program_Error;
      end case;
   end To_Internal_Value;

   -------------------------
   -- From_Internal_Value --
   -------------------------

   function From_Internal_Value
     (Value : Impl.Internal_Value) return Any_Value_Type
   is
   begin
      case Value.Kind is
         when None =>
            return No_Value;

         when Boolean_Value =>
            return Create_Boolean (Impl.As_Boolean (Value));

         when Integer_Value =>
            return Create_Integer (Impl.As_Integer (Value));

         when Character_Value =>
            return Create_Character (Impl.As_Character (Value));

         when Analysis_Unit_Kind_Value =>
            return Create_Analysis_Unit_Kind
                (Impl.As_Analysis_Unit_Kind (Value));
         when Lookup_Kind_Value =>
            return Create_Lookup_Kind (Impl.As_Lookup_Kind (Value));
         when Find_All_Mode_Value =>
            return Create_Find_All_Mode (Impl.As_Find_All_Mode (Value));
         when Ref_Result_Kind_Value =>
            return Create_Ref_Result_Kind (Impl.As_Ref_Result_Kind (Value));
         when Grammar_Rule_Value =>
            return Create_Grammar_Rule (Impl.As_Grammar_Rule (Value));

         when Node_Value =>
            declare
               N : constant Internal_Entity := Impl.As_Node (Value);
            begin
               return Create_Node (Wrap_Node (N.Node, N.Info));
            end;
      end case;
   end From_Internal_Value;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Value_Type) return Boolean is
   begin
      return Self.Value.Value.Boolean_Value;
   end As_Boolean;

   --------------------
   -- Create_Boolean --
   --------------------

   function Create_Boolean (Value : Boolean) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Boolean_Value) do
         Result.Value.Value.Boolean_Value := Value;
      end return;
   end Create_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Value_Type) return Integer is
   begin
      return Self.Value.Value.Integer_Value;
   end As_Integer;

   --------------------
   -- Create_Integer --
   --------------------

   function Create_Integer (Value : Integer) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Integer_Value) do
         Result.Value.Value.Integer_Value := Value;
      end return;
   end Create_Integer;

   --------------------
   -- As_Big_Integer --
   --------------------

   function As_Big_Integer (Self : Value_Type) return Big_Integer is
   begin
      return Result : Big_Integer do
         Result.Set (Self.Value.Value.Big_Integer_Value);
      end return;
   end As_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer (Value : Big_Integer) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Big_Integer_Value) do
         Result.Value.Value.Big_Integer_Value.Set (Value);
      end return;
   end Create_Big_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Value_Type) return Character_Type is
   begin
      return Self.Value.Value.Character_Value;
   end As_Character;

   ----------------------
   -- Create_Character --
   ----------------------

   function Create_Character (Value : Character_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Character_Value) do
         Result.Value.Value.Character_Value := Value;
      end return;
   end Create_Character;

   --------------
   -- As_Token --
   --------------

   function As_Token (Self : Value_Type) return Token_Reference is
   begin
      return Self.Value.Value.Token_Value;
   end As_Token;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token (Value : Token_Reference) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Token_Value) do
         Result.Value.Value.Token_Value := Value;
      end return;
   end Create_Token;

   -----------------------
   -- As_Unbounded_Text --
   -----------------------

   function As_Unbounded_Text (Self : Value_Type) return Unbounded_Text_Type is
   begin
      return Self.Value.Value.Unbounded_Text_Value;
   end As_Unbounded_Text;

   ---------------------------
   -- Create_Unbounded_Text --
   ---------------------------

   function Create_Unbounded_Text
     (Value : Unbounded_Text_Type) return Value_Type
   is
   begin
      return Result : constant Value_Type := Allocate (Unbounded_Text_Value) do
         Result.Value.Value.Unbounded_Text_Value := Value;
      end return;
   end Create_Unbounded_Text;

   ----------------------
   -- As_Analysis_Unit --
   ----------------------

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit is
   begin
      return Self.Value.Value.Analysis_Unit_Value;
   end As_Analysis_Unit;

   --------------------------
   -- Create_Analysis_Unit --
   --------------------------

   function Create_Analysis_Unit (Value : Analysis_Unit) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Analysis_Unit_Value) do
         Result.Value.Value.Analysis_Unit_Value := Value;
      end return;
   end Create_Analysis_Unit;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Value_Type) return Ada_Node is
   begin
      return Self.Value.Value.Node_Value;
   end As_Node;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node (Value : Ada_Node'Class) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Node_Value) do
         Result.Value.Value.Node_Value := Value.As_Ada_Node;
      end return;
   end Create_Node;

   function As_Analysis_Unit_Kind (Self : Value_Type) return Analysis_Unit_Kind
   is
   begin
      return Self.Value.Value.Analysis_Unit_Kind_Value;
   end As_Analysis_Unit_Kind;

   function Create_Analysis_Unit_Kind
     (Value : Analysis_Unit_Kind) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Analysis_Unit_Kind_Value) do
         Result.Value.Value.Analysis_Unit_Kind_Value := Value;
      end return;
   end Create_Analysis_Unit_Kind;
   function As_Lookup_Kind (Self : Value_Type) return Lookup_Kind is
   begin
      return Self.Value.Value.Lookup_Kind_Value;
   end As_Lookup_Kind;

   function Create_Lookup_Kind (Value : Lookup_Kind) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Lookup_Kind_Value) do
         Result.Value.Value.Lookup_Kind_Value := Value;
      end return;
   end Create_Lookup_Kind;
   function As_Find_All_Mode (Self : Value_Type) return Find_All_Mode is
   begin
      return Self.Value.Value.Find_All_Mode_Value;
   end As_Find_All_Mode;

   function Create_Find_All_Mode (Value : Find_All_Mode) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Find_All_Mode_Value) do
         Result.Value.Value.Find_All_Mode_Value := Value;
      end return;
   end Create_Find_All_Mode;
   function As_Ref_Result_Kind (Self : Value_Type) return Ref_Result_Kind is
   begin
      return Self.Value.Value.Ref_Result_Kind_Value;
   end As_Ref_Result_Kind;

   function Create_Ref_Result_Kind (Value : Ref_Result_Kind) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Ref_Result_Kind_Value) do
         Result.Value.Value.Ref_Result_Kind_Value := Value;
      end return;
   end Create_Ref_Result_Kind;
   function As_Grammar_Rule (Self : Value_Type) return Grammar_Rule is
   begin
      return Self.Value.Value.Grammar_Rule_Value;
   end As_Grammar_Rule;

   function Create_Grammar_Rule (Value : Grammar_Rule) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Grammar_Rule_Value) do
         Result.Value.Value.Grammar_Rule_Value := Value;
      end return;
   end Create_Grammar_Rule;

   function As_Text_Type (Self : Value_Type) return Text_Type is
   begin
      return Result : Text_Type (Self.Value.Value.Text_Type_Value'Range) do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Text_Type_Value.all (I);
         end loop;
      end return;

   end As_Text_Type;

   function Create_Text_Type (Value : Text_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Text_Type_Value) do
         Result.Value.Value.Text_Type_Value := new Text_Type (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Text_Type_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Text_Type;
   function As_Aspect (Self : Value_Type) return Aspect is
   begin
      return Self.Value.Value.Aspect_Value;
   end As_Aspect;

   function Create_Aspect (Value : Aspect) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Aspect_Value) do
         Result.Value.Value.Aspect_Value := Value;
      end return;
   end Create_Aspect;
   function As_Completion_Item (Self : Value_Type) return Completion_Item is
   begin
      return Self.Value.Value.Completion_Item_Value;
   end As_Completion_Item;

   function Create_Completion_Item (Value : Completion_Item) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Completion_Item_Value) do
         Result.Value.Value.Completion_Item_Value := Value;
      end return;
   end Create_Completion_Item;
   function As_Completion_Item_Array
     (Self : Value_Type) return Completion_Item_Array
   is
   begin
      return
        Result : Completion_Item_Array
          (Self.Value.Value.Completion_Item_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Completion_Item_Array_Value.all (I);
         end loop;
      end return;

   end As_Completion_Item_Array;

   function Create_Completion_Item_Array
     (Value : Completion_Item_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Completion_Item_Array_Value)
      do
         Result.Value.Value.Completion_Item_Array_Value :=
           new Completion_Item_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Completion_Item_Array_Value.all (I) :=
              Value (I);
         end loop;

      end return;
   end Create_Completion_Item_Array;
   function As_Discrete_Range (Self : Value_Type) return Discrete_Range is
   begin
      return Self.Value.Value.Discrete_Range_Value;
   end As_Discrete_Range;

   function Create_Discrete_Range (Value : Discrete_Range) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Discrete_Range_Value) do
         Result.Value.Value.Discrete_Range_Value := Value;
      end return;
   end Create_Discrete_Range;
   function As_Doc_Annotation (Self : Value_Type) return Doc_Annotation is
   begin
      return Self.Value.Value.Doc_Annotation_Value;
   end As_Doc_Annotation;

   function Create_Doc_Annotation (Value : Doc_Annotation) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Doc_Annotation_Value) do
         Result.Value.Value.Doc_Annotation_Value := Value;
      end return;
   end Create_Doc_Annotation;
   function As_Doc_Annotation_Array
     (Self : Value_Type) return Doc_Annotation_Array
   is
   begin
      return
        Result : Doc_Annotation_Array
          (Self.Value.Value.Doc_Annotation_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Doc_Annotation_Array_Value.all (I);
         end loop;
      end return;

   end As_Doc_Annotation_Array;

   function Create_Doc_Annotation_Array
     (Value : Doc_Annotation_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Doc_Annotation_Array_Value)
      do
         Result.Value.Value.Doc_Annotation_Array_Value :=
           new Doc_Annotation_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Doc_Annotation_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Doc_Annotation_Array;
   function As_Ada_Node_Array (Self : Value_Type) return Ada_Node_Array is
   begin
      return
        Result : Ada_Node_Array (Self.Value.Value.Ada_Node_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Ada_Node_Array_Value.all (I);
         end loop;
      end return;

   end As_Ada_Node_Array;

   function Create_Ada_Node_Array (Value : Ada_Node_Array) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Ada_Node_Array_Value) do
         Result.Value.Value.Ada_Node_Array_Value :=
           new Ada_Node_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Ada_Node_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Ada_Node_Array;
   function As_Base_Formal_Param_Decl_Array
     (Self : Value_Type) return Base_Formal_Param_Decl_Array
   is
   begin
      return
        Result : Base_Formal_Param_Decl_Array
          (Self.Value.Value.Base_Formal_Param_Decl_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) :=
              Self.Value.Value.Base_Formal_Param_Decl_Array_Value.all (I);
         end loop;
      end return;

   end As_Base_Formal_Param_Decl_Array;

   function Create_Base_Formal_Param_Decl_Array
     (Value : Base_Formal_Param_Decl_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type :=
          Allocate (Base_Formal_Param_Decl_Array_Value) do
         Result.Value.Value.Base_Formal_Param_Decl_Array_Value :=
           new Base_Formal_Param_Decl_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Base_Formal_Param_Decl_Array_Value.all (I) :=
              Value (I);
         end loop;

      end return;
   end Create_Base_Formal_Param_Decl_Array;
   function As_Base_Type_Decl_Array
     (Self : Value_Type) return Base_Type_Decl_Array
   is
   begin
      return
        Result : Base_Type_Decl_Array
          (Self.Value.Value.Base_Type_Decl_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Base_Type_Decl_Array_Value.all (I);
         end loop;
      end return;

   end As_Base_Type_Decl_Array;

   function Create_Base_Type_Decl_Array
     (Value : Base_Type_Decl_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Base_Type_Decl_Array_Value)
      do
         Result.Value.Value.Base_Type_Decl_Array_Value :=
           new Base_Type_Decl_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Base_Type_Decl_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Base_Type_Decl_Array;
   function As_Basic_Decl_Array (Self : Value_Type) return Basic_Decl_Array is
   begin
      return
        Result : Basic_Decl_Array
          (Self.Value.Value.Basic_Decl_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Basic_Decl_Array_Value.all (I);
         end loop;
      end return;

   end As_Basic_Decl_Array;

   function Create_Basic_Decl_Array
     (Value : Basic_Decl_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Basic_Decl_Array_Value) do
         Result.Value.Value.Basic_Decl_Array_Value :=
           new Basic_Decl_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Basic_Decl_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Basic_Decl_Array;
   function As_Compilation_Unit_Array
     (Self : Value_Type) return Compilation_Unit_Array
   is
   begin
      return
        Result : Compilation_Unit_Array
          (Self.Value.Value.Compilation_Unit_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) :=
              Self.Value.Value.Compilation_Unit_Array_Value.all (I);
         end loop;
      end return;

   end As_Compilation_Unit_Array;

   function Create_Compilation_Unit_Array
     (Value : Compilation_Unit_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Compilation_Unit_Array_Value)
      do
         Result.Value.Value.Compilation_Unit_Array_Value :=
           new Compilation_Unit_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Compilation_Unit_Array_Value.all (I) :=
              Value (I);
         end loop;

      end return;
   end Create_Compilation_Unit_Array;
   function As_Defining_Name_Array
     (Self : Value_Type) return Defining_Name_Array
   is
   begin
      return
        Result : Defining_Name_Array
          (Self.Value.Value.Defining_Name_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Defining_Name_Array_Value.all (I);
         end loop;
      end return;

   end As_Defining_Name_Array;

   function Create_Defining_Name_Array
     (Value : Defining_Name_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Defining_Name_Array_Value) do
         Result.Value.Value.Defining_Name_Array_Value :=
           new Defining_Name_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Defining_Name_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Defining_Name_Array;
   function As_Generic_Instantiation_Array
     (Self : Value_Type) return Generic_Instantiation_Array
   is
   begin
      return
        Result : Generic_Instantiation_Array
          (Self.Value.Value.Generic_Instantiation_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) :=
              Self.Value.Value.Generic_Instantiation_Array_Value.all (I);
         end loop;
      end return;

   end As_Generic_Instantiation_Array;

   function Create_Generic_Instantiation_Array
     (Value : Generic_Instantiation_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type :=
          Allocate (Generic_Instantiation_Array_Value) do
         Result.Value.Value.Generic_Instantiation_Array_Value :=
           new Generic_Instantiation_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Generic_Instantiation_Array_Value.all (I) :=
              Value (I);
         end loop;

      end return;
   end Create_Generic_Instantiation_Array;
   function As_Param_Spec_Array (Self : Value_Type) return Param_Spec_Array is
   begin
      return
        Result : Param_Spec_Array
          (Self.Value.Value.Param_Spec_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Param_Spec_Array_Value.all (I);
         end loop;
      end return;

   end As_Param_Spec_Array;

   function Create_Param_Spec_Array
     (Value : Param_Spec_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Param_Spec_Array_Value) do
         Result.Value.Value.Param_Spec_Array_Value :=
           new Param_Spec_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Param_Spec_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Param_Spec_Array;
   function As_Type_Decl_Array (Self : Value_Type) return Type_Decl_Array is
   begin
      return
        Result : Type_Decl_Array (Self.Value.Value.Type_Decl_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Type_Decl_Array_Value.all (I);
         end loop;
      end return;

   end As_Type_Decl_Array;

   function Create_Type_Decl_Array (Value : Type_Decl_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Type_Decl_Array_Value) do
         Result.Value.Value.Type_Decl_Array_Value :=
           new Type_Decl_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Type_Decl_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Type_Decl_Array;
   function As_Param_Actual (Self : Value_Type) return Param_Actual is
   begin
      return Self.Value.Value.Param_Actual_Value;
   end As_Param_Actual;

   function Create_Param_Actual (Value : Param_Actual) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Param_Actual_Value) do
         Result.Value.Value.Param_Actual_Value := Value;
      end return;
   end Create_Param_Actual;
   function As_Param_Actual_Array (Self : Value_Type) return Param_Actual_Array
   is
   begin
      return
        Result : Param_Actual_Array
          (Self.Value.Value.Param_Actual_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Param_Actual_Array_Value.all (I);
         end loop;
      end return;

   end As_Param_Actual_Array;

   function Create_Param_Actual_Array
     (Value : Param_Actual_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Param_Actual_Array_Value) do
         Result.Value.Value.Param_Actual_Array_Value :=
           new Param_Actual_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Param_Actual_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Param_Actual_Array;
   function As_Ref_Result (Self : Value_Type) return Ref_Result is
   begin
      return Self.Value.Value.Ref_Result_Value;
   end As_Ref_Result;

   function Create_Ref_Result (Value : Ref_Result) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Ref_Result_Value) do
         Result.Value.Value.Ref_Result_Value := Value;
      end return;
   end Create_Ref_Result;
   function As_Ref_Result_Array (Self : Value_Type) return Ref_Result_Array is
   begin
      return
        Result : Ref_Result_Array
          (Self.Value.Value.Ref_Result_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Ref_Result_Array_Value.all (I);
         end loop;
      end return;

   end As_Ref_Result_Array;

   function Create_Ref_Result_Array
     (Value : Ref_Result_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Ref_Result_Array_Value) do
         Result.Value.Value.Ref_Result_Array_Value :=
           new Ref_Result_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Ref_Result_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Ref_Result_Array;
   function As_Refd_Decl (Self : Value_Type) return Refd_Decl is
   begin
      return Self.Value.Value.Refd_Decl_Value;
   end As_Refd_Decl;

   function Create_Refd_Decl (Value : Refd_Decl) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Refd_Decl_Value) do
         Result.Value.Value.Refd_Decl_Value := Value;
      end return;
   end Create_Refd_Decl;
   function As_Refd_Def (Self : Value_Type) return Refd_Def is
   begin
      return Self.Value.Value.Refd_Def_Value;
   end As_Refd_Def;

   function Create_Refd_Def (Value : Refd_Def) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Refd_Def_Value) do
         Result.Value.Value.Refd_Def_Value := Value;
      end return;
   end Create_Refd_Def;
   function As_Substitution (Self : Value_Type) return Substitution is
   begin
      return Self.Value.Value.Substitution_Value;
   end As_Substitution;

   function Create_Substitution (Value : Substitution) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Substitution_Value) do
         Result.Value.Value.Substitution_Value := Value;
      end return;
   end Create_Substitution;
   function As_Substitution_Array (Self : Value_Type) return Substitution_Array
   is
   begin
      return
        Result : Substitution_Array
          (Self.Value.Value.Substitution_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Substitution_Array_Value.all (I);
         end loop;
      end return;

   end As_Substitution_Array;

   function Create_Substitution_Array
     (Value : Substitution_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Substitution_Array_Value) do
         Result.Value.Value.Substitution_Array_Value :=
           new Substitution_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Substitution_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Substitution_Array;
   function As_Analysis_Unit_Array
     (Self : Value_Type) return Analysis_Unit_Array
   is
   begin
      return
        Result : Analysis_Unit_Array
          (Self.Value.Value.Analysis_Unit_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) := Self.Value.Value.Analysis_Unit_Array_Value.all (I);
         end loop;
      end return;

   end As_Analysis_Unit_Array;

   function Create_Analysis_Unit_Array
     (Value : Analysis_Unit_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type := Allocate (Analysis_Unit_Array_Value) do
         Result.Value.Value.Analysis_Unit_Array_Value :=
           new Analysis_Unit_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Analysis_Unit_Array_Value.all (I) := Value (I);
         end loop;

      end return;
   end Create_Analysis_Unit_Array;
   function As_Unbounded_Text_Type_Array
     (Self : Value_Type) return Unbounded_Text_Type_Array
   is
   begin
      return
        Result : Unbounded_Text_Type_Array
          (Self.Value.Value.Unbounded_Text_Type_Array_Value'Range)
      do
         for I in Result'Range loop
            Result (I) :=
              Self.Value.Value.Unbounded_Text_Type_Array_Value.all (I);
         end loop;
      end return;

   end As_Unbounded_Text_Type_Array;

   function Create_Unbounded_Text_Type_Array
     (Value : Unbounded_Text_Type_Array) return Value_Type
   is
   begin
      return
        Result : constant Value_Type :=
          Allocate (Unbounded_Text_Type_Array_Value) do
         Result.Value.Value.Unbounded_Text_Type_Array_Value :=
           new Unbounded_Text_Type_Array (Value'Range);
         for I in Value'Range loop
            Result.Value.Value.Unbounded_Text_Type_Array_Value.all (I) :=
              Value (I);
         end loop;

      end return;
   end Create_Unbounded_Text_Type_Array;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return String is
   begin
      return Impl.DSL_Name (Id);
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id is
   begin
      return Impl.Lookup_DSL_Name (Name);
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Abstract (Id);
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type is
   begin
      return Impl.Kind_For (Id);
   end Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : Ada_Node_Kind_Type) return Node_Type_Id is
   begin
      return Impl.Id_For_Kind (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Root_Node (Id);
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      return Impl.Base_Type (Id);
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Impl.Derived_Types (Id);
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Derived_From (Id, Parent);
   end Is_Derived_From;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Constraint : Value_Constraint) return String is
   begin

      case Constraint.Kind is
         when Boolean_Value =>
            return "Bool";
         when Integer_Value =>
            return "Int";
         when Big_Integer_Value =>
            return "BigInt";
         when Character_Value =>
            return "Character";
         when Token_Value =>
            return "Token";
         when Unbounded_Text_Value =>
            return "Symbol";
         when Analysis_Unit_Value =>
            return "AnalysisUnit";
         when Analysis_Unit_Kind_Value =>
            return "AnalysisUnitKind";
         when Lookup_Kind_Value =>
            return "LookupKind";
         when Find_All_Mode_Value =>
            return "FindAllMode";
         when Ref_Result_Kind_Value =>
            return "RefResultKind";
         when Grammar_Rule_Value =>
            return "GrammarRule";
         when Text_Type_Value =>
            return "Character.array";
         when Aspect_Value =>
            return "Aspect";
         when Completion_Item_Value =>
            return "CompletionItem";
         when Completion_Item_Array_Value =>
            return "CompletionItem.array";
         when Discrete_Range_Value =>
            return "DiscreteRange";
         when Doc_Annotation_Value =>
            return "DocAnnotation";
         when Doc_Annotation_Array_Value =>
            return "DocAnnotation.array";
         when Ada_Node_Array_Value =>
            return "AdaNode.entity.array";
         when Base_Formal_Param_Decl_Array_Value =>
            return "BaseFormalParamDecl.entity.array";
         when Base_Type_Decl_Array_Value =>
            return "BaseTypeDecl.entity.array";
         when Basic_Decl_Array_Value =>
            return "BasicDecl.entity.array";
         when Compilation_Unit_Array_Value =>
            return "CompilationUnit.entity.array";
         when Defining_Name_Array_Value =>
            return "DefiningName.entity.array";
         when Generic_Instantiation_Array_Value =>
            return "GenericInstantiation.entity.array";
         when Param_Spec_Array_Value =>
            return "ParamSpec.entity.array";
         when Type_Decl_Array_Value =>
            return "TypeDecl.entity.array";
         when Param_Actual_Value =>
            return "ParamActual";
         when Param_Actual_Array_Value =>
            return "ParamActual.array";
         when Ref_Result_Value =>
            return "RefResult";
         when Ref_Result_Array_Value =>
            return "RefResult.array";
         when Refd_Decl_Value =>
            return "RefdDecl";
         when Refd_Def_Value =>
            return "RefdDef";
         when Substitution_Value =>
            return "Substitution";
         when Substitution_Array_Value =>
            return "Substitution.array";
         when Analysis_Unit_Array_Value =>
            return "AnalysisUnit.array";
         when Unbounded_Text_Type_Array_Value =>
            return "Symbol.array";

         when Node_Value =>
            return DSL_Name (Constraint.Node_Type);
      end case;
   end DSL_Name;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies
     (Value : Value_Type; Constraint : Value_Constraint) return Boolean
   is
   begin
      if Value.Value.Value.Kind /= Constraint.Kind then
         return False;
      end if;

      case Constraint.Kind is
         when Node_Value =>
            return

            --  A null node always satisfies the type constraint
            Value.Value.Value.Node_Value.Is_Null

            --  Else, check that the type of the node is derived from the type
            --  of the constraint.

              or else Is_Derived_From
                (Id_For_Kind (Value.Value.Value.Node_Value.Kind),
                 Constraint.Node_Type);

         when others =>
            return True;
      end case;
   end Satisfies;

   --------------------
   -- Node_Data_Name --
   --------------------

   function Node_Data_Name (Node_Data : Node_Data_Reference) return String is
   begin
      return Impl.Node_Data_Name (Node_Data);
   end Node_Data_Name;

   --------------------
   -- Node_Data_Type --
   --------------------

   function Node_Data_Type
     (Node_Data : Node_Data_Reference) return Value_Constraint
   is
   begin
      return Impl.Node_Data_Type (Node_Data);
   end Node_Data_Type;

   --------------------
   -- Eval_Node_Data --
   --------------------

   function Eval_Node_Data
     (Node      : Ada_Node'Class; Node_Data : Node_Data_Reference;
      Arguments : Value_Array) return Value_Type
   is
   begin
      case Node_Data is
         when Field_Reference =>
            if Arguments'Length > 0 then
               raise Node_Data_Evaluation_Error with "fields take no argument";
            end if;
            pragma Warnings (Off, "value not in range of type");
            return Create_Node (Eval_Field (Node, Node_Data));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Eval_Property (Node, Node_Data, Arguments);
      end case;
   end Eval_Node_Data;

   ----------------------
   -- Lookup_Node_Data --
   ----------------------

   function Lookup_Node_Data
     (Id : Node_Type_Id; Name : String) return Any_Node_Data_Reference
   is
   begin
      return Impl.Lookup_Node_Data (Id, Name);
   end Lookup_Node_Data;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Field : Field_Reference) return String is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Field_Name (Field);
      pragma Warnings (On, "value not in range of type");
   end Field_Name;

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type (Field : Field_Reference) return Node_Type_Id is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Field_Type (Field);
      pragma Warnings (On, "value not in range of type");
   end Field_Type;

   ----------------
   -- Eval_Field --
   ----------------

   function Eval_Field
     (Node : Ada_Node'Class; Field : Field_Reference) return Ada_Node
   is
      Ent : constant Internal_Entity := Unwrap_Entity (Node);

      pragma Warnings (Off, "value not in range of type");
      Result : constant Bare_Ada_Node := Impl.Eval_Field (Ent.Node, Field);
      pragma Warnings (On, "value not in range of type");
   begin
      return Wrap_Node (Result, Ent.Info);
   end Eval_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : Ada_Node_Kind_Type; Field : Field_Reference) return Positive
   is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Index (Kind, Field);
      pragma Warnings (On, "value not in range of type");
   end Index;

   --------------------------------
   -- Field_Reference_From_Index --
   --------------------------------

   function Field_Reference_From_Index
     (Kind : Ada_Node_Kind_Type; Index : Positive) return Field_Reference
   is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Field_Reference_From_Index (Kind, Index);
      pragma Warnings (On, "value not in range of type");
   end Field_Reference_From_Index;

   ------------
   -- Fields --
   ------------

   function Fields (Kind : Ada_Node_Kind_Type) return Field_Reference_Array is
   begin
      return Impl.Fields (Kind);
   end Fields;

   ------------
   -- Fields --
   ------------

   function Fields (Id : Node_Type_Id) return Field_Reference_Array is
   begin
      return Impl.Fields (Id);
   end Fields;

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return String is
   begin
      return Impl.Property_Name (Property);
   end Property_Name;

   --------------------------
   -- Property_Return_Type --
   --------------------------

   function Property_Return_Type
     (Property : Property_Reference) return Value_Constraint
   is
   begin
      return Impl.Property_Return_Type (Property);
   end Property_Return_Type;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Value_Constraint_Array
   is
   begin
      return Impl.Property_Argument_Types (Property);
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property : Property_Reference; Argument_Number : Positive) return String
   is
   begin
      return Impl.Property_Argument_Name (Property, Argument_Number);
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property : Property_Reference; Argument_Number : Positive)
      return Any_Value_Type
   is
      Desc : Impl.Property_Descriptor renames
        Impl.Property_Descriptors (Property).all;
   begin
      Impl.Check_Argument_Number (Desc, Argument_Number);
      return From_Internal_Value
          (Desc.Argument_Default_Values (Argument_Number));
   end Property_Argument_Default_Value;

   -------------------
   -- Eval_Property --
   -------------------

   function Eval_Property
     (Node      : Ada_Node'Class; Property : Property_Reference;
      Arguments : Value_Array) return Value_Type
   is
      Kind : constant Ada_Node_Kind_Type := Node.Kind;
      Desc : Impl.Property_Descriptor renames
        Impl.Property_Descriptors (Property).all;
      Result : Any_Value_Type := No_Value;
   begin
      --  First, check that arguments match the property signature

      if Arguments'Length /= Desc.Arity then
         raise Node_Data_Evaluation_Error with "invalid number of arguments";
      end if;

      for I in Desc.Argument_Types'Range loop
         declare
            Arg : Value_Type renames Arguments (I - 1 + Arguments'First);
         begin
            if not Satisfies (Arg, Desc.Argument_Types (I)) then
               raise Node_Data_Evaluation_Error
                 with "invalid type for argument " &
                 Desc.Argument_Names (I).all;
            end if;
         end;
      end loop;

      --  Now, we can proceed with the property evaluation

      case Property is
         when Ada_Node_P_Declarative_Scope =>
            Result := Create_Node (Node.P_Declarative_Scope);
         when Ada_Node_P_Complete =>
            Result := Create_Completion_Item_Array (Node.P_Complete);
         when Ada_Node_P_Valid_Keywords =>
            Result := Create_Unbounded_Text_Type_Array (Node.P_Valid_Keywords);
         when Ada_Node_P_Generic_Instantiations =>
            Result :=
              Create_Generic_Instantiation_Array
                (Node.P_Generic_Instantiations);
         when Ada_Node_P_Semantic_Parent =>
            Result := Create_Node (Node.P_Semantic_Parent);
         when Ada_Node_P_Parent_Basic_Decl =>
            Result := Create_Node (Node.P_Parent_Basic_Decl);
         when Ada_Node_P_Filter_Is_Imported_By =>
            declare
               Units : constant Analysis_Unit_Array :=
                 As_Analysis_Unit_Array (Arguments (Arguments'First + 0));
               Transitive : constant Boolean :=
                 As_Boolean (Arguments (Arguments'First + 1));
            begin
               Result :=
                 Create_Analysis_Unit_Array
                   (Node.P_Filter_Is_Imported_By (Units, Transitive));
            end;
         when Ada_Node_P_Xref_Entry_Point =>
            Result := Create_Boolean (Node.P_Xref_Entry_Point);
         when Ada_Node_P_Resolve_Names =>
            Result := Create_Boolean (Node.P_Resolve_Names);
         when Ada_Node_P_Standard_Unit =>
            Result := Create_Analysis_Unit (Node.P_Standard_Unit);
         when Ada_Node_P_Std_Entity =>
            declare
               Sym : constant Unbounded_Text_Type :=
                 As_Unbounded_Text (Arguments (Arguments'First + 0));
            begin
               Result := Create_Node (Node.P_Std_Entity (Sym));
            end;
         when Ada_Node_P_Bool_Type =>
            Result := Create_Node (Node.P_Bool_Type);
         when Ada_Node_P_Int_Type =>
            Result := Create_Node (Node.P_Int_Type);
         when Ada_Node_P_Universal_Int_Type =>
            Result := Create_Node (Node.P_Universal_Int_Type);
         when Ada_Node_P_Universal_Real_Type =>
            Result := Create_Node (Node.P_Universal_Real_Type);
         when Ada_Node_P_Top_Level_Decl =>
            declare
               Unit : constant Analysis_Unit :=
                 As_Analysis_Unit (Arguments (Arguments'First + 0));
            begin
               Result := Create_Node (Node.P_Top_Level_Decl (Unit));
            end;
         when Ada_Node_P_Choice_Match =>
            declare
               Value : constant Big_Integer :=
                 As_Big_Integer (Arguments (Arguments'First + 0));
            begin
               Result := Create_Boolean (Node.P_Choice_Match (Value));
            end;
         when Ada_Node_P_Gnat_Xref =>
            declare
               Imprecise_Fallback : constant Boolean :=
                 As_Boolean (Arguments (Arguments'First + 0));
            begin
               Result := Create_Node (Node.P_Gnat_Xref (Imprecise_Fallback));
            end;
         when Ada_Node_Parent =>
            Result := Create_Node (Node.Parent);
         when Ada_Node_Parents =>
            Result := Create_Ada_Node_Array (Node.Parents);
         when Ada_Node_Children =>
            Result := Create_Ada_Node_Array (Node.Children);
         when Ada_Node_Token_Start =>
            Result := Create_Token (Node.Token_Start);
         when Ada_Node_Token_End =>
            Result := Create_Token (Node.Token_End);
         when Ada_Node_Child_Index =>
            Result := Create_Integer (Node.Child_Index);
         when Ada_Node_Previous_Sibling =>
            Result := Create_Node (Node.Previous_Sibling);
         when Ada_Node_Next_Sibling =>
            Result := Create_Node (Node.Next_Sibling);
         when Ada_Node_Unit =>
            Result := Create_Analysis_Unit (Node.Unit);
         when Ada_Node_Is_Ghost =>
            Result := Create_Boolean (Node.Is_Ghost);
         when Ada_Node_Full_Sloc_Image =>
            Result := Create_Text_Type (Node.Full_Sloc_Image);
         when others =>
            null;
      end case;
      case Ada_Ada_Node (Kind) is
         when Ada_Assoc_List_Range =>
            declare
               N_Bare_Assoc_List : constant Analysis.Assoc_List :=
                 Node.As_Assoc_List;
            begin
               case Property is
                  when Assoc_List_P_Zip_With_Params =>
                     declare
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Param_Actual_Array
                            (N_Bare_Assoc_List.P_Zip_With_Params
                               (Imprecise_Fallback));
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Basic_Decl =>
            declare
               N_Bare_Basic_Decl : constant Analysis.Basic_Decl :=
                 Node.As_Basic_Decl;
            begin
               case Property is
                  when Basic_Decl_P_Is_Formal =>
                     Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Formal);
                  when Basic_Decl_P_Doc_Annotations =>
                     Result :=
                       Create_Doc_Annotation_Array
                         (N_Bare_Basic_Decl.P_Doc_Annotations);
                  when Basic_Decl_P_Doc =>
                     Result := Create_Text_Type (N_Bare_Basic_Decl.P_Doc);
                  when Basic_Decl_P_Previous_Part_For_Decl =>
                     Result :=
                       Create_Node
                         (N_Bare_Basic_Decl.P_Previous_Part_For_Decl);
                  when Basic_Decl_P_Canonical_Part =>
                     Result :=
                       Create_Node (N_Bare_Basic_Decl.P_Canonical_Part);
                  when Basic_Decl_P_Is_Static_Decl =>
                     declare
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Boolean
                            (N_Bare_Basic_Decl.P_Is_Static_Decl
                               (Imprecise_Fallback));
                     end;
                  when Basic_Decl_P_Is_Imported =>
                     Result :=
                       Create_Boolean (N_Bare_Basic_Decl.P_Is_Imported);
                  when Basic_Decl_P_Get_Aspect_Assoc =>
                     declare
                        Name : constant Unbounded_Text_Type :=
                          As_Unbounded_Text (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Node
                            (N_Bare_Basic_Decl.P_Get_Aspect_Assoc (Name));
                     end;
                  when Basic_Decl_P_Get_Aspect_Spec_Expr =>
                     declare
                        Name : constant Unbounded_Text_Type :=
                          As_Unbounded_Text (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Node
                            (N_Bare_Basic_Decl.P_Get_Aspect_Spec_Expr (Name));
                     end;
                  when Basic_Decl_P_Get_Aspect =>
                     declare
                        Name : constant Unbounded_Text_Type :=
                          As_Unbounded_Text (Arguments (Arguments'First + 0));
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 1));
                     begin
                        Result :=
                          Create_Aspect
                            (N_Bare_Basic_Decl.P_Get_Aspect
                               (Name, Imprecise_Fallback));
                     end;
                  when Basic_Decl_P_Has_Aspect =>
                     declare
                        Name : constant Unbounded_Text_Type :=
                          As_Unbounded_Text (Arguments (Arguments'First + 0));
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 1));
                     begin
                        Result :=
                          Create_Boolean
                            (N_Bare_Basic_Decl.P_Has_Aspect
                               (Name, Imprecise_Fallback));
                     end;
                  when Basic_Decl_P_Get_Pragma =>
                     declare
                        Name : constant Unbounded_Text_Type :=
                          As_Unbounded_Text (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Node (N_Bare_Basic_Decl.P_Get_Pragma (Name));
                     end;
                  when Basic_Decl_P_Get_Representation_Clause =>
                     declare
                        Name : constant Unbounded_Text_Type :=
                          As_Unbounded_Text (Arguments (Arguments'First + 0));
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 1));
                     begin
                        Result :=
                          Create_Node
                            (N_Bare_Basic_Decl.P_Get_Representation_Clause
                               (Name, Imprecise_Fallback));
                     end;
                  when Basic_Decl_P_Is_Compilation_Unit_Root =>
                     Result :=
                       Create_Boolean
                         (N_Bare_Basic_Decl.P_Is_Compilation_Unit_Root);
                  when Basic_Decl_P_Is_Visible =>
                     declare
                        From_Node : constant Ada_Node :=
                          As_Node (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Boolean
                            (N_Bare_Basic_Decl.P_Is_Visible (From_Node));
                     end;
                  when Basic_Decl_P_Base_Subp_Declarations =>
                     Result :=
                       Create_Basic_Decl_Array
                         (N_Bare_Basic_Decl.P_Base_Subp_Declarations);
                  when Basic_Decl_P_Root_Subp_Declarations =>
                     declare
                        Origin : constant Ada_Node :=
                          As_Node (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Basic_Decl_Array
                            (N_Bare_Basic_Decl.P_Root_Subp_Declarations
                               (Origin));
                     end;
                  when Basic_Decl_P_Find_All_Overrides =>
                     declare
                        Units : constant Analysis_Unit_Array :=
                          As_Analysis_Unit_Array
                            (Arguments (Arguments'First + 0));
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 1));
                     begin
                        Result :=
                          Create_Basic_Decl_Array
                            (N_Bare_Basic_Decl.P_Find_All_Overrides
                               (Units, Imprecise_Fallback));
                     end;
                  when Basic_Decl_P_Defining_Names =>
                     Result :=
                       Create_Defining_Name_Array
                         (N_Bare_Basic_Decl.P_Defining_Names);
                  when Basic_Decl_P_Defining_Name =>
                     Result := Create_Node (N_Bare_Basic_Decl.P_Defining_Name);
                  when Basic_Decl_P_Type_Expression =>
                     Result :=
                       Create_Node (N_Bare_Basic_Decl.P_Type_Expression);
                  when Basic_Decl_P_Subp_Spec_Or_Null =>
                     declare
                        Follow_Generic : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Node
                            (N_Bare_Basic_Decl.P_Subp_Spec_Or_Null
                               (Follow_Generic));
                     end;
                  when Basic_Decl_P_Is_Subprogram =>
                     Result :=
                       Create_Boolean (N_Bare_Basic_Decl.P_Is_Subprogram);
                  when Basic_Decl_P_Relative_Name =>
                     Result := Create_Node (N_Bare_Basic_Decl.P_Relative_Name);
                  when Basic_Decl_P_Relative_Name_Text =>
                     Result :=
                       Create_Unbounded_Text
                         (N_Bare_Basic_Decl.P_Relative_Name_Text);
                  when Basic_Decl_P_Next_Part_For_Decl =>
                     Result :=
                       Create_Node (N_Bare_Basic_Decl.P_Next_Part_For_Decl);
                  when Basic_Decl_P_Body_Part_For_Decl =>
                     Result :=
                       Create_Node (N_Bare_Basic_Decl.P_Body_Part_For_Decl);
                  when Basic_Decl_P_Fully_Qualified_Name_Array =>
                     Result :=
                       Create_Unbounded_Text_Type_Array
                         (N_Bare_Basic_Decl.P_Fully_Qualified_Name_Array);
                  when Basic_Decl_P_Fully_Qualified_Name =>
                     Result :=
                       Create_Text_Type
                         (N_Bare_Basic_Decl.P_Fully_Qualified_Name);
                  when Basic_Decl_P_Canonical_Fully_Qualified_Name =>
                     Result :=
                       Create_Text_Type
                         (N_Bare_Basic_Decl.P_Canonical_Fully_Qualified_Name);
                  when Basic_Decl_P_Unique_Identifying_Name =>
                     Result :=
                       Create_Text_Type
                         (N_Bare_Basic_Decl.P_Unique_Identifying_Name);
                  when others =>
                     null;
               end case;
               case Ada_Basic_Decl (Kind) is
                  when Ada_Body_Node =>
                     declare
                        N_Bare_Body_Node : constant Analysis.Body_Node :=
                          N_Bare_Basic_Decl.As_Body_Node;
                     begin
                        case Property is
                           when Body_Node_P_Previous_Part =>
                              Result :=
                                Create_Node (N_Bare_Body_Node.P_Previous_Part);
                           when Body_Node_P_Decl_Part =>
                              Result :=
                                Create_Node (N_Bare_Body_Node.P_Decl_Part);
                           when Body_Node_P_Subunit_Root =>
                              Result :=
                                Create_Node (N_Bare_Body_Node.P_Subunit_Root);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Base_Formal_Param_Decl =>
                     declare
                        N_Bare_Base_Formal_Param_Decl : constant Analysis
                          .Base_Formal_Param_Decl :=
                          N_Bare_Basic_Decl.As_Base_Formal_Param_Decl;
                     begin
                        case Property is
                           when Base_Formal_Param_Decl_P_Formal_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Formal_Param_Decl
                                        .P_Formal_Type
                                        (Origin));
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Base_Type_Decl =>
                     declare
                        N_Bare_Base_Type_Decl : constant Analysis
                          .Base_Type_Decl :=
                          N_Bare_Basic_Decl.As_Base_Type_Decl;
                     begin
                        case Property is
                           when Base_Type_Decl_P_Base_Subtype =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Base_Subtype
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Private_Completion =>
                              Result :=
                                Create_Node
                                  (N_Bare_Base_Type_Decl.P_Private_Completion);
                           when Base_Type_Decl_P_Get_Record_Representation_Clause =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl
                                        .P_Get_Record_Representation_Clause
                                        (Imprecise_Fallback));
                              end;
                           when Base_Type_Decl_P_Get_Enum_Representation_Clause =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl
                                        .P_Get_Enum_Representation_Clause
                                        (Imprecise_Fallback));
                              end;
                           when Base_Type_Decl_P_Is_Record_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Record_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Array_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Array_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Find_Derived_Types =>
                              declare
                                 Root : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 1));
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 2));
                              begin
                                 Result :=
                                   Create_Type_Decl_Array
                                     (N_Bare_Base_Type_Decl
                                        .P_Find_Derived_Types
                                        (Root, Origin, Imprecise_Fallback));
                              end;
                           when Base_Type_Decl_P_Is_Real_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Real_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Float_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Float_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Fixed_Point =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Fixed_Point
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Enum_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Enum_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Access_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Access_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Char_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Char_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Discrete_Range =>
                              Result :=
                                Create_Discrete_Range
                                  (N_Bare_Base_Type_Decl.P_Discrete_Range);
                           when Base_Type_Decl_P_Is_Discrete_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Discrete_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Int_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Int_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Accessed_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Accessed_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Tagged_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Tagged_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Base_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Base_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Base_Types =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Base_Type_Decl_Array
                                     (N_Bare_Base_Type_Decl.P_Base_Types
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Find_All_Derived_Types =>
                              declare
                                 Units : constant Analysis_Unit_Array :=
                                   As_Analysis_Unit_Array
                                     (Arguments (Arguments'First + 0));
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 1));
                              begin
                                 Result :=
                                   Create_Type_Decl_Array
                                     (N_Bare_Base_Type_Decl
                                        .P_Find_All_Derived_Types
                                        (Units, Imprecise_Fallback));
                              end;
                           when Base_Type_Decl_P_Comp_Type =>
                              declare
                                 Is_Subscript : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 1));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Comp_Type
                                        (Is_Subscript, Origin));
                              end;
                           when Base_Type_Decl_P_Index_Type =>
                              declare
                                 Dim : constant Integer :=
                                   As_Integer
                                     (Arguments (Arguments'First + 0));
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 1));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Index_Type
                                        (Dim, Origin));
                              end;
                           when Base_Type_Decl_P_Is_Derived_Type =>
                              declare
                                 Other_Type : constant Base_Type_Decl :=
                                   As_Node (Arguments (Arguments'First + 0))
                                     .As_Base_Type_Decl;
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 1));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Derived_Type
                                        (Other_Type, Origin));
                              end;
                           when Base_Type_Decl_P_Is_Interface_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Is_Interface_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Matching_Type =>
                              declare
                                 Expected_Type : constant Base_Type_Decl :=
                                   As_Node (Arguments (Arguments'First + 0))
                                     .As_Base_Type_Decl;
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 1));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl.P_Matching_Type
                                        (Expected_Type, Origin));
                              end;
                           when Base_Type_Decl_P_Canonical_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Canonical_Type
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Previous_Part =>
                              declare
                                 Go_To_Incomplete : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Previous_Part
                                        (Go_To_Incomplete));
                              end;
                           when Base_Type_Decl_P_Next_Part =>
                              Result :=
                                Create_Node
                                  (N_Bare_Base_Type_Decl.P_Next_Part);
                           when Base_Type_Decl_P_Full_View =>
                              Result :=
                                Create_Node
                                  (N_Bare_Base_Type_Decl.P_Full_View);
                           when Base_Type_Decl_P_Is_Definite_Subtype =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Base_Type_Decl
                                        .P_Is_Definite_Subtype
                                        (Origin));
                              end;
                           when Base_Type_Decl_P_Is_Private =>
                              Result :=
                                Create_Boolean
                                  (N_Bare_Base_Type_Decl.P_Is_Private);
                           when Base_Type_Decl_P_Root_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Type_Decl.P_Root_Type
                                        (Origin));
                              end;
                           when others =>
                              null;
                        end case;
                        case Ada_Base_Type_Decl (Kind) is
                           when Ada_Type_Decl_Range =>
                              declare
                                 N_Bare_Type_Decl : constant Analysis
                                   .Type_Decl :=
                                   N_Bare_Base_Type_Decl.As_Type_Decl;
                              begin
                                 case Property is
                                    when Type_Decl_P_Get_Primitives =>
                                       declare
                                          Only_Inherited : constant Boolean :=
                                            As_Boolean
                                              (Arguments
                                                 (Arguments'First + 0));
                                       begin
                                          Result :=
                                            Create_Basic_Decl_Array
                                              (N_Bare_Type_Decl
                                                 .P_Get_Primitives
                                                 (Only_Inherited));
                                       end;
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Basic_Subp_Decl =>
                     declare
                        N_Bare_Basic_Subp_Decl : constant Analysis
                          .Basic_Subp_Decl :=
                          N_Bare_Basic_Decl.As_Basic_Subp_Decl;
                     begin
                        case Property is
                           when Basic_Subp_Decl_P_Subp_Decl_Spec =>
                              Result :=
                                Create_Node
                                  (N_Bare_Basic_Subp_Decl.P_Subp_Decl_Spec);
                           when Basic_Subp_Decl_P_Body_Part =>
                              Result :=
                                Create_Node
                                  (N_Bare_Basic_Subp_Decl.P_Body_Part);
                           when others =>
                              null;
                        end case;
                        case Ada_Basic_Subp_Decl (Kind) is
                           when Ada_Enum_Literal_Decl_Range =>
                              declare
                                 N_Bare_Enum_Literal_Decl : constant Analysis
                                   .Enum_Literal_Decl :=
                                   N_Bare_Basic_Subp_Decl.As_Enum_Literal_Decl;
                              begin
                                 case Property is
                                    when Enum_Literal_Decl_P_Enum_Type =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_Enum_Literal_Decl
                                              .P_Enum_Type);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Object_Decl_Range =>
                     declare
                        N_Bare_Object_Decl : constant Analysis.Object_Decl :=
                          N_Bare_Basic_Decl.As_Object_Decl;
                     begin
                        case Property is
                           when Object_Decl_P_Public_Part_Decl =>
                              Result :=
                                Create_Node
                                  (N_Bare_Object_Decl.P_Public_Part_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Base_Package_Decl =>
                     declare
                        N_Bare_Base_Package_Decl : constant Analysis
                          .Base_Package_Decl :=
                          N_Bare_Basic_Decl.As_Base_Package_Decl;
                     begin
                        case Property is
                           when Base_Package_Decl_P_Body_Part =>
                              Result :=
                                Create_Node
                                  (N_Bare_Base_Package_Decl.P_Body_Part);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Instantiation =>
                     declare
                        N_Bare_Generic_Instantiation : constant Analysis
                          .Generic_Instantiation :=
                          N_Bare_Basic_Decl.As_Generic_Instantiation;
                     begin
                        case Property is
                           when Generic_Instantiation_P_Designated_Generic_Decl =>
                              Result :=
                                Create_Node
                                  (N_Bare_Generic_Instantiation
                                     .P_Designated_Generic_Decl);
                           when others =>
                              null;
                        end case;
                        case Ada_Generic_Instantiation (Kind) is
                           when Ada_Generic_Subp_Instantiation_Range =>
                              declare
                                 N_Bare_Generic_Subp_Instantiation : constant Analysis
                                   .Generic_Subp_Instantiation :=
                                   N_Bare_Generic_Instantiation
                                     .As_Generic_Subp_Instantiation;
                              begin
                                 case Property is
                                    when Generic_Subp_Instantiation_P_Designated_Subp =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_Generic_Subp_Instantiation
                                              .P_Designated_Subp);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Package_Renaming_Decl_Range =>
                     declare
                        N_Bare_Package_Renaming_Decl : constant Analysis
                          .Package_Renaming_Decl :=
                          N_Bare_Basic_Decl.As_Package_Renaming_Decl;
                     begin
                        case Property is
                           when Package_Renaming_Decl_P_Renamed_Package =>
                              Result :=
                                Create_Node
                                  (N_Bare_Package_Renaming_Decl
                                     .P_Renamed_Package);
                           when Package_Renaming_Decl_P_Final_Renamed_Package =>
                              Result :=
                                Create_Node
                                  (N_Bare_Package_Renaming_Decl
                                     .P_Final_Renamed_Package);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Subp_Decl_Range =>
                     declare
                        N_Bare_Generic_Subp_Decl : constant Analysis
                          .Generic_Subp_Decl :=
                          N_Bare_Basic_Decl.As_Generic_Subp_Decl;
                     begin
                        case Property is
                           when Generic_Subp_Decl_P_Body_Part =>
                              Result :=
                                Create_Node
                                  (N_Bare_Generic_Subp_Decl.P_Body_Part);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Package_Decl_Range =>
                     declare
                        N_Bare_Generic_Package_Decl : constant Analysis
                          .Generic_Package_Decl :=
                          N_Bare_Basic_Decl.As_Generic_Package_Decl;
                     begin
                        case Property is
                           when Generic_Package_Decl_P_Body_Part =>
                              Result :=
                                Create_Node
                                  (N_Bare_Generic_Package_Decl.P_Body_Part);
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Base_Formal_Param_Holder =>
            declare
               N_Bare_Base_Formal_Param_Holder : constant Analysis
                 .Base_Formal_Param_Holder :=
                 Node.As_Base_Formal_Param_Holder;
            begin
               case Property is
                  when Base_Formal_Param_Holder_P_Abstract_Formal_Params =>
                     Result :=
                       Create_Base_Formal_Param_Decl_Array
                         (N_Bare_Base_Formal_Param_Holder
                            .P_Abstract_Formal_Params);
                  when Base_Formal_Param_Holder_P_Nb_Min_Params =>
                     Result :=
                       Create_Integer
                         (N_Bare_Base_Formal_Param_Holder.P_Nb_Min_Params);
                  when Base_Formal_Param_Holder_P_Nb_Max_Params =>
                     Result :=
                       Create_Integer
                         (N_Bare_Base_Formal_Param_Holder.P_Nb_Max_Params);
                  when Base_Formal_Param_Holder_P_Param_Types =>
                     declare
                        Origin : constant Ada_Node :=
                          As_Node (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Base_Type_Decl_Array
                            (N_Bare_Base_Formal_Param_Holder.P_Param_Types
                               (Origin));
                     end;
                  when others =>
                     null;
               end case;
               case Ada_Base_Formal_Param_Holder (Kind) is
                  when Ada_Base_Subp_Spec =>
                     declare
                        N_Bare_Base_Subp_Spec : constant Analysis
                          .Base_Subp_Spec :=
                          N_Bare_Base_Formal_Param_Holder.As_Base_Subp_Spec;
                     begin
                        case Property is
                           when Base_Subp_Spec_P_Returns =>
                              Result :=
                                Create_Node (N_Bare_Base_Subp_Spec.P_Returns);
                           when Base_Subp_Spec_P_Params =>
                              Result :=
                                Create_Param_Spec_Array
                                  (N_Bare_Base_Subp_Spec.P_Params);
                           when Base_Subp_Spec_P_Primitive_Subp_Types =>
                              Result :=
                                Create_Base_Type_Decl_Array
                                  (N_Bare_Base_Subp_Spec
                                     .P_Primitive_Subp_Types);
                           when Base_Subp_Spec_P_Primitive_Subp_First_Type =>
                              Result :=
                                Create_Node
                                  (N_Bare_Base_Subp_Spec
                                     .P_Primitive_Subp_First_Type);
                           when Base_Subp_Spec_P_Primitive_Subp_Tagged_Type =>
                              Result :=
                                Create_Node
                                  (N_Bare_Base_Subp_Spec
                                     .P_Primitive_Subp_Tagged_Type);
                           when Base_Subp_Spec_P_Return_Type =>
                              declare
                                 Origin : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Base_Subp_Spec.P_Return_Type
                                        (Origin));
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Tagged_Node =>
            declare
               N_Bare_Tagged_Node : constant Analysis.Tagged_Node :=
                 Node.As_Tagged_Node;
            begin
               case Property is
                  when Tagged_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Tagged_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Abstract_Node =>
            declare
               N_Bare_Abstract_Node : constant Analysis.Abstract_Node :=
                 Node.As_Abstract_Node;
            begin
               case Property is
                  when Abstract_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Abstract_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Limited_Node =>
            declare
               N_Bare_Limited_Node : constant Analysis.Limited_Node :=
                 Node.As_Limited_Node;
            begin
               case Property is
                  when Limited_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Limited_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Private_Node =>
            declare
               N_Bare_Private_Node : constant Analysis.Private_Node :=
                 Node.As_Private_Node;
            begin
               case Property is
                  when Private_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Private_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Aliased_Node =>
            declare
               N_Bare_Aliased_Node : constant Analysis.Aliased_Node :=
                 Node.As_Aliased_Node;
            begin
               case Property is
                  when Aliased_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Aliased_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Not_Null =>
            declare
               N_Bare_Not_Null : constant Analysis.Not_Null :=
                 Node.As_Not_Null;
            begin
               case Property is
                  when Not_Null_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Not_Null.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Constant_Node =>
            declare
               N_Bare_Constant_Node : constant Analysis.Constant_Node :=
                 Node.As_Constant_Node;
            begin
               case Property is
                  when Constant_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Constant_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_All_Node =>
            declare
               N_Bare_All_Node : constant Analysis.All_Node :=
                 Node.As_All_Node;
            begin
               case Property is
                  when All_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_All_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Abort_Node =>
            declare
               N_Bare_Abort_Node : constant Analysis.Abort_Node :=
                 Node.As_Abort_Node;
            begin
               case Property is
                  when Abort_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Abort_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Reverse_Node =>
            declare
               N_Bare_Reverse_Node : constant Analysis.Reverse_Node :=
                 Node.As_Reverse_Node;
            begin
               case Property is
                  when Reverse_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Reverse_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_With_Private =>
            declare
               N_Bare_With_Private : constant Analysis.With_Private :=
                 Node.As_With_Private;
            begin
               case Property is
                  when With_Private_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_With_Private.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Until_Node =>
            declare
               N_Bare_Until_Node : constant Analysis.Until_Node :=
                 Node.As_Until_Node;
            begin
               case Property is
                  when Until_Node_P_As_Bool =>
                     Result := Create_Boolean (N_Bare_Until_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Synchronized_Node =>
            declare
               N_Bare_Synchronized_Node : constant Analysis
                 .Synchronized_Node :=
                 Node.As_Synchronized_Node;
            begin
               case Property is
                  when Synchronized_Node_P_As_Bool =>
                     Result :=
                       Create_Boolean (N_Bare_Synchronized_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Protected_Node =>
            declare
               N_Bare_Protected_Node : constant Analysis.Protected_Node :=
                 Node.As_Protected_Node;
            begin
               case Property is
                  when Protected_Node_P_As_Bool =>
                     Result :=
                       Create_Boolean (N_Bare_Protected_Node.P_As_Bool);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Base_Assoc =>
            declare
               N_Bare_Base_Assoc : constant Analysis.Base_Assoc :=
                 Node.As_Base_Assoc;
            begin
               case Property is
                  when Base_Assoc_P_Assoc_Expr =>
                     Result := Create_Node (N_Bare_Base_Assoc.P_Assoc_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Basic_Assoc =>
            declare
               N_Bare_Basic_Assoc : constant Analysis.Basic_Assoc :=
                 Node.As_Basic_Assoc;
            begin
               case Property is
                  when Basic_Assoc_P_Get_Params =>
                     declare
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Defining_Name_Array
                            (N_Bare_Basic_Assoc.P_Get_Params
                               (Imprecise_Fallback));
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Type_Expr =>
            declare
               N_Bare_Type_Expr : constant Analysis.Type_Expr :=
                 Node.As_Type_Expr;
            begin
               case Property is
                  when Type_Expr_P_Type_Name =>
                     Result := Create_Node (N_Bare_Type_Expr.P_Type_Name);
                  when Type_Expr_P_Designated_Type_Decl =>
                     Result :=
                       Create_Node (N_Bare_Type_Expr.P_Designated_Type_Decl);
                  when Type_Expr_P_Designated_Type_Decl_From =>
                     declare
                        Origin_Node : constant Ada_Node :=
                          As_Node (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Node
                            (N_Bare_Type_Expr.P_Designated_Type_Decl_From
                               (Origin_Node));
                     end;
                  when others =>
                     null;
               end case;
               case Ada_Type_Expr (Kind) is
                  when Ada_Subtype_Indication_Range =>
                     declare
                        N_Bare_Subtype_Indication : constant Analysis
                          .Subtype_Indication :=
                          N_Bare_Type_Expr.As_Subtype_Indication;
                     begin
                        case Property is
                           when Subtype_Indication_P_Is_Static_Subtype =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Subtype_Indication
                                        .P_Is_Static_Subtype
                                        (Imprecise_Fallback));
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Pragma_Node_Range =>
            declare
               N_Bare_Pragma_Node : constant Analysis.Pragma_Node :=
                 Node.As_Pragma_Node;
            begin
               case Property is
                  when Pragma_Node_P_Associated_Decls =>
                     Result :=
                       Create_Basic_Decl_Array
                         (N_Bare_Pragma_Node.P_Associated_Decls);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Expr =>
            declare
               N_Bare_Expr : constant Analysis.Expr := Node.As_Expr;
            begin
               case Property is
                  when Expr_P_Expression_Type =>
                     Result := Create_Node (N_Bare_Expr.P_Expression_Type);
                  when Expr_P_Is_Static_Expr =>
                     declare
                        Imprecise_Fallback : constant Boolean :=
                          As_Boolean (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Boolean
                            (N_Bare_Expr.P_Is_Static_Expr
                               (Imprecise_Fallback));
                     end;
                  when Expr_P_First_Corresponding_Decl =>
                     Result :=
                       Create_Node (N_Bare_Expr.P_First_Corresponding_Decl);
                  when Expr_P_Eval_As_Int =>
                     Result := Create_Big_Integer (N_Bare_Expr.P_Eval_As_Int);
                  when Expr_P_Eval_As_Int_In_Env =>
                     declare
                        Env : constant Substitution_Array :=
                          As_Substitution_Array
                            (Arguments (Arguments'First + 0));
                     begin
                        Result :=
                          Create_Big_Integer
                            (N_Bare_Expr.P_Eval_As_Int_In_Env (Env));
                     end;
                  when Expr_P_Matching_Nodes =>
                     Result :=
                       Create_Ada_Node_Array (N_Bare_Expr.P_Matching_Nodes);
                  when others =>
                     null;
               end case;
               case Ada_Expr (Kind) is
                  when Ada_Name =>
                     declare
                        N_Bare_Name : constant Analysis.Name :=
                          N_Bare_Expr.As_Name;
                     begin
                        case Property is
                           when Name_P_Enclosing_Defining_Name =>
                              Result :=
                                Create_Node
                                  (N_Bare_Name.P_Enclosing_Defining_Name);
                           when Name_P_Is_Defining =>
                              Result :=
                                Create_Boolean (N_Bare_Name.P_Is_Defining);
                           when Name_P_Name_Is =>
                              declare
                                 Sym : constant Unbounded_Text_Type :=
                                   As_Unbounded_Text
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Name_Is (Sym));
                              end;
                           when Name_P_Is_Direct_Call =>
                              Result :=
                                Create_Boolean (N_Bare_Name.P_Is_Direct_Call);
                           when Name_P_Is_Access_Call =>
                              Result :=
                                Create_Boolean (N_Bare_Name.P_Is_Access_Call);
                           when Name_P_Is_Call =>
                              Result := Create_Boolean (N_Bare_Name.P_Is_Call);
                           when Name_P_Is_Dot_Call =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Is_Dot_Call
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Failsafe_Referenced_Def_Name =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Refd_Def
                                     (N_Bare_Name
                                        .P_Failsafe_Referenced_Def_Name
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Referenced_Defining_Name =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Name.P_Referenced_Defining_Name
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_All_Env_Elements =>
                              declare
                                 Seq : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                                 Seq_From : constant Ada_Node :=
                                   As_Node (Arguments (Arguments'First + 1));
                              begin
                                 Result :=
                                   Create_Ada_Node_Array
                                     (N_Bare_Name.P_All_Env_Elements
                                        (Seq, Seq_From));
                              end;
                           when Name_P_Called_Subp_Spec =>
                              Result :=
                                Create_Node (N_Bare_Name.P_Called_Subp_Spec);
                           when Name_P_Referenced_Decl =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Node
                                     (N_Bare_Name.P_Referenced_Decl
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Failsafe_Referenced_Decl =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Refd_Decl
                                     (N_Bare_Name.P_Failsafe_Referenced_Decl
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Referenced_Decl_Internal =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Refd_Decl
                                     (N_Bare_Name.P_Referenced_Decl_Internal
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Name_Designated_Type =>
                              Result :=
                                Create_Node
                                  (N_Bare_Name.P_Name_Designated_Type);
                           when Name_P_Is_Static_Subtype =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Is_Static_Subtype
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Name_Matches =>
                              declare
                                 N : constant Name :=
                                   As_Node (Arguments (Arguments'First + 0))
                                     .As_Name;
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Name_Matches (N));
                              end;
                           when Name_P_Relative_Name =>
                              Result :=
                                Create_Node (N_Bare_Name.P_Relative_Name);
                           when Name_P_Is_Operator_Name =>
                              Result :=
                                Create_Boolean
                                  (N_Bare_Name.P_Is_Operator_Name);
                           when Name_P_Is_Write_Reference =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Is_Write_Reference
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Is_Dispatching_Call =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Is_Dispatching_Call
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_Is_Static_Call =>
                              declare
                                 Imprecise_Fallback : constant Boolean :=
                                   As_Boolean
                                     (Arguments (Arguments'First + 0));
                              begin
                                 Result :=
                                   Create_Boolean
                                     (N_Bare_Name.P_Is_Static_Call
                                        (Imprecise_Fallback));
                              end;
                           when Name_P_As_Symbol_Array =>
                              Result :=
                                Create_Unbounded_Text_Type_Array
                                  (N_Bare_Name.P_As_Symbol_Array);
                           when others =>
                              null;
                        end case;
                        case Ada_Name (Kind) is
                           when Ada_Call_Expr_Range =>
                              declare
                                 N_Bare_Call_Expr : constant Analysis
                                   .Call_Expr :=
                                   N_Bare_Name.As_Call_Expr;
                              begin
                                 case Property is
                                    when Call_Expr_P_Is_Array_Slice =>
                                       Result :=
                                         Create_Boolean
                                           (N_Bare_Call_Expr.P_Is_Array_Slice);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Single_Tok_Node =>
                              declare
                                 N_Bare_Single_Tok_Node : constant Analysis
                                   .Single_Tok_Node :=
                                   N_Bare_Name.As_Single_Tok_Node;
                              begin
                                 case Property is
                                    when Single_Tok_Node_P_Canonical_Text =>
                                       Result :=
                                         Create_Unbounded_Text
                                           (N_Bare_Single_Tok_Node
                                              .P_Canonical_Text);
                                    when others =>
                                       null;
                                 end case;
                                 case Ada_Single_Tok_Node (Kind) is
                                    when Ada_String_Literal_Range =>
                                       declare
                                          N_Bare_String_Literal : constant Analysis
                                            .String_Literal :=
                                            N_Bare_Single_Tok_Node
                                              .As_String_Literal;
                                       begin
                                          case Property is
                                             when String_Literal_P_Denoted_Value =>
                                                Result :=
                                                  Create_Text_Type
                                                    (N_Bare_String_Literal
                                                       .P_Denoted_Value);
                                             when others =>
                                                null;
                                          end case;
                                       end;
                                    when Ada_Char_Literal_Range =>
                                       declare
                                          N_Bare_Char_Literal : constant Analysis
                                            .Char_Literal :=
                                            N_Bare_Single_Tok_Node
                                              .As_Char_Literal;
                                       begin
                                          case Property is
                                             when Char_Literal_P_Denoted_Value =>
                                                Result :=
                                                  Create_Character
                                                    (N_Bare_Char_Literal
                                                       .P_Denoted_Value);
                                             when others =>
                                                null;
                                          end case;
                                       end;
                                    when Ada_Int_Literal_Range =>
                                       declare
                                          N_Bare_Int_Literal : constant Analysis
                                            .Int_Literal :=
                                            N_Bare_Single_Tok_Node
                                              .As_Int_Literal;
                                       begin
                                          case Property is
                                             when Int_Literal_P_Denoted_Value =>
                                                Result :=
                                                  Create_Big_Integer
                                                    (N_Bare_Int_Literal
                                                       .P_Denoted_Value);
                                             when others =>
                                                null;
                                          end case;
                                       end;
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Defining_Name_Range =>
                              declare
                                 N_Bare_Defining_Name : constant Analysis
                                   .Defining_Name :=
                                   N_Bare_Name.As_Defining_Name;
                              begin
                                 case Property is
                                    when Defining_Name_P_Basic_Decl =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_Defining_Name.P_Basic_Decl);
                                    when Defining_Name_P_Find_Refs =>
                                       declare
                                          Root : constant Ada_Node :=
                                            As_Node
                                              (Arguments
                                                 (Arguments'First + 0));
                                          Origin : constant Ada_Node :=
                                            As_Node
                                              (Arguments
                                                 (Arguments'First + 1));
                                          Imprecise_Fallback : constant Boolean :=
                                            As_Boolean
                                              (Arguments
                                                 (Arguments'First + 2));
                                       begin
                                          Result :=
                                            Create_Ref_Result_Array
                                              (N_Bare_Defining_Name.P_Find_Refs
                                                 (Root, Origin,
                                                  Imprecise_Fallback));
                                       end;
                                    when Defining_Name_P_Find_All_References =>
                                       declare
                                          Units : constant Analysis_Unit_Array :=
                                            As_Analysis_Unit_Array
                                              (Arguments
                                                 (Arguments'First + 0));
                                          Imprecise_Fallback : constant Boolean :=
                                            As_Boolean
                                              (Arguments
                                                 (Arguments'First + 1));
                                       begin
                                          Result :=
                                            Create_Ref_Result_Array
                                              (N_Bare_Defining_Name
                                                 .P_Find_All_References
                                                 (Units, Imprecise_Fallback));
                                       end;
                                    when Defining_Name_P_Find_All_Calls =>
                                       declare
                                          Units : constant Analysis_Unit_Array :=
                                            As_Analysis_Unit_Array
                                              (Arguments
                                                 (Arguments'First + 0));
                                          Imprecise_Fallback : constant Boolean :=
                                            As_Boolean
                                              (Arguments
                                                 (Arguments'First + 1));
                                       begin
                                          Result :=
                                            Create_Ref_Result_Array
                                              (N_Bare_Defining_Name
                                                 .P_Find_All_Calls
                                                 (Units, Imprecise_Fallback));
                                       end;
                                    when Defining_Name_P_Next_Part =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_Defining_Name.P_Next_Part);
                                    when Defining_Name_P_Previous_Part =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_Defining_Name
                                              .P_Previous_Part);
                                    when Defining_Name_P_Canonical_Part =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_Defining_Name
                                              .P_Canonical_Part);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_End_Name_Range =>
                              declare
                                 N_Bare_End_Name : constant Analysis
                                   .End_Name :=
                                   N_Bare_Name.As_End_Name;
                              begin
                                 case Property is
                                    when End_Name_P_Basic_Decl =>
                                       Result :=
                                         Create_Node
                                           (N_Bare_End_Name.P_Basic_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Allocator_Range =>
                     declare
                        N_Bare_Allocator : constant Analysis.Allocator :=
                          N_Bare_Expr.As_Allocator;
                     begin
                        case Property is
                           when Allocator_P_Get_Allocated_Type =>
                              Result :=
                                Create_Node
                                  (N_Bare_Allocator.P_Get_Allocated_Type);
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Compilation_Unit_Range =>
            declare
               N_Bare_Compilation_Unit : constant Analysis.Compilation_Unit :=
                 Node.As_Compilation_Unit;
            begin
               case Property is
                  when Compilation_Unit_P_Syntactic_Fully_Qualified_Name =>
                     Result :=
                       Create_Unbounded_Text_Type_Array
                         (N_Bare_Compilation_Unit
                            .P_Syntactic_Fully_Qualified_Name);
                  when Compilation_Unit_P_Unit_Kind =>
                     Result :=
                       Create_Analysis_Unit_Kind
                         (N_Bare_Compilation_Unit.P_Unit_Kind);
                  when Compilation_Unit_P_Withed_Units =>
                     Result :=
                       Create_Compilation_Unit_Array
                         (N_Bare_Compilation_Unit.P_Withed_Units);
                  when Compilation_Unit_P_Imported_Units =>
                     Result :=
                       Create_Compilation_Unit_Array
                         (N_Bare_Compilation_Unit.P_Imported_Units);
                  when Compilation_Unit_P_Unit_Dependencies =>
                     Result :=
                       Create_Compilation_Unit_Array
                         (N_Bare_Compilation_Unit.P_Unit_Dependencies);
                  when Compilation_Unit_P_Decl =>
                     Result := Create_Node (N_Bare_Compilation_Unit.P_Decl);
                  when Compilation_Unit_P_Is_Preelaborable =>
                     Result :=
                       Create_Boolean
                         (N_Bare_Compilation_Unit.P_Is_Preelaborable);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Subunit_Range =>
            declare
               N_Bare_Subunit : constant Analysis.Subunit := Node.As_Subunit;
            begin
               case Property is
                  when Subunit_P_Body_Root =>
                     Result := Create_Node (N_Bare_Subunit.P_Body_Root);
                  when others =>
                     null;
               end case;
            end;
         when others =>
            null;
      end case;

      if Result = No_Value then
         raise Node_Data_Evaluation_Error with "no such field on this node";
      end if;
      return Result;
   end Eval_Property;

   ----------------
   -- Properties --
   ----------------

   function Properties
     (Kind : Ada_Node_Kind_Type) return Property_Reference_Array
   is
   begin
      return Impl.Properties (Kind);
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
   begin
      return Impl.Properties (Id);
   end Properties;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : Ada_Node_Kind_Type) return Token_Kind is
   begin
      return Impl.Token_Node_Kind (Kind);
   end Token_Node_Kind;

end Libadalang.Introspection;
