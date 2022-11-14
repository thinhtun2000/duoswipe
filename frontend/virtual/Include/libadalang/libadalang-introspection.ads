private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

--  This package provides primitives to inspect the structure of parse trees.
--  It answers questions such as: what is the index of a syntax field in a
--  node? or conversely, to what syntax field does a couple (index, node
--  kind) correspond?
--
--  For instance, the following code snippet prints the name of the first
--  syntax field in ``Node``:
--
--  .. code-block:: ada
--
--     declare
--        Field_Ref : constant Field_Reference :=
--           Field_Reference_From_Index (Node.Kind, 1);
--     begin
--        Ada.Text_IO.Put_Line (Field_Name (Field_Ref));
--     end;

package Libadalang.Introspection is

   use Support.Text;

   ----------------
   -- Node types --
   ----------------

   function DSL_Name (Id : Node_Type_Id) return String;
   --  Return the name corresponding to Id in the Langkit DSL

   function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id;
   --  Look for the node type for which Name is in the Lankgit DSL. Return it
   --  if found, otherwise return None.

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Return whether Id designates an abstract node

   function Is_Concrete (Id : Node_Type_Id) return Boolean is
     (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type;
   --  Return the node kind corresponding to Id. This raises a Constraint_Error
   --  if Id designates an abstract node.

   function Id_For_Kind (Kind : Ada_Node_Kind_Type) return Node_Type_Id;
   --  Return the node type corresponding to the given node Kind

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Return whether Id is a reference for the root node type

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  If Id is the root node type, raise a Constaint_Error. Otherwise, return
   --  a reference to Id's base type.

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Return type references for all direct derivations for Id

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Return whether the type that Id represents is derives (directly or
   --  indirectly) from the type that Parent represents.

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Any_Value_Type is private;
   --  Polymorphic value to contain Kind values. This type has by-reference
   --  semantics, so copying it is cheap.

   No_Value : constant Any_Value_Type;
   --  Special Any_Value_Type to mean: no reference to a value

   subtype Value_Type is Any_Value_Type with
        Dynamic_Predicate => Value_Type /= No_Value;

   function Kind (Self : Value_Type) return Value_Kind;
   --  Return the kind of values that Value holds

   --  Accessors and constructors for inner value

   function As_Boolean (Self : Value_Type) return Boolean with
      Pre => Kind (Self) = Boolean_Value;
   function Create_Boolean (Value : Boolean) return Value_Type;

   function As_Integer (Self : Value_Type) return Integer with
      Pre => Kind (Self) = Integer_Value;
   function Create_Integer (Value : Integer) return Value_Type;

   function As_Big_Integer (Self : Value_Type) return Big_Integer with
      Pre => Kind (Self) = Big_Integer_Value;
   function Create_Big_Integer (Value : Big_Integer) return Value_Type;

   function As_Character (Self : Value_Type) return Character_Type with
      Pre => Kind (Self) = Character_Value;
   function Create_Character (Value : Character_Type) return Value_Type;

   function As_Token (Self : Value_Type) return Token_Reference with
      Pre => Kind (Self) = Token_Value;
   function Create_Token (Value : Token_Reference) return Value_Type;

   function As_Unbounded_Text
     (Self : Value_Type) return Unbounded_Text_Type with
      Pre => Kind (Self) = Unbounded_Text_Value;
   function Create_Unbounded_Text
     (Value : Unbounded_Text_Type) return Value_Type;

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit with
      Pre => Kind (Self) = Analysis_Unit_Value;
   function Create_Analysis_Unit (Value : Analysis_Unit) return Value_Type;

   function As_Node (Self : Value_Type) return Ada_Node with
      Pre => Kind (Self) = Node_Value;
   function Create_Node (Value : Ada_Node'Class) return Value_Type;

   function As_Analysis_Unit_Kind
     (Self : Value_Type) return Analysis_Unit_Kind with
      Pre => Kind (Self) = Analysis_Unit_Kind_Value;
   function Create_Analysis_Unit_Kind
     (Value : Analysis_Unit_Kind) return Value_Type;
   function As_Lookup_Kind (Self : Value_Type) return Lookup_Kind with
      Pre => Kind (Self) = Lookup_Kind_Value;
   function Create_Lookup_Kind (Value : Lookup_Kind) return Value_Type;
   function As_Find_All_Mode (Self : Value_Type) return Find_All_Mode with
      Pre => Kind (Self) = Find_All_Mode_Value;
   function Create_Find_All_Mode (Value : Find_All_Mode) return Value_Type;
   function As_Ref_Result_Kind (Self : Value_Type) return Ref_Result_Kind with
      Pre => Kind (Self) = Ref_Result_Kind_Value;
   function Create_Ref_Result_Kind (Value : Ref_Result_Kind) return Value_Type;
   function As_Grammar_Rule (Self : Value_Type) return Grammar_Rule with
      Pre => Kind (Self) = Grammar_Rule_Value;
   function Create_Grammar_Rule (Value : Grammar_Rule) return Value_Type;

   function As_Text_Type (Self : Value_Type) return Text_Type with
      Pre => Kind (Self) = Text_Type_Value;
   function Create_Text_Type (Value : Text_Type) return Value_Type;
   function As_Aspect (Self : Value_Type) return Aspect with
      Pre => Kind (Self) = Aspect_Value;
   function Create_Aspect (Value : Aspect) return Value_Type;
   function As_Completion_Item (Self : Value_Type) return Completion_Item with
      Pre => Kind (Self) = Completion_Item_Value;
   function Create_Completion_Item (Value : Completion_Item) return Value_Type;
   function As_Completion_Item_Array
     (Self : Value_Type) return Completion_Item_Array with
      Pre => Kind (Self) = Completion_Item_Array_Value;
   function Create_Completion_Item_Array
     (Value : Completion_Item_Array) return Value_Type;
   function As_Discrete_Range (Self : Value_Type) return Discrete_Range with
      Pre => Kind (Self) = Discrete_Range_Value;
   function Create_Discrete_Range (Value : Discrete_Range) return Value_Type;
   function As_Doc_Annotation (Self : Value_Type) return Doc_Annotation with
      Pre => Kind (Self) = Doc_Annotation_Value;
   function Create_Doc_Annotation (Value : Doc_Annotation) return Value_Type;
   function As_Doc_Annotation_Array
     (Self : Value_Type) return Doc_Annotation_Array with
      Pre => Kind (Self) = Doc_Annotation_Array_Value;
   function Create_Doc_Annotation_Array
     (Value : Doc_Annotation_Array) return Value_Type;
   function As_Ada_Node_Array (Self : Value_Type) return Ada_Node_Array with
      Pre => Kind (Self) = Ada_Node_Array_Value;
   function Create_Ada_Node_Array (Value : Ada_Node_Array) return Value_Type;
   function As_Base_Formal_Param_Decl_Array
     (Self : Value_Type) return Base_Formal_Param_Decl_Array with
      Pre => Kind (Self) = Base_Formal_Param_Decl_Array_Value;
   function Create_Base_Formal_Param_Decl_Array
     (Value : Base_Formal_Param_Decl_Array) return Value_Type;
   function As_Base_Type_Decl_Array
     (Self : Value_Type) return Base_Type_Decl_Array with
      Pre => Kind (Self) = Base_Type_Decl_Array_Value;
   function Create_Base_Type_Decl_Array
     (Value : Base_Type_Decl_Array) return Value_Type;
   function As_Basic_Decl_Array
     (Self : Value_Type) return Basic_Decl_Array with
      Pre => Kind (Self) = Basic_Decl_Array_Value;
   function Create_Basic_Decl_Array
     (Value : Basic_Decl_Array) return Value_Type;
   function As_Compilation_Unit_Array
     (Self : Value_Type) return Compilation_Unit_Array with
      Pre => Kind (Self) = Compilation_Unit_Array_Value;
   function Create_Compilation_Unit_Array
     (Value : Compilation_Unit_Array) return Value_Type;
   function As_Defining_Name_Array
     (Self : Value_Type) return Defining_Name_Array with
      Pre => Kind (Self) = Defining_Name_Array_Value;
   function Create_Defining_Name_Array
     (Value : Defining_Name_Array) return Value_Type;
   function As_Generic_Instantiation_Array
     (Self : Value_Type) return Generic_Instantiation_Array with
      Pre => Kind (Self) = Generic_Instantiation_Array_Value;
   function Create_Generic_Instantiation_Array
     (Value : Generic_Instantiation_Array) return Value_Type;
   function As_Param_Spec_Array
     (Self : Value_Type) return Param_Spec_Array with
      Pre => Kind (Self) = Param_Spec_Array_Value;
   function Create_Param_Spec_Array
     (Value : Param_Spec_Array) return Value_Type;
   function As_Type_Decl_Array (Self : Value_Type) return Type_Decl_Array with
      Pre => Kind (Self) = Type_Decl_Array_Value;
   function Create_Type_Decl_Array (Value : Type_Decl_Array) return Value_Type;
   function As_Param_Actual (Self : Value_Type) return Param_Actual with
      Pre => Kind (Self) = Param_Actual_Value;
   function Create_Param_Actual (Value : Param_Actual) return Value_Type;
   function As_Param_Actual_Array
     (Self : Value_Type) return Param_Actual_Array with
      Pre => Kind (Self) = Param_Actual_Array_Value;
   function Create_Param_Actual_Array
     (Value : Param_Actual_Array) return Value_Type;
   function As_Ref_Result (Self : Value_Type) return Ref_Result with
      Pre => Kind (Self) = Ref_Result_Value;
   function Create_Ref_Result (Value : Ref_Result) return Value_Type;
   function As_Ref_Result_Array
     (Self : Value_Type) return Ref_Result_Array with
      Pre => Kind (Self) = Ref_Result_Array_Value;
   function Create_Ref_Result_Array
     (Value : Ref_Result_Array) return Value_Type;
   function As_Refd_Decl (Self : Value_Type) return Refd_Decl with
      Pre => Kind (Self) = Refd_Decl_Value;
   function Create_Refd_Decl (Value : Refd_Decl) return Value_Type;
   function As_Refd_Def (Self : Value_Type) return Refd_Def with
      Pre => Kind (Self) = Refd_Def_Value;
   function Create_Refd_Def (Value : Refd_Def) return Value_Type;
   function As_Substitution (Self : Value_Type) return Substitution with
      Pre => Kind (Self) = Substitution_Value;
   function Create_Substitution (Value : Substitution) return Value_Type;
   function As_Substitution_Array
     (Self : Value_Type) return Substitution_Array with
      Pre => Kind (Self) = Substitution_Array_Value;
   function Create_Substitution_Array
     (Value : Substitution_Array) return Value_Type;
   function As_Analysis_Unit_Array
     (Self : Value_Type) return Analysis_Unit_Array with
      Pre => Kind (Self) = Analysis_Unit_Array_Value;
   function Create_Analysis_Unit_Array
     (Value : Analysis_Unit_Array) return Value_Type;
   function As_Unbounded_Text_Type_Array
     (Self : Value_Type) return Unbounded_Text_Type_Array with
      Pre => Kind (Self) = Unbounded_Text_Type_Array_Value;
   function Create_Unbounded_Text_Type_Array
     (Value : Unbounded_Text_Type_Array) return Value_Type;

   type Value_Array is array (Positive range <>) of Value_Type;
   type Any_Value_Array is array (Positive range <>) of Any_Value_Type;

   function DSL_Name (Constraint : Value_Constraint) return String;
   --  Return the name corresponding to Constraint in the Langkit DSL

   function Satisfies
     (Value : Value_Type; Constraint : Value_Constraint) return Boolean;
   --  Return whether the given Value satisfy the given Constraint

   ---------------
   -- Node data --
   ---------------

   function Node_Data_Name (Node_Data : Node_Data_Reference) return String;
   --  Return a lower-case name for Node_Data

   function Node_Data_Type
     (Node_Data : Node_Data_Reference) return Value_Constraint;
   --  Return the constraint associated with Node_Data's type (or its return
   --  type).

   function Eval_Node_Data
     (Node      : Ada_Node'Class; Node_Data : Node_Data_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate Node_Data on the given Node and the given arguments. If node
   --  data evaluation raises a Property_Error, forward it. Otherwise, return
   --  its result.
   --
   --  This raises a Node_Data_Evaluation_Error if Node has no such node data
   --  or if the provided arguments are invalid for it.

   function Lookup_Node_Data
     (Id : Node_Type_Id; Name : String) return Any_Node_Data_Reference;
   --  Look for the node data corresponding to the given Name (lower-case name)
   --  in the given node type reference (Id). Return it if found, otherwise
   --  return None.

   -------------------
   -- Syntax fields --
   -------------------

   function Field_Name (Field : Field_Reference) return String;
   --  Return a lower-case name for ``Field``

   function Field_Type (Field : Field_Reference) return Node_Type_Id;
   --  Return a reference to the node type that covers what Field can contain

   function Eval_Field
     (Node : Ada_Node'Class; Field : Field_Reference) return Ada_Node;
   --  Evaluate Field on the given Node. Return the corresponding Node.
   --
   --  This raises a Node_Data_Evaluation_Error if Node has no such field.

   function Index
     (Kind : Ada_Node_Kind_Type; Field : Field_Reference) return Positive;
   --  Return the index in nodes to access the given ``Field`` considering the
   --  given ``Kind`` of node.

   function Field_Reference_From_Index
     (Kind : Ada_Node_Kind_Type; Index : Positive) return Field_Reference;
   --  Return the field reference corresponding to the given ``Index`` in nodes
   --  of the given ``Kind``. Raise an ``Invalid_Field`` exception if there is
   --  no field corresponding to this index.

   function Fields (Kind : Ada_Node_Kind_Type) return Field_Reference_Array;
   --  Return the list of fields that nodes of the given ``Kind`` have. This
   --  returns an empty array for list nodes.

   function Fields (Id : Node_Type_Id) return Field_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

   ----------------
   -- Properties --
   ----------------

   function Property_Name (Property : Property_Reference) return String;
   --  Return a lower-case name for ``Property``

   function Property_Return_Type
     (Property : Property_Reference) return Value_Constraint;
   --  Return the type constraint for Property's return type

   function Property_Argument_Types
     (Property : Property_Reference) return Value_Constraint_Array with
      Post => Property_Argument_Types'Result'Length = 0
      or else Property_Argument_Types'Result'First = 1;
      --  Return the type constraints for Property's arguments

   function Property_Argument_Name
     (Property : Property_Reference; Argument_Number : Positive) return String;
   --  Return the lower-cased name for Property's argument whose index is
   --  Argument_Number. This raises a Property_Error if Property has no
   --  such argument.

   function Property_Argument_Default_Value
     (Property : Property_Reference; Argument_Number : Positive)
      return Any_Value_Type;
   --  If the argument corresponding to Argument_Number of the given Property
   --  has a default value, return it. Return No_Value otherwise. This raises
   --  a Property_Error if Property has no such argument.

   function Eval_Property
     (Node      : Ada_Node'Class; Property : Property_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate Property on the given Node and the given arguments. If the
   --  property raises a Property_Error, forward it, otherwise return its
   --  result.
   --
   --  This raises a Node_Data_Evaluation_Error if Node has no such property or
   --  if the provided arguments are invalid for this property.

   function Properties
     (Kind : Ada_Node_Kind_Type) return Property_Reference_Array;
   --  Return the list of properties that nodes of the given ``Kind`` have

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

   ------------
   -- Tokens --
   ------------

   function Token_Node_Kind (Kind : Ada_Node_Kind_Type) return Token_Kind with
      Pre => Is_Token_Node (Kind);
      --  Return the token kind corresponding to the given token node kind

private

   type Value_Record;
   type Value_Access is access all Value_Record;

   --  In order to avoid Any_Value_Type to be tagged (which makes all its
   --  primitives dispatching, which has awful consequences, such as making
   --  some code patterns illegal, or making GNAT slow, wrap the access in
   --  a dedicated controlled object and make Any_Value_Type contain this
   --  wrapper.

   type Value_Access_Wrapper is new Ada.Finalization.Controlled with record
      Value : Value_Access;
   end record;

   overriding procedure Adjust (Self : in out Value_Access_Wrapper);
   overriding procedure Finalize (Self : in out Value_Access_Wrapper);

   type Any_Value_Type is record
      Value : Value_Access_Wrapper;
   end record;

   No_Value : constant Any_Value_Type :=
     (Value => (Ada.Finalization.Controlled with Value => null));

   type Text_Type_Access is access all Text_Type;
   procedure Free is new Ada.Unchecked_Deallocation
     (Text_Type, Text_Type_Access);
   type Completion_Item_Array_Access is access all Completion_Item_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Completion_Item_Array, Completion_Item_Array_Access);
   type Doc_Annotation_Array_Access is access all Doc_Annotation_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Doc_Annotation_Array, Doc_Annotation_Array_Access);
   type Ada_Node_Array_Access is access all Ada_Node_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Ada_Node_Array, Ada_Node_Array_Access);
   type Base_Formal_Param_Decl_Array_Access is
     access all Base_Formal_Param_Decl_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Base_Formal_Param_Decl_Array, Base_Formal_Param_Decl_Array_Access);
   type Base_Type_Decl_Array_Access is access all Base_Type_Decl_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Base_Type_Decl_Array, Base_Type_Decl_Array_Access);
   type Basic_Decl_Array_Access is access all Basic_Decl_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Decl_Array, Basic_Decl_Array_Access);
   type Compilation_Unit_Array_Access is access all Compilation_Unit_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Compilation_Unit_Array, Compilation_Unit_Array_Access);
   type Defining_Name_Array_Access is access all Defining_Name_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Defining_Name_Array, Defining_Name_Array_Access);
   type Generic_Instantiation_Array_Access is
     access all Generic_Instantiation_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Generic_Instantiation_Array, Generic_Instantiation_Array_Access);
   type Param_Spec_Array_Access is access all Param_Spec_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Param_Spec_Array, Param_Spec_Array_Access);
   type Type_Decl_Array_Access is access all Type_Decl_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Type_Decl_Array, Type_Decl_Array_Access);
   type Param_Actual_Array_Access is access all Param_Actual_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Param_Actual_Array, Param_Actual_Array_Access);
   type Ref_Result_Array_Access is access all Ref_Result_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Ref_Result_Array, Ref_Result_Array_Access);
   type Substitution_Array_Access is access all Substitution_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Substitution_Array, Substitution_Array_Access);
   type Analysis_Unit_Array_Access is access all Analysis_Unit_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Array, Analysis_Unit_Array_Access);
   type Unbounded_Text_Type_Array_Access is
     access all Unbounded_Text_Type_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Unbounded_Text_Type_Array, Unbounded_Text_Type_Array_Access);

   type Value_Record (Kind : Value_Kind := Value_Kind'First) is limited record
      Ref_Count : Natural;

      case Kind is
         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Big_Integer_Value =>
            Big_Integer_Value : Big_Integer;

         when Character_Value =>
            Character_Value : Character_Type;

         when Token_Value =>
            Token_Value : Token_Reference;

         when Unbounded_Text_Value =>
            Unbounded_Text_Value : Unbounded_Text_Type;

         when Analysis_Unit_Value =>
            Analysis_Unit_Value : Analysis_Unit;

         when Node_Value =>
            Node_Value : Ada_Node;

         when Analysis_Unit_Kind_Value =>
            Analysis_Unit_Kind_Value : Analysis_Unit_Kind;
         when Lookup_Kind_Value =>
            Lookup_Kind_Value : Lookup_Kind;
         when Find_All_Mode_Value =>
            Find_All_Mode_Value : Find_All_Mode;
         when Ref_Result_Kind_Value =>
            Ref_Result_Kind_Value : Ref_Result_Kind;
         when Grammar_Rule_Value =>
            Grammar_Rule_Value : Grammar_Rule;

         when Text_Type_Value =>
            Text_Type_Value : Text_Type_Access;
         when Aspect_Value =>
            Aspect_Value : Aspect;
         when Completion_Item_Value =>
            Completion_Item_Value : Completion_Item;
         when Completion_Item_Array_Value =>
            Completion_Item_Array_Value : Completion_Item_Array_Access;
         when Discrete_Range_Value =>
            Discrete_Range_Value : Discrete_Range;
         when Doc_Annotation_Value =>
            Doc_Annotation_Value : Doc_Annotation;
         when Doc_Annotation_Array_Value =>
            Doc_Annotation_Array_Value : Doc_Annotation_Array_Access;
         when Ada_Node_Array_Value =>
            Ada_Node_Array_Value : Ada_Node_Array_Access;
         when Base_Formal_Param_Decl_Array_Value =>
            Base_Formal_Param_Decl_Array_Value : Base_Formal_Param_Decl_Array_Access;
         when Base_Type_Decl_Array_Value =>
            Base_Type_Decl_Array_Value : Base_Type_Decl_Array_Access;
         when Basic_Decl_Array_Value =>
            Basic_Decl_Array_Value : Basic_Decl_Array_Access;
         when Compilation_Unit_Array_Value =>
            Compilation_Unit_Array_Value : Compilation_Unit_Array_Access;
         when Defining_Name_Array_Value =>
            Defining_Name_Array_Value : Defining_Name_Array_Access;
         when Generic_Instantiation_Array_Value =>
            Generic_Instantiation_Array_Value : Generic_Instantiation_Array_Access;
         when Param_Spec_Array_Value =>
            Param_Spec_Array_Value : Param_Spec_Array_Access;
         when Type_Decl_Array_Value =>
            Type_Decl_Array_Value : Type_Decl_Array_Access;
         when Param_Actual_Value =>
            Param_Actual_Value : Param_Actual;
         when Param_Actual_Array_Value =>
            Param_Actual_Array_Value : Param_Actual_Array_Access;
         when Ref_Result_Value =>
            Ref_Result_Value : Ref_Result;
         when Ref_Result_Array_Value =>
            Ref_Result_Array_Value : Ref_Result_Array_Access;
         when Refd_Decl_Value =>
            Refd_Decl_Value : Refd_Decl;
         when Refd_Def_Value =>
            Refd_Def_Value : Refd_Def;
         when Substitution_Value =>
            Substitution_Value : Substitution;
         when Substitution_Array_Value =>
            Substitution_Array_Value : Substitution_Array_Access;
         when Analysis_Unit_Array_Value =>
            Analysis_Unit_Array_Value : Analysis_Unit_Array_Access;
         when Unbounded_Text_Type_Array_Value =>
            Unbounded_Text_Type_Array_Value : Unbounded_Text_Type_Array_Access;
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Value_Record, Value_Access);

end Libadalang.Introspection;
