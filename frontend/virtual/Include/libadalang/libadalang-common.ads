with GNATCOLL.GMP.Integers;
with GNATCOLL.Traces;

with Langkit_Support.Errors;
with Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handlers;

--  This package provides types and functions used in the whole Libadalang
--  package tree.

package Libadalang.Common is

   use Support.Slocs, Support.Text;

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LIBADALANG.MAIN_TRACE", GNATCOLL.Traces.From_Config);

   Default_Charset : constant String := "iso-8859-1";
   --  Default charset to use when creating analysis contexts

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;
   --  Shortcut for ``GNATCOLL.GMP.Integers.Big_Integer``

   -------------------------------------
   -- Symbols and token data handlers --
   -------------------------------------

   type Precomputed_Symbol_Index is
     (
Precomputed_Symbol, --  "&"

      Precomputed_Symbol_1, --  "&""+"

      Precomputed_Symbol_10, --  "="

      Precomputed_Symbol_11, --  ">"

      Precomputed_Symbol_12, --  ">="

      Precomputed_Symbol_2, --  "*"

      Precomputed_Symbol_20, --  <<>>

      Precomputed_Symbol_3, --  "**"

      Precomputed_Symbol_4, --  "+"

      Precomputed_Symbol_5, --  "-"

      Precomputed_Symbol_6, --  "/"

      Precomputed_Symbol_7, --  "/="

      Precomputed_Symbol_8, --  "<"

      Precomputed_Symbol_9, --  "<="

      Precomputed_Symbol_Abort, --  abort

      Precomputed_Symbol_Abs, --  "abs"

      Precomputed_Symbol_Abs_158, --  abs

      Precomputed_Symbol_Abstract, --  abstract

      Precomputed_Symbol_Accept, --  accept

      Precomputed_Symbol_Access, --  Access

      Precomputed_Symbol_Access_161, --  access

      Precomputed_Symbol_Ada, --  Ada

      Precomputed_Symbol_Address, --  Address

      Precomputed_Symbol_Address_Size, --  Address_Size

      Precomputed_Symbol_Aft, --  Aft

      Precomputed_Symbol_Aliased, --  aliased

      Precomputed_Symbol_Alignment, --  Alignment

      Precomputed_Symbol_All, --  all

      Precomputed_Symbol_And, --  "and"

      Precomputed_Symbol_And_164, --  and

      Precomputed_Symbol_Array, --  array

      Precomputed_Symbol_Assert, --  Assert

      Precomputed_Symbol_At, --  at

      Precomputed_Symbol_Aux_Dec, --  Aux_DEC

      Precomputed_Symbol_Base, --  Base

      Precomputed_Symbol_Begin, --  begin

      Precomputed_Symbol_Body, --  body

      Precomputed_Symbol_Boolean, --  Boolean

      Precomputed_Symbol_Callable, --  Callable

      Precomputed_Symbol_Case, --  case

      Precomputed_Symbol_Ceiling, --  Ceiling

      Precomputed_Symbol_Class, --  Class

      Precomputed_Symbol_Compile_Time_Error, --  Compile_Time_Error

      Precomputed_Symbol_Compile_Time_Warning, --  Compile_Time_Warning

      Precomputed_Symbol_Component_Size, --  Component_Size

      Precomputed_Symbol_Constant, --  constant

      Precomputed_Symbol_Constant_Indexing, --  Constant_Indexing

      Precomputed_Symbol_Convention, --  Convention

      Precomputed_Symbol_Cursor, --  Cursor

      Precomputed_Symbol_Declare, --  declare

      Precomputed_Symbol_Delay, --  delay

      Precomputed_Symbol_Delta, --  delta

      Precomputed_Symbol_Depends, --  Depends

      Precomputed_Symbol_Descriptor_Size, --  Descriptor_Size

      Precomputed_Symbol_Digits, --  Digits

      Precomputed_Symbol_Digits_174, --  digits

      Precomputed_Symbol_Do, --  do

      Precomputed_Symbol_Duration, --  Duration

      Precomputed_Symbol_Elaborate_Body, --  Elaborate_Body

      Precomputed_Symbol_Element, --  Element

      Precomputed_Symbol_Else, --  else

      Precomputed_Symbol_Elsif, --  elsif

      Precomputed_Symbol_End, --  end

      Precomputed_Symbol_Entry, --  entry

      Precomputed_Symbol_Epsilon, --  Epsilon

      Precomputed_Symbol_Exception, --  exception

      Precomputed_Symbol_Exception_Id, --  Exception_Id

      Precomputed_Symbol_Exception_Occurrence, --  Exception_Occurrence

      Precomputed_Symbol_Exceptions, --  Exceptions

      Precomputed_Symbol_Exit, --  exit

      Precomputed_Symbol_Export, --  Export

      Precomputed_Symbol_Finalization_Size, --  Finalization_Size

      Precomputed_Symbol_First, --  First

      Precomputed_Symbol_First_Bit, --  First_Bit

      Precomputed_Symbol_Floor, --  Floor

      Precomputed_Symbol_For, --  for

      Precomputed_Symbol_Fore, --  Fore

      Precomputed_Symbol_Function, --  function

      Precomputed_Symbol_Generic, --  generic

      Precomputed_Symbol_Global, --  Global

      Precomputed_Symbol_Goto, --  goto

      Precomputed_Symbol_Identity, --  Identity

      Precomputed_Symbol_If, --  if

      Precomputed_Symbol_Image, --  Image

      Precomputed_Symbol_Img, --  Img

      Precomputed_Symbol_Implicit_Dereference, --  Implicit_Dereference

      Precomputed_Symbol_Import, --  Import

      Precomputed_Symbol_Import_Function, --  Import_Function

      Precomputed_Symbol_Import_Procedure, --  Import_Procedure

      Precomputed_Symbol_In, --  in

      Precomputed_Symbol_Inline, --  Inline

      Precomputed_Symbol_Input, --  Input

      Precomputed_Symbol_Integer, --  Integer

      Precomputed_Symbol_Interface, --  Interface

      Precomputed_Symbol_Interface_188, --  interface

      Precomputed_Symbol_Is, --  is

      Precomputed_Symbol_Iterable, --  Iterable

      Precomputed_Symbol_Iterator_Element, --  Iterator_Element

      Precomputed_Symbol_Iterator_Interfaces, --  Iterator_Interfaces

      Precomputed_Symbol_Large, --  Large

      Precomputed_Symbol_Last, --  Last

      Precomputed_Symbol_Last_Bit, --  Last_Bit

      Precomputed_Symbol_Length, --  Length

      Precomputed_Symbol_Limited, --  limited

      Precomputed_Symbol_Loop, --  loop

      Precomputed_Symbol_Loop_Entry, --  Loop_Entry

      Precomputed_Symbol_Loop_Invariant, --  Loop_Invariant

      Precomputed_Symbol_Machine_Mantissa, --  Machine_Mantissa

      Precomputed_Symbol_Mantissa, --  Mantissa

      Precomputed_Symbol_Max, --  Max

      Precomputed_Symbol_Max_Size_In_Storage_Elements, --  Max_Size_In_Storage_Elements

      Precomputed_Symbol_Maximum_Alignment, --  Maximum_Alignment

      Precomputed_Symbol_Min, --  Min

      Precomputed_Symbol_Mod, --  "mod"

      Precomputed_Symbol_Mod_192, --  mod

      Precomputed_Symbol_Model, --  Model

      Precomputed_Symbol_Model_Epsilon, --  Model_Epsilon

      Precomputed_Symbol_Model_Mantissa, --  Model_Mantissa

      Precomputed_Symbol_Model_Of, --  Model_Of

      Precomputed_Symbol_Model_Post, --  Model_Post

      Precomputed_Symbol_Model_Pre, --  Model_Pre

      Precomputed_Symbol_Modulus, --  Modulus

      Precomputed_Symbol_New, --  new

      Precomputed_Symbol_Nextpart, --  __nextpart

      Precomputed_Symbol_Not, --  "not"

      Precomputed_Symbol_Not_194, --  not

      Precomputed_Symbol_Null, --  null

      Precomputed_Symbol_Object_Size, --  Object_Size

      Precomputed_Symbol_Of, --  of

      Precomputed_Symbol_Old, --  Old

      Precomputed_Symbol_Or, --  "or"

      Precomputed_Symbol_Or_197, --  or

      Precomputed_Symbol_Others, --  others

      Precomputed_Symbol_Out, --  out

      Precomputed_Symbol_Output, --  Output

      Precomputed_Symbol_Overriding, --  overriding

      Precomputed_Symbol_Pack, --  Pack

      Precomputed_Symbol_Package, --  package

      Precomputed_Symbol_Pos, --  Pos

      Precomputed_Symbol_Position, --  Position

      Precomputed_Symbol_Post, --  Post

      Precomputed_Symbol_Pragma, --  pragma

      Precomputed_Symbol_Pre, --  Pre

      Precomputed_Symbol_Pred, --  Pred

      Precomputed_Symbol_Predicate, --  Predicate

      Precomputed_Symbol_Preelaborate, --  Preelaborate

      Precomputed_Symbol_Private, --  private

      Precomputed_Symbol_Privatepart, --  __privatepart

      Precomputed_Symbol_Procedure, --  procedure

      Precomputed_Symbol_Protected, --  protected

      Precomputed_Symbol_Pure, --  Pure

      Precomputed_Symbol_Raise, --  raise

      Precomputed_Symbol_Range, --  Range

      Precomputed_Symbol_Range_207, --  range

      Precomputed_Symbol_Read, --  Read

      Precomputed_Symbol_Record, --  record

      Precomputed_Symbol_Rem, --  "rem"

      Precomputed_Symbol_Rem_209, --  rem

      Precomputed_Symbol_Remote_Call_Interface, --  Remote_Call_Interface

      Precomputed_Symbol_Remote_Types, --  Remote_Types

      Precomputed_Symbol_Renames, --  renames

      Precomputed_Symbol_Requeue, --  requeue

      Precomputed_Symbol_Result, --  Result

      Precomputed_Symbol_Return, --  return

      Precomputed_Symbol_Reverse, --  reverse

      Precomputed_Symbol_Root_Storage_Pool, --  Root_Storage_Pool

      Precomputed_Symbol_Root_Stream_Type, --  Root_Stream_Type

      Precomputed_Symbol_Round, --  Round

      Precomputed_Symbol_Rounding, --  Rounding

      Precomputed_Symbol_Safe_Large, --  Safe_Large

      Precomputed_Symbol_Safe_Small, --  Safe_Small

      Precomputed_Symbol_Select, --  select

      Precomputed_Symbol_Separate, --  separate

      Precomputed_Symbol_Shared_Passive, --  Shared_Passive

      Precomputed_Symbol_Size, --  Size

      Precomputed_Symbol_Small, --  Small

      Precomputed_Symbol_Some, --  some

      Precomputed_Symbol_Standard, --  Standard

      Precomputed_Symbol_Static_Predicate, --  Static_Predicate

      Precomputed_Symbol_Storage_Pool, --  Storage_Pool

      Precomputed_Symbol_Storage_Pools, --  Storage_Pools

      Precomputed_Symbol_Streams, --  Streams

      Precomputed_Symbol_String, --  String

      Precomputed_Symbol_Style_Checks, --  Style_Checks

      Precomputed_Symbol_Subtype, --  subtype

      Precomputed_Symbol_Succ, --  Succ

      Precomputed_Symbol_Synchronized, --  synchronized

      Precomputed_Symbol_System, --  System

      Precomputed_Symbol_System_Allocator_Alignment, --  System_Allocator_Alignment

      Precomputed_Symbol_Tag, --  Tag

      Precomputed_Symbol_Tagged, --  tagged

      Precomputed_Symbol_Tags, --  Tags

      Precomputed_Symbol_Target_Name, --  Target_Name

      Precomputed_Symbol_Task, --  task

      Precomputed_Symbol_Task_Id, --  Task_Id

      Precomputed_Symbol_Task_Identification, --  Task_Identification

      Precomputed_Symbol_Terminate, --  terminate

      Precomputed_Symbol_Then, --  then

      Precomputed_Symbol_Type, --  type

      Precomputed_Symbol_Type_Class, --  Type_Class

      Precomputed_Symbol_Unchecked_Access, --  Unchecked_Access

      Precomputed_Symbol_Universal_Int_Type, --  Universal_Int_Type_

      Precomputed_Symbol_Universal_Real_Type, --  Universal_Real_Type_

      Precomputed_Symbol_Unreferenced, --  Unreferenced

      Precomputed_Symbol_Unrestricted_Access, --  Unrestricted_Access

      Precomputed_Symbol_Until, --  until

      Precomputed_Symbol_Update, --  Update

      Precomputed_Symbol_Use, --  use

      Precomputed_Symbol_Vads_Size, --  VADS_Size

      Precomputed_Symbol_Val, --  Val

      Precomputed_Symbol_Valid, --  Valid

      Precomputed_Symbol_Value, --  Value

      Precomputed_Symbol_Value_Size, --  Value_Size

      Precomputed_Symbol_Variable_Indexing, --  Variable_Indexing

      Precomputed_Symbol_Volatile, --  Volatile

      Precomputed_Symbol_Warnings, --  Warnings

      Precomputed_Symbol_When, --  when

      Precomputed_Symbol_While, --  while

      Precomputed_Symbol_Wide_Image, --  Wide_Image

      Precomputed_Symbol_Wide_String, --  Wide_String

      Precomputed_Symbol_Wide_Value, --  Wide_Value

      Precomputed_Symbol_Wide_Wide_Image, --  Wide_Wide_Image

      Precomputed_Symbol_Wide_Wide_String, --  Wide_Wide_String

      Precomputed_Symbol_Wide_Wide_Value, --  Wide_Wide_Value

      Precomputed_Symbol_Width, --  Width

      Precomputed_Symbol_With, --  with

      Precomputed_Symbol_Word_Size, --  Word_Size

      Precomputed_Symbol_Write, --  Write

      Precomputed_Symbol_Xor, --  "xor"

      Precomputed_Symbol_Xor_229 --  xor
   );

   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type;

   package Symbols is new Langkit_Support.Symbols
     (Precomputed_Symbol_Index, Precomputed_Symbol);

   package Token_Data_Handlers is new Langkit_Support.Token_Data_Handlers
     (Precomputed_Symbol_Index, Precomputed_Symbol, Symbols);

   use Symbols, Token_Data_Handlers;

   -----------
   -- Nodes --
   -----------

   type Ada_Node_Kind_Type is
     (Ada_Abort_Absent, Ada_Abort_Present, Ada_Abstract_Absent,
      Ada_Abstract_Present, Ada_Ada_Node_List, Ada_Alternatives_List,
      Ada_Constraint_List, Ada_Decl_List, Ada_Stmt_List, Ada_Aspect_Assoc_List,
      Ada_Base_Assoc_List, Ada_Assoc_List, Ada_Case_Expr_Alternative_List,
      Ada_Case_Stmt_Alternative_List, Ada_Compilation_Unit_List,
      Ada_Contract_Case_Assoc_List, Ada_Defining_Name_List,
      Ada_Discriminant_Spec_List, Ada_Elsif_Expr_Part_List,
      Ada_Elsif_Stmt_Part_List, Ada_Enum_Literal_Decl_List,
      Ada_Expr_Alternatives_List, Ada_Discriminant_Choice_List, Ada_Name_List,
      Ada_Parent_List, Ada_Param_Spec_List, Ada_Pragma_Node_List,
      Ada_Select_When_Part_List, Ada_Unconstrained_Array_Index_List,
      Ada_Variant_List, Ada_Aliased_Absent, Ada_Aliased_Present,
      Ada_All_Absent, Ada_All_Present, Ada_Constrained_Array_Indices,
      Ada_Unconstrained_Array_Indices, Ada_Aspect_Assoc, Ada_At_Clause,
      Ada_Attribute_Def_Clause, Ada_Enum_Rep_Clause, Ada_Record_Rep_Clause,
      Ada_Aspect_Spec, Ada_Contract_Case_Assoc, Ada_Pragma_Argument_Assoc,
      Ada_Entry_Spec, Ada_Enum_Subp_Spec, Ada_Subp_Spec, Ada_Component_List,
      Ada_Known_Discriminant_Part, Ada_Unknown_Discriminant_Part,
      Ada_Entry_Completion_Formal_Params, Ada_Generic_Formal_Part,
      Ada_Null_Record_Def, Ada_Record_Def, Ada_Aggregate_Assoc,
      Ada_Multi_Dim_Array_Assoc, Ada_Discriminant_Assoc, Ada_Param_Assoc,
      Ada_Component_Decl, Ada_Discriminant_Spec, Ada_Generic_Formal_Obj_Decl,
      Ada_Generic_Formal_Package, Ada_Generic_Formal_Subp_Decl,
      Ada_Generic_Formal_Type_Decl, Ada_Param_Spec,
      Ada_Generic_Package_Internal, Ada_Package_Decl,
      Ada_Discrete_Base_Subtype_Decl, Ada_Subtype_Decl,
      Ada_Classwide_Type_Decl, Ada_Incomplete_Type_Decl,
      Ada_Incomplete_Tagged_Type_Decl, Ada_Protected_Type_Decl,
      Ada_Task_Type_Decl, Ada_Single_Task_Type_Decl, Ada_Type_Decl,
      Ada_Anonymous_Type_Decl, Ada_Synth_Anonymous_Type_Decl,
      Ada_Abstract_Subp_Decl, Ada_Abstract_Formal_Subp_Decl,
      Ada_Concrete_Formal_Subp_Decl, Ada_Subp_Decl, Ada_Entry_Decl,
      Ada_Enum_Literal_Decl, Ada_Generic_Subp_Internal, Ada_Expr_Function,
      Ada_Null_Subp_Decl, Ada_Subp_Body, Ada_Subp_Renaming_Decl,
      Ada_Package_Body_Stub, Ada_Protected_Body_Stub, Ada_Subp_Body_Stub,
      Ada_Task_Body_Stub, Ada_Entry_Body, Ada_Package_Body, Ada_Protected_Body,
      Ada_Task_Body, Ada_Entry_Index_Spec, Ada_Error_Decl, Ada_Exception_Decl,
      Ada_Exception_Handler, Ada_For_Loop_Var_Decl, Ada_Generic_Package_Decl,
      Ada_Generic_Subp_Decl, Ada_Generic_Package_Instantiation,
      Ada_Generic_Subp_Instantiation, Ada_Generic_Package_Renaming_Decl,
      Ada_Generic_Subp_Renaming_Decl, Ada_Label_Decl, Ada_Named_Stmt_Decl,
      Ada_Number_Decl, Ada_Object_Decl, Ada_Anonymous_Object_Decl,
      Ada_Extended_Return_Stmt_Object_Decl, Ada_Package_Renaming_Decl,
      Ada_Single_Protected_Decl, Ada_Single_Task_Decl,
      Ada_Case_Stmt_Alternative, Ada_Compilation_Unit, Ada_Component_Clause,
      Ada_Component_Def, Ada_Constant_Absent, Ada_Constant_Present,
      Ada_Delta_Constraint, Ada_Digits_Constraint, Ada_Discriminant_Constraint,
      Ada_Index_Constraint, Ada_Range_Constraint, Ada_Declarative_Part,
      Ada_Private_Part, Ada_Public_Part, Ada_Elsif_Expr_Part,
      Ada_Elsif_Stmt_Part, Ada_Allocator, Ada_Aggregate,
      Ada_Null_Record_Aggregate, Ada_Bin_Op, Ada_Relation_Op, Ada_Box_Expr,
      Ada_Case_Expr, Ada_Case_Expr_Alternative, Ada_Contract_Cases,
      Ada_If_Expr, Ada_Membership_Expr, Ada_Attribute_Ref,
      Ada_Update_Attribute_Ref, Ada_Call_Expr, Ada_Defining_Name,
      Ada_Discrete_Subtype_Name, Ada_Dotted_Name, Ada_End_Name,
      Ada_Explicit_Deref, Ada_Qual_Expr, Ada_Char_Literal, Ada_Identifier,
      Ada_Op_Abs, Ada_Op_And, Ada_Op_And_Then, Ada_Op_Concat, Ada_Op_Div,
      Ada_Op_Double_Dot, Ada_Op_Eq, Ada_Op_Gt, Ada_Op_Gte, Ada_Op_In,
      Ada_Op_Lt, Ada_Op_Lte, Ada_Op_Minus, Ada_Op_Mod, Ada_Op_Mult, Ada_Op_Neq,
      Ada_Op_Not, Ada_Op_Not_In, Ada_Op_Or, Ada_Op_Or_Else, Ada_Op_Plus,
      Ada_Op_Pow, Ada_Op_Rem, Ada_Op_Xor, Ada_String_Literal, Ada_Null_Literal,
      Ada_Int_Literal, Ada_Real_Literal, Ada_Target_Name, Ada_Paren_Expr,
      Ada_Quantified_Expr, Ada_Raise_Expr, Ada_Un_Op, Ada_Handled_Stmts,
      Ada_Interface_Kind_Limited, Ada_Interface_Kind_Protected,
      Ada_Interface_Kind_Synchronized, Ada_Interface_Kind_Task,
      Ada_Iter_Type_In, Ada_Iter_Type_Of, Ada_Library_Item, Ada_Limited_Absent,
      Ada_Limited_Present, Ada_For_Loop_Spec, Ada_While_Loop_Spec,
      Ada_Mode_Default, Ada_Mode_In, Ada_Mode_In_Out, Ada_Mode_Out,
      Ada_Not_Null_Absent, Ada_Not_Null_Present, Ada_Null_Component_Decl,
      Ada_Others_Designator, Ada_Overriding_Not_Overriding,
      Ada_Overriding_Overriding, Ada_Overriding_Unspecified, Ada_Params,
      Ada_Pragma_Node, Ada_Prim_Type_Accessor, Ada_Private_Absent,
      Ada_Private_Present, Ada_Protected_Def, Ada_Protected_Absent,
      Ada_Protected_Present, Ada_Quantifier_All, Ada_Quantifier_Some,
      Ada_Range_Spec, Ada_Renaming_Clause, Ada_Synthetic_Renaming_Clause,
      Ada_Reverse_Absent, Ada_Reverse_Present, Ada_Select_When_Part,
      Ada_Accept_Stmt, Ada_Accept_Stmt_With_Stmts, Ada_For_Loop_Stmt,
      Ada_Loop_Stmt, Ada_While_Loop_Stmt, Ada_Begin_Block, Ada_Decl_Block,
      Ada_Case_Stmt, Ada_Extended_Return_Stmt, Ada_If_Stmt, Ada_Named_Stmt,
      Ada_Select_Stmt, Ada_Error_Stmt, Ada_Abort_Stmt, Ada_Assign_Stmt,
      Ada_Call_Stmt, Ada_Delay_Stmt, Ada_Exit_Stmt, Ada_Goto_Stmt, Ada_Label,
      Ada_Null_Stmt, Ada_Raise_Stmt, Ada_Requeue_Stmt, Ada_Return_Stmt,
      Ada_Terminate_Alternative, Ada_Subp_Kind_Function,
      Ada_Subp_Kind_Procedure, Ada_Subunit, Ada_Synchronized_Absent,
      Ada_Synchronized_Present, Ada_Tagged_Absent, Ada_Tagged_Present,
      Ada_Task_Def, Ada_Access_To_Subp_Def, Ada_Anonymous_Type_Access_Def,
      Ada_Type_Access_Def, Ada_Array_Type_Def, Ada_Derived_Type_Def,
      Ada_Enum_Type_Def, Ada_Formal_Discrete_Type_Def, Ada_Interface_Type_Def,
      Ada_Mod_Int_Type_Def, Ada_Private_Type_Def, Ada_Decimal_Fixed_Point_Def,
      Ada_Floating_Point_Def, Ada_Ordinary_Fixed_Point_Def,
      Ada_Record_Type_Def, Ada_Signed_Int_Type_Def, Ada_Anonymous_Type,
      Ada_Enum_Lit_Synth_Type_Expr, Ada_Subtype_Indication,
      Ada_Constrained_Subtype_Indication, Ada_Discrete_Subtype_Indication,
      Ada_Unconstrained_Array_Index, Ada_Until_Absent, Ada_Until_Present,
      Ada_Use_Package_Clause, Ada_Use_Type_Clause, Ada_Variant,
      Ada_Variant_Part, Ada_With_Clause, Ada_With_Private_Absent,
      Ada_With_Private_Present);
   --  Type for concrete nodes

   for Ada_Node_Kind_Type use (Ada_Abort_Absent => 1, Ada_Abort_Present => 2,
      Ada_Abstract_Absent => 3, Ada_Abstract_Present => 4,
      Ada_Ada_Node_List => 5, Ada_Alternatives_List => 6,
      Ada_Constraint_List => 7, Ada_Decl_List => 8, Ada_Stmt_List => 9,
      Ada_Aspect_Assoc_List => 10, Ada_Base_Assoc_List => 11,
      Ada_Assoc_List => 12, Ada_Case_Expr_Alternative_List => 13,
      Ada_Case_Stmt_Alternative_List => 14, Ada_Compilation_Unit_List => 15,
      Ada_Contract_Case_Assoc_List => 16, Ada_Defining_Name_List => 17,
      Ada_Discriminant_Spec_List => 18, Ada_Elsif_Expr_Part_List => 19,
      Ada_Elsif_Stmt_Part_List => 20, Ada_Enum_Literal_Decl_List => 21,
      Ada_Expr_Alternatives_List => 22, Ada_Discriminant_Choice_List => 23,
      Ada_Name_List => 24, Ada_Parent_List => 25, Ada_Param_Spec_List => 26,
      Ada_Pragma_Node_List => 27, Ada_Select_When_Part_List => 28,
      Ada_Unconstrained_Array_Index_List        => 29, Ada_Variant_List => 30,
      Ada_Aliased_Absent => 31, Ada_Aliased_Present => 32,
      Ada_All_Absent                            => 33, Ada_All_Present => 34,
      Ada_Constrained_Array_Indices             => 35,
      Ada_Unconstrained_Array_Indices           => 36, Ada_Aspect_Assoc => 37,
      Ada_At_Clause => 38, Ada_Attribute_Def_Clause => 39,
      Ada_Enum_Rep_Clause => 40, Ada_Record_Rep_Clause => 41,
      Ada_Aspect_Spec => 42, Ada_Contract_Case_Assoc => 43,
      Ada_Pragma_Argument_Assoc                 => 44, Ada_Entry_Spec => 45,
      Ada_Enum_Subp_Spec => 46, Ada_Subp_Spec => 47, Ada_Component_List => 48,
      Ada_Known_Discriminant_Part => 49, Ada_Unknown_Discriminant_Part => 50,
      Ada_Entry_Completion_Formal_Params => 51, Ada_Generic_Formal_Part => 52,
      Ada_Null_Record_Def                       => 53, Ada_Record_Def => 54,
      Ada_Aggregate_Assoc => 55, Ada_Multi_Dim_Array_Assoc => 56,
      Ada_Discriminant_Assoc                    => 57, Ada_Param_Assoc => 58,
      Ada_Component_Decl => 59, Ada_Discriminant_Spec => 60,
      Ada_Generic_Formal_Obj_Decl => 61, Ada_Generic_Formal_Package => 62,
      Ada_Generic_Formal_Subp_Decl => 63, Ada_Generic_Formal_Type_Decl => 64,
      Ada_Param_Spec => 65, Ada_Generic_Package_Internal => 66,
      Ada_Package_Decl => 67, Ada_Discrete_Base_Subtype_Decl => 68,
      Ada_Subtype_Decl => 69, Ada_Classwide_Type_Decl => 70,
      Ada_Incomplete_Type_Decl => 71, Ada_Incomplete_Tagged_Type_Decl => 72,
      Ada_Protected_Type_Decl => 73, Ada_Task_Type_Decl => 74,
      Ada_Single_Task_Type_Decl                 => 75, Ada_Type_Decl => 76,
      Ada_Anonymous_Type_Decl => 77, Ada_Synth_Anonymous_Type_Decl => 78,
      Ada_Abstract_Subp_Decl => 79, Ada_Abstract_Formal_Subp_Decl => 80,
      Ada_Concrete_Formal_Subp_Decl             => 81, Ada_Subp_Decl => 82,
      Ada_Entry_Decl => 83, Ada_Enum_Literal_Decl => 84,
      Ada_Generic_Subp_Internal                 => 85, Ada_Expr_Function => 86,
      Ada_Null_Subp_Decl                        => 87, Ada_Subp_Body => 88,
      Ada_Subp_Renaming_Decl => 89, Ada_Package_Body_Stub => 90,
      Ada_Protected_Body_Stub => 91, Ada_Subp_Body_Stub => 92,
      Ada_Task_Body_Stub => 93, Ada_Entry_Body => 94, Ada_Package_Body => 95,
      Ada_Protected_Body                        => 96, Ada_Task_Body => 97,
      Ada_Entry_Index_Spec                      => 98, Ada_Error_Decl => 99,
      Ada_Exception_Decl => 100, Ada_Exception_Handler => 101,
      Ada_For_Loop_Var_Decl => 102, Ada_Generic_Package_Decl => 103,
      Ada_Generic_Subp_Decl => 104, Ada_Generic_Package_Instantiation => 105,
      Ada_Generic_Subp_Instantiation            => 106,
      Ada_Generic_Package_Renaming_Decl         => 107,
      Ada_Generic_Subp_Renaming_Decl            => 108, Ada_Label_Decl => 109,
      Ada_Named_Stmt_Decl                       => 110, Ada_Number_Decl => 111,
      Ada_Object_Decl => 112, Ada_Anonymous_Object_Decl => 113,
      Ada_Extended_Return_Stmt_Object_Decl      => 114,
      Ada_Package_Renaming_Decl => 115, Ada_Single_Protected_Decl => 116,
      Ada_Single_Task_Decl => 117, Ada_Case_Stmt_Alternative => 118,
      Ada_Compilation_Unit => 119, Ada_Component_Clause => 120,
      Ada_Component_Def => 121, Ada_Constant_Absent => 122,
      Ada_Constant_Present => 123, Ada_Delta_Constraint => 124,
      Ada_Digits_Constraint => 125, Ada_Discriminant_Constraint => 126,
      Ada_Index_Constraint => 127, Ada_Range_Constraint => 128,
      Ada_Declarative_Part => 129, Ada_Private_Part => 130,
      Ada_Public_Part => 131, Ada_Elsif_Expr_Part => 132,
      Ada_Elsif_Stmt_Part => 133, Ada_Allocator => 134, Ada_Aggregate => 135,
      Ada_Null_Record_Aggregate                 => 136, Ada_Bin_Op => 137,
      Ada_Relation_Op => 138, Ada_Box_Expr => 139, Ada_Case_Expr => 140,
      Ada_Case_Expr_Alternative => 141, Ada_Contract_Cases => 142,
      Ada_If_Expr => 143, Ada_Membership_Expr => 144, Ada_Attribute_Ref => 145,
      Ada_Update_Attribute_Ref                  => 146, Ada_Call_Expr => 147,
      Ada_Defining_Name => 148, Ada_Discrete_Subtype_Name => 149,
      Ada_Dotted_Name => 150, Ada_End_Name => 151, Ada_Explicit_Deref => 152,
      Ada_Qual_Expr => 153, Ada_Char_Literal => 154, Ada_Identifier => 155,
      Ada_Op_Abs => 156, Ada_Op_And => 157, Ada_Op_And_Then => 158,
      Ada_Op_Concat => 159, Ada_Op_Div => 160, Ada_Op_Double_Dot => 161,
      Ada_Op_Eq => 162, Ada_Op_Gt => 163, Ada_Op_Gte => 164, Ada_Op_In => 165,
      Ada_Op_Lt => 166, Ada_Op_Lte => 167, Ada_Op_Minus => 168,
      Ada_Op_Mod => 169, Ada_Op_Mult => 170, Ada_Op_Neq => 171,
      Ada_Op_Not => 172, Ada_Op_Not_In => 173, Ada_Op_Or => 174,
      Ada_Op_Or_Else => 175, Ada_Op_Plus => 176, Ada_Op_Pow => 177,
      Ada_Op_Rem => 178, Ada_Op_Xor => 179, Ada_String_Literal => 180,
      Ada_Null_Literal => 181, Ada_Int_Literal => 182, Ada_Real_Literal => 183,
      Ada_Target_Name                           => 184, Ada_Paren_Expr => 185,
      Ada_Quantified_Expr => 186, Ada_Raise_Expr => 187, Ada_Un_Op => 188,
      Ada_Handled_Stmts => 189, Ada_Interface_Kind_Limited => 190,
      Ada_Interface_Kind_Protected              => 191,
      Ada_Interface_Kind_Synchronized => 192, Ada_Interface_Kind_Task => 193,
      Ada_Iter_Type_In => 194, Ada_Iter_Type_Of => 195,
      Ada_Library_Item => 196, Ada_Limited_Absent => 197,
      Ada_Limited_Present => 198, Ada_For_Loop_Spec => 199,
      Ada_While_Loop_Spec => 200, Ada_Mode_Default => 201, Ada_Mode_In => 202,
      Ada_Mode_In_Out => 203, Ada_Mode_Out => 204, Ada_Not_Null_Absent => 205,
      Ada_Not_Null_Present => 206, Ada_Null_Component_Decl => 207,
      Ada_Others_Designator => 208, Ada_Overriding_Not_Overriding => 209,
      Ada_Overriding_Overriding => 210, Ada_Overriding_Unspecified => 211,
      Ada_Params => 212, Ada_Pragma_Node => 213, Ada_Prim_Type_Accessor => 214,
      Ada_Private_Absent => 215, Ada_Private_Present => 216,
      Ada_Protected_Def => 217, Ada_Protected_Absent => 218,
      Ada_Protected_Present => 219, Ada_Quantifier_All => 220,
      Ada_Quantifier_Some                       => 221, Ada_Range_Spec => 222,
      Ada_Renaming_Clause => 223, Ada_Synthetic_Renaming_Clause => 224,
      Ada_Reverse_Absent => 225, Ada_Reverse_Present => 226,
      Ada_Select_When_Part                      => 227, Ada_Accept_Stmt => 228,
      Ada_Accept_Stmt_With_Stmts => 229, Ada_For_Loop_Stmt => 230,
      Ada_Loop_Stmt => 231, Ada_While_Loop_Stmt => 232, Ada_Begin_Block => 233,
      Ada_Decl_Block                            => 234, Ada_Case_Stmt => 235,
      Ada_Extended_Return_Stmt                  => 236, Ada_If_Stmt => 237,
      Ada_Named_Stmt => 238, Ada_Select_Stmt => 239, Ada_Error_Stmt => 240,
      Ada_Abort_Stmt => 241, Ada_Assign_Stmt => 242, Ada_Call_Stmt => 243,
      Ada_Delay_Stmt => 244, Ada_Exit_Stmt => 245, Ada_Goto_Stmt => 246,
      Ada_Label => 247, Ada_Null_Stmt => 248, Ada_Raise_Stmt => 249,
      Ada_Requeue_Stmt                          => 250, Ada_Return_Stmt => 251,
      Ada_Terminate_Alternative => 252, Ada_Subp_Kind_Function => 253,
      Ada_Subp_Kind_Procedure                   => 254, Ada_Subunit => 255,
      Ada_Synchronized_Absent => 256, Ada_Synchronized_Present => 257,
      Ada_Tagged_Absent => 258, Ada_Tagged_Present => 259, Ada_Task_Def => 260,
      Ada_Access_To_Subp_Def => 261, Ada_Anonymous_Type_Access_Def => 262,
      Ada_Type_Access_Def => 263, Ada_Array_Type_Def => 264,
      Ada_Derived_Type_Def => 265, Ada_Enum_Type_Def => 266,
      Ada_Formal_Discrete_Type_Def => 267, Ada_Interface_Type_Def => 268,
      Ada_Mod_Int_Type_Def => 269, Ada_Private_Type_Def => 270,
      Ada_Decimal_Fixed_Point_Def => 271, Ada_Floating_Point_Def => 272,
      Ada_Ordinary_Fixed_Point_Def => 273, Ada_Record_Type_Def => 274,
      Ada_Signed_Int_Type_Def => 275, Ada_Anonymous_Type => 276,
      Ada_Enum_Lit_Synth_Type_Expr => 277, Ada_Subtype_Indication => 278,
      Ada_Constrained_Subtype_Indication        => 279,
      Ada_Discrete_Subtype_Indication           => 280,
      Ada_Unconstrained_Array_Index => 281, Ada_Until_Absent => 282,
      Ada_Until_Present => 283, Ada_Use_Package_Clause => 284,
      Ada_Use_Type_Clause => 285, Ada_Variant => 286, Ada_Variant_Part => 287,
      Ada_With_Clause => 288, Ada_With_Private_Absent => 289,
      Ada_With_Private_Present                  => 290);

   subtype Ada_Ada_Node is
     Ada_Node_Kind_Type range Ada_Abort_Absent .. Ada_With_Private_Present;
      --% no-document: True
   subtype Ada_Abort_Node is
     Ada_Node_Kind_Type range Ada_Abort_Absent .. Ada_Abort_Present;
      --% no-document: True
   subtype Ada_Abort_Absent_Range is
     Ada_Node_Kind_Type range Ada_Abort_Absent .. Ada_Abort_Absent;
      --% no-document: True
   subtype Ada_Abort_Present_Range is
     Ada_Node_Kind_Type range Ada_Abort_Present .. Ada_Abort_Present;
      --% no-document: True
   subtype Ada_Abstract_Node is
     Ada_Node_Kind_Type range Ada_Abstract_Absent .. Ada_Abstract_Present;
      --% no-document: True
   subtype Ada_Abstract_Absent_Range is
     Ada_Node_Kind_Type range Ada_Abstract_Absent .. Ada_Abstract_Absent;
      --% no-document: True
   subtype Ada_Abstract_Present_Range is
     Ada_Node_Kind_Type range Ada_Abstract_Present .. Ada_Abstract_Present;
      --% no-document: True
   subtype Ada_Ada_List is
     Ada_Node_Kind_Type range Ada_Ada_Node_List .. Ada_Variant_List;
      --% no-document: True
   subtype Ada_Ada_Node_List_Range is
     Ada_Node_Kind_Type range Ada_Ada_Node_List .. Ada_Stmt_List;
      --% no-document: True
   subtype Ada_Alternatives_List_Range is
     Ada_Node_Kind_Type range Ada_Alternatives_List .. Ada_Alternatives_List;
      --% no-document: True
   subtype Ada_Constraint_List_Range is
     Ada_Node_Kind_Type range Ada_Constraint_List .. Ada_Constraint_List;
      --% no-document: True
   subtype Ada_Decl_List_Range is
     Ada_Node_Kind_Type range Ada_Decl_List .. Ada_Decl_List;
      --% no-document: True
   subtype Ada_Stmt_List_Range is
     Ada_Node_Kind_Type range Ada_Stmt_List .. Ada_Stmt_List;
      --% no-document: True
   subtype Ada_Aspect_Assoc_List_Range is
     Ada_Node_Kind_Type range Ada_Aspect_Assoc_List .. Ada_Aspect_Assoc_List;
      --% no-document: True
   subtype Ada_Base_Assoc_List_Range is
     Ada_Node_Kind_Type range Ada_Base_Assoc_List .. Ada_Base_Assoc_List;
      --% no-document: True
   subtype Ada_Basic_Assoc_List is
     Ada_Node_Kind_Type range Ada_Assoc_List .. Ada_Assoc_List;
      --% no-document: True
   subtype Ada_Assoc_List_Range is
     Ada_Node_Kind_Type range Ada_Assoc_List .. Ada_Assoc_List;
      --% no-document: True
   subtype Ada_Case_Expr_Alternative_List_Range is
     Ada_Node_Kind_Type range Ada_Case_Expr_Alternative_List ..
         Ada_Case_Expr_Alternative_List;
      --% no-document: True
   subtype Ada_Case_Stmt_Alternative_List_Range is
     Ada_Node_Kind_Type range Ada_Case_Stmt_Alternative_List ..
         Ada_Case_Stmt_Alternative_List;
      --% no-document: True
   subtype Ada_Compilation_Unit_List_Range is
     Ada_Node_Kind_Type range Ada_Compilation_Unit_List ..
         Ada_Compilation_Unit_List;
      --% no-document: True
   subtype Ada_Contract_Case_Assoc_List_Range is
     Ada_Node_Kind_Type range Ada_Contract_Case_Assoc_List ..
         Ada_Contract_Case_Assoc_List;
      --% no-document: True
   subtype Ada_Defining_Name_List_Range is
     Ada_Node_Kind_Type range Ada_Defining_Name_List .. Ada_Defining_Name_List;
      --% no-document: True
   subtype Ada_Discriminant_Spec_List_Range is
     Ada_Node_Kind_Type range Ada_Discriminant_Spec_List ..
         Ada_Discriminant_Spec_List;
      --% no-document: True
   subtype Ada_Elsif_Expr_Part_List_Range is
     Ada_Node_Kind_Type range Ada_Elsif_Expr_Part_List ..
         Ada_Elsif_Expr_Part_List;
      --% no-document: True
   subtype Ada_Elsif_Stmt_Part_List_Range is
     Ada_Node_Kind_Type range Ada_Elsif_Stmt_Part_List ..
         Ada_Elsif_Stmt_Part_List;
      --% no-document: True
   subtype Ada_Enum_Literal_Decl_List_Range is
     Ada_Node_Kind_Type range Ada_Enum_Literal_Decl_List ..
         Ada_Enum_Literal_Decl_List;
      --% no-document: True
   subtype Ada_Expr_List is
     Ada_Node_Kind_Type range Ada_Expr_Alternatives_List ..
         Ada_Expr_Alternatives_List;
      --% no-document: True
   subtype Ada_Expr_Alternatives_List_Range is
     Ada_Node_Kind_Type range Ada_Expr_Alternatives_List ..
         Ada_Expr_Alternatives_List;
      --% no-document: True
   subtype Ada_Identifier_List is
     Ada_Node_Kind_Type range Ada_Discriminant_Choice_List ..
         Ada_Discriminant_Choice_List;
      --% no-document: True
   subtype Ada_Discriminant_Choice_List_Range is
     Ada_Node_Kind_Type range Ada_Discriminant_Choice_List ..
         Ada_Discriminant_Choice_List;
      --% no-document: True
   subtype Ada_Name_List_Range is
     Ada_Node_Kind_Type range Ada_Name_List .. Ada_Parent_List;
      --% no-document: True
   subtype Ada_Parent_List_Range is
     Ada_Node_Kind_Type range Ada_Parent_List .. Ada_Parent_List;
      --% no-document: True
   subtype Ada_Param_Spec_List_Range is
     Ada_Node_Kind_Type range Ada_Param_Spec_List .. Ada_Param_Spec_List;
      --% no-document: True
   subtype Ada_Pragma_Node_List_Range is
     Ada_Node_Kind_Type range Ada_Pragma_Node_List .. Ada_Pragma_Node_List;
      --% no-document: True
   subtype Ada_Select_When_Part_List_Range is
     Ada_Node_Kind_Type range Ada_Select_When_Part_List ..
         Ada_Select_When_Part_List;
      --% no-document: True
   subtype Ada_Unconstrained_Array_Index_List_Range is
     Ada_Node_Kind_Type range Ada_Unconstrained_Array_Index_List ..
         Ada_Unconstrained_Array_Index_List;
      --% no-document: True
   subtype Ada_Variant_List_Range is
     Ada_Node_Kind_Type range Ada_Variant_List .. Ada_Variant_List;
      --% no-document: True
   subtype Ada_Aliased_Node is
     Ada_Node_Kind_Type range Ada_Aliased_Absent .. Ada_Aliased_Present;
      --% no-document: True
   subtype Ada_Aliased_Absent_Range is
     Ada_Node_Kind_Type range Ada_Aliased_Absent .. Ada_Aliased_Absent;
      --% no-document: True
   subtype Ada_Aliased_Present_Range is
     Ada_Node_Kind_Type range Ada_Aliased_Present .. Ada_Aliased_Present;
      --% no-document: True
   subtype Ada_All_Node is
     Ada_Node_Kind_Type range Ada_All_Absent .. Ada_All_Present;
      --% no-document: True
   subtype Ada_All_Absent_Range is
     Ada_Node_Kind_Type range Ada_All_Absent .. Ada_All_Absent;
      --% no-document: True
   subtype Ada_All_Present_Range is
     Ada_Node_Kind_Type range Ada_All_Present .. Ada_All_Present;
      --% no-document: True
   subtype Ada_Array_Indices is
     Ada_Node_Kind_Type range Ada_Constrained_Array_Indices ..
         Ada_Unconstrained_Array_Indices;
      --% no-document: True
   subtype Ada_Constrained_Array_Indices_Range is
     Ada_Node_Kind_Type range Ada_Constrained_Array_Indices ..
         Ada_Constrained_Array_Indices;
      --% no-document: True
   subtype Ada_Unconstrained_Array_Indices_Range is
     Ada_Node_Kind_Type range Ada_Unconstrained_Array_Indices ..
         Ada_Unconstrained_Array_Indices;
      --% no-document: True
   subtype Ada_Aspect_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Aspect_Assoc .. Ada_Aspect_Assoc;
      --% no-document: True
   subtype Ada_Aspect_Clause is
     Ada_Node_Kind_Type range Ada_At_Clause .. Ada_Record_Rep_Clause;
      --% no-document: True
   subtype Ada_At_Clause_Range is
     Ada_Node_Kind_Type range Ada_At_Clause .. Ada_At_Clause;
      --% no-document: True
   subtype Ada_Attribute_Def_Clause_Range is
     Ada_Node_Kind_Type range Ada_Attribute_Def_Clause ..
         Ada_Attribute_Def_Clause;
      --% no-document: True
   subtype Ada_Enum_Rep_Clause_Range is
     Ada_Node_Kind_Type range Ada_Enum_Rep_Clause .. Ada_Enum_Rep_Clause;
      --% no-document: True
   subtype Ada_Record_Rep_Clause_Range is
     Ada_Node_Kind_Type range Ada_Record_Rep_Clause .. Ada_Record_Rep_Clause;
      --% no-document: True
   subtype Ada_Aspect_Spec_Range is
     Ada_Node_Kind_Type range Ada_Aspect_Spec .. Ada_Aspect_Spec;
      --% no-document: True
   subtype Ada_Base_Assoc is
     Ada_Node_Kind_Type range Ada_Contract_Case_Assoc ..
         Ada_Pragma_Argument_Assoc;
      --% no-document: True
   subtype Ada_Contract_Case_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Contract_Case_Assoc ..
         Ada_Contract_Case_Assoc;
      --% no-document: True
   subtype Ada_Pragma_Argument_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Pragma_Argument_Assoc ..
         Ada_Pragma_Argument_Assoc;
      --% no-document: True
   subtype Ada_Base_Formal_Param_Holder is
     Ada_Node_Kind_Type range Ada_Entry_Spec .. Ada_Generic_Formal_Part;
      --% no-document: True
   subtype Ada_Base_Subp_Spec is
     Ada_Node_Kind_Type range Ada_Entry_Spec .. Ada_Subp_Spec;
      --% no-document: True
   subtype Ada_Entry_Spec_Range is
     Ada_Node_Kind_Type range Ada_Entry_Spec .. Ada_Entry_Spec;
      --% no-document: True
   subtype Ada_Enum_Subp_Spec_Range is
     Ada_Node_Kind_Type range Ada_Enum_Subp_Spec .. Ada_Enum_Subp_Spec;
      --% no-document: True
   subtype Ada_Subp_Spec_Range is
     Ada_Node_Kind_Type range Ada_Subp_Spec .. Ada_Subp_Spec;
      --% no-document: True
   subtype Ada_Component_List_Range is
     Ada_Node_Kind_Type range Ada_Component_List .. Ada_Component_List;
      --% no-document: True
   subtype Ada_Discriminant_Part is
     Ada_Node_Kind_Type range Ada_Known_Discriminant_Part ..
         Ada_Unknown_Discriminant_Part;
      --% no-document: True
   subtype Ada_Known_Discriminant_Part_Range is
     Ada_Node_Kind_Type range Ada_Known_Discriminant_Part ..
         Ada_Known_Discriminant_Part;
      --% no-document: True
   subtype Ada_Unknown_Discriminant_Part_Range is
     Ada_Node_Kind_Type range Ada_Unknown_Discriminant_Part ..
         Ada_Unknown_Discriminant_Part;
      --% no-document: True
   subtype Ada_Entry_Completion_Formal_Params_Range is
     Ada_Node_Kind_Type range Ada_Entry_Completion_Formal_Params ..
         Ada_Entry_Completion_Formal_Params;
      --% no-document: True
   subtype Ada_Generic_Formal_Part_Range is
     Ada_Node_Kind_Type range Ada_Generic_Formal_Part ..
         Ada_Generic_Formal_Part;
      --% no-document: True
   subtype Ada_Base_Record_Def is
     Ada_Node_Kind_Type range Ada_Null_Record_Def .. Ada_Record_Def;
      --% no-document: True
   subtype Ada_Null_Record_Def_Range is
     Ada_Node_Kind_Type range Ada_Null_Record_Def .. Ada_Null_Record_Def;
      --% no-document: True
   subtype Ada_Record_Def_Range is
     Ada_Node_Kind_Type range Ada_Record_Def .. Ada_Record_Def;
      --% no-document: True
   subtype Ada_Basic_Assoc is
     Ada_Node_Kind_Type range Ada_Aggregate_Assoc .. Ada_Param_Assoc;
      --% no-document: True
   subtype Ada_Aggregate_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Aggregate_Assoc .. Ada_Multi_Dim_Array_Assoc;
      --% no-document: True
   subtype Ada_Multi_Dim_Array_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Multi_Dim_Array_Assoc ..
         Ada_Multi_Dim_Array_Assoc;
      --% no-document: True
   subtype Ada_Discriminant_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Discriminant_Assoc .. Ada_Discriminant_Assoc;
      --% no-document: True
   subtype Ada_Param_Assoc_Range is
     Ada_Node_Kind_Type range Ada_Param_Assoc .. Ada_Param_Assoc;
      --% no-document: True
   subtype Ada_Basic_Decl is
     Ada_Node_Kind_Type range Ada_Component_Decl .. Ada_Single_Task_Decl;
      --% no-document: True
   subtype Ada_Base_Formal_Param_Decl is
     Ada_Node_Kind_Type range Ada_Component_Decl .. Ada_Param_Spec;
      --% no-document: True
   subtype Ada_Component_Decl_Range is
     Ada_Node_Kind_Type range Ada_Component_Decl .. Ada_Component_Decl;
      --% no-document: True
   subtype Ada_Discriminant_Spec_Range is
     Ada_Node_Kind_Type range Ada_Discriminant_Spec .. Ada_Discriminant_Spec;
      --% no-document: True
   subtype Ada_Generic_Formal is
     Ada_Node_Kind_Type range Ada_Generic_Formal_Obj_Decl ..
         Ada_Generic_Formal_Type_Decl;
      --% no-document: True
   subtype Ada_Generic_Formal_Obj_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Formal_Obj_Decl ..
         Ada_Generic_Formal_Obj_Decl;
      --% no-document: True
   subtype Ada_Generic_Formal_Package_Range is
     Ada_Node_Kind_Type range Ada_Generic_Formal_Package ..
         Ada_Generic_Formal_Package;
      --% no-document: True
   subtype Ada_Generic_Formal_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Formal_Subp_Decl ..
         Ada_Generic_Formal_Subp_Decl;
      --% no-document: True
   subtype Ada_Generic_Formal_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Formal_Type_Decl ..
         Ada_Generic_Formal_Type_Decl;
      --% no-document: True
   subtype Ada_Param_Spec_Range is
     Ada_Node_Kind_Type range Ada_Param_Spec .. Ada_Param_Spec;
      --% no-document: True
   subtype Ada_Base_Package_Decl is
     Ada_Node_Kind_Type range Ada_Generic_Package_Internal .. Ada_Package_Decl;
      --% no-document: True
   subtype Ada_Generic_Package_Internal_Range is
     Ada_Node_Kind_Type range Ada_Generic_Package_Internal ..
         Ada_Generic_Package_Internal;
      --% no-document: True
   subtype Ada_Package_Decl_Range is
     Ada_Node_Kind_Type range Ada_Package_Decl .. Ada_Package_Decl;
      --% no-document: True
   subtype Ada_Base_Type_Decl is
     Ada_Node_Kind_Type range Ada_Discrete_Base_Subtype_Decl ..
         Ada_Synth_Anonymous_Type_Decl;
      --% no-document: True
   subtype Ada_Base_Subtype_Decl is
     Ada_Node_Kind_Type range Ada_Discrete_Base_Subtype_Decl ..
         Ada_Subtype_Decl;
      --% no-document: True
   subtype Ada_Discrete_Base_Subtype_Decl_Range is
     Ada_Node_Kind_Type range Ada_Discrete_Base_Subtype_Decl ..
         Ada_Discrete_Base_Subtype_Decl;
      --% no-document: True
   subtype Ada_Subtype_Decl_Range is
     Ada_Node_Kind_Type range Ada_Subtype_Decl .. Ada_Subtype_Decl;
      --% no-document: True
   subtype Ada_Classwide_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Classwide_Type_Decl ..
         Ada_Classwide_Type_Decl;
      --% no-document: True
   subtype Ada_Incomplete_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Incomplete_Type_Decl ..
         Ada_Incomplete_Tagged_Type_Decl;
      --% no-document: True
   subtype Ada_Incomplete_Tagged_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Incomplete_Tagged_Type_Decl ..
         Ada_Incomplete_Tagged_Type_Decl;
      --% no-document: True
   subtype Ada_Protected_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Protected_Type_Decl ..
         Ada_Protected_Type_Decl;
      --% no-document: True
   subtype Ada_Task_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Task_Type_Decl .. Ada_Single_Task_Type_Decl;
      --% no-document: True
   subtype Ada_Single_Task_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Single_Task_Type_Decl ..
         Ada_Single_Task_Type_Decl;
      --% no-document: True
   subtype Ada_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Type_Decl .. Ada_Synth_Anonymous_Type_Decl;
      --% no-document: True
   subtype Ada_Anonymous_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Anonymous_Type_Decl ..
         Ada_Synth_Anonymous_Type_Decl;
      --% no-document: True
   subtype Ada_Synth_Anonymous_Type_Decl_Range is
     Ada_Node_Kind_Type range Ada_Synth_Anonymous_Type_Decl ..
         Ada_Synth_Anonymous_Type_Decl;
      --% no-document: True
   subtype Ada_Basic_Subp_Decl is
     Ada_Node_Kind_Type range Ada_Abstract_Subp_Decl ..
         Ada_Generic_Subp_Internal;
      --% no-document: True
   subtype Ada_Classic_Subp_Decl is
     Ada_Node_Kind_Type range Ada_Abstract_Subp_Decl .. Ada_Subp_Decl;
      --% no-document: True
   subtype Ada_Abstract_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Abstract_Subp_Decl .. Ada_Abstract_Subp_Decl;
      --% no-document: True
   subtype Ada_Formal_Subp_Decl is
     Ada_Node_Kind_Type range Ada_Abstract_Formal_Subp_Decl ..
         Ada_Concrete_Formal_Subp_Decl;
      --% no-document: True
   subtype Ada_Abstract_Formal_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Abstract_Formal_Subp_Decl ..
         Ada_Abstract_Formal_Subp_Decl;
      --% no-document: True
   subtype Ada_Concrete_Formal_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Concrete_Formal_Subp_Decl ..
         Ada_Concrete_Formal_Subp_Decl;
      --% no-document: True
   subtype Ada_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Subp_Decl .. Ada_Subp_Decl;
      --% no-document: True
   subtype Ada_Entry_Decl_Range is
     Ada_Node_Kind_Type range Ada_Entry_Decl .. Ada_Entry_Decl;
      --% no-document: True
   subtype Ada_Enum_Literal_Decl_Range is
     Ada_Node_Kind_Type range Ada_Enum_Literal_Decl .. Ada_Enum_Literal_Decl;
      --% no-document: True
   subtype Ada_Generic_Subp_Internal_Range is
     Ada_Node_Kind_Type range Ada_Generic_Subp_Internal ..
         Ada_Generic_Subp_Internal;
      --% no-document: True
   subtype Ada_Body_Node is
     Ada_Node_Kind_Type range Ada_Expr_Function .. Ada_Task_Body;
      --% no-document: True
   subtype Ada_Base_Subp_Body is
     Ada_Node_Kind_Type range Ada_Expr_Function .. Ada_Subp_Renaming_Decl;
      --% no-document: True
   subtype Ada_Expr_Function_Range is
     Ada_Node_Kind_Type range Ada_Expr_Function .. Ada_Expr_Function;
      --% no-document: True
   subtype Ada_Null_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Null_Subp_Decl .. Ada_Null_Subp_Decl;
      --% no-document: True
   subtype Ada_Subp_Body_Range is
     Ada_Node_Kind_Type range Ada_Subp_Body .. Ada_Subp_Body;
      --% no-document: True
   subtype Ada_Subp_Renaming_Decl_Range is
     Ada_Node_Kind_Type range Ada_Subp_Renaming_Decl .. Ada_Subp_Renaming_Decl;
      --% no-document: True
   subtype Ada_Body_Stub is
     Ada_Node_Kind_Type range Ada_Package_Body_Stub .. Ada_Task_Body_Stub;
      --% no-document: True
   subtype Ada_Package_Body_Stub_Range is
     Ada_Node_Kind_Type range Ada_Package_Body_Stub .. Ada_Package_Body_Stub;
      --% no-document: True
   subtype Ada_Protected_Body_Stub_Range is
     Ada_Node_Kind_Type range Ada_Protected_Body_Stub ..
         Ada_Protected_Body_Stub;
      --% no-document: True
   subtype Ada_Subp_Body_Stub_Range is
     Ada_Node_Kind_Type range Ada_Subp_Body_Stub .. Ada_Subp_Body_Stub;
      --% no-document: True
   subtype Ada_Task_Body_Stub_Range is
     Ada_Node_Kind_Type range Ada_Task_Body_Stub .. Ada_Task_Body_Stub;
      --% no-document: True
   subtype Ada_Entry_Body_Range is
     Ada_Node_Kind_Type range Ada_Entry_Body .. Ada_Entry_Body;
      --% no-document: True
   subtype Ada_Package_Body_Range is
     Ada_Node_Kind_Type range Ada_Package_Body .. Ada_Package_Body;
      --% no-document: True
   subtype Ada_Protected_Body_Range is
     Ada_Node_Kind_Type range Ada_Protected_Body .. Ada_Protected_Body;
      --% no-document: True
   subtype Ada_Task_Body_Range is
     Ada_Node_Kind_Type range Ada_Task_Body .. Ada_Task_Body;
      --% no-document: True
   subtype Ada_Entry_Index_Spec_Range is
     Ada_Node_Kind_Type range Ada_Entry_Index_Spec .. Ada_Entry_Index_Spec;
      --% no-document: True
   subtype Ada_Error_Decl_Range is
     Ada_Node_Kind_Type range Ada_Error_Decl .. Ada_Error_Decl;
      --% no-document: True
   subtype Ada_Exception_Decl_Range is
     Ada_Node_Kind_Type range Ada_Exception_Decl .. Ada_Exception_Decl;
      --% no-document: True
   subtype Ada_Exception_Handler_Range is
     Ada_Node_Kind_Type range Ada_Exception_Handler .. Ada_Exception_Handler;
      --% no-document: True
   subtype Ada_For_Loop_Var_Decl_Range is
     Ada_Node_Kind_Type range Ada_For_Loop_Var_Decl .. Ada_For_Loop_Var_Decl;
      --% no-document: True
   subtype Ada_Generic_Decl is
     Ada_Node_Kind_Type range Ada_Generic_Package_Decl ..
         Ada_Generic_Subp_Decl;
      --% no-document: True
   subtype Ada_Generic_Package_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Package_Decl ..
         Ada_Generic_Package_Decl;
      --% no-document: True
   subtype Ada_Generic_Subp_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Subp_Decl .. Ada_Generic_Subp_Decl;
      --% no-document: True
   subtype Ada_Generic_Instantiation is
     Ada_Node_Kind_Type range Ada_Generic_Package_Instantiation ..
         Ada_Generic_Subp_Instantiation;
      --% no-document: True
   subtype Ada_Generic_Package_Instantiation_Range is
     Ada_Node_Kind_Type range Ada_Generic_Package_Instantiation ..
         Ada_Generic_Package_Instantiation;
      --% no-document: True
   subtype Ada_Generic_Subp_Instantiation_Range is
     Ada_Node_Kind_Type range Ada_Generic_Subp_Instantiation ..
         Ada_Generic_Subp_Instantiation;
      --% no-document: True
   subtype Ada_Generic_Renaming_Decl is
     Ada_Node_Kind_Type range Ada_Generic_Package_Renaming_Decl ..
         Ada_Generic_Subp_Renaming_Decl;
      --% no-document: True
   subtype Ada_Generic_Package_Renaming_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Package_Renaming_Decl ..
         Ada_Generic_Package_Renaming_Decl;
      --% no-document: True
   subtype Ada_Generic_Subp_Renaming_Decl_Range is
     Ada_Node_Kind_Type range Ada_Generic_Subp_Renaming_Decl ..
         Ada_Generic_Subp_Renaming_Decl;
      --% no-document: True
   subtype Ada_Label_Decl_Range is
     Ada_Node_Kind_Type range Ada_Label_Decl .. Ada_Label_Decl;
      --% no-document: True
   subtype Ada_Named_Stmt_Decl_Range is
     Ada_Node_Kind_Type range Ada_Named_Stmt_Decl .. Ada_Named_Stmt_Decl;
      --% no-document: True
   subtype Ada_Number_Decl_Range is
     Ada_Node_Kind_Type range Ada_Number_Decl .. Ada_Number_Decl;
      --% no-document: True
   subtype Ada_Object_Decl_Range is
     Ada_Node_Kind_Type range Ada_Object_Decl ..
         Ada_Extended_Return_Stmt_Object_Decl;
      --% no-document: True
   subtype Ada_Anonymous_Object_Decl_Range is
     Ada_Node_Kind_Type range Ada_Anonymous_Object_Decl ..
         Ada_Anonymous_Object_Decl;
      --% no-document: True
   subtype Ada_Extended_Return_Stmt_Object_Decl_Range is
     Ada_Node_Kind_Type range Ada_Extended_Return_Stmt_Object_Decl ..
         Ada_Extended_Return_Stmt_Object_Decl;
      --% no-document: True
   subtype Ada_Package_Renaming_Decl_Range is
     Ada_Node_Kind_Type range Ada_Package_Renaming_Decl ..
         Ada_Package_Renaming_Decl;
      --% no-document: True
   subtype Ada_Single_Protected_Decl_Range is
     Ada_Node_Kind_Type range Ada_Single_Protected_Decl ..
         Ada_Single_Protected_Decl;
      --% no-document: True
   subtype Ada_Single_Task_Decl_Range is
     Ada_Node_Kind_Type range Ada_Single_Task_Decl .. Ada_Single_Task_Decl;
      --% no-document: True
   subtype Ada_Case_Stmt_Alternative_Range is
     Ada_Node_Kind_Type range Ada_Case_Stmt_Alternative ..
         Ada_Case_Stmt_Alternative;
      --% no-document: True
   subtype Ada_Compilation_Unit_Range is
     Ada_Node_Kind_Type range Ada_Compilation_Unit .. Ada_Compilation_Unit;
      --% no-document: True
   subtype Ada_Component_Clause_Range is
     Ada_Node_Kind_Type range Ada_Component_Clause .. Ada_Component_Clause;
      --% no-document: True
   subtype Ada_Component_Def_Range is
     Ada_Node_Kind_Type range Ada_Component_Def .. Ada_Component_Def;
      --% no-document: True
   subtype Ada_Constant_Node is
     Ada_Node_Kind_Type range Ada_Constant_Absent .. Ada_Constant_Present;
      --% no-document: True
   subtype Ada_Constant_Absent_Range is
     Ada_Node_Kind_Type range Ada_Constant_Absent .. Ada_Constant_Absent;
      --% no-document: True
   subtype Ada_Constant_Present_Range is
     Ada_Node_Kind_Type range Ada_Constant_Present .. Ada_Constant_Present;
      --% no-document: True
   subtype Ada_Constraint is
     Ada_Node_Kind_Type range Ada_Delta_Constraint .. Ada_Range_Constraint;
      --% no-document: True
   subtype Ada_Delta_Constraint_Range is
     Ada_Node_Kind_Type range Ada_Delta_Constraint .. Ada_Delta_Constraint;
      --% no-document: True
   subtype Ada_Digits_Constraint_Range is
     Ada_Node_Kind_Type range Ada_Digits_Constraint .. Ada_Digits_Constraint;
      --% no-document: True
   subtype Ada_Discriminant_Constraint_Range is
     Ada_Node_Kind_Type range Ada_Discriminant_Constraint ..
         Ada_Discriminant_Constraint;
      --% no-document: True
   subtype Ada_Index_Constraint_Range is
     Ada_Node_Kind_Type range Ada_Index_Constraint .. Ada_Index_Constraint;
      --% no-document: True
   subtype Ada_Range_Constraint_Range is
     Ada_Node_Kind_Type range Ada_Range_Constraint .. Ada_Range_Constraint;
      --% no-document: True
   subtype Ada_Declarative_Part_Range is
     Ada_Node_Kind_Type range Ada_Declarative_Part .. Ada_Public_Part;
      --% no-document: True
   subtype Ada_Private_Part_Range is
     Ada_Node_Kind_Type range Ada_Private_Part .. Ada_Private_Part;
      --% no-document: True
   subtype Ada_Public_Part_Range is
     Ada_Node_Kind_Type range Ada_Public_Part .. Ada_Public_Part;
      --% no-document: True
   subtype Ada_Elsif_Expr_Part_Range is
     Ada_Node_Kind_Type range Ada_Elsif_Expr_Part .. Ada_Elsif_Expr_Part;
      --% no-document: True
   subtype Ada_Elsif_Stmt_Part_Range is
     Ada_Node_Kind_Type range Ada_Elsif_Stmt_Part .. Ada_Elsif_Stmt_Part;
      --% no-document: True
   subtype Ada_Expr is Ada_Node_Kind_Type range Ada_Allocator .. Ada_Un_Op;
      --% no-document: True
   subtype Ada_Allocator_Range is
     Ada_Node_Kind_Type range Ada_Allocator .. Ada_Allocator;
      --% no-document: True
   subtype Ada_Base_Aggregate is
     Ada_Node_Kind_Type range Ada_Aggregate .. Ada_Null_Record_Aggregate;
      --% no-document: True
   subtype Ada_Aggregate_Range is
     Ada_Node_Kind_Type range Ada_Aggregate .. Ada_Aggregate;
      --% no-document: True
   subtype Ada_Null_Record_Aggregate_Range is
     Ada_Node_Kind_Type range Ada_Null_Record_Aggregate ..
         Ada_Null_Record_Aggregate;
      --% no-document: True
   subtype Ada_Bin_Op_Range is
     Ada_Node_Kind_Type range Ada_Bin_Op .. Ada_Relation_Op;
      --% no-document: True
   subtype Ada_Relation_Op_Range is
     Ada_Node_Kind_Type range Ada_Relation_Op .. Ada_Relation_Op;
      --% no-document: True
   subtype Ada_Box_Expr_Range is
     Ada_Node_Kind_Type range Ada_Box_Expr .. Ada_Box_Expr;
      --% no-document: True
   subtype Ada_Case_Expr_Range is
     Ada_Node_Kind_Type range Ada_Case_Expr .. Ada_Case_Expr;
      --% no-document: True
   subtype Ada_Case_Expr_Alternative_Range is
     Ada_Node_Kind_Type range Ada_Case_Expr_Alternative ..
         Ada_Case_Expr_Alternative;
      --% no-document: True
   subtype Ada_Contract_Cases_Range is
     Ada_Node_Kind_Type range Ada_Contract_Cases .. Ada_Contract_Cases;
      --% no-document: True
   subtype Ada_If_Expr_Range is
     Ada_Node_Kind_Type range Ada_If_Expr .. Ada_If_Expr;
      --% no-document: True
   subtype Ada_Membership_Expr_Range is
     Ada_Node_Kind_Type range Ada_Membership_Expr .. Ada_Membership_Expr;
      --% no-document: True
   subtype Ada_Name is
     Ada_Node_Kind_Type range Ada_Attribute_Ref .. Ada_Target_Name;
      --% no-document: True
   subtype Ada_Attribute_Ref_Range is
     Ada_Node_Kind_Type range Ada_Attribute_Ref .. Ada_Update_Attribute_Ref;
      --% no-document: True
   subtype Ada_Update_Attribute_Ref_Range is
     Ada_Node_Kind_Type range Ada_Update_Attribute_Ref ..
         Ada_Update_Attribute_Ref;
      --% no-document: True
   subtype Ada_Call_Expr_Range is
     Ada_Node_Kind_Type range Ada_Call_Expr .. Ada_Call_Expr;
      --% no-document: True
   subtype Ada_Defining_Name_Range is
     Ada_Node_Kind_Type range Ada_Defining_Name .. Ada_Defining_Name;
      --% no-document: True
   subtype Ada_Discrete_Subtype_Name_Range is
     Ada_Node_Kind_Type range Ada_Discrete_Subtype_Name ..
         Ada_Discrete_Subtype_Name;
      --% no-document: True
   subtype Ada_Dotted_Name_Range is
     Ada_Node_Kind_Type range Ada_Dotted_Name .. Ada_Dotted_Name;
      --% no-document: True
   subtype Ada_End_Name_Range is
     Ada_Node_Kind_Type range Ada_End_Name .. Ada_End_Name;
      --% no-document: True
   subtype Ada_Explicit_Deref_Range is
     Ada_Node_Kind_Type range Ada_Explicit_Deref .. Ada_Explicit_Deref;
      --% no-document: True
   subtype Ada_Qual_Expr_Range is
     Ada_Node_Kind_Type range Ada_Qual_Expr .. Ada_Qual_Expr;
      --% no-document: True
   subtype Ada_Single_Tok_Node is
     Ada_Node_Kind_Type range Ada_Char_Literal .. Ada_Real_Literal;
      --% no-document: True
   subtype Ada_Base_Id is
     Ada_Node_Kind_Type range Ada_Char_Literal .. Ada_String_Literal;
      --% no-document: True
   subtype Ada_Char_Literal_Range is
     Ada_Node_Kind_Type range Ada_Char_Literal .. Ada_Char_Literal;
      --% no-document: True
   subtype Ada_Identifier_Range is
     Ada_Node_Kind_Type range Ada_Identifier .. Ada_Identifier;
      --% no-document: True
   subtype Ada_Op is Ada_Node_Kind_Type range Ada_Op_Abs .. Ada_Op_Xor;
      --% no-document: True
   subtype Ada_Op_Abs_Range is
     Ada_Node_Kind_Type range Ada_Op_Abs .. Ada_Op_Abs;
      --% no-document: True
   subtype Ada_Op_And_Range is
     Ada_Node_Kind_Type range Ada_Op_And .. Ada_Op_And;
      --% no-document: True
   subtype Ada_Op_And_Then_Range is
     Ada_Node_Kind_Type range Ada_Op_And_Then .. Ada_Op_And_Then;
      --% no-document: True
   subtype Ada_Op_Concat_Range is
     Ada_Node_Kind_Type range Ada_Op_Concat .. Ada_Op_Concat;
      --% no-document: True
   subtype Ada_Op_Div_Range is
     Ada_Node_Kind_Type range Ada_Op_Div .. Ada_Op_Div;
      --% no-document: True
   subtype Ada_Op_Double_Dot_Range is
     Ada_Node_Kind_Type range Ada_Op_Double_Dot .. Ada_Op_Double_Dot;
      --% no-document: True
   subtype Ada_Op_Eq_Range is Ada_Node_Kind_Type range Ada_Op_Eq .. Ada_Op_Eq;
      --% no-document: True
   subtype Ada_Op_Gt_Range is Ada_Node_Kind_Type range Ada_Op_Gt .. Ada_Op_Gt;
      --% no-document: True
   subtype Ada_Op_Gte_Range is
     Ada_Node_Kind_Type range Ada_Op_Gte .. Ada_Op_Gte;
      --% no-document: True
   subtype Ada_Op_In_Range is Ada_Node_Kind_Type range Ada_Op_In .. Ada_Op_In;
      --% no-document: True
   subtype Ada_Op_Lt_Range is Ada_Node_Kind_Type range Ada_Op_Lt .. Ada_Op_Lt;
      --% no-document: True
   subtype Ada_Op_Lte_Range is
     Ada_Node_Kind_Type range Ada_Op_Lte .. Ada_Op_Lte;
      --% no-document: True
   subtype Ada_Op_Minus_Range is
     Ada_Node_Kind_Type range Ada_Op_Minus .. Ada_Op_Minus;
      --% no-document: True
   subtype Ada_Op_Mod_Range is
     Ada_Node_Kind_Type range Ada_Op_Mod .. Ada_Op_Mod;
      --% no-document: True
   subtype Ada_Op_Mult_Range is
     Ada_Node_Kind_Type range Ada_Op_Mult .. Ada_Op_Mult;
      --% no-document: True
   subtype Ada_Op_Neq_Range is
     Ada_Node_Kind_Type range Ada_Op_Neq .. Ada_Op_Neq;
      --% no-document: True
   subtype Ada_Op_Not_Range is
     Ada_Node_Kind_Type range Ada_Op_Not .. Ada_Op_Not;
      --% no-document: True
   subtype Ada_Op_Not_In_Range is
     Ada_Node_Kind_Type range Ada_Op_Not_In .. Ada_Op_Not_In;
      --% no-document: True
   subtype Ada_Op_Or_Range is Ada_Node_Kind_Type range Ada_Op_Or .. Ada_Op_Or;
      --% no-document: True
   subtype Ada_Op_Or_Else_Range is
     Ada_Node_Kind_Type range Ada_Op_Or_Else .. Ada_Op_Or_Else;
      --% no-document: True
   subtype Ada_Op_Plus_Range is
     Ada_Node_Kind_Type range Ada_Op_Plus .. Ada_Op_Plus;
      --% no-document: True
   subtype Ada_Op_Pow_Range is
     Ada_Node_Kind_Type range Ada_Op_Pow .. Ada_Op_Pow;
      --% no-document: True
   subtype Ada_Op_Rem_Range is
     Ada_Node_Kind_Type range Ada_Op_Rem .. Ada_Op_Rem;
      --% no-document: True
   subtype Ada_Op_Xor_Range is
     Ada_Node_Kind_Type range Ada_Op_Xor .. Ada_Op_Xor;
      --% no-document: True
   subtype Ada_String_Literal_Range is
     Ada_Node_Kind_Type range Ada_String_Literal .. Ada_String_Literal;
      --% no-document: True
   subtype Ada_Null_Literal_Range is
     Ada_Node_Kind_Type range Ada_Null_Literal .. Ada_Null_Literal;
      --% no-document: True
   subtype Ada_Num_Literal is
     Ada_Node_Kind_Type range Ada_Int_Literal .. Ada_Real_Literal;
      --% no-document: True
   subtype Ada_Int_Literal_Range is
     Ada_Node_Kind_Type range Ada_Int_Literal .. Ada_Int_Literal;
      --% no-document: True
   subtype Ada_Real_Literal_Range is
     Ada_Node_Kind_Type range Ada_Real_Literal .. Ada_Real_Literal;
      --% no-document: True
   subtype Ada_Target_Name_Range is
     Ada_Node_Kind_Type range Ada_Target_Name .. Ada_Target_Name;
      --% no-document: True
   subtype Ada_Paren_Expr_Range is
     Ada_Node_Kind_Type range Ada_Paren_Expr .. Ada_Paren_Expr;
      --% no-document: True
   subtype Ada_Quantified_Expr_Range is
     Ada_Node_Kind_Type range Ada_Quantified_Expr .. Ada_Quantified_Expr;
      --% no-document: True
   subtype Ada_Raise_Expr_Range is
     Ada_Node_Kind_Type range Ada_Raise_Expr .. Ada_Raise_Expr;
      --% no-document: True
   subtype Ada_Un_Op_Range is Ada_Node_Kind_Type range Ada_Un_Op .. Ada_Un_Op;
      --% no-document: True
   subtype Ada_Handled_Stmts_Range is
     Ada_Node_Kind_Type range Ada_Handled_Stmts .. Ada_Handled_Stmts;
      --% no-document: True
   subtype Ada_Interface_Kind is
     Ada_Node_Kind_Type range Ada_Interface_Kind_Limited ..
         Ada_Interface_Kind_Task;
      --% no-document: True
   subtype Ada_Interface_Kind_Limited_Range is
     Ada_Node_Kind_Type range Ada_Interface_Kind_Limited ..
         Ada_Interface_Kind_Limited;
      --% no-document: True
   subtype Ada_Interface_Kind_Protected_Range is
     Ada_Node_Kind_Type range Ada_Interface_Kind_Protected ..
         Ada_Interface_Kind_Protected;
      --% no-document: True
   subtype Ada_Interface_Kind_Synchronized_Range is
     Ada_Node_Kind_Type range Ada_Interface_Kind_Synchronized ..
         Ada_Interface_Kind_Synchronized;
      --% no-document: True
   subtype Ada_Interface_Kind_Task_Range is
     Ada_Node_Kind_Type range Ada_Interface_Kind_Task ..
         Ada_Interface_Kind_Task;
      --% no-document: True
   subtype Ada_Iter_Type is
     Ada_Node_Kind_Type range Ada_Iter_Type_In .. Ada_Iter_Type_Of;
      --% no-document: True
   subtype Ada_Iter_Type_In_Range is
     Ada_Node_Kind_Type range Ada_Iter_Type_In .. Ada_Iter_Type_In;
      --% no-document: True
   subtype Ada_Iter_Type_Of_Range is
     Ada_Node_Kind_Type range Ada_Iter_Type_Of .. Ada_Iter_Type_Of;
      --% no-document: True
   subtype Ada_Library_Item_Range is
     Ada_Node_Kind_Type range Ada_Library_Item .. Ada_Library_Item;
      --% no-document: True
   subtype Ada_Limited_Node is
     Ada_Node_Kind_Type range Ada_Limited_Absent .. Ada_Limited_Present;
      --% no-document: True
   subtype Ada_Limited_Absent_Range is
     Ada_Node_Kind_Type range Ada_Limited_Absent .. Ada_Limited_Absent;
      --% no-document: True
   subtype Ada_Limited_Present_Range is
     Ada_Node_Kind_Type range Ada_Limited_Present .. Ada_Limited_Present;
      --% no-document: True
   subtype Ada_Loop_Spec is
     Ada_Node_Kind_Type range Ada_For_Loop_Spec .. Ada_While_Loop_Spec;
      --% no-document: True
   subtype Ada_For_Loop_Spec_Range is
     Ada_Node_Kind_Type range Ada_For_Loop_Spec .. Ada_For_Loop_Spec;
      --% no-document: True
   subtype Ada_While_Loop_Spec_Range is
     Ada_Node_Kind_Type range Ada_While_Loop_Spec .. Ada_While_Loop_Spec;
      --% no-document: True
   subtype Ada_Mode is
     Ada_Node_Kind_Type range Ada_Mode_Default .. Ada_Mode_Out;
      --% no-document: True
   subtype Ada_Mode_Default_Range is
     Ada_Node_Kind_Type range Ada_Mode_Default .. Ada_Mode_Default;
      --% no-document: True
   subtype Ada_Mode_In_Range is
     Ada_Node_Kind_Type range Ada_Mode_In .. Ada_Mode_In;
      --% no-document: True
   subtype Ada_Mode_In_Out_Range is
     Ada_Node_Kind_Type range Ada_Mode_In_Out .. Ada_Mode_In_Out;
      --% no-document: True
   subtype Ada_Mode_Out_Range is
     Ada_Node_Kind_Type range Ada_Mode_Out .. Ada_Mode_Out;
      --% no-document: True
   subtype Ada_Not_Null is
     Ada_Node_Kind_Type range Ada_Not_Null_Absent .. Ada_Not_Null_Present;
      --% no-document: True
   subtype Ada_Not_Null_Absent_Range is
     Ada_Node_Kind_Type range Ada_Not_Null_Absent .. Ada_Not_Null_Absent;
      --% no-document: True
   subtype Ada_Not_Null_Present_Range is
     Ada_Node_Kind_Type range Ada_Not_Null_Present .. Ada_Not_Null_Present;
      --% no-document: True
   subtype Ada_Null_Component_Decl_Range is
     Ada_Node_Kind_Type range Ada_Null_Component_Decl ..
         Ada_Null_Component_Decl;
      --% no-document: True
   subtype Ada_Others_Designator_Range is
     Ada_Node_Kind_Type range Ada_Others_Designator .. Ada_Others_Designator;
      --% no-document: True
   subtype Ada_Overriding_Node is
     Ada_Node_Kind_Type range Ada_Overriding_Not_Overriding ..
         Ada_Overriding_Unspecified;
      --% no-document: True
   subtype Ada_Overriding_Not_Overriding_Range is
     Ada_Node_Kind_Type range Ada_Overriding_Not_Overriding ..
         Ada_Overriding_Not_Overriding;
      --% no-document: True
   subtype Ada_Overriding_Overriding_Range is
     Ada_Node_Kind_Type range Ada_Overriding_Overriding ..
         Ada_Overriding_Overriding;
      --% no-document: True
   subtype Ada_Overriding_Unspecified_Range is
     Ada_Node_Kind_Type range Ada_Overriding_Unspecified ..
         Ada_Overriding_Unspecified;
      --% no-document: True
   subtype Ada_Params_Range is
     Ada_Node_Kind_Type range Ada_Params .. Ada_Params;
      --% no-document: True
   subtype Ada_Pragma_Node_Range is
     Ada_Node_Kind_Type range Ada_Pragma_Node .. Ada_Pragma_Node;
      --% no-document: True
   subtype Ada_Prim_Type_Accessor_Range is
     Ada_Node_Kind_Type range Ada_Prim_Type_Accessor .. Ada_Prim_Type_Accessor;
      --% no-document: True
   subtype Ada_Private_Node is
     Ada_Node_Kind_Type range Ada_Private_Absent .. Ada_Private_Present;
      --% no-document: True
   subtype Ada_Private_Absent_Range is
     Ada_Node_Kind_Type range Ada_Private_Absent .. Ada_Private_Absent;
      --% no-document: True
   subtype Ada_Private_Present_Range is
     Ada_Node_Kind_Type range Ada_Private_Present .. Ada_Private_Present;
      --% no-document: True
   subtype Ada_Protected_Def_Range is
     Ada_Node_Kind_Type range Ada_Protected_Def .. Ada_Protected_Def;
      --% no-document: True
   subtype Ada_Protected_Node is
     Ada_Node_Kind_Type range Ada_Protected_Absent .. Ada_Protected_Present;
      --% no-document: True
   subtype Ada_Protected_Absent_Range is
     Ada_Node_Kind_Type range Ada_Protected_Absent .. Ada_Protected_Absent;
      --% no-document: True
   subtype Ada_Protected_Present_Range is
     Ada_Node_Kind_Type range Ada_Protected_Present .. Ada_Protected_Present;
      --% no-document: True
   subtype Ada_Quantifier is
     Ada_Node_Kind_Type range Ada_Quantifier_All .. Ada_Quantifier_Some;
      --% no-document: True
   subtype Ada_Quantifier_All_Range is
     Ada_Node_Kind_Type range Ada_Quantifier_All .. Ada_Quantifier_All;
      --% no-document: True
   subtype Ada_Quantifier_Some_Range is
     Ada_Node_Kind_Type range Ada_Quantifier_Some .. Ada_Quantifier_Some;
      --% no-document: True
   subtype Ada_Range_Spec_Range is
     Ada_Node_Kind_Type range Ada_Range_Spec .. Ada_Range_Spec;
      --% no-document: True
   subtype Ada_Renaming_Clause_Range is
     Ada_Node_Kind_Type range Ada_Renaming_Clause ..
         Ada_Synthetic_Renaming_Clause;
      --% no-document: True
   subtype Ada_Synthetic_Renaming_Clause_Range is
     Ada_Node_Kind_Type range Ada_Synthetic_Renaming_Clause ..
         Ada_Synthetic_Renaming_Clause;
      --% no-document: True
   subtype Ada_Reverse_Node is
     Ada_Node_Kind_Type range Ada_Reverse_Absent .. Ada_Reverse_Present;
      --% no-document: True
   subtype Ada_Reverse_Absent_Range is
     Ada_Node_Kind_Type range Ada_Reverse_Absent .. Ada_Reverse_Absent;
      --% no-document: True
   subtype Ada_Reverse_Present_Range is
     Ada_Node_Kind_Type range Ada_Reverse_Present .. Ada_Reverse_Present;
      --% no-document: True
   subtype Ada_Select_When_Part_Range is
     Ada_Node_Kind_Type range Ada_Select_When_Part .. Ada_Select_When_Part;
      --% no-document: True
   subtype Ada_Stmt is
     Ada_Node_Kind_Type range Ada_Accept_Stmt .. Ada_Terminate_Alternative;
      --% no-document: True
   subtype Ada_Composite_Stmt is
     Ada_Node_Kind_Type range Ada_Accept_Stmt .. Ada_Select_Stmt;
      --% no-document: True
   subtype Ada_Accept_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Accept_Stmt .. Ada_Accept_Stmt_With_Stmts;
      --% no-document: True
   subtype Ada_Accept_Stmt_With_Stmts_Range is
     Ada_Node_Kind_Type range Ada_Accept_Stmt_With_Stmts ..
         Ada_Accept_Stmt_With_Stmts;
      --% no-document: True
   subtype Ada_Base_Loop_Stmt is
     Ada_Node_Kind_Type range Ada_For_Loop_Stmt .. Ada_While_Loop_Stmt;
      --% no-document: True
   subtype Ada_For_Loop_Stmt_Range is
     Ada_Node_Kind_Type range Ada_For_Loop_Stmt .. Ada_For_Loop_Stmt;
      --% no-document: True
   subtype Ada_Loop_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Loop_Stmt .. Ada_Loop_Stmt;
      --% no-document: True
   subtype Ada_While_Loop_Stmt_Range is
     Ada_Node_Kind_Type range Ada_While_Loop_Stmt .. Ada_While_Loop_Stmt;
      --% no-document: True
   subtype Ada_Block_Stmt is
     Ada_Node_Kind_Type range Ada_Begin_Block .. Ada_Decl_Block;
      --% no-document: True
   subtype Ada_Begin_Block_Range is
     Ada_Node_Kind_Type range Ada_Begin_Block .. Ada_Begin_Block;
      --% no-document: True
   subtype Ada_Decl_Block_Range is
     Ada_Node_Kind_Type range Ada_Decl_Block .. Ada_Decl_Block;
      --% no-document: True
   subtype Ada_Case_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Case_Stmt .. Ada_Case_Stmt;
      --% no-document: True
   subtype Ada_Extended_Return_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Extended_Return_Stmt ..
         Ada_Extended_Return_Stmt;
      --% no-document: True
   subtype Ada_If_Stmt_Range is
     Ada_Node_Kind_Type range Ada_If_Stmt .. Ada_If_Stmt;
      --% no-document: True
   subtype Ada_Named_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Named_Stmt .. Ada_Named_Stmt;
      --% no-document: True
   subtype Ada_Select_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Select_Stmt .. Ada_Select_Stmt;
      --% no-document: True
   subtype Ada_Error_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Error_Stmt .. Ada_Error_Stmt;
      --% no-document: True
   subtype Ada_Simple_Stmt is
     Ada_Node_Kind_Type range Ada_Abort_Stmt .. Ada_Terminate_Alternative;
      --% no-document: True
   subtype Ada_Abort_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Abort_Stmt .. Ada_Abort_Stmt;
      --% no-document: True
   subtype Ada_Assign_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Assign_Stmt .. Ada_Assign_Stmt;
      --% no-document: True
   subtype Ada_Call_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Call_Stmt .. Ada_Call_Stmt;
      --% no-document: True
   subtype Ada_Delay_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Delay_Stmt .. Ada_Delay_Stmt;
      --% no-document: True
   subtype Ada_Exit_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Exit_Stmt .. Ada_Exit_Stmt;
      --% no-document: True
   subtype Ada_Goto_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Goto_Stmt .. Ada_Goto_Stmt;
      --% no-document: True
   subtype Ada_Label_Range is Ada_Node_Kind_Type range Ada_Label .. Ada_Label;
      --% no-document: True
   subtype Ada_Null_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Null_Stmt .. Ada_Null_Stmt;
      --% no-document: True
   subtype Ada_Raise_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Raise_Stmt .. Ada_Raise_Stmt;
      --% no-document: True
   subtype Ada_Requeue_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Requeue_Stmt .. Ada_Requeue_Stmt;
      --% no-document: True
   subtype Ada_Return_Stmt_Range is
     Ada_Node_Kind_Type range Ada_Return_Stmt .. Ada_Return_Stmt;
      --% no-document: True
   subtype Ada_Terminate_Alternative_Range is
     Ada_Node_Kind_Type range Ada_Terminate_Alternative ..
         Ada_Terminate_Alternative;
      --% no-document: True
   subtype Ada_Subp_Kind is
     Ada_Node_Kind_Type range Ada_Subp_Kind_Function ..
         Ada_Subp_Kind_Procedure;
      --% no-document: True
   subtype Ada_Subp_Kind_Function_Range is
     Ada_Node_Kind_Type range Ada_Subp_Kind_Function .. Ada_Subp_Kind_Function;
      --% no-document: True
   subtype Ada_Subp_Kind_Procedure_Range is
     Ada_Node_Kind_Type range Ada_Subp_Kind_Procedure ..
         Ada_Subp_Kind_Procedure;
      --% no-document: True
   subtype Ada_Subunit_Range is
     Ada_Node_Kind_Type range Ada_Subunit .. Ada_Subunit;
      --% no-document: True
   subtype Ada_Synchronized_Node is
     Ada_Node_Kind_Type range Ada_Synchronized_Absent ..
         Ada_Synchronized_Present;
      --% no-document: True
   subtype Ada_Synchronized_Absent_Range is
     Ada_Node_Kind_Type range Ada_Synchronized_Absent ..
         Ada_Synchronized_Absent;
      --% no-document: True
   subtype Ada_Synchronized_Present_Range is
     Ada_Node_Kind_Type range Ada_Synchronized_Present ..
         Ada_Synchronized_Present;
      --% no-document: True
   subtype Ada_Tagged_Node is
     Ada_Node_Kind_Type range Ada_Tagged_Absent .. Ada_Tagged_Present;
      --% no-document: True
   subtype Ada_Tagged_Absent_Range is
     Ada_Node_Kind_Type range Ada_Tagged_Absent .. Ada_Tagged_Absent;
      --% no-document: True
   subtype Ada_Tagged_Present_Range is
     Ada_Node_Kind_Type range Ada_Tagged_Present .. Ada_Tagged_Present;
      --% no-document: True
   subtype Ada_Task_Def_Range is
     Ada_Node_Kind_Type range Ada_Task_Def .. Ada_Task_Def;
      --% no-document: True
   subtype Ada_Type_Def is
     Ada_Node_Kind_Type range Ada_Access_To_Subp_Def ..
         Ada_Signed_Int_Type_Def;
      --% no-document: True
   subtype Ada_Access_Def is
     Ada_Node_Kind_Type range Ada_Access_To_Subp_Def .. Ada_Type_Access_Def;
      --% no-document: True
   subtype Ada_Access_To_Subp_Def_Range is
     Ada_Node_Kind_Type range Ada_Access_To_Subp_Def .. Ada_Access_To_Subp_Def;
      --% no-document: True
   subtype Ada_Base_Type_Access_Def is
     Ada_Node_Kind_Type range Ada_Anonymous_Type_Access_Def ..
         Ada_Type_Access_Def;
      --% no-document: True
   subtype Ada_Anonymous_Type_Access_Def_Range is
     Ada_Node_Kind_Type range Ada_Anonymous_Type_Access_Def ..
         Ada_Anonymous_Type_Access_Def;
      --% no-document: True
   subtype Ada_Type_Access_Def_Range is
     Ada_Node_Kind_Type range Ada_Type_Access_Def .. Ada_Type_Access_Def;
      --% no-document: True
   subtype Ada_Array_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Array_Type_Def .. Ada_Array_Type_Def;
      --% no-document: True
   subtype Ada_Derived_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Derived_Type_Def .. Ada_Derived_Type_Def;
      --% no-document: True
   subtype Ada_Enum_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Enum_Type_Def .. Ada_Enum_Type_Def;
      --% no-document: True
   subtype Ada_Formal_Discrete_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Formal_Discrete_Type_Def ..
         Ada_Formal_Discrete_Type_Def;
      --% no-document: True
   subtype Ada_Interface_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Interface_Type_Def .. Ada_Interface_Type_Def;
      --% no-document: True
   subtype Ada_Mod_Int_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Mod_Int_Type_Def .. Ada_Mod_Int_Type_Def;
      --% no-document: True
   subtype Ada_Private_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Private_Type_Def .. Ada_Private_Type_Def;
      --% no-document: True
   subtype Ada_Real_Type_Def is
     Ada_Node_Kind_Type range Ada_Decimal_Fixed_Point_Def ..
         Ada_Ordinary_Fixed_Point_Def;
      --% no-document: True
   subtype Ada_Decimal_Fixed_Point_Def_Range is
     Ada_Node_Kind_Type range Ada_Decimal_Fixed_Point_Def ..
         Ada_Decimal_Fixed_Point_Def;
      --% no-document: True
   subtype Ada_Floating_Point_Def_Range is
     Ada_Node_Kind_Type range Ada_Floating_Point_Def .. Ada_Floating_Point_Def;
      --% no-document: True
   subtype Ada_Ordinary_Fixed_Point_Def_Range is
     Ada_Node_Kind_Type range Ada_Ordinary_Fixed_Point_Def ..
         Ada_Ordinary_Fixed_Point_Def;
      --% no-document: True
   subtype Ada_Record_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Record_Type_Def .. Ada_Record_Type_Def;
      --% no-document: True
   subtype Ada_Signed_Int_Type_Def_Range is
     Ada_Node_Kind_Type range Ada_Signed_Int_Type_Def ..
         Ada_Signed_Int_Type_Def;
      --% no-document: True
   subtype Ada_Type_Expr is
     Ada_Node_Kind_Type range Ada_Anonymous_Type ..
         Ada_Discrete_Subtype_Indication;
      --% no-document: True
   subtype Ada_Anonymous_Type_Range is
     Ada_Node_Kind_Type range Ada_Anonymous_Type .. Ada_Anonymous_Type;
      --% no-document: True
   subtype Ada_Enum_Lit_Synth_Type_Expr_Range is
     Ada_Node_Kind_Type range Ada_Enum_Lit_Synth_Type_Expr ..
         Ada_Enum_Lit_Synth_Type_Expr;
      --% no-document: True
   subtype Ada_Subtype_Indication_Range is
     Ada_Node_Kind_Type range Ada_Subtype_Indication ..
         Ada_Discrete_Subtype_Indication;
      --% no-document: True
   subtype Ada_Constrained_Subtype_Indication_Range is
     Ada_Node_Kind_Type range Ada_Constrained_Subtype_Indication ..
         Ada_Constrained_Subtype_Indication;
      --% no-document: True
   subtype Ada_Discrete_Subtype_Indication_Range is
     Ada_Node_Kind_Type range Ada_Discrete_Subtype_Indication ..
         Ada_Discrete_Subtype_Indication;
      --% no-document: True
   subtype Ada_Unconstrained_Array_Index_Range is
     Ada_Node_Kind_Type range Ada_Unconstrained_Array_Index ..
         Ada_Unconstrained_Array_Index;
      --% no-document: True
   subtype Ada_Until_Node is
     Ada_Node_Kind_Type range Ada_Until_Absent .. Ada_Until_Present;
      --% no-document: True
   subtype Ada_Until_Absent_Range is
     Ada_Node_Kind_Type range Ada_Until_Absent .. Ada_Until_Absent;
      --% no-document: True
   subtype Ada_Until_Present_Range is
     Ada_Node_Kind_Type range Ada_Until_Present .. Ada_Until_Present;
      --% no-document: True
   subtype Ada_Use_Clause is
     Ada_Node_Kind_Type range Ada_Use_Package_Clause .. Ada_Use_Type_Clause;
      --% no-document: True
   subtype Ada_Use_Package_Clause_Range is
     Ada_Node_Kind_Type range Ada_Use_Package_Clause .. Ada_Use_Package_Clause;
      --% no-document: True
   subtype Ada_Use_Type_Clause_Range is
     Ada_Node_Kind_Type range Ada_Use_Type_Clause .. Ada_Use_Type_Clause;
      --% no-document: True
   subtype Ada_Variant_Range is
     Ada_Node_Kind_Type range Ada_Variant .. Ada_Variant;
      --% no-document: True
   subtype Ada_Variant_Part_Range is
     Ada_Node_Kind_Type range Ada_Variant_Part .. Ada_Variant_Part;
      --% no-document: True
   subtype Ada_With_Clause_Range is
     Ada_Node_Kind_Type range Ada_With_Clause .. Ada_With_Clause;
      --% no-document: True
   subtype Ada_With_Private is
     Ada_Node_Kind_Type range Ada_With_Private_Absent ..
         Ada_With_Private_Present;
      --% no-document: True
   subtype Ada_With_Private_Absent_Range is
     Ada_Node_Kind_Type range Ada_With_Private_Absent ..
         Ada_With_Private_Absent;
      --% no-document: True
   subtype Ada_With_Private_Present_Range is
     Ada_Node_Kind_Type range Ada_With_Private_Present ..
         Ada_With_Private_Present;
      --% no-document: True

   subtype Synthetic_Nodes is Ada_Node_Kind_Type with
        Static_Predicate => Synthetic_Nodes in Ada_Enum_Subp_Spec |
            Ada_Discrete_Base_Subtype_Decl | Ada_Classwide_Type_Decl |
            Ada_Synth_Anonymous_Type_Decl | Ada_Anonymous_Object_Decl |
            Ada_Prim_Type_Accessor | Ada_Synthetic_Renaming_Clause |
            Ada_Anonymous_Type_Access_Def | Ada_Enum_Lit_Synth_Type_Expr;
      --  Set of nodes that are synthetic.
      --
      --  Parsers cannot create synthetic nodes, so these correspond to no
      --  source text. These nodes are created dynamically for convenience
      --  during semantic analysis.

   type Analysis_Unit_Kind is (Unit_Specification, Unit_Body) with
      Convention => C;
      --  Specify a kind of analysis unit. Specification units provide
      --  an interface to the outer world while body units provide an
      --  implementation for the corresponding interface.

   function Trace_Image (Self : Analysis_Unit_Kind) return String is
     (Self'Image);

   type Lookup_Kind is (Recursive, Flat, Minimal) with
      Convention => C;

   function Trace_Image (Self : Lookup_Kind) return String is (Self'Image);

   type Find_All_Mode is (References, Derivedtypes) with
      Convention => C;

   function Trace_Image (Self : Find_All_Mode) return String is (Self'Image);

   type Ref_Result_Kind is (Noref, Precise, Imprecise, Error) with
      Convention => C;
      --  Kind for the result of a cross reference operation.
      --
      --  - ``NoRef`` is for no reference, it is the null value for this enum.
      --
      --  - ``Precise`` is when the reference result is precise.
      --
      --  - ``Imprecise`` is when there was an error computing the precise
      --  result, and a result was gotten in an imprecise fashion.
      --
      --  - ``Error`` is for unrecoverable errors (either there is no imprecise
      --  path for the request you made, or the imprecise path errored out too.

   function Trace_Image (Self : Ref_Result_Kind) return String is (Self'Image);

   type Grammar_Rule is
     (Decimal_Fixed_Point_Def_Rule, Relation_Rule, Aspect_Spec_Rule,
      Simple_Subp_Decl_Rule, Anonymous_Type_Decl_Rule, Formal_Subp_Decl_Rule,
      Param_Spec_Rule, Decl_Part_Rule, Aspect_Clause_Rule, If_Expr_Rule,
      Task_Def_Rule, Select_Stmt_Rule, Param_Specs_Rule, Iloop_Stmt_Rule,
      Defining_Id_List_Rule, Int_Literal_Rule, Protected_Body_Stub_Rule,
      Update_Attr_Aggregate_Rule, Array_Type_Def_Rule, Protected_El_Rule,
      Simple_Stmt_Rule, Interface_Type_Def_Rule, Compilation_Unit_Rule,
      Component_Clause_Rule, Signed_Int_Type_Def_Rule, Null_Literal_Rule,
      Type_Decl_Rule, Range_Spec_Rule, Paren_Expr_Rule, Derived_Type_Def_Rule,
      Box_Expr_Rule, Term_Rule, Others_Designator_Rule, If_Stmt_Rule,
      Generic_Formal_Part_Rule, Discriminant_Part_Rule, Protected_Def_Rule,
      Renaming_Clause_Rule, Single_Task_Decl_Rule,
      Discrete_Subtype_Indication_Rule, Boolean_Op_Rule, Mode_Rule,
      Rel_Op_Rule, Accept_Stmt_Rule, Task_Body_Stub_Rule,
      Discriminant_Constraint_Rule, Subtype_Decl_Rule,
      Membership_Choice_List_Rule, Delta_Constraint_Rule, Static_Name_Rule,
      Char_Literal_Rule, Discrete_Subtype_Definition_Rule,
      Overriding_Indicator_Rule, Range_Constraint_Rule, Recov_Decl_Part_Rule,
      Package_Renaming_Decl_Rule, String_Literal_Rule, Package_Body_Rule,
      Case_Stmt_Rule, Record_Def_Rule, Stmts_Rule, Contract_Cases_Expr_Rule,
      Block_Stmt_Rule, Label_Rule, Abstract_Subp_Decl_Rule,
      For_Loop_Param_Spec_Rule, Direct_Name_Or_Target_Name_Rule,
      Assignment_Stmt_Rule, Sub_Object_Decl_Rule, Subunit_Rule,
      Param_Assoc_Rule, Floating_Point_Def_Rule, Compound_Stmt_Rule,
      Package_Body_Stub_Rule, Body_Rule, Discr_Spec_List_Rule,
      Object_Decl_Rule, Record_Type_Def_Rule, Compilation_Rule,
      Library_Unit_Renaming_Decl_Rule, Stmt_Rule, Entry_Body_Rule,
      With_Clause_Rule, Terminate_Alternative_Rule,
      Ext_Ret_Stmt_Object_Decl_Rule, Subtype_Indication_Rule, Identifier_Rule,
      Library_Unit_Body_Rule, Constraint_Rule, Sexpr_Or_Box_Rule,
      Use_Clause_Rule, Subp_Spec_Rule, Unop_Term_Rule, Null_Subp_Decl_Rule,
      Ext_Return_Stmt_Rule, Use_Package_Clause_Rule, Variant_Part_Rule,
      Membership_Choice_Rule, Exception_Handler_Rule, Primary_Rule,
      Constraint_List_Rule, Raise_Expr_Rule, Return_Stmt_Rule,
      Generic_Decl_Rule, Component_Decl_Rule, Anonymous_Type_Rule,
      Mod_Int_Type_Def_Rule, Pragma_Argument_Rule, Component_Def_Rule,
      Aggregate_Assoc_Rule, Expr_Fn_Rule, Call_Suffix_Rule, Aspect_Assoc_Rule,
      Protected_Body_Rule, Iblock_Stmt_Rule, Name_Rule, Context_Item_Rule,
      Task_Type_Decl_Rule, Discriminant_Spec_Rule, Generic_Renaming_Decl_Rule,
      Delay_Stmt_Rule, Use_Type_Clause_Rule, Real_Type_Def_Rule,
      Call_Stmt_Rule, Body_Stub_Rule, Factor_Rule, Defining_Id_Rule,
      Entry_Decl_Rule, Protected_Type_Decl_Rule, Formal_Discrete_Type_Def_Rule,
      Discrete_Range_Rule, Pragma_Rule, Requeue_Stmt_Rule, Raise_Stmt_Rule,
      Type_Def_Rule, Qual_Name_Internal_Rule, Conditional_Expr_Rule,
      Contract_Case_Assoc_Rule, Basic_Decl_Rule, Expr_Rule,
      Subp_Renaming_Decl_Rule, Simple_Expr_Rule, Case_Alt_Rule,
      Generic_Formal_Decl_Rule, Enum_Literal_Decl_Rule, Digits_Constraint_Rule,
      Goto_Stmt_Rule, Direct_Name_Rule, Handled_Stmts_Rule, Task_Item_Rule,
      Case_Expr_Rule, Target_Name_Rule, Protected_Decl_Rule, Task_Body_Rule,
      Abort_Stmt_Rule, Dec_Literal_Rule, Exception_Decl_Rule,
      Package_Decl_Rule, Subp_Body_Stub_Rule, Discriminant_Assoc_Rule,
      Qualified_Name_Rule, Enum_Type_Def_Rule, Choice_List_Rule,
      Allocator_Rule, Defining_Name_Rule, Subp_Body_Rule,
      Update_Attr_Content_Rule, Case_Expr_Alt_Rule, Generic_Instantiation_Rule,
      Library_Item_Rule, Exit_Stmt_Rule, Constrained_Subtype_Indication_Rule,
      Variant_Rule, Subtype_Name_Rule, Index_Constraint_Rule,
      Protected_Op_Rule, Loop_Stmt_Rule, Aggregate_Rule, Choice_Rule,
      Num_Literal_Rule, Quantified_Expr_Rule, Unconstrained_Index_Rule,
      Parent_List_Rule, Component_Item_Rule, Type_Expr_Rule, Subp_Decl_Rule,
      Component_List_Rule, Multidim_Array_Assoc_Rule, Access_Def_Rule,
      Null_Stmt_Rule, Number_Decl_Rule, Ordinary_Fixed_Point_Def_Rule,
      Basic_Decls_Rule) with
      Convention => C;
      --  Gramar rule to use for parsing.

   function Trace_Image (Self : Grammar_Rule) return String is (Self'Image);

   Default_Grammar_Rule : constant Grammar_Rule := Compilation_Rule;
   --  Default grammar rule to use when parsing analysis units

   type Lexer_Input_Kind is (File,
   --  Readable source file

Bytes_Buffer,
   --  Buffer of undecoded bytes

Text_Buffer
      --  Buffer of decoded bytes
      );
   --  Kind of lexer input

   subtype Undecoded_Lexer_Input is
     Lexer_Input_Kind range File .. Bytes_Buffer;

   type Token_Kind is
     (Ada_Termination, Ada_Lexing_Failure, Ada_Identifier, Ada_All, Ada_Abort,
      Ada_Else, Ada_New, Ada_Return, Ada_Abs, Ada_Elsif, Ada_Not, Ada_Reverse,
      Ada_End, Ada_Null, Ada_Accept, Ada_Entry, Ada_Select, Ada_Access,
      Ada_Exception, Ada_Of, Ada_Separate, Ada_Exit, Ada_Or, Ada_Others,
      Ada_Subtype, Ada_And, Ada_For, Ada_Out, Ada_Array, Ada_Function, Ada_At,
      Ada_Generic, Ada_Package, Ada_Task, Ada_Begin, Ada_Goto, Ada_Pragma,
      Ada_Terminate, Ada_Body, Ada_Private, Ada_Then, Ada_If, Ada_Procedure,
      Ada_Type, Ada_Case, Ada_In, Ada_Constant, Ada_Is, Ada_Raise, Ada_Use,
      Ada_Declare, Ada_Range, Ada_Delay, Ada_Limited, Ada_Record, Ada_When,
      Ada_Delta, Ada_Loop, Ada_Rem, Ada_While, Ada_Digits, Ada_Renames, Ada_Do,
      Ada_Mod, Ada_Xor, Ada_Par_Close, Ada_Par_Open, Ada_Semicolon, Ada_Colon,
      Ada_Comma, Ada_Doubledot, Ada_Dot, Ada_Diamond, Ada_Lte, Ada_Gte,
      Ada_Arrow, Ada_Equal, Ada_Lt, Ada_Gt, Ada_Plus, Ada_Minus, Ada_Power,
      Ada_Mult, Ada_Amp, Ada_Notequal, Ada_Divide, Ada_Tick, Ada_Pipe,
      Ada_Assign, Ada_Label_Start, Ada_Label_End, Ada_Target, Ada_String,
      Ada_Char, Ada_With, Ada_Decimal, Ada_Integer, Ada_Comment, Ada_Prep_Line,
      Ada_Whitespace);
   --  Kind of token: indentifier, string literal, ...

   type Token_Family is (Alphanumericals, Default_Family);
   --  Groups of token kinds, to make the processing of some groups of token
   --  uniform.

   Token_Kind_To_Family : array (Token_Kind) of Token_Family :=
     (Ada_Termination => Default_Family, Ada_Lexing_Failure => Default_Family,
      Ada_Identifier  => Alphanumericals, Ada_All => Alphanumericals,
      Ada_Abort       => Alphanumericals, Ada_Else => Alphanumericals,
      Ada_New         => Alphanumericals, Ada_Return => Alphanumericals,
      Ada_Abs         => Alphanumericals, Ada_Elsif => Alphanumericals,
      Ada_Not         => Alphanumericals, Ada_Reverse => Alphanumericals,
      Ada_End         => Alphanumericals, Ada_Null => Alphanumericals,
      Ada_Accept      => Alphanumericals, Ada_Entry => Alphanumericals,
      Ada_Select      => Alphanumericals, Ada_Access => Alphanumericals,
      Ada_Exception   => Alphanumericals, Ada_Of => Alphanumericals,
      Ada_Separate    => Alphanumericals, Ada_Exit => Alphanumericals,
      Ada_Or          => Alphanumericals, Ada_Others => Alphanumericals,
      Ada_Subtype     => Alphanumericals, Ada_And => Alphanumericals,
      Ada_For         => Alphanumericals, Ada_Out => Alphanumericals,
      Ada_Array       => Alphanumericals, Ada_Function => Alphanumericals,
      Ada_At          => Alphanumericals, Ada_Generic => Alphanumericals,
      Ada_Package     => Alphanumericals, Ada_Task => Alphanumericals,
      Ada_Begin       => Alphanumericals, Ada_Goto => Alphanumericals,
      Ada_Pragma      => Alphanumericals, Ada_Terminate => Alphanumericals,
      Ada_Body        => Alphanumericals, Ada_Private => Alphanumericals,
      Ada_Then        => Alphanumericals, Ada_If => Alphanumericals,
      Ada_Procedure   => Alphanumericals, Ada_Type => Alphanumericals,
      Ada_Case        => Alphanumericals, Ada_In => Alphanumericals,
      Ada_Constant    => Alphanumericals, Ada_Is => Alphanumericals,
      Ada_Raise       => Alphanumericals, Ada_Use => Alphanumericals,
      Ada_Declare     => Alphanumericals, Ada_Range => Alphanumericals,
      Ada_Delay       => Alphanumericals, Ada_Limited => Alphanumericals,
      Ada_Record      => Alphanumericals, Ada_When => Alphanumericals,
      Ada_Delta       => Alphanumericals, Ada_Loop => Alphanumericals,
      Ada_Rem         => Alphanumericals, Ada_While => Alphanumericals,
      Ada_Digits      => Alphanumericals, Ada_Renames => Alphanumericals,
      Ada_Do          => Alphanumericals, Ada_Mod => Alphanumericals,
      Ada_Xor         => Alphanumericals, Ada_Par_Close => Default_Family,
      Ada_Par_Open    => Default_Family, Ada_Semicolon => Default_Family,
      Ada_Colon       => Default_Family, Ada_Comma => Default_Family,
      Ada_Doubledot   => Default_Family, Ada_Dot => Default_Family,
      Ada_Diamond     => Default_Family, Ada_Lte => Default_Family,
      Ada_Gte         => Default_Family, Ada_Arrow => Default_Family,
      Ada_Equal       => Default_Family, Ada_Lt => Default_Family,
      Ada_Gt          => Default_Family, Ada_Plus => Default_Family,
      Ada_Minus       => Default_Family, Ada_Power => Default_Family,
      Ada_Mult        => Default_Family, Ada_Amp => Default_Family,
      Ada_Notequal    => Default_Family, Ada_Divide => Default_Family,
      Ada_Tick        => Default_Family, Ada_Pipe => Default_Family,
      Ada_Assign      => Default_Family, Ada_Label_Start => Default_Family,
      Ada_Label_End   => Default_Family, Ada_Target => Default_Family,
      Ada_String      => Default_Family, Ada_Char => Default_Family,
      Ada_With        => Alphanumericals, Ada_Decimal => Alphanumericals,
      Ada_Integer     => Alphanumericals, Ada_Comment => Default_Family,
      Ada_Prep_Line   => Default_Family, Ada_Whitespace => Default_Family);
   --  Associate a token family to all token kinds
   --
   --% document-value: False

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   --  Return a human-readable name for a token kind.

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type;
   --  Return the canonical literal corresponding to this token kind, or an
   --  empty string if this token has no literal.

   function Token_Error_Image (Token_Id : Token_Kind) return String;
   --  Return a string representation of ``Token_Id`` that is suitable in error
   --  messages.

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind with
      Inline;
   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind with
      Inline;

   function Is_Token_Node (Kind : Ada_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a token node

   function Is_List_Node (Kind : Ada_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a list node

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the
   --  ``Libadalang.Analysis.Traverse`` function.

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Reference is private;
   --  Reference to a token in an analysis unit.

   No_Token : constant Token_Reference;

   type Token_Data_Type is private;

   function "<" (Left, Right : Token_Reference) return Boolean;
   --  Assuming ``Left`` and ``Right`` belong to the same analysis unit, return
   --  whether ``Left`` came before ``Right`` in the source file.

   function Next
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference;
   --  Return a reference to the next token in the corresponding analysis unit.

   function Previous
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference;
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function Data (Token : Token_Reference) return Token_Data_Type;
   --  Return the data associated to ``Token``

   function Is_Equivalent (L, R : Token_Reference) return Boolean;
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   function Image (Token : Token_Reference) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Reference) return Text_Type;
   --  Return the text of the token as ``Text_Type``

   function Debug_Text (Token : Token_Reference) return String;
   --  Return the text of the token as ``String``

   function Text (First, Last : Token_Reference) return Text_Type;
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``.
   --
   --  This raises a ``Constraint_Error`` if ``First`` and ``Last`` don't
   --  belong to the same analysis unit.

   function Debug_Text (First, Last : Token_Reference) return String;
   --  Like ``Text``, to get the source buffer slice as a string

   function Get_Symbol (Token : Token_Reference) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

   function Kind (Token_Data : Token_Data_Type) return Token_Kind;
   --  Kind for this token.

   function Is_Trivia (Token : Token_Reference) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Index (Token : Token_Reference) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Index (Token_Data : Token_Data_Type) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   function Convert
     (TDH      : Token_Data_Handler; Token : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type;
   --  Turn data from ``TDH`` and ``Raw_Data`` into a user-ready token data
   --  record.

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the ``Child_Record`` type

   type Symbolization_Result (Success : Boolean; Size : Natural) is record
      case Success is
         when True =>
            Symbol : Text_Type (1 .. Size);
            --  Text for successfully symbolized identifiers

         when False =>
            Error_Message : Text_Type (1 .. Size);
            --  Message describing why symbolization failed
      end case;
   end record;
   --  Holder for results of the symbolization process, conditionned by whether
   --  this process was successful.

   function Create_Symbol (Name : Text_Type) return Symbolization_Result is
     ((Success => True, Size => Name'Length, Symbol => Name));
   --  Shortcut to create successful symbolization results

   function Create_Error (Message : Text_Type) return Symbolization_Result is
     ((Success => False, Size => Message'Length, Error_Message => Message));
   --  Shortcut to create failed symbolization results

   function Raw_Data (T : Token_Reference) return Stored_Token_Data;
   --  Return the raw token data for ``T``

   Invalid_Input : exception renames Langkit_Support.Errors.Invalid_Input;
   --  Raised by lexing functions (``Libadalang.Lexer``) when the input
   --  contains an invalid byte sequence.

   Invalid_Symbol_Error : exception renames
     Langkit_Support.Errors.Invalid_Symbol_Error;
   --  Exception raise when an invalid symbol is passed to a subprogram.

   Invalid_Unit_Name_Error : exception renames
     Langkit_Support.Errors.Invalid_Unit_Name_Error;
   --  Raised when an invalid unit name is provided.

   Native_Exception : exception renames
     Langkit_Support.Errors.Native_Exception;
   --  Exception raised in language bindings when the underlying C API reports
   --  an unexpected error that occurred in the library.
   --
   --  This kind of exception is raised for internal errors: they should never
   --  happen in normal situations and if they are raised at some point, it
   --  means the library state is potentially corrupted.
   --
   --  Nevertheless, the library does its best not to crash the program,
   --  materializing internal errors using this kind of exception.

   Precondition_Failure : exception renames
     Langkit_Support.Errors.Precondition_Failure;
   --  Exception raised when an API is called while its preconditions are not
   --  satisfied.

   Property_Error : exception renames Langkit_Support.Errors.Property_Error;
   --  Exception that is raised when an error occurs while evaluating any
   --  function whose name starts with ``P_``. This is the only exceptions
   --  that such functions can raise.

   Stale_Reference_Error : exception renames
     Langkit_Support.Errors.Stale_Reference_Error;
   --  Exception raised while trying to access data that was deallocated. This
   --  happens when one tries to use a node whose unit has been reparsed, for
   --  instance.

   Unknown_Charset : exception renames Langkit_Support.Errors.Unknown_Charset;
   --  Raised by lexing functions (``Libadalang.Lexer``) when the input charset
   --  is not supported.

   -------------------
   -- Introspection --
   -------------------

   Invalid_Field : exception renames
     Langkit_Support.Errors.Introspection.Invalid_Field;
   --  Raised when introspection functions (``Libadalang.Introspection``) are
   --  requested an invalid field.

   Node_Data_Evaluation_Error : exception renames
     Langkit_Support.Errors.Introspection.Node_Data_Evaluation_Error;
   --  Raised when introspection functions (``Libadalang.Introspection``) are
   --  improperly used to evaluate a node data (field or property).

   ---------------
   -- Rewriting --
   ---------------

   Template_Args_Error : exception renames
     Langkit_Support.Errors.Rewriting.Template_Args_Error;
   --  Exception raised when the provided arguments for a template don't match
   --  what the template expects.

   Template_Format_Error : exception renames
     Langkit_Support.Errors.Rewriting.Template_Format_Error;
   --  Exception raised when a template has an invalid syntax, such as badly
   --  formatted placeholders.

   Template_Instantiation_Error : exception renames
     Langkit_Support.Errors.Rewriting.Template_Instantiation_Error;
   --  Exception raised when the instantiation of a template cannot be parsed.

   -------------------
   -- Introspection --
   -------------------

   --  Unlike Ada_Node_Kind_Type, the following enumeration contains entries
   --  for abstract nodes.

   type Any_Node_Type_Id is
     (None, Ada_Node_Type_Id, Abort_Node_Type_Id, Abort_Absent_Type_Id,
      Abort_Present_Type_Id, Abstract_Node_Type_Id, Abstract_Absent_Type_Id,
      Abstract_Present_Type_Id, Ada_List_Type_Id, Ada_Node_List_Type_Id,
      Alternatives_List_Type_Id, Constraint_List_Type_Id, Decl_List_Type_Id,
      Stmt_List_Type_Id, Aspect_Assoc_List_Type_Id, Base_Assoc_List_Type_Id,
      Basic_Assoc_List_Type_Id, Assoc_List_Type_Id,
      Case_Expr_Alternative_List_Type_Id, Case_Stmt_Alternative_List_Type_Id,
      Compilation_Unit_List_Type_Id, Contract_Case_Assoc_List_Type_Id,
      Defining_Name_List_Type_Id, Discriminant_Spec_List_Type_Id,
      Elsif_Expr_Part_List_Type_Id, Elsif_Stmt_Part_List_Type_Id,
      Enum_Literal_Decl_List_Type_Id, Expr_List_Type_Id,
      Expr_Alternatives_List_Type_Id, Identifier_List_Type_Id,
      Discriminant_Choice_List_Type_Id, Name_List_Type_Id, Parent_List_Type_Id,
      Param_Spec_List_Type_Id, Pragma_Node_List_Type_Id,
      Select_When_Part_List_Type_Id, Unconstrained_Array_Index_List_Type_Id,
      Variant_List_Type_Id, Aliased_Node_Type_Id, Aliased_Absent_Type_Id,
      Aliased_Present_Type_Id, All_Node_Type_Id, All_Absent_Type_Id,
      All_Present_Type_Id, Array_Indices_Type_Id,
      Constrained_Array_Indices_Type_Id, Unconstrained_Array_Indices_Type_Id,
      Aspect_Assoc_Type_Id, Aspect_Clause_Type_Id, At_Clause_Type_Id,
      Attribute_Def_Clause_Type_Id, Enum_Rep_Clause_Type_Id,
      Record_Rep_Clause_Type_Id, Aspect_Spec_Type_Id, Base_Assoc_Type_Id,
      Contract_Case_Assoc_Type_Id, Pragma_Argument_Assoc_Type_Id,
      Base_Formal_Param_Holder_Type_Id, Base_Subp_Spec_Type_Id,
      Entry_Spec_Type_Id, Enum_Subp_Spec_Type_Id, Subp_Spec_Type_Id,
      Component_List_Type_Id, Discriminant_Part_Type_Id,
      Known_Discriminant_Part_Type_Id, Unknown_Discriminant_Part_Type_Id,
      Entry_Completion_Formal_Params_Type_Id, Generic_Formal_Part_Type_Id,
      Base_Record_Def_Type_Id, Null_Record_Def_Type_Id, Record_Def_Type_Id,
      Basic_Assoc_Type_Id, Aggregate_Assoc_Type_Id,
      Multi_Dim_Array_Assoc_Type_Id, Discriminant_Assoc_Type_Id,
      Param_Assoc_Type_Id, Basic_Decl_Type_Id, Base_Formal_Param_Decl_Type_Id,
      Component_Decl_Type_Id, Discriminant_Spec_Type_Id,
      Generic_Formal_Type_Id, Generic_Formal_Obj_Decl_Type_Id,
      Generic_Formal_Package_Type_Id, Generic_Formal_Subp_Decl_Type_Id,
      Generic_Formal_Type_Decl_Type_Id, Param_Spec_Type_Id,
      Base_Package_Decl_Type_Id, Generic_Package_Internal_Type_Id,
      Package_Decl_Type_Id, Base_Type_Decl_Type_Id, Base_Subtype_Decl_Type_Id,
      Discrete_Base_Subtype_Decl_Type_Id, Subtype_Decl_Type_Id,
      Classwide_Type_Decl_Type_Id, Incomplete_Type_Decl_Type_Id,
      Incomplete_Tagged_Type_Decl_Type_Id, Protected_Type_Decl_Type_Id,
      Task_Type_Decl_Type_Id, Single_Task_Type_Decl_Type_Id, Type_Decl_Type_Id,
      Anonymous_Type_Decl_Type_Id, Synth_Anonymous_Type_Decl_Type_Id,
      Basic_Subp_Decl_Type_Id, Classic_Subp_Decl_Type_Id,
      Abstract_Subp_Decl_Type_Id, Formal_Subp_Decl_Type_Id,
      Abstract_Formal_Subp_Decl_Type_Id, Concrete_Formal_Subp_Decl_Type_Id,
      Subp_Decl_Type_Id, Entry_Decl_Type_Id, Enum_Literal_Decl_Type_Id,
      Generic_Subp_Internal_Type_Id, Body_Node_Type_Id, Base_Subp_Body_Type_Id,
      Expr_Function_Type_Id, Null_Subp_Decl_Type_Id, Subp_Body_Type_Id,
      Subp_Renaming_Decl_Type_Id, Body_Stub_Type_Id, Package_Body_Stub_Type_Id,
      Protected_Body_Stub_Type_Id, Subp_Body_Stub_Type_Id,
      Task_Body_Stub_Type_Id, Entry_Body_Type_Id, Package_Body_Type_Id,
      Protected_Body_Type_Id, Task_Body_Type_Id, Entry_Index_Spec_Type_Id,
      Error_Decl_Type_Id, Exception_Decl_Type_Id, Exception_Handler_Type_Id,
      For_Loop_Var_Decl_Type_Id, Generic_Decl_Type_Id,
      Generic_Package_Decl_Type_Id, Generic_Subp_Decl_Type_Id,
      Generic_Instantiation_Type_Id, Generic_Package_Instantiation_Type_Id,
      Generic_Subp_Instantiation_Type_Id, Generic_Renaming_Decl_Type_Id,
      Generic_Package_Renaming_Decl_Type_Id,
      Generic_Subp_Renaming_Decl_Type_Id, Label_Decl_Type_Id,
      Named_Stmt_Decl_Type_Id, Number_Decl_Type_Id, Object_Decl_Type_Id,
      Anonymous_Object_Decl_Type_Id, Extended_Return_Stmt_Object_Decl_Type_Id,
      Package_Renaming_Decl_Type_Id, Single_Protected_Decl_Type_Id,
      Single_Task_Decl_Type_Id, Case_Stmt_Alternative_Type_Id,
      Compilation_Unit_Type_Id, Component_Clause_Type_Id,
      Component_Def_Type_Id, Constant_Node_Type_Id, Constant_Absent_Type_Id,
      Constant_Present_Type_Id, Constraint_Type_Id, Delta_Constraint_Type_Id,
      Digits_Constraint_Type_Id, Discriminant_Constraint_Type_Id,
      Index_Constraint_Type_Id, Range_Constraint_Type_Id,
      Declarative_Part_Type_Id, Private_Part_Type_Id, Public_Part_Type_Id,
      Elsif_Expr_Part_Type_Id, Elsif_Stmt_Part_Type_Id, Expr_Type_Id,
      Allocator_Type_Id, Base_Aggregate_Type_Id, Aggregate_Type_Id,
      Null_Record_Aggregate_Type_Id, Bin_Op_Type_Id, Relation_Op_Type_Id,
      Box_Expr_Type_Id, Case_Expr_Type_Id, Case_Expr_Alternative_Type_Id,
      Contract_Cases_Type_Id, If_Expr_Type_Id, Membership_Expr_Type_Id,
      Name_Type_Id, Attribute_Ref_Type_Id, Update_Attribute_Ref_Type_Id,
      Call_Expr_Type_Id, Defining_Name_Type_Id, Discrete_Subtype_Name_Type_Id,
      Dotted_Name_Type_Id, End_Name_Type_Id, Explicit_Deref_Type_Id,
      Qual_Expr_Type_Id, Single_Tok_Node_Type_Id, Base_Id_Type_Id,
      Char_Literal_Type_Id, Identifier_Type_Id, Op_Type_Id, Op_Abs_Type_Id,
      Op_And_Type_Id, Op_And_Then_Type_Id, Op_Concat_Type_Id, Op_Div_Type_Id,
      Op_Double_Dot_Type_Id, Op_Eq_Type_Id, Op_Gt_Type_Id, Op_Gte_Type_Id,
      Op_In_Type_Id, Op_Lt_Type_Id, Op_Lte_Type_Id, Op_Minus_Type_Id,
      Op_Mod_Type_Id, Op_Mult_Type_Id, Op_Neq_Type_Id, Op_Not_Type_Id,
      Op_Not_In_Type_Id, Op_Or_Type_Id, Op_Or_Else_Type_Id, Op_Plus_Type_Id,
      Op_Pow_Type_Id, Op_Rem_Type_Id, Op_Xor_Type_Id, String_Literal_Type_Id,
      Null_Literal_Type_Id, Num_Literal_Type_Id, Int_Literal_Type_Id,
      Real_Literal_Type_Id, Target_Name_Type_Id, Paren_Expr_Type_Id,
      Quantified_Expr_Type_Id, Raise_Expr_Type_Id, Un_Op_Type_Id,
      Handled_Stmts_Type_Id, Interface_Kind_Type_Id,
      Interface_Kind_Limited_Type_Id, Interface_Kind_Protected_Type_Id,
      Interface_Kind_Synchronized_Type_Id, Interface_Kind_Task_Type_Id,
      Iter_Type_Type_Id, Iter_Type_In_Type_Id, Iter_Type_Of_Type_Id,
      Library_Item_Type_Id, Limited_Node_Type_Id, Limited_Absent_Type_Id,
      Limited_Present_Type_Id, Loop_Spec_Type_Id, For_Loop_Spec_Type_Id,
      While_Loop_Spec_Type_Id, Mode_Type_Id, Mode_Default_Type_Id,
      Mode_In_Type_Id, Mode_In_Out_Type_Id, Mode_Out_Type_Id, Not_Null_Type_Id,
      Not_Null_Absent_Type_Id, Not_Null_Present_Type_Id,
      Null_Component_Decl_Type_Id, Others_Designator_Type_Id,
      Overriding_Node_Type_Id, Overriding_Not_Overriding_Type_Id,
      Overriding_Overriding_Type_Id, Overriding_Unspecified_Type_Id,
      Params_Type_Id, Pragma_Node_Type_Id, Prim_Type_Accessor_Type_Id,
      Private_Node_Type_Id, Private_Absent_Type_Id, Private_Present_Type_Id,
      Protected_Def_Type_Id, Protected_Node_Type_Id, Protected_Absent_Type_Id,
      Protected_Present_Type_Id, Quantifier_Type_Id, Quantifier_All_Type_Id,
      Quantifier_Some_Type_Id, Range_Spec_Type_Id, Renaming_Clause_Type_Id,
      Synthetic_Renaming_Clause_Type_Id, Reverse_Node_Type_Id,
      Reverse_Absent_Type_Id, Reverse_Present_Type_Id,
      Select_When_Part_Type_Id, Stmt_Type_Id, Composite_Stmt_Type_Id,
      Accept_Stmt_Type_Id, Accept_Stmt_With_Stmts_Type_Id,
      Base_Loop_Stmt_Type_Id, For_Loop_Stmt_Type_Id, Loop_Stmt_Type_Id,
      While_Loop_Stmt_Type_Id, Block_Stmt_Type_Id, Begin_Block_Type_Id,
      Decl_Block_Type_Id, Case_Stmt_Type_Id, Extended_Return_Stmt_Type_Id,
      If_Stmt_Type_Id, Named_Stmt_Type_Id, Select_Stmt_Type_Id,
      Error_Stmt_Type_Id, Simple_Stmt_Type_Id, Abort_Stmt_Type_Id,
      Assign_Stmt_Type_Id, Call_Stmt_Type_Id, Delay_Stmt_Type_Id,
      Exit_Stmt_Type_Id, Goto_Stmt_Type_Id, Label_Type_Id, Null_Stmt_Type_Id,
      Raise_Stmt_Type_Id, Requeue_Stmt_Type_Id, Return_Stmt_Type_Id,
      Terminate_Alternative_Type_Id, Subp_Kind_Type_Id,
      Subp_Kind_Function_Type_Id, Subp_Kind_Procedure_Type_Id, Subunit_Type_Id,
      Synchronized_Node_Type_Id, Synchronized_Absent_Type_Id,
      Synchronized_Present_Type_Id, Tagged_Node_Type_Id, Tagged_Absent_Type_Id,
      Tagged_Present_Type_Id, Task_Def_Type_Id, Type_Def_Type_Id,
      Access_Def_Type_Id, Access_To_Subp_Def_Type_Id,
      Base_Type_Access_Def_Type_Id, Anonymous_Type_Access_Def_Type_Id,
      Type_Access_Def_Type_Id, Array_Type_Def_Type_Id,
      Derived_Type_Def_Type_Id, Enum_Type_Def_Type_Id,
      Formal_Discrete_Type_Def_Type_Id, Interface_Type_Def_Type_Id,
      Mod_Int_Type_Def_Type_Id, Private_Type_Def_Type_Id,
      Real_Type_Def_Type_Id, Decimal_Fixed_Point_Def_Type_Id,
      Floating_Point_Def_Type_Id, Ordinary_Fixed_Point_Def_Type_Id,
      Record_Type_Def_Type_Id, Signed_Int_Type_Def_Type_Id, Type_Expr_Type_Id,
      Anonymous_Type_Type_Id, Enum_Lit_Synth_Type_Expr_Type_Id,
      Subtype_Indication_Type_Id, Constrained_Subtype_Indication_Type_Id,
      Discrete_Subtype_Indication_Type_Id, Unconstrained_Array_Index_Type_Id,
      Until_Node_Type_Id, Until_Absent_Type_Id, Until_Present_Type_Id,
      Use_Clause_Type_Id, Use_Package_Clause_Type_Id, Use_Type_Clause_Type_Id,
      Variant_Type_Id, Variant_Part_Type_Id, With_Clause_Type_Id,
      With_Private_Type_Id, With_Private_Absent_Type_Id,
      With_Private_Present_Type_Id);

   subtype Node_Type_Id is
     Any_Node_Type_Id range Ada_Node_Type_Id .. With_Private_Present_Type_Id;

   type Node_Type_Id_Array is array (Positive range <>) of Node_Type_Id;

   type Any_Value_Kind is
     (None, Boolean_Value, Integer_Value, Big_Integer_Value, Character_Value,
      Token_Value, Unbounded_Text_Value, Analysis_Unit_Value, Node_Value
,
      Analysis_Unit_Kind_Value, Lookup_Kind_Value, Find_All_Mode_Value,
      Ref_Result_Kind_Value, Grammar_Rule_Value
, Text_Type_Value, Aspect_Value,
      Completion_Item_Value, Completion_Item_Array_Value, Discrete_Range_Value,
      Doc_Annotation_Value, Doc_Annotation_Array_Value, Ada_Node_Array_Value,
      Base_Formal_Param_Decl_Array_Value, Base_Type_Decl_Array_Value,
      Basic_Decl_Array_Value, Compilation_Unit_Array_Value,
      Defining_Name_Array_Value, Generic_Instantiation_Array_Value,
      Param_Spec_Array_Value, Type_Decl_Array_Value, Param_Actual_Value,
      Param_Actual_Array_Value, Ref_Result_Value, Ref_Result_Array_Value,
      Refd_Decl_Value, Refd_Def_Value, Substitution_Value,
      Substitution_Array_Value, Analysis_Unit_Array_Value,
      Unbounded_Text_Type_Array_Value);
   subtype Value_Kind is
     Any_Value_Kind range Boolean_Value .. Any_Value_Kind'Last;
   --  Enumeration for all types used to interact with properties

   type Value_Constraint (Kind : Value_Kind := Value_Kind'First) is record
      case Kind is
         when Node_Value =>
            Node_Type : Node_Type_Id;
            --  Base type for nodes that satisfy this constraint

         when others =>
            null;
      end case;
   end record;
   --  Constraint for a polymorphic value

   type Value_Constraint_Array is
     array (Positive range <>) of Value_Constraint;

   type Any_Node_Data_Reference is
     (None, Constrained_Array_Indices_F_List,
      Unconstrained_Array_Indices_F_Types, Aspect_Assoc_F_Id,
      Aspect_Assoc_F_Expr, At_Clause_F_Name, At_Clause_F_Expr,
      Attribute_Def_Clause_F_Attribute_Expr, Attribute_Def_Clause_F_Expr,
      Enum_Rep_Clause_F_Type_Name, Enum_Rep_Clause_F_Aggregate,
      Record_Rep_Clause_F_Name, Record_Rep_Clause_F_At_Expr,
      Record_Rep_Clause_F_Components, Aspect_Spec_F_Aspect_Assocs,
      Contract_Case_Assoc_F_Guard, Contract_Case_Assoc_F_Consequence,
      Pragma_Argument_Assoc_F_Id, Pragma_Argument_Assoc_F_Expr,
      Entry_Spec_F_Entry_Name, Entry_Spec_F_Family_Type,
      Entry_Spec_F_Entry_Params, Subp_Spec_F_Subp_Kind, Subp_Spec_F_Subp_Name,
      Subp_Spec_F_Subp_Params, Subp_Spec_F_Subp_Returns,
      Component_List_F_Components, Component_List_F_Variant_Part,
      Known_Discriminant_Part_F_Discr_Specs,
      Entry_Completion_Formal_Params_F_Params, Generic_Formal_Part_F_Decls,
      Base_Record_Def_F_Components, Aggregate_Assoc_F_Designators,
      Aggregate_Assoc_F_R_Expr, Discriminant_Assoc_F_Ids,
      Discriminant_Assoc_F_Discr_Expr, Param_Assoc_F_Designator,
      Param_Assoc_F_R_Expr, Basic_Decl_F_Aspects, Component_Decl_F_Ids,
      Component_Decl_F_Component_Def, Component_Decl_F_Default_Expr,
      Discriminant_Spec_F_Ids, Discriminant_Spec_F_Type_Expr,
      Discriminant_Spec_F_Default_Expr, Generic_Formal_F_Decl,
      Param_Spec_F_Ids, Param_Spec_F_Has_Aliased, Param_Spec_F_Mode,
      Param_Spec_F_Type_Expr, Param_Spec_F_Default_Expr,
      Base_Package_Decl_F_Package_Name, Base_Package_Decl_F_Public_Part,
      Base_Package_Decl_F_Private_Part, Base_Package_Decl_F_End_Name,
      Base_Type_Decl_F_Name, Subtype_Decl_F_Subtype,
      Incomplete_Type_Decl_F_Discriminants,
      Incomplete_Tagged_Type_Decl_F_Has_Abstract,
      Protected_Type_Decl_F_Discriminants, Protected_Type_Decl_F_Interfaces,
      Protected_Type_Decl_F_Definition, Task_Type_Decl_F_Discriminants,
      Task_Type_Decl_F_Definition, Type_Decl_F_Discriminants,
      Type_Decl_F_Type_Def, Classic_Subp_Decl_F_Overriding,
      Classic_Subp_Decl_F_Subp_Spec, Formal_Subp_Decl_F_Default_Expr,
      Entry_Decl_F_Overriding, Entry_Decl_F_Spec, Enum_Literal_Decl_F_Name,
      Generic_Subp_Internal_F_Subp_Spec, Base_Subp_Body_F_Overriding,
      Base_Subp_Body_F_Subp_Spec, Expr_Function_F_Expr, Subp_Body_F_Decls,
      Subp_Body_F_Stmts, Subp_Body_F_End_Name, Subp_Renaming_Decl_F_Renames,
      Package_Body_Stub_F_Name, Protected_Body_Stub_F_Name,
      Subp_Body_Stub_F_Overriding, Subp_Body_Stub_F_Subp_Spec,
      Task_Body_Stub_F_Name, Entry_Body_F_Entry_Name, Entry_Body_F_Index_Spec,
      Entry_Body_F_Params, Entry_Body_F_Barrier, Entry_Body_F_Decls,
      Entry_Body_F_Stmts, Entry_Body_F_End_Name, Package_Body_F_Package_Name,
      Package_Body_F_Decls, Package_Body_F_Stmts, Package_Body_F_End_Name,
      Protected_Body_F_Name, Protected_Body_F_Decls, Protected_Body_F_End_Name,
      Task_Body_F_Name, Task_Body_F_Decls, Task_Body_F_Stmts,
      Task_Body_F_End_Name, Entry_Index_Spec_F_Id, Entry_Index_Spec_F_Subtype,
      Exception_Decl_F_Ids, Exception_Decl_F_Renames,
      Exception_Handler_F_Exception_Name,
      Exception_Handler_F_Handled_Exceptions, Exception_Handler_F_Stmts,
      For_Loop_Var_Decl_F_Id, For_Loop_Var_Decl_F_Id_Type,
      Generic_Decl_F_Formal_Part, Generic_Package_Decl_F_Package_Decl,
      Generic_Subp_Decl_F_Subp_Decl, Generic_Package_Instantiation_F_Name,
      Generic_Package_Instantiation_F_Generic_Pkg_Name,
      Generic_Package_Instantiation_F_Params,
      Generic_Subp_Instantiation_F_Overriding,
      Generic_Subp_Instantiation_F_Kind,
      Generic_Subp_Instantiation_F_Subp_Name,
      Generic_Subp_Instantiation_F_Generic_Subp_Name,
      Generic_Subp_Instantiation_F_Params,
      Generic_Package_Renaming_Decl_F_Name,
      Generic_Package_Renaming_Decl_F_Renames,
      Generic_Subp_Renaming_Decl_F_Kind, Generic_Subp_Renaming_Decl_F_Name,
      Generic_Subp_Renaming_Decl_F_Renames, Label_Decl_F_Name,
      Named_Stmt_Decl_F_Name, Number_Decl_F_Ids, Number_Decl_F_Expr,
      Object_Decl_F_Ids, Object_Decl_F_Has_Aliased, Object_Decl_F_Has_Constant,
      Object_Decl_F_Mode, Object_Decl_F_Type_Expr, Object_Decl_F_Default_Expr,
      Object_Decl_F_Renaming_Clause, Package_Renaming_Decl_F_Name,
      Package_Renaming_Decl_F_Renames, Single_Protected_Decl_F_Name,
      Single_Protected_Decl_F_Interfaces, Single_Protected_Decl_F_Definition,
      Single_Task_Decl_F_Task_Type, Case_Stmt_Alternative_F_Choices,
      Case_Stmt_Alternative_F_Stmts, Compilation_Unit_F_Prelude,
      Compilation_Unit_F_Body, Compilation_Unit_F_Pragmas,
      Component_Clause_F_Id, Component_Clause_F_Position,
      Component_Clause_F_Range, Component_Def_F_Has_Aliased,
      Component_Def_F_Has_Constant, Component_Def_F_Type_Expr,
      Delta_Constraint_F_Digits, Delta_Constraint_F_Range,
      Digits_Constraint_F_Digits, Digits_Constraint_F_Range,
      Discriminant_Constraint_F_Constraints, Index_Constraint_F_Constraints,
      Range_Constraint_F_Range, Declarative_Part_F_Decls,
      Elsif_Expr_Part_F_Cond_Expr, Elsif_Expr_Part_F_Then_Expr,
      Elsif_Stmt_Part_F_Cond_Expr, Elsif_Stmt_Part_F_Stmts,
      Allocator_F_Subpool, Allocator_F_Type_Or_Expr,
      Base_Aggregate_F_Ancestor_Expr, Base_Aggregate_F_Assocs, Bin_Op_F_Left,
      Bin_Op_F_Op, Bin_Op_F_Right, Case_Expr_F_Expr, Case_Expr_F_Cases,
      Case_Expr_Alternative_F_Choices, Case_Expr_Alternative_F_Expr,
      Contract_Cases_F_Contract_Cases, If_Expr_F_Cond_Expr,
      If_Expr_F_Then_Expr, If_Expr_F_Alternatives, If_Expr_F_Else_Expr,
      Membership_Expr_F_Expr, Membership_Expr_F_Op,
      Membership_Expr_F_Membership_Exprs, Attribute_Ref_F_Prefix,
      Attribute_Ref_F_Attribute, Attribute_Ref_F_Args, Call_Expr_F_Name,
      Call_Expr_F_Suffix, Defining_Name_F_Name,
      Discrete_Subtype_Name_F_Subtype, Dotted_Name_F_Prefix,
      Dotted_Name_F_Suffix, End_Name_F_Name, Explicit_Deref_F_Prefix,
      Qual_Expr_F_Prefix, Qual_Expr_F_Suffix, Paren_Expr_F_Expr,
      Quantified_Expr_F_Quantifier, Quantified_Expr_F_Loop_Spec,
      Quantified_Expr_F_Expr, Raise_Expr_F_Exception_Name,
      Raise_Expr_F_Error_Message, Un_Op_F_Op, Un_Op_F_Expr,
      Handled_Stmts_F_Stmts, Handled_Stmts_F_Exceptions,
      Library_Item_F_Has_Private, Library_Item_F_Item,
      For_Loop_Spec_F_Var_Decl, For_Loop_Spec_F_Loop_Type,
      For_Loop_Spec_F_Has_Reverse, For_Loop_Spec_F_Iter_Expr,
      While_Loop_Spec_F_Expr, Params_F_Params, Pragma_Node_F_Id,
      Pragma_Node_F_Args, Protected_Def_F_Public_Part,
      Protected_Def_F_Private_Part, Protected_Def_F_End_Name,
      Range_Spec_F_Range, Renaming_Clause_F_Renamed_Object,
      Select_When_Part_F_Cond_Expr, Select_When_Part_F_Stmts,
      Accept_Stmt_F_Name, Accept_Stmt_F_Entry_Index_Expr, Accept_Stmt_F_Params,
      Accept_Stmt_With_Stmts_F_Stmts, Accept_Stmt_With_Stmts_F_End_Name,
      Base_Loop_Stmt_F_Spec, Base_Loop_Stmt_F_Stmts, Base_Loop_Stmt_F_End_Name,
      Begin_Block_F_Stmts, Begin_Block_F_End_Name, Decl_Block_F_Decls,
      Decl_Block_F_Stmts, Decl_Block_F_End_Name, Case_Stmt_F_Expr,
      Case_Stmt_F_Alternatives, Extended_Return_Stmt_F_Decl,
      Extended_Return_Stmt_F_Stmts, If_Stmt_F_Cond_Expr, If_Stmt_F_Then_Stmts,
      If_Stmt_F_Alternatives, If_Stmt_F_Else_Stmts, Named_Stmt_F_Decl,
      Named_Stmt_F_Stmt, Select_Stmt_F_Guards, Select_Stmt_F_Else_Stmts,
      Select_Stmt_F_Abort_Stmts, Abort_Stmt_F_Names, Assign_Stmt_F_Dest,
      Assign_Stmt_F_Expr, Call_Stmt_F_Call, Delay_Stmt_F_Has_Until,
      Delay_Stmt_F_Expr, Exit_Stmt_F_Loop_Name, Exit_Stmt_F_Cond_Expr,
      Goto_Stmt_F_Label_Name, Label_F_Decl, Raise_Stmt_F_Exception_Name,
      Raise_Stmt_F_Error_Message, Requeue_Stmt_F_Call_Name,
      Requeue_Stmt_F_Has_Abort, Return_Stmt_F_Return_Expr, Subunit_F_Name,
      Subunit_F_Body, Task_Def_F_Interfaces, Task_Def_F_Public_Part,
      Task_Def_F_Private_Part, Task_Def_F_End_Name, Access_Def_F_Has_Not_Null,
      Access_To_Subp_Def_F_Has_Protected, Access_To_Subp_Def_F_Subp_Spec,
      Anonymous_Type_Access_Def_F_Type_Decl, Type_Access_Def_F_Has_All,
      Type_Access_Def_F_Has_Constant, Type_Access_Def_F_Subtype_Indication,
      Array_Type_Def_F_Indices, Array_Type_Def_F_Component_Type,
      Derived_Type_Def_F_Has_Abstract, Derived_Type_Def_F_Has_Limited,
      Derived_Type_Def_F_Has_Synchronized,
      Derived_Type_Def_F_Subtype_Indication, Derived_Type_Def_F_Interfaces,
      Derived_Type_Def_F_Record_Extension, Derived_Type_Def_F_Has_With_Private,
      Enum_Type_Def_F_Enum_Literals, Interface_Type_Def_F_Interface_Kind,
      Interface_Type_Def_F_Interfaces, Mod_Int_Type_Def_F_Expr,
      Private_Type_Def_F_Has_Abstract, Private_Type_Def_F_Has_Tagged,
      Private_Type_Def_F_Has_Limited, Decimal_Fixed_Point_Def_F_Delta,
      Decimal_Fixed_Point_Def_F_Digits, Decimal_Fixed_Point_Def_F_Range,
      Floating_Point_Def_F_Num_Digits, Floating_Point_Def_F_Range,
      Ordinary_Fixed_Point_Def_F_Delta, Ordinary_Fixed_Point_Def_F_Range,
      Record_Type_Def_F_Has_Abstract, Record_Type_Def_F_Has_Tagged,
      Record_Type_Def_F_Has_Limited, Record_Type_Def_F_Record_Def,
      Signed_Int_Type_Def_F_Range, Anonymous_Type_F_Type_Decl,
      Subtype_Indication_F_Has_Not_Null, Subtype_Indication_F_Name,
      Subtype_Indication_F_Constraint,
      Unconstrained_Array_Index_F_Subtype_Indication,
      Use_Package_Clause_F_Packages, Use_Type_Clause_F_Has_All,
      Use_Type_Clause_F_Types, Variant_F_Choices, Variant_F_Components,
      Variant_Part_F_Discr_Name, Variant_Part_F_Variant,
      With_Clause_F_Has_Limited, With_Clause_F_Has_Private,
      With_Clause_F_Packages, Ada_Node_P_Declarative_Scope,
      Ada_Node_P_Complete, Ada_Node_P_Valid_Keywords,
      Ada_Node_P_Generic_Instantiations, Ada_Node_P_Semantic_Parent,
      Ada_Node_P_Parent_Basic_Decl, Ada_Node_P_Filter_Is_Imported_By,
      Ada_Node_P_Xref_Entry_Point, Ada_Node_P_Resolve_Names,
      Ada_Node_P_Standard_Unit, Ada_Node_P_Std_Entity, Ada_Node_P_Bool_Type,
      Ada_Node_P_Int_Type, Ada_Node_P_Universal_Int_Type,
      Ada_Node_P_Universal_Real_Type, Ada_Node_P_Top_Level_Decl,
      Ada_Node_P_Choice_Match, Ada_Node_P_Gnat_Xref, Ada_Node_Parent,
      Ada_Node_Parents, Ada_Node_Children, Ada_Node_Token_Start,
      Ada_Node_Token_End, Ada_Node_Child_Index, Ada_Node_Previous_Sibling,
      Ada_Node_Next_Sibling, Ada_Node_Unit, Ada_Node_Is_Ghost,
      Ada_Node_Full_Sloc_Image, Abort_Node_P_As_Bool, Abstract_Node_P_As_Bool,
      Assoc_List_P_Zip_With_Params, Aliased_Node_P_As_Bool, All_Node_P_As_Bool,
      Base_Assoc_P_Assoc_Expr,
      Base_Formal_Param_Holder_P_Abstract_Formal_Params,
      Base_Formal_Param_Holder_P_Nb_Min_Params,
      Base_Formal_Param_Holder_P_Nb_Max_Params,
      Base_Formal_Param_Holder_P_Param_Types, Base_Subp_Spec_P_Returns,
      Base_Subp_Spec_P_Params, Base_Subp_Spec_P_Primitive_Subp_Types,
      Base_Subp_Spec_P_Primitive_Subp_First_Type,
      Base_Subp_Spec_P_Primitive_Subp_Tagged_Type,
      Base_Subp_Spec_P_Return_Type, Basic_Assoc_P_Get_Params,
      Basic_Decl_P_Is_Formal, Basic_Decl_P_Doc_Annotations, Basic_Decl_P_Doc,
      Basic_Decl_P_Previous_Part_For_Decl, Basic_Decl_P_Canonical_Part,
      Basic_Decl_P_Is_Static_Decl, Basic_Decl_P_Is_Imported,
      Basic_Decl_P_Get_Aspect_Assoc, Basic_Decl_P_Get_Aspect_Spec_Expr,
      Basic_Decl_P_Get_Aspect, Basic_Decl_P_Has_Aspect,
      Basic_Decl_P_Get_Pragma, Basic_Decl_P_Get_Representation_Clause,
      Basic_Decl_P_Is_Compilation_Unit_Root, Basic_Decl_P_Is_Visible,
      Basic_Decl_P_Base_Subp_Declarations, Basic_Decl_P_Root_Subp_Declarations,
      Basic_Decl_P_Find_All_Overrides, Basic_Decl_P_Defining_Names,
      Basic_Decl_P_Defining_Name, Basic_Decl_P_Type_Expression,
      Basic_Decl_P_Subp_Spec_Or_Null, Basic_Decl_P_Is_Subprogram,
      Basic_Decl_P_Relative_Name, Basic_Decl_P_Relative_Name_Text,
      Basic_Decl_P_Next_Part_For_Decl, Basic_Decl_P_Body_Part_For_Decl,
      Basic_Decl_P_Fully_Qualified_Name_Array,
      Basic_Decl_P_Fully_Qualified_Name,
      Basic_Decl_P_Canonical_Fully_Qualified_Name,
      Basic_Decl_P_Unique_Identifying_Name,
      Base_Formal_Param_Decl_P_Formal_Type, Base_Package_Decl_P_Body_Part,
      Base_Type_Decl_P_Base_Subtype, Base_Type_Decl_P_Private_Completion,
      Base_Type_Decl_P_Get_Record_Representation_Clause,
      Base_Type_Decl_P_Get_Enum_Representation_Clause,
      Base_Type_Decl_P_Is_Record_Type, Base_Type_Decl_P_Is_Array_Type,
      Base_Type_Decl_P_Find_Derived_Types, Base_Type_Decl_P_Is_Real_Type,
      Base_Type_Decl_P_Is_Float_Type, Base_Type_Decl_P_Is_Fixed_Point,
      Base_Type_Decl_P_Is_Enum_Type, Base_Type_Decl_P_Is_Access_Type,
      Base_Type_Decl_P_Is_Char_Type, Base_Type_Decl_P_Discrete_Range,
      Base_Type_Decl_P_Is_Discrete_Type, Base_Type_Decl_P_Is_Int_Type,
      Base_Type_Decl_P_Accessed_Type, Base_Type_Decl_P_Is_Tagged_Type,
      Base_Type_Decl_P_Base_Type, Base_Type_Decl_P_Base_Types,
      Base_Type_Decl_P_Find_All_Derived_Types, Base_Type_Decl_P_Comp_Type,
      Base_Type_Decl_P_Index_Type, Base_Type_Decl_P_Is_Derived_Type,
      Base_Type_Decl_P_Is_Interface_Type, Base_Type_Decl_P_Matching_Type,
      Base_Type_Decl_P_Canonical_Type, Base_Type_Decl_P_Previous_Part,
      Base_Type_Decl_P_Next_Part, Base_Type_Decl_P_Full_View,
      Base_Type_Decl_P_Is_Definite_Subtype, Base_Type_Decl_P_Is_Private,
      Base_Type_Decl_P_Root_Type, Type_Decl_P_Get_Primitives,
      Basic_Subp_Decl_P_Subp_Decl_Spec, Basic_Subp_Decl_P_Body_Part,
      Enum_Literal_Decl_P_Enum_Type, Body_Node_P_Previous_Part,
      Body_Node_P_Decl_Part, Body_Node_P_Subunit_Root,
      Generic_Package_Decl_P_Body_Part, Generic_Subp_Decl_P_Body_Part,
      Generic_Instantiation_P_Designated_Generic_Decl,
      Generic_Subp_Instantiation_P_Designated_Subp,
      Object_Decl_P_Public_Part_Decl, Package_Renaming_Decl_P_Renamed_Package,
      Package_Renaming_Decl_P_Final_Renamed_Package,
      Compilation_Unit_P_Syntactic_Fully_Qualified_Name,
      Compilation_Unit_P_Unit_Kind, Compilation_Unit_P_Withed_Units,
      Compilation_Unit_P_Imported_Units, Compilation_Unit_P_Unit_Dependencies,
      Compilation_Unit_P_Decl, Compilation_Unit_P_Is_Preelaborable,
      Constant_Node_P_As_Bool, Expr_P_Expression_Type, Expr_P_Is_Static_Expr,
      Expr_P_First_Corresponding_Decl, Expr_P_Eval_As_Int,
      Expr_P_Eval_As_Int_In_Env, Expr_P_Matching_Nodes,
      Allocator_P_Get_Allocated_Type, Name_P_Enclosing_Defining_Name,
      Name_P_Is_Defining, Name_P_Name_Is, Name_P_Is_Direct_Call,
      Name_P_Is_Access_Call, Name_P_Is_Call, Name_P_Is_Dot_Call,
      Name_P_Failsafe_Referenced_Def_Name, Name_P_Referenced_Defining_Name,
      Name_P_All_Env_Elements, Name_P_Called_Subp_Spec, Name_P_Referenced_Decl,
      Name_P_Failsafe_Referenced_Decl, Name_P_Referenced_Decl_Internal,
      Name_P_Name_Designated_Type, Name_P_Is_Static_Subtype,
      Name_P_Name_Matches, Name_P_Relative_Name, Name_P_Is_Operator_Name,
      Name_P_Is_Write_Reference, Name_P_Is_Dispatching_Call,
      Name_P_Is_Static_Call, Name_P_As_Symbol_Array,
      Call_Expr_P_Is_Array_Slice, Defining_Name_P_Basic_Decl,
      Defining_Name_P_Find_Refs, Defining_Name_P_Find_All_References,
      Defining_Name_P_Find_All_Calls, Defining_Name_P_Next_Part,
      Defining_Name_P_Previous_Part, Defining_Name_P_Canonical_Part,
      End_Name_P_Basic_Decl, Single_Tok_Node_P_Canonical_Text,
      Char_Literal_P_Denoted_Value, String_Literal_P_Denoted_Value,
      Int_Literal_P_Denoted_Value, Limited_Node_P_As_Bool, Not_Null_P_As_Bool,
      Pragma_Node_P_Associated_Decls, Private_Node_P_As_Bool,
      Protected_Node_P_As_Bool, Reverse_Node_P_As_Bool, Subunit_P_Body_Root,
      Synchronized_Node_P_As_Bool, Tagged_Node_P_As_Bool,
      Type_Expr_P_Type_Name, Type_Expr_P_Designated_Type_Decl,
      Type_Expr_P_Designated_Type_Decl_From,
      Subtype_Indication_P_Is_Static_Subtype, Until_Node_P_As_Bool,
      With_Private_P_As_Bool);
   subtype Node_Data_Reference is
     Any_Node_Data_Reference range Constrained_Array_Indices_F_List ..
         With_Private_P_As_Bool;
   --  Enumeration of all data attached to nodes (syntax fields and properties)

   type Node_Data_Reference_Array is
     array (Positive range <>) of Node_Data_Reference;

   subtype Field_Reference is
     Node_Data_Reference range
Constrained_Array_Indices_F_List ..
         With_Clause_F_Packages;
   --  Enumeration of all syntax fields for regular nodes

   type Field_Reference_Array is array (Positive range <>) of Field_Reference;

   subtype Property_Reference is
     Node_Data_Reference range Ada_Node_P_Declarative_Scope ..
         With_Private_P_As_Bool;
   --  Enumeration of all available node properties

   type Property_Reference_Array is
     array (Positive range <>) of Property_Reference;

   type Language_Version is (Ada_83, Ada_95, Ada_2005, Ada_2012);
--  Enum representing a version of the Ada language

private

   type Token_Reference is record
      TDH : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Index : Token_Or_Trivia_Index;
      --  Identifier for the trivia or the token this refers to
   end record;

   No_Token : constant Token_Reference := (null, No_Token_Or_Trivia_Index);

   type Token_Data_Type is record
      Kind : Token_Kind;
      --  See documentation for the Kind accessor

      Is_Trivia : Boolean;
      --  See documentation for the Is_Trivia accessor

      Index : Token_Index;
      --  See documentation for the Index accessor

      Source_Buffer : Text_Cst_Access;
      --  Text for the original source file

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in Source_Buffer for the text corresponding to this token

      Sloc_Range : Source_Location_Range;
      --  See documenation for the Sloc_Range accessor
   end record;

end Libadalang.Common;
