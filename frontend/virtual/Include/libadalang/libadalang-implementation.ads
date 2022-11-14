with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

with GNATCOLL.Traces;
with GNATCOLL.GMP.Integers;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;

with Langkit_Support.Bump_Ptr; use Langkit_Support.Bump_Ptr;
with Langkit_Support.Cheap_Sets;
with Langkit_Support.Lexical_Env;
with Langkit_Support.Types;    use Langkit_Support.Types;
with Langkit_Support.Vectors;

with Libadalang.Parsers; use Libadalang.Parsers;
with Libadalang.Common;  use Libadalang.Common;
use Libadalang.Common.Symbols;
use Libadalang.Common.Token_Data_Handlers;
with Libadalang.Lexer_Implementation; use Libadalang.Lexer_Implementation;

--  Internal package: low-level primitives to implement public types and
--  operations in Libadalang.Analysis.

private package Libadalang.Implementation is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   Unexpected_Call_Depth : exception;
   --  Raised when the Call_Depth for two matching calls to Enter_Call and
   --  Exit_Call don't match, i.e. when there is a bug in the counting of
   --  recursive calls.

   procedure Enter_Call
     (Context : Internal_Context; Call_Depth : access Natural);
   --  Increment the call depth in Context. If the depth exceeds Context's
   --  maximum, raise a Property_Error for "stack overflow".
   --
   --  Note that in the case of an exception, the depth is still incremented.
   --  This means that all calls to Enter_Call must be wrapped in an exception
   --  handler which calls Exit_Call on exception.
   --
   --  Put in Call_Depth the incremented call depth.

   procedure Exit_Call (Context : Internal_Context; Call_Depth : Natural);
   --  Decrement the call depth in Context. If Call_Depth does not match the
   --  current call depth, raise an Unexpected_Call_Depth.

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type Root_Node_Record;
   type Bare_Ada_Node is access all Root_Node_Record;
   No_Bare_Ada_Node : constant Bare_Ada_Node := null;
   --  Most generic AST node type

   function "<" (Left, Right : Bare_Ada_Node) return Boolean;
   --  Abritrary but deterministic ordering criteria for parsing nodes. This
   --  handles null nodes as well. Raise a Property_Error for synthetic nodes.

   function Is_Null (Node : Bare_Ada_Node) return Boolean;
   function Kind (Node : Bare_Ada_Node) return Ada_Node_Kind_Type;

   subtype Bare_Abort_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abort_Node)
        or else Kind (Bare_Abort_Node) in Ada_Abort_Node;
   subtype Bare_Abort_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abort_Absent)
        or else Kind (Bare_Abort_Absent) in Ada_Abort_Absent_Range;
   subtype Bare_Abort_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abort_Present)
        or else Kind (Bare_Abort_Present) in Ada_Abort_Present_Range;
   subtype Bare_Abstract_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abstract_Node)
        or else Kind (Bare_Abstract_Node) in Ada_Abstract_Node;
   subtype Bare_Abstract_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abstract_Absent)
        or else Kind (Bare_Abstract_Absent) in Ada_Abstract_Absent_Range;
   subtype Bare_Abstract_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abstract_Present)
        or else Kind (Bare_Abstract_Present) in Ada_Abstract_Present_Range;
   subtype Bare_Ada_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Ada_List)
        or else Kind (Bare_Ada_List) in Ada_Ada_List;
   subtype Bare_Ada_Node_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Ada_Node_List)
        or else Kind (Bare_Ada_Node_List) in Ada_Ada_Node_List_Range;
   subtype Bare_Alternatives_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Alternatives_List)
        or else Kind (Bare_Alternatives_List) in Ada_Alternatives_List_Range;
   subtype Bare_Constraint_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constraint_List)
        or else Kind (Bare_Constraint_List) in Ada_Constraint_List_Range;
   subtype Bare_Decl_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Decl_List)
        or else Kind (Bare_Decl_List) in Ada_Decl_List_Range;
   subtype Bare_Stmt_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Stmt_List)
        or else Kind (Bare_Stmt_List) in Ada_Stmt_List_Range;
   subtype Bare_Aspect_Assoc_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aspect_Assoc_List)
        or else Kind (Bare_Aspect_Assoc_List) in Ada_Aspect_Assoc_List_Range;
   subtype Bare_Base_Assoc_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Assoc_List)
        or else Kind (Bare_Base_Assoc_List) in Ada_Base_Assoc_List_Range;
   subtype Bare_Basic_Assoc_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Basic_Assoc_List)
        or else Kind (Bare_Basic_Assoc_List) in Ada_Basic_Assoc_List;
   subtype Bare_Assoc_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Assoc_List)
        or else Kind (Bare_Assoc_List) in Ada_Assoc_List_Range;
   subtype Bare_Case_Expr_Alternative_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Case_Expr_Alternative_List)
        or else Kind (Bare_Case_Expr_Alternative_List) in
          Ada_Case_Expr_Alternative_List_Range;
   subtype Bare_Case_Stmt_Alternative_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Case_Stmt_Alternative_List)
        or else Kind (Bare_Case_Stmt_Alternative_List) in
          Ada_Case_Stmt_Alternative_List_Range;
   subtype Bare_Compilation_Unit_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Compilation_Unit_List)
        or else Kind (Bare_Compilation_Unit_List) in
          Ada_Compilation_Unit_List_Range;
   subtype Bare_Contract_Case_Assoc_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Contract_Case_Assoc_List)
        or else Kind (Bare_Contract_Case_Assoc_List) in
          Ada_Contract_Case_Assoc_List_Range;
   subtype Bare_Defining_Name_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Defining_Name_List)
        or else Kind (Bare_Defining_Name_List) in Ada_Defining_Name_List_Range;
   subtype Bare_Discriminant_Spec_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discriminant_Spec_List)
        or else Kind (Bare_Discriminant_Spec_List) in
          Ada_Discriminant_Spec_List_Range;
   subtype Bare_Elsif_Expr_Part_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Elsif_Expr_Part_List)
        or else Kind (Bare_Elsif_Expr_Part_List) in
          Ada_Elsif_Expr_Part_List_Range;
   subtype Bare_Elsif_Stmt_Part_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Elsif_Stmt_Part_List)
        or else Kind (Bare_Elsif_Stmt_Part_List) in
          Ada_Elsif_Stmt_Part_List_Range;
   subtype Bare_Enum_Literal_Decl_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Enum_Literal_Decl_List)
        or else Kind (Bare_Enum_Literal_Decl_List) in
          Ada_Enum_Literal_Decl_List_Range;
   subtype Bare_Expr_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Expr_List)
        or else Kind (Bare_Expr_List) in Ada_Expr_List;
   subtype Bare_Expr_Alternatives_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Expr_Alternatives_List)
        or else Kind (Bare_Expr_Alternatives_List) in
          Ada_Expr_Alternatives_List_Range;
   subtype Bare_Identifier_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Identifier_List)
        or else Kind (Bare_Identifier_List) in Ada_Identifier_List;
   subtype Bare_Discriminant_Choice_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discriminant_Choice_List)
        or else Kind (Bare_Discriminant_Choice_List) in
          Ada_Discriminant_Choice_List_Range;
   subtype Bare_Name_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Name_List)
        or else Kind (Bare_Name_List) in Ada_Name_List_Range;
   subtype Bare_Parent_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Parent_List)
        or else Kind (Bare_Parent_List) in Ada_Parent_List_Range;
   subtype Bare_Param_Spec_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Param_Spec_List)
        or else Kind (Bare_Param_Spec_List) in Ada_Param_Spec_List_Range;
   subtype Bare_Pragma_Node_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Pragma_Node_List)
        or else Kind (Bare_Pragma_Node_List) in Ada_Pragma_Node_List_Range;
   subtype Bare_Select_When_Part_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Select_When_Part_List)
        or else Kind (Bare_Select_When_Part_List) in
          Ada_Select_When_Part_List_Range;
   subtype Bare_Unconstrained_Array_Index_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Unconstrained_Array_Index_List)
        or else Kind (Bare_Unconstrained_Array_Index_List) in
          Ada_Unconstrained_Array_Index_List_Range;
   subtype Bare_Variant_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Variant_List)
        or else Kind (Bare_Variant_List) in Ada_Variant_List_Range;
   subtype Bare_Aliased_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aliased_Node)
        or else Kind (Bare_Aliased_Node) in Ada_Aliased_Node;
   subtype Bare_Aliased_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aliased_Absent)
        or else Kind (Bare_Aliased_Absent) in Ada_Aliased_Absent_Range;
   subtype Bare_Aliased_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aliased_Present)
        or else Kind (Bare_Aliased_Present) in Ada_Aliased_Present_Range;
   subtype Bare_All_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_All_Node)
        or else Kind (Bare_All_Node) in Ada_All_Node;
   subtype Bare_All_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_All_Absent)
        or else Kind (Bare_All_Absent) in Ada_All_Absent_Range;
   subtype Bare_All_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_All_Present)
        or else Kind (Bare_All_Present) in Ada_All_Present_Range;
   subtype Bare_Array_Indices is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Array_Indices)
        or else Kind (Bare_Array_Indices) in Ada_Array_Indices;
   subtype Bare_Constrained_Array_Indices is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constrained_Array_Indices)
        or else Kind (Bare_Constrained_Array_Indices) in
          Ada_Constrained_Array_Indices_Range;
   subtype Bare_Unconstrained_Array_Indices is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Unconstrained_Array_Indices)
        or else Kind (Bare_Unconstrained_Array_Indices) in
          Ada_Unconstrained_Array_Indices_Range;
   subtype Bare_Aspect_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aspect_Assoc)
        or else Kind (Bare_Aspect_Assoc) in Ada_Aspect_Assoc_Range;
   subtype Bare_Aspect_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aspect_Clause)
        or else Kind (Bare_Aspect_Clause) in Ada_Aspect_Clause;
   subtype Bare_At_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_At_Clause)
        or else Kind (Bare_At_Clause) in Ada_At_Clause_Range;
   subtype Bare_Attribute_Def_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Attribute_Def_Clause)
        or else Kind (Bare_Attribute_Def_Clause) in
          Ada_Attribute_Def_Clause_Range;
   subtype Bare_Enum_Rep_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Enum_Rep_Clause)
        or else Kind (Bare_Enum_Rep_Clause) in Ada_Enum_Rep_Clause_Range;
   subtype Bare_Record_Rep_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Record_Rep_Clause)
        or else Kind (Bare_Record_Rep_Clause) in Ada_Record_Rep_Clause_Range;
   subtype Bare_Aspect_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aspect_Spec)
        or else Kind (Bare_Aspect_Spec) in Ada_Aspect_Spec_Range;
   subtype Bare_Base_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Assoc)
        or else Kind (Bare_Base_Assoc) in Ada_Base_Assoc;
   subtype Bare_Contract_Case_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Contract_Case_Assoc)
        or else Kind (Bare_Contract_Case_Assoc) in
          Ada_Contract_Case_Assoc_Range;
   subtype Bare_Pragma_Argument_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Pragma_Argument_Assoc)
        or else Kind (Bare_Pragma_Argument_Assoc) in
          Ada_Pragma_Argument_Assoc_Range;
   subtype Bare_Base_Formal_Param_Holder is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Formal_Param_Holder)
        or else Kind (Bare_Base_Formal_Param_Holder) in
          Ada_Base_Formal_Param_Holder;
   subtype Bare_Base_Subp_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Subp_Spec)
        or else Kind (Bare_Base_Subp_Spec) in Ada_Base_Subp_Spec;
   subtype Bare_Entry_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Entry_Spec)
        or else Kind (Bare_Entry_Spec) in Ada_Entry_Spec_Range;
   subtype Bare_Enum_Subp_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Enum_Subp_Spec)
        or else Kind (Bare_Enum_Subp_Spec) in Ada_Enum_Subp_Spec_Range;
   subtype Bare_Subp_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Spec)
        or else Kind (Bare_Subp_Spec) in Ada_Subp_Spec_Range;
   subtype Bare_Component_List is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Component_List)
        or else Kind (Bare_Component_List) in Ada_Component_List_Range;
   subtype Bare_Discriminant_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discriminant_Part)
        or else Kind (Bare_Discriminant_Part) in Ada_Discriminant_Part;
   subtype Bare_Known_Discriminant_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Known_Discriminant_Part)
        or else Kind (Bare_Known_Discriminant_Part) in
          Ada_Known_Discriminant_Part_Range;
   subtype Bare_Unknown_Discriminant_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Unknown_Discriminant_Part)
        or else Kind (Bare_Unknown_Discriminant_Part) in
          Ada_Unknown_Discriminant_Part_Range;
   subtype Bare_Entry_Completion_Formal_Params is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Entry_Completion_Formal_Params)
        or else Kind (Bare_Entry_Completion_Formal_Params) in
          Ada_Entry_Completion_Formal_Params_Range;
   subtype Bare_Generic_Formal_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Formal_Part)
        or else Kind (Bare_Generic_Formal_Part) in
          Ada_Generic_Formal_Part_Range;
   subtype Bare_Base_Record_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Record_Def)
        or else Kind (Bare_Base_Record_Def) in Ada_Base_Record_Def;
   subtype Bare_Null_Record_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Null_Record_Def)
        or else Kind (Bare_Null_Record_Def) in Ada_Null_Record_Def_Range;
   subtype Bare_Record_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Record_Def)
        or else Kind (Bare_Record_Def) in Ada_Record_Def_Range;
   subtype Bare_Basic_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Basic_Assoc)
        or else Kind (Bare_Basic_Assoc) in Ada_Basic_Assoc;
   subtype Bare_Aggregate_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aggregate_Assoc)
        or else Kind (Bare_Aggregate_Assoc) in Ada_Aggregate_Assoc_Range;
   subtype Bare_Multi_Dim_Array_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Multi_Dim_Array_Assoc)
        or else Kind (Bare_Multi_Dim_Array_Assoc) in
          Ada_Multi_Dim_Array_Assoc_Range;
   subtype Bare_Discriminant_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discriminant_Assoc)
        or else Kind (Bare_Discriminant_Assoc) in Ada_Discriminant_Assoc_Range;
   subtype Bare_Param_Assoc is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Param_Assoc)
        or else Kind (Bare_Param_Assoc) in Ada_Param_Assoc_Range;
   subtype Bare_Basic_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Basic_Decl)
        or else Kind (Bare_Basic_Decl) in Ada_Basic_Decl;
   subtype Bare_Base_Formal_Param_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Formal_Param_Decl)
        or else Kind (Bare_Base_Formal_Param_Decl) in
          Ada_Base_Formal_Param_Decl;
   subtype Bare_Component_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Component_Decl)
        or else Kind (Bare_Component_Decl) in Ada_Component_Decl_Range;
   subtype Bare_Discriminant_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discriminant_Spec)
        or else Kind (Bare_Discriminant_Spec) in Ada_Discriminant_Spec_Range;
   subtype Bare_Generic_Formal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Formal)
        or else Kind (Bare_Generic_Formal) in Ada_Generic_Formal;
   subtype Bare_Generic_Formal_Obj_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Formal_Obj_Decl)
        or else Kind (Bare_Generic_Formal_Obj_Decl) in
          Ada_Generic_Formal_Obj_Decl_Range;
   subtype Bare_Generic_Formal_Package is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Formal_Package)
        or else Kind (Bare_Generic_Formal_Package) in
          Ada_Generic_Formal_Package_Range;
   subtype Bare_Generic_Formal_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Formal_Subp_Decl)
        or else Kind (Bare_Generic_Formal_Subp_Decl) in
          Ada_Generic_Formal_Subp_Decl_Range;
   subtype Bare_Generic_Formal_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Formal_Type_Decl)
        or else Kind (Bare_Generic_Formal_Type_Decl) in
          Ada_Generic_Formal_Type_Decl_Range;
   subtype Bare_Param_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Param_Spec)
        or else Kind (Bare_Param_Spec) in Ada_Param_Spec_Range;
   subtype Bare_Base_Package_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Package_Decl)
        or else Kind (Bare_Base_Package_Decl) in Ada_Base_Package_Decl;
   subtype Bare_Generic_Package_Internal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Package_Internal)
        or else Kind (Bare_Generic_Package_Internal) in
          Ada_Generic_Package_Internal_Range;
   subtype Bare_Package_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Package_Decl)
        or else Kind (Bare_Package_Decl) in Ada_Package_Decl_Range;
   subtype Bare_Base_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Type_Decl)
        or else Kind (Bare_Base_Type_Decl) in Ada_Base_Type_Decl;
   subtype Bare_Base_Subtype_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Subtype_Decl)
        or else Kind (Bare_Base_Subtype_Decl) in Ada_Base_Subtype_Decl;
   subtype Bare_Discrete_Base_Subtype_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discrete_Base_Subtype_Decl)
        or else Kind (Bare_Discrete_Base_Subtype_Decl) in
          Ada_Discrete_Base_Subtype_Decl_Range;
   subtype Bare_Subtype_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subtype_Decl)
        or else Kind (Bare_Subtype_Decl) in Ada_Subtype_Decl_Range;
   subtype Bare_Classwide_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Classwide_Type_Decl)
        or else Kind (Bare_Classwide_Type_Decl) in
          Ada_Classwide_Type_Decl_Range;
   subtype Bare_Incomplete_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Incomplete_Type_Decl)
        or else Kind (Bare_Incomplete_Type_Decl) in
          Ada_Incomplete_Type_Decl_Range;
   subtype Bare_Incomplete_Tagged_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Incomplete_Tagged_Type_Decl)
        or else Kind (Bare_Incomplete_Tagged_Type_Decl) in
          Ada_Incomplete_Tagged_Type_Decl_Range;
   subtype Bare_Protected_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Type_Decl)
        or else Kind (Bare_Protected_Type_Decl) in
          Ada_Protected_Type_Decl_Range;
   subtype Bare_Task_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Task_Type_Decl)
        or else Kind (Bare_Task_Type_Decl) in Ada_Task_Type_Decl_Range;
   subtype Bare_Single_Task_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Single_Task_Type_Decl)
        or else Kind (Bare_Single_Task_Type_Decl) in
          Ada_Single_Task_Type_Decl_Range;
   subtype Bare_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Type_Decl)
        or else Kind (Bare_Type_Decl) in Ada_Type_Decl_Range;
   subtype Bare_Anonymous_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Anonymous_Type_Decl)
        or else Kind (Bare_Anonymous_Type_Decl) in
          Ada_Anonymous_Type_Decl_Range;
   subtype Bare_Synth_Anonymous_Type_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Synth_Anonymous_Type_Decl)
        or else Kind (Bare_Synth_Anonymous_Type_Decl) in
          Ada_Synth_Anonymous_Type_Decl_Range;
   subtype Bare_Basic_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Basic_Subp_Decl)
        or else Kind (Bare_Basic_Subp_Decl) in Ada_Basic_Subp_Decl;
   subtype Bare_Classic_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Classic_Subp_Decl)
        or else Kind (Bare_Classic_Subp_Decl) in Ada_Classic_Subp_Decl;
   subtype Bare_Abstract_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abstract_Subp_Decl)
        or else Kind (Bare_Abstract_Subp_Decl) in Ada_Abstract_Subp_Decl_Range;
   subtype Bare_Formal_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Formal_Subp_Decl)
        or else Kind (Bare_Formal_Subp_Decl) in Ada_Formal_Subp_Decl;
   subtype Bare_Abstract_Formal_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abstract_Formal_Subp_Decl)
        or else Kind (Bare_Abstract_Formal_Subp_Decl) in
          Ada_Abstract_Formal_Subp_Decl_Range;
   subtype Bare_Concrete_Formal_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Concrete_Formal_Subp_Decl)
        or else Kind (Bare_Concrete_Formal_Subp_Decl) in
          Ada_Concrete_Formal_Subp_Decl_Range;
   subtype Bare_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Decl)
        or else Kind (Bare_Subp_Decl) in Ada_Subp_Decl_Range;
   subtype Bare_Entry_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Entry_Decl)
        or else Kind (Bare_Entry_Decl) in Ada_Entry_Decl_Range;
   subtype Bare_Enum_Literal_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Enum_Literal_Decl)
        or else Kind (Bare_Enum_Literal_Decl) in Ada_Enum_Literal_Decl_Range;
   subtype Bare_Generic_Subp_Internal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Subp_Internal)
        or else Kind (Bare_Generic_Subp_Internal) in
          Ada_Generic_Subp_Internal_Range;
   subtype Bare_Body_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Body_Node)
        or else Kind (Bare_Body_Node) in Ada_Body_Node;
   subtype Bare_Base_Subp_Body is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Subp_Body)
        or else Kind (Bare_Base_Subp_Body) in Ada_Base_Subp_Body;
   subtype Bare_Expr_Function is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Expr_Function)
        or else Kind (Bare_Expr_Function) in Ada_Expr_Function_Range;
   subtype Bare_Null_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Null_Subp_Decl)
        or else Kind (Bare_Null_Subp_Decl) in Ada_Null_Subp_Decl_Range;
   subtype Bare_Subp_Body is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Body)
        or else Kind (Bare_Subp_Body) in Ada_Subp_Body_Range;
   subtype Bare_Subp_Renaming_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Renaming_Decl)
        or else Kind (Bare_Subp_Renaming_Decl) in Ada_Subp_Renaming_Decl_Range;
   subtype Bare_Body_Stub is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Body_Stub)
        or else Kind (Bare_Body_Stub) in Ada_Body_Stub;
   subtype Bare_Package_Body_Stub is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Package_Body_Stub)
        or else Kind (Bare_Package_Body_Stub) in Ada_Package_Body_Stub_Range;
   subtype Bare_Protected_Body_Stub is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Body_Stub)
        or else Kind (Bare_Protected_Body_Stub) in
          Ada_Protected_Body_Stub_Range;
   subtype Bare_Subp_Body_Stub is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Body_Stub)
        or else Kind (Bare_Subp_Body_Stub) in Ada_Subp_Body_Stub_Range;
   subtype Bare_Task_Body_Stub is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Task_Body_Stub)
        or else Kind (Bare_Task_Body_Stub) in Ada_Task_Body_Stub_Range;
   subtype Bare_Entry_Body is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Entry_Body)
        or else Kind (Bare_Entry_Body) in Ada_Entry_Body_Range;
   subtype Bare_Package_Body is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Package_Body)
        or else Kind (Bare_Package_Body) in Ada_Package_Body_Range;
   subtype Bare_Protected_Body is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Body)
        or else Kind (Bare_Protected_Body) in Ada_Protected_Body_Range;
   subtype Bare_Task_Body is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Task_Body)
        or else Kind (Bare_Task_Body) in Ada_Task_Body_Range;
   subtype Bare_Entry_Index_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Entry_Index_Spec)
        or else Kind (Bare_Entry_Index_Spec) in Ada_Entry_Index_Spec_Range;
   subtype Bare_Error_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Error_Decl)
        or else Kind (Bare_Error_Decl) in Ada_Error_Decl_Range;
   subtype Bare_Exception_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Exception_Decl)
        or else Kind (Bare_Exception_Decl) in Ada_Exception_Decl_Range;
   subtype Bare_Exception_Handler is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Exception_Handler)
        or else Kind (Bare_Exception_Handler) in Ada_Exception_Handler_Range;
   subtype Bare_For_Loop_Var_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_For_Loop_Var_Decl)
        or else Kind (Bare_For_Loop_Var_Decl) in Ada_For_Loop_Var_Decl_Range;
   subtype Bare_Generic_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Decl)
        or else Kind (Bare_Generic_Decl) in Ada_Generic_Decl;
   subtype Bare_Generic_Package_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Package_Decl)
        or else Kind (Bare_Generic_Package_Decl) in
          Ada_Generic_Package_Decl_Range;
   subtype Bare_Generic_Subp_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Subp_Decl)
        or else Kind (Bare_Generic_Subp_Decl) in Ada_Generic_Subp_Decl_Range;
   subtype Bare_Generic_Instantiation is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Instantiation)
        or else Kind (Bare_Generic_Instantiation) in Ada_Generic_Instantiation;
   subtype Bare_Generic_Package_Instantiation is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Package_Instantiation)
        or else Kind (Bare_Generic_Package_Instantiation) in
          Ada_Generic_Package_Instantiation_Range;
   subtype Bare_Generic_Subp_Instantiation is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Subp_Instantiation)
        or else Kind (Bare_Generic_Subp_Instantiation) in
          Ada_Generic_Subp_Instantiation_Range;
   subtype Bare_Generic_Renaming_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Renaming_Decl)
        or else Kind (Bare_Generic_Renaming_Decl) in Ada_Generic_Renaming_Decl;
   subtype Bare_Generic_Package_Renaming_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Package_Renaming_Decl)
        or else Kind (Bare_Generic_Package_Renaming_Decl) in
          Ada_Generic_Package_Renaming_Decl_Range;
   subtype Bare_Generic_Subp_Renaming_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Generic_Subp_Renaming_Decl)
        or else Kind (Bare_Generic_Subp_Renaming_Decl) in
          Ada_Generic_Subp_Renaming_Decl_Range;
   subtype Bare_Label_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Label_Decl)
        or else Kind (Bare_Label_Decl) in Ada_Label_Decl_Range;
   subtype Bare_Named_Stmt_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Named_Stmt_Decl)
        or else Kind (Bare_Named_Stmt_Decl) in Ada_Named_Stmt_Decl_Range;
   subtype Bare_Number_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Number_Decl)
        or else Kind (Bare_Number_Decl) in Ada_Number_Decl_Range;
   subtype Bare_Object_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Object_Decl)
        or else Kind (Bare_Object_Decl) in Ada_Object_Decl_Range;
   subtype Bare_Anonymous_Object_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Anonymous_Object_Decl)
        or else Kind (Bare_Anonymous_Object_Decl) in
          Ada_Anonymous_Object_Decl_Range;
   subtype Bare_Extended_Return_Stmt_Object_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Extended_Return_Stmt_Object_Decl)
        or else Kind (Bare_Extended_Return_Stmt_Object_Decl) in
          Ada_Extended_Return_Stmt_Object_Decl_Range;
   subtype Bare_Package_Renaming_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Package_Renaming_Decl)
        or else Kind (Bare_Package_Renaming_Decl) in
          Ada_Package_Renaming_Decl_Range;
   subtype Bare_Single_Protected_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Single_Protected_Decl)
        or else Kind (Bare_Single_Protected_Decl) in
          Ada_Single_Protected_Decl_Range;
   subtype Bare_Single_Task_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Single_Task_Decl)
        or else Kind (Bare_Single_Task_Decl) in Ada_Single_Task_Decl_Range;
   subtype Bare_Case_Stmt_Alternative is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Case_Stmt_Alternative)
        or else Kind (Bare_Case_Stmt_Alternative) in
          Ada_Case_Stmt_Alternative_Range;
   subtype Bare_Compilation_Unit is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Compilation_Unit)
        or else Kind (Bare_Compilation_Unit) in Ada_Compilation_Unit_Range;
   subtype Bare_Component_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Component_Clause)
        or else Kind (Bare_Component_Clause) in Ada_Component_Clause_Range;
   subtype Bare_Component_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Component_Def)
        or else Kind (Bare_Component_Def) in Ada_Component_Def_Range;
   subtype Bare_Constant_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constant_Node)
        or else Kind (Bare_Constant_Node) in Ada_Constant_Node;
   subtype Bare_Constant_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constant_Absent)
        or else Kind (Bare_Constant_Absent) in Ada_Constant_Absent_Range;
   subtype Bare_Constant_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constant_Present)
        or else Kind (Bare_Constant_Present) in Ada_Constant_Present_Range;
   subtype Bare_Constraint is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constraint)
        or else Kind (Bare_Constraint) in Ada_Constraint;
   subtype Bare_Delta_Constraint is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Delta_Constraint)
        or else Kind (Bare_Delta_Constraint) in Ada_Delta_Constraint_Range;
   subtype Bare_Digits_Constraint is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Digits_Constraint)
        or else Kind (Bare_Digits_Constraint) in Ada_Digits_Constraint_Range;
   subtype Bare_Discriminant_Constraint is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discriminant_Constraint)
        or else Kind (Bare_Discriminant_Constraint) in
          Ada_Discriminant_Constraint_Range;
   subtype Bare_Index_Constraint is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Index_Constraint)
        or else Kind (Bare_Index_Constraint) in Ada_Index_Constraint_Range;
   subtype Bare_Range_Constraint is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Range_Constraint)
        or else Kind (Bare_Range_Constraint) in Ada_Range_Constraint_Range;
   subtype Bare_Declarative_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Declarative_Part)
        or else Kind (Bare_Declarative_Part) in Ada_Declarative_Part_Range;
   subtype Bare_Private_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Private_Part)
        or else Kind (Bare_Private_Part) in Ada_Private_Part_Range;
   subtype Bare_Public_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Public_Part)
        or else Kind (Bare_Public_Part) in Ada_Public_Part_Range;
   subtype Bare_Elsif_Expr_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Elsif_Expr_Part)
        or else Kind (Bare_Elsif_Expr_Part) in Ada_Elsif_Expr_Part_Range;
   subtype Bare_Elsif_Stmt_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Elsif_Stmt_Part)
        or else Kind (Bare_Elsif_Stmt_Part) in Ada_Elsif_Stmt_Part_Range;
   subtype Bare_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Expr)
        or else Kind (Bare_Expr) in Ada_Expr;
   subtype Bare_Allocator is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Allocator)
        or else Kind (Bare_Allocator) in Ada_Allocator_Range;
   subtype Bare_Base_Aggregate is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Aggregate)
        or else Kind (Bare_Base_Aggregate) in Ada_Base_Aggregate;
   subtype Bare_Aggregate is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Aggregate)
        or else Kind (Bare_Aggregate) in Ada_Aggregate_Range;
   subtype Bare_Null_Record_Aggregate is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Null_Record_Aggregate)
        or else Kind (Bare_Null_Record_Aggregate) in
          Ada_Null_Record_Aggregate_Range;
   subtype Bare_Bin_Op is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Bin_Op)
        or else Kind (Bare_Bin_Op) in Ada_Bin_Op_Range;
   subtype Bare_Relation_Op is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Relation_Op)
        or else Kind (Bare_Relation_Op) in Ada_Relation_Op_Range;
   subtype Bare_Box_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Box_Expr)
        or else Kind (Bare_Box_Expr) in Ada_Box_Expr_Range;
   subtype Bare_Case_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Case_Expr)
        or else Kind (Bare_Case_Expr) in Ada_Case_Expr_Range;
   subtype Bare_Case_Expr_Alternative is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Case_Expr_Alternative)
        or else Kind (Bare_Case_Expr_Alternative) in
          Ada_Case_Expr_Alternative_Range;
   subtype Bare_Contract_Cases is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Contract_Cases)
        or else Kind (Bare_Contract_Cases) in Ada_Contract_Cases_Range;
   subtype Bare_If_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_If_Expr)
        or else Kind (Bare_If_Expr) in Ada_If_Expr_Range;
   subtype Bare_Membership_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Membership_Expr)
        or else Kind (Bare_Membership_Expr) in Ada_Membership_Expr_Range;
   subtype Bare_Name is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Name)
        or else Kind (Bare_Name) in Ada_Name;
   subtype Bare_Attribute_Ref is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Attribute_Ref)
        or else Kind (Bare_Attribute_Ref) in Ada_Attribute_Ref_Range;
   subtype Bare_Update_Attribute_Ref is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Update_Attribute_Ref)
        or else Kind (Bare_Update_Attribute_Ref) in
          Ada_Update_Attribute_Ref_Range;
   subtype Bare_Call_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Call_Expr)
        or else Kind (Bare_Call_Expr) in Ada_Call_Expr_Range;
   subtype Bare_Defining_Name is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Defining_Name)
        or else Kind (Bare_Defining_Name) in Ada_Defining_Name_Range;
   subtype Bare_Discrete_Subtype_Name is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discrete_Subtype_Name)
        or else Kind (Bare_Discrete_Subtype_Name) in
          Ada_Discrete_Subtype_Name_Range;
   subtype Bare_Dotted_Name is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Dotted_Name)
        or else Kind (Bare_Dotted_Name) in Ada_Dotted_Name_Range;
   subtype Bare_End_Name is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_End_Name)
        or else Kind (Bare_End_Name) in Ada_End_Name_Range;
   subtype Bare_Explicit_Deref is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Explicit_Deref)
        or else Kind (Bare_Explicit_Deref) in Ada_Explicit_Deref_Range;
   subtype Bare_Qual_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Qual_Expr)
        or else Kind (Bare_Qual_Expr) in Ada_Qual_Expr_Range;
   subtype Bare_Single_Tok_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Single_Tok_Node)
        or else Kind (Bare_Single_Tok_Node) in Ada_Single_Tok_Node;
   subtype Bare_Base_Id is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Id)
        or else Kind (Bare_Base_Id) in Ada_Base_Id;
   subtype Bare_Char_Literal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Char_Literal)
        or else Kind (Bare_Char_Literal) in Ada_Char_Literal_Range;
   subtype Bare_Identifier is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Identifier)
        or else Kind (Bare_Identifier) in Ada_Identifier_Range;
   subtype Bare_Op is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op)
        or else Kind (Bare_Op) in Ada_Op;
   subtype Bare_Op_Abs is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Abs)
        or else Kind (Bare_Op_Abs) in Ada_Op_Abs_Range;
   subtype Bare_Op_And is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_And)
        or else Kind (Bare_Op_And) in Ada_Op_And_Range;
   subtype Bare_Op_And_Then is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_And_Then)
        or else Kind (Bare_Op_And_Then) in Ada_Op_And_Then_Range;
   subtype Bare_Op_Concat is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Concat)
        or else Kind (Bare_Op_Concat) in Ada_Op_Concat_Range;
   subtype Bare_Op_Div is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Div)
        or else Kind (Bare_Op_Div) in Ada_Op_Div_Range;
   subtype Bare_Op_Double_Dot is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Double_Dot)
        or else Kind (Bare_Op_Double_Dot) in Ada_Op_Double_Dot_Range;
   subtype Bare_Op_Eq is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Eq)
        or else Kind (Bare_Op_Eq) in Ada_Op_Eq_Range;
   subtype Bare_Op_Gt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Gt)
        or else Kind (Bare_Op_Gt) in Ada_Op_Gt_Range;
   subtype Bare_Op_Gte is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Gte)
        or else Kind (Bare_Op_Gte) in Ada_Op_Gte_Range;
   subtype Bare_Op_In is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_In)
        or else Kind (Bare_Op_In) in Ada_Op_In_Range;
   subtype Bare_Op_Lt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Lt)
        or else Kind (Bare_Op_Lt) in Ada_Op_Lt_Range;
   subtype Bare_Op_Lte is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Lte)
        or else Kind (Bare_Op_Lte) in Ada_Op_Lte_Range;
   subtype Bare_Op_Minus is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Minus)
        or else Kind (Bare_Op_Minus) in Ada_Op_Minus_Range;
   subtype Bare_Op_Mod is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Mod)
        or else Kind (Bare_Op_Mod) in Ada_Op_Mod_Range;
   subtype Bare_Op_Mult is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Mult)
        or else Kind (Bare_Op_Mult) in Ada_Op_Mult_Range;
   subtype Bare_Op_Neq is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Neq)
        or else Kind (Bare_Op_Neq) in Ada_Op_Neq_Range;
   subtype Bare_Op_Not is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Not)
        or else Kind (Bare_Op_Not) in Ada_Op_Not_Range;
   subtype Bare_Op_Not_In is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Not_In)
        or else Kind (Bare_Op_Not_In) in Ada_Op_Not_In_Range;
   subtype Bare_Op_Or is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Or)
        or else Kind (Bare_Op_Or) in Ada_Op_Or_Range;
   subtype Bare_Op_Or_Else is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Or_Else)
        or else Kind (Bare_Op_Or_Else) in Ada_Op_Or_Else_Range;
   subtype Bare_Op_Plus is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Plus)
        or else Kind (Bare_Op_Plus) in Ada_Op_Plus_Range;
   subtype Bare_Op_Pow is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Pow)
        or else Kind (Bare_Op_Pow) in Ada_Op_Pow_Range;
   subtype Bare_Op_Rem is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Rem)
        or else Kind (Bare_Op_Rem) in Ada_Op_Rem_Range;
   subtype Bare_Op_Xor is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Op_Xor)
        or else Kind (Bare_Op_Xor) in Ada_Op_Xor_Range;
   subtype Bare_String_Literal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_String_Literal)
        or else Kind (Bare_String_Literal) in Ada_String_Literal_Range;
   subtype Bare_Null_Literal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Null_Literal)
        or else Kind (Bare_Null_Literal) in Ada_Null_Literal_Range;
   subtype Bare_Num_Literal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Num_Literal)
        or else Kind (Bare_Num_Literal) in Ada_Num_Literal;
   subtype Bare_Int_Literal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Int_Literal)
        or else Kind (Bare_Int_Literal) in Ada_Int_Literal_Range;
   subtype Bare_Real_Literal is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Real_Literal)
        or else Kind (Bare_Real_Literal) in Ada_Real_Literal_Range;
   subtype Bare_Target_Name is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Target_Name)
        or else Kind (Bare_Target_Name) in Ada_Target_Name_Range;
   subtype Bare_Paren_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Paren_Expr)
        or else Kind (Bare_Paren_Expr) in Ada_Paren_Expr_Range;
   subtype Bare_Quantified_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Quantified_Expr)
        or else Kind (Bare_Quantified_Expr) in Ada_Quantified_Expr_Range;
   subtype Bare_Raise_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Raise_Expr)
        or else Kind (Bare_Raise_Expr) in Ada_Raise_Expr_Range;
   subtype Bare_Un_Op is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Un_Op)
        or else Kind (Bare_Un_Op) in Ada_Un_Op_Range;
   subtype Bare_Handled_Stmts is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Handled_Stmts)
        or else Kind (Bare_Handled_Stmts) in Ada_Handled_Stmts_Range;
   subtype Bare_Interface_Kind is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Interface_Kind)
        or else Kind (Bare_Interface_Kind) in Ada_Interface_Kind;
   subtype Bare_Interface_Kind_Limited is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Interface_Kind_Limited)
        or else Kind (Bare_Interface_Kind_Limited) in
          Ada_Interface_Kind_Limited_Range;
   subtype Bare_Interface_Kind_Protected is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Interface_Kind_Protected)
        or else Kind (Bare_Interface_Kind_Protected) in
          Ada_Interface_Kind_Protected_Range;
   subtype Bare_Interface_Kind_Synchronized is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Interface_Kind_Synchronized)
        or else Kind (Bare_Interface_Kind_Synchronized) in
          Ada_Interface_Kind_Synchronized_Range;
   subtype Bare_Interface_Kind_Task is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Interface_Kind_Task)
        or else Kind (Bare_Interface_Kind_Task) in
          Ada_Interface_Kind_Task_Range;
   subtype Bare_Iter_Type is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Iter_Type)
        or else Kind (Bare_Iter_Type) in Ada_Iter_Type;
   subtype Bare_Iter_Type_In is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Iter_Type_In)
        or else Kind (Bare_Iter_Type_In) in Ada_Iter_Type_In_Range;
   subtype Bare_Iter_Type_Of is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Iter_Type_Of)
        or else Kind (Bare_Iter_Type_Of) in Ada_Iter_Type_Of_Range;
   subtype Bare_Library_Item is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Library_Item)
        or else Kind (Bare_Library_Item) in Ada_Library_Item_Range;
   subtype Bare_Limited_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Limited_Node)
        or else Kind (Bare_Limited_Node) in Ada_Limited_Node;
   subtype Bare_Limited_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Limited_Absent)
        or else Kind (Bare_Limited_Absent) in Ada_Limited_Absent_Range;
   subtype Bare_Limited_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Limited_Present)
        or else Kind (Bare_Limited_Present) in Ada_Limited_Present_Range;
   subtype Bare_Loop_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Loop_Spec)
        or else Kind (Bare_Loop_Spec) in Ada_Loop_Spec;
   subtype Bare_For_Loop_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_For_Loop_Spec)
        or else Kind (Bare_For_Loop_Spec) in Ada_For_Loop_Spec_Range;
   subtype Bare_While_Loop_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_While_Loop_Spec)
        or else Kind (Bare_While_Loop_Spec) in Ada_While_Loop_Spec_Range;
   subtype Bare_Mode is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Mode)
        or else Kind (Bare_Mode) in Ada_Mode;
   subtype Bare_Mode_Default is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Mode_Default)
        or else Kind (Bare_Mode_Default) in Ada_Mode_Default_Range;
   subtype Bare_Mode_In is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Mode_In)
        or else Kind (Bare_Mode_In) in Ada_Mode_In_Range;
   subtype Bare_Mode_In_Out is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Mode_In_Out)
        or else Kind (Bare_Mode_In_Out) in Ada_Mode_In_Out_Range;
   subtype Bare_Mode_Out is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Mode_Out)
        or else Kind (Bare_Mode_Out) in Ada_Mode_Out_Range;
   subtype Bare_Not_Null is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Not_Null)
        or else Kind (Bare_Not_Null) in Ada_Not_Null;
   subtype Bare_Not_Null_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Not_Null_Absent)
        or else Kind (Bare_Not_Null_Absent) in Ada_Not_Null_Absent_Range;
   subtype Bare_Not_Null_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Not_Null_Present)
        or else Kind (Bare_Not_Null_Present) in Ada_Not_Null_Present_Range;
   subtype Bare_Null_Component_Decl is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Null_Component_Decl)
        or else Kind (Bare_Null_Component_Decl) in
          Ada_Null_Component_Decl_Range;
   subtype Bare_Others_Designator is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Others_Designator)
        or else Kind (Bare_Others_Designator) in Ada_Others_Designator_Range;
   subtype Bare_Overriding_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Overriding_Node)
        or else Kind (Bare_Overriding_Node) in Ada_Overriding_Node;
   subtype Bare_Overriding_Not_Overriding is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Overriding_Not_Overriding)
        or else Kind (Bare_Overriding_Not_Overriding) in
          Ada_Overriding_Not_Overriding_Range;
   subtype Bare_Overriding_Overriding is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Overriding_Overriding)
        or else Kind (Bare_Overriding_Overriding) in
          Ada_Overriding_Overriding_Range;
   subtype Bare_Overriding_Unspecified is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Overriding_Unspecified)
        or else Kind (Bare_Overriding_Unspecified) in
          Ada_Overriding_Unspecified_Range;
   subtype Bare_Params is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Params)
        or else Kind (Bare_Params) in Ada_Params_Range;
   subtype Bare_Pragma_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Pragma_Node)
        or else Kind (Bare_Pragma_Node) in Ada_Pragma_Node_Range;
   subtype Bare_Prim_Type_Accessor is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Prim_Type_Accessor)
        or else Kind (Bare_Prim_Type_Accessor) in Ada_Prim_Type_Accessor_Range;
   subtype Bare_Private_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Private_Node)
        or else Kind (Bare_Private_Node) in Ada_Private_Node;
   subtype Bare_Private_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Private_Absent)
        or else Kind (Bare_Private_Absent) in Ada_Private_Absent_Range;
   subtype Bare_Private_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Private_Present)
        or else Kind (Bare_Private_Present) in Ada_Private_Present_Range;
   subtype Bare_Protected_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Def)
        or else Kind (Bare_Protected_Def) in Ada_Protected_Def_Range;
   subtype Bare_Protected_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Node)
        or else Kind (Bare_Protected_Node) in Ada_Protected_Node;
   subtype Bare_Protected_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Absent)
        or else Kind (Bare_Protected_Absent) in Ada_Protected_Absent_Range;
   subtype Bare_Protected_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Protected_Present)
        or else Kind (Bare_Protected_Present) in Ada_Protected_Present_Range;
   subtype Bare_Quantifier is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Quantifier)
        or else Kind (Bare_Quantifier) in Ada_Quantifier;
   subtype Bare_Quantifier_All is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Quantifier_All)
        or else Kind (Bare_Quantifier_All) in Ada_Quantifier_All_Range;
   subtype Bare_Quantifier_Some is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Quantifier_Some)
        or else Kind (Bare_Quantifier_Some) in Ada_Quantifier_Some_Range;
   subtype Bare_Range_Spec is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Range_Spec)
        or else Kind (Bare_Range_Spec) in Ada_Range_Spec_Range;
   subtype Bare_Renaming_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Renaming_Clause)
        or else Kind (Bare_Renaming_Clause) in Ada_Renaming_Clause_Range;
   subtype Bare_Synthetic_Renaming_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Synthetic_Renaming_Clause)
        or else Kind (Bare_Synthetic_Renaming_Clause) in
          Ada_Synthetic_Renaming_Clause_Range;
   subtype Bare_Reverse_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Reverse_Node)
        or else Kind (Bare_Reverse_Node) in Ada_Reverse_Node;
   subtype Bare_Reverse_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Reverse_Absent)
        or else Kind (Bare_Reverse_Absent) in Ada_Reverse_Absent_Range;
   subtype Bare_Reverse_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Reverse_Present)
        or else Kind (Bare_Reverse_Present) in Ada_Reverse_Present_Range;
   subtype Bare_Select_When_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Select_When_Part)
        or else Kind (Bare_Select_When_Part) in Ada_Select_When_Part_Range;
   subtype Bare_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Stmt)
        or else Kind (Bare_Stmt) in Ada_Stmt;
   subtype Bare_Composite_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Composite_Stmt)
        or else Kind (Bare_Composite_Stmt) in Ada_Composite_Stmt;
   subtype Bare_Accept_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Accept_Stmt)
        or else Kind (Bare_Accept_Stmt) in Ada_Accept_Stmt_Range;
   subtype Bare_Accept_Stmt_With_Stmts is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Accept_Stmt_With_Stmts)
        or else Kind (Bare_Accept_Stmt_With_Stmts) in
          Ada_Accept_Stmt_With_Stmts_Range;
   subtype Bare_Base_Loop_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Loop_Stmt)
        or else Kind (Bare_Base_Loop_Stmt) in Ada_Base_Loop_Stmt;
   subtype Bare_For_Loop_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_For_Loop_Stmt)
        or else Kind (Bare_For_Loop_Stmt) in Ada_For_Loop_Stmt_Range;
   subtype Bare_Loop_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Loop_Stmt)
        or else Kind (Bare_Loop_Stmt) in Ada_Loop_Stmt_Range;
   subtype Bare_While_Loop_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_While_Loop_Stmt)
        or else Kind (Bare_While_Loop_Stmt) in Ada_While_Loop_Stmt_Range;
   subtype Bare_Block_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Block_Stmt)
        or else Kind (Bare_Block_Stmt) in Ada_Block_Stmt;
   subtype Bare_Begin_Block is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Begin_Block)
        or else Kind (Bare_Begin_Block) in Ada_Begin_Block_Range;
   subtype Bare_Decl_Block is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Decl_Block)
        or else Kind (Bare_Decl_Block) in Ada_Decl_Block_Range;
   subtype Bare_Case_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Case_Stmt)
        or else Kind (Bare_Case_Stmt) in Ada_Case_Stmt_Range;
   subtype Bare_Extended_Return_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Extended_Return_Stmt)
        or else Kind (Bare_Extended_Return_Stmt) in
          Ada_Extended_Return_Stmt_Range;
   subtype Bare_If_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_If_Stmt)
        or else Kind (Bare_If_Stmt) in Ada_If_Stmt_Range;
   subtype Bare_Named_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Named_Stmt)
        or else Kind (Bare_Named_Stmt) in Ada_Named_Stmt_Range;
   subtype Bare_Select_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Select_Stmt)
        or else Kind (Bare_Select_Stmt) in Ada_Select_Stmt_Range;
   subtype Bare_Error_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Error_Stmt)
        or else Kind (Bare_Error_Stmt) in Ada_Error_Stmt_Range;
   subtype Bare_Simple_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Simple_Stmt)
        or else Kind (Bare_Simple_Stmt) in Ada_Simple_Stmt;
   subtype Bare_Abort_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Abort_Stmt)
        or else Kind (Bare_Abort_Stmt) in Ada_Abort_Stmt_Range;
   subtype Bare_Assign_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Assign_Stmt)
        or else Kind (Bare_Assign_Stmt) in Ada_Assign_Stmt_Range;
   subtype Bare_Call_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Call_Stmt)
        or else Kind (Bare_Call_Stmt) in Ada_Call_Stmt_Range;
   subtype Bare_Delay_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Delay_Stmt)
        or else Kind (Bare_Delay_Stmt) in Ada_Delay_Stmt_Range;
   subtype Bare_Exit_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Exit_Stmt)
        or else Kind (Bare_Exit_Stmt) in Ada_Exit_Stmt_Range;
   subtype Bare_Goto_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Goto_Stmt)
        or else Kind (Bare_Goto_Stmt) in Ada_Goto_Stmt_Range;
   subtype Bare_Label is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Label)
        or else Kind (Bare_Label) in Ada_Label_Range;
   subtype Bare_Null_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Null_Stmt)
        or else Kind (Bare_Null_Stmt) in Ada_Null_Stmt_Range;
   subtype Bare_Raise_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Raise_Stmt)
        or else Kind (Bare_Raise_Stmt) in Ada_Raise_Stmt_Range;
   subtype Bare_Requeue_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Requeue_Stmt)
        or else Kind (Bare_Requeue_Stmt) in Ada_Requeue_Stmt_Range;
   subtype Bare_Return_Stmt is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Return_Stmt)
        or else Kind (Bare_Return_Stmt) in Ada_Return_Stmt_Range;
   subtype Bare_Terminate_Alternative is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Terminate_Alternative)
        or else Kind (Bare_Terminate_Alternative) in
          Ada_Terminate_Alternative_Range;
   subtype Bare_Subp_Kind is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Kind)
        or else Kind (Bare_Subp_Kind) in Ada_Subp_Kind;
   subtype Bare_Subp_Kind_Function is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Kind_Function)
        or else Kind (Bare_Subp_Kind_Function) in Ada_Subp_Kind_Function_Range;
   subtype Bare_Subp_Kind_Procedure is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subp_Kind_Procedure)
        or else Kind (Bare_Subp_Kind_Procedure) in
          Ada_Subp_Kind_Procedure_Range;
   subtype Bare_Subunit is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subunit)
        or else Kind (Bare_Subunit) in Ada_Subunit_Range;
   subtype Bare_Synchronized_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Synchronized_Node)
        or else Kind (Bare_Synchronized_Node) in Ada_Synchronized_Node;
   subtype Bare_Synchronized_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Synchronized_Absent)
        or else Kind (Bare_Synchronized_Absent) in
          Ada_Synchronized_Absent_Range;
   subtype Bare_Synchronized_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Synchronized_Present)
        or else Kind (Bare_Synchronized_Present) in
          Ada_Synchronized_Present_Range;
   subtype Bare_Tagged_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Tagged_Node)
        or else Kind (Bare_Tagged_Node) in Ada_Tagged_Node;
   subtype Bare_Tagged_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Tagged_Absent)
        or else Kind (Bare_Tagged_Absent) in Ada_Tagged_Absent_Range;
   subtype Bare_Tagged_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Tagged_Present)
        or else Kind (Bare_Tagged_Present) in Ada_Tagged_Present_Range;
   subtype Bare_Task_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Task_Def)
        or else Kind (Bare_Task_Def) in Ada_Task_Def_Range;
   subtype Bare_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Type_Def)
        or else Kind (Bare_Type_Def) in Ada_Type_Def;
   subtype Bare_Access_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Access_Def)
        or else Kind (Bare_Access_Def) in Ada_Access_Def;
   subtype Bare_Access_To_Subp_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Access_To_Subp_Def)
        or else Kind (Bare_Access_To_Subp_Def) in Ada_Access_To_Subp_Def_Range;
   subtype Bare_Base_Type_Access_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Base_Type_Access_Def)
        or else Kind (Bare_Base_Type_Access_Def) in Ada_Base_Type_Access_Def;
   subtype Bare_Anonymous_Type_Access_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Anonymous_Type_Access_Def)
        or else Kind (Bare_Anonymous_Type_Access_Def) in
          Ada_Anonymous_Type_Access_Def_Range;
   subtype Bare_Type_Access_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Type_Access_Def)
        or else Kind (Bare_Type_Access_Def) in Ada_Type_Access_Def_Range;
   subtype Bare_Array_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Array_Type_Def)
        or else Kind (Bare_Array_Type_Def) in Ada_Array_Type_Def_Range;
   subtype Bare_Derived_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Derived_Type_Def)
        or else Kind (Bare_Derived_Type_Def) in Ada_Derived_Type_Def_Range;
   subtype Bare_Enum_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Enum_Type_Def)
        or else Kind (Bare_Enum_Type_Def) in Ada_Enum_Type_Def_Range;
   subtype Bare_Formal_Discrete_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Formal_Discrete_Type_Def)
        or else Kind (Bare_Formal_Discrete_Type_Def) in
          Ada_Formal_Discrete_Type_Def_Range;
   subtype Bare_Interface_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Interface_Type_Def)
        or else Kind (Bare_Interface_Type_Def) in Ada_Interface_Type_Def_Range;
   subtype Bare_Mod_Int_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Mod_Int_Type_Def)
        or else Kind (Bare_Mod_Int_Type_Def) in Ada_Mod_Int_Type_Def_Range;
   subtype Bare_Private_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Private_Type_Def)
        or else Kind (Bare_Private_Type_Def) in Ada_Private_Type_Def_Range;
   subtype Bare_Real_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Real_Type_Def)
        or else Kind (Bare_Real_Type_Def) in Ada_Real_Type_Def;
   subtype Bare_Decimal_Fixed_Point_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Decimal_Fixed_Point_Def)
        or else Kind (Bare_Decimal_Fixed_Point_Def) in
          Ada_Decimal_Fixed_Point_Def_Range;
   subtype Bare_Floating_Point_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Floating_Point_Def)
        or else Kind (Bare_Floating_Point_Def) in Ada_Floating_Point_Def_Range;
   subtype Bare_Ordinary_Fixed_Point_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Ordinary_Fixed_Point_Def)
        or else Kind (Bare_Ordinary_Fixed_Point_Def) in
          Ada_Ordinary_Fixed_Point_Def_Range;
   subtype Bare_Record_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Record_Type_Def)
        or else Kind (Bare_Record_Type_Def) in Ada_Record_Type_Def_Range;
   subtype Bare_Signed_Int_Type_Def is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Signed_Int_Type_Def)
        or else Kind (Bare_Signed_Int_Type_Def) in
          Ada_Signed_Int_Type_Def_Range;
   subtype Bare_Type_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Type_Expr)
        or else Kind (Bare_Type_Expr) in Ada_Type_Expr;
   subtype Bare_Anonymous_Type is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Anonymous_Type)
        or else Kind (Bare_Anonymous_Type) in Ada_Anonymous_Type_Range;
   subtype Bare_Enum_Lit_Synth_Type_Expr is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Enum_Lit_Synth_Type_Expr)
        or else Kind (Bare_Enum_Lit_Synth_Type_Expr) in
          Ada_Enum_Lit_Synth_Type_Expr_Range;
   subtype Bare_Subtype_Indication is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Subtype_Indication)
        or else Kind (Bare_Subtype_Indication) in Ada_Subtype_Indication_Range;
   subtype Bare_Constrained_Subtype_Indication is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Constrained_Subtype_Indication)
        or else Kind (Bare_Constrained_Subtype_Indication) in
          Ada_Constrained_Subtype_Indication_Range;
   subtype Bare_Discrete_Subtype_Indication is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Discrete_Subtype_Indication)
        or else Kind (Bare_Discrete_Subtype_Indication) in
          Ada_Discrete_Subtype_Indication_Range;
   subtype Bare_Unconstrained_Array_Index is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Unconstrained_Array_Index)
        or else Kind (Bare_Unconstrained_Array_Index) in
          Ada_Unconstrained_Array_Index_Range;
   subtype Bare_Until_Node is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Until_Node)
        or else Kind (Bare_Until_Node) in Ada_Until_Node;
   subtype Bare_Until_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Until_Absent)
        or else Kind (Bare_Until_Absent) in Ada_Until_Absent_Range;
   subtype Bare_Until_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Until_Present)
        or else Kind (Bare_Until_Present) in Ada_Until_Present_Range;
   subtype Bare_Use_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Use_Clause)
        or else Kind (Bare_Use_Clause) in Ada_Use_Clause;
   subtype Bare_Use_Package_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Use_Package_Clause)
        or else Kind (Bare_Use_Package_Clause) in Ada_Use_Package_Clause_Range;
   subtype Bare_Use_Type_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Use_Type_Clause)
        or else Kind (Bare_Use_Type_Clause) in Ada_Use_Type_Clause_Range;
   subtype Bare_Variant is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Variant)
        or else Kind (Bare_Variant) in Ada_Variant_Range;
   subtype Bare_Variant_Part is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_Variant_Part)
        or else Kind (Bare_Variant_Part) in Ada_Variant_Part_Range;
   subtype Bare_With_Clause is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_With_Clause)
        or else Kind (Bare_With_Clause) in Ada_With_Clause_Range;
   subtype Bare_With_Private is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_With_Private)
        or else Kind (Bare_With_Private) in Ada_With_Private;
   subtype Bare_With_Private_Absent is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_With_Private_Absent)
        or else Kind (Bare_With_Private_Absent) in
          Ada_With_Private_Absent_Range;
   subtype Bare_With_Private_Present is Bare_Ada_Node with
        Dynamic_Predicate => Is_Null (Bare_With_Private_Present)
        or else Kind (Bare_With_Private_Present) in
          Ada_With_Private_Present_Range;

   package Alloc_AST_List_Array is new Langkit_Support.Bump_Ptr.Array_Alloc
     (Element_T => Bare_Ada_Node, Index_Type => Positive);
   --  Allocator for array of nodes, used in list nodes

   type Rewriting_Handle_Pointer is new System.Address;
   No_Rewriting_Handle_Pointer : constant Rewriting_Handle_Pointer :=
     Rewriting_Handle_Pointer (System.Null_Address);

   Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("LANGKIT.PROPERTIES", GNATCOLL.Traces.On);

   function Short_Text_Image (Self : Bare_Ada_Node) return Text_Type;
   --  Return a short representation of the node, containing just the kind name
   --  and the sloc, or "None" if Self is null.

   function Is_Token_Node (Node : Bare_Ada_Node) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic (Node : Bare_Ada_Node) return Boolean;
   --  Return whether this node is synthetic.

   ---------------------------
   -- Environments handling --
   ---------------------------

   type Internal_Metadata;

   type Internal_Metadata is record

      Dottable_Subp : aliased Boolean;
      --  Whether the stored element is a subprogram accessed through the dot
      --  notation

      Access_Entity : aliased Boolean;
      --  Whether the accessed entity is an anonymous access to it or not.

      Primitive : aliased Bare_Ada_Node;
      --  The type for which this subprogram is a primitive, if any

      Primitive_Real_Type : aliased Bare_Ada_Node;
      --  The type for which this subprogram is a primitive, if any

   end record with
      Convention => C;

   function Hash (R : Internal_Metadata) return Hash_Type;

   function Trace_Image (R : Internal_Metadata) return String;

   No_Metadata : constant Internal_Metadata :=
     (Dottable_Subp => False, Access_Entity => False,
      Primitive => No_Bare_Ada_Node, Primitive_Real_Type => No_Bare_Ada_Node);

   function Combine (L, R : Internal_Metadata) return Internal_Metadata;
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : Bare_Ada_Node) return Boolean;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg this does not handle general visibility issues, just sequentiality
   --  of declarations.

   function AST_Envs_Node_Text_Image
     (Node : Bare_Ada_Node; Short : Boolean := True) return Text_Type;
   --  Return a "sourcefile:lineno:columnno" corresponding to the starting
   --  sloc of Node. Used to create a human-readable representation for env.
   --  rebindings.

   function Is_Rebindable (Node : Bare_Ada_Node) return Boolean;

   procedure Register_Rebinding
     (Node : Bare_Ada_Node; Rebinding : System.Address);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.
   --
   --  TODO??? For now the rebinding must be represented as an untyped
   --  pointer because we probably need some big refactoring to provide to
   --  Langkit_Support.Lexical_Env a procedure that has visibility on both
   --  Env_Rebindings and on the analysis unit record.

   function Element_Parent (Node : Bare_Ada_Node) return Bare_Ada_Node;

   function Hash (Node : Bare_Ada_Node) return Hash_Type;
   function Node_Unit (Node : Bare_Ada_Node) return Internal_Unit;
   function Named_Hash (Node : Bare_Ada_Node) return Hash_Type is
     (Hash (Node));

   No_Analysis_Unit : constant Internal_Unit := null;

   function Unit_Version (Unit : Internal_Unit) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   function Context_Version (Unit : Internal_Unit) return Integer;
   --  Return the version of the analysis context associated with Unit

   type Ref_Category is (Inherited_Primitives, Nocat);
   type Ref_Categories is array (Ref_Category) of Boolean;
   pragma Pack (Ref_Categories);

   package AST_Envs is new Langkit_Support.Lexical_Env
     (Precomputed_Symbol_Index => Precomputed_Symbol_Index,
      Precomputed_Symbol       => Precomputed_Symbol, Symbols => Symbols,
      Unit_T                   => Internal_Unit, No_Unit => No_Analysis_Unit,
      Get_Unit_Version => Unit_Version, Get_Context_Version => Context_Version,
      Node_Type => Bare_Ada_Node, Node_Metadata => Internal_Metadata,
      No_Node => null, Empty_Metadata => No_Metadata, Node_Unit => Node_Unit,
      Node_Hash => Named_Hash, Metadata_Hash => Hash, Combine => Combine,
      Node_Text_Image          => AST_Envs_Node_Text_Image,
      Register_Rebinding => Register_Rebinding, Ref_Category => Ref_Category,
      Ref_Categories           => Ref_Categories);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   No_Entity_Info : constant Internal_Entity_Info :=
     (No_Metadata, null, False);
   No_Entity : constant Internal_Entity := (null, No_Entity_Info);

   function Create_Internal_Entity
     (Node : Bare_Ada_Node; Info : Internal_Entity_Info)
      return Internal_Entity;

   function Hash_Entity (Self : Internal_Entity) return Hash_Type;
   --  Hash function to use in the public API. It's like the regular one, but
   --  disregards metadata.

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean;
   --  Equality function to use in the public API. It's like the regular one,
   --  but disregards metadata.

   type Lexical_Env_Array_Record;
   type Lexical_Env_Array_Access is access all Lexical_Env_Array_Record;

   type Internal_Lexical_Env_Array is array (Positive range <>) of Lexical_Env;

   type Lexical_Env_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Lexical_Env_Array (1 .. N);
   end record;

   Empty_Lexical_Env_Array_Record : aliased Lexical_Env_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Lexical_Env_Array_Type : constant Lexical_Env_Array_Access :=
     Empty_Lexical_Env_Array_Record'Access;

   function Create_Lexical_Env_Array
     (Items_Count : Natural) return Lexical_Env_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Lexical_Env_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Lexical_Env;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access;

   function Length (T : Lexical_Env_Array_Access) return Natural;

   procedure Inc_Ref (T : Lexical_Env_Array_Access);
   procedure Dec_Ref (T : in out Lexical_Env_Array_Access);

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean;

   function Trace_Image (A : Lexical_Env_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Lexical_Env_Array_Record, Lexical_Env_Array_Access);

   type Internal_Entity_Array_Record;
   type Internal_Entity_Array_Access is
     access all Internal_Entity_Array_Record;

   type Internal_Internal_Entity_Array is
     array (Positive range <>) of Internal_Entity;

   type Internal_Entity_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Array (1 .. N);
   end record;

   Empty_Ada_Node_Array_Record : aliased Internal_Entity_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Array_Type : constant Internal_Entity_Array_Access :=
     Empty_Ada_Node_Array_Record'Access;

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access;

   function Create_Internal_Entity_Array
     (Items_Count : Natural) return Internal_Entity_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array)
      return Internal_Entity_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access;

   function Length (T : Internal_Entity_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Array_Access);

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Entity_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Array_Record, Internal_Entity_Array_Access);

   type Bare_Ada_Node_Array_Record;
   type Bare_Ada_Node_Array_Access is access all Bare_Ada_Node_Array_Record;

   type Internal_Bare_Ada_Node_Array is
     array (Positive range <>) of Bare_Ada_Node;

   type Bare_Ada_Node_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Bare_Ada_Node_Array (1 .. N);
   end record;

   Empty_Bare_Ada_Node_Array_Record : aliased Bare_Ada_Node_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Ada_Node_Array_Type : constant Bare_Ada_Node_Array_Access :=
     Empty_Bare_Ada_Node_Array_Record'Access;

   function Create_Bare_Ada_Node_Array
     (Items_Count : Natural) return Bare_Ada_Node_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Ada_Node_Array
     (Items : Internal_Bare_Ada_Node_Array) return Bare_Ada_Node_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Bare_Ada_Node_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Bare_Ada_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Bare_Ada_Node_Array_Access) return Bare_Ada_Node_Array_Access;

   function Length (T : Bare_Ada_Node_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Ada_Node_Array_Access);
   procedure Dec_Ref (T : in out Bare_Ada_Node_Array_Access);

   function Equivalent (L, R : Bare_Ada_Node_Array_Access) return Boolean;

   function Trace_Image (A : Bare_Ada_Node_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bare_Ada_Node_Array_Record, Bare_Ada_Node_Array_Access);

   function Hash (B : Boolean) return Hash_Type;

   function Hash (I : Integer) return Hash_Type;

   function Hash (Info : Internal_Entity_Info) return Hash_Type;

   function Hash (R : Internal_Entity) return Hash_Type;

   --------------------------
   -- Big integers wrapper --
   --------------------------

   type Big_Integer_Record is limited record
      Value     : GNATCOLL.GMP.Integers.Big_Integer;
      Ref_Count : Integer;
      --  Number of owners. When it drops to 0, this record can be destroyed.
      --  If -1, this is a static big integer: Inc_Ref and Dec_Ref are no-ops.
   end record;

   type Big_Integer_Type is access all Big_Integer_Record;

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type;
   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type;
   function Create_Big_Integer (Int : Integer) return Big_Integer_Type;
   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer;

   No_Big_Integer_Record : aliased Big_Integer_Record :=
     (Value => <>, Ref_Count => -1);
   No_Big_Integer : constant Big_Integer_Type := No_Big_Integer_Record'Access;

   function To_Integer (Big_Int : Big_Integer_Type) return Integer;
   --  Convert Big_Int into a regular integer, raising a Property_Error if it
   --  is out of range.

   procedure Inc_Ref (Big_Int : Big_Integer_Type);
   procedure Dec_Ref (Big_Int : in out Big_Integer_Type);

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean;
   function "<" (Left, Right : Big_Integer_Type) return Boolean;
   function "<=" (Left, Right : Big_Integer_Type) return Boolean;
   function ">" (Left, Right : Big_Integer_Type) return Boolean;
   function ">=" (Left, Right : Big_Integer_Type) return Boolean;

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type;

   function Trace_Image (I : Big_Integer_Type) return String;

   function Trace_Image
     (Node : Bare_Ada_Node; Decoration : Boolean := True) return String;

   function Is_Incomplete (Node : Bare_Ada_Node) return Boolean;
   --  Return whether this node is incomplete or not. Incomplete nodes are a
   --  result of the parsing of a node failing as a result of a NoBacktrack
   --  parser annotation.

   function Kind_Name (Node : Bare_Ada_Node) return String;
   --  Return the concrete kind for Node

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   Kind_To_Node_Children_Count : constant array
     (Ada_Node_Kind_Type) of Integer :=
     (Ada_Abort_Absent => 0, Ada_Abort_Present => 0, Ada_Abstract_Absent => 0,
      Ada_Abstract_Present                 => 0, Ada_Ada_Node_List => -1,
      Ada_Alternatives_List                => -1, Ada_Constraint_List => -1,
      Ada_Decl_List => -1, Ada_Stmt_List => -1, Ada_Aspect_Assoc_List => -1,
      Ada_Base_Assoc_List                  => -1, Ada_Assoc_List => -1,
      Ada_Case_Expr_Alternative_List       => -1,
      Ada_Case_Stmt_Alternative_List => -1, Ada_Compilation_Unit_List => -1,
      Ada_Contract_Case_Assoc_List         => -1, Ada_Defining_Name_List => -1,
      Ada_Discriminant_Spec_List => -1, Ada_Elsif_Expr_Part_List => -1,
      Ada_Elsif_Stmt_Part_List => -1, Ada_Enum_Literal_Decl_List => -1,
      Ada_Expr_Alternatives_List => -1, Ada_Discriminant_Choice_List => -1,
      Ada_Name_List => -1, Ada_Parent_List => -1, Ada_Param_Spec_List => -1,
      Ada_Pragma_Node_List => -1, Ada_Select_When_Part_List => -1,
      Ada_Unconstrained_Array_Index_List   => -1, Ada_Variant_List => -1,
      Ada_Aliased_Absent => 0, Ada_Aliased_Present => 0, Ada_All_Absent => 0,
      Ada_All_Present => 0, Ada_Constrained_Array_Indices => 1,
      Ada_Unconstrained_Array_Indices      => 1, Ada_Aspect_Assoc => 2,
      Ada_At_Clause                        => 2, Ada_Attribute_Def_Clause => 2,
      Ada_Enum_Rep_Clause                  => 2, Ada_Record_Rep_Clause => 3,
      Ada_Aspect_Spec                      => 1, Ada_Contract_Case_Assoc => 2,
      Ada_Pragma_Argument_Assoc            => 2, Ada_Entry_Spec => 3,
      Ada_Enum_Subp_Spec => 0, Ada_Subp_Spec => 4, Ada_Component_List => 2,
      Ada_Known_Discriminant_Part => 1, Ada_Unknown_Discriminant_Part => 0,
      Ada_Entry_Completion_Formal_Params   => 1, Ada_Generic_Formal_Part => 1,
      Ada_Null_Record_Def => 1, Ada_Record_Def => 1, Ada_Aggregate_Assoc => 2,
      Ada_Multi_Dim_Array_Assoc            => 2, Ada_Discriminant_Assoc => 2,
      Ada_Param_Assoc                      => 2, Ada_Component_Decl => 4,
      Ada_Discriminant_Spec => 3, Ada_Generic_Formal_Obj_Decl => 1,
      Ada_Generic_Formal_Package => 1, Ada_Generic_Formal_Subp_Decl => 1,
      Ada_Generic_Formal_Type_Decl         => 1, Ada_Param_Spec => 5,
      Ada_Generic_Package_Internal         => 5, Ada_Package_Decl => 5,
      Ada_Discrete_Base_Subtype_Decl       => 1, Ada_Subtype_Decl => 3,
      Ada_Classwide_Type_Decl              => 1, Ada_Incomplete_Type_Decl => 2,
      Ada_Incomplete_Tagged_Type_Decl      => 3, Ada_Protected_Type_Decl => 5,
      Ada_Task_Type_Decl => 4, Ada_Single_Task_Type_Decl => 4,
      Ada_Type_Decl                        => 4, Ada_Anonymous_Type_Decl => 4,
      Ada_Synth_Anonymous_Type_Decl        => 4, Ada_Abstract_Subp_Decl => 3,
      Ada_Abstract_Formal_Subp_Decl => 4, Ada_Concrete_Formal_Subp_Decl => 4,
      Ada_Subp_Decl => 3, Ada_Entry_Decl => 3, Ada_Enum_Literal_Decl => 1,
      Ada_Generic_Subp_Internal            => 2, Ada_Expr_Function => 4,
      Ada_Null_Subp_Decl => 3, Ada_Subp_Body => 6, Ada_Subp_Renaming_Decl => 4,
      Ada_Package_Body_Stub                => 2, Ada_Protected_Body_Stub => 2,
      Ada_Subp_Body_Stub => 3, Ada_Task_Body_Stub => 2, Ada_Entry_Body => 7,
      Ada_Package_Body => 5, Ada_Protected_Body => 4, Ada_Task_Body => 5,
      Ada_Entry_Index_Spec => 2, Ada_Error_Decl => 0, Ada_Exception_Decl => 3,
      Ada_Exception_Handler                => 3, Ada_For_Loop_Var_Decl => 2,
      Ada_Generic_Package_Decl             => 2, Ada_Generic_Subp_Decl => 2,
      Ada_Generic_Package_Instantiation    => 4,
      Ada_Generic_Subp_Instantiation       => 6,
      Ada_Generic_Package_Renaming_Decl    => 3,
      Ada_Generic_Subp_Renaming_Decl       => 4, Ada_Label_Decl => 1,
      Ada_Named_Stmt_Decl => 1, Ada_Number_Decl => 2, Ada_Object_Decl => 8,
      Ada_Anonymous_Object_Decl            => 8,
      Ada_Extended_Return_Stmt_Object_Decl => 8,
      Ada_Package_Renaming_Decl => 3, Ada_Single_Protected_Decl => 4,
      Ada_Single_Task_Decl => 1, Ada_Case_Stmt_Alternative => 2,
      Ada_Compilation_Unit                 => 3, Ada_Component_Clause => 3,
      Ada_Component_Def                    => 3, Ada_Constant_Absent => 0,
      Ada_Constant_Present                 => 0, Ada_Delta_Constraint => 2,
      Ada_Digits_Constraint => 2, Ada_Discriminant_Constraint => 1,
      Ada_Index_Constraint                 => 1, Ada_Range_Constraint => 1,
      Ada_Declarative_Part => 1, Ada_Private_Part => 1, Ada_Public_Part => 1,
      Ada_Elsif_Expr_Part => 2, Ada_Elsif_Stmt_Part => 2, Ada_Allocator => 2,
      Ada_Aggregate => 2, Ada_Null_Record_Aggregate => 2, Ada_Bin_Op => 3,
      Ada_Relation_Op => 3, Ada_Box_Expr => 0, Ada_Case_Expr => 2,
      Ada_Case_Expr_Alternative            => 2, Ada_Contract_Cases => 1,
      Ada_If_Expr => 4, Ada_Membership_Expr => 3, Ada_Attribute_Ref => 3,
      Ada_Update_Attribute_Ref             => 3, Ada_Call_Expr => 2,
      Ada_Defining_Name => 1, Ada_Discrete_Subtype_Name => 1,
      Ada_Dotted_Name => 2, Ada_End_Name => 1, Ada_Explicit_Deref => 1,
      Ada_Qual_Expr => 2, Ada_Char_Literal => 0, Ada_Identifier => 0,
      Ada_Op_Abs => 0, Ada_Op_And => 0, Ada_Op_And_Then => 0,
      Ada_Op_Concat => 0, Ada_Op_Div => 0, Ada_Op_Double_Dot => 0,
      Ada_Op_Eq => 0, Ada_Op_Gt => 0, Ada_Op_Gte => 0, Ada_Op_In => 0,
      Ada_Op_Lt => 0, Ada_Op_Lte => 0, Ada_Op_Minus => 0, Ada_Op_Mod => 0,
      Ada_Op_Mult => 0, Ada_Op_Neq => 0, Ada_Op_Not => 0, Ada_Op_Not_In => 0,
      Ada_Op_Or => 0, Ada_Op_Or_Else => 0, Ada_Op_Plus => 0, Ada_Op_Pow => 0,
      Ada_Op_Rem => 0, Ada_Op_Xor => 0, Ada_String_Literal => 0,
      Ada_Null_Literal => 0, Ada_Int_Literal => 0, Ada_Real_Literal => 0,
      Ada_Target_Name => 0, Ada_Paren_Expr => 1, Ada_Quantified_Expr => 3,
      Ada_Raise_Expr => 2, Ada_Un_Op => 2, Ada_Handled_Stmts => 2,
      Ada_Interface_Kind_Limited => 0, Ada_Interface_Kind_Protected => 0,
      Ada_Interface_Kind_Synchronized      => 0, Ada_Interface_Kind_Task => 0,
      Ada_Iter_Type_In => 0, Ada_Iter_Type_Of => 0, Ada_Library_Item => 2,
      Ada_Limited_Absent                   => 0, Ada_Limited_Present => 0,
      Ada_For_Loop_Spec => 4, Ada_While_Loop_Spec => 1, Ada_Mode_Default => 0,
      Ada_Mode_In => 0, Ada_Mode_In_Out => 0, Ada_Mode_Out => 0,
      Ada_Not_Null_Absent                  => 0, Ada_Not_Null_Present => 0,
      Ada_Null_Component_Decl              => 0, Ada_Others_Designator => 0,
      Ada_Overriding_Not_Overriding => 0, Ada_Overriding_Overriding => 0,
      Ada_Overriding_Unspecified => 0, Ada_Params => 1, Ada_Pragma_Node => 2,
      Ada_Prim_Type_Accessor               => 0, Ada_Private_Absent => 0,
      Ada_Private_Present                  => 0, Ada_Protected_Def => 3,
      Ada_Protected_Absent                 => 0, Ada_Protected_Present => 0,
      Ada_Quantifier_All => 0, Ada_Quantifier_Some => 0, Ada_Range_Spec => 1,
      Ada_Renaming_Clause => 1, Ada_Synthetic_Renaming_Clause => 1,
      Ada_Reverse_Absent                   => 0, Ada_Reverse_Present => 0,
      Ada_Select_When_Part                 => 2, Ada_Accept_Stmt => 3,
      Ada_Accept_Stmt_With_Stmts           => 5, Ada_For_Loop_Stmt => 3,
      Ada_Loop_Stmt => 3, Ada_While_Loop_Stmt => 3, Ada_Begin_Block => 2,
      Ada_Decl_Block => 3, Ada_Case_Stmt => 2, Ada_Extended_Return_Stmt => 2,
      Ada_If_Stmt => 4, Ada_Named_Stmt => 2, Ada_Select_Stmt => 3,
      Ada_Error_Stmt => 0, Ada_Abort_Stmt => 1, Ada_Assign_Stmt => 2,
      Ada_Call_Stmt => 1, Ada_Delay_Stmt => 2, Ada_Exit_Stmt => 2,
      Ada_Goto_Stmt => 1, Ada_Label => 1, Ada_Null_Stmt => 0,
      Ada_Raise_Stmt => 2, Ada_Requeue_Stmt => 2, Ada_Return_Stmt => 1,
      Ada_Terminate_Alternative            => 0, Ada_Subp_Kind_Function => 0,
      Ada_Subp_Kind_Procedure              => 0, Ada_Subunit => 2,
      Ada_Synchronized_Absent              => 0, Ada_Synchronized_Present => 0,
      Ada_Tagged_Absent => 0, Ada_Tagged_Present => 0, Ada_Task_Def => 4,
      Ada_Access_To_Subp_Def => 3, Ada_Anonymous_Type_Access_Def => 2,
      Ada_Type_Access_Def                  => 4, Ada_Array_Type_Def => 2,
      Ada_Derived_Type_Def                 => 7, Ada_Enum_Type_Def => 1,
      Ada_Formal_Discrete_Type_Def         => 0, Ada_Interface_Type_Def => 2,
      Ada_Mod_Int_Type_Def                 => 1, Ada_Private_Type_Def => 3,
      Ada_Decimal_Fixed_Point_Def          => 3, Ada_Floating_Point_Def => 2,
      Ada_Ordinary_Fixed_Point_Def         => 2, Ada_Record_Type_Def => 4,
      Ada_Signed_Int_Type_Def              => 1, Ada_Anonymous_Type => 1,
      Ada_Enum_Lit_Synth_Type_Expr         => 0, Ada_Subtype_Indication => 3,
      Ada_Constrained_Subtype_Indication   => 3,
      Ada_Discrete_Subtype_Indication => 3, Ada_Unconstrained_Array_Index => 1,
      Ada_Until_Absent                     => 0, Ada_Until_Present => 0,
      Ada_Use_Package_Clause => 1, Ada_Use_Type_Clause => 2, Ada_Variant => 2,
      Ada_Variant_Part                     => 2, Ada_With_Clause => 3,
      Ada_With_Private_Absent => 0, Ada_With_Private_Present => 0);
   --  For each AST node kind, this array gives the number of AST node children
   --  it has. For AST node lists, this is -1 as this number varies from one
   --  list instance to another.

   function First_Child_Index (Node : Bare_Ada_Node) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index (Node : Bare_Ada_Node) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   function Children_Count (Node : Bare_Ada_Node) return Natural;
   --  Return the number of children that Node has

   procedure Get_Child
     (Node   : Bare_Ada_Node; Index : Positive; Index_In_Bounds : out Boolean;
      Result : out Bare_Ada_Node);
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. Index is out-of-bounds), the content of
   --  Result is undefined.

   function Child
     (Node : Bare_Ada_Node; Index : Positive) return Bare_Ada_Node;
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : Bare_Ada_Node) return Internal_Bare_Ada_Node_Array;
   --  Return an array containing all the children of Node. This is an
   --  alternative to the Child/Children_Count pair, useful if you want
   --  the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node : Bare_Ada_Node; Include_Self : Boolean := True)
      return Bare_Ada_Node_Array_Access;
   --  Return the list of parents for this node. This node included in the list
   --  iff Include_Self.

   function Parent (Node : Bare_Ada_Node) return Bare_Ada_Node;

   function Fetch_Sibling
     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info; Offset : Integer)
      return Internal_Entity;
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or No_Entity if there is no such sibling.

   function Traverse
     (Node  : Bare_Ada_Node;
      Visit : access function (Node : Bare_Ada_Node) return Visit_Status)
      return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in prefix order
   --  (i.e. top-down). The order of traversing subtrees follows the order
   --  of declaration of the corresponding attributes in the grammar. The
   --  traversal is controlled as follows by the result returned by Visit:
   --
   --     Into   The traversal continues normally with the syntactic
   --            children of the node just processed.
   --
   --     Over   The children of the node just processed are skipped and
   --            excluded from the traversal, but otherwise processing
   --            continues elsewhere in the tree.
   --
   --     Stop   The entire traversal is immediately abandoned, and the
   --            original call to Traverse returns Stop.

   procedure Traverse
     (Node  : Bare_Ada_Node;
      Visit : access function (Node : Bare_Ada_Node) return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   generic
      type Data_Type is private;
      Reset_After_Traversal : Boolean := False;
   function Traverse_With_Data
     (Node  : Bare_Ada_Node;
      Visit : access function
        (Node : Bare_Ada_Node; Data : in out Data_Type) return Visit_Status;
      Data : in out Data_Type) return Visit_Status;
   --  This is the same as the first Traverse function except it accepts an
   --  argument that is passed to all Visit calls.
   --
   --  If Reset_After_Traversal is True, the Data formal is left unchanged when
   --  Traverse_With_Data returns no matter what Visit does. Visit can change
   --  it otherwise.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range (Node : Bare_Ada_Node) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Bare_Ada_Node; Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Node : Bare_Ada_Node; Sloc : Source_Location) return Bare_Ada_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.

   function Compare
     (Left, Right : Bare_Ada_Node; Relation : Comparison_Relation)
      return Boolean;
   --  If Left and Right don't belong to the same analysis units or if one of
   --  them is null, raise a Property_Error. Otherwise, return the comparison
   --  of their starting source location according to Relation.

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node : Bare_Ada_Node; Show_Slocs : Boolean; Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia (Node : Bare_Ada_Node; Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Ada_Node);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Text_Image (Ent : Internal_Entity) return Text_Type;
   function Image (Ent : Internal_Entity) return String;
   --  Return a representation of this entity as a string.

   package Eq_Node is new Langkit_Support.Adalog.Eq_Same
     (LR_Type => Internal_Entity, Element_Image => Image);
   subtype Logic_Var is Eq_Node.Refs.Raw_Var;
   subtype Logic_Var_Record is Eq_Node.Refs.Var;
   Null_Var        : constant Logic_Var        := null;
   Null_Var_Record : constant Logic_Var_Record :=
     (Reset => True, others => <>);

   subtype Logic_Equation is Relation;
   Null_Logic_Equation : constant Logic_Equation := null;

   function Trace_Image (K : Analysis_Unit_Kind) return String;
   function Trace_Image (B : Boolean) return String;
   function Trace_Image (I : Integer) return String;
   function Trace_Image (S : Symbol_Type) return String;
   function Trace_Image (Env : Lexical_Env) return String;
   function Trace_Image (E : Internal_Entity) return String;
   function Trace_Image (Info : Internal_Entity_Info) return String;
   function Trace_Image (R : Env_Rebindings) return String;
   function Trace_Image (Unit : Internal_Unit) return String;
   function Trace_Image (Eq : Logic_Equation) return String;
   function Trace_Image (Var : Logic_Var) return String;

   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

   type Internal_Entity_Expr;

   type Internal_Aspect;
   --  Composite field representing the aspect of an entity (RM 13).

   type Internal_Entity_Basic_Decl;

   type Internal_Completion_Item;

   type Internal_Discrete_Range;
   --  Represent the range of a discrete type or subtype. The bounds are not
   --  evaluated, you need to call ``eval_as_int`` on them, if they're static,
   --  to get their value.

   type Internal_Doc_Annotation;
   --  Documentation annotation.

   type Internal_Entity_Abort_Node;

   type Internal_Entity_Abort_Absent;

   type Internal_Entity_Abort_Present;

   type Internal_Entity_Stmt;

   type Internal_Entity_Simple_Stmt;

   type Internal_Entity_Abort_Stmt;

   type Internal_Entity_Abstract_Node;

   type Internal_Entity_Abstract_Absent;

   type Internal_Entity_Basic_Subp_Decl;

   type Internal_Entity_Classic_Subp_Decl;

   type Internal_Entity_Formal_Subp_Decl;

   type Internal_Entity_Abstract_Formal_Subp_Decl;

   type Internal_Entity_Abstract_Present;

   type Internal_Entity_Abstract_Subp_Decl;

   type Internal_Entity_Composite_Stmt;

   type Internal_Entity_Accept_Stmt;

   type Internal_Entity_Accept_Stmt_With_Stmts;

   type Internal_Entity_Type_Def;

   type Internal_Entity_Access_Def;

   type Internal_Entity_Access_To_Subp_Def;

   type Internal_Entity_Ada_List;

   type Internal_Entity_Ada_Node_List;

   type Internal_Entity_Base_Aggregate;

   type Internal_Entity_Aggregate;

   type Internal_Entity_Basic_Assoc;

   type Internal_Entity_Aggregate_Assoc;

   type Internal_Entity_Aliased_Node;

   type Internal_Entity_Aliased_Absent;

   type Internal_Entity_Aliased_Present;

   type Internal_Entity_All_Node;

   type Internal_Entity_All_Absent;

   type Internal_Entity_All_Present;

   type Internal_Entity_Allocator;

   type Internal_Entity_Alternatives_List;

   type Internal_Entity_Object_Decl;

   type Internal_Entity_Anonymous_Object_Decl;

   type Internal_Entity_Type_Expr;

   type Internal_Entity_Anonymous_Type;

   type Internal_Entity_Base_Type_Access_Def;

   type Internal_Entity_Anonymous_Type_Access_Def;

   type Internal_Entity_Base_Type_Decl;

   type Internal_Entity_Type_Decl;

   type Internal_Entity_Anonymous_Type_Decl;

   type Internal_Entity_Array_Indices;

   type Internal_Entity_Array_Type_Def;

   type Internal_Entity_Aspect_Assoc;

   type Internal_Entity_Aspect_Assoc_List;

   type Internal_Entity_Aspect_Clause;

   type Internal_Entity_Aspect_Spec;

   type Internal_Entity_Assign_Stmt;

   type Internal_Entity_Basic_Assoc_List;

   type Internal_Entity_Assoc_List;

   type Internal_Entity_At_Clause;

   type Internal_Entity_Attribute_Def_Clause;

   type Internal_Entity_Name;

   type Internal_Entity_Attribute_Ref;

   type Internal_Entity_Base_Assoc;

   type Internal_Entity_Base_Assoc_List;

   type Internal_Entity_Base_Formal_Param_Decl;

   type Internal_Entity_Base_Formal_Param_Holder;

   type Internal_Entity_Single_Tok_Node;

   type Internal_Entity_Base_Id;

   type Internal_Entity_Base_Loop_Stmt;

   type Internal_Entity_Base_Package_Decl;

   type Internal_Entity_Base_Record_Def;

   type Internal_Entity_Body_Node;

   type Internal_Entity_Base_Subp_Body;

   type Internal_Entity_Base_Subp_Spec;

   type Internal_Entity_Base_Subtype_Decl;

   type Internal_Entity_Block_Stmt;

   type Internal_Entity_Begin_Block;

   type Internal_Entity_Bin_Op;

   type Internal_Entity_Body_Stub;

   type Internal_Entity_Box_Expr;

   type Internal_Entity_Call_Expr;

   type Internal_Entity_Call_Stmt;

   type Internal_Entity_Case_Expr;

   type Internal_Entity_Case_Expr_Alternative;

   type Internal_Entity_Case_Expr_Alternative_List;

   type Internal_Entity_Case_Stmt;

   type Internal_Entity_Case_Stmt_Alternative;

   type Internal_Entity_Case_Stmt_Alternative_List;

   type Internal_Entity_Char_Literal;

   type Internal_Entity_Classwide_Type_Decl;

   type Internal_Entity_Compilation_Unit;

   type Internal_Entity_Compilation_Unit_List;

   type Internal_Entity_Component_Clause;

   type Internal_Entity_Component_Decl;

   type Internal_Entity_Component_Def;

   type Internal_Entity_Component_List;

   type Internal_Entity_Concrete_Formal_Subp_Decl;

   type Internal_Entity_Constant_Node;

   type Internal_Entity_Constant_Absent;

   type Internal_Entity_Constant_Present;

   type Internal_Entity_Constrained_Array_Indices;

   type Internal_Entity_Subtype_Indication;

   type Internal_Entity_Constrained_Subtype_Indication;

   type Internal_Entity_Constraint;

   type Internal_Entity_Constraint_List;

   type Internal_Entity_Contract_Case_Assoc;

   type Internal_Entity_Contract_Case_Assoc_List;

   type Internal_Entity_Contract_Cases;

   type Internal_Entity_Real_Type_Def;

   type Internal_Entity_Decimal_Fixed_Point_Def;

   type Internal_Entity_Decl_Block;

   type Internal_Entity_Decl_List;

   type Internal_Entity_Declarative_Part;

   type Internal_Entity_Defining_Name;

   type Internal_Entity_Defining_Name_List;

   type Internal_Entity_Delay_Stmt;

   type Internal_Entity_Delta_Constraint;

   type Internal_Entity_Derived_Type_Def;

   type Internal_Entity_Digits_Constraint;

   type Internal_Entity_Discrete_Base_Subtype_Decl;

   type Internal_Entity_Discrete_Subtype_Indication;

   type Internal_Entity_Discrete_Subtype_Name;

   type Internal_Entity_Discriminant_Assoc;

   type Internal_Entity_Identifier_List;

   type Internal_Entity_Discriminant_Choice_List;

   type Internal_Entity_Discriminant_Constraint;

   type Internal_Entity_Discriminant_Part;

   type Internal_Entity_Discriminant_Spec;

   type Internal_Entity_Discriminant_Spec_List;

   type Internal_Entity_Dotted_Name;

   type Internal_Entity_Elsif_Expr_Part;

   type Internal_Entity_Elsif_Expr_Part_List;

   type Internal_Entity_Elsif_Stmt_Part;

   type Internal_Entity_Elsif_Stmt_Part_List;

   type Internal_Entity_End_Name;

   type Internal_Entity_Entry_Body;

   type Internal_Entity_Entry_Completion_Formal_Params;

   type Internal_Entity_Entry_Decl;

   type Internal_Entity_Entry_Index_Spec;

   type Internal_Entity_Entry_Spec;

   type Internal_Entity_Enum_Lit_Synth_Type_Expr;

   type Internal_Entity_Enum_Literal_Decl;

   type Internal_Entity_Enum_Literal_Decl_List;

   type Internal_Entity_Enum_Rep_Clause;

   type Internal_Entity_Enum_Subp_Spec;

   type Internal_Entity_Enum_Type_Def;

   type Internal_Entity_Error_Decl;

   type Internal_Entity_Error_Stmt;

   type Internal_Entity_Exception_Decl;

   type Internal_Entity_Exception_Handler;

   type Internal_Entity_Exit_Stmt;

   type Internal_Entity_Explicit_Deref;

   type Internal_Entity_Expr_List;

   type Internal_Entity_Expr_Alternatives_List;

   type Internal_Entity_Expr_Function;

   type Internal_Entity_Extended_Return_Stmt;

   type Internal_Entity_Extended_Return_Stmt_Object_Decl;

   type Internal_Entity_Floating_Point_Def;

   type Internal_Entity_Loop_Spec;

   type Internal_Entity_For_Loop_Spec;

   type Internal_Entity_For_Loop_Stmt;

   type Internal_Entity_For_Loop_Var_Decl;

   type Internal_Entity_Formal_Discrete_Type_Def;

   type Internal_Entity_Generic_Decl;

   type Internal_Entity_Generic_Formal;

   type Internal_Entity_Generic_Formal_Obj_Decl;

   type Internal_Entity_Generic_Formal_Package;

   type Internal_Entity_Generic_Formal_Part;

   type Internal_Entity_Generic_Formal_Subp_Decl;

   type Internal_Entity_Generic_Formal_Type_Decl;

   type Internal_Entity_Generic_Instantiation;

   type Internal_Entity_Generic_Package_Decl;

   type Internal_Entity_Generic_Package_Instantiation;

   type Internal_Entity_Generic_Package_Internal;

   type Internal_Entity_Generic_Renaming_Decl;

   type Internal_Entity_Generic_Package_Renaming_Decl;

   type Internal_Entity_Generic_Subp_Decl;

   type Internal_Entity_Generic_Subp_Instantiation;

   type Internal_Entity_Generic_Subp_Internal;

   type Internal_Entity_Generic_Subp_Renaming_Decl;

   type Internal_Entity_Goto_Stmt;

   type Internal_Entity_Handled_Stmts;

   type Internal_Entity_Identifier;

   type Internal_Entity_If_Expr;

   type Internal_Entity_If_Stmt;

   type Internal_Entity_Incomplete_Type_Decl;

   type Internal_Entity_Incomplete_Tagged_Type_Decl;

   type Internal_Entity_Index_Constraint;

   type Internal_Entity_Num_Literal;

   type Internal_Entity_Int_Literal;

   type Internal_Entity_Interface_Kind;

   type Internal_Entity_Interface_Kind_Limited;

   type Internal_Entity_Interface_Kind_Protected;

   type Internal_Entity_Interface_Kind_Synchronized;

   type Internal_Entity_Interface_Kind_Task;

   type Internal_Entity_Interface_Type_Def;

   type Internal_Entity_Iter_Type;

   type Internal_Entity_Iter_Type_In;

   type Internal_Entity_Iter_Type_Of;

   type Internal_Entity_Known_Discriminant_Part;

   type Internal_Entity_Label;

   type Internal_Entity_Label_Decl;

   type Internal_Entity_Library_Item;

   type Internal_Entity_Limited_Node;

   type Internal_Entity_Limited_Absent;

   type Internal_Entity_Limited_Present;

   type Internal_Entity_Loop_Stmt;

   type Internal_Entity_Membership_Expr;

   type Internal_Entity_Mod_Int_Type_Def;

   type Internal_Entity_Mode;

   type Internal_Entity_Mode_Default;

   type Internal_Entity_Mode_In;

   type Internal_Entity_Mode_In_Out;

   type Internal_Entity_Mode_Out;

   type Internal_Entity_Multi_Dim_Array_Assoc;

   type Internal_Entity_Name_List;

   type Internal_Entity_Named_Stmt;

   type Internal_Entity_Named_Stmt_Decl;

   type Internal_Entity_Not_Null;

   type Internal_Entity_Not_Null_Absent;

   type Internal_Entity_Not_Null_Present;

   type Internal_Entity_Null_Component_Decl;

   type Internal_Entity_Null_Literal;

   type Internal_Entity_Null_Record_Aggregate;

   type Internal_Entity_Null_Record_Def;

   type Internal_Entity_Null_Stmt;

   type Internal_Entity_Null_Subp_Decl;

   type Internal_Entity_Number_Decl;

   type Internal_Entity_Op;

   type Internal_Entity_Op_Abs;

   type Internal_Entity_Op_And;

   type Internal_Entity_Op_And_Then;

   type Internal_Entity_Op_Concat;

   type Internal_Entity_Op_Div;

   type Internal_Entity_Op_Double_Dot;

   type Internal_Entity_Op_Eq;

   type Internal_Entity_Op_Gt;

   type Internal_Entity_Op_Gte;

   type Internal_Entity_Op_In;

   type Internal_Entity_Op_Lt;

   type Internal_Entity_Op_Lte;

   type Internal_Entity_Op_Minus;

   type Internal_Entity_Op_Mod;

   type Internal_Entity_Op_Mult;

   type Internal_Entity_Op_Neq;

   type Internal_Entity_Op_Not;

   type Internal_Entity_Op_Not_In;

   type Internal_Entity_Op_Or;

   type Internal_Entity_Op_Or_Else;

   type Internal_Entity_Op_Plus;

   type Internal_Entity_Op_Pow;

   type Internal_Entity_Op_Rem;

   type Internal_Entity_Op_Xor;

   type Internal_Entity_Ordinary_Fixed_Point_Def;

   type Internal_Entity_Others_Designator;

   type Internal_Entity_Overriding_Node;

   type Internal_Entity_Overriding_Not_Overriding;

   type Internal_Entity_Overriding_Overriding;

   type Internal_Entity_Overriding_Unspecified;

   type Internal_Entity_Package_Body;

   type Internal_Entity_Package_Body_Stub;

   type Internal_Entity_Package_Decl;

   type Internal_Entity_Package_Renaming_Decl;

   type Internal_Entity_Param_Assoc;

   type Internal_Entity_Param_Spec;

   type Internal_Entity_Param_Spec_List;

   type Internal_Entity_Params;

   type Internal_Entity_Paren_Expr;

   type Internal_Entity_Parent_List;

   type Internal_Entity_Pragma_Argument_Assoc;

   type Internal_Entity_Pragma_Node;

   type Internal_Entity_Pragma_Node_List;

   type Internal_Entity_Prim_Type_Accessor;

   type Internal_Entity_Private_Node;

   type Internal_Entity_Private_Absent;

   type Internal_Entity_Private_Part;

   type Internal_Entity_Private_Present;

   type Internal_Entity_Private_Type_Def;

   type Internal_Entity_Protected_Node;

   type Internal_Entity_Protected_Absent;

   type Internal_Entity_Protected_Body;

   type Internal_Entity_Protected_Body_Stub;

   type Internal_Entity_Protected_Def;

   type Internal_Entity_Protected_Present;

   type Internal_Entity_Protected_Type_Decl;

   type Internal_Entity_Public_Part;

   type Internal_Entity_Qual_Expr;

   type Internal_Entity_Quantified_Expr;

   type Internal_Entity_Quantifier;

   type Internal_Entity_Quantifier_All;

   type Internal_Entity_Quantifier_Some;

   type Internal_Entity_Raise_Expr;

   type Internal_Entity_Raise_Stmt;

   type Internal_Entity_Range_Constraint;

   type Internal_Entity_Range_Spec;

   type Internal_Entity_Real_Literal;

   type Internal_Entity_Record_Def;

   type Internal_Entity_Record_Rep_Clause;

   type Internal_Entity_Record_Type_Def;

   type Internal_Entity_Relation_Op;

   type Internal_Entity_Renaming_Clause;

   type Internal_Entity_Requeue_Stmt;

   type Internal_Entity_Return_Stmt;

   type Internal_Entity_Reverse_Node;

   type Internal_Entity_Reverse_Absent;

   type Internal_Entity_Reverse_Present;

   type Internal_Entity_Select_Stmt;

   type Internal_Entity_Select_When_Part;

   type Internal_Entity_Select_When_Part_List;

   type Internal_Entity_Signed_Int_Type_Def;

   type Internal_Entity_Single_Protected_Decl;

   type Internal_Entity_Single_Task_Decl;

   type Internal_Entity_Task_Type_Decl;

   type Internal_Entity_Single_Task_Type_Decl;

   type Internal_Entity_Stmt_List;

   type Internal_Entity_String_Literal;

   type Internal_Entity_Subp_Body;

   type Internal_Entity_Subp_Body_Stub;

   type Internal_Entity_Subp_Decl;

   type Internal_Entity_Subp_Kind;

   type Internal_Entity_Subp_Kind_Function;

   type Internal_Entity_Subp_Kind_Procedure;

   type Internal_Entity_Subp_Renaming_Decl;

   type Internal_Entity_Subp_Spec;

   type Internal_Entity_Subtype_Decl;

   type Internal_Entity_Subunit;

   type Internal_Entity_Synchronized_Node;

   type Internal_Entity_Synchronized_Absent;

   type Internal_Entity_Synchronized_Present;

   type Internal_Entity_Synth_Anonymous_Type_Decl;

   type Internal_Entity_Synthetic_Renaming_Clause;

   type Internal_Entity_Tagged_Node;

   type Internal_Entity_Tagged_Absent;

   type Internal_Entity_Tagged_Present;

   type Internal_Entity_Target_Name;

   type Internal_Entity_Task_Body;

   type Internal_Entity_Task_Body_Stub;

   type Internal_Entity_Task_Def;

   type Internal_Entity_Terminate_Alternative;

   type Internal_Entity_Type_Access_Def;

   type Internal_Entity_Un_Op;

   type Internal_Entity_Unconstrained_Array_Index;

   type Internal_Entity_Unconstrained_Array_Index_List;

   type Internal_Entity_Unconstrained_Array_Indices;

   type Internal_Entity_Unknown_Discriminant_Part;

   type Internal_Entity_Until_Node;

   type Internal_Entity_Until_Absent;

   type Internal_Entity_Until_Present;

   type Internal_Entity_Update_Attribute_Ref;

   type Internal_Entity_Use_Clause;

   type Internal_Entity_Use_Package_Clause;

   type Internal_Entity_Use_Type_Clause;

   type Internal_Entity_Variant;

   type Internal_Entity_Variant_List;

   type Internal_Entity_Variant_Part;

   type Internal_Entity_While_Loop_Spec;

   type Internal_Entity_While_Loop_Stmt;

   type Internal_Entity_With_Clause;

   type Internal_Entity_With_Private;

   type Internal_Entity_With_Private_Absent;

   type Internal_Entity_With_Private_Present;

   type Internal_Env_Assoc;

   type Internal_Eval_Discrete_Range;
   --  Represent the range of a discrete type or subtype. The bounds are
   --  already evaluated, so the type of the fields is BigInt.

   type Internal_Expected_Type_For_Expr;
   --  Struct used by ``potential_actuals_for_dispatch`` to store an expression
   --  together with the type that is expected for it.

   type Internal_Logic_Val_Result;
   --  Represent the result of a call to logic_val. ``success`` is True iff
   --  solving the logic equation was successful, and ``value`` holds the
   --  value of the logic variable.

   type Internal_Multidim_Aggregate_Info;
   --  Struct enclosing information about aggregates for multidimensional array
   --  types.

   type Internal_Param_Actual;
   --  Data structure used by zip_with_params property. Associates an
   --  expression (the actual) to a formal param declaration (the parameter).

   type Internal_Single_Actual;

   type Internal_Single_Formal;

   type Internal_Param_Match;
   --  Helper data structure to implement SubpSpec/ParamAssocList matching.
   --
   --  Each value relates to one ParamAssoc.

   type Internal_Ref_Result;
   --  Result for a cross reference query returning a reference.

   type Internal_Refd_Decl;
   --  Result for a cross reference query returning a referenced decl.

   type Internal_Refd_Def;
   --  Result for a cross reference query returning a referenced defining name.

   type Internal_Substitution;
   --  Represent a substitution of a BasicDecl by a given value. This can then
   --  be used as part of an environment in the eval_as_*_in_env property. See
   --  the declaration of those properties for more details.

   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

   type Bare_Compilation_Unit_Array_Record;
   type Bare_Compilation_Unit_Array_Access is
     access all Bare_Compilation_Unit_Array_Record;

   type Bare_Name_Array_Record;
   type Bare_Name_Array_Access is access all Bare_Name_Array_Record;

   type Bare_Single_Tok_Node_Array_Record;
   type Bare_Single_Tok_Node_Array_Access is
     access all Bare_Single_Tok_Node_Array_Record;

   type Character_Type_Array_Record;
   type Character_Type_Array_Access is access all Character_Type_Array_Record;

   type Character_Type_Array_Access_Array_Record;
   type Character_Type_Array_Access_Array_Access is
     access all Character_Type_Array_Access_Array_Record;

   type Internal_Completion_Item_Array_Record;
   type Internal_Completion_Item_Array_Access is
     access all Internal_Completion_Item_Array_Record;

   type Internal_Doc_Annotation_Array_Record;
   type Internal_Doc_Annotation_Array_Access is
     access all Internal_Doc_Annotation_Array_Record;

   type Internal_Entity_Aspect_Assoc_Array_Record;
   type Internal_Entity_Aspect_Assoc_Array_Access is
     access all Internal_Entity_Aspect_Assoc_Array_Record;

   type Internal_Entity_Base_Formal_Param_Decl_Array_Record;
   type Internal_Entity_Base_Formal_Param_Decl_Array_Access is
     access all Internal_Entity_Base_Formal_Param_Decl_Array_Record;

   type Internal_Entity_Base_Type_Decl_Array_Record;
   type Internal_Entity_Base_Type_Decl_Array_Access is
     access all Internal_Entity_Base_Type_Decl_Array_Record;

   type Internal_Entity_Basic_Assoc_Array_Record;
   type Internal_Entity_Basic_Assoc_Array_Access is
     access all Internal_Entity_Basic_Assoc_Array_Record;

   type Internal_Entity_Basic_Decl_Array_Record;
   type Internal_Entity_Basic_Decl_Array_Access is
     access all Internal_Entity_Basic_Decl_Array_Record;

   type Internal_Entity_Compilation_Unit_Array_Record;
   type Internal_Entity_Compilation_Unit_Array_Access is
     access all Internal_Entity_Compilation_Unit_Array_Record;

   type Internal_Entity_Defining_Name_Array_Record;
   type Internal_Entity_Defining_Name_Array_Access is
     access all Internal_Entity_Defining_Name_Array_Record;

   type Internal_Entity_Generic_Instantiation_Array_Record;
   type Internal_Entity_Generic_Instantiation_Array_Access is
     access all Internal_Entity_Generic_Instantiation_Array_Record;

   type Internal_Entity_Param_Spec_Array_Record;
   type Internal_Entity_Param_Spec_Array_Access is
     access all Internal_Entity_Param_Spec_Array_Record;

   type Internal_Entity_Pragma_Node_Array_Record;
   type Internal_Entity_Pragma_Node_Array_Access is
     access all Internal_Entity_Pragma_Node_Array_Record;

   type Internal_Entity_Type_Decl_Array_Record;
   type Internal_Entity_Type_Decl_Array_Access is
     access all Internal_Entity_Type_Decl_Array_Record;

   type Internal_Entity_Type_Expr_Array_Record;
   type Internal_Entity_Type_Expr_Array_Access is
     access all Internal_Entity_Type_Expr_Array_Record;

   type Internal_Entity_Variant_Array_Record;
   type Internal_Entity_Variant_Array_Access is
     access all Internal_Entity_Variant_Array_Record;

   type Internal_Env_Assoc_Array_Record;
   type Internal_Env_Assoc_Array_Access is
     access all Internal_Env_Assoc_Array_Record;

   type Internal_Expected_Type_For_Expr_Array_Record;
   type Internal_Expected_Type_For_Expr_Array_Access is
     access all Internal_Expected_Type_For_Expr_Array_Record;

   type Internal_Param_Actual_Array_Record;
   type Internal_Param_Actual_Array_Access is
     access all Internal_Param_Actual_Array_Record;

   type Internal_Param_Match_Array_Record;
   type Internal_Param_Match_Array_Access is
     access all Internal_Param_Match_Array_Record;

   type Internal_Ref_Result_Array_Record;
   type Internal_Ref_Result_Array_Access is
     access all Internal_Ref_Result_Array_Record;

   type Internal_Single_Actual_Array_Record;
   type Internal_Single_Actual_Array_Access is
     access all Internal_Single_Actual_Array_Record;

   type Internal_Single_Formal_Array_Record;
   type Internal_Single_Formal_Array_Access is
     access all Internal_Single_Formal_Array_Record;

   type Internal_Substitution_Array_Record;
   type Internal_Substitution_Array_Access is
     access all Internal_Substitution_Array_Record;

   type Internal_Unit_Array_Record;
   type Internal_Unit_Array_Access is access all Internal_Unit_Array_Record;

   type Logic_Equation_Array_Record;
   type Logic_Equation_Array_Access is access all Logic_Equation_Array_Record;

   type Symbol_Type_Array_Record;
   type Symbol_Type_Array_Access is access all Symbol_Type_Array_Record;

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   function Ada_Node_P_Declarative_Scope

     (Node : Bare_Ada_Node)
return Bare_Declarative_Part;
--  Return the scope of definition of this basic declaration.

   function Dispatcher_Ada_Node_P_Custom_Id_Text

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;
--  Custom Unique identifying text used to recognize this node. Not applicable
--  to all nodes, but on AdaNode because it spans more than one hierarchy of
--  node types.

   function Ada_Node_P_In_Prepost
(Node : Bare_Ada_Node)
return Boolean;

   function Ada_Node_P_In_Aspect

     (Node : Bare_Ada_Node; Name : Symbol_Type)
return Boolean;
--  Return whether Self is contained by an aspect whose name is ``name``.

   function Ada_Node_P_Empty_Env
(Node : Bare_Ada_Node)
return Lexical_Env;

   function Ada_Node_P_Is_Not_Null

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return True iff this node is not null.

   function Ada_Node_P_Eval_Discrete_Range

     (Node : Bare_Ada_Node; Dr : Internal_Discrete_Range)

      return Internal_Eval_Discrete_Range;
--  Static method. Evaluate the bounds of ``dr``.

   function Ada_Node_P_String_Join

     (Node : Bare_Ada_Node; Strns : Character_Type_Array_Access_Array_Access;
      Sep  : Character_Type_Array_Access)
return Character_Type_Array_Access;
--  Static method. Return the array of strings joined by separator ``sep``.

   function Ada_Node_P_Sym_Join

     (Node   : Bare_Ada_Node; Syms : Symbol_Type_Array_Access;
      Sep    : Character_Type_Array_Access;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;
--  Static method. Return the array of symbols joined by separator ``sep``.

   function Ada_Node_P_Enclosing_Compilation_Unit

     (Node : Bare_Ada_Node)
return Bare_Compilation_Unit;
--  Return the compilation unit containing this node.
--
--  .. note:: This returns the ``CompilationUnit`` node, which is different
--     from the ``AnalysisUnit``. In particular, an analysis unit can contain
--     multiple compilation units.

   function Ada_Node_P_Is_Children_Env

     (Node : Bare_Ada_Node; Parent : Lexical_Env; Current_Env : Lexical_Env)

      return Boolean;
--  Static property. Will return True if current_env is a children of parent.

   function Ada_Node_P_Trigger_Access_Entity

     (Node   : Bare_Ada_Node; Val : Boolean;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Internal_Entity;
--  Return Self as an entity, but with the ``access_entity`` field set to val.
--  Helper for the 'Unrestricted_Access machinery.

   function Ada_Node_P_Without_Md

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity;
--  Return Entity with an empty metadata field.

   function Dispatcher_Ada_Node_P_Complete

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Completion_Item_Array_Access;
--  Return possible completions at this point in the file.

   function Ada_Node_P_Valid_Keywords

     (Node : Bare_Ada_Node)
return Symbol_Type_Array_Access;
--  Return the list of keywords that are valid at this point in the file.
--
--  .. note:: This is work in progress. It will return all keywords for now,
--     without looking at the context.

   function Ada_Node_P_Generic_Instantiations

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Generic_Instantiation_Array_Access;
--  Return the potentially empty list of generic package/subprogram
--  instantiations that led to the creation of this entity. Outer-most
--  instantiations appear last.

   function Ada_Node_P_Generic_Instantiations_Internal

     (Node : Bare_Ada_Node; R : Env_Rebindings)

      return Internal_Entity_Generic_Instantiation_Array_Access;

   function Ada_Node_P_Logic_Val

     (Node : Bare_Ada_Node; From_Node : Internal_Entity; Lvar : Logic_Var)

      return Internal_Logic_Val_Result;

   function Ada_Node_P_Semantic_Parent_Helper

     (Node   : Bare_Ada_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Internal_Entity;

   function Ada_Node_P_Semantic_Parent

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity;
--  Return the semantic parent for this node, if applicable, null otherwise.

   function Ada_Node_P_Parent_Basic_Decl

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the parent basic decl for this node, if applicable, null otherwise.
--
--  .. note:: If the parent BasicDecl of the given node is a generic
--     declaration, this call will return the instantiation from which the node
--     was retrieved instead, if any.

   function Ada_Node_P_Designated_Compilation_Unit

     (Node : Bare_Ada_Node; Name : Symbol_Type_Array_Access;
      Kind : Analysis_Unit_Kind; Load_If_Needed : Boolean := True)

      return Bare_Compilation_Unit;
--  Fetch the compilation unit designated by the given name defined in an
--  analysis unit of the given kind.

   function Ada_Node_P_Get_Unit_Root_Decl

     (Node : Bare_Ada_Node; Name : Symbol_Type_Array_Access;
      Kind : Analysis_Unit_Kind; Load_If_Needed : Boolean := False)

      return Bare_Basic_Decl;
--  If the corresponding analysis unit is loaded, return the root decl node for
--  the given analysis unit ``kind`` and correpsonding to the name ``name``. If
--  it's not loaded, return none.

   function Dispatcher_Ada_Node_P_Xref_Equation

     (Node   : Bare_Ada_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  This is the base property for constructing equations that, when solved,
--  will resolve names and types for every sub expression of the expression you
--  call it on. Note that if you call that on any expression, in some context
--  it might lack full information and return multiple solutions. If you want
--  completely precise resolution, you must call that on the outermost node
--  that supports xref_equation.

   function Dispatcher_Ada_Node_P_Xref_Stop_Resolution

     (Node : Bare_Ada_Node)
return Boolean;

   function Ada_Node_P_Stop_Resolution_Equation

     (Node : Bare_Ada_Node)
return Logic_Equation;

   function Ada_Node_P_Sub_Equation

     (Node   : Bare_Ada_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Wrapper for xref_equation, meant to be used inside of xref_equation when
--  you want to get the sub equation of a sub expression. It is used to change
--  the behavior when xref_equation is called from another xref_equation call,
--  or from the top level, so that we can do resolution in several steps.

   function Ada_Node_P_Resolve_Own_Names

     (Node   : Bare_Ada_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Internal helper for resolve_names. Resolve names for this node up to
--  xref_entry_point and xref_stop_resolution boundaries.

   function Ada_Node_P_Resolve_Children_Names

     (Node   : Bare_Ada_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Internal helper for resolve_names, implementing the recursive logic needed
--  to resolve names across xref_stop_resolution boundaries.

   function Ada_Node_P_Resolve_Names_Internal

     (Node   : Bare_Ada_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Resolves names for this node up to xref_entry_point boundaries.

   function Ada_Node_P_Resolve_Names_Internal_With_Eq

     (Node   : Bare_Ada_Node; Additional_Equation : Logic_Equation;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Resolves names in this node with an additional constraint given by
--  ``additional_equation``, up to xref_entry_point boundaries.

   function Dispatcher_Ada_Node_P_Xref_Entry_Point

     (Node : Bare_Ada_Node)
return Boolean;
--  Designates entities that are entry point for the xref solving
--  infrastructure. If this returns true, then resolve_names can be called
--  on it.
--
--  .. note:: For convenience, and unlike what is defined in the ARM wrt.
--     complete contexts for name resolution, ``xref_entry_points`` can be
--     nested.

   function Ada_Node_P_Resolve_Names

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  This will resolve names for this node. If the operation is successful,
--  then type_var and ref_var will be bound on appropriate subnodes of the
--  statement.

   function Ada_Node_P_Resolve_Names_From_Closest_Entry_Point

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Resolve names from the closest entry point up to this node. Note that
--  unlike ``resolve_names``, this will *not* trigger resolution of every node
--  with stop_resolution that lie in the sub-tree formed by the closest entry
--  point. It will only resolve those that are in the path to resolving Self.
--  Consider for example the following entry point:
--
--  .. code::
--
--  R := (A, B);
--
--  Since aggregate association nodes have ``stop_resolution`` set to True,
--  calling ``resolve_names_from_closest_entry_point`` on ``B`` will resolve
--  nodes ``R`` and ``B`` but not ``A``, because ``A`` does not lie on the
--  path to ``B``.
--
--  This can be useful for resolving aggregates of variant records, because
--  resolution of a component association can safely call the resolution of a
--  discriminant association without triggering an infinite recursion, as both
--  are on different "paths".

   function Ada_Node_P_Parent_Unit_Env_Helper

     (Node : Bare_Ada_Node; Unit : Internal_Unit; Env : Lexical_Env)

      return Lexical_Env;

   function Ada_Node_P_Parent_Unit_Env

     (Node : Bare_Ada_Node; Env : Lexical_Env)
return Lexical_Env;
--  Given env's AnalysisUnit, return the first env that has a different
--  analysis unit in the env parent chain.

   function Ada_Node_P_Std

     (Node : Bare_Ada_Node)
return Internal_Entity_Basic_Decl;
--  Retrieves the package corresponding to the Standard unit. Used to access
--  standard types.

   function Ada_Node_P_Std_Env
(Node : Bare_Ada_Node)
return Lexical_Env;
--  Get the children env of the Standard package.

   function Ada_Node_P_Std_Entity

     (Node : Bare_Ada_Node; Sym : Symbol_Type)
return Internal_Entity;
--  Static property. Return an entity from the standard package with name
--  `sym`.

   function Ada_Node_P_Std_Entity_Implem

     (Node : Bare_Ada_Node; Sym : Symbol_Type)
return Internal_Entity;

   function Ada_Node_P_Bool_Type
(Node : Bare_Ada_Node)
return Internal_Entity;
--  Static method. Return the standard Boolean type.

   function Ada_Node_P_Int_Type
(Node : Bare_Ada_Node)
return Internal_Entity;
--  Static method. Return the standard Integer type.

   function Ada_Node_P_Universal_Int_Type

     (Node : Bare_Ada_Node)
return Internal_Entity;
--  Static method. Return the standard Universal Integer type.

   function Ada_Node_P_Universal_Real_Type

     (Node : Bare_Ada_Node)
return Internal_Entity;
--  Static method. Return the standard Universal Real type.

   function Ada_Node_P_Exc_Id_Type

     (Node : Bare_Ada_Node)
return Internal_Entity_Base_Type_Decl;
--  Return the type Ada.Exceptions.Exception_Id.

   function Ada_Node_P_Task_Id_Type

     (Node : Bare_Ada_Node)
return Internal_Entity_Base_Type_Decl;
--  Return the type Ada.Task_Identification.Task_Id.

   function Ada_Node_P_Has_With_Visibility

     (Node : Bare_Ada_Node; Refd_Unit : Internal_Unit)
return Boolean;
--  Return whether Self's unit has ``with visibility`` on ``refd_unit``.
--
--  In other words, whether Self's unit has a WITH clause on ``refd_unit``, or
--  if its spec, or one of its parent specs has one.

   function Ada_Node_P_Has_Visibility

     (Node : Bare_Ada_Node; Other_Entity : Internal_Entity)
return Boolean;

   function Ada_Node_P_Resolve_Generic_Actual

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity;
--  Helper property to resolve the actuals of generic instantiations.

   function Ada_Node_P_Top_Level_Use_Package_Clauses

     (Node : Bare_Ada_Node)
return Bare_Ada_Node_Array_Access;
--  If Self is a library item or a subunit, return a flat list of all names for
--  top-level UsePackageClause nodes. See UsePackageClause.env_spec.ref_envs
--  for more details.

   function Ada_Node_P_Top_Level_With_Package_Clauses

     (Node : Bare_Ada_Node)
return Bare_Name_Array_Access;
--  Return a flat list of all package names that are with'ed by top-level
--  WithClause nodes of the compilation unit this node lies in.

   function Ada_Node_P_Use_Packages_In_Spec_Of_Subp_Body

     (Node : Bare_Ada_Node)
return Lexical_Env;
--  If Self is a library-level SubpBody, fetch the environments USE'd in its
--  declaration.

   function Ada_Node_P_Nested_Generic_Formal_Part

     (Node : Bare_Ada_Node)
return Lexical_Env;
--  Assuming Self is a generic entity's body that is nested (not a
--  library item), return the lexical environment for the corresponding
--  GenericPackageDecl (or GenericSubpDecl) node. Return an empty environment
--  in all other cases.
--
--  This is a helper for generic formals visibility in generic bodies. See the
--  use in the child_unit macro.
--
--  The following property is evaluated each time we make a recursive lexical
--  environment lookup on a child unit. As it does itself a lot of lookups,
--  memoizing it is very important.

   function Ada_Node_P_Is_Package
(Node : Bare_Ada_Node)
return Boolean;
--  Property helper to determine if an entity is a package or not.

   function Ada_Node_P_Default_Initial_Env

     (Node : Bare_Ada_Node)
return Lexical_Env;
--  Provide the default lexical environment to use in EnvSpec's initial_env.

   function Ada_Node_P_Initial_Env

     (Node : Bare_Ada_Node; Scope : Lexical_Env; Env : Lexical_Env)

      return Lexical_Env;
--  Static method. Return ``scope`` if it is not EmptyEnv, and ``env``
--  otherwise.

   function Ada_Node_P_Env_Assoc

     (Node : Bare_Ada_Node; Key : Symbol_Type; Dest_Env : Lexical_Env)

      return Internal_Env_Assoc;
--  Static method, helper for EnvSpecs. Return the env assoc for ``key``,
--  ``Self`` and ``dest`` if ``dest_env`` is not null, and a null env assoc
--  otherwise.

   function Ada_Node_P_Top_Level_Decl

     (Node : Bare_Ada_Node; Unit : Internal_Unit)
return Bare_Basic_Decl;
--  Static method. Get the top-level decl in ``unit``. This is the body of a
--  Subunit, or the item of a ``LibraryItem``.

   function Ada_Node_P_Unpack_Formals

     (Node          : Bare_Ada_Node;
      Formal_Params : Internal_Entity_Base_Formal_Param_Decl_Array_Access)

      return Internal_Single_Formal_Array_Access;
--  Static method. Couples (identifier, param spec) for all parameters.

   function Ada_Node_P_Unpack_Formals_Impl

     (Node          : Bare_Ada_Node;
      Formal_Params : Internal_Entity_Base_Formal_Param_Decl_Array_Access)

      return Internal_Single_Formal_Array_Access;

   function Ada_Node_P_Match_Formals

     (Node          : Bare_Ada_Node;
      Formal_Params : Internal_Entity_Base_Formal_Param_Decl_Array_Access;
      Params        : Internal_Entity_Assoc_List; Is_Dottable_Subp : Boolean)

      return Internal_Param_Match_Array_Access;
--  Static method. For each ParamAssoc in a AssocList, return whether we could
--  find a matching formal in Self, and whether this formal is optional (i.e.
--  has a default value).

   function Ada_Node_P_Choice_Match

     (Node   : Bare_Ada_Node; Value : Big_Integer_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Assuming that self is a choice expression (such as what can appear in an
--  alternative of a case statement or in the RHS of a membership expression,
--  this property returns whether the given value satisfies it.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--     allow experiments, it is totally unsupported and the API and behavior
--     are very likely to change in the future.

   function Ada_Node_P_Gnat_Xref

     (Node   : Bare_Ada_Node; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Return a cross reference from this name to a defining identifier, trying to
--  mimic GNAT's xrefs as much as possible.

   function Ada_Node_P_Env_Get_Real_From_Node

     (Node : Bare_Ada_Node; From_Node : Bare_Ada_Node)
return Bare_Ada_Node;
--  Static property. Finds the closest BasicSubpDecl / GenericInstantiation.
--  Is used by env_get and env_get_first wrappers to refine from_node. The
--  reason is that inside a declaration named D, one can refer to previous
--  declarations named D. But an env lookup from a node inside D would return
--  that D itself, not previous declarations.

   function Ada_Node_P_Entity_No_Md

     (Node : Bare_Ada_Node; N : Bare_Ada_Node; Rebindings : Env_Rebindings;
      From_Rebound : Boolean)
return Internal_Entity;
--  Static property. Create an entity from the arguments with a null metadata.

   function Ada_Node_P_Bool_Bind

     (Node : Bare_Ada_Node; Type_Var : Logic_Var; Origin : Bare_Ada_Node)

      return Logic_Equation;
--  Static property. Return a logic bind of ``type_var`` match the boolean
--  type.

   function Ada_Node_P_Env_Mappings

     (Node  : Bare_Ada_Node; Defining_Names : Bare_Defining_Name_List;
      Value : Bare_Ada_Node)
return Internal_Env_Assoc_Array_Access;
--  Static method. Create an env mapping array from a list of BaseId to be used
--  as keys, and a node to be used as value in the mappings.

   function Ada_Node_P_Type_Bind_Val

     (Node   : Bare_Ada_Node; Left : Logic_Var; Right : Internal_Entity;
      Origin : Bare_Ada_Node)
return Logic_Equation;

   function Ada_Node_P_Type_Bind_Var

     (Node   : Bare_Ada_Node; Left : Logic_Var; Right : Logic_Var;
      Origin : Bare_Ada_Node)
return Logic_Equation;

   function Ada_Node_P_Comp_Bind

     (Node   : Bare_Ada_Node; Left : Logic_Var; Right : Logic_Var;
      Origin : Bare_Ada_Node)
return Logic_Equation;

   function Ada_Node_P_Universal_Int_Bind

     (Node : Bare_Ada_Node; Type_Var : Logic_Var; Origin : Bare_Ada_Node)

      return Logic_Equation;
--  Static method. Return an equation that will bind type_var to any integer
--  value, corresponding to the notion of universal_integer in the Ada RM.

   function Ada_Node_P_Universal_Real_Bind

     (Node : Bare_Ada_Node; Type_Var : Logic_Var; Origin : Bare_Ada_Node)

      return Logic_Equation;
--  Static method. Return an equation that will bind type_var to any real
--  value, corresponding to the notion of universal_real in the Ada RM.

   function Ada_Node_P_Origin_Node
(Node : Bare_Ada_Node)
return Bare_Ada_Node;
--  Return a null node iff we are in the definition of an aspect clause where
--  sequential lookup needs to be deactivated. Return Self otherwise.

   function Ada_Node_P_Env_Hook
(Node : Bare_Ada_Node)
return Boolean;
--  Hook for the EnvSpec of units.
--
--  Return value is not significant: the only purpose of this property lies in
--  its side effects.

   function Ada_Node_P_Env_Get

     (Node       : Bare_Ada_Node; Env : Lexical_Env; Symbol : Symbol_Type;
      Lookup     : Lookup_Kind    := Recursive;
      From_Node  : Bare_Ada_Node  := No_Bare_Ada_Node;
      Categories : Ref_Categories :=
        (Inherited_Primitives => True, Nocat => True))

      return Internal_Entity_Array_Access;
--  Wrapper for ``env.get``. Refine ``from_node`` so that it starts
--  from the closest ``BasicSubpDecl``/``GenericInstantiation``. (see
--  ``AdaNode.env_get_real_from_node``).

   function Ada_Node_P_Env_Get_First

     (Node       : Bare_Ada_Node; Env : Lexical_Env; Symbol : Symbol_Type;
      Lookup     : Lookup_Kind    := Recursive;
      From_Node  : Bare_Ada_Node  := No_Bare_Ada_Node;
      Categories : Ref_Categories :=
        (Inherited_Primitives => True, Nocat => True))

      return Internal_Entity;
--  Wrapper for ``env.get_first``. Refine ``from_node`` so that it starts
--  from the closest ``BasicSubpDecl``/``GenericInstantiation``. (see
--  ``AdaNode.env_get_real_from_node``).

   function Node_Env

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;
--  For nodes that introduce a new environment, return the parent lexical
--  environment. Return the "inherited" environment otherwise.

   function Children_Env

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;
--  For nodes that introduce a new environment, return it. Return the
--  "inherited" environment otherwise.

   function Parent

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity;
--  Return the lexical parent for this node. Return null for the root AST node
--  or for AST nodes for which no one has a reference to the parent.

   function Parents

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Return an array that contains the lexical parents (this node included).
--  Nearer parents are first in the list.

   function Children

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Return an array that contains the direct lexical children.

   function Token_Start
(Node : Bare_Ada_Node)
return Token_Reference;
--  Return the first token used to parse this node.

   function Token_End
(Node : Bare_Ada_Node)
return Token_Reference;
--  Return the last token used to parse this node.

   function Child_Index
(Node : Bare_Ada_Node)
return Integer;
--  Return the 0-based index for Node in its parent's children.

   function Previous_Sibling

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity;
--  Return the node's previous sibling, if there is one.

   function Next_Sibling

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity;
--  Return the node's next sibling, if there is one.

   function Unit
(Node : Bare_Ada_Node)
return Internal_Unit;
--  Return the analysis unit owning this node.

   function Is_Ghost
(Node : Bare_Ada_Node)
return Boolean;
--  Return whether the node is a ghost.
--
--  Unlike regular nodes, ghost nodes cover no token in the input source: they
--  are logically located instead between two tokens. The "token_first" of all
--  ghost nodes is the token right after this logical position, while they have
--  no "token_last".

   function Text
(Node : Bare_Ada_Node)
return Character_Type_Array_Access;
--  Return the text corresponding to this node. Private property (for internal
--  DSL use).

   function Full_Sloc_Image

     (Node : Bare_Ada_Node)
return Character_Type_Array_Access;
--  Return a string containing the filename + the sloc in GNU conformant
--  format. Useful to create diagnostics from a node.

   function Ada_Node_P_Custom_Id_Text

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;
--  Custom Unique identifying text used to recognize this node. Not applicable
--  to all nodes, but on AdaNode because it spans more than one hierarchy of
--  node types.

   function Ada_Node_P_Complete

     (Node : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Completion_Item_Array_Access;
--  Return possible completions at this point in the file.

   function Ada_Node_P_Xref_Equation

     (Node   : Bare_Ada_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  This is the base property for constructing equations that, when solved,
--  will resolve names and types for every sub expression of the expression you
--  call it on. Note that if you call that on any expression, in some context
--  it might lack full information and return multiple solutions. If you want
--  completely precise resolution, you must call that on the outermost node
--  that supports xref_equation.

   function Ada_Node_P_Xref_Stop_Resolution

     (Node : Bare_Ada_Node)
return Boolean;

   function Ada_Node_P_Xref_Entry_Point
(Node : Bare_Ada_Node)
return Boolean;
--  Designates entities that are entry point for the xref solving
--  infrastructure. If this returns true, then resolve_names can be called
--  on it.
--
--  .. note:: For convenience, and unlike what is defined in the ARM wrt.
--     complete contexts for name resolution, ``xref_entry_points`` can be
--     nested.

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

   type Internal_Entity_Expr is record

      Node : aliased Bare_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Expr : constant Internal_Entity_Expr;

   function Create_Internal_Entity_Expr
     (Node : Bare_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Expr;

   function Trace_Image (R : Internal_Entity_Expr) return String;

   type Internal_Aspect is record

      Exists : aliased Boolean;
      --  Whether the aspect is defined or not

      Node : aliased Internal_Entity;
      --  Syntactic node that defines the aspect

      Value : aliased Internal_Entity_Expr;
      --  Expr node defining the value of the aspect

   end record with
      Convention => C;
   No_Aspect : constant Internal_Aspect;

   function Trace_Image (R : Internal_Aspect) return String;

   type Internal_Entity_Basic_Decl is record

      Node : aliased Bare_Basic_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Basic_Decl : constant Internal_Entity_Basic_Decl;

   function Create_Internal_Entity_Basic_Decl
     (Node : Bare_Basic_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Basic_Decl;

   function Hash (R : Internal_Entity_Basic_Decl) return Hash_Type;

   function Trace_Image (R : Internal_Entity_Basic_Decl) return String;

   type Internal_Completion_Item is record

      Decl : aliased Internal_Entity_Basic_Decl;

      Is_Dot_Call : aliased Boolean;

   end record with
      Convention => C;
   No_Completion_Item : constant Internal_Completion_Item;

   function Trace_Image (R : Internal_Completion_Item) return String;

   type Internal_Discrete_Range is record

      Low_Bound : aliased Internal_Entity_Expr;

      High_Bound : aliased Internal_Entity_Expr;

   end record with
      Convention => C;
   No_Discrete_Range : constant Internal_Discrete_Range;

   function Trace_Image (R : Internal_Discrete_Range) return String;

   type Internal_Doc_Annotation is record

      Key : aliased Character_Type_Array_Access;
      --  Annotation key

      Value : aliased Character_Type_Array_Access;
      --  Annotation value

   end record with
      Convention => C;
   No_Doc_Annotation : constant Internal_Doc_Annotation;

   procedure Inc_Ref (R : Internal_Doc_Annotation);
   procedure Dec_Ref (R : in out Internal_Doc_Annotation);

   function Equivalent (L, R : Internal_Doc_Annotation) return Boolean;

   function Trace_Image (R : Internal_Doc_Annotation) return String;

   type Internal_Entity_Abort_Node is record

      Node : aliased Bare_Abort_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abort_Node : constant Internal_Entity_Abort_Node;

   function Create_Internal_Entity_Abort_Node
     (Node : Bare_Abort_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Abort_Node;

   function Trace_Image (R : Internal_Entity_Abort_Node) return String;

   type Internal_Entity_Abort_Absent is record

      Node : aliased Bare_Abort_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abort_Absent : constant Internal_Entity_Abort_Absent;

   function Create_Internal_Entity_Abort_Absent
     (Node : Bare_Abort_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Abort_Absent;

   function Trace_Image (R : Internal_Entity_Abort_Absent) return String;

   type Internal_Entity_Abort_Present is record

      Node : aliased Bare_Abort_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abort_Present : constant Internal_Entity_Abort_Present;

   function Create_Internal_Entity_Abort_Present
     (Node : Bare_Abort_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Abort_Present;

   function Trace_Image (R : Internal_Entity_Abort_Present) return String;

   type Internal_Entity_Stmt is record

      Node : aliased Bare_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Stmt : constant Internal_Entity_Stmt;

   function Create_Internal_Entity_Stmt
     (Node : Bare_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Stmt;

   function Trace_Image (R : Internal_Entity_Stmt) return String;

   type Internal_Entity_Simple_Stmt is record

      Node : aliased Bare_Simple_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Simple_Stmt : constant Internal_Entity_Simple_Stmt;

   function Create_Internal_Entity_Simple_Stmt
     (Node : Bare_Simple_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Simple_Stmt;

   function Trace_Image (R : Internal_Entity_Simple_Stmt) return String;

   type Internal_Entity_Abort_Stmt is record

      Node : aliased Bare_Abort_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abort_Stmt : constant Internal_Entity_Abort_Stmt;

   function Create_Internal_Entity_Abort_Stmt
     (Node : Bare_Abort_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Abort_Stmt;

   function Trace_Image (R : Internal_Entity_Abort_Stmt) return String;

   type Internal_Entity_Abstract_Node is record

      Node : aliased Bare_Abstract_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abstract_Node : constant Internal_Entity_Abstract_Node;

   function Create_Internal_Entity_Abstract_Node
     (Node : Bare_Abstract_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Abstract_Node;

   function Trace_Image (R : Internal_Entity_Abstract_Node) return String;

   type Internal_Entity_Abstract_Absent is record

      Node : aliased Bare_Abstract_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abstract_Absent : constant Internal_Entity_Abstract_Absent;

   function Create_Internal_Entity_Abstract_Absent
     (Node : Bare_Abstract_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Abstract_Absent;

   function Trace_Image (R : Internal_Entity_Abstract_Absent) return String;

   type Internal_Entity_Basic_Subp_Decl is record

      Node : aliased Bare_Basic_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Basic_Subp_Decl : constant Internal_Entity_Basic_Subp_Decl;

   function Create_Internal_Entity_Basic_Subp_Decl
     (Node : Bare_Basic_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Basic_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Basic_Subp_Decl) return String;

   type Internal_Entity_Classic_Subp_Decl is record

      Node : aliased Bare_Classic_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Classic_Subp_Decl : constant Internal_Entity_Classic_Subp_Decl;

   function Create_Internal_Entity_Classic_Subp_Decl
     (Node : Bare_Classic_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Classic_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Classic_Subp_Decl) return String;

   type Internal_Entity_Formal_Subp_Decl is record

      Node : aliased Bare_Formal_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Formal_Subp_Decl : constant Internal_Entity_Formal_Subp_Decl;

   function Create_Internal_Entity_Formal_Subp_Decl
     (Node : Bare_Formal_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Formal_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Formal_Subp_Decl) return String;

   type Internal_Entity_Abstract_Formal_Subp_Decl is record

      Node : aliased Bare_Abstract_Formal_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abstract_Formal_Subp_Decl : constant Internal_Entity_Abstract_Formal_Subp_Decl;

   function Create_Internal_Entity_Abstract_Formal_Subp_Decl
     (Node : Bare_Abstract_Formal_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Abstract_Formal_Subp_Decl;

   function Trace_Image
     (R : Internal_Entity_Abstract_Formal_Subp_Decl) return String;

   type Internal_Entity_Abstract_Present is record

      Node : aliased Bare_Abstract_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abstract_Present : constant Internal_Entity_Abstract_Present;

   function Create_Internal_Entity_Abstract_Present
     (Node : Bare_Abstract_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Abstract_Present;

   function Trace_Image (R : Internal_Entity_Abstract_Present) return String;

   type Internal_Entity_Abstract_Subp_Decl is record

      Node : aliased Bare_Abstract_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Abstract_Subp_Decl : constant Internal_Entity_Abstract_Subp_Decl;

   function Create_Internal_Entity_Abstract_Subp_Decl
     (Node : Bare_Abstract_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Abstract_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Abstract_Subp_Decl) return String;

   type Internal_Entity_Composite_Stmt is record

      Node : aliased Bare_Composite_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Composite_Stmt : constant Internal_Entity_Composite_Stmt;

   function Create_Internal_Entity_Composite_Stmt
     (Node : Bare_Composite_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Composite_Stmt;

   function Trace_Image (R : Internal_Entity_Composite_Stmt) return String;

   type Internal_Entity_Accept_Stmt is record

      Node : aliased Bare_Accept_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Accept_Stmt : constant Internal_Entity_Accept_Stmt;

   function Create_Internal_Entity_Accept_Stmt
     (Node : Bare_Accept_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Accept_Stmt;

   function Trace_Image (R : Internal_Entity_Accept_Stmt) return String;

   type Internal_Entity_Accept_Stmt_With_Stmts is record

      Node : aliased Bare_Accept_Stmt_With_Stmts;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Accept_Stmt_With_Stmts : constant Internal_Entity_Accept_Stmt_With_Stmts;

   function Create_Internal_Entity_Accept_Stmt_With_Stmts
     (Node : Bare_Accept_Stmt_With_Stmts; Info : Internal_Entity_Info)
      return Internal_Entity_Accept_Stmt_With_Stmts;

   function Trace_Image
     (R : Internal_Entity_Accept_Stmt_With_Stmts) return String;

   type Internal_Entity_Type_Def is record

      Node : aliased Bare_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Type_Def : constant Internal_Entity_Type_Def;

   function Create_Internal_Entity_Type_Def
     (Node : Bare_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Type_Def;

   function Trace_Image (R : Internal_Entity_Type_Def) return String;

   type Internal_Entity_Access_Def is record

      Node : aliased Bare_Access_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Access_Def : constant Internal_Entity_Access_Def;

   function Create_Internal_Entity_Access_Def
     (Node : Bare_Access_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Access_Def;

   function Trace_Image (R : Internal_Entity_Access_Def) return String;

   type Internal_Entity_Access_To_Subp_Def is record

      Node : aliased Bare_Access_To_Subp_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Access_To_Subp_Def : constant Internal_Entity_Access_To_Subp_Def;

   function Create_Internal_Entity_Access_To_Subp_Def
     (Node : Bare_Access_To_Subp_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Access_To_Subp_Def;

   function Trace_Image (R : Internal_Entity_Access_To_Subp_Def) return String;

   type Internal_Entity_Ada_List is record

      Node : aliased Bare_Ada_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Ada_List : constant Internal_Entity_Ada_List;

   function Create_Internal_Entity_Ada_List
     (Node : Bare_Ada_List; Info : Internal_Entity_Info)
      return Internal_Entity_Ada_List;

   function Trace_Image (R : Internal_Entity_Ada_List) return String;

   type Internal_Entity_Ada_Node_List is record

      Node : aliased Bare_Ada_Node_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Ada_Node_List : constant Internal_Entity_Ada_Node_List;

   function Create_Internal_Entity_Ada_Node_List
     (Node : Bare_Ada_Node_List; Info : Internal_Entity_Info)
      return Internal_Entity_Ada_Node_List;

   function Trace_Image (R : Internal_Entity_Ada_Node_List) return String;

   type Internal_Entity_Base_Aggregate is record

      Node : aliased Bare_Base_Aggregate;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Aggregate : constant Internal_Entity_Base_Aggregate;

   function Create_Internal_Entity_Base_Aggregate
     (Node : Bare_Base_Aggregate; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Aggregate;

   function Trace_Image (R : Internal_Entity_Base_Aggregate) return String;

   type Internal_Entity_Aggregate is record

      Node : aliased Bare_Aggregate;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aggregate : constant Internal_Entity_Aggregate;

   function Create_Internal_Entity_Aggregate
     (Node : Bare_Aggregate; Info : Internal_Entity_Info)
      return Internal_Entity_Aggregate;

   function Trace_Image (R : Internal_Entity_Aggregate) return String;

   type Internal_Entity_Basic_Assoc is record

      Node : aliased Bare_Basic_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Basic_Assoc : constant Internal_Entity_Basic_Assoc;

   function Create_Internal_Entity_Basic_Assoc
     (Node : Bare_Basic_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Basic_Assoc;

   function Trace_Image (R : Internal_Entity_Basic_Assoc) return String;

   type Internal_Entity_Aggregate_Assoc is record

      Node : aliased Bare_Aggregate_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aggregate_Assoc : constant Internal_Entity_Aggregate_Assoc;

   function Create_Internal_Entity_Aggregate_Assoc
     (Node : Bare_Aggregate_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Aggregate_Assoc;

   function Trace_Image (R : Internal_Entity_Aggregate_Assoc) return String;

   type Internal_Entity_Aliased_Node is record

      Node : aliased Bare_Aliased_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aliased_Node : constant Internal_Entity_Aliased_Node;

   function Create_Internal_Entity_Aliased_Node
     (Node : Bare_Aliased_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Aliased_Node;

   function Trace_Image (R : Internal_Entity_Aliased_Node) return String;

   type Internal_Entity_Aliased_Absent is record

      Node : aliased Bare_Aliased_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aliased_Absent : constant Internal_Entity_Aliased_Absent;

   function Create_Internal_Entity_Aliased_Absent
     (Node : Bare_Aliased_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Aliased_Absent;

   function Trace_Image (R : Internal_Entity_Aliased_Absent) return String;

   type Internal_Entity_Aliased_Present is record

      Node : aliased Bare_Aliased_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aliased_Present : constant Internal_Entity_Aliased_Present;

   function Create_Internal_Entity_Aliased_Present
     (Node : Bare_Aliased_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Aliased_Present;

   function Trace_Image (R : Internal_Entity_Aliased_Present) return String;

   type Internal_Entity_All_Node is record

      Node : aliased Bare_All_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_All_Node : constant Internal_Entity_All_Node;

   function Create_Internal_Entity_All_Node
     (Node : Bare_All_Node; Info : Internal_Entity_Info)
      return Internal_Entity_All_Node;

   function Trace_Image (R : Internal_Entity_All_Node) return String;

   type Internal_Entity_All_Absent is record

      Node : aliased Bare_All_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_All_Absent : constant Internal_Entity_All_Absent;

   function Create_Internal_Entity_All_Absent
     (Node : Bare_All_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_All_Absent;

   function Trace_Image (R : Internal_Entity_All_Absent) return String;

   type Internal_Entity_All_Present is record

      Node : aliased Bare_All_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_All_Present : constant Internal_Entity_All_Present;

   function Create_Internal_Entity_All_Present
     (Node : Bare_All_Present; Info : Internal_Entity_Info)
      return Internal_Entity_All_Present;

   function Trace_Image (R : Internal_Entity_All_Present) return String;

   type Internal_Entity_Allocator is record

      Node : aliased Bare_Allocator;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Allocator : constant Internal_Entity_Allocator;

   function Create_Internal_Entity_Allocator
     (Node : Bare_Allocator; Info : Internal_Entity_Info)
      return Internal_Entity_Allocator;

   function Trace_Image (R : Internal_Entity_Allocator) return String;

   type Internal_Entity_Alternatives_List is record

      Node : aliased Bare_Alternatives_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Alternatives_List : constant Internal_Entity_Alternatives_List;

   function Create_Internal_Entity_Alternatives_List
     (Node : Bare_Alternatives_List; Info : Internal_Entity_Info)
      return Internal_Entity_Alternatives_List;

   function Trace_Image (R : Internal_Entity_Alternatives_List) return String;

   type Internal_Entity_Object_Decl is record

      Node : aliased Bare_Object_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Object_Decl : constant Internal_Entity_Object_Decl;

   function Create_Internal_Entity_Object_Decl
     (Node : Bare_Object_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Object_Decl;

   function Trace_Image (R : Internal_Entity_Object_Decl) return String;

   type Internal_Entity_Anonymous_Object_Decl is record

      Node : aliased Bare_Anonymous_Object_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Anonymous_Object_Decl : constant Internal_Entity_Anonymous_Object_Decl;

   function Create_Internal_Entity_Anonymous_Object_Decl
     (Node : Bare_Anonymous_Object_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Anonymous_Object_Decl;

   function Trace_Image
     (R : Internal_Entity_Anonymous_Object_Decl) return String;

   type Internal_Entity_Type_Expr is record

      Node : aliased Bare_Type_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Type_Expr : constant Internal_Entity_Type_Expr;

   function Create_Internal_Entity_Type_Expr
     (Node : Bare_Type_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Type_Expr;

   function Hash (R : Internal_Entity_Type_Expr) return Hash_Type;

   function Trace_Image (R : Internal_Entity_Type_Expr) return String;

   type Internal_Entity_Anonymous_Type is record

      Node : aliased Bare_Anonymous_Type;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Anonymous_Type : constant Internal_Entity_Anonymous_Type;

   function Create_Internal_Entity_Anonymous_Type
     (Node : Bare_Anonymous_Type; Info : Internal_Entity_Info)
      return Internal_Entity_Anonymous_Type;

   function Trace_Image (R : Internal_Entity_Anonymous_Type) return String;

   type Internal_Entity_Base_Type_Access_Def is record

      Node : aliased Bare_Base_Type_Access_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Type_Access_Def : constant Internal_Entity_Base_Type_Access_Def;

   function Create_Internal_Entity_Base_Type_Access_Def
     (Node : Bare_Base_Type_Access_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Type_Access_Def;

   function Trace_Image
     (R : Internal_Entity_Base_Type_Access_Def) return String;

   type Internal_Entity_Anonymous_Type_Access_Def is record

      Node : aliased Bare_Anonymous_Type_Access_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Anonymous_Type_Access_Def : constant Internal_Entity_Anonymous_Type_Access_Def;

   function Create_Internal_Entity_Anonymous_Type_Access_Def
     (Node : Bare_Anonymous_Type_Access_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Anonymous_Type_Access_Def;

   function Trace_Image
     (R : Internal_Entity_Anonymous_Type_Access_Def) return String;

   type Internal_Entity_Base_Type_Decl is record

      Node : aliased Bare_Base_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Type_Decl : constant Internal_Entity_Base_Type_Decl;

   function Create_Internal_Entity_Base_Type_Decl
     (Node : Bare_Base_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Type_Decl;

   function Hash (R : Internal_Entity_Base_Type_Decl) return Hash_Type;

   function Trace_Image (R : Internal_Entity_Base_Type_Decl) return String;

   type Internal_Entity_Type_Decl is record

      Node : aliased Bare_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Type_Decl : constant Internal_Entity_Type_Decl;

   function Create_Internal_Entity_Type_Decl
     (Node : Bare_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Type_Decl;

   function Trace_Image (R : Internal_Entity_Type_Decl) return String;

   type Internal_Entity_Anonymous_Type_Decl is record

      Node : aliased Bare_Anonymous_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Anonymous_Type_Decl : constant Internal_Entity_Anonymous_Type_Decl;

   function Create_Internal_Entity_Anonymous_Type_Decl
     (Node : Bare_Anonymous_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Anonymous_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Anonymous_Type_Decl) return String;

   type Internal_Entity_Array_Indices is record

      Node : aliased Bare_Array_Indices;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Array_Indices : constant Internal_Entity_Array_Indices;

   function Create_Internal_Entity_Array_Indices
     (Node : Bare_Array_Indices; Info : Internal_Entity_Info)
      return Internal_Entity_Array_Indices;

   function Trace_Image (R : Internal_Entity_Array_Indices) return String;

   type Internal_Entity_Array_Type_Def is record

      Node : aliased Bare_Array_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Array_Type_Def : constant Internal_Entity_Array_Type_Def;

   function Create_Internal_Entity_Array_Type_Def
     (Node : Bare_Array_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Array_Type_Def;

   function Trace_Image (R : Internal_Entity_Array_Type_Def) return String;

   type Internal_Entity_Aspect_Assoc is record

      Node : aliased Bare_Aspect_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aspect_Assoc : constant Internal_Entity_Aspect_Assoc;

   function Create_Internal_Entity_Aspect_Assoc
     (Node : Bare_Aspect_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Aspect_Assoc;

   function Trace_Image (R : Internal_Entity_Aspect_Assoc) return String;

   type Internal_Entity_Aspect_Assoc_List is record

      Node : aliased Bare_Aspect_Assoc_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aspect_Assoc_List : constant Internal_Entity_Aspect_Assoc_List;

   function Create_Internal_Entity_Aspect_Assoc_List
     (Node : Bare_Aspect_Assoc_List; Info : Internal_Entity_Info)
      return Internal_Entity_Aspect_Assoc_List;

   function Trace_Image (R : Internal_Entity_Aspect_Assoc_List) return String;

   type Internal_Entity_Aspect_Clause is record

      Node : aliased Bare_Aspect_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aspect_Clause : constant Internal_Entity_Aspect_Clause;

   function Create_Internal_Entity_Aspect_Clause
     (Node : Bare_Aspect_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Aspect_Clause;

   function Trace_Image (R : Internal_Entity_Aspect_Clause) return String;

   type Internal_Entity_Aspect_Spec is record

      Node : aliased Bare_Aspect_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Aspect_Spec : constant Internal_Entity_Aspect_Spec;

   function Create_Internal_Entity_Aspect_Spec
     (Node : Bare_Aspect_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Aspect_Spec;

   function Trace_Image (R : Internal_Entity_Aspect_Spec) return String;

   type Internal_Entity_Assign_Stmt is record

      Node : aliased Bare_Assign_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Assign_Stmt : constant Internal_Entity_Assign_Stmt;

   function Create_Internal_Entity_Assign_Stmt
     (Node : Bare_Assign_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Assign_Stmt;

   function Trace_Image (R : Internal_Entity_Assign_Stmt) return String;

   type Internal_Entity_Basic_Assoc_List is record

      Node : aliased Bare_Basic_Assoc_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Basic_Assoc_List : constant Internal_Entity_Basic_Assoc_List;

   function Create_Internal_Entity_Basic_Assoc_List
     (Node : Bare_Basic_Assoc_List; Info : Internal_Entity_Info)
      return Internal_Entity_Basic_Assoc_List;

   function Trace_Image (R : Internal_Entity_Basic_Assoc_List) return String;

   type Internal_Entity_Assoc_List is record

      Node : aliased Bare_Assoc_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Assoc_List : constant Internal_Entity_Assoc_List;

   function Create_Internal_Entity_Assoc_List
     (Node : Bare_Assoc_List; Info : Internal_Entity_Info)
      return Internal_Entity_Assoc_List;

   function Trace_Image (R : Internal_Entity_Assoc_List) return String;

   type Internal_Entity_At_Clause is record

      Node : aliased Bare_At_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_At_Clause : constant Internal_Entity_At_Clause;

   function Create_Internal_Entity_At_Clause
     (Node : Bare_At_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_At_Clause;

   function Trace_Image (R : Internal_Entity_At_Clause) return String;

   type Internal_Entity_Attribute_Def_Clause is record

      Node : aliased Bare_Attribute_Def_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Attribute_Def_Clause : constant Internal_Entity_Attribute_Def_Clause;

   function Create_Internal_Entity_Attribute_Def_Clause
     (Node : Bare_Attribute_Def_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Attribute_Def_Clause;

   function Trace_Image
     (R : Internal_Entity_Attribute_Def_Clause) return String;

   type Internal_Entity_Name is record

      Node : aliased Bare_Name;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Name : constant Internal_Entity_Name;

   function Create_Internal_Entity_Name
     (Node : Bare_Name; Info : Internal_Entity_Info)
      return Internal_Entity_Name;

   function Trace_Image (R : Internal_Entity_Name) return String;

   type Internal_Entity_Attribute_Ref is record

      Node : aliased Bare_Attribute_Ref;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Attribute_Ref : constant Internal_Entity_Attribute_Ref;

   function Create_Internal_Entity_Attribute_Ref
     (Node : Bare_Attribute_Ref; Info : Internal_Entity_Info)
      return Internal_Entity_Attribute_Ref;

   function Trace_Image (R : Internal_Entity_Attribute_Ref) return String;

   type Internal_Entity_Base_Assoc is record

      Node : aliased Bare_Base_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Assoc : constant Internal_Entity_Base_Assoc;

   function Create_Internal_Entity_Base_Assoc
     (Node : Bare_Base_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Assoc;

   function Trace_Image (R : Internal_Entity_Base_Assoc) return String;

   type Internal_Entity_Base_Assoc_List is record

      Node : aliased Bare_Base_Assoc_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Assoc_List : constant Internal_Entity_Base_Assoc_List;

   function Create_Internal_Entity_Base_Assoc_List
     (Node : Bare_Base_Assoc_List; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Assoc_List;

   function Trace_Image (R : Internal_Entity_Base_Assoc_List) return String;

   type Internal_Entity_Base_Formal_Param_Decl is record

      Node : aliased Bare_Base_Formal_Param_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Formal_Param_Decl : constant Internal_Entity_Base_Formal_Param_Decl;

   function Create_Internal_Entity_Base_Formal_Param_Decl
     (Node : Bare_Base_Formal_Param_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Formal_Param_Decl;

   function Trace_Image
     (R : Internal_Entity_Base_Formal_Param_Decl) return String;

   type Internal_Entity_Base_Formal_Param_Holder is record

      Node : aliased Bare_Base_Formal_Param_Holder;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Formal_Param_Holder : constant Internal_Entity_Base_Formal_Param_Holder;

   function Create_Internal_Entity_Base_Formal_Param_Holder
     (Node : Bare_Base_Formal_Param_Holder; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Formal_Param_Holder;

   function Trace_Image
     (R : Internal_Entity_Base_Formal_Param_Holder) return String;

   type Internal_Entity_Single_Tok_Node is record

      Node : aliased Bare_Single_Tok_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Single_Tok_Node : constant Internal_Entity_Single_Tok_Node;

   function Create_Internal_Entity_Single_Tok_Node
     (Node : Bare_Single_Tok_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Single_Tok_Node;

   function Trace_Image (R : Internal_Entity_Single_Tok_Node) return String;

   type Internal_Entity_Base_Id is record

      Node : aliased Bare_Base_Id;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Id : constant Internal_Entity_Base_Id;

   function Create_Internal_Entity_Base_Id
     (Node : Bare_Base_Id; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Id;

   function Trace_Image (R : Internal_Entity_Base_Id) return String;

   type Internal_Entity_Base_Loop_Stmt is record

      Node : aliased Bare_Base_Loop_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Loop_Stmt : constant Internal_Entity_Base_Loop_Stmt;

   function Create_Internal_Entity_Base_Loop_Stmt
     (Node : Bare_Base_Loop_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Loop_Stmt;

   function Trace_Image (R : Internal_Entity_Base_Loop_Stmt) return String;

   type Internal_Entity_Base_Package_Decl is record

      Node : aliased Bare_Base_Package_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Package_Decl : constant Internal_Entity_Base_Package_Decl;

   function Create_Internal_Entity_Base_Package_Decl
     (Node : Bare_Base_Package_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Package_Decl;

   function Trace_Image (R : Internal_Entity_Base_Package_Decl) return String;

   type Internal_Entity_Base_Record_Def is record

      Node : aliased Bare_Base_Record_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Record_Def : constant Internal_Entity_Base_Record_Def;

   function Create_Internal_Entity_Base_Record_Def
     (Node : Bare_Base_Record_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Record_Def;

   function Trace_Image (R : Internal_Entity_Base_Record_Def) return String;

   type Internal_Entity_Body_Node is record

      Node : aliased Bare_Body_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Body_Node : constant Internal_Entity_Body_Node;

   function Create_Internal_Entity_Body_Node
     (Node : Bare_Body_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Body_Node;

   function Trace_Image (R : Internal_Entity_Body_Node) return String;

   type Internal_Entity_Base_Subp_Body is record

      Node : aliased Bare_Base_Subp_Body;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Subp_Body : constant Internal_Entity_Base_Subp_Body;

   function Create_Internal_Entity_Base_Subp_Body
     (Node : Bare_Base_Subp_Body; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Subp_Body;

   function Trace_Image (R : Internal_Entity_Base_Subp_Body) return String;

   type Internal_Entity_Base_Subp_Spec is record

      Node : aliased Bare_Base_Subp_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Subp_Spec : constant Internal_Entity_Base_Subp_Spec;

   function Create_Internal_Entity_Base_Subp_Spec
     (Node : Bare_Base_Subp_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Subp_Spec;

   function Trace_Image (R : Internal_Entity_Base_Subp_Spec) return String;

   type Internal_Entity_Base_Subtype_Decl is record

      Node : aliased Bare_Base_Subtype_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Base_Subtype_Decl : constant Internal_Entity_Base_Subtype_Decl;

   function Create_Internal_Entity_Base_Subtype_Decl
     (Node : Bare_Base_Subtype_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Base_Subtype_Decl;

   function Trace_Image (R : Internal_Entity_Base_Subtype_Decl) return String;

   type Internal_Entity_Block_Stmt is record

      Node : aliased Bare_Block_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Block_Stmt : constant Internal_Entity_Block_Stmt;

   function Create_Internal_Entity_Block_Stmt
     (Node : Bare_Block_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Block_Stmt;

   function Trace_Image (R : Internal_Entity_Block_Stmt) return String;

   type Internal_Entity_Begin_Block is record

      Node : aliased Bare_Begin_Block;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Begin_Block : constant Internal_Entity_Begin_Block;

   function Create_Internal_Entity_Begin_Block
     (Node : Bare_Begin_Block; Info : Internal_Entity_Info)
      return Internal_Entity_Begin_Block;

   function Trace_Image (R : Internal_Entity_Begin_Block) return String;

   type Internal_Entity_Bin_Op is record

      Node : aliased Bare_Bin_Op;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Bin_Op : constant Internal_Entity_Bin_Op;

   function Create_Internal_Entity_Bin_Op
     (Node : Bare_Bin_Op; Info : Internal_Entity_Info)
      return Internal_Entity_Bin_Op;

   function Trace_Image (R : Internal_Entity_Bin_Op) return String;

   type Internal_Entity_Body_Stub is record

      Node : aliased Bare_Body_Stub;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Body_Stub : constant Internal_Entity_Body_Stub;

   function Create_Internal_Entity_Body_Stub
     (Node : Bare_Body_Stub; Info : Internal_Entity_Info)
      return Internal_Entity_Body_Stub;

   function Trace_Image (R : Internal_Entity_Body_Stub) return String;

   type Internal_Entity_Box_Expr is record

      Node : aliased Bare_Box_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Box_Expr : constant Internal_Entity_Box_Expr;

   function Create_Internal_Entity_Box_Expr
     (Node : Bare_Box_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Box_Expr;

   function Trace_Image (R : Internal_Entity_Box_Expr) return String;

   type Internal_Entity_Call_Expr is record

      Node : aliased Bare_Call_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Call_Expr : constant Internal_Entity_Call_Expr;

   function Create_Internal_Entity_Call_Expr
     (Node : Bare_Call_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Call_Expr;

   function Trace_Image (R : Internal_Entity_Call_Expr) return String;

   type Internal_Entity_Call_Stmt is record

      Node : aliased Bare_Call_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Call_Stmt : constant Internal_Entity_Call_Stmt;

   function Create_Internal_Entity_Call_Stmt
     (Node : Bare_Call_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Call_Stmt;

   function Trace_Image (R : Internal_Entity_Call_Stmt) return String;

   type Internal_Entity_Case_Expr is record

      Node : aliased Bare_Case_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Case_Expr : constant Internal_Entity_Case_Expr;

   function Create_Internal_Entity_Case_Expr
     (Node : Bare_Case_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Case_Expr;

   function Trace_Image (R : Internal_Entity_Case_Expr) return String;

   type Internal_Entity_Case_Expr_Alternative is record

      Node : aliased Bare_Case_Expr_Alternative;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Case_Expr_Alternative : constant Internal_Entity_Case_Expr_Alternative;

   function Create_Internal_Entity_Case_Expr_Alternative
     (Node : Bare_Case_Expr_Alternative; Info : Internal_Entity_Info)
      return Internal_Entity_Case_Expr_Alternative;

   function Trace_Image
     (R : Internal_Entity_Case_Expr_Alternative) return String;

   type Internal_Entity_Case_Expr_Alternative_List is record

      Node : aliased Bare_Case_Expr_Alternative_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Case_Expr_Alternative_List : constant Internal_Entity_Case_Expr_Alternative_List;

   function Create_Internal_Entity_Case_Expr_Alternative_List
     (Node : Bare_Case_Expr_Alternative_List; Info : Internal_Entity_Info)
      return Internal_Entity_Case_Expr_Alternative_List;

   function Trace_Image
     (R : Internal_Entity_Case_Expr_Alternative_List) return String;

   type Internal_Entity_Case_Stmt is record

      Node : aliased Bare_Case_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Case_Stmt : constant Internal_Entity_Case_Stmt;

   function Create_Internal_Entity_Case_Stmt
     (Node : Bare_Case_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Case_Stmt;

   function Trace_Image (R : Internal_Entity_Case_Stmt) return String;

   type Internal_Entity_Case_Stmt_Alternative is record

      Node : aliased Bare_Case_Stmt_Alternative;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Case_Stmt_Alternative : constant Internal_Entity_Case_Stmt_Alternative;

   function Create_Internal_Entity_Case_Stmt_Alternative
     (Node : Bare_Case_Stmt_Alternative; Info : Internal_Entity_Info)
      return Internal_Entity_Case_Stmt_Alternative;

   function Trace_Image
     (R : Internal_Entity_Case_Stmt_Alternative) return String;

   type Internal_Entity_Case_Stmt_Alternative_List is record

      Node : aliased Bare_Case_Stmt_Alternative_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Case_Stmt_Alternative_List : constant Internal_Entity_Case_Stmt_Alternative_List;

   function Create_Internal_Entity_Case_Stmt_Alternative_List
     (Node : Bare_Case_Stmt_Alternative_List; Info : Internal_Entity_Info)
      return Internal_Entity_Case_Stmt_Alternative_List;

   function Trace_Image
     (R : Internal_Entity_Case_Stmt_Alternative_List) return String;

   type Internal_Entity_Char_Literal is record

      Node : aliased Bare_Char_Literal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Char_Literal : constant Internal_Entity_Char_Literal;

   function Create_Internal_Entity_Char_Literal
     (Node : Bare_Char_Literal; Info : Internal_Entity_Info)
      return Internal_Entity_Char_Literal;

   function Trace_Image (R : Internal_Entity_Char_Literal) return String;

   type Internal_Entity_Classwide_Type_Decl is record

      Node : aliased Bare_Classwide_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Classwide_Type_Decl : constant Internal_Entity_Classwide_Type_Decl;

   function Create_Internal_Entity_Classwide_Type_Decl
     (Node : Bare_Classwide_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Classwide_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Classwide_Type_Decl) return String;

   type Internal_Entity_Compilation_Unit is record

      Node : aliased Bare_Compilation_Unit;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Compilation_Unit : constant Internal_Entity_Compilation_Unit;

   function Create_Internal_Entity_Compilation_Unit
     (Node : Bare_Compilation_Unit; Info : Internal_Entity_Info)
      return Internal_Entity_Compilation_Unit;

   function Hash (R : Internal_Entity_Compilation_Unit) return Hash_Type;

   function Trace_Image (R : Internal_Entity_Compilation_Unit) return String;

   type Internal_Entity_Compilation_Unit_List is record

      Node : aliased Bare_Compilation_Unit_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Compilation_Unit_List : constant Internal_Entity_Compilation_Unit_List;

   function Create_Internal_Entity_Compilation_Unit_List
     (Node : Bare_Compilation_Unit_List; Info : Internal_Entity_Info)
      return Internal_Entity_Compilation_Unit_List;

   function Trace_Image
     (R : Internal_Entity_Compilation_Unit_List) return String;

   type Internal_Entity_Component_Clause is record

      Node : aliased Bare_Component_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Component_Clause : constant Internal_Entity_Component_Clause;

   function Create_Internal_Entity_Component_Clause
     (Node : Bare_Component_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Component_Clause;

   function Trace_Image (R : Internal_Entity_Component_Clause) return String;

   type Internal_Entity_Component_Decl is record

      Node : aliased Bare_Component_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Component_Decl : constant Internal_Entity_Component_Decl;

   function Create_Internal_Entity_Component_Decl
     (Node : Bare_Component_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Component_Decl;

   function Trace_Image (R : Internal_Entity_Component_Decl) return String;

   type Internal_Entity_Component_Def is record

      Node : aliased Bare_Component_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Component_Def : constant Internal_Entity_Component_Def;

   function Create_Internal_Entity_Component_Def
     (Node : Bare_Component_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Component_Def;

   function Trace_Image (R : Internal_Entity_Component_Def) return String;

   type Internal_Entity_Component_List is record

      Node : aliased Bare_Component_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Component_List : constant Internal_Entity_Component_List;

   function Create_Internal_Entity_Component_List
     (Node : Bare_Component_List; Info : Internal_Entity_Info)
      return Internal_Entity_Component_List;

   function Trace_Image (R : Internal_Entity_Component_List) return String;

   type Internal_Entity_Concrete_Formal_Subp_Decl is record

      Node : aliased Bare_Concrete_Formal_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Concrete_Formal_Subp_Decl : constant Internal_Entity_Concrete_Formal_Subp_Decl;

   function Create_Internal_Entity_Concrete_Formal_Subp_Decl
     (Node : Bare_Concrete_Formal_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Concrete_Formal_Subp_Decl;

   function Trace_Image
     (R : Internal_Entity_Concrete_Formal_Subp_Decl) return String;

   type Internal_Entity_Constant_Node is record

      Node : aliased Bare_Constant_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constant_Node : constant Internal_Entity_Constant_Node;

   function Create_Internal_Entity_Constant_Node
     (Node : Bare_Constant_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Constant_Node;

   function Trace_Image (R : Internal_Entity_Constant_Node) return String;

   type Internal_Entity_Constant_Absent is record

      Node : aliased Bare_Constant_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constant_Absent : constant Internal_Entity_Constant_Absent;

   function Create_Internal_Entity_Constant_Absent
     (Node : Bare_Constant_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Constant_Absent;

   function Trace_Image (R : Internal_Entity_Constant_Absent) return String;

   type Internal_Entity_Constant_Present is record

      Node : aliased Bare_Constant_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constant_Present : constant Internal_Entity_Constant_Present;

   function Create_Internal_Entity_Constant_Present
     (Node : Bare_Constant_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Constant_Present;

   function Trace_Image (R : Internal_Entity_Constant_Present) return String;

   type Internal_Entity_Constrained_Array_Indices is record

      Node : aliased Bare_Constrained_Array_Indices;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constrained_Array_Indices : constant Internal_Entity_Constrained_Array_Indices;

   function Create_Internal_Entity_Constrained_Array_Indices
     (Node : Bare_Constrained_Array_Indices; Info : Internal_Entity_Info)
      return Internal_Entity_Constrained_Array_Indices;

   function Trace_Image
     (R : Internal_Entity_Constrained_Array_Indices) return String;

   type Internal_Entity_Subtype_Indication is record

      Node : aliased Bare_Subtype_Indication;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subtype_Indication : constant Internal_Entity_Subtype_Indication;

   function Create_Internal_Entity_Subtype_Indication
     (Node : Bare_Subtype_Indication; Info : Internal_Entity_Info)
      return Internal_Entity_Subtype_Indication;

   function Trace_Image (R : Internal_Entity_Subtype_Indication) return String;

   type Internal_Entity_Constrained_Subtype_Indication is record

      Node : aliased Bare_Constrained_Subtype_Indication;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constrained_Subtype_Indication : constant Internal_Entity_Constrained_Subtype_Indication;

   function Create_Internal_Entity_Constrained_Subtype_Indication
     (Node : Bare_Constrained_Subtype_Indication; Info : Internal_Entity_Info)
      return Internal_Entity_Constrained_Subtype_Indication;

   function Trace_Image
     (R : Internal_Entity_Constrained_Subtype_Indication) return String;

   type Internal_Entity_Constraint is record

      Node : aliased Bare_Constraint;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constraint : constant Internal_Entity_Constraint;

   function Create_Internal_Entity_Constraint
     (Node : Bare_Constraint; Info : Internal_Entity_Info)
      return Internal_Entity_Constraint;

   function Trace_Image (R : Internal_Entity_Constraint) return String;

   type Internal_Entity_Constraint_List is record

      Node : aliased Bare_Constraint_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Constraint_List : constant Internal_Entity_Constraint_List;

   function Create_Internal_Entity_Constraint_List
     (Node : Bare_Constraint_List; Info : Internal_Entity_Info)
      return Internal_Entity_Constraint_List;

   function Trace_Image (R : Internal_Entity_Constraint_List) return String;

   type Internal_Entity_Contract_Case_Assoc is record

      Node : aliased Bare_Contract_Case_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Contract_Case_Assoc : constant Internal_Entity_Contract_Case_Assoc;

   function Create_Internal_Entity_Contract_Case_Assoc
     (Node : Bare_Contract_Case_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Contract_Case_Assoc;

   function Trace_Image
     (R : Internal_Entity_Contract_Case_Assoc) return String;

   type Internal_Entity_Contract_Case_Assoc_List is record

      Node : aliased Bare_Contract_Case_Assoc_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Contract_Case_Assoc_List : constant Internal_Entity_Contract_Case_Assoc_List;

   function Create_Internal_Entity_Contract_Case_Assoc_List
     (Node : Bare_Contract_Case_Assoc_List; Info : Internal_Entity_Info)
      return Internal_Entity_Contract_Case_Assoc_List;

   function Trace_Image
     (R : Internal_Entity_Contract_Case_Assoc_List) return String;

   type Internal_Entity_Contract_Cases is record

      Node : aliased Bare_Contract_Cases;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Contract_Cases : constant Internal_Entity_Contract_Cases;

   function Create_Internal_Entity_Contract_Cases
     (Node : Bare_Contract_Cases; Info : Internal_Entity_Info)
      return Internal_Entity_Contract_Cases;

   function Trace_Image (R : Internal_Entity_Contract_Cases) return String;

   type Internal_Entity_Real_Type_Def is record

      Node : aliased Bare_Real_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Real_Type_Def : constant Internal_Entity_Real_Type_Def;

   function Create_Internal_Entity_Real_Type_Def
     (Node : Bare_Real_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Real_Type_Def;

   function Trace_Image (R : Internal_Entity_Real_Type_Def) return String;

   type Internal_Entity_Decimal_Fixed_Point_Def is record

      Node : aliased Bare_Decimal_Fixed_Point_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Decimal_Fixed_Point_Def : constant Internal_Entity_Decimal_Fixed_Point_Def;

   function Create_Internal_Entity_Decimal_Fixed_Point_Def
     (Node : Bare_Decimal_Fixed_Point_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Decimal_Fixed_Point_Def;

   function Trace_Image
     (R : Internal_Entity_Decimal_Fixed_Point_Def) return String;

   type Internal_Entity_Decl_Block is record

      Node : aliased Bare_Decl_Block;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Decl_Block : constant Internal_Entity_Decl_Block;

   function Create_Internal_Entity_Decl_Block
     (Node : Bare_Decl_Block; Info : Internal_Entity_Info)
      return Internal_Entity_Decl_Block;

   function Trace_Image (R : Internal_Entity_Decl_Block) return String;

   type Internal_Entity_Decl_List is record

      Node : aliased Bare_Decl_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Decl_List : constant Internal_Entity_Decl_List;

   function Create_Internal_Entity_Decl_List
     (Node : Bare_Decl_List; Info : Internal_Entity_Info)
      return Internal_Entity_Decl_List;

   function Trace_Image (R : Internal_Entity_Decl_List) return String;

   type Internal_Entity_Declarative_Part is record

      Node : aliased Bare_Declarative_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Declarative_Part : constant Internal_Entity_Declarative_Part;

   function Create_Internal_Entity_Declarative_Part
     (Node : Bare_Declarative_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Declarative_Part;

   function Trace_Image (R : Internal_Entity_Declarative_Part) return String;

   type Internal_Entity_Defining_Name is record

      Node : aliased Bare_Defining_Name;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Defining_Name : constant Internal_Entity_Defining_Name;

   function Create_Internal_Entity_Defining_Name
     (Node : Bare_Defining_Name; Info : Internal_Entity_Info)
      return Internal_Entity_Defining_Name;

   function Trace_Image (R : Internal_Entity_Defining_Name) return String;

   type Internal_Entity_Defining_Name_List is record

      Node : aliased Bare_Defining_Name_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Defining_Name_List : constant Internal_Entity_Defining_Name_List;

   function Create_Internal_Entity_Defining_Name_List
     (Node : Bare_Defining_Name_List; Info : Internal_Entity_Info)
      return Internal_Entity_Defining_Name_List;

   function Trace_Image (R : Internal_Entity_Defining_Name_List) return String;

   type Internal_Entity_Delay_Stmt is record

      Node : aliased Bare_Delay_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Delay_Stmt : constant Internal_Entity_Delay_Stmt;

   function Create_Internal_Entity_Delay_Stmt
     (Node : Bare_Delay_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Delay_Stmt;

   function Trace_Image (R : Internal_Entity_Delay_Stmt) return String;

   type Internal_Entity_Delta_Constraint is record

      Node : aliased Bare_Delta_Constraint;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Delta_Constraint : constant Internal_Entity_Delta_Constraint;

   function Create_Internal_Entity_Delta_Constraint
     (Node : Bare_Delta_Constraint; Info : Internal_Entity_Info)
      return Internal_Entity_Delta_Constraint;

   function Trace_Image (R : Internal_Entity_Delta_Constraint) return String;

   type Internal_Entity_Derived_Type_Def is record

      Node : aliased Bare_Derived_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Derived_Type_Def : constant Internal_Entity_Derived_Type_Def;

   function Create_Internal_Entity_Derived_Type_Def
     (Node : Bare_Derived_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Derived_Type_Def;

   function Trace_Image (R : Internal_Entity_Derived_Type_Def) return String;

   type Internal_Entity_Digits_Constraint is record

      Node : aliased Bare_Digits_Constraint;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Digits_Constraint : constant Internal_Entity_Digits_Constraint;

   function Create_Internal_Entity_Digits_Constraint
     (Node : Bare_Digits_Constraint; Info : Internal_Entity_Info)
      return Internal_Entity_Digits_Constraint;

   function Trace_Image (R : Internal_Entity_Digits_Constraint) return String;

   type Internal_Entity_Discrete_Base_Subtype_Decl is record

      Node : aliased Bare_Discrete_Base_Subtype_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discrete_Base_Subtype_Decl : constant Internal_Entity_Discrete_Base_Subtype_Decl;

   function Create_Internal_Entity_Discrete_Base_Subtype_Decl
     (Node : Bare_Discrete_Base_Subtype_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Discrete_Base_Subtype_Decl;

   function Trace_Image
     (R : Internal_Entity_Discrete_Base_Subtype_Decl) return String;

   type Internal_Entity_Discrete_Subtype_Indication is record

      Node : aliased Bare_Discrete_Subtype_Indication;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discrete_Subtype_Indication : constant Internal_Entity_Discrete_Subtype_Indication;

   function Create_Internal_Entity_Discrete_Subtype_Indication
     (Node : Bare_Discrete_Subtype_Indication; Info : Internal_Entity_Info)
      return Internal_Entity_Discrete_Subtype_Indication;

   function Trace_Image
     (R : Internal_Entity_Discrete_Subtype_Indication) return String;

   type Internal_Entity_Discrete_Subtype_Name is record

      Node : aliased Bare_Discrete_Subtype_Name;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discrete_Subtype_Name : constant Internal_Entity_Discrete_Subtype_Name;

   function Create_Internal_Entity_Discrete_Subtype_Name
     (Node : Bare_Discrete_Subtype_Name; Info : Internal_Entity_Info)
      return Internal_Entity_Discrete_Subtype_Name;

   function Trace_Image
     (R : Internal_Entity_Discrete_Subtype_Name) return String;

   type Internal_Entity_Discriminant_Assoc is record

      Node : aliased Bare_Discriminant_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discriminant_Assoc : constant Internal_Entity_Discriminant_Assoc;

   function Create_Internal_Entity_Discriminant_Assoc
     (Node : Bare_Discriminant_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Discriminant_Assoc;

   function Trace_Image (R : Internal_Entity_Discriminant_Assoc) return String;

   type Internal_Entity_Identifier_List is record

      Node : aliased Bare_Identifier_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Identifier_List : constant Internal_Entity_Identifier_List;

   function Create_Internal_Entity_Identifier_List
     (Node : Bare_Identifier_List; Info : Internal_Entity_Info)
      return Internal_Entity_Identifier_List;

   function Trace_Image (R : Internal_Entity_Identifier_List) return String;

   type Internal_Entity_Discriminant_Choice_List is record

      Node : aliased Bare_Discriminant_Choice_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discriminant_Choice_List : constant Internal_Entity_Discriminant_Choice_List;

   function Create_Internal_Entity_Discriminant_Choice_List
     (Node : Bare_Discriminant_Choice_List; Info : Internal_Entity_Info)
      return Internal_Entity_Discriminant_Choice_List;

   function Trace_Image
     (R : Internal_Entity_Discriminant_Choice_List) return String;

   type Internal_Entity_Discriminant_Constraint is record

      Node : aliased Bare_Discriminant_Constraint;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discriminant_Constraint : constant Internal_Entity_Discriminant_Constraint;

   function Create_Internal_Entity_Discriminant_Constraint
     (Node : Bare_Discriminant_Constraint; Info : Internal_Entity_Info)
      return Internal_Entity_Discriminant_Constraint;

   function Trace_Image
     (R : Internal_Entity_Discriminant_Constraint) return String;

   type Internal_Entity_Discriminant_Part is record

      Node : aliased Bare_Discriminant_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discriminant_Part : constant Internal_Entity_Discriminant_Part;

   function Create_Internal_Entity_Discriminant_Part
     (Node : Bare_Discriminant_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Discriminant_Part;

   function Trace_Image (R : Internal_Entity_Discriminant_Part) return String;

   type Internal_Entity_Discriminant_Spec is record

      Node : aliased Bare_Discriminant_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discriminant_Spec : constant Internal_Entity_Discriminant_Spec;

   function Create_Internal_Entity_Discriminant_Spec
     (Node : Bare_Discriminant_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Discriminant_Spec;

   function Trace_Image (R : Internal_Entity_Discriminant_Spec) return String;

   type Internal_Entity_Discriminant_Spec_List is record

      Node : aliased Bare_Discriminant_Spec_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Discriminant_Spec_List : constant Internal_Entity_Discriminant_Spec_List;

   function Create_Internal_Entity_Discriminant_Spec_List
     (Node : Bare_Discriminant_Spec_List; Info : Internal_Entity_Info)
      return Internal_Entity_Discriminant_Spec_List;

   function Trace_Image
     (R : Internal_Entity_Discriminant_Spec_List) return String;

   type Internal_Entity_Dotted_Name is record

      Node : aliased Bare_Dotted_Name;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Dotted_Name : constant Internal_Entity_Dotted_Name;

   function Create_Internal_Entity_Dotted_Name
     (Node : Bare_Dotted_Name; Info : Internal_Entity_Info)
      return Internal_Entity_Dotted_Name;

   function Trace_Image (R : Internal_Entity_Dotted_Name) return String;

   type Internal_Entity_Elsif_Expr_Part is record

      Node : aliased Bare_Elsif_Expr_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Elsif_Expr_Part : constant Internal_Entity_Elsif_Expr_Part;

   function Create_Internal_Entity_Elsif_Expr_Part
     (Node : Bare_Elsif_Expr_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Elsif_Expr_Part;

   function Trace_Image (R : Internal_Entity_Elsif_Expr_Part) return String;

   type Internal_Entity_Elsif_Expr_Part_List is record

      Node : aliased Bare_Elsif_Expr_Part_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Elsif_Expr_Part_List : constant Internal_Entity_Elsif_Expr_Part_List;

   function Create_Internal_Entity_Elsif_Expr_Part_List
     (Node : Bare_Elsif_Expr_Part_List; Info : Internal_Entity_Info)
      return Internal_Entity_Elsif_Expr_Part_List;

   function Trace_Image
     (R : Internal_Entity_Elsif_Expr_Part_List) return String;

   type Internal_Entity_Elsif_Stmt_Part is record

      Node : aliased Bare_Elsif_Stmt_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Elsif_Stmt_Part : constant Internal_Entity_Elsif_Stmt_Part;

   function Create_Internal_Entity_Elsif_Stmt_Part
     (Node : Bare_Elsif_Stmt_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Elsif_Stmt_Part;

   function Trace_Image (R : Internal_Entity_Elsif_Stmt_Part) return String;

   type Internal_Entity_Elsif_Stmt_Part_List is record

      Node : aliased Bare_Elsif_Stmt_Part_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Elsif_Stmt_Part_List : constant Internal_Entity_Elsif_Stmt_Part_List;

   function Create_Internal_Entity_Elsif_Stmt_Part_List
     (Node : Bare_Elsif_Stmt_Part_List; Info : Internal_Entity_Info)
      return Internal_Entity_Elsif_Stmt_Part_List;

   function Trace_Image
     (R : Internal_Entity_Elsif_Stmt_Part_List) return String;

   type Internal_Entity_End_Name is record

      Node : aliased Bare_End_Name;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_End_Name : constant Internal_Entity_End_Name;

   function Create_Internal_Entity_End_Name
     (Node : Bare_End_Name; Info : Internal_Entity_Info)
      return Internal_Entity_End_Name;

   function Trace_Image (R : Internal_Entity_End_Name) return String;

   type Internal_Entity_Entry_Body is record

      Node : aliased Bare_Entry_Body;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Entry_Body : constant Internal_Entity_Entry_Body;

   function Create_Internal_Entity_Entry_Body
     (Node : Bare_Entry_Body; Info : Internal_Entity_Info)
      return Internal_Entity_Entry_Body;

   function Trace_Image (R : Internal_Entity_Entry_Body) return String;

   type Internal_Entity_Entry_Completion_Formal_Params is record

      Node : aliased Bare_Entry_Completion_Formal_Params;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Entry_Completion_Formal_Params : constant Internal_Entity_Entry_Completion_Formal_Params;

   function Create_Internal_Entity_Entry_Completion_Formal_Params
     (Node : Bare_Entry_Completion_Formal_Params; Info : Internal_Entity_Info)
      return Internal_Entity_Entry_Completion_Formal_Params;

   function Trace_Image
     (R : Internal_Entity_Entry_Completion_Formal_Params) return String;

   type Internal_Entity_Entry_Decl is record

      Node : aliased Bare_Entry_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Entry_Decl : constant Internal_Entity_Entry_Decl;

   function Create_Internal_Entity_Entry_Decl
     (Node : Bare_Entry_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Entry_Decl;

   function Trace_Image (R : Internal_Entity_Entry_Decl) return String;

   type Internal_Entity_Entry_Index_Spec is record

      Node : aliased Bare_Entry_Index_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Entry_Index_Spec : constant Internal_Entity_Entry_Index_Spec;

   function Create_Internal_Entity_Entry_Index_Spec
     (Node : Bare_Entry_Index_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Entry_Index_Spec;

   function Trace_Image (R : Internal_Entity_Entry_Index_Spec) return String;

   type Internal_Entity_Entry_Spec is record

      Node : aliased Bare_Entry_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Entry_Spec : constant Internal_Entity_Entry_Spec;

   function Create_Internal_Entity_Entry_Spec
     (Node : Bare_Entry_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Entry_Spec;

   function Trace_Image (R : Internal_Entity_Entry_Spec) return String;

   type Internal_Entity_Enum_Lit_Synth_Type_Expr is record

      Node : aliased Bare_Enum_Lit_Synth_Type_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Enum_Lit_Synth_Type_Expr : constant Internal_Entity_Enum_Lit_Synth_Type_Expr;

   function Create_Internal_Entity_Enum_Lit_Synth_Type_Expr
     (Node : Bare_Enum_Lit_Synth_Type_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Enum_Lit_Synth_Type_Expr;

   function Trace_Image
     (R : Internal_Entity_Enum_Lit_Synth_Type_Expr) return String;

   type Internal_Entity_Enum_Literal_Decl is record

      Node : aliased Bare_Enum_Literal_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Enum_Literal_Decl : constant Internal_Entity_Enum_Literal_Decl;

   function Create_Internal_Entity_Enum_Literal_Decl
     (Node : Bare_Enum_Literal_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Enum_Literal_Decl;

   function Trace_Image (R : Internal_Entity_Enum_Literal_Decl) return String;

   type Internal_Entity_Enum_Literal_Decl_List is record

      Node : aliased Bare_Enum_Literal_Decl_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Enum_Literal_Decl_List : constant Internal_Entity_Enum_Literal_Decl_List;

   function Create_Internal_Entity_Enum_Literal_Decl_List
     (Node : Bare_Enum_Literal_Decl_List; Info : Internal_Entity_Info)
      return Internal_Entity_Enum_Literal_Decl_List;

   function Trace_Image
     (R : Internal_Entity_Enum_Literal_Decl_List) return String;

   type Internal_Entity_Enum_Rep_Clause is record

      Node : aliased Bare_Enum_Rep_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Enum_Rep_Clause : constant Internal_Entity_Enum_Rep_Clause;

   function Create_Internal_Entity_Enum_Rep_Clause
     (Node : Bare_Enum_Rep_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Enum_Rep_Clause;

   function Trace_Image (R : Internal_Entity_Enum_Rep_Clause) return String;

   type Internal_Entity_Enum_Subp_Spec is record

      Node : aliased Bare_Enum_Subp_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Enum_Subp_Spec : constant Internal_Entity_Enum_Subp_Spec;

   function Create_Internal_Entity_Enum_Subp_Spec
     (Node : Bare_Enum_Subp_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Enum_Subp_Spec;

   function Trace_Image (R : Internal_Entity_Enum_Subp_Spec) return String;

   type Internal_Entity_Enum_Type_Def is record

      Node : aliased Bare_Enum_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Enum_Type_Def : constant Internal_Entity_Enum_Type_Def;

   function Create_Internal_Entity_Enum_Type_Def
     (Node : Bare_Enum_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Enum_Type_Def;

   function Trace_Image (R : Internal_Entity_Enum_Type_Def) return String;

   type Internal_Entity_Error_Decl is record

      Node : aliased Bare_Error_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Error_Decl : constant Internal_Entity_Error_Decl;

   function Create_Internal_Entity_Error_Decl
     (Node : Bare_Error_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Error_Decl;

   function Trace_Image (R : Internal_Entity_Error_Decl) return String;

   type Internal_Entity_Error_Stmt is record

      Node : aliased Bare_Error_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Error_Stmt : constant Internal_Entity_Error_Stmt;

   function Create_Internal_Entity_Error_Stmt
     (Node : Bare_Error_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Error_Stmt;

   function Trace_Image (R : Internal_Entity_Error_Stmt) return String;

   type Internal_Entity_Exception_Decl is record

      Node : aliased Bare_Exception_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Exception_Decl : constant Internal_Entity_Exception_Decl;

   function Create_Internal_Entity_Exception_Decl
     (Node : Bare_Exception_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Exception_Decl;

   function Trace_Image (R : Internal_Entity_Exception_Decl) return String;

   type Internal_Entity_Exception_Handler is record

      Node : aliased Bare_Exception_Handler;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Exception_Handler : constant Internal_Entity_Exception_Handler;

   function Create_Internal_Entity_Exception_Handler
     (Node : Bare_Exception_Handler; Info : Internal_Entity_Info)
      return Internal_Entity_Exception_Handler;

   function Trace_Image (R : Internal_Entity_Exception_Handler) return String;

   type Internal_Entity_Exit_Stmt is record

      Node : aliased Bare_Exit_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Exit_Stmt : constant Internal_Entity_Exit_Stmt;

   function Create_Internal_Entity_Exit_Stmt
     (Node : Bare_Exit_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Exit_Stmt;

   function Trace_Image (R : Internal_Entity_Exit_Stmt) return String;

   type Internal_Entity_Explicit_Deref is record

      Node : aliased Bare_Explicit_Deref;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Explicit_Deref : constant Internal_Entity_Explicit_Deref;

   function Create_Internal_Entity_Explicit_Deref
     (Node : Bare_Explicit_Deref; Info : Internal_Entity_Info)
      return Internal_Entity_Explicit_Deref;

   function Trace_Image (R : Internal_Entity_Explicit_Deref) return String;

   type Internal_Entity_Expr_List is record

      Node : aliased Bare_Expr_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Expr_List : constant Internal_Entity_Expr_List;

   function Create_Internal_Entity_Expr_List
     (Node : Bare_Expr_List; Info : Internal_Entity_Info)
      return Internal_Entity_Expr_List;

   function Trace_Image (R : Internal_Entity_Expr_List) return String;

   type Internal_Entity_Expr_Alternatives_List is record

      Node : aliased Bare_Expr_Alternatives_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Expr_Alternatives_List : constant Internal_Entity_Expr_Alternatives_List;

   function Create_Internal_Entity_Expr_Alternatives_List
     (Node : Bare_Expr_Alternatives_List; Info : Internal_Entity_Info)
      return Internal_Entity_Expr_Alternatives_List;

   function Trace_Image
     (R : Internal_Entity_Expr_Alternatives_List) return String;

   type Internal_Entity_Expr_Function is record

      Node : aliased Bare_Expr_Function;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Expr_Function : constant Internal_Entity_Expr_Function;

   function Create_Internal_Entity_Expr_Function
     (Node : Bare_Expr_Function; Info : Internal_Entity_Info)
      return Internal_Entity_Expr_Function;

   function Trace_Image (R : Internal_Entity_Expr_Function) return String;

   type Internal_Entity_Extended_Return_Stmt is record

      Node : aliased Bare_Extended_Return_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Extended_Return_Stmt : constant Internal_Entity_Extended_Return_Stmt;

   function Create_Internal_Entity_Extended_Return_Stmt
     (Node : Bare_Extended_Return_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Extended_Return_Stmt;

   function Trace_Image
     (R : Internal_Entity_Extended_Return_Stmt) return String;

   type Internal_Entity_Extended_Return_Stmt_Object_Decl is record

      Node : aliased Bare_Extended_Return_Stmt_Object_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Extended_Return_Stmt_Object_Decl : constant Internal_Entity_Extended_Return_Stmt_Object_Decl;

   function Create_Internal_Entity_Extended_Return_Stmt_Object_Decl
     (Node : Bare_Extended_Return_Stmt_Object_Decl;
      Info : Internal_Entity_Info)
      return Internal_Entity_Extended_Return_Stmt_Object_Decl;

   function Trace_Image
     (R : Internal_Entity_Extended_Return_Stmt_Object_Decl) return String;

   type Internal_Entity_Floating_Point_Def is record

      Node : aliased Bare_Floating_Point_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Floating_Point_Def : constant Internal_Entity_Floating_Point_Def;

   function Create_Internal_Entity_Floating_Point_Def
     (Node : Bare_Floating_Point_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Floating_Point_Def;

   function Trace_Image (R : Internal_Entity_Floating_Point_Def) return String;

   type Internal_Entity_Loop_Spec is record

      Node : aliased Bare_Loop_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Loop_Spec : constant Internal_Entity_Loop_Spec;

   function Create_Internal_Entity_Loop_Spec
     (Node : Bare_Loop_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Loop_Spec;

   function Trace_Image (R : Internal_Entity_Loop_Spec) return String;

   type Internal_Entity_For_Loop_Spec is record

      Node : aliased Bare_For_Loop_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_For_Loop_Spec : constant Internal_Entity_For_Loop_Spec;

   function Create_Internal_Entity_For_Loop_Spec
     (Node : Bare_For_Loop_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_For_Loop_Spec;

   function Trace_Image (R : Internal_Entity_For_Loop_Spec) return String;

   type Internal_Entity_For_Loop_Stmt is record

      Node : aliased Bare_For_Loop_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_For_Loop_Stmt : constant Internal_Entity_For_Loop_Stmt;

   function Create_Internal_Entity_For_Loop_Stmt
     (Node : Bare_For_Loop_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_For_Loop_Stmt;

   function Trace_Image (R : Internal_Entity_For_Loop_Stmt) return String;

   type Internal_Entity_For_Loop_Var_Decl is record

      Node : aliased Bare_For_Loop_Var_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_For_Loop_Var_Decl : constant Internal_Entity_For_Loop_Var_Decl;

   function Create_Internal_Entity_For_Loop_Var_Decl
     (Node : Bare_For_Loop_Var_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_For_Loop_Var_Decl;

   function Trace_Image (R : Internal_Entity_For_Loop_Var_Decl) return String;

   type Internal_Entity_Formal_Discrete_Type_Def is record

      Node : aliased Bare_Formal_Discrete_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Formal_Discrete_Type_Def : constant Internal_Entity_Formal_Discrete_Type_Def;

   function Create_Internal_Entity_Formal_Discrete_Type_Def
     (Node : Bare_Formal_Discrete_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Formal_Discrete_Type_Def;

   function Trace_Image
     (R : Internal_Entity_Formal_Discrete_Type_Def) return String;

   type Internal_Entity_Generic_Decl is record

      Node : aliased Bare_Generic_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Decl : constant Internal_Entity_Generic_Decl;

   function Create_Internal_Entity_Generic_Decl
     (Node : Bare_Generic_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Decl;

   function Trace_Image (R : Internal_Entity_Generic_Decl) return String;

   type Internal_Entity_Generic_Formal is record

      Node : aliased Bare_Generic_Formal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Formal : constant Internal_Entity_Generic_Formal;

   function Create_Internal_Entity_Generic_Formal
     (Node : Bare_Generic_Formal; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Formal;

   function Trace_Image (R : Internal_Entity_Generic_Formal) return String;

   type Internal_Entity_Generic_Formal_Obj_Decl is record

      Node : aliased Bare_Generic_Formal_Obj_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Formal_Obj_Decl : constant Internal_Entity_Generic_Formal_Obj_Decl;

   function Create_Internal_Entity_Generic_Formal_Obj_Decl
     (Node : Bare_Generic_Formal_Obj_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Formal_Obj_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Formal_Obj_Decl) return String;

   type Internal_Entity_Generic_Formal_Package is record

      Node : aliased Bare_Generic_Formal_Package;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Formal_Package : constant Internal_Entity_Generic_Formal_Package;

   function Create_Internal_Entity_Generic_Formal_Package
     (Node : Bare_Generic_Formal_Package; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Formal_Package;

   function Trace_Image
     (R : Internal_Entity_Generic_Formal_Package) return String;

   type Internal_Entity_Generic_Formal_Part is record

      Node : aliased Bare_Generic_Formal_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Formal_Part : constant Internal_Entity_Generic_Formal_Part;

   function Create_Internal_Entity_Generic_Formal_Part
     (Node : Bare_Generic_Formal_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Formal_Part;

   function Trace_Image
     (R : Internal_Entity_Generic_Formal_Part) return String;

   type Internal_Entity_Generic_Formal_Subp_Decl is record

      Node : aliased Bare_Generic_Formal_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Formal_Subp_Decl : constant Internal_Entity_Generic_Formal_Subp_Decl;

   function Create_Internal_Entity_Generic_Formal_Subp_Decl
     (Node : Bare_Generic_Formal_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Formal_Subp_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Formal_Subp_Decl) return String;

   type Internal_Entity_Generic_Formal_Type_Decl is record

      Node : aliased Bare_Generic_Formal_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Formal_Type_Decl : constant Internal_Entity_Generic_Formal_Type_Decl;

   function Create_Internal_Entity_Generic_Formal_Type_Decl
     (Node : Bare_Generic_Formal_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Formal_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Formal_Type_Decl) return String;

   type Internal_Entity_Generic_Instantiation is record

      Node : aliased Bare_Generic_Instantiation;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Instantiation : constant Internal_Entity_Generic_Instantiation;

   function Create_Internal_Entity_Generic_Instantiation
     (Node : Bare_Generic_Instantiation; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Instantiation;

   function Trace_Image
     (R : Internal_Entity_Generic_Instantiation) return String;

   type Internal_Entity_Generic_Package_Decl is record

      Node : aliased Bare_Generic_Package_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Package_Decl : constant Internal_Entity_Generic_Package_Decl;

   function Create_Internal_Entity_Generic_Package_Decl
     (Node : Bare_Generic_Package_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Package_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Package_Decl) return String;

   type Internal_Entity_Generic_Package_Instantiation is record

      Node : aliased Bare_Generic_Package_Instantiation;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Package_Instantiation : constant Internal_Entity_Generic_Package_Instantiation;

   function Create_Internal_Entity_Generic_Package_Instantiation
     (Node : Bare_Generic_Package_Instantiation; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Package_Instantiation;

   function Trace_Image
     (R : Internal_Entity_Generic_Package_Instantiation) return String;

   type Internal_Entity_Generic_Package_Internal is record

      Node : aliased Bare_Generic_Package_Internal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Package_Internal : constant Internal_Entity_Generic_Package_Internal;

   function Create_Internal_Entity_Generic_Package_Internal
     (Node : Bare_Generic_Package_Internal; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Package_Internal;

   function Trace_Image
     (R : Internal_Entity_Generic_Package_Internal) return String;

   type Internal_Entity_Generic_Renaming_Decl is record

      Node : aliased Bare_Generic_Renaming_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Renaming_Decl : constant Internal_Entity_Generic_Renaming_Decl;

   function Create_Internal_Entity_Generic_Renaming_Decl
     (Node : Bare_Generic_Renaming_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Renaming_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Renaming_Decl) return String;

   type Internal_Entity_Generic_Package_Renaming_Decl is record

      Node : aliased Bare_Generic_Package_Renaming_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Package_Renaming_Decl : constant Internal_Entity_Generic_Package_Renaming_Decl;

   function Create_Internal_Entity_Generic_Package_Renaming_Decl
     (Node : Bare_Generic_Package_Renaming_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Package_Renaming_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Package_Renaming_Decl) return String;

   type Internal_Entity_Generic_Subp_Decl is record

      Node : aliased Bare_Generic_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Subp_Decl : constant Internal_Entity_Generic_Subp_Decl;

   function Create_Internal_Entity_Generic_Subp_Decl
     (Node : Bare_Generic_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Generic_Subp_Decl) return String;

   type Internal_Entity_Generic_Subp_Instantiation is record

      Node : aliased Bare_Generic_Subp_Instantiation;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Subp_Instantiation : constant Internal_Entity_Generic_Subp_Instantiation;

   function Create_Internal_Entity_Generic_Subp_Instantiation
     (Node : Bare_Generic_Subp_Instantiation; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Subp_Instantiation;

   function Trace_Image
     (R : Internal_Entity_Generic_Subp_Instantiation) return String;

   type Internal_Entity_Generic_Subp_Internal is record

      Node : aliased Bare_Generic_Subp_Internal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Subp_Internal : constant Internal_Entity_Generic_Subp_Internal;

   function Create_Internal_Entity_Generic_Subp_Internal
     (Node : Bare_Generic_Subp_Internal; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Subp_Internal;

   function Trace_Image
     (R : Internal_Entity_Generic_Subp_Internal) return String;

   type Internal_Entity_Generic_Subp_Renaming_Decl is record

      Node : aliased Bare_Generic_Subp_Renaming_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Generic_Subp_Renaming_Decl : constant Internal_Entity_Generic_Subp_Renaming_Decl;

   function Create_Internal_Entity_Generic_Subp_Renaming_Decl
     (Node : Bare_Generic_Subp_Renaming_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Generic_Subp_Renaming_Decl;

   function Trace_Image
     (R : Internal_Entity_Generic_Subp_Renaming_Decl) return String;

   type Internal_Entity_Goto_Stmt is record

      Node : aliased Bare_Goto_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Goto_Stmt : constant Internal_Entity_Goto_Stmt;

   function Create_Internal_Entity_Goto_Stmt
     (Node : Bare_Goto_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Goto_Stmt;

   function Trace_Image (R : Internal_Entity_Goto_Stmt) return String;

   type Internal_Entity_Handled_Stmts is record

      Node : aliased Bare_Handled_Stmts;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Handled_Stmts : constant Internal_Entity_Handled_Stmts;

   function Create_Internal_Entity_Handled_Stmts
     (Node : Bare_Handled_Stmts; Info : Internal_Entity_Info)
      return Internal_Entity_Handled_Stmts;

   function Trace_Image (R : Internal_Entity_Handled_Stmts) return String;

   type Internal_Entity_Identifier is record

      Node : aliased Bare_Identifier;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Identifier : constant Internal_Entity_Identifier;

   function Create_Internal_Entity_Identifier
     (Node : Bare_Identifier; Info : Internal_Entity_Info)
      return Internal_Entity_Identifier;

   function Trace_Image (R : Internal_Entity_Identifier) return String;

   type Internal_Entity_If_Expr is record

      Node : aliased Bare_If_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_If_Expr : constant Internal_Entity_If_Expr;

   function Create_Internal_Entity_If_Expr
     (Node : Bare_If_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_If_Expr;

   function Trace_Image (R : Internal_Entity_If_Expr) return String;

   type Internal_Entity_If_Stmt is record

      Node : aliased Bare_If_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_If_Stmt : constant Internal_Entity_If_Stmt;

   function Create_Internal_Entity_If_Stmt
     (Node : Bare_If_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_If_Stmt;

   function Trace_Image (R : Internal_Entity_If_Stmt) return String;

   type Internal_Entity_Incomplete_Type_Decl is record

      Node : aliased Bare_Incomplete_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Incomplete_Type_Decl : constant Internal_Entity_Incomplete_Type_Decl;

   function Create_Internal_Entity_Incomplete_Type_Decl
     (Node : Bare_Incomplete_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Incomplete_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Incomplete_Type_Decl) return String;

   type Internal_Entity_Incomplete_Tagged_Type_Decl is record

      Node : aliased Bare_Incomplete_Tagged_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Incomplete_Tagged_Type_Decl : constant Internal_Entity_Incomplete_Tagged_Type_Decl;

   function Create_Internal_Entity_Incomplete_Tagged_Type_Decl
     (Node : Bare_Incomplete_Tagged_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Incomplete_Tagged_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Incomplete_Tagged_Type_Decl) return String;

   type Internal_Entity_Index_Constraint is record

      Node : aliased Bare_Index_Constraint;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Index_Constraint : constant Internal_Entity_Index_Constraint;

   function Create_Internal_Entity_Index_Constraint
     (Node : Bare_Index_Constraint; Info : Internal_Entity_Info)
      return Internal_Entity_Index_Constraint;

   function Trace_Image (R : Internal_Entity_Index_Constraint) return String;

   type Internal_Entity_Num_Literal is record

      Node : aliased Bare_Num_Literal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Num_Literal : constant Internal_Entity_Num_Literal;

   function Create_Internal_Entity_Num_Literal
     (Node : Bare_Num_Literal; Info : Internal_Entity_Info)
      return Internal_Entity_Num_Literal;

   function Trace_Image (R : Internal_Entity_Num_Literal) return String;

   type Internal_Entity_Int_Literal is record

      Node : aliased Bare_Int_Literal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Int_Literal : constant Internal_Entity_Int_Literal;

   function Create_Internal_Entity_Int_Literal
     (Node : Bare_Int_Literal; Info : Internal_Entity_Info)
      return Internal_Entity_Int_Literal;

   function Trace_Image (R : Internal_Entity_Int_Literal) return String;

   type Internal_Entity_Interface_Kind is record

      Node : aliased Bare_Interface_Kind;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Interface_Kind : constant Internal_Entity_Interface_Kind;

   function Create_Internal_Entity_Interface_Kind
     (Node : Bare_Interface_Kind; Info : Internal_Entity_Info)
      return Internal_Entity_Interface_Kind;

   function Trace_Image (R : Internal_Entity_Interface_Kind) return String;

   type Internal_Entity_Interface_Kind_Limited is record

      Node : aliased Bare_Interface_Kind_Limited;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Interface_Kind_Limited : constant Internal_Entity_Interface_Kind_Limited;

   function Create_Internal_Entity_Interface_Kind_Limited
     (Node : Bare_Interface_Kind_Limited; Info : Internal_Entity_Info)
      return Internal_Entity_Interface_Kind_Limited;

   function Trace_Image
     (R : Internal_Entity_Interface_Kind_Limited) return String;

   type Internal_Entity_Interface_Kind_Protected is record

      Node : aliased Bare_Interface_Kind_Protected;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Interface_Kind_Protected : constant Internal_Entity_Interface_Kind_Protected;

   function Create_Internal_Entity_Interface_Kind_Protected
     (Node : Bare_Interface_Kind_Protected; Info : Internal_Entity_Info)
      return Internal_Entity_Interface_Kind_Protected;

   function Trace_Image
     (R : Internal_Entity_Interface_Kind_Protected) return String;

   type Internal_Entity_Interface_Kind_Synchronized is record

      Node : aliased Bare_Interface_Kind_Synchronized;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Interface_Kind_Synchronized : constant Internal_Entity_Interface_Kind_Synchronized;

   function Create_Internal_Entity_Interface_Kind_Synchronized
     (Node : Bare_Interface_Kind_Synchronized; Info : Internal_Entity_Info)
      return Internal_Entity_Interface_Kind_Synchronized;

   function Trace_Image
     (R : Internal_Entity_Interface_Kind_Synchronized) return String;

   type Internal_Entity_Interface_Kind_Task is record

      Node : aliased Bare_Interface_Kind_Task;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Interface_Kind_Task : constant Internal_Entity_Interface_Kind_Task;

   function Create_Internal_Entity_Interface_Kind_Task
     (Node : Bare_Interface_Kind_Task; Info : Internal_Entity_Info)
      return Internal_Entity_Interface_Kind_Task;

   function Trace_Image
     (R : Internal_Entity_Interface_Kind_Task) return String;

   type Internal_Entity_Interface_Type_Def is record

      Node : aliased Bare_Interface_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Interface_Type_Def : constant Internal_Entity_Interface_Type_Def;

   function Create_Internal_Entity_Interface_Type_Def
     (Node : Bare_Interface_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Interface_Type_Def;

   function Trace_Image (R : Internal_Entity_Interface_Type_Def) return String;

   type Internal_Entity_Iter_Type is record

      Node : aliased Bare_Iter_Type;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Iter_Type : constant Internal_Entity_Iter_Type;

   function Create_Internal_Entity_Iter_Type
     (Node : Bare_Iter_Type; Info : Internal_Entity_Info)
      return Internal_Entity_Iter_Type;

   function Trace_Image (R : Internal_Entity_Iter_Type) return String;

   type Internal_Entity_Iter_Type_In is record

      Node : aliased Bare_Iter_Type_In;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Iter_Type_In : constant Internal_Entity_Iter_Type_In;

   function Create_Internal_Entity_Iter_Type_In
     (Node : Bare_Iter_Type_In; Info : Internal_Entity_Info)
      return Internal_Entity_Iter_Type_In;

   function Trace_Image (R : Internal_Entity_Iter_Type_In) return String;

   type Internal_Entity_Iter_Type_Of is record

      Node : aliased Bare_Iter_Type_Of;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Iter_Type_Of : constant Internal_Entity_Iter_Type_Of;

   function Create_Internal_Entity_Iter_Type_Of
     (Node : Bare_Iter_Type_Of; Info : Internal_Entity_Info)
      return Internal_Entity_Iter_Type_Of;

   function Trace_Image (R : Internal_Entity_Iter_Type_Of) return String;

   type Internal_Entity_Known_Discriminant_Part is record

      Node : aliased Bare_Known_Discriminant_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Known_Discriminant_Part : constant Internal_Entity_Known_Discriminant_Part;

   function Create_Internal_Entity_Known_Discriminant_Part
     (Node : Bare_Known_Discriminant_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Known_Discriminant_Part;

   function Trace_Image
     (R : Internal_Entity_Known_Discriminant_Part) return String;

   type Internal_Entity_Label is record

      Node : aliased Bare_Label;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Label : constant Internal_Entity_Label;

   function Create_Internal_Entity_Label
     (Node : Bare_Label; Info : Internal_Entity_Info)
      return Internal_Entity_Label;

   function Trace_Image (R : Internal_Entity_Label) return String;

   type Internal_Entity_Label_Decl is record

      Node : aliased Bare_Label_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Label_Decl : constant Internal_Entity_Label_Decl;

   function Create_Internal_Entity_Label_Decl
     (Node : Bare_Label_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Label_Decl;

   function Trace_Image (R : Internal_Entity_Label_Decl) return String;

   type Internal_Entity_Library_Item is record

      Node : aliased Bare_Library_Item;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Library_Item : constant Internal_Entity_Library_Item;

   function Create_Internal_Entity_Library_Item
     (Node : Bare_Library_Item; Info : Internal_Entity_Info)
      return Internal_Entity_Library_Item;

   function Trace_Image (R : Internal_Entity_Library_Item) return String;

   type Internal_Entity_Limited_Node is record

      Node : aliased Bare_Limited_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Limited_Node : constant Internal_Entity_Limited_Node;

   function Create_Internal_Entity_Limited_Node
     (Node : Bare_Limited_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Limited_Node;

   function Trace_Image (R : Internal_Entity_Limited_Node) return String;

   type Internal_Entity_Limited_Absent is record

      Node : aliased Bare_Limited_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Limited_Absent : constant Internal_Entity_Limited_Absent;

   function Create_Internal_Entity_Limited_Absent
     (Node : Bare_Limited_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Limited_Absent;

   function Trace_Image (R : Internal_Entity_Limited_Absent) return String;

   type Internal_Entity_Limited_Present is record

      Node : aliased Bare_Limited_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Limited_Present : constant Internal_Entity_Limited_Present;

   function Create_Internal_Entity_Limited_Present
     (Node : Bare_Limited_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Limited_Present;

   function Trace_Image (R : Internal_Entity_Limited_Present) return String;

   type Internal_Entity_Loop_Stmt is record

      Node : aliased Bare_Loop_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Loop_Stmt : constant Internal_Entity_Loop_Stmt;

   function Create_Internal_Entity_Loop_Stmt
     (Node : Bare_Loop_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Loop_Stmt;

   function Trace_Image (R : Internal_Entity_Loop_Stmt) return String;

   type Internal_Entity_Membership_Expr is record

      Node : aliased Bare_Membership_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Membership_Expr : constant Internal_Entity_Membership_Expr;

   function Create_Internal_Entity_Membership_Expr
     (Node : Bare_Membership_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Membership_Expr;

   function Trace_Image (R : Internal_Entity_Membership_Expr) return String;

   type Internal_Entity_Mod_Int_Type_Def is record

      Node : aliased Bare_Mod_Int_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Mod_Int_Type_Def : constant Internal_Entity_Mod_Int_Type_Def;

   function Create_Internal_Entity_Mod_Int_Type_Def
     (Node : Bare_Mod_Int_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Mod_Int_Type_Def;

   function Trace_Image (R : Internal_Entity_Mod_Int_Type_Def) return String;

   type Internal_Entity_Mode is record

      Node : aliased Bare_Mode;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Mode : constant Internal_Entity_Mode;

   function Create_Internal_Entity_Mode
     (Node : Bare_Mode; Info : Internal_Entity_Info)
      return Internal_Entity_Mode;

   function Trace_Image (R : Internal_Entity_Mode) return String;

   type Internal_Entity_Mode_Default is record

      Node : aliased Bare_Mode_Default;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Mode_Default : constant Internal_Entity_Mode_Default;

   function Create_Internal_Entity_Mode_Default
     (Node : Bare_Mode_Default; Info : Internal_Entity_Info)
      return Internal_Entity_Mode_Default;

   function Trace_Image (R : Internal_Entity_Mode_Default) return String;

   type Internal_Entity_Mode_In is record

      Node : aliased Bare_Mode_In;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Mode_In : constant Internal_Entity_Mode_In;

   function Create_Internal_Entity_Mode_In
     (Node : Bare_Mode_In; Info : Internal_Entity_Info)
      return Internal_Entity_Mode_In;

   function Trace_Image (R : Internal_Entity_Mode_In) return String;

   type Internal_Entity_Mode_In_Out is record

      Node : aliased Bare_Mode_In_Out;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Mode_In_Out : constant Internal_Entity_Mode_In_Out;

   function Create_Internal_Entity_Mode_In_Out
     (Node : Bare_Mode_In_Out; Info : Internal_Entity_Info)
      return Internal_Entity_Mode_In_Out;

   function Trace_Image (R : Internal_Entity_Mode_In_Out) return String;

   type Internal_Entity_Mode_Out is record

      Node : aliased Bare_Mode_Out;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Mode_Out : constant Internal_Entity_Mode_Out;

   function Create_Internal_Entity_Mode_Out
     (Node : Bare_Mode_Out; Info : Internal_Entity_Info)
      return Internal_Entity_Mode_Out;

   function Trace_Image (R : Internal_Entity_Mode_Out) return String;

   type Internal_Entity_Multi_Dim_Array_Assoc is record

      Node : aliased Bare_Multi_Dim_Array_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Multi_Dim_Array_Assoc : constant Internal_Entity_Multi_Dim_Array_Assoc;

   function Create_Internal_Entity_Multi_Dim_Array_Assoc
     (Node : Bare_Multi_Dim_Array_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Multi_Dim_Array_Assoc;

   function Trace_Image
     (R : Internal_Entity_Multi_Dim_Array_Assoc) return String;

   type Internal_Entity_Name_List is record

      Node : aliased Bare_Name_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Name_List : constant Internal_Entity_Name_List;

   function Create_Internal_Entity_Name_List
     (Node : Bare_Name_List; Info : Internal_Entity_Info)
      return Internal_Entity_Name_List;

   function Trace_Image (R : Internal_Entity_Name_List) return String;

   type Internal_Entity_Named_Stmt is record

      Node : aliased Bare_Named_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Named_Stmt : constant Internal_Entity_Named_Stmt;

   function Create_Internal_Entity_Named_Stmt
     (Node : Bare_Named_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Named_Stmt;

   function Trace_Image (R : Internal_Entity_Named_Stmt) return String;

   type Internal_Entity_Named_Stmt_Decl is record

      Node : aliased Bare_Named_Stmt_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Named_Stmt_Decl : constant Internal_Entity_Named_Stmt_Decl;

   function Create_Internal_Entity_Named_Stmt_Decl
     (Node : Bare_Named_Stmt_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Named_Stmt_Decl;

   function Trace_Image (R : Internal_Entity_Named_Stmt_Decl) return String;

   type Internal_Entity_Not_Null is record

      Node : aliased Bare_Not_Null;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Not_Null : constant Internal_Entity_Not_Null;

   function Create_Internal_Entity_Not_Null
     (Node : Bare_Not_Null; Info : Internal_Entity_Info)
      return Internal_Entity_Not_Null;

   function Trace_Image (R : Internal_Entity_Not_Null) return String;

   type Internal_Entity_Not_Null_Absent is record

      Node : aliased Bare_Not_Null_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Not_Null_Absent : constant Internal_Entity_Not_Null_Absent;

   function Create_Internal_Entity_Not_Null_Absent
     (Node : Bare_Not_Null_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Not_Null_Absent;

   function Trace_Image (R : Internal_Entity_Not_Null_Absent) return String;

   type Internal_Entity_Not_Null_Present is record

      Node : aliased Bare_Not_Null_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Not_Null_Present : constant Internal_Entity_Not_Null_Present;

   function Create_Internal_Entity_Not_Null_Present
     (Node : Bare_Not_Null_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Not_Null_Present;

   function Trace_Image (R : Internal_Entity_Not_Null_Present) return String;

   type Internal_Entity_Null_Component_Decl is record

      Node : aliased Bare_Null_Component_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Null_Component_Decl : constant Internal_Entity_Null_Component_Decl;

   function Create_Internal_Entity_Null_Component_Decl
     (Node : Bare_Null_Component_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Null_Component_Decl;

   function Trace_Image
     (R : Internal_Entity_Null_Component_Decl) return String;

   type Internal_Entity_Null_Literal is record

      Node : aliased Bare_Null_Literal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Null_Literal : constant Internal_Entity_Null_Literal;

   function Create_Internal_Entity_Null_Literal
     (Node : Bare_Null_Literal; Info : Internal_Entity_Info)
      return Internal_Entity_Null_Literal;

   function Trace_Image (R : Internal_Entity_Null_Literal) return String;

   type Internal_Entity_Null_Record_Aggregate is record

      Node : aliased Bare_Null_Record_Aggregate;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Null_Record_Aggregate : constant Internal_Entity_Null_Record_Aggregate;

   function Create_Internal_Entity_Null_Record_Aggregate
     (Node : Bare_Null_Record_Aggregate; Info : Internal_Entity_Info)
      return Internal_Entity_Null_Record_Aggregate;

   function Trace_Image
     (R : Internal_Entity_Null_Record_Aggregate) return String;

   type Internal_Entity_Null_Record_Def is record

      Node : aliased Bare_Null_Record_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Null_Record_Def : constant Internal_Entity_Null_Record_Def;

   function Create_Internal_Entity_Null_Record_Def
     (Node : Bare_Null_Record_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Null_Record_Def;

   function Trace_Image (R : Internal_Entity_Null_Record_Def) return String;

   type Internal_Entity_Null_Stmt is record

      Node : aliased Bare_Null_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Null_Stmt : constant Internal_Entity_Null_Stmt;

   function Create_Internal_Entity_Null_Stmt
     (Node : Bare_Null_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Null_Stmt;

   function Trace_Image (R : Internal_Entity_Null_Stmt) return String;

   type Internal_Entity_Null_Subp_Decl is record

      Node : aliased Bare_Null_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Null_Subp_Decl : constant Internal_Entity_Null_Subp_Decl;

   function Create_Internal_Entity_Null_Subp_Decl
     (Node : Bare_Null_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Null_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Null_Subp_Decl) return String;

   type Internal_Entity_Number_Decl is record

      Node : aliased Bare_Number_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Number_Decl : constant Internal_Entity_Number_Decl;

   function Create_Internal_Entity_Number_Decl
     (Node : Bare_Number_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Number_Decl;

   function Trace_Image (R : Internal_Entity_Number_Decl) return String;

   type Internal_Entity_Op is record

      Node : aliased Bare_Op;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op : constant Internal_Entity_Op;

   function Create_Internal_Entity_Op
     (Node : Bare_Op; Info : Internal_Entity_Info) return Internal_Entity_Op;

   function Trace_Image (R : Internal_Entity_Op) return String;

   type Internal_Entity_Op_Abs is record

      Node : aliased Bare_Op_Abs;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Abs : constant Internal_Entity_Op_Abs;

   function Create_Internal_Entity_Op_Abs
     (Node : Bare_Op_Abs; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Abs;

   function Trace_Image (R : Internal_Entity_Op_Abs) return String;

   type Internal_Entity_Op_And is record

      Node : aliased Bare_Op_And;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_And : constant Internal_Entity_Op_And;

   function Create_Internal_Entity_Op_And
     (Node : Bare_Op_And; Info : Internal_Entity_Info)
      return Internal_Entity_Op_And;

   function Trace_Image (R : Internal_Entity_Op_And) return String;

   type Internal_Entity_Op_And_Then is record

      Node : aliased Bare_Op_And_Then;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_And_Then : constant Internal_Entity_Op_And_Then;

   function Create_Internal_Entity_Op_And_Then
     (Node : Bare_Op_And_Then; Info : Internal_Entity_Info)
      return Internal_Entity_Op_And_Then;

   function Trace_Image (R : Internal_Entity_Op_And_Then) return String;

   type Internal_Entity_Op_Concat is record

      Node : aliased Bare_Op_Concat;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Concat : constant Internal_Entity_Op_Concat;

   function Create_Internal_Entity_Op_Concat
     (Node : Bare_Op_Concat; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Concat;

   function Trace_Image (R : Internal_Entity_Op_Concat) return String;

   type Internal_Entity_Op_Div is record

      Node : aliased Bare_Op_Div;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Div : constant Internal_Entity_Op_Div;

   function Create_Internal_Entity_Op_Div
     (Node : Bare_Op_Div; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Div;

   function Trace_Image (R : Internal_Entity_Op_Div) return String;

   type Internal_Entity_Op_Double_Dot is record

      Node : aliased Bare_Op_Double_Dot;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Double_Dot : constant Internal_Entity_Op_Double_Dot;

   function Create_Internal_Entity_Op_Double_Dot
     (Node : Bare_Op_Double_Dot; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Double_Dot;

   function Trace_Image (R : Internal_Entity_Op_Double_Dot) return String;

   type Internal_Entity_Op_Eq is record

      Node : aliased Bare_Op_Eq;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Eq : constant Internal_Entity_Op_Eq;

   function Create_Internal_Entity_Op_Eq
     (Node : Bare_Op_Eq; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Eq;

   function Trace_Image (R : Internal_Entity_Op_Eq) return String;

   type Internal_Entity_Op_Gt is record

      Node : aliased Bare_Op_Gt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Gt : constant Internal_Entity_Op_Gt;

   function Create_Internal_Entity_Op_Gt
     (Node : Bare_Op_Gt; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Gt;

   function Trace_Image (R : Internal_Entity_Op_Gt) return String;

   type Internal_Entity_Op_Gte is record

      Node : aliased Bare_Op_Gte;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Gte : constant Internal_Entity_Op_Gte;

   function Create_Internal_Entity_Op_Gte
     (Node : Bare_Op_Gte; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Gte;

   function Trace_Image (R : Internal_Entity_Op_Gte) return String;

   type Internal_Entity_Op_In is record

      Node : aliased Bare_Op_In;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_In : constant Internal_Entity_Op_In;

   function Create_Internal_Entity_Op_In
     (Node : Bare_Op_In; Info : Internal_Entity_Info)
      return Internal_Entity_Op_In;

   function Trace_Image (R : Internal_Entity_Op_In) return String;

   type Internal_Entity_Op_Lt is record

      Node : aliased Bare_Op_Lt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Lt : constant Internal_Entity_Op_Lt;

   function Create_Internal_Entity_Op_Lt
     (Node : Bare_Op_Lt; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Lt;

   function Trace_Image (R : Internal_Entity_Op_Lt) return String;

   type Internal_Entity_Op_Lte is record

      Node : aliased Bare_Op_Lte;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Lte : constant Internal_Entity_Op_Lte;

   function Create_Internal_Entity_Op_Lte
     (Node : Bare_Op_Lte; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Lte;

   function Trace_Image (R : Internal_Entity_Op_Lte) return String;

   type Internal_Entity_Op_Minus is record

      Node : aliased Bare_Op_Minus;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Minus : constant Internal_Entity_Op_Minus;

   function Create_Internal_Entity_Op_Minus
     (Node : Bare_Op_Minus; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Minus;

   function Trace_Image (R : Internal_Entity_Op_Minus) return String;

   type Internal_Entity_Op_Mod is record

      Node : aliased Bare_Op_Mod;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Mod : constant Internal_Entity_Op_Mod;

   function Create_Internal_Entity_Op_Mod
     (Node : Bare_Op_Mod; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Mod;

   function Trace_Image (R : Internal_Entity_Op_Mod) return String;

   type Internal_Entity_Op_Mult is record

      Node : aliased Bare_Op_Mult;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Mult : constant Internal_Entity_Op_Mult;

   function Create_Internal_Entity_Op_Mult
     (Node : Bare_Op_Mult; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Mult;

   function Trace_Image (R : Internal_Entity_Op_Mult) return String;

   type Internal_Entity_Op_Neq is record

      Node : aliased Bare_Op_Neq;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Neq : constant Internal_Entity_Op_Neq;

   function Create_Internal_Entity_Op_Neq
     (Node : Bare_Op_Neq; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Neq;

   function Trace_Image (R : Internal_Entity_Op_Neq) return String;

   type Internal_Entity_Op_Not is record

      Node : aliased Bare_Op_Not;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Not : constant Internal_Entity_Op_Not;

   function Create_Internal_Entity_Op_Not
     (Node : Bare_Op_Not; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Not;

   function Trace_Image (R : Internal_Entity_Op_Not) return String;

   type Internal_Entity_Op_Not_In is record

      Node : aliased Bare_Op_Not_In;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Not_In : constant Internal_Entity_Op_Not_In;

   function Create_Internal_Entity_Op_Not_In
     (Node : Bare_Op_Not_In; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Not_In;

   function Trace_Image (R : Internal_Entity_Op_Not_In) return String;

   type Internal_Entity_Op_Or is record

      Node : aliased Bare_Op_Or;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Or : constant Internal_Entity_Op_Or;

   function Create_Internal_Entity_Op_Or
     (Node : Bare_Op_Or; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Or;

   function Trace_Image (R : Internal_Entity_Op_Or) return String;

   type Internal_Entity_Op_Or_Else is record

      Node : aliased Bare_Op_Or_Else;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Or_Else : constant Internal_Entity_Op_Or_Else;

   function Create_Internal_Entity_Op_Or_Else
     (Node : Bare_Op_Or_Else; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Or_Else;

   function Trace_Image (R : Internal_Entity_Op_Or_Else) return String;

   type Internal_Entity_Op_Plus is record

      Node : aliased Bare_Op_Plus;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Plus : constant Internal_Entity_Op_Plus;

   function Create_Internal_Entity_Op_Plus
     (Node : Bare_Op_Plus; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Plus;

   function Trace_Image (R : Internal_Entity_Op_Plus) return String;

   type Internal_Entity_Op_Pow is record

      Node : aliased Bare_Op_Pow;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Pow : constant Internal_Entity_Op_Pow;

   function Create_Internal_Entity_Op_Pow
     (Node : Bare_Op_Pow; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Pow;

   function Trace_Image (R : Internal_Entity_Op_Pow) return String;

   type Internal_Entity_Op_Rem is record

      Node : aliased Bare_Op_Rem;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Rem : constant Internal_Entity_Op_Rem;

   function Create_Internal_Entity_Op_Rem
     (Node : Bare_Op_Rem; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Rem;

   function Trace_Image (R : Internal_Entity_Op_Rem) return String;

   type Internal_Entity_Op_Xor is record

      Node : aliased Bare_Op_Xor;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Op_Xor : constant Internal_Entity_Op_Xor;

   function Create_Internal_Entity_Op_Xor
     (Node : Bare_Op_Xor; Info : Internal_Entity_Info)
      return Internal_Entity_Op_Xor;

   function Trace_Image (R : Internal_Entity_Op_Xor) return String;

   type Internal_Entity_Ordinary_Fixed_Point_Def is record

      Node : aliased Bare_Ordinary_Fixed_Point_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Ordinary_Fixed_Point_Def : constant Internal_Entity_Ordinary_Fixed_Point_Def;

   function Create_Internal_Entity_Ordinary_Fixed_Point_Def
     (Node : Bare_Ordinary_Fixed_Point_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Ordinary_Fixed_Point_Def;

   function Trace_Image
     (R : Internal_Entity_Ordinary_Fixed_Point_Def) return String;

   type Internal_Entity_Others_Designator is record

      Node : aliased Bare_Others_Designator;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Others_Designator : constant Internal_Entity_Others_Designator;

   function Create_Internal_Entity_Others_Designator
     (Node : Bare_Others_Designator; Info : Internal_Entity_Info)
      return Internal_Entity_Others_Designator;

   function Trace_Image (R : Internal_Entity_Others_Designator) return String;

   type Internal_Entity_Overriding_Node is record

      Node : aliased Bare_Overriding_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Overriding_Node : constant Internal_Entity_Overriding_Node;

   function Create_Internal_Entity_Overriding_Node
     (Node : Bare_Overriding_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Overriding_Node;

   function Trace_Image (R : Internal_Entity_Overriding_Node) return String;

   type Internal_Entity_Overriding_Not_Overriding is record

      Node : aliased Bare_Overriding_Not_Overriding;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Overriding_Not_Overriding : constant Internal_Entity_Overriding_Not_Overriding;

   function Create_Internal_Entity_Overriding_Not_Overriding
     (Node : Bare_Overriding_Not_Overriding; Info : Internal_Entity_Info)
      return Internal_Entity_Overriding_Not_Overriding;

   function Trace_Image
     (R : Internal_Entity_Overriding_Not_Overriding) return String;

   type Internal_Entity_Overriding_Overriding is record

      Node : aliased Bare_Overriding_Overriding;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Overriding_Overriding : constant Internal_Entity_Overriding_Overriding;

   function Create_Internal_Entity_Overriding_Overriding
     (Node : Bare_Overriding_Overriding; Info : Internal_Entity_Info)
      return Internal_Entity_Overriding_Overriding;

   function Trace_Image
     (R : Internal_Entity_Overriding_Overriding) return String;

   type Internal_Entity_Overriding_Unspecified is record

      Node : aliased Bare_Overriding_Unspecified;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Overriding_Unspecified : constant Internal_Entity_Overriding_Unspecified;

   function Create_Internal_Entity_Overriding_Unspecified
     (Node : Bare_Overriding_Unspecified; Info : Internal_Entity_Info)
      return Internal_Entity_Overriding_Unspecified;

   function Trace_Image
     (R : Internal_Entity_Overriding_Unspecified) return String;

   type Internal_Entity_Package_Body is record

      Node : aliased Bare_Package_Body;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Package_Body : constant Internal_Entity_Package_Body;

   function Create_Internal_Entity_Package_Body
     (Node : Bare_Package_Body; Info : Internal_Entity_Info)
      return Internal_Entity_Package_Body;

   function Trace_Image (R : Internal_Entity_Package_Body) return String;

   type Internal_Entity_Package_Body_Stub is record

      Node : aliased Bare_Package_Body_Stub;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Package_Body_Stub : constant Internal_Entity_Package_Body_Stub;

   function Create_Internal_Entity_Package_Body_Stub
     (Node : Bare_Package_Body_Stub; Info : Internal_Entity_Info)
      return Internal_Entity_Package_Body_Stub;

   function Trace_Image (R : Internal_Entity_Package_Body_Stub) return String;

   type Internal_Entity_Package_Decl is record

      Node : aliased Bare_Package_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Package_Decl : constant Internal_Entity_Package_Decl;

   function Create_Internal_Entity_Package_Decl
     (Node : Bare_Package_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Package_Decl;

   function Trace_Image (R : Internal_Entity_Package_Decl) return String;

   type Internal_Entity_Package_Renaming_Decl is record

      Node : aliased Bare_Package_Renaming_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Package_Renaming_Decl : constant Internal_Entity_Package_Renaming_Decl;

   function Create_Internal_Entity_Package_Renaming_Decl
     (Node : Bare_Package_Renaming_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Package_Renaming_Decl;

   function Trace_Image
     (R : Internal_Entity_Package_Renaming_Decl) return String;

   type Internal_Entity_Param_Assoc is record

      Node : aliased Bare_Param_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Param_Assoc : constant Internal_Entity_Param_Assoc;

   function Create_Internal_Entity_Param_Assoc
     (Node : Bare_Param_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Param_Assoc;

   function Trace_Image (R : Internal_Entity_Param_Assoc) return String;

   type Internal_Entity_Param_Spec is record

      Node : aliased Bare_Param_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Param_Spec : constant Internal_Entity_Param_Spec;

   function Create_Internal_Entity_Param_Spec
     (Node : Bare_Param_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Param_Spec;

   function Trace_Image (R : Internal_Entity_Param_Spec) return String;

   type Internal_Entity_Param_Spec_List is record

      Node : aliased Bare_Param_Spec_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Param_Spec_List : constant Internal_Entity_Param_Spec_List;

   function Create_Internal_Entity_Param_Spec_List
     (Node : Bare_Param_Spec_List; Info : Internal_Entity_Info)
      return Internal_Entity_Param_Spec_List;

   function Trace_Image (R : Internal_Entity_Param_Spec_List) return String;

   type Internal_Entity_Params is record

      Node : aliased Bare_Params;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Params : constant Internal_Entity_Params;

   function Create_Internal_Entity_Params
     (Node : Bare_Params; Info : Internal_Entity_Info)
      return Internal_Entity_Params;

   function Trace_Image (R : Internal_Entity_Params) return String;

   type Internal_Entity_Paren_Expr is record

      Node : aliased Bare_Paren_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Paren_Expr : constant Internal_Entity_Paren_Expr;

   function Create_Internal_Entity_Paren_Expr
     (Node : Bare_Paren_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Paren_Expr;

   function Trace_Image (R : Internal_Entity_Paren_Expr) return String;

   type Internal_Entity_Parent_List is record

      Node : aliased Bare_Parent_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Parent_List : constant Internal_Entity_Parent_List;

   function Create_Internal_Entity_Parent_List
     (Node : Bare_Parent_List; Info : Internal_Entity_Info)
      return Internal_Entity_Parent_List;

   function Trace_Image (R : Internal_Entity_Parent_List) return String;

   type Internal_Entity_Pragma_Argument_Assoc is record

      Node : aliased Bare_Pragma_Argument_Assoc;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Pragma_Argument_Assoc : constant Internal_Entity_Pragma_Argument_Assoc;

   function Create_Internal_Entity_Pragma_Argument_Assoc
     (Node : Bare_Pragma_Argument_Assoc; Info : Internal_Entity_Info)
      return Internal_Entity_Pragma_Argument_Assoc;

   function Trace_Image
     (R : Internal_Entity_Pragma_Argument_Assoc) return String;

   type Internal_Entity_Pragma_Node is record

      Node : aliased Bare_Pragma_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Pragma_Node : constant Internal_Entity_Pragma_Node;

   function Create_Internal_Entity_Pragma_Node
     (Node : Bare_Pragma_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Pragma_Node;

   function Trace_Image (R : Internal_Entity_Pragma_Node) return String;

   type Internal_Entity_Pragma_Node_List is record

      Node : aliased Bare_Pragma_Node_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Pragma_Node_List : constant Internal_Entity_Pragma_Node_List;

   function Create_Internal_Entity_Pragma_Node_List
     (Node : Bare_Pragma_Node_List; Info : Internal_Entity_Info)
      return Internal_Entity_Pragma_Node_List;

   function Trace_Image (R : Internal_Entity_Pragma_Node_List) return String;

   type Internal_Entity_Prim_Type_Accessor is record

      Node : aliased Bare_Prim_Type_Accessor;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Prim_Type_Accessor : constant Internal_Entity_Prim_Type_Accessor;

   function Create_Internal_Entity_Prim_Type_Accessor
     (Node : Bare_Prim_Type_Accessor; Info : Internal_Entity_Info)
      return Internal_Entity_Prim_Type_Accessor;

   function Trace_Image (R : Internal_Entity_Prim_Type_Accessor) return String;

   type Internal_Entity_Private_Node is record

      Node : aliased Bare_Private_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Private_Node : constant Internal_Entity_Private_Node;

   function Create_Internal_Entity_Private_Node
     (Node : Bare_Private_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Private_Node;

   function Trace_Image (R : Internal_Entity_Private_Node) return String;

   type Internal_Entity_Private_Absent is record

      Node : aliased Bare_Private_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Private_Absent : constant Internal_Entity_Private_Absent;

   function Create_Internal_Entity_Private_Absent
     (Node : Bare_Private_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Private_Absent;

   function Trace_Image (R : Internal_Entity_Private_Absent) return String;

   type Internal_Entity_Private_Part is record

      Node : aliased Bare_Private_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Private_Part : constant Internal_Entity_Private_Part;

   function Create_Internal_Entity_Private_Part
     (Node : Bare_Private_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Private_Part;

   function Trace_Image (R : Internal_Entity_Private_Part) return String;

   type Internal_Entity_Private_Present is record

      Node : aliased Bare_Private_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Private_Present : constant Internal_Entity_Private_Present;

   function Create_Internal_Entity_Private_Present
     (Node : Bare_Private_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Private_Present;

   function Trace_Image (R : Internal_Entity_Private_Present) return String;

   type Internal_Entity_Private_Type_Def is record

      Node : aliased Bare_Private_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Private_Type_Def : constant Internal_Entity_Private_Type_Def;

   function Create_Internal_Entity_Private_Type_Def
     (Node : Bare_Private_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Private_Type_Def;

   function Trace_Image (R : Internal_Entity_Private_Type_Def) return String;

   type Internal_Entity_Protected_Node is record

      Node : aliased Bare_Protected_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Node : constant Internal_Entity_Protected_Node;

   function Create_Internal_Entity_Protected_Node
     (Node : Bare_Protected_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Node;

   function Trace_Image (R : Internal_Entity_Protected_Node) return String;

   type Internal_Entity_Protected_Absent is record

      Node : aliased Bare_Protected_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Absent : constant Internal_Entity_Protected_Absent;

   function Create_Internal_Entity_Protected_Absent
     (Node : Bare_Protected_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Absent;

   function Trace_Image (R : Internal_Entity_Protected_Absent) return String;

   type Internal_Entity_Protected_Body is record

      Node : aliased Bare_Protected_Body;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Body : constant Internal_Entity_Protected_Body;

   function Create_Internal_Entity_Protected_Body
     (Node : Bare_Protected_Body; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Body;

   function Trace_Image (R : Internal_Entity_Protected_Body) return String;

   type Internal_Entity_Protected_Body_Stub is record

      Node : aliased Bare_Protected_Body_Stub;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Body_Stub : constant Internal_Entity_Protected_Body_Stub;

   function Create_Internal_Entity_Protected_Body_Stub
     (Node : Bare_Protected_Body_Stub; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Body_Stub;

   function Trace_Image
     (R : Internal_Entity_Protected_Body_Stub) return String;

   type Internal_Entity_Protected_Def is record

      Node : aliased Bare_Protected_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Def : constant Internal_Entity_Protected_Def;

   function Create_Internal_Entity_Protected_Def
     (Node : Bare_Protected_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Def;

   function Trace_Image (R : Internal_Entity_Protected_Def) return String;

   type Internal_Entity_Protected_Present is record

      Node : aliased Bare_Protected_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Present : constant Internal_Entity_Protected_Present;

   function Create_Internal_Entity_Protected_Present
     (Node : Bare_Protected_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Present;

   function Trace_Image (R : Internal_Entity_Protected_Present) return String;

   type Internal_Entity_Protected_Type_Decl is record

      Node : aliased Bare_Protected_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Protected_Type_Decl : constant Internal_Entity_Protected_Type_Decl;

   function Create_Internal_Entity_Protected_Type_Decl
     (Node : Bare_Protected_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Protected_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Protected_Type_Decl) return String;

   type Internal_Entity_Public_Part is record

      Node : aliased Bare_Public_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Public_Part : constant Internal_Entity_Public_Part;

   function Create_Internal_Entity_Public_Part
     (Node : Bare_Public_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Public_Part;

   function Trace_Image (R : Internal_Entity_Public_Part) return String;

   type Internal_Entity_Qual_Expr is record

      Node : aliased Bare_Qual_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Qual_Expr : constant Internal_Entity_Qual_Expr;

   function Create_Internal_Entity_Qual_Expr
     (Node : Bare_Qual_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Qual_Expr;

   function Trace_Image (R : Internal_Entity_Qual_Expr) return String;

   type Internal_Entity_Quantified_Expr is record

      Node : aliased Bare_Quantified_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Quantified_Expr : constant Internal_Entity_Quantified_Expr;

   function Create_Internal_Entity_Quantified_Expr
     (Node : Bare_Quantified_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Quantified_Expr;

   function Trace_Image (R : Internal_Entity_Quantified_Expr) return String;

   type Internal_Entity_Quantifier is record

      Node : aliased Bare_Quantifier;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Quantifier : constant Internal_Entity_Quantifier;

   function Create_Internal_Entity_Quantifier
     (Node : Bare_Quantifier; Info : Internal_Entity_Info)
      return Internal_Entity_Quantifier;

   function Trace_Image (R : Internal_Entity_Quantifier) return String;

   type Internal_Entity_Quantifier_All is record

      Node : aliased Bare_Quantifier_All;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Quantifier_All : constant Internal_Entity_Quantifier_All;

   function Create_Internal_Entity_Quantifier_All
     (Node : Bare_Quantifier_All; Info : Internal_Entity_Info)
      return Internal_Entity_Quantifier_All;

   function Trace_Image (R : Internal_Entity_Quantifier_All) return String;

   type Internal_Entity_Quantifier_Some is record

      Node : aliased Bare_Quantifier_Some;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Quantifier_Some : constant Internal_Entity_Quantifier_Some;

   function Create_Internal_Entity_Quantifier_Some
     (Node : Bare_Quantifier_Some; Info : Internal_Entity_Info)
      return Internal_Entity_Quantifier_Some;

   function Trace_Image (R : Internal_Entity_Quantifier_Some) return String;

   type Internal_Entity_Raise_Expr is record

      Node : aliased Bare_Raise_Expr;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Raise_Expr : constant Internal_Entity_Raise_Expr;

   function Create_Internal_Entity_Raise_Expr
     (Node : Bare_Raise_Expr; Info : Internal_Entity_Info)
      return Internal_Entity_Raise_Expr;

   function Trace_Image (R : Internal_Entity_Raise_Expr) return String;

   type Internal_Entity_Raise_Stmt is record

      Node : aliased Bare_Raise_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Raise_Stmt : constant Internal_Entity_Raise_Stmt;

   function Create_Internal_Entity_Raise_Stmt
     (Node : Bare_Raise_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Raise_Stmt;

   function Trace_Image (R : Internal_Entity_Raise_Stmt) return String;

   type Internal_Entity_Range_Constraint is record

      Node : aliased Bare_Range_Constraint;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Range_Constraint : constant Internal_Entity_Range_Constraint;

   function Create_Internal_Entity_Range_Constraint
     (Node : Bare_Range_Constraint; Info : Internal_Entity_Info)
      return Internal_Entity_Range_Constraint;

   function Trace_Image (R : Internal_Entity_Range_Constraint) return String;

   type Internal_Entity_Range_Spec is record

      Node : aliased Bare_Range_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Range_Spec : constant Internal_Entity_Range_Spec;

   function Create_Internal_Entity_Range_Spec
     (Node : Bare_Range_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Range_Spec;

   function Trace_Image (R : Internal_Entity_Range_Spec) return String;

   type Internal_Entity_Real_Literal is record

      Node : aliased Bare_Real_Literal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Real_Literal : constant Internal_Entity_Real_Literal;

   function Create_Internal_Entity_Real_Literal
     (Node : Bare_Real_Literal; Info : Internal_Entity_Info)
      return Internal_Entity_Real_Literal;

   function Trace_Image (R : Internal_Entity_Real_Literal) return String;

   type Internal_Entity_Record_Def is record

      Node : aliased Bare_Record_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Record_Def : constant Internal_Entity_Record_Def;

   function Create_Internal_Entity_Record_Def
     (Node : Bare_Record_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Record_Def;

   function Trace_Image (R : Internal_Entity_Record_Def) return String;

   type Internal_Entity_Record_Rep_Clause is record

      Node : aliased Bare_Record_Rep_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Record_Rep_Clause : constant Internal_Entity_Record_Rep_Clause;

   function Create_Internal_Entity_Record_Rep_Clause
     (Node : Bare_Record_Rep_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Record_Rep_Clause;

   function Trace_Image (R : Internal_Entity_Record_Rep_Clause) return String;

   type Internal_Entity_Record_Type_Def is record

      Node : aliased Bare_Record_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Record_Type_Def : constant Internal_Entity_Record_Type_Def;

   function Create_Internal_Entity_Record_Type_Def
     (Node : Bare_Record_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Record_Type_Def;

   function Trace_Image (R : Internal_Entity_Record_Type_Def) return String;

   type Internal_Entity_Relation_Op is record

      Node : aliased Bare_Relation_Op;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Relation_Op : constant Internal_Entity_Relation_Op;

   function Create_Internal_Entity_Relation_Op
     (Node : Bare_Relation_Op; Info : Internal_Entity_Info)
      return Internal_Entity_Relation_Op;

   function Trace_Image (R : Internal_Entity_Relation_Op) return String;

   type Internal_Entity_Renaming_Clause is record

      Node : aliased Bare_Renaming_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Renaming_Clause : constant Internal_Entity_Renaming_Clause;

   function Create_Internal_Entity_Renaming_Clause
     (Node : Bare_Renaming_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Renaming_Clause;

   function Trace_Image (R : Internal_Entity_Renaming_Clause) return String;

   type Internal_Entity_Requeue_Stmt is record

      Node : aliased Bare_Requeue_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Requeue_Stmt : constant Internal_Entity_Requeue_Stmt;

   function Create_Internal_Entity_Requeue_Stmt
     (Node : Bare_Requeue_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Requeue_Stmt;

   function Trace_Image (R : Internal_Entity_Requeue_Stmt) return String;

   type Internal_Entity_Return_Stmt is record

      Node : aliased Bare_Return_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Return_Stmt : constant Internal_Entity_Return_Stmt;

   function Create_Internal_Entity_Return_Stmt
     (Node : Bare_Return_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Return_Stmt;

   function Trace_Image (R : Internal_Entity_Return_Stmt) return String;

   type Internal_Entity_Reverse_Node is record

      Node : aliased Bare_Reverse_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Reverse_Node : constant Internal_Entity_Reverse_Node;

   function Create_Internal_Entity_Reverse_Node
     (Node : Bare_Reverse_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Reverse_Node;

   function Trace_Image (R : Internal_Entity_Reverse_Node) return String;

   type Internal_Entity_Reverse_Absent is record

      Node : aliased Bare_Reverse_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Reverse_Absent : constant Internal_Entity_Reverse_Absent;

   function Create_Internal_Entity_Reverse_Absent
     (Node : Bare_Reverse_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Reverse_Absent;

   function Trace_Image (R : Internal_Entity_Reverse_Absent) return String;

   type Internal_Entity_Reverse_Present is record

      Node : aliased Bare_Reverse_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Reverse_Present : constant Internal_Entity_Reverse_Present;

   function Create_Internal_Entity_Reverse_Present
     (Node : Bare_Reverse_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Reverse_Present;

   function Trace_Image (R : Internal_Entity_Reverse_Present) return String;

   type Internal_Entity_Select_Stmt is record

      Node : aliased Bare_Select_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Select_Stmt : constant Internal_Entity_Select_Stmt;

   function Create_Internal_Entity_Select_Stmt
     (Node : Bare_Select_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_Select_Stmt;

   function Trace_Image (R : Internal_Entity_Select_Stmt) return String;

   type Internal_Entity_Select_When_Part is record

      Node : aliased Bare_Select_When_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Select_When_Part : constant Internal_Entity_Select_When_Part;

   function Create_Internal_Entity_Select_When_Part
     (Node : Bare_Select_When_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Select_When_Part;

   function Trace_Image (R : Internal_Entity_Select_When_Part) return String;

   type Internal_Entity_Select_When_Part_List is record

      Node : aliased Bare_Select_When_Part_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Select_When_Part_List : constant Internal_Entity_Select_When_Part_List;

   function Create_Internal_Entity_Select_When_Part_List
     (Node : Bare_Select_When_Part_List; Info : Internal_Entity_Info)
      return Internal_Entity_Select_When_Part_List;

   function Trace_Image
     (R : Internal_Entity_Select_When_Part_List) return String;

   type Internal_Entity_Signed_Int_Type_Def is record

      Node : aliased Bare_Signed_Int_Type_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Signed_Int_Type_Def : constant Internal_Entity_Signed_Int_Type_Def;

   function Create_Internal_Entity_Signed_Int_Type_Def
     (Node : Bare_Signed_Int_Type_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Signed_Int_Type_Def;

   function Trace_Image
     (R : Internal_Entity_Signed_Int_Type_Def) return String;

   type Internal_Entity_Single_Protected_Decl is record

      Node : aliased Bare_Single_Protected_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Single_Protected_Decl : constant Internal_Entity_Single_Protected_Decl;

   function Create_Internal_Entity_Single_Protected_Decl
     (Node : Bare_Single_Protected_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Single_Protected_Decl;

   function Trace_Image
     (R : Internal_Entity_Single_Protected_Decl) return String;

   type Internal_Entity_Single_Task_Decl is record

      Node : aliased Bare_Single_Task_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Single_Task_Decl : constant Internal_Entity_Single_Task_Decl;

   function Create_Internal_Entity_Single_Task_Decl
     (Node : Bare_Single_Task_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Single_Task_Decl;

   function Trace_Image (R : Internal_Entity_Single_Task_Decl) return String;

   type Internal_Entity_Task_Type_Decl is record

      Node : aliased Bare_Task_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Task_Type_Decl : constant Internal_Entity_Task_Type_Decl;

   function Create_Internal_Entity_Task_Type_Decl
     (Node : Bare_Task_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Task_Type_Decl;

   function Trace_Image (R : Internal_Entity_Task_Type_Decl) return String;

   type Internal_Entity_Single_Task_Type_Decl is record

      Node : aliased Bare_Single_Task_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Single_Task_Type_Decl : constant Internal_Entity_Single_Task_Type_Decl;

   function Create_Internal_Entity_Single_Task_Type_Decl
     (Node : Bare_Single_Task_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Single_Task_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Single_Task_Type_Decl) return String;

   type Internal_Entity_Stmt_List is record

      Node : aliased Bare_Stmt_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Stmt_List : constant Internal_Entity_Stmt_List;

   function Create_Internal_Entity_Stmt_List
     (Node : Bare_Stmt_List; Info : Internal_Entity_Info)
      return Internal_Entity_Stmt_List;

   function Trace_Image (R : Internal_Entity_Stmt_List) return String;

   type Internal_Entity_String_Literal is record

      Node : aliased Bare_String_Literal;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_String_Literal : constant Internal_Entity_String_Literal;

   function Create_Internal_Entity_String_Literal
     (Node : Bare_String_Literal; Info : Internal_Entity_Info)
      return Internal_Entity_String_Literal;

   function Trace_Image (R : Internal_Entity_String_Literal) return String;

   type Internal_Entity_Subp_Body is record

      Node : aliased Bare_Subp_Body;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Body : constant Internal_Entity_Subp_Body;

   function Create_Internal_Entity_Subp_Body
     (Node : Bare_Subp_Body; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Body;

   function Trace_Image (R : Internal_Entity_Subp_Body) return String;

   type Internal_Entity_Subp_Body_Stub is record

      Node : aliased Bare_Subp_Body_Stub;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Body_Stub : constant Internal_Entity_Subp_Body_Stub;

   function Create_Internal_Entity_Subp_Body_Stub
     (Node : Bare_Subp_Body_Stub; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Body_Stub;

   function Trace_Image (R : Internal_Entity_Subp_Body_Stub) return String;

   type Internal_Entity_Subp_Decl is record

      Node : aliased Bare_Subp_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Decl : constant Internal_Entity_Subp_Decl;

   function Create_Internal_Entity_Subp_Decl
     (Node : Bare_Subp_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Decl;

   function Trace_Image (R : Internal_Entity_Subp_Decl) return String;

   type Internal_Entity_Subp_Kind is record

      Node : aliased Bare_Subp_Kind;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Kind : constant Internal_Entity_Subp_Kind;

   function Create_Internal_Entity_Subp_Kind
     (Node : Bare_Subp_Kind; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Kind;

   function Trace_Image (R : Internal_Entity_Subp_Kind) return String;

   type Internal_Entity_Subp_Kind_Function is record

      Node : aliased Bare_Subp_Kind_Function;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Kind_Function : constant Internal_Entity_Subp_Kind_Function;

   function Create_Internal_Entity_Subp_Kind_Function
     (Node : Bare_Subp_Kind_Function; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Kind_Function;

   function Trace_Image (R : Internal_Entity_Subp_Kind_Function) return String;

   type Internal_Entity_Subp_Kind_Procedure is record

      Node : aliased Bare_Subp_Kind_Procedure;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Kind_Procedure : constant Internal_Entity_Subp_Kind_Procedure;

   function Create_Internal_Entity_Subp_Kind_Procedure
     (Node : Bare_Subp_Kind_Procedure; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Kind_Procedure;

   function Trace_Image
     (R : Internal_Entity_Subp_Kind_Procedure) return String;

   type Internal_Entity_Subp_Renaming_Decl is record

      Node : aliased Bare_Subp_Renaming_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Renaming_Decl : constant Internal_Entity_Subp_Renaming_Decl;

   function Create_Internal_Entity_Subp_Renaming_Decl
     (Node : Bare_Subp_Renaming_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Renaming_Decl;

   function Trace_Image (R : Internal_Entity_Subp_Renaming_Decl) return String;

   type Internal_Entity_Subp_Spec is record

      Node : aliased Bare_Subp_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subp_Spec : constant Internal_Entity_Subp_Spec;

   function Create_Internal_Entity_Subp_Spec
     (Node : Bare_Subp_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_Subp_Spec;

   function Trace_Image (R : Internal_Entity_Subp_Spec) return String;

   type Internal_Entity_Subtype_Decl is record

      Node : aliased Bare_Subtype_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subtype_Decl : constant Internal_Entity_Subtype_Decl;

   function Create_Internal_Entity_Subtype_Decl
     (Node : Bare_Subtype_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Subtype_Decl;

   function Trace_Image (R : Internal_Entity_Subtype_Decl) return String;

   type Internal_Entity_Subunit is record

      Node : aliased Bare_Subunit;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Subunit : constant Internal_Entity_Subunit;

   function Create_Internal_Entity_Subunit
     (Node : Bare_Subunit; Info : Internal_Entity_Info)
      return Internal_Entity_Subunit;

   function Trace_Image (R : Internal_Entity_Subunit) return String;

   type Internal_Entity_Synchronized_Node is record

      Node : aliased Bare_Synchronized_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Synchronized_Node : constant Internal_Entity_Synchronized_Node;

   function Create_Internal_Entity_Synchronized_Node
     (Node : Bare_Synchronized_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Synchronized_Node;

   function Trace_Image (R : Internal_Entity_Synchronized_Node) return String;

   type Internal_Entity_Synchronized_Absent is record

      Node : aliased Bare_Synchronized_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Synchronized_Absent : constant Internal_Entity_Synchronized_Absent;

   function Create_Internal_Entity_Synchronized_Absent
     (Node : Bare_Synchronized_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Synchronized_Absent;

   function Trace_Image
     (R : Internal_Entity_Synchronized_Absent) return String;

   type Internal_Entity_Synchronized_Present is record

      Node : aliased Bare_Synchronized_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Synchronized_Present : constant Internal_Entity_Synchronized_Present;

   function Create_Internal_Entity_Synchronized_Present
     (Node : Bare_Synchronized_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Synchronized_Present;

   function Trace_Image
     (R : Internal_Entity_Synchronized_Present) return String;

   type Internal_Entity_Synth_Anonymous_Type_Decl is record

      Node : aliased Bare_Synth_Anonymous_Type_Decl;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Synth_Anonymous_Type_Decl : constant Internal_Entity_Synth_Anonymous_Type_Decl;

   function Create_Internal_Entity_Synth_Anonymous_Type_Decl
     (Node : Bare_Synth_Anonymous_Type_Decl; Info : Internal_Entity_Info)
      return Internal_Entity_Synth_Anonymous_Type_Decl;

   function Trace_Image
     (R : Internal_Entity_Synth_Anonymous_Type_Decl) return String;

   type Internal_Entity_Synthetic_Renaming_Clause is record

      Node : aliased Bare_Synthetic_Renaming_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Synthetic_Renaming_Clause : constant Internal_Entity_Synthetic_Renaming_Clause;

   function Create_Internal_Entity_Synthetic_Renaming_Clause
     (Node : Bare_Synthetic_Renaming_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Synthetic_Renaming_Clause;

   function Trace_Image
     (R : Internal_Entity_Synthetic_Renaming_Clause) return String;

   type Internal_Entity_Tagged_Node is record

      Node : aliased Bare_Tagged_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Tagged_Node : constant Internal_Entity_Tagged_Node;

   function Create_Internal_Entity_Tagged_Node
     (Node : Bare_Tagged_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Tagged_Node;

   function Trace_Image (R : Internal_Entity_Tagged_Node) return String;

   type Internal_Entity_Tagged_Absent is record

      Node : aliased Bare_Tagged_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Tagged_Absent : constant Internal_Entity_Tagged_Absent;

   function Create_Internal_Entity_Tagged_Absent
     (Node : Bare_Tagged_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Tagged_Absent;

   function Trace_Image (R : Internal_Entity_Tagged_Absent) return String;

   type Internal_Entity_Tagged_Present is record

      Node : aliased Bare_Tagged_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Tagged_Present : constant Internal_Entity_Tagged_Present;

   function Create_Internal_Entity_Tagged_Present
     (Node : Bare_Tagged_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Tagged_Present;

   function Trace_Image (R : Internal_Entity_Tagged_Present) return String;

   type Internal_Entity_Target_Name is record

      Node : aliased Bare_Target_Name;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Target_Name : constant Internal_Entity_Target_Name;

   function Create_Internal_Entity_Target_Name
     (Node : Bare_Target_Name; Info : Internal_Entity_Info)
      return Internal_Entity_Target_Name;

   function Trace_Image (R : Internal_Entity_Target_Name) return String;

   type Internal_Entity_Task_Body is record

      Node : aliased Bare_Task_Body;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Task_Body : constant Internal_Entity_Task_Body;

   function Create_Internal_Entity_Task_Body
     (Node : Bare_Task_Body; Info : Internal_Entity_Info)
      return Internal_Entity_Task_Body;

   function Trace_Image (R : Internal_Entity_Task_Body) return String;

   type Internal_Entity_Task_Body_Stub is record

      Node : aliased Bare_Task_Body_Stub;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Task_Body_Stub : constant Internal_Entity_Task_Body_Stub;

   function Create_Internal_Entity_Task_Body_Stub
     (Node : Bare_Task_Body_Stub; Info : Internal_Entity_Info)
      return Internal_Entity_Task_Body_Stub;

   function Trace_Image (R : Internal_Entity_Task_Body_Stub) return String;

   type Internal_Entity_Task_Def is record

      Node : aliased Bare_Task_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Task_Def : constant Internal_Entity_Task_Def;

   function Create_Internal_Entity_Task_Def
     (Node : Bare_Task_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Task_Def;

   function Trace_Image (R : Internal_Entity_Task_Def) return String;

   type Internal_Entity_Terminate_Alternative is record

      Node : aliased Bare_Terminate_Alternative;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Terminate_Alternative : constant Internal_Entity_Terminate_Alternative;

   function Create_Internal_Entity_Terminate_Alternative
     (Node : Bare_Terminate_Alternative; Info : Internal_Entity_Info)
      return Internal_Entity_Terminate_Alternative;

   function Trace_Image
     (R : Internal_Entity_Terminate_Alternative) return String;

   type Internal_Entity_Type_Access_Def is record

      Node : aliased Bare_Type_Access_Def;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Type_Access_Def : constant Internal_Entity_Type_Access_Def;

   function Create_Internal_Entity_Type_Access_Def
     (Node : Bare_Type_Access_Def; Info : Internal_Entity_Info)
      return Internal_Entity_Type_Access_Def;

   function Trace_Image (R : Internal_Entity_Type_Access_Def) return String;

   type Internal_Entity_Un_Op is record

      Node : aliased Bare_Un_Op;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Un_Op : constant Internal_Entity_Un_Op;

   function Create_Internal_Entity_Un_Op
     (Node : Bare_Un_Op; Info : Internal_Entity_Info)
      return Internal_Entity_Un_Op;

   function Trace_Image (R : Internal_Entity_Un_Op) return String;

   type Internal_Entity_Unconstrained_Array_Index is record

      Node : aliased Bare_Unconstrained_Array_Index;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Unconstrained_Array_Index : constant Internal_Entity_Unconstrained_Array_Index;

   function Create_Internal_Entity_Unconstrained_Array_Index
     (Node : Bare_Unconstrained_Array_Index; Info : Internal_Entity_Info)
      return Internal_Entity_Unconstrained_Array_Index;

   function Trace_Image
     (R : Internal_Entity_Unconstrained_Array_Index) return String;

   type Internal_Entity_Unconstrained_Array_Index_List is record

      Node : aliased Bare_Unconstrained_Array_Index_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Unconstrained_Array_Index_List : constant Internal_Entity_Unconstrained_Array_Index_List;

   function Create_Internal_Entity_Unconstrained_Array_Index_List
     (Node : Bare_Unconstrained_Array_Index_List; Info : Internal_Entity_Info)
      return Internal_Entity_Unconstrained_Array_Index_List;

   function Trace_Image
     (R : Internal_Entity_Unconstrained_Array_Index_List) return String;

   type Internal_Entity_Unconstrained_Array_Indices is record

      Node : aliased Bare_Unconstrained_Array_Indices;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Unconstrained_Array_Indices : constant Internal_Entity_Unconstrained_Array_Indices;

   function Create_Internal_Entity_Unconstrained_Array_Indices
     (Node : Bare_Unconstrained_Array_Indices; Info : Internal_Entity_Info)
      return Internal_Entity_Unconstrained_Array_Indices;

   function Trace_Image
     (R : Internal_Entity_Unconstrained_Array_Indices) return String;

   type Internal_Entity_Unknown_Discriminant_Part is record

      Node : aliased Bare_Unknown_Discriminant_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Unknown_Discriminant_Part : constant Internal_Entity_Unknown_Discriminant_Part;

   function Create_Internal_Entity_Unknown_Discriminant_Part
     (Node : Bare_Unknown_Discriminant_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Unknown_Discriminant_Part;

   function Trace_Image
     (R : Internal_Entity_Unknown_Discriminant_Part) return String;

   type Internal_Entity_Until_Node is record

      Node : aliased Bare_Until_Node;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Until_Node : constant Internal_Entity_Until_Node;

   function Create_Internal_Entity_Until_Node
     (Node : Bare_Until_Node; Info : Internal_Entity_Info)
      return Internal_Entity_Until_Node;

   function Trace_Image (R : Internal_Entity_Until_Node) return String;

   type Internal_Entity_Until_Absent is record

      Node : aliased Bare_Until_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Until_Absent : constant Internal_Entity_Until_Absent;

   function Create_Internal_Entity_Until_Absent
     (Node : Bare_Until_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_Until_Absent;

   function Trace_Image (R : Internal_Entity_Until_Absent) return String;

   type Internal_Entity_Until_Present is record

      Node : aliased Bare_Until_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Until_Present : constant Internal_Entity_Until_Present;

   function Create_Internal_Entity_Until_Present
     (Node : Bare_Until_Present; Info : Internal_Entity_Info)
      return Internal_Entity_Until_Present;

   function Trace_Image (R : Internal_Entity_Until_Present) return String;

   type Internal_Entity_Update_Attribute_Ref is record

      Node : aliased Bare_Update_Attribute_Ref;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Update_Attribute_Ref : constant Internal_Entity_Update_Attribute_Ref;

   function Create_Internal_Entity_Update_Attribute_Ref
     (Node : Bare_Update_Attribute_Ref; Info : Internal_Entity_Info)
      return Internal_Entity_Update_Attribute_Ref;

   function Trace_Image
     (R : Internal_Entity_Update_Attribute_Ref) return String;

   type Internal_Entity_Use_Clause is record

      Node : aliased Bare_Use_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Use_Clause : constant Internal_Entity_Use_Clause;

   function Create_Internal_Entity_Use_Clause
     (Node : Bare_Use_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Use_Clause;

   function Trace_Image (R : Internal_Entity_Use_Clause) return String;

   type Internal_Entity_Use_Package_Clause is record

      Node : aliased Bare_Use_Package_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Use_Package_Clause : constant Internal_Entity_Use_Package_Clause;

   function Create_Internal_Entity_Use_Package_Clause
     (Node : Bare_Use_Package_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Use_Package_Clause;

   function Trace_Image (R : Internal_Entity_Use_Package_Clause) return String;

   type Internal_Entity_Use_Type_Clause is record

      Node : aliased Bare_Use_Type_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Use_Type_Clause : constant Internal_Entity_Use_Type_Clause;

   function Create_Internal_Entity_Use_Type_Clause
     (Node : Bare_Use_Type_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_Use_Type_Clause;

   function Trace_Image (R : Internal_Entity_Use_Type_Clause) return String;

   type Internal_Entity_Variant is record

      Node : aliased Bare_Variant;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Variant : constant Internal_Entity_Variant;

   function Create_Internal_Entity_Variant
     (Node : Bare_Variant; Info : Internal_Entity_Info)
      return Internal_Entity_Variant;

   function Trace_Image (R : Internal_Entity_Variant) return String;

   type Internal_Entity_Variant_List is record

      Node : aliased Bare_Variant_List;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Variant_List : constant Internal_Entity_Variant_List;

   function Create_Internal_Entity_Variant_List
     (Node : Bare_Variant_List; Info : Internal_Entity_Info)
      return Internal_Entity_Variant_List;

   function Trace_Image (R : Internal_Entity_Variant_List) return String;

   type Internal_Entity_Variant_Part is record

      Node : aliased Bare_Variant_Part;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_Variant_Part : constant Internal_Entity_Variant_Part;

   function Create_Internal_Entity_Variant_Part
     (Node : Bare_Variant_Part; Info : Internal_Entity_Info)
      return Internal_Entity_Variant_Part;

   function Trace_Image (R : Internal_Entity_Variant_Part) return String;

   type Internal_Entity_While_Loop_Spec is record

      Node : aliased Bare_While_Loop_Spec;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_While_Loop_Spec : constant Internal_Entity_While_Loop_Spec;

   function Create_Internal_Entity_While_Loop_Spec
     (Node : Bare_While_Loop_Spec; Info : Internal_Entity_Info)
      return Internal_Entity_While_Loop_Spec;

   function Trace_Image (R : Internal_Entity_While_Loop_Spec) return String;

   type Internal_Entity_While_Loop_Stmt is record

      Node : aliased Bare_While_Loop_Stmt;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_While_Loop_Stmt : constant Internal_Entity_While_Loop_Stmt;

   function Create_Internal_Entity_While_Loop_Stmt
     (Node : Bare_While_Loop_Stmt; Info : Internal_Entity_Info)
      return Internal_Entity_While_Loop_Stmt;

   function Trace_Image (R : Internal_Entity_While_Loop_Stmt) return String;

   type Internal_Entity_With_Clause is record

      Node : aliased Bare_With_Clause;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_With_Clause : constant Internal_Entity_With_Clause;

   function Create_Internal_Entity_With_Clause
     (Node : Bare_With_Clause; Info : Internal_Entity_Info)
      return Internal_Entity_With_Clause;

   function Trace_Image (R : Internal_Entity_With_Clause) return String;

   type Internal_Entity_With_Private is record

      Node : aliased Bare_With_Private;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_With_Private : constant Internal_Entity_With_Private;

   function Create_Internal_Entity_With_Private
     (Node : Bare_With_Private; Info : Internal_Entity_Info)
      return Internal_Entity_With_Private;

   function Trace_Image (R : Internal_Entity_With_Private) return String;

   type Internal_Entity_With_Private_Absent is record

      Node : aliased Bare_With_Private_Absent;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_With_Private_Absent : constant Internal_Entity_With_Private_Absent;

   function Create_Internal_Entity_With_Private_Absent
     (Node : Bare_With_Private_Absent; Info : Internal_Entity_Info)
      return Internal_Entity_With_Private_Absent;

   function Trace_Image
     (R : Internal_Entity_With_Private_Absent) return String;

   type Internal_Entity_With_Private_Present is record

      Node : aliased Bare_With_Private_Present;
      --  The stored AST node

      Info : aliased Internal_Entity_Info;
      --  Entity info for this node

   end record with
      Convention => C;
   No_Entity_With_Private_Present : constant Internal_Entity_With_Private_Present;

   function Create_Internal_Entity_With_Private_Present
     (Node : Bare_With_Private_Present; Info : Internal_Entity_Info)
      return Internal_Entity_With_Private_Present;

   function Trace_Image
     (R : Internal_Entity_With_Private_Present) return String;

   type Internal_Env_Assoc is record

      Key : aliased Symbol_Type;

      Val : aliased Bare_Ada_Node;

      Dest_Env : aliased Lexical_Env;

      Metadata : aliased Internal_Metadata;

   end record with
      Convention => C;
   No_Env_Assoc : constant Internal_Env_Assoc;

   procedure Inc_Ref (R : Internal_Env_Assoc);
   procedure Dec_Ref (R : in out Internal_Env_Assoc);

   function Equivalent (L, R : Internal_Env_Assoc) return Boolean;

   function Trace_Image (R : Internal_Env_Assoc) return String;

   type Internal_Eval_Discrete_Range is record

      Low_Bound : aliased Big_Integer_Type;

      High_Bound : aliased Big_Integer_Type;

   end record with
      Convention => C;
   No_Eval_Discrete_Range : constant Internal_Eval_Discrete_Range;

   procedure Inc_Ref (R : Internal_Eval_Discrete_Range);
   procedure Dec_Ref (R : in out Internal_Eval_Discrete_Range);

   function Equivalent (L, R : Internal_Eval_Discrete_Range) return Boolean;

   function Trace_Image (R : Internal_Eval_Discrete_Range) return String;

   type Internal_Expected_Type_For_Expr is record

      Expected_Type : aliased Internal_Entity_Type_Expr;

      Expr : aliased Internal_Entity_Expr;

   end record with
      Convention => C;
   No_Expected_Type_For_Expr : constant Internal_Expected_Type_For_Expr;

   function Trace_Image (R : Internal_Expected_Type_For_Expr) return String;

   type Internal_Logic_Val_Result is record

      Success : aliased Boolean;

      Value : aliased Internal_Entity;

   end record with
      Convention => C;
   No_Logic_Val_Result : constant Internal_Logic_Val_Result;

   function Trace_Image (R : Internal_Logic_Val_Result) return String;

   type Internal_Multidim_Aggregate_Info is record

      Agg : aliased Internal_Entity_Base_Aggregate;
      --  the top level aggregate

      Typ : aliased Internal_Entity_Base_Type_Decl;
      --  the type of the array

      Rank : aliased Integer;
      --  the rank of the original sub-aggregate

   end record with
      Convention => C;
   No_Multidim_Aggregate_Info : constant Internal_Multidim_Aggregate_Info;

   function Trace_Image (R : Internal_Multidim_Aggregate_Info) return String;

   type Internal_Param_Actual is record

      Param : aliased Internal_Entity_Defining_Name;

      Actual : aliased Internal_Entity_Expr;

   end record with
      Convention => C;
   No_Param_Actual : constant Internal_Param_Actual;

   function Trace_Image (R : Internal_Param_Actual) return String;

   type Internal_Single_Actual is record

      Name : aliased Bare_Base_Id;

      Assoc : aliased Internal_Entity_Basic_Assoc;

   end record with
      Convention => C;
   No_Single_Actual : constant Internal_Single_Actual;

   function Trace_Image (R : Internal_Single_Actual) return String;

   type Internal_Single_Formal is record

      Name : aliased Internal_Entity_Defining_Name;

      Spec : aliased Internal_Entity_Base_Formal_Param_Decl;

   end record with
      Convention => C;
   No_Single_Formal : constant Internal_Single_Formal;

   function Trace_Image (R : Internal_Single_Formal) return String;

   type Internal_Param_Match is record

      Has_Matched : aliased Boolean;
      --  Whether the matched ParamAssoc a ParamSpec.

      Actual : aliased Internal_Single_Actual;

      Formal : aliased Internal_Single_Formal;

   end record with
      Convention => C;
   No_Param_Match : constant Internal_Param_Match;

   function Trace_Image (R : Internal_Param_Match) return String;

   type Internal_Ref_Result is record

      Ref : aliased Internal_Entity_Base_Id;

      Kind : aliased Ref_Result_Kind;

   end record with
      Convention => C;
   No_Ref_Result : constant Internal_Ref_Result;

   function Trace_Image (R : Internal_Ref_Result) return String;

   type Internal_Refd_Decl is record

      Decl : aliased Internal_Entity_Basic_Decl;

      Kind : aliased Ref_Result_Kind;

   end record with
      Convention => C;
   No_Refd_Decl : constant Internal_Refd_Decl;

   function Trace_Image (R : Internal_Refd_Decl) return String;

   type Internal_Refd_Def is record

      Def_Name : aliased Internal_Entity_Defining_Name;

      Kind : aliased Ref_Result_Kind;

   end record with
      Convention => C;
   No_Refd_Def : constant Internal_Refd_Def;

   function Trace_Image (R : Internal_Refd_Def) return String;

   type Internal_Substitution is record

      From_Decl : aliased Internal_Entity_Basic_Decl;
      --  The declaration to substitute.

      To_Value : aliased Big_Integer_Type;
      --  The value by which to substitute the declaration.

      Value_Type : aliased Internal_Entity_Base_Type_Decl;
      --  The type of the substituted value.

   end record with
      Convention => C;
   No_Substitution : constant Internal_Substitution;

   procedure Inc_Ref (R : Internal_Substitution);
   procedure Dec_Ref (R : in out Internal_Substitution);

   function Equivalent (L, R : Internal_Substitution) return Boolean;

   function Trace_Image (R : Internal_Substitution) return String;

   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

   type Internal_Bare_Compilation_Unit_Array is
     array (Positive range <>) of Bare_Compilation_Unit;

   type Bare_Compilation_Unit_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Bare_Compilation_Unit_Array (1 .. N);
   end record;

   Empty_Bare_Compilation_Unit_Array_Record : aliased Bare_Compilation_Unit_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Compilation_Unit_Array_Type : constant Bare_Compilation_Unit_Array_Access :=
     Empty_Bare_Compilation_Unit_Array_Record'Access;

   function Create_Bare_Compilation_Unit_Array
     (Items_Count : Natural) return Bare_Compilation_Unit_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Compilation_Unit_Array
     (Items : Internal_Bare_Compilation_Unit_Array)
      return Bare_Compilation_Unit_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Bare_Compilation_Unit_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Bare_Compilation_Unit;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Bare_Compilation_Unit_Array_Access)
      return Bare_Compilation_Unit_Array_Access;

   function Length (T : Bare_Compilation_Unit_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Compilation_Unit_Array_Access);
   procedure Dec_Ref (T : in out Bare_Compilation_Unit_Array_Access);

   function Equivalent
     (L, R : Bare_Compilation_Unit_Array_Access) return Boolean;

   function Trace_Image (A : Bare_Compilation_Unit_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bare_Compilation_Unit_Array_Record, Bare_Compilation_Unit_Array_Access);

   type Internal_Bare_Name_Array is array (Positive range <>) of Bare_Name;

   type Bare_Name_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Bare_Name_Array (1 .. N);
   end record;

   Empty_Bare_Name_Array_Record : aliased Bare_Name_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Name_Array_Type : constant Bare_Name_Array_Access :=
     Empty_Bare_Name_Array_Record'Access;

   function Create_Bare_Name_Array
     (Items_Count : Natural) return Bare_Name_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Name_Array
     (Items : Internal_Bare_Name_Array) return Bare_Name_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T : Bare_Name_Array_Access; Index : Integer; Or_Null : Boolean := False)
      return Bare_Name;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Bare_Name_Array_Access) return Bare_Name_Array_Access;

   function Length (T : Bare_Name_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Name_Array_Access);
   procedure Dec_Ref (T : in out Bare_Name_Array_Access);

   function Equivalent (L, R : Bare_Name_Array_Access) return Boolean;

   function Trace_Image (A : Bare_Name_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bare_Name_Array_Record, Bare_Name_Array_Access);

   type Internal_Bare_Single_Tok_Node_Array is
     array (Positive range <>) of Bare_Single_Tok_Node;

   type Bare_Single_Tok_Node_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Bare_Single_Tok_Node_Array (1 .. N);
   end record;

   Empty_Bare_Single_Tok_Node_Array_Record : aliased Bare_Single_Tok_Node_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Single_Tok_Node_Array_Type : constant Bare_Single_Tok_Node_Array_Access :=
     Empty_Bare_Single_Tok_Node_Array_Record'Access;

   function Create_Bare_Single_Tok_Node_Array
     (Items_Count : Natural) return Bare_Single_Tok_Node_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Single_Tok_Node_Array
     (Items : Internal_Bare_Single_Tok_Node_Array)
      return Bare_Single_Tok_Node_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Bare_Single_Tok_Node_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Bare_Single_Tok_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Bare_Single_Tok_Node_Array_Access)
      return Bare_Single_Tok_Node_Array_Access;

   function Length (T : Bare_Single_Tok_Node_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Single_Tok_Node_Array_Access);
   procedure Dec_Ref (T : in out Bare_Single_Tok_Node_Array_Access);

   function Equivalent
     (L, R : Bare_Single_Tok_Node_Array_Access) return Boolean;

   function Trace_Image (A : Bare_Single_Tok_Node_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bare_Single_Tok_Node_Array_Record, Bare_Single_Tok_Node_Array_Access);

   type Character_Type_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Text_Type (1 .. N);
   end record;

   Empty_Text_Type_Record : aliased Character_Type_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Character_Type_Array_Type : constant Character_Type_Array_Access :=
     Empty_Text_Type_Record'Access;

   function Create_Character_Type_Array
     (Items_Count : Natural) return Character_Type_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Character_Type_Array
     (Items : Text_Type) return Character_Type_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Character_Type_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Character_Type;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Character_Type_Array_Access) return Character_Type_Array_Access;

   function Length (T : Character_Type_Array_Access) return Natural;

   procedure Inc_Ref (T : Character_Type_Array_Access);
   procedure Dec_Ref (T : in out Character_Type_Array_Access);

   function Equivalent (L, R : Character_Type_Array_Access) return Boolean;

   function Trace_Image (A : Character_Type_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Character_Type_Array_Record, Character_Type_Array_Access);

   type Internal_Character_Type_Array_Access_Array is
     array (Positive range <>) of Character_Type_Array_Access;

   type Character_Type_Array_Access_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Character_Type_Array_Access_Array (1 .. N);
   end record;

   Empty_Text_Type_Array_Record : aliased Character_Type_Array_Access_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Character_Type_Array_Access_Array_Type : constant Character_Type_Array_Access_Array_Access :=
     Empty_Text_Type_Array_Record'Access;

   function Create_Character_Type_Array_Access_Array
     (Items_Count : Natural) return Character_Type_Array_Access_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Character_Type_Array_Access_Array
     (Items : Internal_Character_Type_Array_Access_Array)
      return Character_Type_Array_Access_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Character_Type_Array_Access_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Character_Type_Array_Access;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Character_Type_Array_Access_Array_Access)
      return Character_Type_Array_Access_Array_Access;

   function Length
     (T : Character_Type_Array_Access_Array_Access) return Natural;

   procedure Inc_Ref (T : Character_Type_Array_Access_Array_Access);
   procedure Dec_Ref (T : in out Character_Type_Array_Access_Array_Access);

   function Equivalent
     (L, R : Character_Type_Array_Access_Array_Access) return Boolean;

   function Trace_Image
     (A : Character_Type_Array_Access_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Character_Type_Array_Access_Array_Record,
      Character_Type_Array_Access_Array_Access);

   type Internal_Internal_Completion_Item_Array is
     array (Positive range <>) of Internal_Completion_Item;

   type Internal_Completion_Item_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Completion_Item_Array (1 .. N);
   end record;

   Empty_Completion_Item_Array_Record : aliased Internal_Completion_Item_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Completion_Item_Array_Type : constant Internal_Completion_Item_Array_Access :=
     Empty_Completion_Item_Array_Record'Access;

   function Create_Internal_Completion_Item_Array
     (Items_Count : Natural) return Internal_Completion_Item_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Completion_Item_Array
     (Items : Internal_Internal_Completion_Item_Array)
      return Internal_Completion_Item_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Completion_Item_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Completion_Item;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Completion_Item_Array_Access)
      return Internal_Completion_Item_Array_Access;

   function Length (T : Internal_Completion_Item_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Completion_Item_Array_Access);
   procedure Dec_Ref (T : in out Internal_Completion_Item_Array_Access);

   function Equivalent
     (L, R : Internal_Completion_Item_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Completion_Item_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Completion_Item_Array_Record,
      Internal_Completion_Item_Array_Access);

   type Internal_Internal_Doc_Annotation_Array is
     array (Positive range <>) of Internal_Doc_Annotation;

   type Internal_Doc_Annotation_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Doc_Annotation_Array (1 .. N);
   end record;

   Empty_Doc_Annotation_Array_Record : aliased Internal_Doc_Annotation_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Doc_Annotation_Array_Type : constant Internal_Doc_Annotation_Array_Access :=
     Empty_Doc_Annotation_Array_Record'Access;

   function Create_Internal_Doc_Annotation_Array
     (Items_Count : Natural) return Internal_Doc_Annotation_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Doc_Annotation_Array
     (Items : Internal_Internal_Doc_Annotation_Array)
      return Internal_Doc_Annotation_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Doc_Annotation_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Doc_Annotation;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Doc_Annotation_Array_Access)
      return Internal_Doc_Annotation_Array_Access;

   function Length (T : Internal_Doc_Annotation_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Doc_Annotation_Array_Access);
   procedure Dec_Ref (T : in out Internal_Doc_Annotation_Array_Access);

   function Equivalent
     (L, R : Internal_Doc_Annotation_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Doc_Annotation_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Doc_Annotation_Array_Record,
      Internal_Doc_Annotation_Array_Access);

   type Internal_Internal_Entity_Aspect_Assoc_Array is
     array (Positive range <>) of Internal_Entity_Aspect_Assoc;

   type Internal_Entity_Aspect_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Aspect_Assoc_Array (1 .. N);
   end record;

   Empty_Aspect_Assoc_Array_Record : aliased Internal_Entity_Aspect_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Aspect_Assoc_Array_Type : constant Internal_Entity_Aspect_Assoc_Array_Access :=
     Empty_Aspect_Assoc_Array_Record'Access;

   function Create_Internal_Entity_Aspect_Assoc_Array
     (Items_Count : Natural) return Internal_Entity_Aspect_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Aspect_Assoc_Array
     (Items : Internal_Internal_Entity_Aspect_Assoc_Array)
      return Internal_Entity_Aspect_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Aspect_Assoc_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Aspect_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Aspect_Assoc_Array_Access)
      return Internal_Entity_Aspect_Assoc_Array_Access;

   function Length
     (T : Internal_Entity_Aspect_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Aspect_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Aspect_Assoc_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Aspect_Assoc_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Aspect_Assoc_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Aspect_Assoc_Array_Record,
      Internal_Entity_Aspect_Assoc_Array_Access);

   type Internal_Internal_Entity_Base_Formal_Param_Decl_Array is
     array (Positive range <>) of Internal_Entity_Base_Formal_Param_Decl;

   type Internal_Entity_Base_Formal_Param_Decl_Array_Record (N : Natural)
   is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Base_Formal_Param_Decl_Array (1 .. N);
   end record;

   Empty_Base_Formal_Param_Decl_Array_Record : aliased Internal_Entity_Base_Formal_Param_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Base_Formal_Param_Decl_Array_Type : constant Internal_Entity_Base_Formal_Param_Decl_Array_Access :=
     Empty_Base_Formal_Param_Decl_Array_Record'Access;

   function Create_Internal_Entity_Base_Formal_Param_Decl_Array
     (Items_Count : Natural)
      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Base_Formal_Param_Decl_Array
     (Items : Internal_Internal_Entity_Base_Formal_Param_Decl_Array)
      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T : Internal_Entity_Base_Formal_Param_Decl_Array_Access; Index : Integer;
      Or_Null : Boolean := False)
      return Internal_Entity_Base_Formal_Param_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Base_Formal_Param_Decl_Array_Access)
      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Length
     (T : Internal_Entity_Base_Formal_Param_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Base_Formal_Param_Decl_Array_Access);
   procedure Dec_Ref
     (T : in out Internal_Entity_Base_Formal_Param_Decl_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Base_Formal_Param_Decl_Array_Access)
      return Boolean;

   function Trace_Image
     (A : Internal_Entity_Base_Formal_Param_Decl_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Base_Formal_Param_Decl_Array_Record,
      Internal_Entity_Base_Formal_Param_Decl_Array_Access);

   type Internal_Internal_Entity_Base_Type_Decl_Array is
     array (Positive range <>) of Internal_Entity_Base_Type_Decl;

   type Internal_Entity_Base_Type_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Base_Type_Decl_Array (1 .. N);
   end record;

   Empty_Base_Type_Decl_Array_Record : aliased Internal_Entity_Base_Type_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Base_Type_Decl_Array_Type : constant Internal_Entity_Base_Type_Decl_Array_Access :=
     Empty_Base_Type_Decl_Array_Record'Access;

   function Create_Internal_Entity_Base_Type_Decl_Array
     (Items_Count : Natural)
      return Internal_Entity_Base_Type_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Base_Type_Decl_Array
     (Items : Internal_Internal_Entity_Base_Type_Decl_Array)
      return Internal_Entity_Base_Type_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Base_Type_Decl_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Base_Type_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Base_Type_Decl_Array_Access)
      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Length
     (T : Internal_Entity_Base_Type_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Base_Type_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Base_Type_Decl_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Base_Type_Decl_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Base_Type_Decl_Array_Access) return String;

   function Make_Unique
     (A : Internal_Entity_Base_Type_Decl_Array_Access)
      return Internal_Entity_Base_Type_Decl_Array_Access;
   --  Return a copy of A with duplicated elements removed

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Base_Type_Decl_Array_Record,
      Internal_Entity_Base_Type_Decl_Array_Access);

   type Internal_Internal_Entity_Basic_Assoc_Array is
     array (Positive range <>) of Internal_Entity_Basic_Assoc;

   type Internal_Entity_Basic_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Basic_Assoc_Array (1 .. N);
   end record;

   Empty_Basic_Assoc_Array_Record : aliased Internal_Entity_Basic_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Basic_Assoc_Array_Type : constant Internal_Entity_Basic_Assoc_Array_Access :=
     Empty_Basic_Assoc_Array_Record'Access;

   function Create_Internal_Entity_Basic_Assoc_Array
     (Items_Count : Natural) return Internal_Entity_Basic_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Basic_Assoc_Array
     (Items : Internal_Internal_Entity_Basic_Assoc_Array)
      return Internal_Entity_Basic_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Basic_Assoc_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Basic_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Basic_Assoc_Array_Access)
      return Internal_Entity_Basic_Assoc_Array_Access;

   function Length
     (T : Internal_Entity_Basic_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Basic_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Basic_Assoc_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Basic_Assoc_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Basic_Assoc_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Basic_Assoc_Array_Record,
      Internal_Entity_Basic_Assoc_Array_Access);

   type Internal_Internal_Entity_Basic_Decl_Array is
     array (Positive range <>) of Internal_Entity_Basic_Decl;

   type Internal_Entity_Basic_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Basic_Decl_Array (1 .. N);
   end record;

   Empty_Basic_Decl_Array_Record : aliased Internal_Entity_Basic_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Basic_Decl_Array_Type : constant Internal_Entity_Basic_Decl_Array_Access :=
     Empty_Basic_Decl_Array_Record'Access;

   function Create_Internal_Entity_Basic_Decl_Array
     (Items_Count : Natural) return Internal_Entity_Basic_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Basic_Decl_Array
     (Items : Internal_Internal_Entity_Basic_Decl_Array)
      return Internal_Entity_Basic_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Basic_Decl_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Basic_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Basic_Decl_Array_Access)
      return Internal_Entity_Basic_Decl_Array_Access;

   function Length
     (T : Internal_Entity_Basic_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Basic_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Basic_Decl_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Basic_Decl_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Basic_Decl_Array_Access) return String;

   function Make_Unique
     (A : Internal_Entity_Basic_Decl_Array_Access)
      return Internal_Entity_Basic_Decl_Array_Access;
   --  Return a copy of A with duplicated elements removed

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Basic_Decl_Array_Record,
      Internal_Entity_Basic_Decl_Array_Access);

   type Internal_Internal_Entity_Compilation_Unit_Array is
     array (Positive range <>) of Internal_Entity_Compilation_Unit;

   type Internal_Entity_Compilation_Unit_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Compilation_Unit_Array (1 .. N);
   end record;

   Empty_Compilation_Unit_Array_Record : aliased Internal_Entity_Compilation_Unit_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Compilation_Unit_Array_Type : constant Internal_Entity_Compilation_Unit_Array_Access :=
     Empty_Compilation_Unit_Array_Record'Access;

   function Create_Internal_Entity_Compilation_Unit_Array
     (Items_Count : Natural)
      return Internal_Entity_Compilation_Unit_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Compilation_Unit_Array
     (Items : Internal_Internal_Entity_Compilation_Unit_Array)
      return Internal_Entity_Compilation_Unit_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Compilation_Unit_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Compilation_Unit;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Compilation_Unit_Array_Access)
      return Internal_Entity_Compilation_Unit_Array_Access;

   function Length
     (T : Internal_Entity_Compilation_Unit_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Compilation_Unit_Array_Access);
   procedure Dec_Ref
     (T : in out Internal_Entity_Compilation_Unit_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Compilation_Unit_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Compilation_Unit_Array_Access) return String;

   function Make_Unique
     (A : Internal_Entity_Compilation_Unit_Array_Access)
      return Internal_Entity_Compilation_Unit_Array_Access;
   --  Return a copy of A with duplicated elements removed

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Compilation_Unit_Array_Record,
      Internal_Entity_Compilation_Unit_Array_Access);

   type Internal_Internal_Entity_Defining_Name_Array is
     array (Positive range <>) of Internal_Entity_Defining_Name;

   type Internal_Entity_Defining_Name_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Defining_Name_Array (1 .. N);
   end record;

   Empty_Defining_Name_Array_Record : aliased Internal_Entity_Defining_Name_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Defining_Name_Array_Type : constant Internal_Entity_Defining_Name_Array_Access :=
     Empty_Defining_Name_Array_Record'Access;

   function Create_Internal_Entity_Defining_Name_Array
     (Items_Count : Natural) return Internal_Entity_Defining_Name_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Defining_Name_Array
     (Items : Internal_Internal_Entity_Defining_Name_Array)
      return Internal_Entity_Defining_Name_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Defining_Name_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Defining_Name;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Defining_Name_Array_Access)
      return Internal_Entity_Defining_Name_Array_Access;

   function Length
     (T : Internal_Entity_Defining_Name_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Defining_Name_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Defining_Name_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Defining_Name_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Defining_Name_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Defining_Name_Array_Record,
      Internal_Entity_Defining_Name_Array_Access);

   type Internal_Internal_Entity_Generic_Instantiation_Array is
     array (Positive range <>) of Internal_Entity_Generic_Instantiation;

   type Internal_Entity_Generic_Instantiation_Array_Record (N : Natural)
   is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Generic_Instantiation_Array (1 .. N);
   end record;

   Empty_Generic_Instantiation_Array_Record : aliased Internal_Entity_Generic_Instantiation_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Generic_Instantiation_Array_Type : constant Internal_Entity_Generic_Instantiation_Array_Access :=
     Empty_Generic_Instantiation_Array_Record'Access;

   function Create_Internal_Entity_Generic_Instantiation_Array
     (Items_Count : Natural)
      return Internal_Entity_Generic_Instantiation_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Generic_Instantiation_Array
     (Items : Internal_Internal_Entity_Generic_Instantiation_Array)
      return Internal_Entity_Generic_Instantiation_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T : Internal_Entity_Generic_Instantiation_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Generic_Instantiation;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Generic_Instantiation_Array_Access)
      return Internal_Entity_Generic_Instantiation_Array_Access;

   function Length
     (T : Internal_Entity_Generic_Instantiation_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Generic_Instantiation_Array_Access);
   procedure Dec_Ref
     (T : in out Internal_Entity_Generic_Instantiation_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Generic_Instantiation_Array_Access)
      return Boolean;

   function Trace_Image
     (A : Internal_Entity_Generic_Instantiation_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Generic_Instantiation_Array_Record,
      Internal_Entity_Generic_Instantiation_Array_Access);

   type Internal_Internal_Entity_Param_Spec_Array is
     array (Positive range <>) of Internal_Entity_Param_Spec;

   type Internal_Entity_Param_Spec_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Param_Spec_Array (1 .. N);
   end record;

   Empty_Param_Spec_Array_Record : aliased Internal_Entity_Param_Spec_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Param_Spec_Array_Type : constant Internal_Entity_Param_Spec_Array_Access :=
     Empty_Param_Spec_Array_Record'Access;

   function Create_Internal_Entity_Param_Spec_Array
     (Items_Count : Natural) return Internal_Entity_Param_Spec_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Param_Spec_Array
     (Items : Internal_Internal_Entity_Param_Spec_Array)
      return Internal_Entity_Param_Spec_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Param_Spec_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Param_Spec;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Param_Spec_Array_Access)
      return Internal_Entity_Param_Spec_Array_Access;

   function Length
     (T : Internal_Entity_Param_Spec_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Param_Spec_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Param_Spec_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Param_Spec_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Param_Spec_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Param_Spec_Array_Record,
      Internal_Entity_Param_Spec_Array_Access);

   type Internal_Internal_Entity_Pragma_Node_Array is
     array (Positive range <>) of Internal_Entity_Pragma_Node;

   type Internal_Entity_Pragma_Node_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Pragma_Node_Array (1 .. N);
   end record;

   Empty_Pragma_Node_Array_Record : aliased Internal_Entity_Pragma_Node_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Pragma_Node_Array_Type : constant Internal_Entity_Pragma_Node_Array_Access :=
     Empty_Pragma_Node_Array_Record'Access;

   function Create_Internal_Entity_Pragma_Node_Array
     (Items_Count : Natural) return Internal_Entity_Pragma_Node_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Pragma_Node_Array
     (Items : Internal_Internal_Entity_Pragma_Node_Array)
      return Internal_Entity_Pragma_Node_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Pragma_Node_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Pragma_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Pragma_Node_Array_Access)
      return Internal_Entity_Pragma_Node_Array_Access;

   function Length
     (T : Internal_Entity_Pragma_Node_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Pragma_Node_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Pragma_Node_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Pragma_Node_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Pragma_Node_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Pragma_Node_Array_Record,
      Internal_Entity_Pragma_Node_Array_Access);

   type Internal_Internal_Entity_Type_Decl_Array is
     array (Positive range <>) of Internal_Entity_Type_Decl;

   type Internal_Entity_Type_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Type_Decl_Array (1 .. N);
   end record;

   Empty_Type_Decl_Array_Record : aliased Internal_Entity_Type_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Type_Decl_Array_Type : constant Internal_Entity_Type_Decl_Array_Access :=
     Empty_Type_Decl_Array_Record'Access;

   function Create_Internal_Entity_Type_Decl_Array
     (Items_Count : Natural) return Internal_Entity_Type_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Type_Decl_Array
     (Items : Internal_Internal_Entity_Type_Decl_Array)
      return Internal_Entity_Type_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Type_Decl_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Type_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Type_Decl_Array_Access)
      return Internal_Entity_Type_Decl_Array_Access;

   function Length (T : Internal_Entity_Type_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Type_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Type_Decl_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Type_Decl_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Type_Decl_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Type_Decl_Array_Record,
      Internal_Entity_Type_Decl_Array_Access);

   type Internal_Internal_Entity_Type_Expr_Array is
     array (Positive range <>) of Internal_Entity_Type_Expr;

   type Internal_Entity_Type_Expr_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Type_Expr_Array (1 .. N);
   end record;

   Empty_Type_Expr_Array_Record : aliased Internal_Entity_Type_Expr_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Type_Expr_Array_Type : constant Internal_Entity_Type_Expr_Array_Access :=
     Empty_Type_Expr_Array_Record'Access;

   function Create_Internal_Entity_Type_Expr_Array
     (Items_Count : Natural) return Internal_Entity_Type_Expr_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Type_Expr_Array
     (Items : Internal_Internal_Entity_Type_Expr_Array)
      return Internal_Entity_Type_Expr_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Type_Expr_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Type_Expr;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Type_Expr_Array_Access)
      return Internal_Entity_Type_Expr_Array_Access;

   function Length (T : Internal_Entity_Type_Expr_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Type_Expr_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Type_Expr_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Type_Expr_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Type_Expr_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Type_Expr_Array_Record,
      Internal_Entity_Type_Expr_Array_Access);

   type Internal_Internal_Entity_Variant_Array is
     array (Positive range <>) of Internal_Entity_Variant;

   type Internal_Entity_Variant_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Entity_Variant_Array (1 .. N);
   end record;

   Empty_Variant_Array_Record : aliased Internal_Entity_Variant_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Variant_Array_Type : constant Internal_Entity_Variant_Array_Access :=
     Empty_Variant_Array_Record'Access;

   function Create_Internal_Entity_Variant_Array
     (Items_Count : Natural) return Internal_Entity_Variant_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Variant_Array
     (Items : Internal_Internal_Entity_Variant_Array)
      return Internal_Entity_Variant_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Variant_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Variant;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Entity_Variant_Array_Access)
      return Internal_Entity_Variant_Array_Access;

   function Length (T : Internal_Entity_Variant_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Variant_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Variant_Array_Access);

   function Equivalent
     (L, R : Internal_Entity_Variant_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Entity_Variant_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Entity_Variant_Array_Record,
      Internal_Entity_Variant_Array_Access);

   type Internal_Internal_Env_Assoc_Array is
     array (Positive range <>) of Internal_Env_Assoc;

   type Internal_Env_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Env_Assoc_Array (1 .. N);
   end record;

   Empty_Env_Assoc_Array_Record : aliased Internal_Env_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Env_Assoc_Array_Type : constant Internal_Env_Assoc_Array_Access :=
     Empty_Env_Assoc_Array_Record'Access;

   function Create_Internal_Env_Assoc_Array
     (Items_Count : Natural) return Internal_Env_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Env_Assoc_Array
     (Items : Internal_Internal_Env_Assoc_Array)
      return Internal_Env_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Env_Assoc_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Env_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Env_Assoc_Array_Access)
      return Internal_Env_Assoc_Array_Access;

   function Length (T : Internal_Env_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Env_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Env_Assoc_Array_Access);

   function Equivalent (L, R : Internal_Env_Assoc_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Env_Assoc_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Env_Assoc_Array_Record, Internal_Env_Assoc_Array_Access);

   type Internal_Internal_Expected_Type_For_Expr_Array is
     array (Positive range <>) of Internal_Expected_Type_For_Expr;

   type Internal_Expected_Type_For_Expr_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Expected_Type_For_Expr_Array (1 .. N);
   end record;

   Empty_Expected_Type_For_Expr_Array_Record : aliased Internal_Expected_Type_For_Expr_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Expected_Type_For_Expr_Array_Type : constant Internal_Expected_Type_For_Expr_Array_Access :=
     Empty_Expected_Type_For_Expr_Array_Record'Access;

   function Create_Internal_Expected_Type_For_Expr_Array
     (Items_Count : Natural)
      return Internal_Expected_Type_For_Expr_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Expected_Type_For_Expr_Array
     (Items : Internal_Internal_Expected_Type_For_Expr_Array)
      return Internal_Expected_Type_For_Expr_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Expected_Type_For_Expr_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Expected_Type_For_Expr;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Expected_Type_For_Expr_Array_Access)
      return Internal_Expected_Type_For_Expr_Array_Access;

   function Length
     (T : Internal_Expected_Type_For_Expr_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Expected_Type_For_Expr_Array_Access);
   procedure Dec_Ref (T : in out Internal_Expected_Type_For_Expr_Array_Access);

   function Equivalent
     (L, R : Internal_Expected_Type_For_Expr_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Expected_Type_For_Expr_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Expected_Type_For_Expr_Array_Record,
      Internal_Expected_Type_For_Expr_Array_Access);

   type Internal_Internal_Param_Actual_Array is
     array (Positive range <>) of Internal_Param_Actual;

   type Internal_Param_Actual_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Param_Actual_Array (1 .. N);
   end record;

   Empty_Param_Actual_Array_Record : aliased Internal_Param_Actual_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Param_Actual_Array_Type : constant Internal_Param_Actual_Array_Access :=
     Empty_Param_Actual_Array_Record'Access;

   function Create_Internal_Param_Actual_Array
     (Items_Count : Natural) return Internal_Param_Actual_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Param_Actual_Array
     (Items : Internal_Internal_Param_Actual_Array)
      return Internal_Param_Actual_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Param_Actual_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Param_Actual;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Param_Actual_Array_Access)
      return Internal_Param_Actual_Array_Access;

   function Length (T : Internal_Param_Actual_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Param_Actual_Array_Access);
   procedure Dec_Ref (T : in out Internal_Param_Actual_Array_Access);

   function Equivalent
     (L, R : Internal_Param_Actual_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Param_Actual_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Param_Actual_Array_Record, Internal_Param_Actual_Array_Access);

   type Internal_Internal_Param_Match_Array is
     array (Positive range <>) of Internal_Param_Match;

   type Internal_Param_Match_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Param_Match_Array (1 .. N);
   end record;

   Empty_Param_Match_Array_Record : aliased Internal_Param_Match_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Param_Match_Array_Type : constant Internal_Param_Match_Array_Access :=
     Empty_Param_Match_Array_Record'Access;

   function Create_Internal_Param_Match_Array
     (Items_Count : Natural) return Internal_Param_Match_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Param_Match_Array
     (Items : Internal_Internal_Param_Match_Array)
      return Internal_Param_Match_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Param_Match_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Param_Match;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Param_Match_Array_Access)
      return Internal_Param_Match_Array_Access;

   function Length (T : Internal_Param_Match_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Param_Match_Array_Access);
   procedure Dec_Ref (T : in out Internal_Param_Match_Array_Access);

   function Equivalent
     (L, R : Internal_Param_Match_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Param_Match_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Param_Match_Array_Record, Internal_Param_Match_Array_Access);

   type Internal_Internal_Ref_Result_Array is
     array (Positive range <>) of Internal_Ref_Result;

   type Internal_Ref_Result_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Ref_Result_Array (1 .. N);
   end record;

   Empty_Ref_Result_Array_Record : aliased Internal_Ref_Result_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Ref_Result_Array_Type : constant Internal_Ref_Result_Array_Access :=
     Empty_Ref_Result_Array_Record'Access;

   function Create_Internal_Ref_Result_Array
     (Items_Count : Natural) return Internal_Ref_Result_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Ref_Result_Array
     (Items : Internal_Internal_Ref_Result_Array)
      return Internal_Ref_Result_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Ref_Result_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Ref_Result;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Ref_Result_Array_Access)
      return Internal_Ref_Result_Array_Access;

   function Length (T : Internal_Ref_Result_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Ref_Result_Array_Access);
   procedure Dec_Ref (T : in out Internal_Ref_Result_Array_Access);

   function Equivalent
     (L, R : Internal_Ref_Result_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Ref_Result_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Ref_Result_Array_Record, Internal_Ref_Result_Array_Access);

   type Internal_Internal_Single_Actual_Array is
     array (Positive range <>) of Internal_Single_Actual;

   type Internal_Single_Actual_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Single_Actual_Array (1 .. N);
   end record;

   Empty_Single_Actual_Array_Record : aliased Internal_Single_Actual_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Single_Actual_Array_Type : constant Internal_Single_Actual_Array_Access :=
     Empty_Single_Actual_Array_Record'Access;

   function Create_Internal_Single_Actual_Array
     (Items_Count : Natural) return Internal_Single_Actual_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Single_Actual_Array
     (Items : Internal_Internal_Single_Actual_Array)
      return Internal_Single_Actual_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Single_Actual_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Single_Actual;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Single_Actual_Array_Access)
      return Internal_Single_Actual_Array_Access;

   function Length (T : Internal_Single_Actual_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Single_Actual_Array_Access);
   procedure Dec_Ref (T : in out Internal_Single_Actual_Array_Access);

   function Equivalent
     (L, R : Internal_Single_Actual_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Single_Actual_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Single_Actual_Array_Record,
      Internal_Single_Actual_Array_Access);

   type Internal_Internal_Single_Formal_Array is
     array (Positive range <>) of Internal_Single_Formal;

   type Internal_Single_Formal_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Single_Formal_Array (1 .. N);
   end record;

   Empty_Single_Formal_Array_Record : aliased Internal_Single_Formal_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Single_Formal_Array_Type : constant Internal_Single_Formal_Array_Access :=
     Empty_Single_Formal_Array_Record'Access;

   function Create_Internal_Single_Formal_Array
     (Items_Count : Natural) return Internal_Single_Formal_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Single_Formal_Array
     (Items : Internal_Internal_Single_Formal_Array)
      return Internal_Single_Formal_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Single_Formal_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Single_Formal;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Single_Formal_Array_Access)
      return Internal_Single_Formal_Array_Access;

   function Length (T : Internal_Single_Formal_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Single_Formal_Array_Access);
   procedure Dec_Ref (T : in out Internal_Single_Formal_Array_Access);

   function Equivalent
     (L, R : Internal_Single_Formal_Array_Access) return Boolean;

   function Trace_Image
     (A : Internal_Single_Formal_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Single_Formal_Array_Record,
      Internal_Single_Formal_Array_Access);

   type Internal_Internal_Substitution_Array is
     array (Positive range <>) of Internal_Substitution;

   type Internal_Substitution_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Substitution_Array (1 .. N);
   end record;

   Empty_Substitution_Array_Record : aliased Internal_Substitution_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Substitution_Array_Type : constant Internal_Substitution_Array_Access :=
     Empty_Substitution_Array_Record'Access;

   function Create_Internal_Substitution_Array
     (Items_Count : Natural) return Internal_Substitution_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Substitution_Array
     (Items : Internal_Internal_Substitution_Array)
      return Internal_Substitution_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Substitution_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Substitution;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Substitution_Array_Access)
      return Internal_Substitution_Array_Access;

   function Length (T : Internal_Substitution_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Substitution_Array_Access);
   procedure Dec_Ref (T : in out Internal_Substitution_Array_Access);

   function Equivalent
     (L, R : Internal_Substitution_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Substitution_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Substitution_Array_Record, Internal_Substitution_Array_Access);

   type Internal_Internal_Unit_Array is
     array (Positive range <>) of Internal_Unit;

   type Internal_Unit_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Internal_Unit_Array (1 .. N);
   end record;

   Empty_Analysis_Unit_Array_Record : aliased Internal_Unit_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Unit_Array_Type : constant Internal_Unit_Array_Access :=
     Empty_Analysis_Unit_Array_Record'Access;

   function Create_Internal_Unit_Array
     (Items_Count : Natural) return Internal_Unit_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Unit_Array
     (Items : Internal_Internal_Unit_Array) return Internal_Unit_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Unit_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Internal_Unit;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Internal_Unit_Array_Access) return Internal_Unit_Array_Access;

   function Length (T : Internal_Unit_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Unit_Array_Access);
   procedure Dec_Ref (T : in out Internal_Unit_Array_Access);

   function Equivalent (L, R : Internal_Unit_Array_Access) return Boolean;

   function Trace_Image (A : Internal_Unit_Array_Access) return String;

   function Make_Unique
     (A : Internal_Unit_Array_Access) return Internal_Unit_Array_Access;
   --  Return a copy of A with duplicated elements removed

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Unit_Array_Record, Internal_Unit_Array_Access);

   type Internal_Logic_Equation_Array is
     array (Positive range <>) of Logic_Equation;

   type Logic_Equation_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Logic_Equation_Array (1 .. N);
   end record;

   Empty_Logic_Equation_Array_Record : aliased Logic_Equation_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Logic_Equation_Array_Type : constant Logic_Equation_Array_Access :=
     Empty_Logic_Equation_Array_Record'Access;

   function Create_Logic_Equation_Array
     (Items_Count : Natural) return Logic_Equation_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Logic_Equation_Array
     (Items : Internal_Logic_Equation_Array)
      return Logic_Equation_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Logic_Equation_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Logic_Equation;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Logic_Equation_Array_Access) return Logic_Equation_Array_Access;

   function Length (T : Logic_Equation_Array_Access) return Natural;

   procedure Inc_Ref (T : Logic_Equation_Array_Access);
   procedure Dec_Ref (T : in out Logic_Equation_Array_Access);

   function Equivalent (L, R : Logic_Equation_Array_Access) return Boolean;

   function Trace_Image (A : Logic_Equation_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Logic_Equation_Array_Record, Logic_Equation_Array_Access);

   type Internal_Symbol_Type_Array is array (Positive range <>) of Symbol_Type;

   type Symbol_Type_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items : Internal_Symbol_Type_Array (1 .. N);
   end record;

   Empty_Unbounded_Text_Type_Array_Record : aliased Symbol_Type_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Symbol_Type_Array_Type : constant Symbol_Type_Array_Access :=
     Empty_Unbounded_Text_Type_Array_Record'Access;

   function Create_Symbol_Type_Array
     (Items_Count : Natural) return Symbol_Type_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Symbol_Type_Array_Access; Index : Integer;
      Or_Null : Boolean := False) return Symbol_Type;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --  If the result is ref-counted, a new owning reference is returned.

   function Concat
     (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access;

   function Length (T : Symbol_Type_Array_Access) return Natural;

   procedure Inc_Ref (T : Symbol_Type_Array_Access);
   procedure Dec_Ref (T : in out Symbol_Type_Array_Access);

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean;

   function Trace_Image (A : Symbol_Type_Array_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Symbol_Type_Array_Record, Symbol_Type_Array_Access);

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type Root_Node_Record (Kind : Ada_Node_Kind_Type) is record
      Parent : Bare_Ada_Node;
      --  Reference to the parent node, or null if this is the root one

      Unit : Internal_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Self_Env : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Last_Attempted_Child : Integer;
      --  0-based index for the last child we tried to parse for this node. -1
      --  if parsing for all children was successful.

      case Kind is
         when Ada_Ada_List =>

            Count : Natural;
            Nodes : Alloc_AST_List_Array.Element_Array_Access;

            case Kind is
               when Ada_Contract_Case_Assoc_List_Range =>

                  null;

               when Ada_Basic_Assoc_List =>

                  case Kind is
                     when Ada_Assoc_List_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Ada_Node_List_Range =>

                  case Kind is
                     when Ada_Decl_List_Range =>

                        null;

                     when Ada_Stmt_List_Range =>

                        null;

                     when Ada_Alternatives_List_Range =>

                        null;

                     when Ada_Constraint_List_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Param_Spec_List_Range =>

                  null;

               when Ada_Name_List_Range =>

                  case Kind is
                     when Ada_Parent_List_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Identifier_List =>

                  case Kind is
                     when Ada_Discriminant_Choice_List_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Expr_List =>

                  case Kind is
                     when Ada_Expr_Alternatives_List_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Aspect_Assoc_List_Range =>

                  null;

               when Ada_Defining_Name_List_Range =>

                  null;

               when Ada_Elsif_Expr_Part_List_Range =>

                  null;

               when Ada_Select_When_Part_List_Range =>

                  null;

               when Ada_Unconstrained_Array_Index_List_Range =>

                  null;

               when Ada_Pragma_Node_List_Range =>

                  null;

               when Ada_Elsif_Stmt_Part_List_Range =>

                  null;

               when Ada_Discriminant_Spec_List_Range =>

                  null;

               when Ada_Case_Stmt_Alternative_List_Range =>

                  null;

               when Ada_Compilation_Unit_List_Range =>

                  null;

               when Ada_Variant_List_Range =>

                  null;

               when Ada_Base_Assoc_List_Range =>

                  null;

               when Ada_Case_Expr_Alternative_List_Range =>

                  null;

               when Ada_Enum_Literal_Decl_List_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Basic_Decl =>

            case Kind is
               when Ada_Error_Decl_Range =>

                  null;

               when Ada_Body_Node =>

                  case Kind is
                     when Ada_Body_Stub =>

                        case Kind is
                           when Ada_Protected_Body_Stub_Range =>

                              Protected_Body_Stub_F_Name : aliased Bare_Defining_Name :=
                                No_Bare_Ada_Node;
                              Protected_Body_Stub_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Subp_Body_Stub_Range =>

                              Subp_Body_Stub_F_Overriding : aliased Bare_Overriding_Node :=
                                No_Bare_Ada_Node;
                              Subp_Body_Stub_F_Subp_Spec : aliased Bare_Subp_Spec :=
                                No_Bare_Ada_Node;
                              Subp_Body_Stub_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Package_Body_Stub_Range =>

                              Package_Body_Stub_F_Name : aliased Bare_Defining_Name :=
                                No_Bare_Ada_Node;
                              Package_Body_Stub_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Task_Body_Stub_Range =>

                              Task_Body_Stub_F_Name : aliased Bare_Defining_Name :=
                                No_Bare_Ada_Node;
                              Task_Body_Stub_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when others =>
                              null;
                        end case;

                     when Ada_Base_Subp_Body =>

                        Base_Subp_Body_F_Overriding : aliased Bare_Overriding_Node :=
                          No_Bare_Ada_Node;
                        Base_Subp_Body_F_Subp_Spec : aliased Bare_Subp_Spec :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Expr_Function_Range =>

                              Expr_Function_F_Expr : aliased Bare_Expr :=
                                No_Bare_Ada_Node;
                              Expr_Function_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Null_Subp_Decl_Range =>

                              Null_Subp_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Subp_Renaming_Decl_Range =>

                              Subp_Renaming_Decl_F_Renames : aliased Bare_Renaming_Clause :=
                                No_Bare_Ada_Node;
                              Subp_Renaming_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Subp_Body_Range =>

                              Subp_Body_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;
                              Subp_Body_F_Decls : aliased Bare_Declarative_Part :=
                                No_Bare_Ada_Node;
                              Subp_Body_F_Stmts : aliased Bare_Handled_Stmts :=
                                No_Bare_Ada_Node;
                              Subp_Body_F_End_Name : aliased Bare_End_Name :=
                                No_Bare_Ada_Node;

                           when others =>
                              null;
                        end case;

                     when Ada_Package_Body_Range =>

                        Package_Body_F_Package_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Package_Body_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;
                        Package_Body_F_Decls : aliased Bare_Declarative_Part :=
                          No_Bare_Ada_Node;
                        Package_Body_F_Stmts : aliased Bare_Handled_Stmts :=
                          No_Bare_Ada_Node;
                        Package_Body_F_End_Name : aliased Bare_End_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Task_Body_Range =>

                        Task_Body_F_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Task_Body_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;
                        Task_Body_F_Decls : aliased Bare_Declarative_Part :=
                          No_Bare_Ada_Node;
                        Task_Body_F_Stmts : aliased Bare_Handled_Stmts :=
                          No_Bare_Ada_Node;
                        Task_Body_F_End_Name : aliased Bare_End_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Protected_Body_Range =>

                        Protected_Body_F_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Protected_Body_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;
                        Protected_Body_F_Decls : aliased Bare_Declarative_Part :=
                          No_Bare_Ada_Node;
                        Protected_Body_F_End_Name : aliased Bare_End_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Entry_Body_Range =>

                        Entry_Body_F_Entry_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Entry_Body_F_Index_Spec : aliased Bare_Entry_Index_Spec :=
                          No_Bare_Ada_Node;
                        Entry_Body_F_Params : aliased Bare_Entry_Completion_Formal_Params :=
                          No_Bare_Ada_Node;
                        Entry_Body_F_Barrier : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Entry_Body_F_Decls : aliased Bare_Declarative_Part :=
                          No_Bare_Ada_Node;
                        Entry_Body_F_Stmts : aliased Bare_Handled_Stmts :=
                          No_Bare_Ada_Node;
                        Entry_Body_F_End_Name : aliased Bare_End_Name :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Base_Formal_Param_Decl =>

                  case Kind is
                     when Ada_Discriminant_Spec_Range =>

                        Discriminant_Spec_F_Ids : aliased Bare_Defining_Name_List :=
                          No_Bare_Ada_Node;
                        Discriminant_Spec_F_Type_Expr : aliased Bare_Type_Expr :=
                          No_Bare_Ada_Node;
                        Discriminant_Spec_F_Default_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Component_Decl_Range =>

                        Component_Decl_F_Ids : aliased Bare_Defining_Name_List :=
                          No_Bare_Ada_Node;
                        Component_Decl_F_Component_Def : aliased Bare_Component_Def :=
                          No_Bare_Ada_Node;
                        Component_Decl_F_Default_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Component_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Param_Spec_Range =>

                        Param_Spec_F_Ids : aliased Bare_Defining_Name_List :=
                          No_Bare_Ada_Node;
                        Param_Spec_F_Has_Aliased : aliased Bare_Aliased_Node :=
                          No_Bare_Ada_Node;
                        Param_Spec_F_Mode : aliased Bare_Mode :=
                          No_Bare_Ada_Node;
                        Param_Spec_F_Type_Expr : aliased Bare_Type_Expr :=
                          No_Bare_Ada_Node;
                        Param_Spec_F_Default_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Generic_Formal =>

                        Generic_Formal_F_Decl : aliased Bare_Basic_Decl :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Generic_Formal_Obj_Decl_Range =>

                              null;

                           when Ada_Generic_Formal_Type_Decl_Range =>

                              null;

                           when Ada_Generic_Formal_Subp_Decl_Range =>

                              null;

                           when Ada_Generic_Formal_Package_Range =>

                              null;

                           when others =>
                              null;
                        end case;

                     when others =>
                        null;
                  end case;

               when Ada_Base_Type_Decl =>

                  Base_Type_Decl_F_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Classwide_Type_Decl_Range =>

                        null;

                     when Ada_Type_Decl_Range =>

                        Type_Decl_F_Discriminants : aliased Bare_Discriminant_Part :=
                          No_Bare_Ada_Node;
                        Type_Decl_F_Type_Def : aliased Bare_Type_Def :=
                          No_Bare_Ada_Node;
                        Type_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;
                        Type_Decl_Prims_Env : aliased Lexical_Env := Empty_Env;

                        case Kind is
                           when Ada_Anonymous_Type_Decl_Range =>

                              case Kind is
                                 when Ada_Synth_Anonymous_Type_Decl_Range =>

                                    null;

                                 when others =>
                                    null;
                              end case;

                           when others =>
                              null;
                        end case;

                     when Ada_Base_Subtype_Decl =>

                        case Kind is
                           when Ada_Subtype_Decl_Range =>

                              Subtype_Decl_F_Subtype : aliased Bare_Subtype_Indication :=
                                No_Bare_Ada_Node;
                              Subtype_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Discrete_Base_Subtype_Decl_Range =>

                              null;

                           when others =>
                              null;
                        end case;

                     when Ada_Task_Type_Decl_Range =>

                        Task_Type_Decl_F_Discriminants : aliased Bare_Discriminant_Part :=
                          No_Bare_Ada_Node;
                        Task_Type_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;
                        Task_Type_Decl_F_Definition : aliased Bare_Task_Def :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Single_Task_Type_Decl_Range =>

                              null;

                           when others =>
                              null;
                        end case;

                     when Ada_Protected_Type_Decl_Range =>

                        Protected_Type_Decl_F_Discriminants : aliased Bare_Discriminant_Part :=
                          No_Bare_Ada_Node;
                        Protected_Type_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;
                        Protected_Type_Decl_F_Interfaces : aliased Bare_Parent_List :=
                          No_Bare_Ada_Node;
                        Protected_Type_Decl_F_Definition : aliased Bare_Protected_Def :=
                          No_Bare_Ada_Node;

                     when Ada_Incomplete_Type_Decl_Range =>

                        Incomplete_Type_Decl_F_Discriminants : aliased Bare_Discriminant_Part :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Incomplete_Tagged_Type_Decl_Range =>

                              Incomplete_Tagged_Type_Decl_F_Has_Abstract : aliased Bare_Abstract_Node :=
                                No_Bare_Ada_Node;

                           when others =>
                              null;
                        end case;

                     when others =>
                        null;
                  end case;

               when Ada_Basic_Subp_Decl =>

                  case Kind is
                     when Ada_Classic_Subp_Decl =>

                        Classic_Subp_Decl_F_Overriding : aliased Bare_Overriding_Node :=
                          No_Bare_Ada_Node;
                        Classic_Subp_Decl_F_Subp_Spec : aliased Bare_Subp_Spec :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Subp_Decl_Range =>

                              Subp_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Abstract_Subp_Decl_Range =>

                              Abstract_Subp_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                           when Ada_Formal_Subp_Decl =>

                              Formal_Subp_Decl_F_Default_Expr : aliased Bare_Expr :=
                                No_Bare_Ada_Node;
                              Formal_Subp_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                                No_Bare_Ada_Node;

                              case Kind is
                                 when Ada_Concrete_Formal_Subp_Decl_Range =>

                                    null;

                                 when Ada_Abstract_Formal_Subp_Decl_Range =>

                                    null;

                                 when others =>
                                    null;
                              end case;

                           when others =>
                              null;
                        end case;

                     when Ada_Generic_Subp_Internal_Range =>

                        Generic_Subp_Internal_F_Subp_Spec : aliased Bare_Subp_Spec :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Internal_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Enum_Literal_Decl_Range =>

                        Enum_Literal_Decl_F_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Entry_Decl_Range =>

                        Entry_Decl_F_Overriding : aliased Bare_Overriding_Node :=
                          No_Bare_Ada_Node;
                        Entry_Decl_F_Spec : aliased Bare_Entry_Spec :=
                          No_Bare_Ada_Node;
                        Entry_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Single_Task_Decl_Range =>

                  Single_Task_Decl_F_Task_Type : aliased Bare_Single_Task_Type_Decl :=
                    No_Bare_Ada_Node;

               when Ada_Single_Protected_Decl_Range =>

                  Single_Protected_Decl_F_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;
                  Single_Protected_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                    No_Bare_Ada_Node;
                  Single_Protected_Decl_F_Interfaces : aliased Bare_Parent_List :=
                    No_Bare_Ada_Node;
                  Single_Protected_Decl_F_Definition : aliased Bare_Protected_Def :=
                    No_Bare_Ada_Node;

               when Ada_Number_Decl_Range =>

                  Number_Decl_F_Ids : aliased Bare_Defining_Name_List :=
                    No_Bare_Ada_Node;
                  Number_Decl_F_Expr : aliased Bare_Expr := No_Bare_Ada_Node;

               when Ada_Object_Decl_Range =>

                  Object_Decl_F_Ids : aliased Bare_Defining_Name_List :=
                    No_Bare_Ada_Node;
                  Object_Decl_F_Has_Aliased : aliased Bare_Aliased_Node :=
                    No_Bare_Ada_Node;
                  Object_Decl_F_Has_Constant : aliased Bare_Constant_Node :=
                    No_Bare_Ada_Node;
                  Object_Decl_F_Mode : aliased Bare_Mode := No_Bare_Ada_Node;
                  Object_Decl_F_Type_Expr : aliased Bare_Type_Expr :=
                    No_Bare_Ada_Node;
                  Object_Decl_F_Default_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;
                  Object_Decl_F_Renaming_Clause : aliased Bare_Renaming_Clause :=
                    No_Bare_Ada_Node;
                  Object_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Anonymous_Object_Decl_Range =>

                        null;

                     when Ada_Extended_Return_Stmt_Object_Decl_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Base_Package_Decl =>

                  Base_Package_Decl_F_Package_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;
                  Base_Package_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                    No_Bare_Ada_Node;
                  Base_Package_Decl_F_Public_Part : aliased Bare_Public_Part :=
                    No_Bare_Ada_Node;
                  Base_Package_Decl_F_Private_Part : aliased Bare_Private_Part :=
                    No_Bare_Ada_Node;
                  Base_Package_Decl_F_End_Name : aliased Bare_End_Name :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Package_Decl_Range =>

                        null;

                     when Ada_Generic_Package_Internal_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Exception_Decl_Range =>

                  Exception_Decl_F_Ids : aliased Bare_Defining_Name_List :=
                    No_Bare_Ada_Node;
                  Exception_Decl_F_Renames : aliased Bare_Renaming_Clause :=
                    No_Bare_Ada_Node;
                  Exception_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                    No_Bare_Ada_Node;

               when Ada_Generic_Instantiation =>

                  Generic_Instantiation_Inst_Env : aliased Lexical_Env :=
                    Empty_Env;

                  case Kind is
                     when Ada_Generic_Subp_Instantiation_Range =>

                        Generic_Subp_Instantiation_F_Overriding : aliased Bare_Overriding_Node :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Instantiation_F_Kind : aliased Bare_Subp_Kind :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Instantiation_F_Subp_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Instantiation_F_Generic_Subp_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Instantiation_F_Params : aliased Bare_Assoc_List :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Instantiation_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Generic_Package_Instantiation_Range =>

                        Generic_Package_Instantiation_F_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Generic_Package_Instantiation_F_Generic_Pkg_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Generic_Package_Instantiation_F_Params : aliased Bare_Assoc_List :=
                          No_Bare_Ada_Node;
                        Generic_Package_Instantiation_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Package_Renaming_Decl_Range =>

                  Package_Renaming_Decl_F_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;
                  Package_Renaming_Decl_F_Renames : aliased Bare_Renaming_Clause :=
                    No_Bare_Ada_Node;
                  Package_Renaming_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                    No_Bare_Ada_Node;

               when Ada_Generic_Renaming_Decl =>

                  case Kind is
                     when Ada_Generic_Package_Renaming_Decl_Range =>

                        Generic_Package_Renaming_Decl_F_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Generic_Package_Renaming_Decl_F_Renames : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Generic_Package_Renaming_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Generic_Subp_Renaming_Decl_Range =>

                        Generic_Subp_Renaming_Decl_F_Kind : aliased Bare_Subp_Kind :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Renaming_Decl_F_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Renaming_Decl_F_Renames : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Generic_Subp_Renaming_Decl_F_Aspects : aliased Bare_Aspect_Spec :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Generic_Decl =>

                  Generic_Decl_F_Formal_Part : aliased Bare_Generic_Formal_Part :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Generic_Subp_Decl_Range =>

                        Generic_Subp_Decl_F_Subp_Decl : aliased Bare_Generic_Subp_Internal :=
                          No_Bare_Ada_Node;

                     when Ada_Generic_Package_Decl_Range =>

                        Generic_Package_Decl_F_Package_Decl : aliased Bare_Generic_Package_Internal :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_For_Loop_Var_Decl_Range =>

                  For_Loop_Var_Decl_F_Id : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;
                  For_Loop_Var_Decl_F_Id_Type : aliased Bare_Subtype_Indication :=
                    No_Bare_Ada_Node;

               when Ada_Exception_Handler_Range =>

                  Exception_Handler_F_Exception_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;
                  Exception_Handler_F_Handled_Exceptions : aliased Bare_Alternatives_List :=
                    No_Bare_Ada_Node;
                  Exception_Handler_F_Stmts : aliased Bare_Stmt_List :=
                    No_Bare_Ada_Node;

               when Ada_Label_Decl_Range =>

                  Label_Decl_F_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;

               when Ada_Named_Stmt_Decl_Range =>

                  Named_Stmt_Decl_F_Name : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;

               when Ada_Entry_Index_Spec_Range =>

                  Entry_Index_Spec_F_Id : aliased Bare_Defining_Name :=
                    No_Bare_Ada_Node;
                  Entry_Index_Spec_F_Subtype : aliased Bare_Ada_Node :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Base_Formal_Param_Holder =>

            case Kind is
               when Ada_Discriminant_Part =>

                  case Kind is
                     when Ada_Known_Discriminant_Part_Range =>

                        Known_Discriminant_Part_F_Discr_Specs : aliased Bare_Discriminant_Spec_List :=
                          No_Bare_Ada_Node;

                     when Ada_Unknown_Discriminant_Part_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Component_List_Range =>

                  Component_List_F_Components : aliased Bare_Ada_Node_List :=
                    No_Bare_Ada_Node;
                  Component_List_F_Variant_Part : aliased Bare_Variant_Part :=
                    No_Bare_Ada_Node;

               when Ada_Generic_Formal_Part_Range =>

                  Generic_Formal_Part_F_Decls : aliased Bare_Ada_Node_List :=
                    No_Bare_Ada_Node;

               when Ada_Base_Subp_Spec =>

                  case Kind is
                     when Ada_Enum_Subp_Spec_Range =>

                        null;

                     when Ada_Subp_Spec_Range =>

                        Subp_Spec_F_Subp_Kind : aliased Bare_Subp_Kind :=
                          No_Bare_Ada_Node;
                        Subp_Spec_F_Subp_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Subp_Spec_F_Subp_Params : aliased Bare_Params :=
                          No_Bare_Ada_Node;
                        Subp_Spec_F_Subp_Returns : aliased Bare_Type_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Entry_Spec_Range =>

                        Entry_Spec_F_Entry_Name : aliased Bare_Defining_Name :=
                          No_Bare_Ada_Node;
                        Entry_Spec_F_Family_Type : aliased Bare_Ada_Node :=
                          No_Bare_Ada_Node;
                        Entry_Spec_F_Entry_Params : aliased Bare_Params :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Entry_Completion_Formal_Params_Range =>

                  Entry_Completion_Formal_Params_F_Params : aliased Bare_Params :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Type_Def =>

            case Kind is
               when Ada_Record_Type_Def_Range =>

                  Record_Type_Def_F_Has_Abstract : aliased Bare_Abstract_Node :=
                    No_Bare_Ada_Node;
                  Record_Type_Def_F_Has_Tagged : aliased Bare_Tagged_Node :=
                    No_Bare_Ada_Node;
                  Record_Type_Def_F_Has_Limited : aliased Bare_Limited_Node :=
                    No_Bare_Ada_Node;
                  Record_Type_Def_F_Record_Def : aliased Bare_Base_Record_Def :=
                    No_Bare_Ada_Node;

               when Ada_Real_Type_Def =>

                  case Kind is
                     when Ada_Floating_Point_Def_Range =>

                        Floating_Point_Def_F_Num_Digits : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Floating_Point_Def_F_Range : aliased Bare_Range_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Ordinary_Fixed_Point_Def_Range =>

                        Ordinary_Fixed_Point_Def_F_Delta : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Ordinary_Fixed_Point_Def_F_Range : aliased Bare_Range_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Decimal_Fixed_Point_Def_Range =>

                        Decimal_Fixed_Point_Def_F_Delta : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Decimal_Fixed_Point_Def_F_Digits : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Decimal_Fixed_Point_Def_F_Range : aliased Bare_Range_Spec :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Enum_Type_Def_Range =>

                  Enum_Type_Def_F_Enum_Literals : aliased Bare_Enum_Literal_Decl_List :=
                    No_Bare_Ada_Node;

               when Ada_Derived_Type_Def_Range =>

                  Derived_Type_Def_F_Has_Abstract : aliased Bare_Abstract_Node :=
                    No_Bare_Ada_Node;
                  Derived_Type_Def_F_Has_Limited : aliased Bare_Limited_Node :=
                    No_Bare_Ada_Node;
                  Derived_Type_Def_F_Has_Synchronized : aliased Bare_Synchronized_Node :=
                    No_Bare_Ada_Node;
                  Derived_Type_Def_F_Subtype_Indication : aliased Bare_Subtype_Indication :=
                    No_Bare_Ada_Node;
                  Derived_Type_Def_F_Interfaces : aliased Bare_Parent_List :=
                    No_Bare_Ada_Node;
                  Derived_Type_Def_F_Record_Extension : aliased Bare_Base_Record_Def :=
                    No_Bare_Ada_Node;
                  Derived_Type_Def_F_Has_With_Private : aliased Bare_With_Private :=
                    No_Bare_Ada_Node;

               when Ada_Private_Type_Def_Range =>

                  Private_Type_Def_F_Has_Abstract : aliased Bare_Abstract_Node :=
                    No_Bare_Ada_Node;
                  Private_Type_Def_F_Has_Tagged : aliased Bare_Tagged_Node :=
                    No_Bare_Ada_Node;
                  Private_Type_Def_F_Has_Limited : aliased Bare_Limited_Node :=
                    No_Bare_Ada_Node;

               when Ada_Signed_Int_Type_Def_Range =>

                  Signed_Int_Type_Def_F_Range : aliased Bare_Range_Spec :=
                    No_Bare_Ada_Node;

               when Ada_Mod_Int_Type_Def_Range =>

                  Mod_Int_Type_Def_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when Ada_Array_Type_Def_Range =>

                  Array_Type_Def_F_Indices : aliased Bare_Array_Indices :=
                    No_Bare_Ada_Node;
                  Array_Type_Def_F_Component_Type : aliased Bare_Component_Def :=
                    No_Bare_Ada_Node;

               when Ada_Interface_Type_Def_Range =>

                  Interface_Type_Def_F_Interface_Kind : aliased Bare_Interface_Kind :=
                    No_Bare_Ada_Node;
                  Interface_Type_Def_F_Interfaces : aliased Bare_Parent_List :=
                    No_Bare_Ada_Node;

               when Ada_Access_Def =>

                  Access_Def_F_Has_Not_Null : aliased Bare_Not_Null :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Access_To_Subp_Def_Range =>

                        Access_To_Subp_Def_F_Has_Protected : aliased Bare_Protected_Node :=
                          No_Bare_Ada_Node;
                        Access_To_Subp_Def_F_Subp_Spec : aliased Bare_Subp_Spec :=
                          No_Bare_Ada_Node;

                     when Ada_Base_Type_Access_Def =>

                        case Kind is
                           when Ada_Type_Access_Def_Range =>

                              Type_Access_Def_F_Has_All : aliased Bare_All_Node :=
                                No_Bare_Ada_Node;
                              Type_Access_Def_F_Has_Constant : aliased Bare_Constant_Node :=
                                No_Bare_Ada_Node;
                              Type_Access_Def_F_Subtype_Indication : aliased Bare_Subtype_Indication :=
                                No_Bare_Ada_Node;

                           when Ada_Anonymous_Type_Access_Def_Range =>

                              Anonymous_Type_Access_Def_F_Type_Decl : aliased Bare_Base_Type_Decl :=
                                No_Bare_Ada_Node;

                           when others =>
                              null;
                        end case;

                     when others =>
                        null;
                  end case;

               when Ada_Formal_Discrete_Type_Def_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Variant_Range =>

            Variant_F_Choices : aliased Bare_Alternatives_List :=
              No_Bare_Ada_Node;
            Variant_F_Components : aliased Bare_Component_List :=
              No_Bare_Ada_Node;

         when Ada_Variant_Part_Range =>

            Variant_Part_F_Discr_Name : aliased Bare_Identifier :=
              No_Bare_Ada_Node;
            Variant_Part_F_Variant : aliased Bare_Variant_List :=
              No_Bare_Ada_Node;

         when Ada_Base_Record_Def =>

            Base_Record_Def_F_Components : aliased Bare_Component_List :=
              No_Bare_Ada_Node;

            case Kind is
               when Ada_Record_Def_Range =>

                  null;

               when Ada_Null_Record_Def_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Tagged_Node =>

            case Kind is
               when Ada_Tagged_Present_Range =>

                  null;

               when Ada_Tagged_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Abstract_Node =>

            case Kind is
               when Ada_Abstract_Present_Range =>

                  null;

               when Ada_Abstract_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Limited_Node =>

            case Kind is
               when Ada_Limited_Present_Range =>

                  null;

               when Ada_Limited_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Private_Node =>

            case Kind is
               when Ada_Private_Present_Range =>

                  null;

               when Ada_Private_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Aliased_Node =>

            case Kind is
               when Ada_Aliased_Present_Range =>

                  null;

               when Ada_Aliased_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Not_Null =>

            case Kind is
               when Ada_Not_Null_Present_Range =>

                  null;

               when Ada_Not_Null_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Constant_Node =>

            case Kind is
               when Ada_Constant_Present_Range =>

                  null;

               when Ada_Constant_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_All_Node =>

            case Kind is
               when Ada_All_Present_Range =>

                  null;

               when Ada_All_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Abort_Node =>

            case Kind is
               when Ada_Abort_Present_Range =>

                  null;

               when Ada_Abort_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Reverse_Node =>

            case Kind is
               when Ada_Reverse_Present_Range =>

                  null;

               when Ada_Reverse_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_With_Private =>

            case Kind is
               when Ada_With_Private_Present_Range =>

                  null;

               when Ada_With_Private_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Until_Node =>

            case Kind is
               when Ada_Until_Present_Range =>

                  null;

               when Ada_Until_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Synchronized_Node =>

            case Kind is
               when Ada_Synchronized_Present_Range =>

                  null;

               when Ada_Synchronized_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Protected_Node =>

            case Kind is
               when Ada_Protected_Present_Range =>

                  null;

               when Ada_Protected_Absent_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Prim_Type_Accessor_Range =>

            Prim_Type_Accessor_Prim_Type : aliased Internal_Entity_Base_Type_Decl :=
              No_Entity_Base_Type_Decl;

         when Ada_Base_Assoc =>

            case Kind is
               when Ada_Pragma_Argument_Assoc_Range =>

                  Pragma_Argument_Assoc_F_Id : aliased Bare_Identifier :=
                    No_Bare_Ada_Node;
                  Pragma_Argument_Assoc_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when Ada_Contract_Case_Assoc_Range =>

                  Contract_Case_Assoc_F_Guard : aliased Bare_Ada_Node :=
                    No_Bare_Ada_Node;
                  Contract_Case_Assoc_F_Consequence : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Constraint =>

            case Kind is
               when Ada_Range_Constraint_Range =>

                  Range_Constraint_F_Range : aliased Bare_Range_Spec :=
                    No_Bare_Ada_Node;

               when Ada_Digits_Constraint_Range =>

                  Digits_Constraint_F_Digits : aliased Bare_Expr :=
                    No_Bare_Ada_Node;
                  Digits_Constraint_F_Range : aliased Bare_Range_Spec :=
                    No_Bare_Ada_Node;

               when Ada_Delta_Constraint_Range =>

                  Delta_Constraint_F_Digits : aliased Bare_Expr :=
                    No_Bare_Ada_Node;
                  Delta_Constraint_F_Range : aliased Bare_Range_Spec :=
                    No_Bare_Ada_Node;

               when Ada_Index_Constraint_Range =>

                  Index_Constraint_F_Constraints : aliased Bare_Constraint_List :=
                    No_Bare_Ada_Node;

               when Ada_Discriminant_Constraint_Range =>

                  Discriminant_Constraint_F_Constraints : aliased Bare_Assoc_List :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Basic_Assoc =>

            case Kind is
               when Ada_Discriminant_Assoc_Range =>

                  Discriminant_Assoc_F_Ids : aliased Bare_Discriminant_Choice_List :=
                    No_Bare_Ada_Node;
                  Discriminant_Assoc_F_Discr_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when Ada_Param_Assoc_Range =>

                  Param_Assoc_F_Designator : aliased Bare_Ada_Node :=
                    No_Bare_Ada_Node;
                  Param_Assoc_F_R_Expr : aliased Bare_Expr := No_Bare_Ada_Node;

               when Ada_Aggregate_Assoc_Range =>

                  Aggregate_Assoc_F_Designators : aliased Bare_Alternatives_List :=
                    No_Bare_Ada_Node;
                  Aggregate_Assoc_F_R_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Multi_Dim_Array_Assoc_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;

         when Ada_Array_Indices =>

            case Kind is
               when Ada_Unconstrained_Array_Indices_Range =>

                  Unconstrained_Array_Indices_F_Types : aliased Bare_Unconstrained_Array_Index_List :=
                    No_Bare_Ada_Node;

               when Ada_Constrained_Array_Indices_Range =>

                  Constrained_Array_Indices_F_List : aliased Bare_Constraint_List :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Component_Def_Range =>

            Component_Def_F_Has_Aliased : aliased Bare_Aliased_Node :=
              No_Bare_Ada_Node;
            Component_Def_F_Has_Constant : aliased Bare_Constant_Node :=
              No_Bare_Ada_Node;
            Component_Def_F_Type_Expr : aliased Bare_Type_Expr :=
              No_Bare_Ada_Node;

         when Ada_Interface_Kind =>

            case Kind is
               when Ada_Interface_Kind_Limited_Range =>

                  null;

               when Ada_Interface_Kind_Task_Range =>

                  null;

               when Ada_Interface_Kind_Protected_Range =>

                  null;

               when Ada_Interface_Kind_Synchronized_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Task_Def_Range =>

            Task_Def_F_Interfaces : aliased Bare_Parent_List :=
              No_Bare_Ada_Node;
            Task_Def_F_Public_Part : aliased Bare_Public_Part :=
              No_Bare_Ada_Node;
            Task_Def_F_Private_Part : aliased Bare_Private_Part :=
              No_Bare_Ada_Node;
            Task_Def_F_End_Name : aliased Bare_End_Name := No_Bare_Ada_Node;

         when Ada_Protected_Def_Range =>

            Protected_Def_F_Public_Part : aliased Bare_Public_Part :=
              No_Bare_Ada_Node;
            Protected_Def_F_Private_Part : aliased Bare_Private_Part :=
              No_Bare_Ada_Node;
            Protected_Def_F_End_Name : aliased Bare_End_Name :=
              No_Bare_Ada_Node;

         when Ada_Null_Component_Decl_Range =>

            null;

         when Ada_With_Clause_Range =>

            With_Clause_F_Has_Limited : aliased Bare_Limited_Node :=
              No_Bare_Ada_Node;
            With_Clause_F_Has_Private : aliased Bare_Private_Node :=
              No_Bare_Ada_Node;
            With_Clause_F_Packages : aliased Bare_Name_List :=
              No_Bare_Ada_Node;

         when Ada_Use_Clause =>

            case Kind is
               when Ada_Use_Package_Clause_Range =>

                  Use_Package_Clause_F_Packages : aliased Bare_Name_List :=
                    No_Bare_Ada_Node;

               when Ada_Use_Type_Clause_Range =>

                  Use_Type_Clause_F_Has_All : aliased Bare_All_Node :=
                    No_Bare_Ada_Node;
                  Use_Type_Clause_F_Types : aliased Bare_Name_List :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Type_Expr =>

            case Kind is
               when Ada_Enum_Lit_Synth_Type_Expr_Range =>

                  null;

               when Ada_Anonymous_Type_Range =>

                  Anonymous_Type_F_Type_Decl : aliased Bare_Anonymous_Type_Decl :=
                    No_Bare_Ada_Node;

               when Ada_Subtype_Indication_Range =>

                  Subtype_Indication_F_Has_Not_Null : aliased Bare_Not_Null :=
                    No_Bare_Ada_Node;
                  Subtype_Indication_F_Name : aliased Bare_Name :=
                    No_Bare_Ada_Node;
                  Subtype_Indication_F_Constraint : aliased Bare_Constraint :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Constrained_Subtype_Indication_Range =>

                        null;

                     when Ada_Discrete_Subtype_Indication_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;

         when Ada_Mode =>

            case Kind is
               when Ada_Mode_In_Range =>

                  null;

               when Ada_Mode_Out_Range =>

                  null;

               when Ada_Mode_In_Out_Range =>

                  null;

               when Ada_Mode_Default_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Aspect_Spec_Range =>

            Aspect_Spec_F_Aspect_Assocs : aliased Bare_Aspect_Assoc_List :=
              No_Bare_Ada_Node;

         when Ada_Overriding_Node =>

            case Kind is
               when Ada_Overriding_Overriding_Range =>

                  null;

               when Ada_Overriding_Not_Overriding_Range =>

                  null;

               when Ada_Overriding_Unspecified_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Pragma_Node_Range =>

            Pragma_Node_F_Id   : aliased Bare_Identifier := No_Bare_Ada_Node;
            Pragma_Node_F_Args : aliased Bare_Base_Assoc_List :=
              No_Bare_Ada_Node;

         when Ada_Aspect_Clause =>

            case Kind is
               when Ada_Enum_Rep_Clause_Range =>

                  Enum_Rep_Clause_F_Type_Name : aliased Bare_Name :=
                    No_Bare_Ada_Node;
                  Enum_Rep_Clause_F_Aggregate : aliased Bare_Base_Aggregate :=
                    No_Bare_Ada_Node;

               when Ada_Attribute_Def_Clause_Range =>

                  Attribute_Def_Clause_F_Attribute_Expr : aliased Bare_Name :=
                    No_Bare_Ada_Node;
                  Attribute_Def_Clause_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when Ada_Record_Rep_Clause_Range =>

                  Record_Rep_Clause_F_Name : aliased Bare_Name :=
                    No_Bare_Ada_Node;
                  Record_Rep_Clause_F_At_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;
                  Record_Rep_Clause_F_Components : aliased Bare_Ada_Node_List :=
                    No_Bare_Ada_Node;

               when Ada_At_Clause_Range =>

                  At_Clause_F_Name : aliased Bare_Base_Id := No_Bare_Ada_Node;
                  At_Clause_F_Expr : aliased Bare_Expr    := No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Component_Clause_Range =>

            Component_Clause_F_Id : aliased Bare_Identifier :=
              No_Bare_Ada_Node;
            Component_Clause_F_Position : aliased Bare_Expr :=
              No_Bare_Ada_Node;
            Component_Clause_F_Range : aliased Bare_Range_Spec :=
              No_Bare_Ada_Node;

         when Ada_Aspect_Assoc_Range =>

            Aspect_Assoc_F_Id   : aliased Bare_Name := No_Bare_Ada_Node;
            Aspect_Assoc_F_Expr : aliased Bare_Expr := No_Bare_Ada_Node;

         when Ada_Declarative_Part_Range =>

            Declarative_Part_F_Decls : aliased Bare_Ada_Node_List :=
              No_Bare_Ada_Node;

            case Kind is
               when Ada_Private_Part_Range =>

                  null;

               when Ada_Public_Part_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Renaming_Clause_Range =>

            Renaming_Clause_F_Renamed_Object : aliased Bare_Name :=
              No_Bare_Ada_Node;

            case Kind is
               when Ada_Synthetic_Renaming_Clause_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Subp_Kind =>

            case Kind is
               when Ada_Subp_Kind_Procedure_Range =>

                  null;

               when Ada_Subp_Kind_Function_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Expr =>

            Expr_Type_Var : aliased Logic_Var_Record := Null_Var_Record;

            case Kind is
               when Ada_Contract_Cases_Range =>

                  Contract_Cases_F_Contract_Cases : aliased Bare_Contract_Case_Assoc_List :=
                    No_Bare_Ada_Node;

               when Ada_Paren_Expr_Range =>

                  Paren_Expr_F_Expr : aliased Bare_Expr := No_Bare_Ada_Node;

               when Ada_Un_Op_Range =>

                  Un_Op_F_Op   : aliased Bare_Op   := No_Bare_Ada_Node;
                  Un_Op_F_Expr : aliased Bare_Expr := No_Bare_Ada_Node;

               when Ada_Bin_Op_Range =>

                  Bin_Op_F_Left  : aliased Bare_Expr := No_Bare_Ada_Node;
                  Bin_Op_F_Op    : aliased Bare_Op   := No_Bare_Ada_Node;
                  Bin_Op_F_Right : aliased Bare_Expr := No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Relation_Op_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Membership_Expr_Range =>

                  Membership_Expr_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;
                  Membership_Expr_F_Op : aliased Bare_Op := No_Bare_Ada_Node;
                  Membership_Expr_F_Membership_Exprs : aliased Bare_Expr_Alternatives_List :=
                    No_Bare_Ada_Node;

               when Ada_Base_Aggregate =>

                  Base_Aggregate_F_Ancestor_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;
                  Base_Aggregate_F_Assocs : aliased Bare_Assoc_List :=
                    No_Bare_Ada_Node;

                  case Kind is
                     when Ada_Aggregate_Range =>

                        null;

                     when Ada_Null_Record_Aggregate_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Name =>

                  case Kind is
                     when Ada_Discrete_Subtype_Name_Range =>

                        Discrete_Subtype_Name_F_Subtype : aliased Bare_Discrete_Subtype_Indication :=
                          No_Bare_Ada_Node;

                     when Ada_Target_Name_Range =>

                        null;

                     when Ada_Call_Expr_Range =>

                        Call_Expr_F_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Call_Expr_F_Suffix : aliased Bare_Ada_Node :=
                          No_Bare_Ada_Node;
                        Call_Expr_R_Called_Spec : aliased Logic_Var_Record :=
                          Null_Var_Record;

                     when Ada_Explicit_Deref_Range =>

                        Explicit_Deref_F_Prefix : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Explicit_Deref_R_Called_Spec : aliased Logic_Var_Record :=
                          Null_Var_Record;

                     when Ada_Single_Tok_Node =>

                        Single_Tok_Node_R_Ref_Var : aliased Logic_Var_Record :=
                          Null_Var_Record;
                        Single_Tok_Node_R_Called_Spec : aliased Logic_Var_Record :=
                          Null_Var_Record;

                        case Kind is
                           when Ada_Base_Id =>

                              case Kind is
                                 when Ada_Op =>

                                    case Kind is
                                       when Ada_Op_And_Range =>

                                          null;

                                       when Ada_Op_Or_Range =>

                                          null;

                                       when Ada_Op_Or_Else_Range =>

                                          null;

                                       when Ada_Op_And_Then_Range =>

                                          null;

                                       when Ada_Op_Xor_Range =>

                                          null;

                                       when Ada_Op_In_Range =>

                                          null;

                                       when Ada_Op_Not_In_Range =>

                                          null;

                                       when Ada_Op_Abs_Range =>

                                          null;

                                       when Ada_Op_Not_Range =>

                                          null;

                                       when Ada_Op_Pow_Range =>

                                          null;

                                       when Ada_Op_Mult_Range =>

                                          null;

                                       when Ada_Op_Div_Range =>

                                          null;

                                       when Ada_Op_Mod_Range =>

                                          null;

                                       when Ada_Op_Rem_Range =>

                                          null;

                                       when Ada_Op_Plus_Range =>

                                          null;

                                       when Ada_Op_Minus_Range =>

                                          null;

                                       when Ada_Op_Concat_Range =>

                                          null;

                                       when Ada_Op_Eq_Range =>

                                          null;

                                       when Ada_Op_Neq_Range =>

                                          null;

                                       when Ada_Op_Lt_Range =>

                                          null;

                                       when Ada_Op_Lte_Range =>

                                          null;

                                       when Ada_Op_Gt_Range =>

                                          null;

                                       when Ada_Op_Gte_Range =>

                                          null;

                                       when Ada_Op_Double_Dot_Range =>

                                          null;

                                       when others =>
                                          null;
                                    end case;

                                 when Ada_Identifier_Range =>

                                    null;

                                 when Ada_String_Literal_Range =>

                                    null;

                                 when Ada_Char_Literal_Range =>

                                    null;

                                 when others =>
                                    null;
                              end case;

                           when Ada_Num_Literal =>

                              case Kind is
                                 when Ada_Real_Literal_Range =>

                                    null;

                                 when Ada_Int_Literal_Range =>

                                    null;

                                 when others =>
                                    null;
                              end case;

                           when Ada_Null_Literal_Range =>

                              null;

                           when others =>
                              null;
                        end case;

                     when Ada_Defining_Name_Range =>

                        Defining_Name_F_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;

                     when Ada_End_Name_Range =>

                        End_Name_F_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Qual_Expr_Range =>

                        Qual_Expr_F_Prefix : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Qual_Expr_F_Suffix : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Attribute_Ref_Range =>

                        Attribute_Ref_F_Prefix : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Attribute_Ref_F_Attribute : aliased Bare_Identifier :=
                          No_Bare_Ada_Node;
                        Attribute_Ref_F_Args : aliased Bare_Ada_Node :=
                          No_Bare_Ada_Node;
                        Attribute_Ref_R_Ref_Var : aliased Logic_Var_Record :=
                          Null_Var_Record;

                        case Kind is
                           when Ada_Update_Attribute_Ref_Range =>

                              null;

                           when others =>
                              null;
                        end case;

                     when Ada_Dotted_Name_Range =>

                        Dotted_Name_F_Prefix : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Dotted_Name_F_Suffix : aliased Bare_Base_Id :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when Ada_Box_Expr_Range =>

                  null;

               when Ada_If_Expr_Range =>

                  If_Expr_F_Cond_Expr : aliased Bare_Expr := No_Bare_Ada_Node;
                  If_Expr_F_Then_Expr : aliased Bare_Expr := No_Bare_Ada_Node;
                  If_Expr_F_Alternatives : aliased Bare_Elsif_Expr_Part_List :=
                    No_Bare_Ada_Node;
                  If_Expr_F_Else_Expr : aliased Bare_Expr := No_Bare_Ada_Node;

               when Ada_Case_Expr_Range =>

                  Case_Expr_F_Expr  : aliased Bare_Expr := No_Bare_Ada_Node;
                  Case_Expr_F_Cases : aliased Bare_Case_Expr_Alternative_List :=
                    No_Bare_Ada_Node;

               when Ada_Case_Expr_Alternative_Range =>

                  Case_Expr_Alternative_F_Choices : aliased Bare_Alternatives_List :=
                    No_Bare_Ada_Node;
                  Case_Expr_Alternative_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when Ada_Quantified_Expr_Range =>

                  Quantified_Expr_F_Quantifier : aliased Bare_Quantifier :=
                    No_Bare_Ada_Node;
                  Quantified_Expr_F_Loop_Spec : aliased Bare_For_Loop_Spec :=
                    No_Bare_Ada_Node;
                  Quantified_Expr_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when Ada_Allocator_Range =>

                  Allocator_F_Subpool : aliased Bare_Name := No_Bare_Ada_Node;
                  Allocator_F_Type_Or_Expr : aliased Bare_Ada_Node :=
                    No_Bare_Ada_Node;

               when Ada_Raise_Expr_Range =>

                  Raise_Expr_F_Exception_Name : aliased Bare_Name :=
                    No_Bare_Ada_Node;
                  Raise_Expr_F_Error_Message : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Others_Designator_Range =>

            null;

         when Ada_Elsif_Expr_Part_Range =>

            Elsif_Expr_Part_F_Cond_Expr : aliased Bare_Expr :=
              No_Bare_Ada_Node;
            Elsif_Expr_Part_F_Then_Expr : aliased Bare_Expr :=
              No_Bare_Ada_Node;

         when Ada_Quantifier =>

            case Kind is
               when Ada_Quantifier_All_Range =>

                  null;

               when Ada_Quantifier_Some_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Iter_Type =>

            case Kind is
               when Ada_Iter_Type_In_Range =>

                  null;

               when Ada_Iter_Type_Of_Range =>

                  null;

               when others =>
                  null;
            end case;

         when Ada_Loop_Spec =>

            case Kind is
               when Ada_For_Loop_Spec_Range =>

                  For_Loop_Spec_F_Var_Decl : aliased Bare_For_Loop_Var_Decl :=
                    No_Bare_Ada_Node;
                  For_Loop_Spec_F_Loop_Type : aliased Bare_Iter_Type :=
                    No_Bare_Ada_Node;
                  For_Loop_Spec_F_Has_Reverse : aliased Bare_Reverse_Node :=
                    No_Bare_Ada_Node;
                  For_Loop_Spec_F_Iter_Expr : aliased Bare_Ada_Node :=
                    No_Bare_Ada_Node;

               when Ada_While_Loop_Spec_Range =>

                  While_Loop_Spec_F_Expr : aliased Bare_Expr :=
                    No_Bare_Ada_Node;

               when others =>
                  null;
            end case;

         when Ada_Compilation_Unit_Range =>

            Compilation_Unit_F_Prelude : aliased Bare_Ada_Node_List :=
              No_Bare_Ada_Node;
            Compilation_Unit_F_Body : aliased Bare_Ada_Node :=
              No_Bare_Ada_Node;
            Compilation_Unit_F_Pragmas : aliased Bare_Pragma_Node_List :=
              No_Bare_Ada_Node;
            Compilation_Unit_No_Env : aliased Lexical_Env := Empty_Env;

         when Ada_Handled_Stmts_Range =>

            Handled_Stmts_F_Stmts : aliased Bare_Stmt_List := No_Bare_Ada_Node;
            Handled_Stmts_F_Exceptions : aliased Bare_Ada_Node_List :=
              No_Bare_Ada_Node;

         when Ada_Stmt =>

            case Kind is
               when Ada_Error_Stmt_Range =>

                  null;

               when Ada_Simple_Stmt =>

                  case Kind is
                     when Ada_Call_Stmt_Range =>

                        Call_Stmt_F_Call : aliased Bare_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Null_Stmt_Range =>

                        null;

                     when Ada_Assign_Stmt_Range =>

                        Assign_Stmt_F_Dest : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Assign_Stmt_F_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Goto_Stmt_Range =>

                        Goto_Stmt_F_Label_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;

                     when Ada_Exit_Stmt_Range =>

                        Exit_Stmt_F_Loop_Name : aliased Bare_Identifier :=
                          No_Bare_Ada_Node;
                        Exit_Stmt_F_Cond_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Return_Stmt_Range =>

                        Return_Stmt_F_Return_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Requeue_Stmt_Range =>

                        Requeue_Stmt_F_Call_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Requeue_Stmt_F_Has_Abort : aliased Bare_Abort_Node :=
                          No_Bare_Ada_Node;

                     when Ada_Abort_Stmt_Range =>

                        Abort_Stmt_F_Names : aliased Bare_Name_List :=
                          No_Bare_Ada_Node;

                     when Ada_Delay_Stmt_Range =>

                        Delay_Stmt_F_Has_Until : aliased Bare_Until_Node :=
                          No_Bare_Ada_Node;
                        Delay_Stmt_F_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Raise_Stmt_Range =>

                        Raise_Stmt_F_Exception_Name : aliased Bare_Name :=
                          No_Bare_Ada_Node;
                        Raise_Stmt_F_Error_Message : aliased Bare_Expr :=
                          No_Bare_Ada_Node;

                     when Ada_Label_Range =>

                        Label_F_Decl : aliased Bare_Label_Decl :=
                          No_Bare_Ada_Node;

                     when Ada_Terminate_Alternative_Range =>

                        null;

                     when others =>
                        null;
                  end case;

               when Ada_Composite_Stmt =>

                  case Kind is
                     when Ada_If_Stmt_Range =>

                        If_Stmt_F_Cond_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        If_Stmt_F_Then_Stmts : aliased Bare_Stmt_List :=
                          No_Bare_Ada_Node;
                        If_Stmt_F_Alternatives : aliased Bare_Elsif_Stmt_Part_List :=
                          No_Bare_Ada_Node;
                        If_Stmt_F_Else_Stmts : aliased Bare_Stmt_List :=
                          No_Bare_Ada_Node;

                     when Ada_Named_Stmt_Range =>

                        Named_Stmt_F_Decl : aliased Bare_Named_Stmt_Decl :=
                          No_Bare_Ada_Node;
                        Named_Stmt_F_Stmt : aliased Bare_Composite_Stmt :=
                          No_Bare_Ada_Node;

                     when Ada_Base_Loop_Stmt =>

                        Base_Loop_Stmt_F_Spec : aliased Bare_Loop_Spec :=
                          No_Bare_Ada_Node;
                        Base_Loop_Stmt_F_Stmts : aliased Bare_Stmt_List :=
                          No_Bare_Ada_Node;
                        Base_Loop_Stmt_F_End_Name : aliased Bare_End_Name :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Loop_Stmt_Range =>

                              null;

                           when Ada_For_Loop_Stmt_Range =>

                              null;

                           when Ada_While_Loop_Stmt_Range =>

                              null;

                           when others =>
                              null;
                        end case;

                     when Ada_Block_Stmt =>

                        case Kind is
                           when Ada_Decl_Block_Range =>

                              Decl_Block_F_Decls : aliased Bare_Declarative_Part :=
                                No_Bare_Ada_Node;
                              Decl_Block_F_Stmts : aliased Bare_Handled_Stmts :=
                                No_Bare_Ada_Node;
                              Decl_Block_F_End_Name : aliased Bare_End_Name :=
                                No_Bare_Ada_Node;

                           when Ada_Begin_Block_Range =>

                              Begin_Block_F_Stmts : aliased Bare_Handled_Stmts :=
                                No_Bare_Ada_Node;
                              Begin_Block_F_End_Name : aliased Bare_End_Name :=
                                No_Bare_Ada_Node;

                           when others =>
                              null;
                        end case;

                     when Ada_Extended_Return_Stmt_Range =>

                        Extended_Return_Stmt_F_Decl : aliased Bare_Extended_Return_Stmt_Object_Decl :=
                          No_Bare_Ada_Node;
                        Extended_Return_Stmt_F_Stmts : aliased Bare_Handled_Stmts :=
                          No_Bare_Ada_Node;

                     when Ada_Case_Stmt_Range =>

                        Case_Stmt_F_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Case_Stmt_F_Alternatives : aliased Bare_Case_Stmt_Alternative_List :=
                          No_Bare_Ada_Node;

                     when Ada_Accept_Stmt_Range =>

                        Accept_Stmt_F_Name : aliased Bare_Identifier :=
                          No_Bare_Ada_Node;
                        Accept_Stmt_F_Entry_Index_Expr : aliased Bare_Expr :=
                          No_Bare_Ada_Node;
                        Accept_Stmt_F_Params : aliased Bare_Entry_Completion_Formal_Params :=
                          No_Bare_Ada_Node;

                        case Kind is
                           when Ada_Accept_Stmt_With_Stmts_Range =>

                              Accept_Stmt_With_Stmts_F_Stmts : aliased Bare_Handled_Stmts :=
                                No_Bare_Ada_Node;
                              Accept_Stmt_With_Stmts_F_End_Name : aliased Bare_End_Name :=
                                No_Bare_Ada_Node;

                           when others =>
                              null;
                        end case;

                     when Ada_Select_Stmt_Range =>

                        Select_Stmt_F_Guards : aliased Bare_Select_When_Part_List :=
                          No_Bare_Ada_Node;
                        Select_Stmt_F_Else_Stmts : aliased Bare_Stmt_List :=
                          No_Bare_Ada_Node;
                        Select_Stmt_F_Abort_Stmts : aliased Bare_Stmt_List :=
                          No_Bare_Ada_Node;

                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;

         when Ada_Elsif_Stmt_Part_Range =>

            Elsif_Stmt_Part_F_Cond_Expr : aliased Bare_Expr :=
              No_Bare_Ada_Node;
            Elsif_Stmt_Part_F_Stmts : aliased Bare_Stmt_List :=
              No_Bare_Ada_Node;

         when Ada_Case_Stmt_Alternative_Range =>

            Case_Stmt_Alternative_F_Choices : aliased Bare_Alternatives_List :=
              No_Bare_Ada_Node;
            Case_Stmt_Alternative_F_Stmts : aliased Bare_Stmt_List :=
              No_Bare_Ada_Node;

         when Ada_Select_When_Part_Range =>

            Select_When_Part_F_Cond_Expr : aliased Bare_Expr :=
              No_Bare_Ada_Node;
            Select_When_Part_F_Stmts : aliased Bare_Stmt_List :=
              No_Bare_Ada_Node;

         when Ada_Subunit_Range =>

            Subunit_F_Name : aliased Bare_Name      := No_Bare_Ada_Node;
            Subunit_F_Body : aliased Bare_Body_Node := No_Bare_Ada_Node;

         when Ada_Library_Item_Range =>

            Library_Item_F_Has_Private : aliased Bare_Private_Node :=
              No_Bare_Ada_Node;
            Library_Item_F_Item : aliased Bare_Basic_Decl := No_Bare_Ada_Node;

         when Ada_Range_Spec_Range =>

            Range_Spec_F_Range : aliased Bare_Expr := No_Bare_Ada_Node;

         when Ada_Params_Range =>

            Params_F_Params : aliased Bare_Param_Spec_List := No_Bare_Ada_Node;

         when Ada_Unconstrained_Array_Index_Range =>

            Unconstrained_Array_Index_F_Subtype_Indication : aliased Bare_Subtype_Indication :=
              No_Bare_Ada_Node;

         when others =>
            null;
      end case;

   end record;

   procedure Initialize
     (Self : Bare_Ada_Node; Kind : Ada_Node_Kind_Type; Unit : Internal_Unit;
      Token_Start_Index : Token_Index; Token_End_Index : Token_Index;
      Parent            : Bare_Ada_Node := null;
      Self_Env          : Lexical_Env   := AST_Envs.Empty_Env);
   --  Helper for parsers, to initialize a freshly allocated node

   function Pre_Env_Actions
     (Self            : Bare_Ada_Node; Bound_Env, Root_Env : Lexical_Env;
      Add_To_Env_Only : Boolean := False) return Lexical_Env;
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not
   --  by the user.
   --
   --  The return value is the initial environment to be passed to
   --  Post_Env_Actions.

   procedure Post_Env_Actions
     (Self : Bare_Ada_Node; Bound_Env, Root_Env : Lexical_Env);
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   function Get_Symbol (Node : Bare_Ada_Node) return Symbol_Type with
      Pre => Node = null or else Is_Token_Node (Node);
      --  Assuming Node is a token node, return the corresponding symbol for
      --  the token it contains.

   function Image (Self : Symbol_Type) return Character_Type_Array_Access;
   --  Transform a Symbol into an internal String

   function Text (Node : Bare_Ada_Node) return Text_Type;
   --  Retun the fragment of text from which Node was parsed

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   -----------------------
   -- Generic list type --
   -----------------------

   function Length (Node : Bare_Ada_List) return Natural;

   function Children (Node : Bare_Ada_Node) return Bare_Ada_Node_Array_Access;
   --  Return an array containing all the children of Node. This is an
   --  alternative to the Child/Children_Count pair, useful if you want
   --  the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Item
     (Node : Bare_Ada_List; Index : Positive) return Bare_Ada_Node renames
     Child;

   function Get
     (Node : Bare_Ada_List; Index : Integer; Or_Null : Boolean := False)
      return Bare_Ada_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.

   procedure Reset_Logic_Vars (Node : Bare_Ada_Node);
   --  Reset the logic variables attached to this node

   procedure Set_Parents (Node, Parent : Bare_Ada_Node);
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : Bare_Ada_Node);
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   function Get (A : AST_Envs.Entity_Array; Index : Integer) return Entity;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   function Group
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env;
   --  Convenience wrapper for uniform types handling in code generation

   package Bare_Ada_Node_Vectors is new Langkit_Support.Vectors
     (Bare_Ada_Node);

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that Referenced_Env belongs to is visible from
   --  the unit that Base_Env belongs to. If at least one of these two lexical
   --  environments does not belong to a particular analysis unit, this raises
   --  a Property_Error.

   function Populate_Lexical_Env (Node : Bare_Ada_Node) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Token
     (Node : Bare_Ada_Node; Index : Token_Index) return Token_Reference;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Reference values.

   function Stored_Token
     (Node : Bare_Ada_Node; Token : Token_Reference) return Token_Index;
   --  Helper for properties. This is used to turn a Token_Reference value into
   --  a Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   type Bare_Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Bare_Ada_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an node or a token

   type Bare_Children_Array is array (Positive range <>) of Bare_Child_Record;

   function Children_With_Trivia
     (Node : Bare_Ada_Node) return Bare_Children_Array;
   --  Implementation for Analysis.Children_With_Trivia

   function Dispatcher_Abort_Node_P_As_Bool

     (Node : Bare_Abort_Node)
return Boolean;
--  Return whether this is an instance of AbortPresent

   function Abort_Absent_P_As_Bool
(Node : Bare_Abort_Absent)
return Boolean;

   function Abort_Present_P_As_Bool
(Node : Bare_Abort_Present)
return Boolean;

   function Dispatcher_Abstract_Node_P_As_Bool

     (Node : Bare_Abstract_Node)
return Boolean;
--  Return whether this is an instance of AbstractPresent

   function Abstract_Absent_P_As_Bool

     (Node : Bare_Abstract_Absent)
return Boolean;

   function Abstract_Present_P_As_Bool

     (Node : Bare_Abstract_Present)
return Boolean;

   function Assoc_List_P_Unpacked_Params

     (Node : Bare_Assoc_List; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Single_Actual_Array_Access;
--  Given the list of ParamAssoc, that can in certain case designate several
--  actual parameters at once, create an unpacked list of SingleActual
--  instances.

   function Assoc_List_P_Zip_With_Params

     (Node   : Bare_Assoc_List; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Param_Actual_Array_Access;
--  Returns an array of pairs, associating formal parameters to actual
--  expressions. The formals to match are retrieved by resolving the call
--  which this AssocList represents the actuals of.

   function Dispatcher_Aliased_Node_P_As_Bool

     (Node : Bare_Aliased_Node)
return Boolean;
--  Return whether this is an instance of AliasedPresent

   function Aliased_Absent_P_As_Bool

     (Node : Bare_Aliased_Absent)
return Boolean;

   function Aliased_Present_P_As_Bool

     (Node : Bare_Aliased_Present)
return Boolean;

   function Dispatcher_All_Node_P_As_Bool

     (Node : Bare_All_Node)
return Boolean;
--  Return whether this is an instance of AllPresent

   function All_Absent_P_As_Bool
(Node : Bare_All_Absent)
return Boolean;

   function All_Present_P_As_Bool
(Node : Bare_All_Present)
return Boolean;

   function Dispatcher_Array_Indices_P_Ndims

     (Node : Bare_Array_Indices)
return Integer;
--  Number of dimensions described in this node.

   function Dispatcher_Array_Indices_P_Constrain_Index_Expr

     (Node   : Bare_Array_Indices; Index_Expr : Internal_Entity_Expr;
      Dim    : Integer; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Add a constraint on an expression passed as the index of an array access
--  expression.
--
--  For example::
--
--  type A is array (Integer range 1 .. 10) of Integer;
--
--  A_Inst : A;
--
--  A_Inst (2); -- ^ Will add constraint on lit that it needs to be of type --
--  Integer.

   function Dispatcher_Array_Indices_P_Index_Type

     (Node   : Bare_Array_Indices; Dim : Integer; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   procedure Initialize_Fields_For_Constrained_Array_Indices
     (Self                             : Bare_Constrained_Array_Indices;
      Constrained_Array_Indices_F_List : Bare_Constraint_List);

   function Constrained_Array_Indices_F_List
     (Node : Bare_Constrained_Array_Indices) return Bare_Constraint_List;

   function Constrained_Array_Indices_P_Ndims

     (Node : Bare_Constrained_Array_Indices)
return Integer;

   function Constrained_Array_Indices_P_Constrain_Index_Expr

     (Node : Bare_Constrained_Array_Indices; Index_Expr : Internal_Entity_Expr;
      Dim    : Integer; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Constrained_Array_Indices_P_Xref_Equation

     (Node   : Bare_Constrained_Array_Indices; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Constrained_Array_Indices_P_Index_Type

     (Node   : Bare_Constrained_Array_Indices; Dim : Integer;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   procedure Initialize_Fields_For_Unconstrained_Array_Indices
     (Self                                : Bare_Unconstrained_Array_Indices;
      Unconstrained_Array_Indices_F_Types : Bare_Unconstrained_Array_Index_List);

   function Unconstrained_Array_Indices_F_Types
     (Node : Bare_Unconstrained_Array_Indices)
      return Bare_Unconstrained_Array_Index_List;

   function Unconstrained_Array_Indices_P_Ndims

     (Node : Bare_Unconstrained_Array_Indices)
return Integer;

   function Unconstrained_Array_Indices_P_Constrain_Index_Expr

     (Node       : Bare_Unconstrained_Array_Indices;
      Index_Expr : Internal_Entity_Expr; Dim : Integer; Origin : Bare_Ada_Node;
      E_Info     : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Unconstrained_Array_Indices_P_Index_Type

     (Node   : Bare_Unconstrained_Array_Indices; Dim : Integer;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Unconstrained_Array_Indices_P_Xref_Equation

     (Node   : Bare_Unconstrained_Array_Indices; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   procedure Initialize_Fields_For_Aspect_Assoc
     (Self                : Bare_Aspect_Assoc; Aspect_Assoc_F_Id : Bare_Name;
      Aspect_Assoc_F_Expr : Bare_Expr);

   function Aspect_Assoc_F_Id (Node : Bare_Aspect_Assoc) return Bare_Name;

   function Aspect_Assoc_F_Expr (Node : Bare_Aspect_Assoc) return Bare_Expr;

   function Aspect_Assoc_P_Xref_Entry_Point

     (Node : Bare_Aspect_Assoc)
return Boolean;

   function Aspect_Assoc_P_Xref_Equation

     (Node   : Bare_Aspect_Assoc; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Aspect_Assoc_P_Aspect_Name

     (Node : Bare_Aspect_Assoc; N : Internal_Entity_Name)

      return Character_Type_Array_Access;
--  Return the string representation of the given name, which must be a Name
--  that can appear in an aspect association id.

   function Aspect_Clause_P_Xref_Entry_Point

     (Node : Bare_Aspect_Clause)
return Boolean;

   function Aspect_Clause_P_Xref_Equation

     (Node   : Bare_Aspect_Clause; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_At_Clause
     (Self             : Bare_At_Clause; At_Clause_F_Name : Bare_Base_Id;
      At_Clause_F_Expr : Bare_Expr);

   function At_Clause_F_Name (Node : Bare_At_Clause) return Bare_Base_Id;

   function At_Clause_F_Expr (Node : Bare_At_Clause) return Bare_Expr;

   procedure Initialize_Fields_For_Attribute_Def_Clause
     (Self                                  : Bare_Attribute_Def_Clause;
      Attribute_Def_Clause_F_Attribute_Expr : Bare_Name;
      Attribute_Def_Clause_F_Expr           : Bare_Expr);

   function Attribute_Def_Clause_F_Attribute_Expr
     (Node : Bare_Attribute_Def_Clause) return Bare_Name;

   function Attribute_Def_Clause_F_Expr
     (Node : Bare_Attribute_Def_Clause) return Bare_Expr;

   function Attribute_Def_Clause_P_Xref_Entry_Point

     (Node : Bare_Attribute_Def_Clause)
return Boolean;

   function Attribute_Def_Clause_P_Xref_Equation

     (Node   : Bare_Attribute_Def_Clause; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   procedure Initialize_Fields_For_Enum_Rep_Clause
     (Self : Bare_Enum_Rep_Clause; Enum_Rep_Clause_F_Type_Name : Bare_Name;
      Enum_Rep_Clause_F_Aggregate : Bare_Base_Aggregate);

   function Enum_Rep_Clause_F_Type_Name
     (Node : Bare_Enum_Rep_Clause) return Bare_Name;

   function Enum_Rep_Clause_F_Aggregate
     (Node : Bare_Enum_Rep_Clause) return Bare_Base_Aggregate;

   function Enum_Rep_Clause_P_Xref_Equation

     (Node   : Bare_Enum_Rep_Clause; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Record_Rep_Clause
     (Self : Bare_Record_Rep_Clause; Record_Rep_Clause_F_Name : Bare_Name;
      Record_Rep_Clause_F_At_Expr    : Bare_Expr;
      Record_Rep_Clause_F_Components : Bare_Ada_Node_List);

   function Record_Rep_Clause_F_Name
     (Node : Bare_Record_Rep_Clause) return Bare_Name;

   function Record_Rep_Clause_F_At_Expr
     (Node : Bare_Record_Rep_Clause) return Bare_Expr;

   function Record_Rep_Clause_F_Components
     (Node : Bare_Record_Rep_Clause) return Bare_Ada_Node_List;

   function Record_Rep_Clause_P_Xref_Equation

     (Node : Bare_Record_Rep_Clause; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Aspect_Spec
     (Self                        : Bare_Aspect_Spec;
      Aspect_Spec_F_Aspect_Assocs : Bare_Aspect_Assoc_List);

   function Aspect_Spec_F_Aspect_Assocs
     (Node : Bare_Aspect_Spec) return Bare_Aspect_Assoc_List;

   function Dispatcher_Base_Assoc_P_Assoc_Expr

     (Node : Bare_Base_Assoc; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;
--  Returns the expression side of this assoc node.

   procedure Initialize_Fields_For_Contract_Case_Assoc
     (Self                              : Bare_Contract_Case_Assoc;
      Contract_Case_Assoc_F_Guard       : Bare_Ada_Node;
      Contract_Case_Assoc_F_Consequence : Bare_Expr);

   function Contract_Case_Assoc_F_Guard
     (Node : Bare_Contract_Case_Assoc) return Bare_Ada_Node;

   function Contract_Case_Assoc_F_Consequence
     (Node : Bare_Contract_Case_Assoc) return Bare_Expr;

   function Contract_Case_Assoc_P_Assoc_Expr

     (Node   : Bare_Contract_Case_Assoc;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   procedure Initialize_Fields_For_Pragma_Argument_Assoc
     (Self                         : Bare_Pragma_Argument_Assoc;
      Pragma_Argument_Assoc_F_Id   : Bare_Identifier;
      Pragma_Argument_Assoc_F_Expr : Bare_Expr);

   function Pragma_Argument_Assoc_F_Id
     (Node : Bare_Pragma_Argument_Assoc) return Bare_Identifier;

   function Pragma_Argument_Assoc_F_Expr
     (Node : Bare_Pragma_Argument_Assoc) return Bare_Expr;

   function Pragma_Argument_Assoc_P_Assoc_Expr

     (Node   : Bare_Pragma_Argument_Assoc;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   function Dispatcher_Base_Formal_Param_Holder_P_Abstract_Formal_Params

     (Node   : Bare_Base_Formal_Param_Holder;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;
--  Return the list of abstract formal parameters for this holder.

   function Base_Formal_Param_Holder_P_Unpacked_Formal_Params

     (Node   : Bare_Base_Formal_Param_Holder;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Single_Formal_Array_Access;
--  Couples (identifier, param spec) for all parameters

   function Base_Formal_Param_Holder_P_Match_Param_List

     (Node   : Bare_Base_Formal_Param_Holder;
      Params : Internal_Entity_Assoc_List; Is_Dottable_Subp : Boolean;
      Env    : Lexical_Env; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Param_Match_Array_Access;

   function Base_Formal_Param_Holder_P_Nb_Min_Params

     (Node : Bare_Base_Formal_Param_Holder)
return Integer;
--  Return the minimum number of parameters this subprogram can be called while
--  still being a legal call.

   function Base_Formal_Param_Holder_P_Nb_Max_Params

     (Node : Bare_Base_Formal_Param_Holder)
return Integer;
--  Return the maximum number of parameters this subprogram can be called while
--  still being a legal call.

   function Base_Formal_Param_Holder_P_Paramless

     (Node   : Bare_Base_Formal_Param_Holder; Dottable_Subp : Boolean;
      Can_Be : Boolean := True)
return Boolean;
--  Utility function. Given a subprogram spec and whether the subprogram was
--  referenced using the dot notation, determine if it can be called without
--  parameters (and hence without a callexpr).

   function Base_Formal_Param_Holder_P_Is_Matching_Param_List

     (Node   : Bare_Base_Formal_Param_Holder;
      Params : Internal_Entity_Assoc_List; Is_Dottable_Subp : Boolean;
      Env    : Lexical_Env)
return Boolean;
--  Return whether a AssocList is a match for this SubpSpec, i.e. whether the
--  argument count (and designators, if any) match.

   function Base_Formal_Param_Holder_P_Param_Types

     (Node   : Bare_Base_Formal_Param_Holder;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Returns the type of each parameter of Self.

   function Base_Formal_Param_Holder_P_Real_Type

     (Node   : Bare_Base_Formal_Param_Holder;
      Typ    : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the real type denoted by ``typ``, taking into account that ``typ``
--  might be the type of a derived primitive. In that case, return the derived
--  primitive type.

   function Base_Formal_Param_Holder_P_Match_Formal_Params

     (Node        : Bare_Base_Formal_Param_Holder;
      Other       : Internal_Entity_Base_Formal_Param_Holder;
      Match_Names : Boolean              := True;
      E_Info      : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Check whether self's params match other's.

   function Dispatcher_Base_Subp_Spec_P_Name

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Bare_Defining_Name;

   function Dispatcher_Base_Subp_Spec_P_Returns

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;
--  Syntax property. Return the type expression node corresponding to the
--  return of this subprogram spec.

   function Dispatcher_Base_Subp_Spec_P_Params

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Param_Spec_Array_Access;
--  Returns the array of parameters specification for this subprogram spec.

   function Base_Subp_Spec_P_Abstract_Formal_Params

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Base_Subp_Spec_P_Match_Return_Type

     (Node   : Bare_Base_Subp_Spec; Other : Internal_Entity_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subp_Spec_P_Match_Signature

     (Node       : Bare_Base_Subp_Spec; Other : Internal_Entity_Base_Subp_Spec;
      Match_Name : Boolean; Use_Entity_Info : Boolean := True;
      E_Info     : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether SubpSpec's signature matches Self's.
--
--  Note that the comparison for types isn't just a name comparison: it
--  compares the canonical types.
--
--  If match_name is False, then the name of subprogram will not be checked.
--
--  If use_entity_info is True and Entity's metadata has values for fields
--  `primitive` and `primitive_real_type` (e.g. if it was retrieved from a
--  primitive_env), those will be taken into account and match_signature
--  will return True if `other` overrides `Entity`.

   function Base_Subp_Spec_P_Defining_Env

     (Node   : Bare_Base_Subp_Spec; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Helper for BasicDecl.defining_env.

   function Base_Subp_Spec_P_Potential_Dottable_Type

     (Node   : Bare_Base_Subp_Spec; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If self meets the criteria for being a subprogram callable via the dot
--  notation, return the type of dottable elements.

   function Base_Subp_Spec_P_Candidate_Type_For_Primitive

     (Node   : Bare_Base_Subp_Spec; Typ : Internal_Entity_Type_Expr;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If the given type expression designates a type of which Self is a
--  primitive, return that designated type. Otherwise return null.

   function Base_Subp_Spec_P_Get_Primitive_Subp_Types

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Return the types of which this subprogram is a primitive of.

   function Base_Subp_Spec_P_Get_Primitive_Subp_First_Type

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the first type of which this subprogram is a primitive of.

   function Base_Subp_Spec_P_Get_Primitive_Subp_Tagged_Type

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If this subprogram is a primitive for a tagged type, then return this type.

   function Base_Subp_Spec_P_Decl_Spec

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;
--  If this subp spec is that of the body of an entity, this property returns
--  the subp spec of the declaration of that entity. It returns itself
--  otherwise.

   function Base_Subp_Spec_P_Primitive_Subp_Types

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Return the types of which this subprogram is a primitive of.

   function Base_Subp_Spec_P_Primitive_Subp_First_Type

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the first type of which this subprogram is a primitive of.

   function Base_Subp_Spec_P_Primitive_Subp_Tagged_Type

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If this subprogram is a primitive for a tagged type, then return this type.

   function Base_Subp_Spec_P_Dottable_Subp_Of

     (Node   : Bare_Base_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Returns whether the subprogram containing this spec is a subprogram
--  callable via the dot notation.

   function Base_Subp_Spec_P_Return_Type

     (Node   : Bare_Base_Subp_Spec; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Returns the return type of Self, if applicable (e.g. if Self is a
--  subprogram). Else, returns null.

   function Base_Subp_Spec_P_Xref_Entry_Point

     (Node : Bare_Base_Subp_Spec)
return Boolean;

   function Base_Subp_Spec_P_Xref_Equation

     (Node   : Bare_Base_Subp_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Entry_Spec
     (Self : Bare_Entry_Spec; Entry_Spec_F_Entry_Name : Bare_Defining_Name;
      Entry_Spec_F_Family_Type  : Bare_Ada_Node;
      Entry_Spec_F_Entry_Params : Bare_Params);

   function Entry_Spec_F_Entry_Name
     (Node : Bare_Entry_Spec) return Bare_Defining_Name;

   function Entry_Spec_F_Family_Type
     (Node : Bare_Entry_Spec) return Bare_Ada_Node;

   function Entry_Spec_F_Entry_Params
     (Node : Bare_Entry_Spec) return Bare_Params;

   function Entry_Spec_P_Name

     (Node : Bare_Entry_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Bare_Defining_Name;

   function Entry_Spec_P_Params

     (Node : Bare_Entry_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Param_Spec_Array_Access;

   function Entry_Spec_P_Returns

     (Node : Bare_Entry_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Enum_Subp_Spec_P_Enum_Decl

     (Node   : Bare_Enum_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Enum_Literal_Decl;

   function Enum_Subp_Spec_P_Name

     (Node   : Bare_Enum_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Bare_Defining_Name;

   function Enum_Subp_Spec_P_Returns

     (Node   : Bare_Enum_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Enum_Subp_Spec_P_Params

     (Node   : Bare_Enum_Subp_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Param_Spec_Array_Access;

   procedure Initialize_Fields_For_Subp_Spec
     (Self : Bare_Subp_Spec; Subp_Spec_F_Subp_Kind : Bare_Subp_Kind;
      Subp_Spec_F_Subp_Name    : Bare_Defining_Name;
      Subp_Spec_F_Subp_Params  : Bare_Params;
      Subp_Spec_F_Subp_Returns : Bare_Type_Expr);

   function Subp_Spec_F_Subp_Kind
     (Node : Bare_Subp_Spec) return Bare_Subp_Kind;

   function Subp_Spec_F_Subp_Name
     (Node : Bare_Subp_Spec) return Bare_Defining_Name;

   function Subp_Spec_F_Subp_Params (Node : Bare_Subp_Spec) return Bare_Params;

   function Subp_Spec_F_Subp_Returns
     (Node : Bare_Subp_Spec) return Bare_Type_Expr;

   function Subp_Spec_P_Name

     (Node : Bare_Subp_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Bare_Defining_Name;

   function Subp_Spec_P_Params

     (Node : Bare_Subp_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Param_Spec_Array_Access;

   function Subp_Spec_P_Returns

     (Node : Bare_Subp_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   procedure Initialize_Fields_For_Component_List
     (Self                          : Bare_Component_List;
      Component_List_F_Components   : Bare_Ada_Node_List;
      Component_List_F_Variant_Part : Bare_Variant_Part);

   function Component_List_F_Components
     (Node : Bare_Component_List) return Bare_Ada_Node_List;

   function Component_List_F_Variant_Part
     (Node : Bare_Component_List) return Bare_Variant_Part;

   function Component_List_P_Type_Def

     (Node   : Bare_Component_List;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Def;

   function Component_List_P_Type_Decl

     (Node   : Bare_Component_List;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Decl;

   function Component_List_P_Parent_Component_List

     (Node   : Bare_Component_List;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Component_List;

   function Component_List_P_Abstract_Formal_Params_For_Assocs

     (Node   : Bare_Component_List; Assocs : Internal_Entity_Assoc_List;
      Env    : Lexical_Env; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Component_List_P_Abstract_Formal_Params_Impl

     (Node                  : Bare_Component_List;
      Discriminants         : Internal_Param_Match_Array_Access;
      Include_Discriminants : Boolean := True; Recurse : Boolean := True;
      E_Info                : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Component_List_P_Abstract_Formal_Params

     (Node   : Bare_Component_List;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Discriminant_Part_P_Abstract_Formal_Params

     (Node   : Bare_Discriminant_Part;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   procedure Initialize_Fields_For_Known_Discriminant_Part
     (Self                                  : Bare_Known_Discriminant_Part;
      Known_Discriminant_Part_F_Discr_Specs : Bare_Discriminant_Spec_List);

   function Known_Discriminant_Part_F_Discr_Specs
     (Node : Bare_Known_Discriminant_Part) return Bare_Discriminant_Spec_List;

   function Known_Discriminant_Part_P_Abstract_Formal_Params

     (Node   : Bare_Known_Discriminant_Part;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   procedure Initialize_Fields_For_Entry_Completion_Formal_Params
     (Self : Bare_Entry_Completion_Formal_Params;
      Entry_Completion_Formal_Params_F_Params : Bare_Params);

   function Entry_Completion_Formal_Params_F_Params
     (Node : Bare_Entry_Completion_Formal_Params) return Bare_Params;

   function Entry_Completion_Formal_Params_P_Abstract_Formal_Params

     (Node   : Bare_Entry_Completion_Formal_Params;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   procedure Initialize_Fields_For_Generic_Formal_Part
     (Self                        : Bare_Generic_Formal_Part;
      Generic_Formal_Part_F_Decls : Bare_Ada_Node_List);

   function Generic_Formal_Part_F_Decls
     (Node : Bare_Generic_Formal_Part) return Bare_Ada_Node_List;

   function Generic_Formal_Part_P_Abstract_Formal_Params

     (Node   : Bare_Generic_Formal_Part;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   procedure Initialize_Fields_For_Base_Record_Def
     (Self                         : Bare_Base_Record_Def;
      Base_Record_Def_F_Components : Bare_Component_List);

   function Base_Record_Def_F_Components
     (Node : Bare_Base_Record_Def) return Bare_Component_List;

   function Base_Record_Def_P_Comps

     (Node   : Bare_Base_Record_Def;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Component_List;

   procedure Initialize_Fields_For_Null_Record_Def
     (Self                         : Bare_Null_Record_Def;
      Base_Record_Def_F_Components : Bare_Component_List);

   procedure Initialize_Fields_For_Record_Def
     (Self                         : Bare_Record_Def;
      Base_Record_Def_F_Components : Bare_Component_List);

   function Dispatcher_Basic_Assoc_P_Expr

     (Node : Bare_Basic_Assoc; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   function Dispatcher_Basic_Assoc_P_Names

     (Node : Bare_Basic_Assoc)
return Bare_Ada_Node_Array_Access;

   function Basic_Assoc_P_Get_Params

     (Node   : Bare_Basic_Assoc; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;
--  Return the list of parameters that this association refers to.

   procedure Initialize_Fields_For_Aggregate_Assoc
     (Self                          : Bare_Aggregate_Assoc;
      Aggregate_Assoc_F_Designators : Bare_Alternatives_List;
      Aggregate_Assoc_F_R_Expr      : Bare_Expr);

   function Aggregate_Assoc_F_Designators
     (Node : Bare_Aggregate_Assoc) return Bare_Alternatives_List;

   function Aggregate_Assoc_F_R_Expr
     (Node : Bare_Aggregate_Assoc) return Bare_Expr;

   function Aggregate_Assoc_P_Expr

     (Node   : Bare_Aggregate_Assoc;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   function Aggregate_Assoc_P_Names

     (Node : Bare_Aggregate_Assoc)
return Bare_Ada_Node_Array_Access;

   function Aggregate_Assoc_P_Xref_Stop_Resolution

     (Node : Bare_Aggregate_Assoc)
return Boolean;

   function Aggregate_Assoc_P_Base_Aggregate

     (Node   : Bare_Aggregate_Assoc;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Aggregate;

   function Aggregate_Assoc_P_Xref_Equation

     (Node   : Bare_Aggregate_Assoc; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Aggregate_Assoc_P_Record_Assoc_Equation

     (Node   : Bare_Aggregate_Assoc; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Equation for the case where this is an aggregate assoc for a record type.

   function Aggregate_Assoc_P_Array_Assoc_Equation

     (Node   : Bare_Aggregate_Assoc; Atd : Internal_Entity_Array_Type_Def;
      Mra    : Internal_Multidim_Aggregate_Info; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;
--  Equation for the case where this is an aggregate assoc for an array type.

   function Aggregate_Assoc_P_Globals_Assoc_Equation

     (Node   : Bare_Aggregate_Assoc; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Equation for the case where this is an aggregate assoc for a Globals
--  aspect.

   function Aggregate_Assoc_P_Depends_Assoc_Equation

     (Node   : Bare_Aggregate_Assoc; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Equation for the case where this is an aggregate assoc for a Depends
--  aspect.

   procedure Initialize_Fields_For_Multi_Dim_Array_Assoc
     (Self                          : Bare_Multi_Dim_Array_Assoc;
      Aggregate_Assoc_F_Designators : Bare_Alternatives_List;
      Aggregate_Assoc_F_R_Expr      : Bare_Expr);

   procedure Initialize_Fields_For_Discriminant_Assoc
     (Self                            : Bare_Discriminant_Assoc;
      Discriminant_Assoc_F_Ids        : Bare_Discriminant_Choice_List;
      Discriminant_Assoc_F_Discr_Expr : Bare_Expr);

   function Discriminant_Assoc_F_Ids
     (Node : Bare_Discriminant_Assoc) return Bare_Discriminant_Choice_List;

   function Discriminant_Assoc_F_Discr_Expr
     (Node : Bare_Discriminant_Assoc) return Bare_Expr;

   function Discriminant_Assoc_P_Expr

     (Node   : Bare_Discriminant_Assoc;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   function Discriminant_Assoc_P_Names

     (Node : Bare_Discriminant_Assoc)
return Bare_Ada_Node_Array_Access;

   procedure Initialize_Fields_For_Param_Assoc
     (Self : Bare_Param_Assoc; Param_Assoc_F_Designator : Bare_Ada_Node;
      Param_Assoc_F_R_Expr : Bare_Expr);

   function Param_Assoc_F_Designator
     (Node : Bare_Param_Assoc) return Bare_Ada_Node;

   function Param_Assoc_F_R_Expr (Node : Bare_Param_Assoc) return Bare_Expr;

   function Param_Assoc_P_Expr

     (Node : Bare_Param_Assoc; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   function Param_Assoc_P_Names

     (Node : Bare_Param_Assoc)
return Bare_Ada_Node_Array_Access;

   function Param_Assoc_P_Xref_Entry_Point

     (Node : Bare_Param_Assoc)
return Boolean;

   function Param_Assoc_P_Xref_Equation

     (Node   : Bare_Param_Assoc; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Param_Assoc_P_Is_Static_Attribute_Assoc

     (Node : Bare_Param_Assoc)
return Boolean;

   function Basic_Decl_F_Aspects
     (Node : Bare_Basic_Decl) return Bare_Aspect_Spec;

   function Basic_Decl_P_Env_Hook_Basic_Decl

     (Node : Bare_Basic_Decl)
return Boolean;
--  Helper for AdaNode.env_hook. Handle library-level unit decl nodes.

   function Basic_Decl_P_Is_Formal
(Node : Bare_Basic_Decl)
return Boolean;
--  Whether this decl is the nested decl of a generic formal declaration.

   function Dispatcher_Basic_Decl_P_Previous_Part_For_Decl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the previous part for this decl, if applicable.
--
--  .. note:: It is not named previous_part, because BaseTypeDecl has a more
--     precise version of previous_part that returns a BaseTypeDecl. Probably,
--     we want to rename the specific versions, and have the root property be
--     named previous_part. (TODO R925-008)

   function Basic_Decl_P_Canonical_Part

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the canonical part for this decl. In the case of decls composed of
--  several parts, the canonical part will be the first part.

   function Dispatcher_Basic_Decl_P_Is_Static_Decl

     (Node   : Bare_Basic_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this declaration is static.

   function Basic_Decl_P_Unshed_Rebindings

     (Node   : Bare_Basic_Decl; Rebindings : Env_Rebindings;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Put ``rebindings`` back on ``Entity`` if ``Entity`` is rebound somewhere in
--  the chain of rebindings. Ensure coherency, e.g. that if Entity already has
--  some rebindings, the one that we add are a superset of the one it already
--  has.

   function Dispatcher_Basic_Decl_P_Is_Imported

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Whether this declaration is imported from another language.

   function Basic_Decl_P_Decl_Private_Part

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Private_Part;

   function Dispatcher_Basic_Decl_P_Declarative_Region

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Declarative_Part;
--  Return the (first) declarative region of this BasicDecl, if applicable.

   function Dispatcher_Basic_Decl_P_Get_Aspect_Assoc

     (Node   : Bare_Basic_Decl; Name : Symbol_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Aspect_Assoc;
--  Return the aspect with name ``name`` for this entity.

   function Basic_Decl_P_Get_Aspect_Spec_Expr

     (Node   : Bare_Basic_Decl; Name : Symbol_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;
--  Return the expression associated to the aspect with name ``name`` for this
--  entity.

   function Basic_Decl_P_Library_Item_Pragmas

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Pragma_Node_List;
--  If this entity is a library item, return the compilation unit pragmas.

   function Basic_Decl_P_Get_Aspect

     (Node               : Bare_Basic_Decl; Name : Symbol_Type;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)
return Internal_Aspect;
--  Return the aspect with name ``name`` associated to this entity.
--
--  Aspects are properties of entities that can be specified by the Ada
--  program, either via aspect specifications, pragmas, or attributes.
--
--  This will return the syntactic node corresponding to attribute directly.

   function Basic_Decl_P_Has_Aspect

     (Node               : Bare_Basic_Decl; Name : Symbol_Type;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns whether the boolean aspect named ``name`` is set on the entity
--  represented by this node.
--
--  "Aspect" is used as in RM terminology (see RM 13).

   function Basic_Decl_P_Is_Valid_Pragma_For_Name

     (Node   : Bare_Basic_Decl; Name : Symbol_Type; Decl : Internal_Entity;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Helper property for ``get_pragma``. Used to check that ``decl`` is a
--  pragma declaration that has the given name and is a valid pragma for
--  this declaration.

   function Basic_Decl_P_Get_Pragma

     (Node   : Bare_Basic_Decl; Name : Symbol_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Pragma_Node;
--  Return the pragma with name ``name`` associated to this entity.

   function Basic_Decl_P_Get_Representation_Clause

     (Node               : Bare_Basic_Decl; Name : Symbol_Type;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Attribute_Def_Clause;
--  Return the representation clause associated to this type decl that defines
--  the given attribute name.

   function Basic_Decl_P_Is_Compilation_Unit_Root

     (Node : Bare_Basic_Decl)
return Boolean;
--  Whether a BasicDecl is the root decl for its unit.

   function Basic_Decl_P_Populate_Dependent_Units

     (Node : Bare_Basic_Decl)
return Bare_Ada_Node_Array_Access;

   function Basic_Decl_P_Should_Ref_Generic_Formals

     (Node : Bare_Basic_Decl)
return Boolean;
--  Helper property used to determine whether we should add a referenced_env to
--  the generic formal part of a given entity.

   function Basic_Decl_P_Is_In_Public_Part

     (Node : Bare_Basic_Decl)
return Boolean;

   function Dispatcher_Basic_Decl_P_Is_In_Private_Part

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Basic_Decl_P_Is_Visible

     (Node   : Bare_Basic_Decl; From_Node : Internal_Entity;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this declaration is visible from the point of view of the
--  given ``origin`` node.
--
--  .. ATTENTION:: Only package-level (public or private) declarations are
--     supported for now.

   function Basic_Decl_P_Subp_Decl_Match_Signature

     (Node   : Bare_Basic_Decl; Other : Internal_Entity_Basic_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Basic_Decl_P_Base_Subp_Declarations

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  If Self declares a primitive subprogram of some tagged type T, return the
--  set of all subprogram declarations that it overrides (including itself).

   function Basic_Decl_P_Root_Subp_Declarations

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  If Self declares a primitive subprogram of some tagged type T, return the
--  root subprogram declarations that it overrides. There can be several, as
--  in the following scenario:
--
--  - package Root defines the root tagged type T and subprogram Foo.
--
--  - package Itf defines interface I and abstract subprogram Foo.
--
--  - package D defines "type U is new Root.T and Itf.I" and an overriding
--  subprogram Foo.
--
--  Here, root_subp_declarations of Foo defined in package D will return both
--  Foo from package Root and Foo from package Itf.

   function Basic_Decl_P_Find_All_Overrides

     (Node               : Bare_Basic_Decl; Units : Internal_Unit_Array_Access;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  If Self is the declaration of a primitive of some type T, return the list
--  of all subprogram that override this subprogram among the given units.

   function Dispatcher_Basic_Decl_P_Defining_Names

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;
--  Get all the names of this basic declaration.

   function Basic_Decl_P_Defining_Name

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Get the name of this declaration. If this declaration has several names, it
--  will return the first one.

   function Dispatcher_Basic_Decl_P_Defining_Env

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Return a lexical environment that contains entities that are accessible as
--  suffixes when Self is a prefix.

   function Basic_Decl_P_Identity_Type

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Dispatcher_Basic_Decl_P_Array_Ndims

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Basic_Decl_P_Is_Array

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Basic_Decl_P_Expr_Type

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type declaration corresponding to this basic declaration has
--  when it is used in an expression context. For example, for this basic
--  declaration::
--
--  type Int is range 0 .. 100;
--
--  A : Int := 12;
--
--  the declaration of the Int type will be returned. For this declaration::
--
--  type F is delta 0.01 digits 10;
--
--  function B return F;
--
--  expr_type will return the declaration of the type F.

   function Dispatcher_Basic_Decl_P_Type_Expression

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;
--  Return the type expression for this BasicDecl if applicable, a null
--  otherwise.

   function Basic_Decl_P_Subp_Spec_Or_Null

     (Node   : Bare_Basic_Decl; Follow_Generic : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;
--  If Self is a Subp, returns the specification of this subprogram.
--
--  If ``follow_generic`` is True, will also work for instances of
--  ``GenericSubpDecl``.

   function Basic_Decl_P_Formal_Param_Holder_Or_Null

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Holder;

   function Basic_Decl_P_Is_Subprogram
(Node : Bare_Basic_Decl)
return Boolean;
--  Return True if self is a subprogram node in the general sense (which is, an
--  entity that can be called). This includes separates and entries.

   function Basic_Decl_P_Is_Stream_Subprogram_For_Type

     (Node       : Bare_Basic_Decl; Typ : Internal_Entity_Base_Type_Decl;
      Return_Obj : Boolean; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Basic_Decl_P_Can_Be_Paramless

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return true if entity can be a paramless subprogram entity, when used in an
--  expression context.

   function Basic_Decl_P_Is_Paramless

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return true if entity is a paramless subprogram entity, when used in an
--  expression context.

   function Dispatcher_Basic_Decl_P_Constrain_Prefix

     (Node   : Bare_Basic_Decl; Prefix : Bare_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  This method is used when self is a candidate suffix in a dotted expression,
--  to express the potential constraint that the suffix could express on the
--  prefix.
--
--  For example, given this code::
--
--  1 type P is record 2 A, B : Integer; 3 end record; 4 5 P_Inst : P; 7 8
--  P_Inst.A; ^^^^^^^^
--
--  A references the A ComponentDecl at line 2, and the constraint that we want
--  to express on the prefix (P_Inst), is that it needs to be of type P.

   function Basic_Decl_P_Relative_Name

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;
--  Return the relative name for Self. If Self's defining name is ``A.B.C``,
--  return C as a node.

   function Basic_Decl_P_Relative_Name_Text

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Symbol_Type;
--  Return the relative name for Self, as text.

   function Basic_Decl_P_Name_Symbol

     (Node : Bare_Basic_Decl)
return Symbol_Type;

   function Basic_Decl_P_Basic_Decl_Next_Part_For_Decl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Implementation of next_part_for_decl for basic decls, that can be reused by
--  subclasses when they override next_part_for_decl.

   function Dispatcher_Basic_Decl_P_Next_Part_For_Decl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the next part of this declaration, if applicable.
--
--  .. note:: It is not named next_part, because BaseTypeDecl has a more
--     precise version of next_part that returns a BaseTypeDecl. Probably, we
--     want to rename the specific versions, and have the root property be
--     named next_part. (TODO R925-008)

   function Basic_Decl_P_Body_Part_For_Decl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Body_Node;
--  Return the body corresponding to this declaration, if applicable.
--
--  .. note:: It is not named body_part, subclasses have more precise versions
--     named body_part and returning a more precise result. Probably, we want
--     to rename the specific versions, and have the root property be named
--     previous_part. (TODO R925-008)

   function Basic_Decl_P_Decl_Scope

     (Node : Bare_Basic_Decl; Follow_Private : Boolean := True;
      Env  : Lexical_Env; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Basic_Decl_P_Fully_Qualified_Name_Impl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Bare_Single_Tok_Node_Array_Access;
--  Return the fully qualified name corresponding to this declaration, as an
--  array of symbols.

   function Basic_Decl_P_Fully_Qualified_Name_Array

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Symbol_Type_Array_Access;
--  Return the fully qualified name corresponding to this declaration, as an
--  array of symbols.

   function Basic_Decl_P_Fully_Qualified_Name

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;
--  Return the fully qualified name corresponding to this declaration.

   function Basic_Decl_P_Canonical_Fully_Qualified_Name

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;
--  Return a canonical representation of the fully qualified name corresponding
--  to this declaration.

   function Basic_Decl_P_Unique_Identifying_Name

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;
--  Return a unique identifying name for this declaration, provided this
--  declaration is a public declaration. In the case of subprograms, this
--  will include the profile.
--
--  .. attention:: This will only return a unique name for public declarations.
--     Notably, anything nested in an unnamed declare block won't be handled
--     correctly.

   function Basic_Decl_P_Custom_Id_Text

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;

   function Basic_Decl_P_Does_Aspects_Make_Preelaborable

     (Node   : Bare_Basic_Decl; From_Body : Boolean;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Implementation helper for ``CompilationUnit.is_preelaborable``.
--
--  Return whether ``Entity`` has aspects that make it preelaborable.
--
--  If ``from_body``, consider that ``Entity`` is a spec and that we are
--  computing whether its body is preelaborable.

   function Basic_Decl_P_Previous_Part_For_Decl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the previous part for this decl, if applicable.
--
--  .. note:: It is not named previous_part, because BaseTypeDecl has a more
--     precise version of previous_part that returns a BaseTypeDecl. Probably,
--     we want to rename the specific versions, and have the root property be
--     named previous_part. (TODO R925-008)

   function Basic_Decl_P_Is_Static_Decl

     (Node   : Bare_Basic_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this declaration is static.

   function Basic_Decl_P_Is_Imported

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Whether this declaration is imported from another language.

   function Basic_Decl_P_Declarative_Region

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Declarative_Part;
--  Return the (first) declarative region of this BasicDecl, if applicable.

   function Basic_Decl_P_Get_Aspect_Assoc

     (Node   : Bare_Basic_Decl; Name : Symbol_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Aspect_Assoc;
--  Return the aspect with name ``name`` for this entity.

   function Basic_Decl_P_Is_In_Private_Part

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Basic_Decl_P_Defining_Env

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Return a lexical environment that contains entities that are accessible as
--  suffixes when Self is a prefix.

   function Basic_Decl_P_Array_Ndims

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Basic_Decl_P_Expr_Type

     (Node   : Bare_Basic_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type declaration corresponding to this basic declaration has
--  when it is used in an expression context. For example, for this basic
--  declaration::
--
--  type Int is range 0 .. 100;
--
--  A : Int := 12;
--
--  the declaration of the Int type will be returned. For this declaration::
--
--  type F is delta 0.01 digits 10;
--
--  function B return F;
--
--  expr_type will return the declaration of the type F.

   function Basic_Decl_P_Type_Expression

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;
--  Return the type expression for this BasicDecl if applicable, a null
--  otherwise.

   function Basic_Decl_P_Constrain_Prefix

     (Node   : Bare_Basic_Decl; Prefix : Bare_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  This method is used when self is a candidate suffix in a dotted expression,
--  to express the potential constraint that the suffix could express on the
--  prefix.
--
--  For example, given this code::
--
--  1 type P is record 2 A, B : Integer; 3 end record; 4 5 P_Inst : P; 7 8
--  P_Inst.A; ^^^^^^^^
--
--  A references the A ComponentDecl at line 2, and the constraint that we want
--  to express on the prefix (P_Inst), is that it needs to be of type P.

   function Basic_Decl_P_Next_Part_For_Decl

     (Node : Bare_Basic_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the next part of this declaration, if applicable.
--
--  .. note:: It is not named next_part, because BaseTypeDecl has a more
--     precise version of next_part that returns a BaseTypeDecl. Probably, we
--     want to rename the specific versions, and have the root property be
--     named next_part. (TODO R925-008)

   function Dispatcher_Base_Formal_Param_Decl_P_Is_Mandatory

     (Node : Bare_Base_Formal_Param_Decl)
return Boolean;

   function Base_Formal_Param_Decl_P_Formal_Type

     (Node   : Bare_Base_Formal_Param_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type for this formal.

   function Base_Formal_Param_Decl_P_Parent_Decl

     (Node   : Bare_Base_Formal_Param_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Base_Formal_Param_Decl_P_Get_Param

     (Node   : Bare_Base_Formal_Param_Decl; Part : Internal_Entity_Basic_Decl;
      Param  : Internal_Entity_Defining_Name := No_Entity_Defining_Name;
      E_Info : Internal_Entity_Info          := No_Entity_Info)

      return Internal_Entity_Defining_Name;

   function Base_Formal_Param_Decl_P_Decl_Param

     (Node   : Bare_Base_Formal_Param_Decl;
      Param  : Internal_Entity_Defining_Name;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  If self is a ParamSpec of a subprogram body, go fetch the equivalent spec
--  in the subprogram decl.

   function Base_Formal_Param_Decl_P_Next_Part_For_Decl

     (Node   : Bare_Base_Formal_Param_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Base_Formal_Param_Decl_P_Previous_Part_For_Decl

     (Node   : Bare_Base_Formal_Param_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Base_Formal_Param_Decl_P_Is_Mandatory

     (Node : Bare_Base_Formal_Param_Decl)
return Boolean;

   procedure Initialize_Fields_For_Component_Decl
     (Self                           : Bare_Component_Decl;
      Component_Decl_F_Ids           : Bare_Defining_Name_List;
      Component_Decl_F_Component_Def : Bare_Component_Def;
      Component_Decl_F_Default_Expr  : Bare_Expr;
      Component_Decl_F_Aspects       : Bare_Aspect_Spec);

   function Component_Decl_F_Ids
     (Node : Bare_Component_Decl) return Bare_Defining_Name_List;

   function Component_Decl_F_Component_Def
     (Node : Bare_Component_Decl) return Bare_Component_Def;

   function Component_Decl_F_Default_Expr
     (Node : Bare_Component_Decl) return Bare_Expr;

   function Component_Decl_P_Defining_Env

     (Node   : Bare_Component_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  See BasicDecl.defining_env

   function Component_Decl_P_Defining_Names

     (Node   : Bare_Component_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Component_Decl_P_Array_Ndims

     (Node   : Bare_Component_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Component_Decl_P_Type_Expression

     (Node   : Bare_Component_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Component_Decl_P_Constrain_Prefix

     (Node   : Bare_Component_Decl; Prefix : Bare_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Component_Decl_P_Container_Type

     (Node   : Bare_Component_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the defining container type for this component declaration.

   function Component_Decl_P_Xref_Equation

     (Node   : Bare_Component_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Component_Decl_P_Xref_Entry_Point

     (Node : Bare_Component_Decl)
return Boolean;

   function Env_Mappings_1

     (Node : Bare_Component_Decl)
return Internal_Env_Assoc_Array_Access;

   function Bare_Component_Decl_Pre_Env_Actions
     (Self : Bare_Component_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Discriminant_Spec
     (Self                             : Bare_Discriminant_Spec;
      Discriminant_Spec_F_Ids          : Bare_Defining_Name_List;
      Discriminant_Spec_F_Type_Expr    : Bare_Type_Expr;
      Discriminant_Spec_F_Default_Expr : Bare_Expr);

   function Discriminant_Spec_F_Ids
     (Node : Bare_Discriminant_Spec) return Bare_Defining_Name_List;

   function Discriminant_Spec_F_Type_Expr
     (Node : Bare_Discriminant_Spec) return Bare_Type_Expr;

   function Discriminant_Spec_F_Default_Expr
     (Node : Bare_Discriminant_Spec) return Bare_Expr;

   function Discriminant_Spec_P_Defining_Names

     (Node   : Bare_Discriminant_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Discriminant_Spec_P_Defining_Env

     (Node   : Bare_Discriminant_Spec; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Discriminant_Spec_P_Type_Expression

     (Node   : Bare_Discriminant_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Discriminant_Spec_P_Xref_Entry_Point

     (Node : Bare_Discriminant_Spec)
return Boolean;

   function Discriminant_Spec_P_Xref_Equation

     (Node : Bare_Discriminant_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Env_Mappings_0

     (Node : Bare_Discriminant_Spec)
return Internal_Env_Assoc_Array_Access;

   function Bare_Discriminant_Spec_Pre_Env_Actions
     (Self                : Bare_Discriminant_Spec;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Generic_Formal
     (Self : Bare_Generic_Formal; Generic_Formal_F_Decl : Bare_Basic_Decl);

   function Generic_Formal_F_Decl
     (Node : Bare_Generic_Formal) return Bare_Basic_Decl;

   function Generic_Formal_P_Defining_Names

     (Node   : Bare_Generic_Formal;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Generic_Formal_P_Type_Expression

     (Node   : Bare_Generic_Formal;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Generic_Formal_P_Xref_Entry_Point

     (Node : Bare_Generic_Formal)
return Boolean;

   function Generic_Formal_P_Xref_Equation

     (Node   : Bare_Generic_Formal; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Generic_Formal_Obj_Decl
     (Self                  : Bare_Generic_Formal_Obj_Decl;
      Generic_Formal_F_Decl : Bare_Basic_Decl);

   function Generic_Formal_Obj_Decl_P_Mode

     (Node   : Bare_Generic_Formal_Obj_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Mode;

   procedure Initialize_Fields_For_Generic_Formal_Package
     (Self                  : Bare_Generic_Formal_Package;
      Generic_Formal_F_Decl : Bare_Basic_Decl);

   procedure Initialize_Fields_For_Generic_Formal_Subp_Decl
     (Self                  : Bare_Generic_Formal_Subp_Decl;
      Generic_Formal_F_Decl : Bare_Basic_Decl);

   procedure Initialize_Fields_For_Generic_Formal_Type_Decl
     (Self                  : Bare_Generic_Formal_Type_Decl;
      Generic_Formal_F_Decl : Bare_Basic_Decl);

   procedure Initialize_Fields_For_Param_Spec
     (Self : Bare_Param_Spec; Param_Spec_F_Ids : Bare_Defining_Name_List;
      Param_Spec_F_Has_Aliased  : Bare_Aliased_Node;
      Param_Spec_F_Mode : Bare_Mode; Param_Spec_F_Type_Expr : Bare_Type_Expr;
      Param_Spec_F_Default_Expr : Bare_Expr);

   function Param_Spec_F_Ids
     (Node : Bare_Param_Spec) return Bare_Defining_Name_List;

   function Param_Spec_F_Has_Aliased
     (Node : Bare_Param_Spec) return Bare_Aliased_Node;

   function Param_Spec_F_Mode (Node : Bare_Param_Spec) return Bare_Mode;

   function Param_Spec_F_Type_Expr
     (Node : Bare_Param_Spec) return Bare_Type_Expr;

   function Param_Spec_F_Default_Expr
     (Node : Bare_Param_Spec) return Bare_Expr;

   function Param_Spec_P_Is_Mandatory
(Node : Bare_Param_Spec)
return Boolean;

   function Param_Spec_P_Defining_Names

     (Node : Bare_Param_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Param_Spec_P_Type_Expression

     (Node : Bare_Param_Spec; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Param_Spec_P_Defining_Env

     (Node   : Bare_Param_Spec; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Param_Spec_P_Xref_Equation

     (Node   : Bare_Param_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Param_Spec_P_Xref_Entry_Point

     (Node : Bare_Param_Spec)
return Boolean;

   function Env_Mappings_19

     (Node : Bare_Param_Spec)
return Internal_Env_Assoc_Array_Access;

   function Bare_Param_Spec_Pre_Env_Actions
     (Self : Bare_Param_Spec; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Base_Package_Decl
     (Self                             : Bare_Base_Package_Decl;
      Base_Package_Decl_F_Package_Name : Bare_Defining_Name;
      Base_Package_Decl_F_Aspects      : Bare_Aspect_Spec;
      Base_Package_Decl_F_Public_Part  : Bare_Public_Part;
      Base_Package_Decl_F_Private_Part : Bare_Private_Part;
      Base_Package_Decl_F_End_Name     : Bare_End_Name);

   function Base_Package_Decl_F_Package_Name
     (Node : Bare_Base_Package_Decl) return Bare_Defining_Name;

   function Base_Package_Decl_F_Public_Part
     (Node : Bare_Base_Package_Decl) return Bare_Public_Part;

   function Base_Package_Decl_F_Private_Part
     (Node : Bare_Base_Package_Decl) return Bare_Private_Part;

   function Base_Package_Decl_F_End_Name
     (Node : Bare_Base_Package_Decl) return Bare_End_Name;

   function Base_Package_Decl_P_Defining_Names

     (Node   : Bare_Base_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Base_Package_Decl_P_Defining_Env

     (Node   : Bare_Base_Package_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Base_Package_Decl_P_Body_Part

     (Node   : Bare_Base_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Package_Body;
--  Return the PackageBody corresponding to this node.

   function Base_Package_Decl_P_Declarative_Region

     (Node   : Bare_Base_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Declarative_Part;

   procedure Initialize_Fields_For_Generic_Package_Internal
     (Self                             : Bare_Generic_Package_Internal;
      Base_Package_Decl_F_Package_Name : Bare_Defining_Name;
      Base_Package_Decl_F_Aspects      : Bare_Aspect_Spec;
      Base_Package_Decl_F_Public_Part  : Bare_Public_Part;
      Base_Package_Decl_F_Private_Part : Bare_Private_Part;
      Base_Package_Decl_F_End_Name     : Bare_End_Name);

   function Env_Trans_Parent_98

     (Node : Bare_Generic_Package_Internal)
return Boolean;

   function Bare_Generic_Package_Internal_Pre_Env_Actions
     (Self                : Bare_Generic_Package_Internal;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Package_Decl
     (Self                             : Bare_Package_Decl;
      Base_Package_Decl_F_Package_Name : Bare_Defining_Name;
      Base_Package_Decl_F_Aspects      : Bare_Aspect_Spec;
      Base_Package_Decl_F_Public_Part  : Bare_Public_Part;
      Base_Package_Decl_F_Private_Part : Bare_Private_Part;
      Base_Package_Decl_F_End_Name     : Bare_End_Name);

   function Env_Do_37
(Node : Bare_Package_Decl)
return Boolean;

   function Initial_Env_38

     (Node   : Bare_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_39

     (Node   : Bare_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_40
(Node : Bare_Package_Decl)
return Boolean;

   function Env_Do_41

     (Node : Bare_Package_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_42

     (Node : Bare_Package_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_43
(Node : Bare_Package_Decl)
return Boolean;

   function Ref_Env_Nodes_44

     (Node : Bare_Package_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_45
(Node : Bare_Package_Decl)
return Boolean;

   function Bare_Package_Decl_Pre_Env_Actions
     (Self : Bare_Package_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Base_Type_Decl
     (Self : Bare_Base_Type_Decl; Base_Type_Decl_F_Name : Bare_Defining_Name);

   function Base_Type_Decl_F_Name
     (Node : Bare_Base_Type_Decl) return Bare_Defining_Name;

   function Base_Type_Decl_P_Defining_Names

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Base_Type_Decl_P_Base_Subtype

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If this type decl is a subtype decl, return the base subtype. If not,
--  return ``Self``.

   function Base_Type_Decl_P_Anonymous_Access_Type

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Type_Decl_P_Anonymous_Access_Type_Or_Null

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Type_Decl_P_Private_Completion

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the private completion for this type, if there is one.

   function Base_Type_Decl_P_Model_Of_Type

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type for which this type is a model, if applicable.

   function Base_Type_Decl_P_Modeled_Type

     (Node   : Bare_Base_Type_Decl; From_Unit : Internal_Unit;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return model type for this type if applicable.

   function Base_Type_Decl_P_Is_View_Of_Type

     (Node   : Bare_Base_Type_Decl; Comp_View : Internal_Entity_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Predicate that will return true if comp_view is a more complete view of
--  type typ, or if it is the same view of type typ.

   function Base_Type_Decl_P_Is_Array_Or_Rec

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Type_Decl_P_Get_Record_Representation_Clause

     (Node   : Bare_Base_Type_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Record_Rep_Clause;
--  Return the record representation clause associated to this type decl, if
--  applicable (i.e. this type decl defines a record type).

   function Base_Type_Decl_P_Get_Enum_Representation_Clause

     (Node   : Bare_Base_Type_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Enum_Rep_Clause;
--  Return the enum representation clause associated to this type decl, if
--  applicable (i.e. this type decl defines an enum type).

   function Dispatcher_Base_Type_Decl_P_Primitives_Env

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Dispatcher_Base_Type_Decl_P_Is_Record_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is a record type.

   function Base_Type_Decl_P_Is_Array_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is an array type.

   function Base_Type_Decl_P_Find_Derived_Types

     (Node   : Bare_Base_Type_Decl; Root : Internal_Entity;
      Origin : Bare_Ada_Node; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Decl_Array_Access;
--  Find types derived from self in the given ``root`` and its children.

   function Dispatcher_Base_Type_Decl_P_Is_Task_Type

     (Node : Bare_Base_Type_Decl)
return Boolean;
--  Whether type is a task type

   function Dispatcher_Base_Type_Decl_P_Is_Real_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a real type or not.

   function Dispatcher_Base_Type_Decl_P_Is_Float_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a float type or not.

   function Dispatcher_Base_Type_Decl_P_Is_Fixed_Point

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a fixed point type or not.

   function Dispatcher_Base_Type_Decl_P_Is_Enum_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is an enum type

   function Dispatcher_Base_Type_Decl_P_Is_Classwide

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Base_Type_Decl_P_Is_Access_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is an access type or not

   function Base_Type_Decl_P_Is_Implicit_Deref

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is an implicitly dereferenceable type or not

   function Dispatcher_Base_Type_Decl_P_Has_Ud_Indexing

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether self has user defined indexing or not

   function Dispatcher_Base_Type_Decl_P_Constant_Indexing_Fns

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  For a type with user defined indexing, return the set of all
--  Constant_Indexing functions.

   function Dispatcher_Base_Type_Decl_P_Variable_Indexing_Fns

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  For a type with user defined indexing, return the set of all
--  Variable_Indexing functions.

   function Dispatcher_Base_Type_Decl_P_Get_Imp_Deref

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;
--  If self has an Implicit_Dereference aspect, return its expression

   function Dispatcher_Base_Type_Decl_P_Access_Def

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Access_Def;

   function Dispatcher_Base_Type_Decl_P_Is_Char_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a character type or not

   function Dispatcher_Base_Type_Decl_P_Classwide_Type

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Classwide_Type_Decl;

   function Base_Type_Decl_P_Is_Universal_Type

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is one of the two universal types (universal
--  integer or universal real).
--
--  .. note:: Returns False if Self is null.

   function Base_Type_Decl_P_Is_Not_Universal_Type

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is *not* one of the two universal types (universal
--  integer or universal real).
--
--  .. note:: Returns False if Self is null.

   function Base_Type_Decl_P_Array_Ndims

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Dispatcher_Base_Type_Decl_P_Discrete_Range

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;
--  Return the discrete range for this type decl, if applicable.

   function Base_Type_Decl_P_Static_Predicate

     (Node   : Bare_Base_Type_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;
--  Return the expression from the Static_Predicate or the Predicate aspect
--  defined on this type.

   function Base_Type_Decl_P_Satisfies_Type_Predicates

     (Node               : Bare_Base_Type_Decl; Value : Big_Integer_Type;
      Imprecise_Fallback : Boolean              := False;
      Origin             : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info             : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return true if the given value satisfies all of this type's static
--  predicates, including its parent predicates (in case this is a derived
--  type) and its base type predicate (if this is a subtype declaration).
--  Return true if no type predicates are defined for this type.

   function Base_Type_Decl_P_Is_Iterator_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Base_Type_Decl_P_Is_Discrete_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a discrete type or not.

   function Dispatcher_Base_Type_Decl_P_Is_Int_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is an integer type or not.

   function Base_Type_Decl_P_Is_Str_Type_Or_Null

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Base_Type_Decl_P_Accessed_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If this type is an access type, or a type with an Implicit_Dereference
--  aspect, return the type of a dereference of an instance of this type.

   function Base_Type_Decl_P_Final_Accessed_Type

     (Node   : Bare_Base_Type_Decl; First_Call : Boolean := True;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Call accessed_type recursively until we get the most nested accessed type.
--  For example, for the following code::
--
--  type A is access Integer; type AA is access A; type AAA is access AA;
--
--  ``AAA``'s final_accessed_type is Integer.

   function Base_Type_Decl_P_Is_Access_To

     (Node   : Bare_Base_Type_Decl; Typ : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Base_Type_Decl_P_Is_Subp_Access_Of

     (Node   : Bare_Base_Type_Decl; Entity : Internal_Entity_Basic_Decl;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Returns whether self is an access type whose accessed type matches other.

   function Base_Type_Decl_P_Is_Generic_Formal

     (Node : Bare_Base_Type_Decl)
return Boolean;
--  Return whether this type declaration is a generic formal.

   function Dispatcher_Base_Type_Decl_P_Is_Tagged_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is tagged or not

   function Dispatcher_Base_Type_Decl_P_Base_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the base type entity for this derived type declaration

   function Base_Type_Decl_P_Base_Types

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Return the list of base types for Self.

   function Dispatcher_Base_Type_Decl_P_Base_Interfaces

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Base_Type_Decl_P_Find_All_Derived_Types

     (Node : Bare_Base_Type_Decl; Units : Internal_Unit_Array_Access;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Decl_Array_Access;
--  Return the list of all types that inherit (directly or inderictly) from
--  Self among the given units.

   function Dispatcher_Base_Type_Decl_P_Record_Def

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Record_Def;

   function Dispatcher_Base_Type_Decl_P_Array_Def

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Type_Def;

   function Base_Type_Decl_P_Array_Def_With_Deref

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Type_Def;
--  Return the array definition corresponding to type `Self` in the context of
--  array-indexing, e.g. implicitly dereferencing if `Self` is an access.

   function Base_Type_Decl_P_Is_Array_Def_With_Deref

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Type_Decl_P_Comp_Type

     (Node   : Bare_Base_Type_Decl; Is_Subscript : Boolean := False;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the component type of `Self`, if applicable. The component type is
--  the type you'll get if you call a value whose type is `Self`. So it can
--  either be:
--
--  1. The component type for an array. 2. The return type for an access to
--  function.

   function Base_Type_Decl_P_Index_Type

     (Node   : Bare_Base_Type_Decl; Dim : Integer;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the index type for dimension ``dim`` for this type, if applicable.

   function Base_Type_Decl_P_Expr_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Type_Decl_P_Is_Derived_Type

     (Node : Bare_Base_Type_Decl; Other_Type : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is derived from other_type.

   function Dispatcher_Base_Type_Decl_P_Is_Iterable_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is a type that is iterable in a for .. of loop

   function Base_Type_Decl_P_Is_Interface_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return True iff this type declaration is an interface definition.

   function Dispatcher_Base_Type_Decl_P_Iterable_Comp_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Type_Decl_P_Matching_Prefix_Type

     (Node           : Bare_Base_Type_Decl;
      Container_Type : Internal_Entity_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info         : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Given a dotted expression A.B, where container_type is the container type
--  for B, and Self is a potential type for A, returns whether Self is a valid
--  type for A in the dotted expression.

   function Base_Type_Decl_P_Matching_Access_Type

     (Node          : Bare_Base_Type_Decl;
      Expected_Type : Internal_Entity_Base_Type_Decl; For_Assignment : Boolean;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Whether self is a matching access type for expected_type.

   function Base_Type_Decl_P_Matching_Formal_Prim_Type

     (Node : Bare_Base_Type_Decl; Formal_Type : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Base_Type_Decl_P_Matching_Formal_Type_Inverted

     (Node : Bare_Base_Type_Decl; Formal_Type : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Base_Type_Decl_P_Matching_Formal_Type

     (Node : Bare_Base_Type_Decl; Formal_Type : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Base_Type_Decl_P_Matching_Formal_Type_Impl

     (Node : Bare_Base_Type_Decl; Formal_Type : Internal_Entity_Base_Type_Decl;
      Accept_Derived : Boolean              := False; Origin : Bare_Ada_Node;
      E_Info         : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Type_Decl_P_Matching_Assign_Type

     (Node          : Bare_Base_Type_Decl;
      Expected_Type : Internal_Entity_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info        : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Type_Decl_P_Matching_Type

     (Node          : Bare_Base_Type_Decl;
      Expected_Type : Internal_Entity_Base_Type_Decl;
      Origin        : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info        : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether ``self`` matches ``expected_type``.

   function Base_Type_Decl_P_Matching_Allocator_Type

     (Node           : Bare_Base_Type_Decl;
      Allocated_Type : Internal_Entity_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info         : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Base_Type_Decl_P_Canonical_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the canonical type declaration for this type declaration. For
--  subtypes, it will return the base type declaration.

   function Base_Type_Decl_P_Base_Subtype_Or_Null

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Type_Decl_P_Classwide_Type_Node

     (Node : Bare_Base_Type_Decl)
return Bare_Classwide_Type_Decl;

   function Base_Type_Decl_P_Scalar_Base_Subtype_Node

     (Node : Bare_Base_Type_Decl)
return Bare_Discrete_Base_Subtype_Decl;
--  Helper for scalar_base_subtype. Return the interned node for the subtype
--  entity.

   function Base_Type_Decl_P_Scalar_Base_Subtype

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Discrete_Base_Subtype_Decl;
--  Return the base subtype for this type. Note that this is only legal for
--  scalar types.

   function Dispatcher_Base_Type_Decl_P_Previous_Part

     (Node   : Bare_Base_Type_Decl; Go_To_Incomplete : Boolean := True;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Returns the previous part for this type decl.

   function Base_Type_Decl_P_Next_Part

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Returns the next part for this type decl.

   function Base_Type_Decl_P_Full_View

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the full completion of this type.

   function Base_Type_Decl_P_Is_Definite_Subtype

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns whether this is a definite subtype.
--
--  For convenience, this will return ``False`` for incomplete types, even
--  though the correct answer is more akin to "non applicable".

   function Dispatcher_Base_Type_Decl_P_Is_Private

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether node is a private view of corresponding type.

   function Dispatcher_Base_Type_Decl_P_Discriminants_List

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Dispatcher_Base_Type_Decl_P_Root_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type that is at the root of the derivation hierarchy (ignoring
--  secondary interfaces derivations for tagged types)

   function Base_Type_Decl_P_Next_Part_For_Decl

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Env_Mappings_2

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Base_Type_Decl_P_Primitives_Env

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Base_Type_Decl_P_Is_Record_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is a record type.

   function Base_Type_Decl_P_Is_Task_Type

     (Node : Bare_Base_Type_Decl)
return Boolean;
--  Whether type is a task type

   function Base_Type_Decl_P_Is_Real_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a real type or not.

   function Base_Type_Decl_P_Is_Float_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a float type or not.

   function Base_Type_Decl_P_Is_Fixed_Point

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a fixed point type or not.

   function Base_Type_Decl_P_Is_Enum_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is an enum type

   function Base_Type_Decl_P_Is_Classwide

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Type_Decl_P_Is_Access_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is an access type or not

   function Base_Type_Decl_P_Has_Ud_Indexing

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether self has user defined indexing or not

   function Base_Type_Decl_P_Constant_Indexing_Fns

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  For a type with user defined indexing, return the set of all
--  Constant_Indexing functions.

   function Base_Type_Decl_P_Variable_Indexing_Fns

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  For a type with user defined indexing, return the set of all
--  Variable_Indexing functions.

   function Base_Type_Decl_P_Get_Imp_Deref

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;
--  If self has an Implicit_Dereference aspect, return its expression

   function Base_Type_Decl_P_Access_Def

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Access_Def;

   function Base_Type_Decl_P_Is_Char_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a character type or not

   function Base_Type_Decl_P_Classwide_Type

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Classwide_Type_Decl;

   function Base_Type_Decl_P_Discrete_Range

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;
--  Return the discrete range for this type decl, if applicable.

   function Base_Type_Decl_P_Is_Discrete_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a discrete type or not.

   function Base_Type_Decl_P_Is_Int_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is an integer type or not.

   function Base_Type_Decl_P_Accessed_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If this type is an access type, or a type with an Implicit_Dereference
--  aspect, return the type of a dereference of an instance of this type.

   function Base_Type_Decl_P_Is_Tagged_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is tagged or not

   function Base_Type_Decl_P_Base_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the base type entity for this derived type declaration

   function Base_Type_Decl_P_Base_Interfaces

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Base_Type_Decl_P_Record_Def

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Record_Def;

   function Base_Type_Decl_P_Array_Def

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Type_Def;

   function Base_Type_Decl_P_Is_Iterable_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is a type that is iterable in a for .. of loop

   function Base_Type_Decl_P_Iterable_Comp_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Type_Decl_P_Canonical_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the canonical type declaration for this type declaration. For
--  subtypes, it will return the base type declaration.

   function Base_Type_Decl_P_Previous_Part

     (Node   : Bare_Base_Type_Decl; Go_To_Incomplete : Boolean := True;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Returns the previous part for this type decl.

   function Base_Type_Decl_P_Is_Private

     (Node   : Bare_Base_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether node is a private view of corresponding type.

   function Base_Type_Decl_P_Root_Type

     (Node   : Bare_Base_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type that is at the root of the derivation hierarchy (ignoring
--  secondary interfaces derivations for tagged types)

   function Bare_Base_Type_Decl_Pre_Env_Actions
     (Self : Bare_Base_Type_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Base_Subtype_Decl
     (Self                  : Bare_Base_Subtype_Decl;
      Base_Type_Decl_F_Name : Bare_Defining_Name);

   function Base_Subtype_Decl_P_From_Type_Bound

     (Node   : Bare_Base_Subtype_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Dispatcher_Base_Subtype_Decl_P_From_Type

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Subtype_Decl_P_Primitives_Env

     (Node   : Bare_Base_Subtype_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Base_Subtype_Decl_P_Array_Ndims

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Base_Subtype_Decl_P_Defining_Env

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Base_Subtype_Decl_P_Canonical_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Subtype_Decl_P_Record_Def

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Record_Def;

   function Base_Subtype_Decl_P_Accessed_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Subtype_Decl_P_Is_Int_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Discrete_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Real_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Float_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Fixed_Point

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Enum_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Access_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Access_Def

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Access_Def;

   function Base_Subtype_Decl_P_Is_Char_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Tagged_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Base_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Subtype_Decl_P_Array_Def

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Type_Def;

   function Base_Subtype_Decl_P_Is_Classwide

     (Node   : Bare_Base_Subtype_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Discriminants_List

     (Node   : Bare_Base_Subtype_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Base_Subtype_Decl_P_Is_Iterable_Type

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Iterable_Comp_Type

     (Node   : Bare_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Subtype_Decl_P_Is_Record_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Is_Private

     (Node   : Bare_Base_Subtype_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Base_Subtype_Decl_P_Root_Type

     (Node   : Bare_Base_Subtype_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   procedure Initialize_Fields_For_Discrete_Base_Subtype_Decl
     (Self                  : Bare_Discrete_Base_Subtype_Decl;
      Base_Type_Decl_F_Name : Bare_Defining_Name);

   function Discrete_Base_Subtype_Decl_P_Is_Static_Decl

     (Node               : Bare_Discrete_Base_Subtype_Decl;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Discrete_Base_Subtype_Decl_P_From_Type

     (Node   : Bare_Discrete_Base_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   procedure Initialize_Fields_For_Subtype_Decl
     (Self : Bare_Subtype_Decl; Base_Type_Decl_F_Name : Bare_Defining_Name;
      Subtype_Decl_F_Subtype : Bare_Subtype_Indication;
      Subtype_Decl_F_Aspects : Bare_Aspect_Spec);

   function Subtype_Decl_F_Subtype
     (Node : Bare_Subtype_Decl) return Bare_Subtype_Indication;

   function Subtype_Decl_P_From_Type

     (Node   : Bare_Subtype_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Subtype_Decl_P_Discrete_Range

     (Node   : Bare_Subtype_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;

   function Subtype_Decl_P_Xref_Equation

     (Node   : Bare_Subtype_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Subtype_Decl_P_Is_Static_Decl

     (Node   : Bare_Subtype_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Subtype_Decl_P_Xref_Entry_Point

     (Node : Bare_Subtype_Decl)
return Boolean;

   procedure Initialize_Fields_For_Classwide_Type_Decl
     (Self                  : Bare_Classwide_Type_Decl;
      Base_Type_Decl_F_Name : Bare_Defining_Name);

   function Classwide_Type_Decl_P_Typedecl

     (Node   : Bare_Classwide_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Classwide_Type_Decl_P_Is_Classwide

     (Node   : Bare_Classwide_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Classwide_Type_Decl_P_Is_Tagged_Type

     (Node   : Bare_Classwide_Type_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Classwide_Type_Decl_P_Base_Type

     (Node   : Bare_Classwide_Type_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Classwide_Type_Decl_P_Base_Interfaces

     (Node   : Bare_Classwide_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Classwide_Type_Decl_P_Record_Def

     (Node   : Bare_Classwide_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Record_Def;

   function Classwide_Type_Decl_P_Classwide_Type

     (Node   : Bare_Classwide_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Classwide_Type_Decl;

   function Classwide_Type_Decl_P_Is_Iterable_Type

     (Node   : Bare_Classwide_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Classwide_Type_Decl_P_Iterable_Comp_Type

     (Node   : Bare_Classwide_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Classwide_Type_Decl_P_Defining_Env

     (Node   : Bare_Classwide_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Classwide_Type_Decl_P_Is_Private

     (Node   : Bare_Classwide_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Classwide_Type_Decl_P_Is_In_Private_Part

     (Node   : Bare_Classwide_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Classwide_Type_Decl_P_Get_Aspect_Assoc

     (Node   : Bare_Classwide_Type_Decl; Name : Symbol_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Aspect_Assoc;

   function Classwide_Type_Decl_P_Discriminants_List

     (Node   : Bare_Classwide_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Classwide_Type_Decl_P_Previous_Part

     (Node   : Bare_Classwide_Type_Decl; Go_To_Incomplete : Boolean := True;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Classwide_Type_Decl_P_Canonical_Type

     (Node   : Bare_Classwide_Type_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Bare_Classwide_Type_Decl_Pre_Env_Actions
     (Self                : Bare_Classwide_Type_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Incomplete_Type_Decl
     (Self                                 : Bare_Incomplete_Type_Decl;
      Base_Type_Decl_F_Name                : Bare_Defining_Name;
      Incomplete_Type_Decl_F_Discriminants : Bare_Discriminant_Part);

   function Incomplete_Type_Decl_F_Discriminants
     (Node : Bare_Incomplete_Type_Decl) return Bare_Discriminant_Part;

   function Incomplete_Type_Decl_P_Find_Next_Part_In

     (Node      : Bare_Incomplete_Type_Decl;
      Decl_Part : Internal_Entity_Declarative_Part;
      E_Info    : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Searches for the next part of Self inside the given declarative part. Since
--  Self is an IncompleteTypeDecl, the next part will necessarily be the first
--  type declaration of the same name that is not Self.

   function Incomplete_Type_Decl_P_Defining_Env

     (Node   : Bare_Incomplete_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Incomplete_Type_Decl_P_Discriminants_List

     (Node   : Bare_Incomplete_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Env_Mappings_185

     (Node   : Bare_Incomplete_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_186

     (Node : Bare_Incomplete_Type_Decl)
return Boolean;

   function Bare_Incomplete_Type_Decl_Pre_Env_Actions
     (Self                : Bare_Incomplete_Type_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Incomplete_Tagged_Type_Decl
     (Self : Bare_Incomplete_Tagged_Type_Decl;
      Base_Type_Decl_F_Name                      : Bare_Defining_Name;
      Incomplete_Type_Decl_F_Discriminants       : Bare_Discriminant_Part;
      Incomplete_Tagged_Type_Decl_F_Has_Abstract : Bare_Abstract_Node);

   function Incomplete_Tagged_Type_Decl_F_Has_Abstract
     (Node : Bare_Incomplete_Tagged_Type_Decl) return Bare_Abstract_Node;

   function Incomplete_Tagged_Type_Decl_P_Is_Tagged_Type

     (Node   : Bare_Incomplete_Tagged_Type_Decl;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Protected_Type_Decl
     (Self                                : Bare_Protected_Type_Decl;
      Base_Type_Decl_F_Name               : Bare_Defining_Name;
      Protected_Type_Decl_F_Discriminants : Bare_Discriminant_Part;
      Protected_Type_Decl_F_Aspects       : Bare_Aspect_Spec;
      Protected_Type_Decl_F_Interfaces    : Bare_Parent_List;
      Protected_Type_Decl_F_Definition    : Bare_Protected_Def);

   function Protected_Type_Decl_F_Discriminants
     (Node : Bare_Protected_Type_Decl) return Bare_Discriminant_Part;

   function Protected_Type_Decl_F_Interfaces
     (Node : Bare_Protected_Type_Decl) return Bare_Parent_List;

   function Protected_Type_Decl_F_Definition
     (Node : Bare_Protected_Type_Decl) return Bare_Protected_Def;

   function Protected_Type_Decl_P_Discriminants_List

     (Node   : Bare_Protected_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Protected_Type_Decl_P_Defining_Env

     (Node   : Bare_Protected_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Protected_Type_Decl_P_Next_Part_For_Decl

     (Node   : Bare_Protected_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Env_Mappings_11

     (Node   : Bare_Protected_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_12

     (Node : Bare_Protected_Type_Decl)
return Boolean;

   function Bare_Protected_Type_Decl_Pre_Env_Actions
     (Self                : Bare_Protected_Type_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Task_Type_Decl
     (Self : Bare_Task_Type_Decl; Base_Type_Decl_F_Name : Bare_Defining_Name;
      Task_Type_Decl_F_Discriminants : Bare_Discriminant_Part;
      Task_Type_Decl_F_Aspects       : Bare_Aspect_Spec;
      Task_Type_Decl_F_Definition    : Bare_Task_Def);

   function Task_Type_Decl_F_Discriminants
     (Node : Bare_Task_Type_Decl) return Bare_Discriminant_Part;

   function Task_Type_Decl_F_Definition
     (Node : Bare_Task_Type_Decl) return Bare_Task_Def;

   function Task_Type_Decl_P_Is_Task_Type

     (Node : Bare_Task_Type_Decl)
return Boolean;

   function Task_Type_Decl_P_Defining_Env

     (Node   : Bare_Task_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Task_Type_Decl_P_Discriminants_List

     (Node   : Bare_Task_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Env_Mappings_8

     (Node   : Bare_Task_Type_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_9
(Node : Bare_Task_Type_Decl)
return Boolean;

   function Bare_Task_Type_Decl_Pre_Env_Actions
     (Self : Bare_Task_Type_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Single_Task_Type_Decl
     (Self                           : Bare_Single_Task_Type_Decl;
      Base_Type_Decl_F_Name          : Bare_Defining_Name;
      Task_Type_Decl_F_Discriminants : Bare_Discriminant_Part;
      Task_Type_Decl_F_Aspects       : Bare_Aspect_Spec;
      Task_Type_Decl_F_Definition    : Bare_Task_Def);

   function Env_Trans_Parent_10

     (Node : Bare_Single_Task_Type_Decl)
return Boolean;

   function Bare_Single_Task_Type_Decl_Pre_Env_Actions
     (Self                : Bare_Single_Task_Type_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Type_Decl
     (Self : Bare_Type_Decl; Base_Type_Decl_F_Name : Bare_Defining_Name;
      Type_Decl_F_Discriminants : Bare_Discriminant_Part;
      Type_Decl_F_Type_Def      : Bare_Type_Def;
      Type_Decl_F_Aspects       : Bare_Aspect_Spec);

   function Type_Decl_F_Discriminants
     (Node : Bare_Type_Decl) return Bare_Discriminant_Part;

   function Type_Decl_F_Type_Def (Node : Bare_Type_Decl) return Bare_Type_Def;

   function Type_Decl_P_Is_Iterable_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether Self is a type that is iterable in a for .. of loop

   function Type_Decl_P_Iterable_Comp_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Decl_P_Discrete_Range

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;

   function Type_Decl_P_Discriminants_List

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;

   function Type_Decl_P_Array_Ndims

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Type_Decl_P_Is_Record_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Real_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Float_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Fixed_Point

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Int_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Access_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Static_Decl

     (Node   : Bare_Type_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Accessed_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Decl_P_Access_Def

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Access_Def;

   function Type_Decl_P_Is_Tagged_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Base_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Decl_P_Base_Interfaces

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Type_Decl_P_Is_Char_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Enum_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Is_Private

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Type_Decl_P_Array_Def

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Type_Def;

   function Type_Decl_P_Root_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Decl_P_Defining_Env

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Type_Decl_P_Record_Def

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Record_Def;

   function Type_Decl_P_Xref_Entry_Point

     (Node : Bare_Type_Decl)
return Boolean;

   function Type_Decl_P_Xref_Equation

     (Node   : Bare_Type_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Type_Decl_P_Is_Discrete_Type

     (Node   : Bare_Type_Decl; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Decl_P_Own_Primitives_Env

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;
--  Return the environment containing the primitives for Self.

   function Type_Decl_P_Own_Primitives_Envs

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env_Array_Access;
--  Return the environments containing the primitives for Self and its previous
--  parts, if there are some.

   function Type_Decl_P_Primitives_Envs

     (Node   : Bare_Type_Decl; Include_Self : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env_Array_Access;
--  Return the environments containing the primitives for Self and all its base
--  types.

   function Type_Decl_P_Primitive_Type_Accessor

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Bare_Prim_Type_Accessor;
--  Return a synthetic node that wraps around this type as an entity. This
--  works around the fact that we cannot store an entity in the entity info,
--  allowing us to access the full primitive_real_type.

   function Type_Decl_P_Compute_Primitives_Env

     (Node   : Bare_Type_Decl; Include_Self : Boolean := True;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Return a environment containing all primitives accessible to Self, with the
--  adjusted `primitive_real_type` metadata field.

   function Type_Decl_P_Parent_Primitives_Env

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Type_Decl_P_Primitives_Env

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Type_Decl_P_Get_Primitives

     (Node   : Bare_Type_Decl; Only_Inherited : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  Return the list of all primitive operations that are available on this
--  type. If `only_inherited` is True, it will only return the primitives that
--  are implicitly inherited by this type, discarding those explicitly defined
--  on this type.

   function Type_Decl_P_Get_Imp_Deref

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Expr;

   function Type_Decl_P_Has_Ud_Indexing

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;

   function Type_Decl_P_Constant_Indexing_Fns

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;

   function Type_Decl_P_Variable_Indexing_Fns

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;

   function Env_Mappings_3

     (Node : Bare_Type_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_4
(Node : Bare_Type_Decl)
return Boolean;

   function Ref_Env_Nodes_5

     (Node : Bare_Type_Decl)
return Bare_Ada_Node_Array_Access;

   function Env_Dest_6
(Node : Bare_Type_Decl)
return Lexical_Env;

   function Ref_Cond_7
(Node : Bare_Type_Decl)
return Boolean;

   function Bare_Type_Decl_Pre_Env_Actions
     (Self : Bare_Type_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Bare_Type_Decl_Post_Env_Actions
     (Self : Bare_Type_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env);

   procedure Initialize_Fields_For_Anonymous_Type_Decl
     (Self                      : Bare_Anonymous_Type_Decl;
      Base_Type_Decl_F_Name     : Bare_Defining_Name;
      Type_Decl_F_Discriminants : Bare_Discriminant_Part;
      Type_Decl_F_Type_Def      : Bare_Type_Def;
      Type_Decl_F_Aspects       : Bare_Aspect_Spec);

   function Anonymous_Type_Decl_P_Access_Def_Matches

     (Node : Bare_Anonymous_Type_Decl; Other : Internal_Entity_Base_Type_Decl;
      For_Assignment : Boolean; Origin : Bare_Ada_Node;
      E_Info         : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns whether: 1. Self and other are both access types. 2. Their access
--  def matches structurally. If for_assignment is True, matching_assign_type
--  is used instead of matching_type to compare the two access defs.

   function Anonymous_Type_Decl_P_Xref_Entry_Point

     (Node : Bare_Anonymous_Type_Decl)
return Boolean;

   function Bare_Anonymous_Type_Decl_Pre_Env_Actions
     (Self                : Bare_Anonymous_Type_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Synth_Anonymous_Type_Decl
     (Self                      : Bare_Synth_Anonymous_Type_Decl;
      Base_Type_Decl_F_Name     : Bare_Defining_Name;
      Type_Decl_F_Discriminants : Bare_Discriminant_Part;
      Type_Decl_F_Type_Def      : Bare_Type_Def;
      Type_Decl_F_Aspects       : Bare_Aspect_Spec);

   function Basic_Subp_Decl_P_Defining_Names

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Basic_Subp_Decl_P_Defining_Env

     (Node   : Bare_Basic_Subp_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Basic_Subp_Decl_P_Type_Expression

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;
--  The expr type of a subprogram declaration is the return type of the
--  subprogram if the subprogram is a function.

   function Basic_Subp_Decl_P_Get_Body_In_Env

     (Node   : Bare_Basic_Subp_Decl; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Body_Node;

   function Basic_Subp_Decl_P_Next_Part_For_Decl

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Basic_Subp_Decl_P_Constrain_Prefix

     (Node : Bare_Basic_Subp_Decl; Prefix : Bare_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Basic_Subp_Decl_P_Expr_Type

     (Node   : Bare_Basic_Subp_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Dispatcher_Basic_Subp_Decl_P_Subp_Decl_Spec

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;
--  Return the specification for this subprogram

   function Basic_Subp_Decl_P_Body_Part

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Body;
--  Return the BaseSubpBody corresponding to this node.

   function Env_Do_20
(Node : Bare_Basic_Subp_Decl)
return Boolean;

   function Initial_Env_21

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_22

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_23
(Node : Bare_Basic_Subp_Decl)
return Boolean;

   function Env_Do_24

     (Node : Bare_Basic_Subp_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_25

     (Node : Bare_Basic_Subp_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_26
(Node : Bare_Basic_Subp_Decl)
return Boolean;

   function Env_Mappings_27

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Env_Mappings_28

     (Node   : Bare_Basic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Bare_Basic_Subp_Decl_Pre_Env_Actions
     (Self : Bare_Basic_Subp_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Bare_Basic_Subp_Decl_Post_Env_Actions
     (Self : Bare_Basic_Subp_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env);

   procedure Initialize_Fields_For_Classic_Subp_Decl
     (Self                           : Bare_Classic_Subp_Decl;
      Classic_Subp_Decl_F_Overriding : Bare_Overriding_Node;
      Classic_Subp_Decl_F_Subp_Spec  : Bare_Subp_Spec);

   function Classic_Subp_Decl_F_Overriding
     (Node : Bare_Classic_Subp_Decl) return Bare_Overriding_Node;

   function Classic_Subp_Decl_F_Subp_Spec
     (Node : Bare_Classic_Subp_Decl) return Bare_Subp_Spec;

   function Classic_Subp_Decl_P_Subp_Decl_Spec

     (Node   : Bare_Classic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;

   procedure Initialize_Fields_For_Abstract_Subp_Decl
     (Self                           : Bare_Abstract_Subp_Decl;
      Classic_Subp_Decl_F_Overriding : Bare_Overriding_Node;
      Classic_Subp_Decl_F_Subp_Spec  : Bare_Subp_Spec;
      Abstract_Subp_Decl_F_Aspects   : Bare_Aspect_Spec);

   procedure Initialize_Fields_For_Formal_Subp_Decl
     (Self                            : Bare_Formal_Subp_Decl;
      Classic_Subp_Decl_F_Overriding  : Bare_Overriding_Node;
      Classic_Subp_Decl_F_Subp_Spec   : Bare_Subp_Spec;
      Formal_Subp_Decl_F_Default_Expr : Bare_Expr;
      Formal_Subp_Decl_F_Aspects      : Bare_Aspect_Spec);

   function Formal_Subp_Decl_F_Default_Expr
     (Node : Bare_Formal_Subp_Decl) return Bare_Expr;

   function Formal_Subp_Decl_P_Defining_Names

     (Node   : Bare_Formal_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Formal_Subp_Decl_P_Xref_Equation

     (Node : Bare_Formal_Subp_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Abstract_Formal_Subp_Decl
     (Self                            : Bare_Abstract_Formal_Subp_Decl;
      Classic_Subp_Decl_F_Overriding  : Bare_Overriding_Node;
      Classic_Subp_Decl_F_Subp_Spec   : Bare_Subp_Spec;
      Formal_Subp_Decl_F_Default_Expr : Bare_Expr;
      Formal_Subp_Decl_F_Aspects      : Bare_Aspect_Spec);

   procedure Initialize_Fields_For_Concrete_Formal_Subp_Decl
     (Self                            : Bare_Concrete_Formal_Subp_Decl;
      Classic_Subp_Decl_F_Overriding  : Bare_Overriding_Node;
      Classic_Subp_Decl_F_Subp_Spec   : Bare_Subp_Spec;
      Formal_Subp_Decl_F_Default_Expr : Bare_Expr;
      Formal_Subp_Decl_F_Aspects      : Bare_Aspect_Spec);

   procedure Initialize_Fields_For_Subp_Decl
     (Self                           : Bare_Subp_Decl;
      Classic_Subp_Decl_F_Overriding : Bare_Overriding_Node;
      Classic_Subp_Decl_F_Subp_Spec  : Bare_Subp_Spec;
      Subp_Decl_F_Aspects            : Bare_Aspect_Spec);

   procedure Initialize_Fields_For_Entry_Decl
     (Self : Bare_Entry_Decl; Entry_Decl_F_Overriding : Bare_Overriding_Node;
      Entry_Decl_F_Spec    : Bare_Entry_Spec;
      Entry_Decl_F_Aspects : Bare_Aspect_Spec);

   function Entry_Decl_F_Overriding
     (Node : Bare_Entry_Decl) return Bare_Overriding_Node;

   function Entry_Decl_F_Spec (Node : Bare_Entry_Decl) return Bare_Entry_Spec;

   function Entry_Decl_P_Subp_Decl_Spec

     (Node : Bare_Entry_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;

   function Entry_Decl_P_Defining_Names

     (Node : Bare_Entry_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Mappings_110

     (Node : Bare_Entry_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_111
(Node : Bare_Entry_Decl)
return Boolean;

   function Bare_Entry_Decl_Pre_Env_Actions
     (Self : Bare_Entry_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Enum_Literal_Decl
     (Self                     : Bare_Enum_Literal_Decl;
      Enum_Literal_Decl_F_Name : Bare_Defining_Name);

   function Enum_Literal_Decl_F_Name
     (Node : Bare_Enum_Literal_Decl) return Bare_Defining_Name;

   function Enum_Literal_Decl_P_Is_Static_Decl

     (Node   : Bare_Enum_Literal_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Enum_Literal_Decl_P_Enum_Type

     (Node   : Bare_Enum_Literal_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Decl;
--  Return the enum type corresponding to this enum literal.

   function Enum_Literal_Decl_P_Defining_Names

     (Node   : Bare_Enum_Literal_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Enum_Literal_Decl_P_Synth_Type_Expr

     (Node   : Bare_Enum_Literal_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Enum_Lit_Synth_Type_Expr;

   function Enum_Literal_Decl_P_Subp_Decl_Spec

     (Node   : Bare_Enum_Literal_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;

   function Env_Mappings_108

     (Node   : Bare_Enum_Literal_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Mappings_109

     (Node   : Bare_Enum_Literal_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Bare_Enum_Literal_Decl_Pre_Env_Actions
     (Self                : Bare_Enum_Literal_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Generic_Subp_Internal
     (Self                              : Bare_Generic_Subp_Internal;
      Generic_Subp_Internal_F_Subp_Spec : Bare_Subp_Spec;
      Generic_Subp_Internal_F_Aspects   : Bare_Aspect_Spec);

   function Generic_Subp_Internal_F_Subp_Spec
     (Node : Bare_Generic_Subp_Internal) return Bare_Subp_Spec;

   function Generic_Subp_Internal_P_Subp_Decl_Spec

     (Node   : Bare_Generic_Subp_Internal;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Spec;

   function Env_Trans_Parent_90

     (Node : Bare_Generic_Subp_Internal)
return Boolean;

   function Bare_Generic_Subp_Internal_Pre_Env_Actions
     (Self                : Bare_Generic_Subp_Internal;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   function Body_Node_P_Env_Hook_Body
(Node : Bare_Body_Node)
return Boolean;
--  Helper for the AdaNode.env_hook. Handle library-level unit body nodes.

   function Body_Node_P_Subunit_Stub_Env

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Body_Node_P_Subunit_Decl_Env

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Body_Node_P_In_Scope

     (Node : Bare_Body_Node; Origin : Bare_Ada_Node)
return Boolean;
--  Return True if ``origin`` is directly in the scope of this body.

   function Body_Node_P_Body_Decl_Scope

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;
--  Return the scope of this body's decl.

   function Body_Node_P_Subp_Previous_Part

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the decl corresponding to this body. Specialized implementation for
--  subprogram bodies.

   function Body_Node_P_Package_Previous_Part

     (Node   : Bare_Body_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the BasePackageDecl corresponding to this node.
--
--  If the case of generic package declarations, this returns the
--  ``package_decl`` field instead of the ``GenericPackageDecl`` itself.

   function Body_Node_P_Task_Previous_Part

     (Node   : Bare_Body_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the task decl corresponding to this node.

   function Body_Node_P_Protected_Previous_Part

     (Node   : Bare_Body_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the ProtectedDecl corresponding to this node.

   function Body_Node_P_Unbound_Previous_Part

     (Node   : Bare_Body_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the previous part for this body. Might be a declaration or a body
--  stub.

   function Body_Node_P_Stub_Decl_Env

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Body_Node_P_Previous_Part

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the previous part for this body. Might be a declaration or a body
--  stub.

   function Body_Node_P_Decl_Part

     (Node : Bare_Body_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the decl corresponding to this node if applicable.

   function Body_Node_P_Is_Subunit
(Node : Bare_Body_Node)
return Boolean;

   function Body_Node_P_Subunit_Root

     (Node : Bare_Body_Node)
return Internal_Entity_Basic_Decl;
--  If self is a subunit, return the body in which it is rooted.

   function Body_Node_P_Body_Scope

     (Node       : Bare_Body_Node; Follow_Private : Boolean;
      Force_Decl : Boolean              := False; Env : Lexical_Env;
      E_Info     : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Return the scope for this body. If follow_private, then returns the private
--  part if possible.
--
--  If force_decl, then returns the corresponding declaration's scope, rather
--  than the parent body's scope.

   procedure Initialize_Fields_For_Base_Subp_Body
     (Self                        : Bare_Base_Subp_Body;
      Base_Subp_Body_F_Overriding : Bare_Overriding_Node;
      Base_Subp_Body_F_Subp_Spec  : Bare_Subp_Spec);

   function Base_Subp_Body_F_Overriding
     (Node : Bare_Base_Subp_Body) return Bare_Overriding_Node;

   function Base_Subp_Body_F_Subp_Spec
     (Node : Bare_Base_Subp_Body) return Bare_Subp_Spec;

   function Base_Subp_Body_P_Defining_Names

     (Node   : Bare_Base_Subp_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Base_Subp_Body_P_Defining_Env

     (Node   : Bare_Base_Subp_Body; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Base_Subp_Body_P_Type_Expression

     (Node   : Bare_Base_Subp_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Base_Subp_Body_P_Expr_Type

     (Node   : Bare_Base_Subp_Body; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Env_Do_114
(Node : Bare_Base_Subp_Body)
return Boolean;

   function Initial_Env_115

     (Node   : Bare_Base_Subp_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_116

     (Node   : Bare_Base_Subp_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Env_Trans_Parent_117
(Node : Bare_Base_Subp_Body)
return Boolean;

   function Env_Do_118

     (Node : Bare_Base_Subp_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_119

     (Node : Bare_Base_Subp_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_120
(Node : Bare_Base_Subp_Body)
return Boolean;

   function Ref_Env_Nodes_121

     (Node : Bare_Base_Subp_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_122
(Node : Bare_Base_Subp_Body)
return Boolean;

   function Ref_Env_Nodes_123

     (Node : Bare_Base_Subp_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_124
(Node : Bare_Base_Subp_Body)
return Boolean;

   function Env_Mappings_125

     (Node   : Bare_Base_Subp_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Env_Mappings_126

     (Node   : Bare_Base_Subp_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Bare_Base_Subp_Body_Pre_Env_Actions
     (Self : Bare_Base_Subp_Body; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Bare_Base_Subp_Body_Post_Env_Actions
     (Self : Bare_Base_Subp_Body; Bound_Env, Root_Env : AST_Envs.Lexical_Env);

   procedure Initialize_Fields_For_Expr_Function
     (Self                        : Bare_Expr_Function;
      Base_Subp_Body_F_Overriding : Bare_Overriding_Node;
      Base_Subp_Body_F_Subp_Spec  : Bare_Subp_Spec;
      Expr_Function_F_Expr        : Bare_Expr;
      Expr_Function_F_Aspects     : Bare_Aspect_Spec);

   function Expr_Function_F_Expr (Node : Bare_Expr_Function) return Bare_Expr;

   function Expr_Function_P_Xref_Equation

     (Node   : Bare_Expr_Function; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Expr_Function_P_Xref_Entry_Point

     (Node : Bare_Expr_Function)
return Boolean;

   procedure Initialize_Fields_For_Null_Subp_Decl
     (Self                        : Bare_Null_Subp_Decl;
      Base_Subp_Body_F_Overriding : Bare_Overriding_Node;
      Base_Subp_Body_F_Subp_Spec  : Bare_Subp_Spec;
      Null_Subp_Decl_F_Aspects    : Bare_Aspect_Spec);

   procedure Initialize_Fields_For_Subp_Body
     (Self                        : Bare_Subp_Body;
      Base_Subp_Body_F_Overriding : Bare_Overriding_Node;
      Base_Subp_Body_F_Subp_Spec  : Bare_Subp_Spec;
      Subp_Body_F_Aspects         : Bare_Aspect_Spec;
      Subp_Body_F_Decls           : Bare_Declarative_Part;
      Subp_Body_F_Stmts           : Bare_Handled_Stmts;
      Subp_Body_F_End_Name        : Bare_End_Name);

   function Subp_Body_F_Decls
     (Node : Bare_Subp_Body) return Bare_Declarative_Part;

   function Subp_Body_F_Stmts
     (Node : Bare_Subp_Body) return Bare_Handled_Stmts;

   function Subp_Body_F_End_Name (Node : Bare_Subp_Body) return Bare_End_Name;

   function Subp_Body_P_Declarative_Region

     (Node : Bare_Subp_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Declarative_Part;

   procedure Initialize_Fields_For_Subp_Renaming_Decl
     (Self                         : Bare_Subp_Renaming_Decl;
      Base_Subp_Body_F_Overriding  : Bare_Overriding_Node;
      Base_Subp_Body_F_Subp_Spec   : Bare_Subp_Spec;
      Subp_Renaming_Decl_F_Renames : Bare_Renaming_Clause;
      Subp_Renaming_Decl_F_Aspects : Bare_Aspect_Spec);

   function Subp_Renaming_Decl_F_Renames
     (Node : Bare_Subp_Renaming_Decl) return Bare_Renaming_Clause;

   function Subp_Renaming_Decl_P_Xref_Entry_Point

     (Node : Bare_Subp_Renaming_Decl)
return Boolean;

   function Subp_Renaming_Decl_P_Xref_Equation

     (Node   : Bare_Subp_Renaming_Decl; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Body_Stub_P_Next_Part_For_Decl

     (Node : Bare_Body_Stub; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   procedure Initialize_Fields_For_Package_Body_Stub
     (Self                        : Bare_Package_Body_Stub;
      Package_Body_Stub_F_Name    : Bare_Defining_Name;
      Package_Body_Stub_F_Aspects : Bare_Aspect_Spec);

   function Package_Body_Stub_F_Name
     (Node : Bare_Package_Body_Stub) return Bare_Defining_Name;

   function Package_Body_Stub_P_Defining_Names

     (Node   : Bare_Package_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Mappings_181

     (Node   : Bare_Package_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_182

     (Node : Bare_Package_Body_Stub)
return Boolean;

   function Bare_Package_Body_Stub_Pre_Env_Actions
     (Self                : Bare_Package_Body_Stub;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Protected_Body_Stub
     (Self                          : Bare_Protected_Body_Stub;
      Protected_Body_Stub_F_Name    : Bare_Defining_Name;
      Protected_Body_Stub_F_Aspects : Bare_Aspect_Spec);

   function Protected_Body_Stub_F_Name
     (Node : Bare_Protected_Body_Stub) return Bare_Defining_Name;

   function Protected_Body_Stub_P_Defining_Names

     (Node   : Bare_Protected_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Mappings_177

     (Node   : Bare_Protected_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_178

     (Node : Bare_Protected_Body_Stub)
return Boolean;

   function Bare_Protected_Body_Stub_Pre_Env_Actions
     (Self                : Bare_Protected_Body_Stub;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Subp_Body_Stub
     (Self                        : Bare_Subp_Body_Stub;
      Subp_Body_Stub_F_Overriding : Bare_Overriding_Node;
      Subp_Body_Stub_F_Subp_Spec  : Bare_Subp_Spec;
      Subp_Body_Stub_F_Aspects    : Bare_Aspect_Spec);

   function Subp_Body_Stub_F_Overriding
     (Node : Bare_Subp_Body_Stub) return Bare_Overriding_Node;

   function Subp_Body_Stub_F_Subp_Spec
     (Node : Bare_Subp_Body_Stub) return Bare_Subp_Spec;

   function Subp_Body_Stub_P_Defining_Names

     (Node   : Bare_Subp_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Subp_Body_Stub_P_Type_Expression

     (Node   : Bare_Subp_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Env_Mappings_179

     (Node   : Bare_Subp_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_180
(Node : Bare_Subp_Body_Stub)
return Boolean;

   function Bare_Subp_Body_Stub_Pre_Env_Actions
     (Self : Bare_Subp_Body_Stub; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Task_Body_Stub
     (Self : Bare_Task_Body_Stub; Task_Body_Stub_F_Name : Bare_Defining_Name;
      Task_Body_Stub_F_Aspects : Bare_Aspect_Spec);

   function Task_Body_Stub_F_Name
     (Node : Bare_Task_Body_Stub) return Bare_Defining_Name;

   function Task_Body_Stub_P_Defining_Names

     (Node   : Bare_Task_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Mappings_183

     (Node   : Bare_Task_Body_Stub;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_184
(Node : Bare_Task_Body_Stub)
return Boolean;

   function Bare_Task_Body_Stub_Pre_Env_Actions
     (Self : Bare_Task_Body_Stub; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Entry_Body
     (Self : Bare_Entry_Body; Entry_Body_F_Entry_Name : Bare_Defining_Name;
      Entry_Body_F_Index_Spec : Bare_Entry_Index_Spec;
      Entry_Body_F_Params     : Bare_Entry_Completion_Formal_Params;
      Entry_Body_F_Barrier    : Bare_Expr;
      Entry_Body_F_Decls      : Bare_Declarative_Part;
      Entry_Body_F_Stmts      : Bare_Handled_Stmts;
      Entry_Body_F_End_Name   : Bare_End_Name);

   function Entry_Body_F_Entry_Name
     (Node : Bare_Entry_Body) return Bare_Defining_Name;

   function Entry_Body_F_Index_Spec
     (Node : Bare_Entry_Body) return Bare_Entry_Index_Spec;

   function Entry_Body_F_Params
     (Node : Bare_Entry_Body) return Bare_Entry_Completion_Formal_Params;

   function Entry_Body_F_Barrier (Node : Bare_Entry_Body) return Bare_Expr;

   function Entry_Body_F_Decls
     (Node : Bare_Entry_Body) return Bare_Declarative_Part;

   function Entry_Body_F_Stmts
     (Node : Bare_Entry_Body) return Bare_Handled_Stmts;

   function Entry_Body_F_End_Name
     (Node : Bare_Entry_Body) return Bare_End_Name;

   function Entry_Body_P_Defining_Names

     (Node : Bare_Entry_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Trans_Parent_175
(Node : Bare_Entry_Body)
return Boolean;

   function Bare_Entry_Body_Pre_Env_Actions
     (Self : Bare_Entry_Body; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Package_Body
     (Self                        : Bare_Package_Body;
      Package_Body_F_Package_Name : Bare_Defining_Name;
      Package_Body_F_Aspects      : Bare_Aspect_Spec;
      Package_Body_F_Decls        : Bare_Declarative_Part;
      Package_Body_F_Stmts        : Bare_Handled_Stmts;
      Package_Body_F_End_Name     : Bare_End_Name);

   function Package_Body_F_Package_Name
     (Node : Bare_Package_Body) return Bare_Defining_Name;

   function Package_Body_F_Decls
     (Node : Bare_Package_Body) return Bare_Declarative_Part;

   function Package_Body_F_Stmts
     (Node : Bare_Package_Body) return Bare_Handled_Stmts;

   function Package_Body_F_End_Name
     (Node : Bare_Package_Body) return Bare_End_Name;

   function Package_Body_P_Defining_Names

     (Node   : Bare_Package_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Package_Body_P_Defining_Env

     (Node   : Bare_Package_Body; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Package_Body_P_Declarative_Region

     (Node   : Bare_Package_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Declarative_Part;

   function Package_Body_P_Package_Decl_Uses_Clauses_Envs

     (Node   : Bare_Package_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Return the environments for the use clauses of the package decl of this
--  body. Used because they need to be explicitly referenced.

   function Env_Do_136
(Node : Bare_Package_Body)
return Boolean;

   function Initial_Env_137

     (Node   : Bare_Package_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_138

     (Node   : Bare_Package_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_139
(Node : Bare_Package_Body)
return Boolean;

   function Env_Do_140

     (Node : Bare_Package_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_141

     (Node : Bare_Package_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_142
(Node : Bare_Package_Body)
return Boolean;

   function Ref_Env_Nodes_143

     (Node : Bare_Package_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_144
(Node : Bare_Package_Body)
return Boolean;

   function Ref_Env_Nodes_145

     (Node : Bare_Package_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_146
(Node : Bare_Package_Body)
return Boolean;

   function Ref_Env_Nodes_147

     (Node : Bare_Package_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_148
(Node : Bare_Package_Body)
return Boolean;

   function Ref_Env_Nodes_149

     (Node : Bare_Package_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_150
(Node : Bare_Package_Body)
return Boolean;

   function Bare_Package_Body_Pre_Env_Actions
     (Self : Bare_Package_Body; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Protected_Body
     (Self : Bare_Protected_Body; Protected_Body_F_Name : Bare_Defining_Name;
      Protected_Body_F_Aspects  : Bare_Aspect_Spec;
      Protected_Body_F_Decls    : Bare_Declarative_Part;
      Protected_Body_F_End_Name : Bare_End_Name);

   function Protected_Body_F_Name
     (Node : Bare_Protected_Body) return Bare_Defining_Name;

   function Protected_Body_F_Decls
     (Node : Bare_Protected_Body) return Bare_Declarative_Part;

   function Protected_Body_F_End_Name
     (Node : Bare_Protected_Body) return Bare_End_Name;

   function Protected_Body_P_Defining_Names

     (Node   : Bare_Protected_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Do_163
(Node : Bare_Protected_Body)
return Boolean;

   function Initial_Env_164

     (Node   : Bare_Protected_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_165

     (Node   : Bare_Protected_Body;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_166
(Node : Bare_Protected_Body)
return Boolean;

   function Env_Do_167

     (Node : Bare_Protected_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_168

     (Node : Bare_Protected_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_169
(Node : Bare_Protected_Body)
return Boolean;

   function Ref_Env_Nodes_170

     (Node : Bare_Protected_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_171
(Node : Bare_Protected_Body)
return Boolean;

   function Ref_Env_Nodes_172

     (Node : Bare_Protected_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_173

     (Node : Bare_Protected_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_174
(Node : Bare_Protected_Body)
return Boolean;

   function Bare_Protected_Body_Pre_Env_Actions
     (Self : Bare_Protected_Body; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Task_Body
     (Self : Bare_Task_Body; Task_Body_F_Name : Bare_Defining_Name;
      Task_Body_F_Aspects  : Bare_Aspect_Spec;
      Task_Body_F_Decls    : Bare_Declarative_Part;
      Task_Body_F_Stmts    : Bare_Handled_Stmts;
      Task_Body_F_End_Name : Bare_End_Name);

   function Task_Body_F_Name (Node : Bare_Task_Body) return Bare_Defining_Name;

   function Task_Body_F_Decls
     (Node : Bare_Task_Body) return Bare_Declarative_Part;

   function Task_Body_F_Stmts
     (Node : Bare_Task_Body) return Bare_Handled_Stmts;

   function Task_Body_F_End_Name (Node : Bare_Task_Body) return Bare_End_Name;

   function Task_Body_P_Defining_Names

     (Node : Bare_Task_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Task_Body_P_Task_Type_Decl_Scope

     (Node : Bare_Task_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Task_Body_P_Task_Type

     (Node : Bare_Task_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Task_Type_Decl;

   function Env_Do_151
(Node : Bare_Task_Body)
return Boolean;

   function Initial_Env_152

     (Node : Bare_Task_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Env_Mappings_153

     (Node : Bare_Task_Body; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_154
(Node : Bare_Task_Body)
return Boolean;

   function Env_Do_155

     (Node : Bare_Task_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_156

     (Node : Bare_Task_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_157
(Node : Bare_Task_Body)
return Boolean;

   function Ref_Env_Nodes_158

     (Node : Bare_Task_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_159
(Node : Bare_Task_Body)
return Boolean;

   function Ref_Env_Nodes_160

     (Node : Bare_Task_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_161

     (Node : Bare_Task_Body)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_162
(Node : Bare_Task_Body)
return Boolean;

   function Bare_Task_Body_Pre_Env_Actions
     (Self : Bare_Task_Body; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Entry_Index_Spec
     (Self : Bare_Entry_Index_Spec; Entry_Index_Spec_F_Id : Bare_Defining_Name;
      Entry_Index_Spec_F_Subtype : Bare_Ada_Node);

   function Entry_Index_Spec_F_Id
     (Node : Bare_Entry_Index_Spec) return Bare_Defining_Name;

   function Entry_Index_Spec_F_Subtype
     (Node : Bare_Entry_Index_Spec) return Bare_Ada_Node;

   function Entry_Index_Spec_P_Defining_Names

     (Node   : Bare_Entry_Index_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Entry_Index_Spec_P_Defining_Env

     (Node   : Bare_Entry_Index_Spec; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Entry_Index_Spec_P_Expr_Type

     (Node   : Bare_Entry_Index_Spec; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Entry_Index_Spec_P_Xref_Equation

     (Node : Bare_Entry_Index_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Entry_Index_Spec_P_Xref_Entry_Point

     (Node : Bare_Entry_Index_Spec)
return Boolean;

   function Env_Mappings_176

     (Node   : Bare_Entry_Index_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Bare_Entry_Index_Spec_Pre_Env_Actions
     (Self : Bare_Entry_Index_Spec; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   function Error_Decl_P_Defining_Names

     (Node : Bare_Error_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   procedure Initialize_Fields_For_Exception_Decl
     (Self                     : Bare_Exception_Decl;
      Exception_Decl_F_Ids     : Bare_Defining_Name_List;
      Exception_Decl_F_Renames : Bare_Renaming_Clause;
      Exception_Decl_F_Aspects : Bare_Aspect_Spec);

   function Exception_Decl_F_Ids
     (Node : Bare_Exception_Decl) return Bare_Defining_Name_List;

   function Exception_Decl_F_Renames
     (Node : Bare_Exception_Decl) return Bare_Renaming_Clause;

   function Exception_Decl_P_Defining_Names

     (Node   : Bare_Exception_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Exception_Decl_P_Next_Part_For_Decl

     (Node   : Bare_Exception_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  An exception declaration never has a next part.

   function Exception_Decl_P_Xref_Entry_Point

     (Node : Bare_Exception_Decl)
return Boolean;

   function Exception_Decl_P_Xref_Equation

     (Node   : Bare_Exception_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Env_Mappings_46

     (Node : Bare_Exception_Decl)
return Internal_Env_Assoc_Array_Access;

   function Bare_Exception_Decl_Pre_Env_Actions
     (Self : Bare_Exception_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Exception_Handler
     (Self                                   : Bare_Exception_Handler;
      Exception_Handler_F_Exception_Name     : Bare_Defining_Name;
      Exception_Handler_F_Handled_Exceptions : Bare_Alternatives_List;
      Exception_Handler_F_Stmts              : Bare_Stmt_List);

   function Exception_Handler_F_Exception_Name
     (Node : Bare_Exception_Handler) return Bare_Defining_Name;

   function Exception_Handler_F_Handled_Exceptions
     (Node : Bare_Exception_Handler) return Bare_Alternatives_List;

   function Exception_Handler_F_Stmts
     (Node : Bare_Exception_Handler) return Bare_Stmt_List;

   function Exception_Handler_P_Defining_Names

     (Node   : Bare_Exception_Handler;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Exception_Handler_P_Expr_Type

     (Node   : Bare_Exception_Handler; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Exception_Handler_P_Xref_Equation

     (Node : Bare_Exception_Handler; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Exception_Handler_P_Xref_Entry_Point

     (Node : Bare_Exception_Handler)
return Boolean;

   function Env_Trans_Parent_127

     (Node : Bare_Exception_Handler)
return Boolean;

   function Env_Mappings_128

     (Node   : Bare_Exception_Handler;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Bare_Exception_Handler_Pre_Env_Actions
     (Self                : Bare_Exception_Handler;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_For_Loop_Var_Decl
     (Self                        : Bare_For_Loop_Var_Decl;
      For_Loop_Var_Decl_F_Id      : Bare_Defining_Name;
      For_Loop_Var_Decl_F_Id_Type : Bare_Subtype_Indication);

   function For_Loop_Var_Decl_F_Id
     (Node : Bare_For_Loop_Var_Decl) return Bare_Defining_Name;

   function For_Loop_Var_Decl_F_Id_Type
     (Node : Bare_For_Loop_Var_Decl) return Bare_Subtype_Indication;

   function For_Loop_Var_Decl_P_Defining_Names

     (Node   : Bare_For_Loop_Var_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function For_Loop_Var_Decl_P_Defining_Env

     (Node   : Bare_For_Loop_Var_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function For_Loop_Var_Decl_P_Expr_Type

     (Node   : Bare_For_Loop_Var_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Env_Mappings_112

     (Node : Bare_For_Loop_Var_Decl)
return Internal_Env_Assoc;

   function Bare_For_Loop_Var_Decl_Pre_Env_Actions
     (Self                : Bare_For_Loop_Var_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Generic_Decl
     (Self                       : Bare_Generic_Decl;
      Generic_Decl_F_Formal_Part : Bare_Generic_Formal_Part);

   function Generic_Decl_F_Formal_Part
     (Node : Bare_Generic_Decl) return Bare_Generic_Formal_Part;

   function Dispatcher_Generic_Decl_P_Decl

     (Node   : Bare_Generic_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   procedure Initialize_Fields_For_Generic_Package_Decl
     (Self                                : Bare_Generic_Package_Decl;
      Generic_Decl_F_Formal_Part          : Bare_Generic_Formal_Part;
      Generic_Package_Decl_F_Package_Decl : Bare_Generic_Package_Internal);

   function Generic_Package_Decl_F_Package_Decl
     (Node : Bare_Generic_Package_Decl) return Bare_Generic_Package_Internal;

   function Generic_Package_Decl_P_Defining_Env

     (Node   : Bare_Generic_Package_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Generic_Package_Decl_P_Defining_Names

     (Node   : Bare_Generic_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Generic_Package_Decl_P_Body_Part

     (Node   : Bare_Generic_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Package_Body;
--  Return the PackageBody corresponding to this node, or null if there is
--  none.

   function Generic_Package_Decl_P_Decl

     (Node   : Bare_Generic_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Env_Do_99
(Node : Bare_Generic_Package_Decl)
return Boolean;

   function Initial_Env_100

     (Node   : Bare_Generic_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_101

     (Node   : Bare_Generic_Package_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_102

     (Node : Bare_Generic_Package_Decl)
return Boolean;

   function Env_Do_103

     (Node : Bare_Generic_Package_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_104

     (Node : Bare_Generic_Package_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_105
(Node : Bare_Generic_Package_Decl)
return Boolean;

   function Ref_Env_Nodes_106

     (Node : Bare_Generic_Package_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_107
(Node : Bare_Generic_Package_Decl)
return Boolean;

   function Bare_Generic_Package_Decl_Pre_Env_Actions
     (Self                : Bare_Generic_Package_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Generic_Subp_Decl
     (Self                          : Bare_Generic_Subp_Decl;
      Generic_Decl_F_Formal_Part    : Bare_Generic_Formal_Part;
      Generic_Subp_Decl_F_Subp_Decl : Bare_Generic_Subp_Internal);

   function Generic_Subp_Decl_F_Subp_Decl
     (Node : Bare_Generic_Subp_Decl) return Bare_Generic_Subp_Internal;

   function Generic_Subp_Decl_P_Defining_Names

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Generic_Subp_Decl_P_Body_Part

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Subp_Body;
--  Return the BaseSubpBody corresponding to this node.

   function Generic_Subp_Decl_P_Decl

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Generic_Subp_Decl_P_Is_Imported

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Generic_Subp_Decl_P_Next_Part_For_Decl

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Env_Do_91
(Node : Bare_Generic_Subp_Decl)
return Boolean;

   function Initial_Env_92

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_93

     (Node   : Bare_Generic_Subp_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_94
(Node : Bare_Generic_Subp_Decl)
return Boolean;

   function Env_Do_95

     (Node : Bare_Generic_Subp_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_96

     (Node : Bare_Generic_Subp_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_97
(Node : Bare_Generic_Subp_Decl)
return Boolean;

   function Bare_Generic_Subp_Decl_Pre_Env_Actions
     (Self                : Bare_Generic_Subp_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Generic_Instantiation
     (Self : Bare_Generic_Instantiation);

   function Dispatcher_Generic_Instantiation_P_Generic_Entity_Name

     (Node   : Bare_Generic_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;
--  Return the name of the generic entity designated by this generic
--  instantiation.

   function Dispatcher_Generic_Instantiation_P_Generic_Inst_Params

     (Node   : Bare_Generic_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Assoc_List;
--  Return the parameters of this generic instantiation

   function Generic_Instantiation_P_Is_Any_Formal

     (Node   : Bare_Generic_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Generic_Instantiation_P_Nonbound_Generic_Decl

     (Node : Bare_Generic_Instantiation)
return Internal_Entity_Generic_Decl;
--  Return the formal package designated by the right hand part of this generic
--  package instantiation.

   function Dispatcher_Generic_Instantiation_P_Designated_Generic_Decl

     (Node   : Bare_Generic_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the generic decl entity designated by this instantiation, containing
--  the generic context. This is equivalent to the expanded generic unit in
--  GNAT.

   function Generic_Instantiation_P_Xref_Entry_Point

     (Node : Bare_Generic_Instantiation)
return Boolean;

   function Generic_Instantiation_P_Xref_Equation

     (Node   : Bare_Generic_Instantiation; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   procedure Initialize_Fields_For_Generic_Package_Instantiation
     (Self : Bare_Generic_Package_Instantiation;
      Generic_Package_Instantiation_F_Name             : Bare_Defining_Name;
      Generic_Package_Instantiation_F_Generic_Pkg_Name : Bare_Name;
      Generic_Package_Instantiation_F_Params           : Bare_Assoc_List;
      Generic_Package_Instantiation_F_Aspects          : Bare_Aspect_Spec);

   function Generic_Package_Instantiation_F_Name
     (Node : Bare_Generic_Package_Instantiation) return Bare_Defining_Name;

   function Generic_Package_Instantiation_F_Generic_Pkg_Name
     (Node : Bare_Generic_Package_Instantiation) return Bare_Name;

   function Generic_Package_Instantiation_F_Params
     (Node : Bare_Generic_Package_Instantiation) return Bare_Assoc_List;

   function Generic_Package_Instantiation_P_Generic_Entity_Name

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Generic_Package_Instantiation_P_Generic_Inst_Params

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Assoc_List;

   function Generic_Package_Instantiation_P_Designated_Package

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Package_Decl;

   function Generic_Package_Instantiation_P_Designated_Generic_Decl

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Generic_Package_Instantiation_P_Defining_Env_Impl

     (Node             : Bare_Generic_Package_Instantiation;
      Inst_From_Formal : Boolean              := False; Origin : Bare_Ada_Node;
      E_Info           : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Specialized function for getting the defining env for this generic
--  instantiation.
--
--  If ``inst_from_formal`` is True, we know that this generic package
--  instantiation is coming from a rebound formal package, and that we
--  need visibility on the formals.

   function Generic_Package_Instantiation_P_Defining_Env

     (Node   : Bare_Generic_Package_Instantiation; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Generic_Package_Instantiation_P_Defining_Names

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Do_55

     (Node : Bare_Generic_Package_Instantiation)
return Boolean;

   function Initial_Env_56

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_57

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_58

     (Node : Bare_Generic_Package_Instantiation)
return Boolean;

   function Env_Do_59

     (Node : Bare_Generic_Package_Instantiation)

      return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_60

     (Node : Bare_Generic_Package_Instantiation)

      return Bare_Ada_Node_Array_Access;

   function Ref_Cond_61

     (Node : Bare_Generic_Package_Instantiation)
return Boolean;

   function Env_Mappings_62

     (Node   : Bare_Generic_Package_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Bare_Generic_Package_Instantiation_Pre_Env_Actions
     (Self                : Bare_Generic_Package_Instantiation;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Bare_Generic_Package_Instantiation_Post_Env_Actions
     (Self                : Bare_Generic_Package_Instantiation;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env);

   procedure Initialize_Fields_For_Generic_Subp_Instantiation
     (Self : Bare_Generic_Subp_Instantiation;
      Generic_Subp_Instantiation_F_Overriding        : Bare_Overriding_Node;
      Generic_Subp_Instantiation_F_Kind              : Bare_Subp_Kind;
      Generic_Subp_Instantiation_F_Subp_Name         : Bare_Defining_Name;
      Generic_Subp_Instantiation_F_Generic_Subp_Name : Bare_Name;
      Generic_Subp_Instantiation_F_Params            : Bare_Assoc_List;
      Generic_Subp_Instantiation_F_Aspects           : Bare_Aspect_Spec);

   function Generic_Subp_Instantiation_F_Overriding
     (Node : Bare_Generic_Subp_Instantiation) return Bare_Overriding_Node;

   function Generic_Subp_Instantiation_F_Kind
     (Node : Bare_Generic_Subp_Instantiation) return Bare_Subp_Kind;

   function Generic_Subp_Instantiation_F_Subp_Name
     (Node : Bare_Generic_Subp_Instantiation) return Bare_Defining_Name;

   function Generic_Subp_Instantiation_F_Generic_Subp_Name
     (Node : Bare_Generic_Subp_Instantiation) return Bare_Name;

   function Generic_Subp_Instantiation_F_Params
     (Node : Bare_Generic_Subp_Instantiation) return Bare_Assoc_List;

   function Generic_Subp_Instantiation_P_Defining_Names

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Generic_Subp_Instantiation_P_Generic_Entity_Name

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Generic_Subp_Instantiation_P_Generic_Inst_Params

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Assoc_List;

   function Generic_Subp_Instantiation_P_Designated_Subp

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Internal_Entity;
--  Return the subprogram decl designated by this instantiation.

   function Generic_Subp_Instantiation_P_Designated_Generic_Decl

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Env_Do_47
(Node : Bare_Generic_Subp_Instantiation)
return Boolean;

   function Initial_Env_48

     (Node : Bare_Generic_Subp_Instantiation)
return Lexical_Env;

   function Env_Trans_Parent_49

     (Node : Bare_Generic_Subp_Instantiation)
return Boolean;

   function Env_Do_50

     (Node : Bare_Generic_Subp_Instantiation)

      return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_51

     (Node : Bare_Generic_Subp_Instantiation)

      return Bare_Ada_Node_Array_Access;

   function Ref_Cond_52

     (Node : Bare_Generic_Subp_Instantiation)
return Boolean;

   function Env_Mappings_53

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc_Array_Access;

   function Env_Mappings_54

     (Node   : Bare_Generic_Subp_Instantiation;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Bare_Generic_Subp_Instantiation_Pre_Env_Actions
     (Self                : Bare_Generic_Subp_Instantiation;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Bare_Generic_Subp_Instantiation_Post_Env_Actions
     (Self                : Bare_Generic_Subp_Instantiation;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env);

   function Dispatcher_Generic_Renaming_Decl_P_Renaming_Name

     (Node   : Bare_Generic_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Generic_Renaming_Decl_P_Resolve

     (Node   : Bare_Generic_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Generic_Decl;
--  Resolve the GenericDecl this renaming decl is pointing at

   procedure Initialize_Fields_For_Generic_Package_Renaming_Decl
     (Self : Bare_Generic_Package_Renaming_Decl;
      Generic_Package_Renaming_Decl_F_Name    : Bare_Defining_Name;
      Generic_Package_Renaming_Decl_F_Renames : Bare_Name;
      Generic_Package_Renaming_Decl_F_Aspects : Bare_Aspect_Spec);

   function Generic_Package_Renaming_Decl_F_Name
     (Node : Bare_Generic_Package_Renaming_Decl) return Bare_Defining_Name;

   function Generic_Package_Renaming_Decl_F_Renames
     (Node : Bare_Generic_Package_Renaming_Decl) return Bare_Name;

   function Generic_Package_Renaming_Decl_P_Defining_Names

     (Node   : Bare_Generic_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Generic_Package_Renaming_Decl_P_Defining_Env

     (Node   : Bare_Generic_Package_Renaming_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Generic_Package_Renaming_Decl_P_Renaming_Name

     (Node   : Bare_Generic_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Env_Do_72

     (Node : Bare_Generic_Package_Renaming_Decl)
return Boolean;

   function Initial_Env_73

     (Node : Bare_Generic_Package_Renaming_Decl)
return Lexical_Env;

   function Env_Mappings_74

     (Node   : Bare_Generic_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_75

     (Node : Bare_Generic_Package_Renaming_Decl)
return Boolean;

   function Env_Do_76

     (Node : Bare_Generic_Package_Renaming_Decl)

      return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_77

     (Node : Bare_Generic_Package_Renaming_Decl)

      return Bare_Ada_Node_Array_Access;

   function Ref_Cond_78

     (Node : Bare_Generic_Package_Renaming_Decl)
return Boolean;

   function Ref_Env_Nodes_79

     (Node : Bare_Generic_Package_Renaming_Decl)

      return Bare_Ada_Node_Array_Access;

   function Ref_Cond_80

     (Node : Bare_Generic_Package_Renaming_Decl)
return Boolean;

   function Bare_Generic_Package_Renaming_Decl_Pre_Env_Actions
     (Self                : Bare_Generic_Package_Renaming_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Generic_Subp_Renaming_Decl
     (Self                                 : Bare_Generic_Subp_Renaming_Decl;
      Generic_Subp_Renaming_Decl_F_Kind    : Bare_Subp_Kind;
      Generic_Subp_Renaming_Decl_F_Name    : Bare_Defining_Name;
      Generic_Subp_Renaming_Decl_F_Renames : Bare_Name;
      Generic_Subp_Renaming_Decl_F_Aspects : Bare_Aspect_Spec);

   function Generic_Subp_Renaming_Decl_F_Kind
     (Node : Bare_Generic_Subp_Renaming_Decl) return Bare_Subp_Kind;

   function Generic_Subp_Renaming_Decl_F_Name
     (Node : Bare_Generic_Subp_Renaming_Decl) return Bare_Defining_Name;

   function Generic_Subp_Renaming_Decl_F_Renames
     (Node : Bare_Generic_Subp_Renaming_Decl) return Bare_Name;

   function Generic_Subp_Renaming_Decl_P_Defining_Names

     (Node   : Bare_Generic_Subp_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Generic_Subp_Renaming_Decl_P_Renaming_Name

     (Node   : Bare_Generic_Subp_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Env_Do_81
(Node : Bare_Generic_Subp_Renaming_Decl)
return Boolean;

   function Initial_Env_82

     (Node : Bare_Generic_Subp_Renaming_Decl)
return Lexical_Env;

   function Env_Mappings_83

     (Node   : Bare_Generic_Subp_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_84

     (Node : Bare_Generic_Subp_Renaming_Decl)
return Boolean;

   function Env_Do_85

     (Node : Bare_Generic_Subp_Renaming_Decl)

      return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_86

     (Node : Bare_Generic_Subp_Renaming_Decl)

      return Bare_Ada_Node_Array_Access;

   function Ref_Cond_87

     (Node : Bare_Generic_Subp_Renaming_Decl)
return Boolean;

   function Ref_Env_Nodes_88

     (Node : Bare_Generic_Subp_Renaming_Decl)

      return Bare_Ada_Node_Array_Access;

   function Ref_Cond_89

     (Node : Bare_Generic_Subp_Renaming_Decl)
return Boolean;

   function Bare_Generic_Subp_Renaming_Decl_Pre_Env_Actions
     (Self                : Bare_Generic_Subp_Renaming_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Label_Decl
     (Self : Bare_Label_Decl; Label_Decl_F_Name : Bare_Defining_Name);

   function Label_Decl_F_Name
     (Node : Bare_Label_Decl) return Bare_Defining_Name;

   function Label_Decl_P_Defining_Names

     (Node : Bare_Label_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Env_Mappings_129

     (Node : Bare_Label_Decl)
return Internal_Env_Assoc;

   function Bare_Label_Decl_Pre_Env_Actions
     (Self : Bare_Label_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Named_Stmt_Decl
     (Self                   : Bare_Named_Stmt_Decl;
      Named_Stmt_Decl_F_Name : Bare_Defining_Name);

   function Named_Stmt_Decl_F_Name
     (Node : Bare_Named_Stmt_Decl) return Bare_Defining_Name;

   function Named_Stmt_Decl_P_Defining_Names

     (Node   : Bare_Named_Stmt_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Named_Stmt_Decl_P_Defining_Env

     (Node   : Bare_Named_Stmt_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   procedure Initialize_Fields_For_Number_Decl
     (Self : Bare_Number_Decl; Number_Decl_F_Ids : Bare_Defining_Name_List;
      Number_Decl_F_Expr : Bare_Expr);

   function Number_Decl_F_Ids
     (Node : Bare_Number_Decl) return Bare_Defining_Name_List;

   function Number_Decl_F_Expr (Node : Bare_Number_Decl) return Bare_Expr;

   function Number_Decl_P_Defining_Names

     (Node : Bare_Number_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Number_Decl_P_Expr_Type

     (Node   : Bare_Number_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Number_Decl_P_Xref_Entry_Point

     (Node : Bare_Number_Decl)
return Boolean;

   function Number_Decl_P_Is_Static_Decl

     (Node   : Bare_Number_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Number_Decl_P_Xref_Equation

     (Node   : Bare_Number_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Env_Mappings_33

     (Node : Bare_Number_Decl)
return Internal_Env_Assoc_Array_Access;

   function Bare_Number_Decl_Pre_Env_Actions
     (Self : Bare_Number_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Object_Decl
     (Self : Bare_Object_Decl; Object_Decl_F_Ids : Bare_Defining_Name_List;
      Object_Decl_F_Has_Aliased     : Bare_Aliased_Node;
      Object_Decl_F_Has_Constant    : Bare_Constant_Node;
      Object_Decl_F_Mode : Bare_Mode; Object_Decl_F_Type_Expr : Bare_Type_Expr;
      Object_Decl_F_Default_Expr    : Bare_Expr;
      Object_Decl_F_Renaming_Clause : Bare_Renaming_Clause;
      Object_Decl_F_Aspects         : Bare_Aspect_Spec);

   function Object_Decl_F_Ids
     (Node : Bare_Object_Decl) return Bare_Defining_Name_List;

   function Object_Decl_F_Has_Aliased
     (Node : Bare_Object_Decl) return Bare_Aliased_Node;

   function Object_Decl_F_Has_Constant
     (Node : Bare_Object_Decl) return Bare_Constant_Node;

   function Object_Decl_F_Mode (Node : Bare_Object_Decl) return Bare_Mode;

   function Object_Decl_F_Type_Expr
     (Node : Bare_Object_Decl) return Bare_Type_Expr;

   function Object_Decl_F_Default_Expr
     (Node : Bare_Object_Decl) return Bare_Expr;

   function Object_Decl_F_Renaming_Clause
     (Node : Bare_Object_Decl) return Bare_Renaming_Clause;

   function Object_Decl_P_Defining_Names

     (Node : Bare_Object_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Object_Decl_P_Defining_Env

     (Node   : Bare_Object_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Object_Decl_P_Type_Expression

     (Node : Bare_Object_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Expr;

   function Object_Decl_P_Is_Static_Decl

     (Node   : Bare_Object_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Object_Decl_P_Xref_Equation

     (Node   : Bare_Object_Decl; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Object_Decl_P_Public_Part_Decl

     (Node : Bare_Object_Decl; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  If this object decl is the constant completion of an object decl in the
--  public part, return the object decl from the public part.

   function Object_Decl_P_Xref_Entry_Point

     (Node : Bare_Object_Decl)
return Boolean;

   function Env_Mappings_34

     (Node : Bare_Object_Decl)
return Internal_Env_Assoc_Array_Access;

   function Bare_Object_Decl_Pre_Env_Actions
     (Self : Bare_Object_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Anonymous_Object_Decl
     (Self                          : Bare_Anonymous_Object_Decl;
      Object_Decl_F_Ids             : Bare_Defining_Name_List;
      Object_Decl_F_Has_Aliased     : Bare_Aliased_Node;
      Object_Decl_F_Has_Constant    : Bare_Constant_Node;
      Object_Decl_F_Mode : Bare_Mode; Object_Decl_F_Type_Expr : Bare_Type_Expr;
      Object_Decl_F_Default_Expr    : Bare_Expr;
      Object_Decl_F_Renaming_Clause : Bare_Renaming_Clause;
      Object_Decl_F_Aspects         : Bare_Aspect_Spec);

   function Anonymous_Object_Decl_P_Defining_Names

     (Node   : Bare_Anonymous_Object_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Anonymous_Object_Decl_P_Is_Static_Decl

     (Node : Bare_Anonymous_Object_Decl; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Extended_Return_Stmt_Object_Decl
     (Self                          : Bare_Extended_Return_Stmt_Object_Decl;
      Object_Decl_F_Ids             : Bare_Defining_Name_List;
      Object_Decl_F_Has_Aliased     : Bare_Aliased_Node;
      Object_Decl_F_Has_Constant    : Bare_Constant_Node;
      Object_Decl_F_Mode : Bare_Mode; Object_Decl_F_Type_Expr : Bare_Type_Expr;
      Object_Decl_F_Default_Expr    : Bare_Expr;
      Object_Decl_F_Renaming_Clause : Bare_Renaming_Clause;
      Object_Decl_F_Aspects         : Bare_Aspect_Spec);

   procedure Initialize_Fields_For_Package_Renaming_Decl
     (Self                            : Bare_Package_Renaming_Decl;
      Package_Renaming_Decl_F_Name    : Bare_Defining_Name;
      Package_Renaming_Decl_F_Renames : Bare_Renaming_Clause;
      Package_Renaming_Decl_F_Aspects : Bare_Aspect_Spec);

   function Package_Renaming_Decl_F_Name
     (Node : Bare_Package_Renaming_Decl) return Bare_Defining_Name;

   function Package_Renaming_Decl_F_Renames
     (Node : Bare_Package_Renaming_Decl) return Bare_Renaming_Clause;

   function Package_Renaming_Decl_P_Renamed_Package

     (Node   : Bare_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the declaration of the package that is renamed by Self.

   function Package_Renaming_Decl_P_Final_Renamed_Package

     (Node   : Bare_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the declaration of the package that is ultimately renamed by Self,
--  skipping through all intermediate package renamings.

   function Package_Renaming_Decl_P_Defining_Names

     (Node   : Bare_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Package_Renaming_Decl_P_Defining_Env

     (Node   : Bare_Package_Renaming_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Package_Renaming_Decl_P_Xref_Entry_Point

     (Node : Bare_Package_Renaming_Decl)
return Boolean;

   function Package_Renaming_Decl_P_Xref_Equation

     (Node   : Bare_Package_Renaming_Decl; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Env_Do_63
(Node : Bare_Package_Renaming_Decl)
return Boolean;

   function Initial_Env_64

     (Node   : Bare_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_65

     (Node   : Bare_Package_Renaming_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_66

     (Node : Bare_Package_Renaming_Decl)
return Boolean;

   function Env_Do_67

     (Node : Bare_Package_Renaming_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Env_Nodes_68

     (Node : Bare_Package_Renaming_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_69
(Node : Bare_Package_Renaming_Decl)
return Boolean;

   function Ref_Env_Nodes_70

     (Node : Bare_Package_Renaming_Decl)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_71
(Node : Bare_Package_Renaming_Decl)
return Boolean;

   function Bare_Package_Renaming_Decl_Pre_Env_Actions
     (Self                : Bare_Package_Renaming_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Single_Protected_Decl
     (Self                               : Bare_Single_Protected_Decl;
      Single_Protected_Decl_F_Name       : Bare_Defining_Name;
      Single_Protected_Decl_F_Aspects    : Bare_Aspect_Spec;
      Single_Protected_Decl_F_Interfaces : Bare_Parent_List;
      Single_Protected_Decl_F_Definition : Bare_Protected_Def);

   function Single_Protected_Decl_F_Name
     (Node : Bare_Single_Protected_Decl) return Bare_Defining_Name;

   function Single_Protected_Decl_F_Interfaces
     (Node : Bare_Single_Protected_Decl) return Bare_Parent_List;

   function Single_Protected_Decl_F_Definition
     (Node : Bare_Single_Protected_Decl) return Bare_Protected_Def;

   function Single_Protected_Decl_P_Defining_Names

     (Node   : Bare_Single_Protected_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Single_Protected_Decl_P_Defining_Env

     (Node   : Bare_Single_Protected_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_31

     (Node   : Bare_Single_Protected_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Env_Assoc;

   function Env_Trans_Parent_32

     (Node : Bare_Single_Protected_Decl)
return Boolean;

   function Bare_Single_Protected_Decl_Pre_Env_Actions
     (Self                : Bare_Single_Protected_Decl;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Single_Task_Decl
     (Self                         : Bare_Single_Task_Decl;
      Single_Task_Decl_F_Task_Type : Bare_Single_Task_Type_Decl);

   function Single_Task_Decl_F_Task_Type
     (Node : Bare_Single_Task_Decl) return Bare_Single_Task_Type_Decl;

   function Single_Task_Decl_P_Defining_Names

     (Node   : Bare_Single_Task_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name_Array_Access;

   function Single_Task_Decl_P_Expr_Type

     (Node   : Bare_Single_Task_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Single_Task_Decl_P_Defining_Env

     (Node   : Bare_Single_Task_Decl; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Env_Mappings_29

     (Node : Bare_Single_Task_Decl)
return Internal_Env_Assoc;

   function Env_Trans_Parent_30
(Node : Bare_Single_Task_Decl)
return Boolean;

   function Bare_Single_Task_Decl_Pre_Env_Actions
     (Self : Bare_Single_Task_Decl; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Case_Stmt_Alternative
     (Self                            : Bare_Case_Stmt_Alternative;
      Case_Stmt_Alternative_F_Choices : Bare_Alternatives_List;
      Case_Stmt_Alternative_F_Stmts   : Bare_Stmt_List);

   function Case_Stmt_Alternative_F_Choices
     (Node : Bare_Case_Stmt_Alternative) return Bare_Alternatives_List;

   function Case_Stmt_Alternative_F_Stmts
     (Node : Bare_Case_Stmt_Alternative) return Bare_Stmt_List;

   procedure Initialize_Fields_For_Compilation_Unit
     (Self                       : Bare_Compilation_Unit;
      Compilation_Unit_F_Prelude : Bare_Ada_Node_List;
      Compilation_Unit_F_Body    : Bare_Ada_Node;
      Compilation_Unit_F_Pragmas : Bare_Pragma_Node_List);

   function Compilation_Unit_F_Prelude
     (Node : Bare_Compilation_Unit) return Bare_Ada_Node_List;

   function Compilation_Unit_F_Body
     (Node : Bare_Compilation_Unit) return Bare_Ada_Node;

   function Compilation_Unit_F_Pragmas
     (Node : Bare_Compilation_Unit) return Bare_Pragma_Node_List;

   function Compilation_Unit_P_Syntactic_Fully_Qualified_Name

     (Node : Bare_Compilation_Unit)
return Symbol_Type_Array_Access;
--  Return the syntactic fully qualified name of this compilation unit.

   function Compilation_Unit_P_Unit_Kind

     (Node : Bare_Compilation_Unit)
return Analysis_Unit_Kind;
--  Return the kind corresponding to this analysis unit.

   function Compilation_Unit_P_Withed_Units

     (Node : Bare_Compilation_Unit)

      return Internal_Entity_Compilation_Unit_Array_Access;
--  Look for all "with" clauses at the top of this compilation unit and return
--  all the compilation units designated by them.

   function Compilation_Unit_P_Imported_Units

     (Node : Bare_Compilation_Unit)

      return Internal_Entity_Compilation_Unit_Array_Access;
--  Return all the compilation units that are directly imported by this one.
--  This includes "with"ed units as well as the direct parent unit.

   function Compilation_Unit_P_Unit_Dependencies_Helper

     (Node     : Bare_Compilation_Unit;
      Visited  : Internal_Entity_Compilation_Unit_Array_Access;
      To_Visit : Internal_Entity_Compilation_Unit_Array_Access)

      return Internal_Entity_Compilation_Unit_Array_Access;
--  Helper function for "unit_dependencies" that computes transitively the unit
--  dependencies of the given ``to_visit`` units. The ``visited`` set of units
--  is used to terminate the search once a fix-point has been reached, which
--  is when all direct dependencies of ``to_visit`` are already included in
--  the ``visited`` set.

   function Compilation_Unit_P_Unit_Dependencies

     (Node   : Bare_Compilation_Unit;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Compilation_Unit_Array_Access;
--  Return the list of all the compilation units that are (direct and indirect)
--  dependencies of this one.

   function Compilation_Unit_P_Decl

     (Node : Bare_Compilation_Unit)
return Bare_Basic_Decl;
--  Get the root basic decl defined in this compilation unit.

   function Compilation_Unit_P_Is_Preelaborable_Impl

     (Node   : Bare_Compilation_Unit; From_Body : Boolean;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Implementation helper for ``is_preelaborable``.
--
--  Return whether ``Entity`` or its spec (if any) make it
--  preelaborable. ``from_body`` has the same semantics as
--  in ``does_aspects_make_preelaborate``.

   function Compilation_Unit_P_Is_Preelaborable

     (Node   : Bare_Compilation_Unit;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether this compilation unit is preelaborable or not.

   function Initial_Env_113
(Node : Bare_Compilation_Unit)
return Lexical_Env;

   function Bare_Compilation_Unit_Pre_Env_Actions
     (Self : Bare_Compilation_Unit; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Component_Clause
     (Self : Bare_Component_Clause; Component_Clause_F_Id : Bare_Identifier;
      Component_Clause_F_Position : Bare_Expr;
      Component_Clause_F_Range    : Bare_Range_Spec);

   function Component_Clause_F_Id
     (Node : Bare_Component_Clause) return Bare_Identifier;

   function Component_Clause_F_Position
     (Node : Bare_Component_Clause) return Bare_Expr;

   function Component_Clause_F_Range
     (Node : Bare_Component_Clause) return Bare_Range_Spec;

   function Component_Clause_P_Xref_Entry_Point

     (Node : Bare_Component_Clause)
return Boolean;

   function Component_Clause_P_Xref_Equation

     (Node : Bare_Component_Clause; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Component_Def
     (Self                         : Bare_Component_Def;
      Component_Def_F_Has_Aliased  : Bare_Aliased_Node;
      Component_Def_F_Has_Constant : Bare_Constant_Node;
      Component_Def_F_Type_Expr    : Bare_Type_Expr);

   function Component_Def_F_Has_Aliased
     (Node : Bare_Component_Def) return Bare_Aliased_Node;

   function Component_Def_F_Has_Constant
     (Node : Bare_Component_Def) return Bare_Constant_Node;

   function Component_Def_F_Type_Expr
     (Node : Bare_Component_Def) return Bare_Type_Expr;

   function Component_Def_P_Xref_Equation

     (Node   : Bare_Component_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Dispatcher_Constant_Node_P_As_Bool

     (Node : Bare_Constant_Node)
return Boolean;
--  Return whether this is an instance of ConstantPresent

   function Constant_Absent_P_As_Bool

     (Node : Bare_Constant_Absent)
return Boolean;

   function Constant_Present_P_As_Bool

     (Node : Bare_Constant_Present)
return Boolean;

   function Constraint_P_Subtype

     (Node : Bare_Constraint; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Constraint_P_Is_Static

     (Node   : Bare_Constraint; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Delta_Constraint
     (Self : Bare_Delta_Constraint; Delta_Constraint_F_Digits : Bare_Expr;
      Delta_Constraint_F_Range : Bare_Range_Spec);

   function Delta_Constraint_F_Digits
     (Node : Bare_Delta_Constraint) return Bare_Expr;

   function Delta_Constraint_F_Range
     (Node : Bare_Delta_Constraint) return Bare_Range_Spec;

   function Delta_Constraint_P_Xref_Equation

     (Node : Bare_Delta_Constraint; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Digits_Constraint
     (Self : Bare_Digits_Constraint; Digits_Constraint_F_Digits : Bare_Expr;
      Digits_Constraint_F_Range : Bare_Range_Spec);

   function Digits_Constraint_F_Digits
     (Node : Bare_Digits_Constraint) return Bare_Expr;

   function Digits_Constraint_F_Range
     (Node : Bare_Digits_Constraint) return Bare_Range_Spec;

   function Digits_Constraint_P_Xref_Equation

     (Node : Bare_Digits_Constraint; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Discriminant_Constraint
     (Self                                  : Bare_Discriminant_Constraint;
      Discriminant_Constraint_F_Constraints : Bare_Assoc_List);

   function Discriminant_Constraint_F_Constraints
     (Node : Bare_Discriminant_Constraint) return Bare_Assoc_List;

   function Discriminant_Constraint_P_Xref_Equation

     (Node   : Bare_Discriminant_Constraint; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   procedure Initialize_Fields_For_Index_Constraint
     (Self                           : Bare_Index_Constraint;
      Index_Constraint_F_Constraints : Bare_Constraint_List);

   function Index_Constraint_F_Constraints
     (Node : Bare_Index_Constraint) return Bare_Constraint_List;

   function Index_Constraint_P_Xref_Equation

     (Node : Bare_Index_Constraint; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Range_Constraint
     (Self                     : Bare_Range_Constraint;
      Range_Constraint_F_Range : Bare_Range_Spec);

   function Range_Constraint_F_Range
     (Node : Bare_Range_Constraint) return Bare_Range_Spec;

   function Range_Constraint_P_Xref_Equation

     (Node : Bare_Range_Constraint; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Declarative_Part
     (Self                     : Bare_Declarative_Part;
      Declarative_Part_F_Decls : Bare_Ada_Node_List);

   function Declarative_Part_F_Decls
     (Node : Bare_Declarative_Part) return Bare_Ada_Node_List;

   function Declarative_Part_P_Types_With_Models

     (Node : Bare_Declarative_Part)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Declarative_Part_P_Use_Clauses_Envs

     (Node   : Bare_Declarative_Part;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Returns the envs for all the use clauses declared in this declarative part.

   procedure Initialize_Fields_For_Private_Part
     (Self : Bare_Private_Part; Declarative_Part_F_Decls : Bare_Ada_Node_List);

   function Env_Mappings_35

     (Node : Bare_Private_Part)
return Internal_Env_Assoc;

   function Env_Trans_Parent_36
(Node : Bare_Private_Part)
return Boolean;

   function Bare_Private_Part_Pre_Env_Actions
     (Self : Bare_Private_Part; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Public_Part
     (Self : Bare_Public_Part; Declarative_Part_F_Decls : Bare_Ada_Node_List);

   procedure Initialize_Fields_For_Elsif_Expr_Part
     (Self : Bare_Elsif_Expr_Part; Elsif_Expr_Part_F_Cond_Expr : Bare_Expr;
      Elsif_Expr_Part_F_Then_Expr : Bare_Expr);

   function Elsif_Expr_Part_F_Cond_Expr
     (Node : Bare_Elsif_Expr_Part) return Bare_Expr;

   function Elsif_Expr_Part_F_Then_Expr
     (Node : Bare_Elsif_Expr_Part) return Bare_Expr;

   procedure Initialize_Fields_For_Elsif_Stmt_Part
     (Self : Bare_Elsif_Stmt_Part; Elsif_Stmt_Part_F_Cond_Expr : Bare_Expr;
      Elsif_Stmt_Part_F_Stmts : Bare_Stmt_List);

   function Elsif_Stmt_Part_F_Cond_Expr
     (Node : Bare_Elsif_Stmt_Part) return Bare_Expr;

   function Elsif_Stmt_Part_F_Stmts
     (Node : Bare_Elsif_Stmt_Part) return Bare_Stmt_List;

   procedure Initialize_Fields_For_Expr (Self : Bare_Expr);

   function Expr_P_Type_Val
(Node : Bare_Expr)
return Internal_Entity;

   function Expr_P_Expression_Type

     (Node : Bare_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the declaration corresponding to the type of this expression after
--  name resolution.

   function Expr_P_Is_Static_Expr

     (Node   : Bare_Expr; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this expression is static according to the ARM definition of
--  static. See RM 4.9.

   function Dispatcher_Expr_P_First_Corresponding_Decl

     (Node : Bare_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the first decl that is lexically named like self in self's scope.

   function Expr_P_Eval_As_Int

     (Node : Bare_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Big_Integer_Type;
--  Statically evaluates self, and returns the value of the evaluation as an
--  integer.
--
--  .. note:: In order for a call to this not to raise, the expression needs to
--     be a static expression, as specified in the ARM section 4.9. You can
--     verify whether an expression is static with the ``is_static_expr``
--     property.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--     allow experiments, it is totally unsupported and the API and behavior
--     are very likely to change in the future.

   function Expr_P_Discrete_Range

     (Node : Bare_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;
--  Return the discrete range for this expression, if applicable.

   function Dispatcher_Expr_P_Designated_Env_No_Overloading

     (Node   : Bare_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Returns the lexical environment designated by this name, assuming that this
--  name cannot be overloaded.

   function Dispatcher_Expr_P_Designated_Env

     (Node   : Bare_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Returns the lexical environment designated by this name.
--
--  If this name involves overloading, this will return a combination of the
--  various candidate lexical environments.

   function Expr_P_Env_Elements

     (Node   : Bare_Expr; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Dispatcher_Expr_P_Env_Elements_Impl

     (Node   : Bare_Expr; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Returns the list of annotated elements in the lexical environment that can
--  statically be a match for expr before overloading analysis.

   function Expr_P_Matching_Nodes

     (Node : Bare_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Return the list of AST nodes that can be a match for this expression before
--  overloading analysis.

   function Expr_P_Call_Argument_Equation

     (Node : Bare_Expr; Formal_Type : Internal_Entity_Base_Type_Decl;
      Call_Is_Primitive_Of : Bare_Base_Type_Decl; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;
--  Generate the equation that binds the type_var of this expression given its
--  expected type in the context of a subprogram call. Handles the case where
--  that call is a primitive of the given call_is_primitive_of type.

   function Expr_P_Create_Object_Decl_Wrapper

     (Node        : Bare_Expr; Type_Expr : Internal_Entity_Type_Expr;
      As_Renaming : Boolean)
return Bare_Anonymous_Object_Decl;
--  Create an anonymous object decl in which Self appears either as the default
--  expression (when ``as_renaming`` is False), or as the renamed object (when
--  ``as_renaming`` is True).

   function Expr_P_First_Corresponding_Decl

     (Node : Bare_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the first decl that is lexically named like self in self's scope.

   function Expr_P_Designated_Env_No_Overloading

     (Node   : Bare_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Returns the lexical environment designated by this name, assuming that this
--  name cannot be overloaded.

   function Expr_P_Designated_Env

     (Node   : Bare_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Returns the lexical environment designated by this name.
--
--  If this name involves overloading, this will return a combination of the
--  various candidate lexical environments.

   function Expr_P_Env_Elements_Impl

     (Node   : Bare_Expr; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Returns the list of annotated elements in the lexical environment that can
--  statically be a match for expr before overloading analysis.

   procedure Initialize_Fields_For_Allocator
     (Self : Bare_Allocator; Allocator_F_Subpool : Bare_Name;
      Allocator_F_Type_Or_Expr : Bare_Ada_Node);

   function Allocator_F_Subpool (Node : Bare_Allocator) return Bare_Name;

   function Allocator_F_Type_Or_Expr
     (Node : Bare_Allocator) return Bare_Ada_Node;

   function Allocator_P_Get_Allocated_Type

     (Node : Bare_Allocator; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the allocated type for this allocator.

   function Allocator_P_Xref_Equation

     (Node   : Bare_Allocator; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Base_Aggregate
     (Self : Bare_Base_Aggregate; Base_Aggregate_F_Ancestor_Expr : Bare_Expr;
      Base_Aggregate_F_Assocs : Bare_Assoc_List);

   function Base_Aggregate_F_Ancestor_Expr
     (Node : Bare_Base_Aggregate) return Bare_Expr;

   function Base_Aggregate_F_Assocs
     (Node : Bare_Base_Aggregate) return Bare_Assoc_List;

   function Base_Aggregate_P_Xref_Equation

     (Node   : Bare_Base_Aggregate; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Base_Aggregate_P_Multidim_Root_Aggregate

     (Node   : Bare_Base_Aggregate; R : Integer := 0; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Multidim_Aggregate_Info;
--  Return the root parent aggregate if Self is part of a multidimensional
--  array aggregate (either the root or a sub-aggregate).

   function Base_Aggregate_P_All_Discriminants

     (Node : Bare_Base_Aggregate; Origin : Bare_Ada_Node)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;
--  Return the list of all discriminants that must be associated by this
--  aggregate.

   function Base_Aggregate_P_All_Components

     (Node   : Bare_Base_Aggregate; Origin : Bare_Ada_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;
--  Return the list of all components that must be associated by this
--  aggregate.

   function Base_Aggregate_P_Matched_Discriminants

     (Node   : Bare_Base_Aggregate; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Param_Match_Array_Access;
--  Return the list of all discriminants specified by this aggregate, together
--  with the actual used for it.

   function Base_Aggregate_P_Matched_Components

     (Node   : Bare_Base_Aggregate; Origin : Bare_Ada_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Param_Match_Array_Access;
--  Return the list of all components specified by this aggregate, together
--  with the actual used for it.

   function Base_Aggregate_P_First_Unmatched_Formal

     (Node   : Bare_Base_Aggregate; Origin : Bare_Ada_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Single_Formal;
--  Return the first discriminant or component that is not matched explicitly.

   procedure Initialize_Fields_For_Aggregate
     (Self : Bare_Aggregate; Base_Aggregate_F_Ancestor_Expr : Bare_Expr;
      Base_Aggregate_F_Assocs : Bare_Assoc_List);

   procedure Initialize_Fields_For_Null_Record_Aggregate
     (Self                           : Bare_Null_Record_Aggregate;
      Base_Aggregate_F_Ancestor_Expr : Bare_Expr;
      Base_Aggregate_F_Assocs        : Bare_Assoc_List);

   procedure Initialize_Fields_For_Bin_Op
     (Self : Bare_Bin_Op; Bin_Op_F_Left : Bare_Expr; Bin_Op_F_Op : Bare_Op;
      Bin_Op_F_Right : Bare_Expr);

   function Bin_Op_F_Left (Node : Bare_Bin_Op) return Bare_Expr;

   function Bin_Op_F_Op (Node : Bare_Bin_Op) return Bare_Op;

   function Bin_Op_F_Right (Node : Bare_Bin_Op) return Bare_Expr;

   function Bin_Op_P_Xref_Equation

     (Node   : Bare_Bin_Op; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Dispatcher_Bin_Op_P_No_Overload_Equation

     (Node : Bare_Bin_Op; Origin : Bare_Ada_Node)
return Logic_Equation;
--  When no subprogram is found for this node's operator, use this property to
--  construct the xref equation for this node.

   function Bin_Op_P_No_Overload_Equation

     (Node : Bare_Bin_Op; Origin : Bare_Ada_Node)
return Logic_Equation;
--  When no subprogram is found for this node's operator, use this property to
--  construct the xref equation for this node.

   procedure Initialize_Fields_For_Relation_Op
     (Self        : Bare_Relation_Op; Bin_Op_F_Left : Bare_Expr;
      Bin_Op_F_Op : Bare_Op; Bin_Op_F_Right : Bare_Expr);

   function Relation_Op_P_No_Overload_Equation

     (Node : Bare_Relation_Op; Origin : Bare_Ada_Node)
return Logic_Equation;

   procedure Initialize_Fields_For_Box_Expr (Self : Bare_Box_Expr);

   function Box_Expr_P_Xref_Equation

     (Node   : Bare_Box_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Case_Expr
     (Self              : Bare_Case_Expr; Case_Expr_F_Expr : Bare_Expr;
      Case_Expr_F_Cases : Bare_Case_Expr_Alternative_List);

   function Case_Expr_F_Expr (Node : Bare_Case_Expr) return Bare_Expr;

   function Case_Expr_F_Cases
     (Node : Bare_Case_Expr) return Bare_Case_Expr_Alternative_List;

   function Case_Expr_P_Xref_Equation

     (Node   : Bare_Case_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Case_Expr_Alternative
     (Self                            : Bare_Case_Expr_Alternative;
      Case_Expr_Alternative_F_Choices : Bare_Alternatives_List;
      Case_Expr_Alternative_F_Expr    : Bare_Expr);

   function Case_Expr_Alternative_F_Choices
     (Node : Bare_Case_Expr_Alternative) return Bare_Alternatives_List;

   function Case_Expr_Alternative_F_Expr
     (Node : Bare_Case_Expr_Alternative) return Bare_Expr;

   procedure Initialize_Fields_For_Contract_Cases
     (Self                            : Bare_Contract_Cases;
      Contract_Cases_F_Contract_Cases : Bare_Contract_Case_Assoc_List);

   function Contract_Cases_F_Contract_Cases
     (Node : Bare_Contract_Cases) return Bare_Contract_Case_Assoc_List;

   procedure Initialize_Fields_For_If_Expr
     (Self                   : Bare_If_Expr; If_Expr_F_Cond_Expr : Bare_Expr;
      If_Expr_F_Then_Expr    : Bare_Expr;
      If_Expr_F_Alternatives : Bare_Elsif_Expr_Part_List;
      If_Expr_F_Else_Expr    : Bare_Expr);

   function If_Expr_F_Cond_Expr (Node : Bare_If_Expr) return Bare_Expr;

   function If_Expr_F_Then_Expr (Node : Bare_If_Expr) return Bare_Expr;

   function If_Expr_F_Alternatives
     (Node : Bare_If_Expr) return Bare_Elsif_Expr_Part_List;

   function If_Expr_F_Else_Expr (Node : Bare_If_Expr) return Bare_Expr;

   function If_Expr_P_Xref_Equation

     (Node   : Bare_If_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Membership_Expr
     (Self : Bare_Membership_Expr; Membership_Expr_F_Expr : Bare_Expr;
      Membership_Expr_F_Op               : Bare_Op;
      Membership_Expr_F_Membership_Exprs : Bare_Expr_Alternatives_List);

   function Membership_Expr_F_Expr
     (Node : Bare_Membership_Expr) return Bare_Expr;

   function Membership_Expr_F_Op (Node : Bare_Membership_Expr) return Bare_Op;

   function Membership_Expr_F_Membership_Exprs
     (Node : Bare_Membership_Expr) return Bare_Expr_Alternatives_List;

   function Membership_Expr_P_Xref_Equation

     (Node   : Bare_Membership_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Name (Self : Bare_Name);

   function Name_P_Enclosing_Defining_Name

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  If this name is part of a defining name, return the enclosing defining name
--  node.

   function Name_P_Is_Defining

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return True if this name is part of a defining name.

   function Dispatcher_Name_P_Parent_Scope

     (Node : Bare_Name; Env : Lexical_Env)
return Lexical_Env;
--  Returns the lexical environment that is the scope in which the entity
--  designated by this name is defined/used.

   function Name_P_Name_Is

     (Node : Bare_Name; Sym : Symbol_Type)
return Boolean;
--  Helper. Check that this name matches ``sym``.

   function Name_P_Is_Direct_Call

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return True iff this name represents a call to a subprogram which is
--  referred by its defining name. (i.e. not through a subprogram access).

   function Name_P_Is_Access_Call

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return True iff this name represents a call to subprogram through an access
--  type.

   function Name_P_Is_Call

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Returns True if this Name corresponds to a call.

   function Name_P_Is_Dot_Call

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns True if this Name corresponds to a dot notation call.

   function Name_P_Failsafe_Referenced_Def_Name

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Refd_Def;
--  Failsafe version of ``referenced_defining_name``. Returns a ``RefdDef``,
--  which can be precise, imprecise, or error.

   function Name_P_Referenced_Defining_Name

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Like ``referenced_decl``, but will return the defining identifier for the
--  decl, rather than the basic declaration node itself.

   function Name_P_Gnat_Xref_Decl

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Implementation helper for DefiningName.gnat_xref. TODO: Get rid of that by
--  inlining in DefiningName.gnat_xref.

   function Dispatcher_Name_P_All_Env_Els_Impl

     (Node     : Bare_Name; Seq : Boolean := True;
      Seq_From : Bare_Ada_Node := No_Bare_Ada_Node; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Name_P_All_Env_Elements

     (Node     : Bare_Name; Seq : Boolean := True;
      Seq_From : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info   : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Return all elements in self's scope that are lexically named like Self.

   function Name_P_First_Corresponding_Decl

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;

   function Name_P_Bottom_Up_Name_Equation

     (Node   : Bare_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Name_P_Innermost_Name
(Node : Bare_Name)
return Bare_Name;
--  Helper property. Return the innermost name following the name chain. For
--  example, given::
--
--  A (B) (C) (D) ^-----------^ Self ^-------^ Self.name ^---^ Self.name.name
--
--  `Self.innermost_name` will return the node corresponding to
--  `Self.name.name`.

   function Name_P_Parent_Name_Equation

     (Node : Bare_Name; Typ : Internal_Entity_Base_Type_Decl; Root : Bare_Name;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Construct the xref equation for the chain of parent nested names.

   function Name_P_Subtype_Indication_Equation

     (Node   : Bare_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Name_P_Parent_Name

     (Node : Bare_Name; Stop_At : Bare_Name)
return Bare_Name;
--  Will return the parent name until the stop point.

   function Name_P_Parent_Callexpr

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Call_Expr;
--  If this name qualifies the prefix in a call expression, this returns the
--  corresponding CallExpr node. Return null otherwise. For example::
--
--  C (12, 15); ^ parent_callexpr = <CallExpr>
--
--  A.B.C (12, 15); ^ parent_callexpr = <CallExpr>
--
--  A.B.C (12, 15); ^ parent_callexpr = null
--
--  C (12, 15); ^ parent_callexpr = null

   function Name_P_Is_Range_Attribute
(Node : Bare_Name)
return Boolean;
--  Predicate that returns True if self is a range attribute ref.

   function Dispatcher_Name_P_Scope

     (Node : Bare_Name; Env : Lexical_Env)
return Lexical_Env;
--  Lexical environment this identifier represents. This is similar to
--  designated_env although it handles only cases for child units and it is
--  used only during the environment population pass so it does not return
--  orphan environments.

   function Name_P_Is_Simple_Name
(Node : Bare_Name)
return Boolean;
--  Returns whether Self is a BaseId or a DottedName composed only of BaseIds.

   function Dispatcher_Name_P_Ref_Var
(Node : Bare_Name)
return Logic_Var;
--  This property proxies the logic variable that points to the entity that
--  this name refers to. For example, for a simple dotted name::
--
--  A.B
--
--  The dotted name's ref var is the one of the SingleTokNode B.

   function Dispatcher_Name_P_Subp_Spec_Var

     (Node : Bare_Name)
return Logic_Var;
--  This logic variable holds the specification of the subprogram or subprogram
--  access that is being called by this exact Name.

   function Dispatcher_Name_P_Defines_Subp_Spec_Var

     (Node : Bare_Name)
return Boolean;

   function Name_P_Called_Subp_Spec

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Holder;
--  Return the subprogram specification of the subprogram or subprogram access
--  that is being called by this exact Name, if relevant.

   function Name_P_Referenced_Decl

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Return the declaration this node references after name resolution. If
--  imprecise_fallback is True, errors raised during resolution of the xref
--  equation are catched and a fallback mechanism is triggered, which tries
--  to find the referenced declaration in an ad-hoc way.

   function Name_P_Failsafe_Referenced_Decl

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Refd_Decl;
--  Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which can
--  be precise, imprecise, or error.

   function Name_P_Referenced_Decl_Internal

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Refd_Decl;
--  Return the declaration this node references. Try not to run name res if
--  already resolved. INTERNAL USE ONLY.

   function Dispatcher_Name_P_Designated_Type_Impl

     (Node   : Bare_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Assuming this name designates a type, return this type.
--
--  Since in Ada this can be resolved locally without any non-local analysis,
--  this doesn't use logic equations.

   function Name_P_Name_Designated_Type

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Like SubtypeIndication.designated_type, but on names, since because of
--  Ada's ambiguous grammar, some subtype indications will be parsed as names.

   function Name_P_Is_Static_Subtype

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns whether Self denotes a static subtype or not.

   function Name_P_Name_Designated_Type_Env

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Lexical_Env;

   function Name_P_Referenced_Unit

     (Node : Bare_Name; Kind : Analysis_Unit_Kind)
return Internal_Unit;
--  Shortcut for: `.internal_referenced_unit(kind, True)`.

   function Name_P_Matches
(Node : Bare_Name; N : Bare_Name)
return Boolean;
--  Return whether two names match each other.
--
--  This compares the symbol for Identifier and StringLiteral nodes. We
--  consider that there is no match for all other node kinds.

   function Name_P_Name_Matches

     (Node : Bare_Name; N : Internal_Entity_Name)
return Boolean;
--  Return whether two names match each other.
--
--  This compares the symbol for Identifier and StringLiteral nodes. We
--  consider that there is no match for all other node kinds.

   function Name_P_Use_Package_Name_Designated_Env

     (Node : Bare_Name)
return Lexical_Env;
--  Assuming Self is a name that is the direct child of a UsePackageClause's
--  package name list, return the memoized designated environment for it.

   function Dispatcher_Name_P_Relative_Name

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;
--  Returns the relative name of this instance. For example, for a prefix
--  A.B.C, this will return C.

   function Dispatcher_Name_P_Name_Symbol

     (Node : Bare_Name)
return Symbol_Type;

   function Dispatcher_Name_P_Base_Name

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;
--  Returns the base name of this instance. For example, for a prefix A.B.C,
--  this will return A.B.

   function Name_P_Xref_No_Overloading

     (Node    : Bare_Name; Sequential : Boolean := True;
      All_Els : Boolean := False; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info  : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Simple xref equation for names. Doesn't try to resolve overloads. If
--  ``all_els`` is True, then the name will be bound to the domain of all
--  elements that corresponds. Else, it will be bound to the first one.
--
--  ``sequential`` determines whether the lookup will be sequential or not.

   function Name_P_Is_Prefix
(Node : Bare_Name)
return Boolean;
--  Returns whether Self is the prefix in name. Is used to determine whether
--  lookups on this name should be recursive or not, without having to pass
--  down the information as a function parameter.

   function Name_P_Is_Suffix
(Node : Bare_Name)
return Boolean;
--  Returns whether Self is the suffix in name.

   function Name_P_Is_Operator_Name

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return whether the name that Self designates is an operator.

   function Name_P_Is_Write_Reference

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether this name is a write reference.
--
--  For example, `X` is a write reference in the following cases::
--
--  1. `X := 2;` 2. `X (2) := 2;` 3. `P(F => X)` where F is declared `out` or
--  `in out`. 4. `X'Access`. 5. `X.C := 2`, `R.X := 2`
--
--  .. note:: This is an experimental feature. There might be some discrepancy
--     with the GNAT concept of "write reference".

   function Name_P_Potential_Actuals_For_Dispatch

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Expected_Type_For_Expr_Array_Access;
--  Assuming Self is a call to a subprogram, return an array of pairs
--  (expected_type, expression) for each expression in and around the call
--  that could be used for performing a dynamic dispatch for this call.

   function Name_P_Is_Dispatching_Call

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns True if this Name corresponds to a dispatching call, including:
--
--  - calls done through subprogram access types.
--
--  - calls to dispatching subprograms, in the object-oriented sense.
--
--  .. note:: This is an experimental feature. There might be some discrepancy
--     with the GNAT concept of "dispatching call".

   function Name_P_Is_Static_Call

     (Node   : Bare_Name; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns True if this Name corresponds to a static non-dispatching call. In
--  other words, this will return True if and only if the target of the call is
--  known statically.
--
--  .. note:: This is an experimental feature. There might be some discrepancy
--     with the GNAT concept of "static call".

   function Name_P_As_Single_Tok_Node_Array

     (Node : Bare_Name)
return Bare_Single_Tok_Node_Array_Access;
--  Return the array of SingleTokNode nodes that compose this name.

   function Name_P_As_Symbol_Array

     (Node : Bare_Name)
return Symbol_Type_Array_Access;
--  Turn this name into an array of symbols.
--
--  For instance, a node with name ``A.B.C`` is turned into ``['A', 'B',
--  'C']``.

   function Name_P_Parent_Scope

     (Node : Bare_Name; Env : Lexical_Env)
return Lexical_Env;
--  Returns the lexical environment that is the scope in which the entity
--  designated by this name is defined/used.

   function Name_P_All_Env_Els_Impl

     (Node     : Bare_Name; Seq : Boolean := True;
      Seq_From : Bare_Ada_Node := No_Bare_Ada_Node; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Name_P_Scope

     (Node : Bare_Name; Env : Lexical_Env)
return Lexical_Env;
--  Lexical environment this identifier represents. This is similar to
--  designated_env although it handles only cases for child units and it is
--  used only during the environment population pass so it does not return
--  orphan environments.

   function Name_P_Ref_Var
(Node : Bare_Name)
return Logic_Var;
--  This property proxies the logic variable that points to the entity that
--  this name refers to. For example, for a simple dotted name::
--
--  A.B
--
--  The dotted name's ref var is the one of the SingleTokNode B.

   function Name_P_Subp_Spec_Var
(Node : Bare_Name)
return Logic_Var;
--  This logic variable holds the specification of the subprogram or subprogram
--  access that is being called by this exact Name.

   function Name_P_Defines_Subp_Spec_Var
(Node : Bare_Name)
return Boolean;

   function Name_P_Designated_Type_Impl

     (Node   : Bare_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Assuming this name designates a type, return this type.
--
--  Since in Ada this can be resolved locally without any non-local analysis,
--  this doesn't use logic equations.

   function Name_P_Relative_Name

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;
--  Returns the relative name of this instance. For example, for a prefix
--  A.B.C, this will return C.

   function Name_P_Name_Symbol
(Node : Bare_Name)
return Symbol_Type;

   function Name_P_Base_Name

     (Node : Bare_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;
--  Returns the base name of this instance. For example, for a prefix A.B.C,
--  this will return A.B.

   procedure Initialize_Fields_For_Attribute_Ref
     (Self : Bare_Attribute_Ref; Attribute_Ref_F_Prefix : Bare_Name;
      Attribute_Ref_F_Attribute : Bare_Identifier;
      Attribute_Ref_F_Args      : Bare_Ada_Node);

   function Attribute_Ref_F_Prefix
     (Node : Bare_Attribute_Ref) return Bare_Name;

   function Attribute_Ref_F_Attribute
     (Node : Bare_Attribute_Ref) return Bare_Identifier;

   function Attribute_Ref_F_Args
     (Node : Bare_Attribute_Ref) return Bare_Ada_Node;

   function Attribute_Ref_P_Ref_Var

     (Node : Bare_Attribute_Ref)
return Logic_Var;

   function Attribute_Ref_P_Relative_Name

     (Node   : Bare_Attribute_Ref;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Attribute_Ref_P_Designated_Type_Impl

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Attribute_Ref_P_Args_List

     (Node   : Bare_Attribute_Ref;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Assoc_List;

   function Attribute_Ref_P_Env_Elements_Impl

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Attribute_Ref_P_Is_Access_Attr

     (Node   : Bare_Attribute_Ref;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Attribute_Ref_P_Designated_Env

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Attribute_Ref_P_Designated_Env_Model_Attr

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Attribute_Ref_P_Xref_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Float_Funcs_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Equation for float function attributes with profile (T) -> T with T being
--  any float type.

   function Attribute_Ref_P_Type_Class_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Implementation of the Type_Class attribute, provided for compatibility with
--  DEC 83.

   function Attribute_Ref_P_Storage_Pool_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Equation for the Storage_Pool attribute.

   function Attribute_Ref_P_Model_Attr_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Bind_To_Prefix_Eq

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Result_Attr_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Tag_Attr_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Stream_Attrs_Equation

     (Node   : Bare_Attribute_Ref; Return_Obj : Boolean := False;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Address_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Identity_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Universal_Real_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Standard_Attr_Equation

     (Node : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node)

      return Logic_Equation;

   function Attribute_Ref_P_Succpred_Xref_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Minmax_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Value_Equation

     (Node : Bare_Attribute_Ref; Str_Type : Internal_Entity; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Attribute_Ref_P_Image_Equation

     (Node : Bare_Attribute_Ref; Str_Type : Internal_Entity; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Attribute_Ref_P_Img_Equation

     (Node : Bare_Attribute_Ref; Str_Type : Internal_Entity; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Attribute_Ref_P_Pos_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Val_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Access_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Size_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Array_Attr_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Attribute_Ref_P_Subtype_Attr_Equation

     (Node   : Bare_Attribute_Ref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Generates the xref equation for a an attribute that is defined on any
--  subtype and that evaluates to an universal integer.

   procedure Initialize_Fields_For_Update_Attribute_Ref
     (Self : Bare_Update_Attribute_Ref; Attribute_Ref_F_Prefix : Bare_Name;
      Attribute_Ref_F_Attribute : Bare_Identifier;
      Attribute_Ref_F_Args      : Bare_Ada_Node);

   procedure Initialize_Fields_For_Call_Expr
     (Self               : Bare_Call_Expr; Call_Expr_F_Name : Bare_Name;
      Call_Expr_F_Suffix : Bare_Ada_Node);

   function Call_Expr_F_Name (Node : Bare_Call_Expr) return Bare_Name;

   function Call_Expr_F_Suffix (Node : Bare_Call_Expr) return Bare_Ada_Node;

   function Call_Expr_P_Ref_Var
(Node : Bare_Call_Expr)
return Logic_Var;

   function Call_Expr_P_Subp_Spec_Var
(Node : Bare_Call_Expr)
return Logic_Var;

   function Call_Expr_P_Defines_Subp_Spec_Var

     (Node : Bare_Call_Expr)
return Boolean;

   function Call_Expr_P_Relative_Name

     (Node : Bare_Call_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Call_Expr_P_Designated_Env

     (Node   : Bare_Call_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Call_Expr_P_Env_Elements_Impl

     (Node   : Bare_Call_Expr; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Call_Expr_P_Designated_Type_Impl

     (Node   : Bare_Call_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Call_Expr_P_Params

     (Node : Bare_Call_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Assoc_List;

   function Call_Expr_P_Check_Array_Slice

     (Node   : Bare_Call_Expr; Typ : Internal_Entity_Base_Type_Decl;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return whether this CallExpr can correspond to taking a slice of the given
--  array type.

   function Call_Expr_P_Is_Array_Slice

     (Node : Bare_Call_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Boolean;
--  Return whether this CallExpr is actually an access to a slice of the array
--  denoted by the prefix of this CallExpr.

   function Call_Expr_P_Xref_Equation

     (Node   : Bare_Call_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Call_Expr_P_Type_Conv_Xref_Equation

     (Node   : Bare_Call_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Helper for xref_equation, handles construction of the equation in type
--  conversion cases.

   function Call_Expr_P_Entity_Equation

     (Node : Bare_Call_Expr; S : Internal_Entity_Basic_Decl; Root : Bare_Name;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Call_Expr_P_General_Xref_Equation

     (Node   : Bare_Call_Expr; Root : Bare_Name; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;
--  Helper for xref_equation, handles construction of the equation in
--  subprogram call cases.

   function Call_Expr_P_Operator_Equation

     (Node   : Bare_Call_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;
--  Equation for built-in operators.

   function Call_Expr_P_Subscriptable_Type_Equation

     (Node             : Bare_Call_Expr; Typ : Internal_Entity_Base_Type_Decl;
      Constrain_Params : Boolean := True; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;
--  Construct an equation verifying if Self is conformant to the type
--  designator passed in parameter.

   function Call_Expr_P_Subprogram_Equation

     (Node          : Bare_Call_Expr;
      Subp_Spec     : Internal_Entity_Base_Formal_Param_Holder;
      Dottable_Subp : Boolean; Primitive : Bare_Ada_Node; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Call_Expr_P_Check_For_Type

     (Node   : Bare_Call_Expr; Typ : Internal_Entity_Base_Type_Decl;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Check that self is an appropriate CallExpr for given type, which must be a
--  subscriptable type (eg; a type for which it makes senses to do a call expr
--  on an instance of the type, like an array type, or an access to subprogram
--  type.

   procedure Initialize_Fields_For_Defining_Name
     (Self : Bare_Defining_Name; Defining_Name_F_Name : Bare_Name);

   function Defining_Name_F_Name (Node : Bare_Defining_Name) return Bare_Name;

   function Defining_Name_P_Parent_Scope

     (Node : Bare_Defining_Name; Env : Lexical_Env)
return Lexical_Env;

   function Defining_Name_P_Scope

     (Node : Bare_Defining_Name; Env : Lexical_Env)
return Lexical_Env;

   function Defining_Name_P_Relative_Name

     (Node   : Bare_Defining_Name;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Defining_Name_P_Ref_Var

     (Node : Bare_Defining_Name)
return Logic_Var;

   function Defining_Name_P_Env_Elements_Impl

     (Node   : Bare_Defining_Name; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Defining_Name_P_All_Env_Els_Impl

     (Node     : Bare_Defining_Name; Seq : Boolean := True;
      Seq_From : Bare_Ada_Node := No_Bare_Ada_Node; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Defining_Name_P_Basic_Decl

     (Node   : Bare_Defining_Name;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Returns this DefiningName's basic declaration

   function Defining_Name_P_Find_Refs

     (Node   : Bare_Defining_Name; Root : Internal_Entity;
      Origin : Bare_Ada_Node; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Ref_Result_Array_Access;
--  Find all references to this defining name in the given ``root`` and its
--  children.

   function Defining_Name_P_Is_Referenced_By

     (Node               : Bare_Defining_Name; Id : Internal_Entity_Base_Id;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)
return Ref_Result_Kind;
--  Returns True iff the given node is an identifier referring to Self. Note
--  that this takes into account both direct references as well as potential
--  references.
--
--  Potential references can occur in the context of dispatching calls: an
--  identifier having for direct reference the declaration of an overridable
--  subprogram is considered a potential reference to all subprograms that
--  override it if the identifier appears in a dispatching call.

   function Defining_Name_P_Find_All_References

     (Node : Bare_Defining_Name; Units : Internal_Unit_Array_Access;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)

      return Internal_Ref_Result_Array_Access;
--  Searches all references to this defining name in the given list of units.

   function Defining_Name_P_Find_Matching_Name

     (Node   : Bare_Defining_Name; Bd : Internal_Entity_Basic_Decl;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Helper for navigation proxies. Will return the defining name matching Self
--  on the given BasicDecl.

   function Defining_Name_P_Find_All_Calls

     (Node : Bare_Defining_Name; Units : Internal_Unit_Array_Access;
      Imprecise_Fallback : Boolean              := False;
      E_Info             : Internal_Entity_Info := No_Entity_Info)

      return Internal_Ref_Result_Array_Access;
--  Return the list of all possible calls to the subprogram which Self is the
--  defining name of.
--
--  This will return the name corresponding to the call, excluding the
--  parameters if there are any. For instance, it will return `A` for the
--  `A (B)` call.
--
--  .. note:: This does not yet support calls done inside generics.

   function Defining_Name_P_Next_Part

     (Node   : Bare_Defining_Name;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Like ``BasicDecl.next_part_for_decl`` on a defining name

   function Defining_Name_P_Previous_Part

     (Node   : Bare_Defining_Name;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Like ``BasicDecl.previous_part_for_decl`` on a defining name

   function Defining_Name_P_Canonical_Part

     (Node   : Bare_Defining_Name;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Defining_Name;
--  Like ``BasicDecl.canonical_part`` on a defining name

   function Defining_Name_P_Xref_Equation

     (Node   : Bare_Defining_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Defining_Name_P_Xref_Entry_Point

     (Node : Bare_Defining_Name)
return Boolean;

   procedure Initialize_Fields_For_Discrete_Subtype_Name
     (Self                            : Bare_Discrete_Subtype_Name;
      Discrete_Subtype_Name_F_Subtype : Bare_Discrete_Subtype_Indication);

   function Discrete_Subtype_Name_F_Subtype
     (Node : Bare_Discrete_Subtype_Name)
      return Bare_Discrete_Subtype_Indication;

   procedure Initialize_Fields_For_Dotted_Name
     (Self : Bare_Dotted_Name; Dotted_Name_F_Prefix : Bare_Name;
      Dotted_Name_F_Suffix : Bare_Base_Id);

   function Dotted_Name_F_Prefix (Node : Bare_Dotted_Name) return Bare_Name;

   function Dotted_Name_F_Suffix (Node : Bare_Dotted_Name) return Bare_Base_Id;

   function Dotted_Name_P_Ref_Var
(Node : Bare_Dotted_Name)
return Logic_Var;

   function Dotted_Name_P_Subp_Spec_Var

     (Node : Bare_Dotted_Name)
return Logic_Var;

   function Dotted_Name_P_Defines_Subp_Spec_Var

     (Node : Bare_Dotted_Name)
return Boolean;

   function Dotted_Name_P_Complete

     (Node : Bare_Dotted_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Completion_Item_Array_Access;

   function Dotted_Name_P_Designated_Env_No_Overloading

     (Node   : Bare_Dotted_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Dotted_Name_P_Designated_Env

     (Node   : Bare_Dotted_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Dotted_Name_P_All_Env_Els_Impl

     (Node     : Bare_Dotted_Name; Seq : Boolean := True;
      Seq_From : Bare_Ada_Node := No_Bare_Ada_Node; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Dotted_Name_P_Scope

     (Node : Bare_Dotted_Name; Env : Lexical_Env)
return Lexical_Env;

   function Dotted_Name_P_Parent_Scope

     (Node : Bare_Dotted_Name; Env : Lexical_Env)
return Lexical_Env;

   function Dotted_Name_P_Relative_Name

     (Node : Bare_Dotted_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Dotted_Name_P_Base_Name

     (Node : Bare_Dotted_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Dotted_Name_P_Env_Elements_Impl

     (Node   : Bare_Dotted_Name; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Dotted_Name_P_Designated_Type_Impl

     (Node   : Bare_Dotted_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Dotted_Name_P_Xref_Equation

     (Node   : Bare_Dotted_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_End_Name
     (Self : Bare_End_Name; End_Name_F_Name : Bare_Name);

   function End_Name_F_Name (Node : Bare_End_Name) return Bare_Name;

   function End_Name_P_Parent_Scope

     (Node : Bare_End_Name; Env : Lexical_Env)
return Lexical_Env;

   function End_Name_P_Scope

     (Node : Bare_End_Name; Env : Lexical_Env)
return Lexical_Env;

   function End_Name_P_Relative_Name

     (Node : Bare_End_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function End_Name_P_Ref_Var
(Node : Bare_End_Name)
return Logic_Var;

   function End_Name_P_Env_Elements_Impl

     (Node   : Bare_End_Name; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function End_Name_P_Basic_Decl

     (Node : Bare_End_Name; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl;
--  Returns this EndName's basic declaration

   function End_Name_P_Xref_Equation

     (Node   : Bare_End_Name; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function End_Name_P_Xref_Entry_Point
(Node : Bare_End_Name)
return Boolean;

   procedure Initialize_Fields_For_Explicit_Deref
     (Self : Bare_Explicit_Deref; Explicit_Deref_F_Prefix : Bare_Name);

   function Explicit_Deref_F_Prefix
     (Node : Bare_Explicit_Deref) return Bare_Name;

   function Explicit_Deref_P_Ref_Var

     (Node : Bare_Explicit_Deref)
return Logic_Var;

   function Explicit_Deref_P_Subp_Spec_Var

     (Node : Bare_Explicit_Deref)
return Logic_Var;

   function Explicit_Deref_P_Defines_Subp_Spec_Var

     (Node : Bare_Explicit_Deref)
return Boolean;

   function Explicit_Deref_P_Relative_Name

     (Node   : Bare_Explicit_Deref;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Explicit_Deref_P_Designated_Env

     (Node   : Bare_Explicit_Deref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Explicit_Deref_P_Env_Elements_Impl

     (Node   : Bare_Explicit_Deref; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Explicit_Deref_P_Eq_For_Type

     (Node : Bare_Explicit_Deref; Typ : Internal_Entity_Base_Type_Decl;
      Env  : Lexical_Env; Origin : Bare_Ada_Node)
return Logic_Equation;

   function Explicit_Deref_P_Xref_Equation

     (Node   : Bare_Explicit_Deref; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Explicit_Deref_P_General_Xref_Equation

     (Node   : Bare_Explicit_Deref; Root : Bare_Name := No_Bare_Ada_Node;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Qual_Expr
     (Self               : Bare_Qual_Expr; Qual_Expr_F_Prefix : Bare_Name;
      Qual_Expr_F_Suffix : Bare_Expr);

   function Qual_Expr_F_Prefix (Node : Bare_Qual_Expr) return Bare_Name;

   function Qual_Expr_F_Suffix (Node : Bare_Qual_Expr) return Bare_Expr;

   function Qual_Expr_P_Ref_Var
(Node : Bare_Qual_Expr)
return Logic_Var;

   function Qual_Expr_P_Relative_Name

     (Node : Bare_Qual_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Qual_Expr_P_General_Xref_Equation

     (Node   : Bare_Qual_Expr; Root : Bare_Name := No_Bare_Ada_Node;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Qual_Expr_P_Xref_Equation

     (Node   : Bare_Qual_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Qual_Expr_P_Designated_Type

     (Node : Bare_Qual_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Qual_Expr_P_Designated_Env

     (Node   : Bare_Qual_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   procedure Initialize_Fields_For_Single_Tok_Node
     (Self : Bare_Single_Tok_Node);

   function Single_Tok_Node_P_Relative_Name

     (Node   : Bare_Single_Tok_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Single_Tok_Node;

   function Single_Tok_Node_P_Ref_Var

     (Node : Bare_Single_Tok_Node)
return Logic_Var;

   function Single_Tok_Node_P_Subp_Spec_Var

     (Node : Bare_Single_Tok_Node)
return Logic_Var;

   function Single_Tok_Node_P_Defines_Subp_Spec_Var

     (Node : Bare_Single_Tok_Node)
return Boolean;

   function Single_Tok_Node_P_Sym

     (Node : Bare_Single_Tok_Node)
return Symbol_Type;
--  Shortcut to get the symbol of this node. We keep this short form, even
--  though the public property canonical_text is equivalent because it is
--  very used inside of the internal properties

   function Single_Tok_Node_P_Canonical_Text

     (Node : Bare_Single_Tok_Node)
return Symbol_Type;
--  Return a canonicalized version of this node's text.

   function Single_Tok_Node_P_Env_Get_First_Visible

     (Node        : Bare_Single_Tok_Node; Lex_Env : Lexical_Env;
      Lookup_Type : Lookup_Kind; From_Node : Bare_Ada_Node)

      return Internal_Entity;
--  Like env.get_first, but returning the first visible element in the Ada
--  sense.

   procedure Initialize_Fields_For_Base_Id (Self : Bare_Base_Id);

   function Base_Id_P_Scope

     (Node : Bare_Base_Id; Env : Lexical_Env)
return Lexical_Env;

   function Base_Id_P_Designated_Env_No_Overloading

     (Node   : Bare_Base_Id; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Base_Id_P_Designated_Env

     (Node   : Bare_Base_Id; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Decoupled implementation for designated_env, specifically used by
--  DottedName when the parent is a library level package.

   function Base_Id_P_Pkg_Env

     (Node   : Bare_Base_Id; From_Pkg : Internal_Entity_Basic_Decl;
      Env    : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;
--  Return the lexical environment for this identifier, should it be a package.
--  This method handles resolving to the most visible part of a package -
--  private or body - if necessary. It also unwinds package renamings if
--  necessary.
--
--  If ``inst_from_formal`` is True, we know that bd is a generic package
--  instantiation coming from a rebound formal package, and that we need
--  visibility on the formals.

   function Base_Id_P_Parent_Scope

     (Node : Bare_Base_Id; Env : Lexical_Env)
return Lexical_Env;

   function Base_Id_P_Designated_Type_Impl_Get_Real_Type

     (Node : Bare_Base_Id; N : Internal_Entity)

      return Internal_Entity_Base_Type_Decl;
--  Helper property for ``designated_type_impl``. Returns the actual type
--  defined by the given node, if any.

   function Base_Id_P_Designated_Type_Impl

     (Node   : Bare_Base_Id; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Base_Id_P_Env_Elements_Impl

     (Node   : Bare_Base_Id; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Base_Id_P_All_Env_Els_Impl

     (Node     : Bare_Base_Id; Seq : Boolean := True;
      Seq_From : Bare_Ada_Node := No_Bare_Ada_Node; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;

   function Base_Id_P_Env_Elements_Baseid

     (Node   : Bare_Base_Id; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Array_Access;
--  Decoupled implementation for env_elements_impl, specifically used by
--  designated_env when the parent is a library level package.

   function Base_Id_P_Xref_Equation

     (Node   : Bare_Base_Id; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Base_Id_P_Base_Id_Xref_Equation

     (Node   : Bare_Base_Id; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Char_Literal (Self : Bare_Char_Literal);

   function Char_Literal_P_Xref_Equation

     (Node   : Bare_Char_Literal; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Identifier (Self : Bare_Identifier);

   function Identifier_P_Is_Attr_With_Args

     (Node : Bare_Identifier)
return Boolean;

   procedure Initialize_Fields_For_Op (Self : Bare_Op);

   function Op_P_Subprogram_Symbol
(Node : Bare_Op)
return Symbol_Type;
--  Return the symbol that needs to be used to define an overload of this
--  operator.

   function Op_P_Subprograms_For_Symbol

     (Node : Bare_Op; Sym : Symbol_Type; From_Node : Internal_Entity)

      return Internal_Entity_Basic_Decl_Array_Access;

   function Op_P_Subprograms

     (Node : Bare_Op; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  Return the subprograms corresponding to this operator accessible in the
--  lexical environment.

   function Op_P_Name_Symbol
(Node : Bare_Op)
return Symbol_Type;

   function Op_P_Xref_Equation

     (Node   : Bare_Op; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Op_Abs (Self : Bare_Op_Abs);

   procedure Initialize_Fields_For_Op_And (Self : Bare_Op_And);

   procedure Initialize_Fields_For_Op_And_Then (Self : Bare_Op_And_Then);

   procedure Initialize_Fields_For_Op_Concat (Self : Bare_Op_Concat);

   procedure Initialize_Fields_For_Op_Div (Self : Bare_Op_Div);

   procedure Initialize_Fields_For_Op_Double_Dot (Self : Bare_Op_Double_Dot);

   procedure Initialize_Fields_For_Op_Eq (Self : Bare_Op_Eq);

   procedure Initialize_Fields_For_Op_Gt (Self : Bare_Op_Gt);

   procedure Initialize_Fields_For_Op_Gte (Self : Bare_Op_Gte);

   procedure Initialize_Fields_For_Op_In (Self : Bare_Op_In);

   procedure Initialize_Fields_For_Op_Lt (Self : Bare_Op_Lt);

   procedure Initialize_Fields_For_Op_Lte (Self : Bare_Op_Lte);

   procedure Initialize_Fields_For_Op_Minus (Self : Bare_Op_Minus);

   procedure Initialize_Fields_For_Op_Mod (Self : Bare_Op_Mod);

   procedure Initialize_Fields_For_Op_Mult (Self : Bare_Op_Mult);

   procedure Initialize_Fields_For_Op_Neq (Self : Bare_Op_Neq);

   procedure Initialize_Fields_For_Op_Not (Self : Bare_Op_Not);

   procedure Initialize_Fields_For_Op_Not_In (Self : Bare_Op_Not_In);

   procedure Initialize_Fields_For_Op_Or (Self : Bare_Op_Or);

   procedure Initialize_Fields_For_Op_Or_Else (Self : Bare_Op_Or_Else);

   procedure Initialize_Fields_For_Op_Plus (Self : Bare_Op_Plus);

   procedure Initialize_Fields_For_Op_Pow (Self : Bare_Op_Pow);

   procedure Initialize_Fields_For_Op_Rem (Self : Bare_Op_Rem);

   procedure Initialize_Fields_For_Op_Xor (Self : Bare_Op_Xor);

   procedure Initialize_Fields_For_String_Literal (Self : Bare_String_Literal);

   function String_Literal_P_Xref_Equation

     (Node   : Bare_String_Literal; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Null_Literal (Self : Bare_Null_Literal);

   function Null_Literal_P_Xref_Equation

     (Node   : Bare_Null_Literal; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Num_Literal (Self : Bare_Num_Literal);

   procedure Initialize_Fields_For_Int_Literal (Self : Bare_Int_Literal);

   function Int_Literal_P_Xref_Equation

     (Node   : Bare_Int_Literal; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Real_Literal (Self : Bare_Real_Literal);

   function Real_Literal_P_Xref_Equation

     (Node   : Bare_Real_Literal; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Target_Name (Self : Bare_Target_Name);

   procedure Initialize_Fields_For_Paren_Expr
     (Self : Bare_Paren_Expr; Paren_Expr_F_Expr : Bare_Expr);

   function Paren_Expr_F_Expr (Node : Bare_Paren_Expr) return Bare_Expr;

   function Paren_Expr_P_Xref_Equation

     (Node   : Bare_Paren_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Quantified_Expr
     (Self                         : Bare_Quantified_Expr;
      Quantified_Expr_F_Quantifier : Bare_Quantifier;
      Quantified_Expr_F_Loop_Spec  : Bare_For_Loop_Spec;
      Quantified_Expr_F_Expr       : Bare_Expr);

   function Quantified_Expr_F_Quantifier
     (Node : Bare_Quantified_Expr) return Bare_Quantifier;

   function Quantified_Expr_F_Loop_Spec
     (Node : Bare_Quantified_Expr) return Bare_For_Loop_Spec;

   function Quantified_Expr_F_Expr
     (Node : Bare_Quantified_Expr) return Bare_Expr;

   function Quantified_Expr_P_Xref_Equation

     (Node   : Bare_Quantified_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Raise_Expr
     (Self : Bare_Raise_Expr; Raise_Expr_F_Exception_Name : Bare_Name;
      Raise_Expr_F_Error_Message : Bare_Expr);

   function Raise_Expr_F_Exception_Name
     (Node : Bare_Raise_Expr) return Bare_Name;

   function Raise_Expr_F_Error_Message
     (Node : Bare_Raise_Expr) return Bare_Expr;

   function Raise_Expr_P_Xref_Equation

     (Node   : Bare_Raise_Expr; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Un_Op
     (Self : Bare_Un_Op; Un_Op_F_Op : Bare_Op; Un_Op_F_Expr : Bare_Expr);

   function Un_Op_F_Op (Node : Bare_Un_Op) return Bare_Op;

   function Un_Op_F_Expr (Node : Bare_Un_Op) return Bare_Expr;

   function Un_Op_P_Xref_Equation

     (Node   : Bare_Un_Op; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Handled_Stmts
     (Self : Bare_Handled_Stmts; Handled_Stmts_F_Stmts : Bare_Stmt_List;
      Handled_Stmts_F_Exceptions : Bare_Ada_Node_List);

   function Handled_Stmts_F_Stmts
     (Node : Bare_Handled_Stmts) return Bare_Stmt_List;

   function Handled_Stmts_F_Exceptions
     (Node : Bare_Handled_Stmts) return Bare_Ada_Node_List;

   procedure Initialize_Fields_For_Library_Item
     (Self : Bare_Library_Item; Library_Item_F_Has_Private : Bare_Private_Node;
      Library_Item_F_Item : Bare_Basic_Decl);

   function Library_Item_F_Has_Private
     (Node : Bare_Library_Item) return Bare_Private_Node;

   function Library_Item_F_Item
     (Node : Bare_Library_Item) return Bare_Basic_Decl;

   function Dispatcher_Limited_Node_P_As_Bool

     (Node : Bare_Limited_Node)
return Boolean;
--  Return whether this is an instance of LimitedPresent

   function Limited_Absent_P_As_Bool

     (Node : Bare_Limited_Absent)
return Boolean;

   function Limited_Present_P_As_Bool

     (Node : Bare_Limited_Present)
return Boolean;

   procedure Initialize_Fields_For_For_Loop_Spec
     (Self                        : Bare_For_Loop_Spec;
      For_Loop_Spec_F_Var_Decl    : Bare_For_Loop_Var_Decl;
      For_Loop_Spec_F_Loop_Type   : Bare_Iter_Type;
      For_Loop_Spec_F_Has_Reverse : Bare_Reverse_Node;
      For_Loop_Spec_F_Iter_Expr   : Bare_Ada_Node);

   function For_Loop_Spec_F_Var_Decl
     (Node : Bare_For_Loop_Spec) return Bare_For_Loop_Var_Decl;

   function For_Loop_Spec_F_Loop_Type
     (Node : Bare_For_Loop_Spec) return Bare_Iter_Type;

   function For_Loop_Spec_F_Has_Reverse
     (Node : Bare_For_Loop_Spec) return Bare_Reverse_Node;

   function For_Loop_Spec_F_Iter_Expr
     (Node : Bare_For_Loop_Spec) return Bare_Ada_Node;

   function For_Loop_Spec_P_Iter_Type

     (Node   : Bare_For_Loop_Spec;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function For_Loop_Spec_P_Xref_Equation

     (Node   : Bare_For_Loop_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function For_Loop_Spec_P_Iterator_Xref_Equation

     (Node   : Bare_For_Loop_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function For_Loop_Spec_P_Xref_Entry_Point

     (Node : Bare_For_Loop_Spec)
return Boolean;

   procedure Initialize_Fields_For_While_Loop_Spec
     (Self : Bare_While_Loop_Spec; While_Loop_Spec_F_Expr : Bare_Expr);

   function While_Loop_Spec_F_Expr
     (Node : Bare_While_Loop_Spec) return Bare_Expr;

   function While_Loop_Spec_P_Xref_Equation

     (Node   : Bare_While_Loop_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Mode_P_Is_Writable
(Node : Bare_Mode)
return Boolean;
--  Return whether this mode allows the qualified entity to be written or not.

   function Dispatcher_Not_Null_P_As_Bool

     (Node : Bare_Not_Null)
return Boolean;
--  Return whether this is an instance of NotNullPresent

   function Not_Null_Absent_P_As_Bool

     (Node : Bare_Not_Null_Absent)
return Boolean;

   function Not_Null_Present_P_As_Bool

     (Node : Bare_Not_Null_Present)
return Boolean;

   function Others_Designator_P_Xref_Equation

     (Node : Bare_Others_Designator; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Params
     (Self : Bare_Params; Params_F_Params : Bare_Param_Spec_List);

   function Params_F_Params (Node : Bare_Params) return Bare_Param_Spec_List;

   procedure Initialize_Fields_For_Pragma_Node
     (Self : Bare_Pragma_Node; Pragma_Node_F_Id : Bare_Identifier;
      Pragma_Node_F_Args : Bare_Base_Assoc_List);

   function Pragma_Node_F_Id (Node : Bare_Pragma_Node) return Bare_Identifier;

   function Pragma_Node_F_Args
     (Node : Bare_Pragma_Node) return Bare_Base_Assoc_List;

   function Pragma_Node_P_Xref_Entry_Point

     (Node : Bare_Pragma_Node)
return Boolean;

   function Pragma_Node_P_Xref_Equation

     (Node   : Bare_Pragma_Node; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Pragma_Node_P_Associated_Entity_Name

     (Node : Bare_Pragma_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;

   function Pragma_Node_P_Associated_Decls_Helper

     (Node : Bare_Pragma_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;

   function Pragma_Node_P_Associated_Decls

     (Node : Bare_Pragma_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Basic_Decl_Array_Access;
--  Return an array of ``BasicDecl`` instances associated with this pragma, or
--  an empty array if non applicable.

   procedure Initialize_Fields_For_Prim_Type_Accessor
     (Self : Bare_Prim_Type_Accessor);

   function Prim_Type_Accessor_P_Get_Prim_Type

     (Node : Bare_Prim_Type_Accessor)
return Internal_Entity_Base_Type_Decl;

   function Dispatcher_Private_Node_P_As_Bool

     (Node : Bare_Private_Node)
return Boolean;
--  Return whether this is an instance of PrivatePresent

   function Private_Absent_P_As_Bool

     (Node : Bare_Private_Absent)
return Boolean;

   function Private_Present_P_As_Bool

     (Node : Bare_Private_Present)
return Boolean;

   procedure Initialize_Fields_For_Protected_Def
     (Self                         : Bare_Protected_Def;
      Protected_Def_F_Public_Part  : Bare_Public_Part;
      Protected_Def_F_Private_Part : Bare_Private_Part;
      Protected_Def_F_End_Name     : Bare_End_Name);

   function Protected_Def_F_Public_Part
     (Node : Bare_Protected_Def) return Bare_Public_Part;

   function Protected_Def_F_Private_Part
     (Node : Bare_Protected_Def) return Bare_Private_Part;

   function Protected_Def_F_End_Name
     (Node : Bare_Protected_Def) return Bare_End_Name;

   function Dispatcher_Protected_Node_P_As_Bool

     (Node : Bare_Protected_Node)
return Boolean;
--  Return whether this is an instance of ProtectedPresent

   function Protected_Absent_P_As_Bool

     (Node : Bare_Protected_Absent)
return Boolean;

   function Protected_Present_P_As_Bool

     (Node : Bare_Protected_Present)
return Boolean;

   procedure Initialize_Fields_For_Range_Spec
     (Self : Bare_Range_Spec; Range_Spec_F_Range : Bare_Expr);

   function Range_Spec_F_Range (Node : Bare_Range_Spec) return Bare_Expr;

   function Range_Spec_P_Xref_Equation

     (Node   : Bare_Range_Spec; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Renaming_Clause
     (Self                             : Bare_Renaming_Clause;
      Renaming_Clause_F_Renamed_Object : Bare_Name);

   function Renaming_Clause_F_Renamed_Object
     (Node : Bare_Renaming_Clause) return Bare_Name;

   procedure Initialize_Fields_For_Synthetic_Renaming_Clause
     (Self                             : Bare_Synthetic_Renaming_Clause;
      Renaming_Clause_F_Renamed_Object : Bare_Name);

   function Dispatcher_Reverse_Node_P_As_Bool

     (Node : Bare_Reverse_Node)
return Boolean;
--  Return whether this is an instance of ReversePresent

   function Reverse_Absent_P_As_Bool

     (Node : Bare_Reverse_Absent)
return Boolean;

   function Reverse_Present_P_As_Bool

     (Node : Bare_Reverse_Present)
return Boolean;

   procedure Initialize_Fields_For_Select_When_Part
     (Self : Bare_Select_When_Part; Select_When_Part_F_Cond_Expr : Bare_Expr;
      Select_When_Part_F_Stmts : Bare_Stmt_List);

   function Select_When_Part_F_Cond_Expr
     (Node : Bare_Select_When_Part) return Bare_Expr;

   function Select_When_Part_F_Stmts
     (Node : Bare_Select_When_Part) return Bare_Stmt_List;

   function Select_When_Part_P_Xref_Equation

     (Node : Bare_Select_When_Part; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Stmt_P_Xref_Entry_Point
(Node : Bare_Stmt)
return Boolean;

   procedure Initialize_Fields_For_Accept_Stmt
     (Self : Bare_Accept_Stmt; Accept_Stmt_F_Name : Bare_Identifier;
      Accept_Stmt_F_Entry_Index_Expr : Bare_Expr;
      Accept_Stmt_F_Params           : Bare_Entry_Completion_Formal_Params);

   function Accept_Stmt_F_Name
     (Node : Bare_Accept_Stmt) return Bare_Identifier;

   function Accept_Stmt_F_Entry_Index_Expr
     (Node : Bare_Accept_Stmt) return Bare_Expr;

   function Accept_Stmt_F_Params
     (Node : Bare_Accept_Stmt) return Bare_Entry_Completion_Formal_Params;

   function Accept_Stmt_P_Designated_Entry

     (Node   : Bare_Accept_Stmt; Origin : Bare_Ada_Node; Env : Lexical_Env;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Entry_Decl;

   function Accept_Stmt_P_Xref_Equation

     (Node   : Bare_Accept_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Env_Trans_Parent_135
(Node : Bare_Accept_Stmt)
return Boolean;

   function Bare_Accept_Stmt_Pre_Env_Actions
     (Self : Bare_Accept_Stmt; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Accept_Stmt_With_Stmts
     (Self : Bare_Accept_Stmt_With_Stmts; Accept_Stmt_F_Name : Bare_Identifier;
      Accept_Stmt_F_Entry_Index_Expr    : Bare_Expr;
      Accept_Stmt_F_Params              : Bare_Entry_Completion_Formal_Params;
      Accept_Stmt_With_Stmts_F_Stmts    : Bare_Handled_Stmts;
      Accept_Stmt_With_Stmts_F_End_Name : Bare_End_Name);

   function Accept_Stmt_With_Stmts_F_Stmts
     (Node : Bare_Accept_Stmt_With_Stmts) return Bare_Handled_Stmts;

   function Accept_Stmt_With_Stmts_F_End_Name
     (Node : Bare_Accept_Stmt_With_Stmts) return Bare_End_Name;

   procedure Initialize_Fields_For_Base_Loop_Stmt
     (Self : Bare_Base_Loop_Stmt; Base_Loop_Stmt_F_Spec : Bare_Loop_Spec;
      Base_Loop_Stmt_F_Stmts    : Bare_Stmt_List;
      Base_Loop_Stmt_F_End_Name : Bare_End_Name);

   function Base_Loop_Stmt_F_Spec
     (Node : Bare_Base_Loop_Stmt) return Bare_Loop_Spec;

   function Base_Loop_Stmt_F_Stmts
     (Node : Bare_Base_Loop_Stmt) return Bare_Stmt_List;

   function Base_Loop_Stmt_F_End_Name
     (Node : Bare_Base_Loop_Stmt) return Bare_End_Name;

   function Base_Loop_Stmt_P_Xref_Equation

     (Node   : Bare_Base_Loop_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_For_Loop_Stmt
     (Self : Bare_For_Loop_Stmt; Base_Loop_Stmt_F_Spec : Bare_Loop_Spec;
      Base_Loop_Stmt_F_Stmts    : Bare_Stmt_List;
      Base_Loop_Stmt_F_End_Name : Bare_End_Name);

   function Env_Trans_Parent_132
(Node : Bare_For_Loop_Stmt)
return Boolean;

   function Bare_For_Loop_Stmt_Pre_Env_Actions
     (Self : Bare_For_Loop_Stmt; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Loop_Stmt
     (Self : Bare_Loop_Stmt; Base_Loop_Stmt_F_Spec : Bare_Loop_Spec;
      Base_Loop_Stmt_F_Stmts    : Bare_Stmt_List;
      Base_Loop_Stmt_F_End_Name : Bare_End_Name);

   procedure Initialize_Fields_For_While_Loop_Stmt
     (Self : Bare_While_Loop_Stmt; Base_Loop_Stmt_F_Spec : Bare_Loop_Spec;
      Base_Loop_Stmt_F_Stmts    : Bare_Stmt_List;
      Base_Loop_Stmt_F_End_Name : Bare_End_Name);

   function Block_Stmt_P_Xref_Equation

     (Node   : Bare_Block_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Env_Trans_Parent_133
(Node : Bare_Block_Stmt)
return Boolean;

   function Bare_Block_Stmt_Pre_Env_Actions
     (Self : Bare_Block_Stmt; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Begin_Block
     (Self : Bare_Begin_Block; Begin_Block_F_Stmts : Bare_Handled_Stmts;
      Begin_Block_F_End_Name : Bare_End_Name);

   function Begin_Block_F_Stmts
     (Node : Bare_Begin_Block) return Bare_Handled_Stmts;

   function Begin_Block_F_End_Name
     (Node : Bare_Begin_Block) return Bare_End_Name;

   procedure Initialize_Fields_For_Decl_Block
     (Self : Bare_Decl_Block; Decl_Block_F_Decls : Bare_Declarative_Part;
      Decl_Block_F_Stmts    : Bare_Handled_Stmts;
      Decl_Block_F_End_Name : Bare_End_Name);

   function Decl_Block_F_Decls
     (Node : Bare_Decl_Block) return Bare_Declarative_Part;

   function Decl_Block_F_Stmts
     (Node : Bare_Decl_Block) return Bare_Handled_Stmts;

   function Decl_Block_F_End_Name
     (Node : Bare_Decl_Block) return Bare_End_Name;

   procedure Initialize_Fields_For_Case_Stmt
     (Self                     : Bare_Case_Stmt; Case_Stmt_F_Expr : Bare_Expr;
      Case_Stmt_F_Alternatives : Bare_Case_Stmt_Alternative_List);

   function Case_Stmt_F_Expr (Node : Bare_Case_Stmt) return Bare_Expr;

   function Case_Stmt_F_Alternatives
     (Node : Bare_Case_Stmt) return Bare_Case_Stmt_Alternative_List;

   function Case_Stmt_P_Xref_Equation

     (Node   : Bare_Case_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Extended_Return_Stmt
     (Self                         : Bare_Extended_Return_Stmt;
      Extended_Return_Stmt_F_Decl  : Bare_Extended_Return_Stmt_Object_Decl;
      Extended_Return_Stmt_F_Stmts : Bare_Handled_Stmts);

   function Extended_Return_Stmt_F_Decl
     (Node : Bare_Extended_Return_Stmt)
      return Bare_Extended_Return_Stmt_Object_Decl;

   function Extended_Return_Stmt_F_Stmts
     (Node : Bare_Extended_Return_Stmt) return Bare_Handled_Stmts;

   function Extended_Return_Stmt_P_Xref_Equation

     (Node   : Bare_Extended_Return_Stmt; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Env_Trans_Parent_134

     (Node : Bare_Extended_Return_Stmt)
return Boolean;

   function Bare_Extended_Return_Stmt_Pre_Env_Actions
     (Self                : Bare_Extended_Return_Stmt;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_If_Stmt
     (Self                   : Bare_If_Stmt; If_Stmt_F_Cond_Expr : Bare_Expr;
      If_Stmt_F_Then_Stmts   : Bare_Stmt_List;
      If_Stmt_F_Alternatives : Bare_Elsif_Stmt_Part_List;
      If_Stmt_F_Else_Stmts   : Bare_Stmt_List);

   function If_Stmt_F_Cond_Expr (Node : Bare_If_Stmt) return Bare_Expr;

   function If_Stmt_F_Then_Stmts (Node : Bare_If_Stmt) return Bare_Stmt_List;

   function If_Stmt_F_Alternatives
     (Node : Bare_If_Stmt) return Bare_Elsif_Stmt_Part_List;

   function If_Stmt_F_Else_Stmts (Node : Bare_If_Stmt) return Bare_Stmt_List;

   function If_Stmt_P_Xref_Equation

     (Node   : Bare_If_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Named_Stmt
     (Self : Bare_Named_Stmt; Named_Stmt_F_Decl : Bare_Named_Stmt_Decl;
      Named_Stmt_F_Stmt : Bare_Composite_Stmt);

   function Named_Stmt_F_Decl
     (Node : Bare_Named_Stmt) return Bare_Named_Stmt_Decl;

   function Named_Stmt_F_Stmt
     (Node : Bare_Named_Stmt) return Bare_Composite_Stmt;

   function Named_Stmt_P_Xref_Equation

     (Node   : Bare_Named_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Env_Mappings_130

     (Node : Bare_Named_Stmt)
return Internal_Env_Assoc;

   function Env_Trans_Parent_131
(Node : Bare_Named_Stmt)
return Boolean;

   function Bare_Named_Stmt_Pre_Env_Actions
     (Self : Bare_Named_Stmt; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Select_Stmt
     (Self                      : Bare_Select_Stmt;
      Select_Stmt_F_Guards      : Bare_Select_When_Part_List;
      Select_Stmt_F_Else_Stmts  : Bare_Stmt_List;
      Select_Stmt_F_Abort_Stmts : Bare_Stmt_List);

   function Select_Stmt_F_Guards
     (Node : Bare_Select_Stmt) return Bare_Select_When_Part_List;

   function Select_Stmt_F_Else_Stmts
     (Node : Bare_Select_Stmt) return Bare_Stmt_List;

   function Select_Stmt_F_Abort_Stmts
     (Node : Bare_Select_Stmt) return Bare_Stmt_List;

   function Select_Stmt_P_Xref_Equation

     (Node   : Bare_Select_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Abort_Stmt
     (Self : Bare_Abort_Stmt; Abort_Stmt_F_Names : Bare_Name_List);

   function Abort_Stmt_F_Names (Node : Bare_Abort_Stmt) return Bare_Name_List;

   function Abort_Stmt_P_Xref_Equation

     (Node   : Bare_Abort_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Assign_Stmt
     (Self               : Bare_Assign_Stmt; Assign_Stmt_F_Dest : Bare_Name;
      Assign_Stmt_F_Expr : Bare_Expr);

   function Assign_Stmt_F_Dest (Node : Bare_Assign_Stmt) return Bare_Name;

   function Assign_Stmt_F_Expr (Node : Bare_Assign_Stmt) return Bare_Expr;

   function Assign_Stmt_P_Xref_Equation

     (Node   : Bare_Assign_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Call_Stmt
     (Self : Bare_Call_Stmt; Call_Stmt_F_Call : Bare_Name);

   function Call_Stmt_F_Call (Node : Bare_Call_Stmt) return Bare_Name;

   function Call_Stmt_P_Xref_Equation

     (Node   : Bare_Call_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Delay_Stmt
     (Self : Bare_Delay_Stmt; Delay_Stmt_F_Has_Until : Bare_Until_Node;
      Delay_Stmt_F_Expr : Bare_Expr);

   function Delay_Stmt_F_Has_Until
     (Node : Bare_Delay_Stmt) return Bare_Until_Node;

   function Delay_Stmt_F_Expr (Node : Bare_Delay_Stmt) return Bare_Expr;

   function Delay_Stmt_P_Xref_Equation

     (Node   : Bare_Delay_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Exit_Stmt
     (Self : Bare_Exit_Stmt; Exit_Stmt_F_Loop_Name : Bare_Identifier;
      Exit_Stmt_F_Cond_Expr : Bare_Expr);

   function Exit_Stmt_F_Loop_Name
     (Node : Bare_Exit_Stmt) return Bare_Identifier;

   function Exit_Stmt_F_Cond_Expr (Node : Bare_Exit_Stmt) return Bare_Expr;

   function Exit_Stmt_P_Xref_Equation

     (Node   : Bare_Exit_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Goto_Stmt
     (Self : Bare_Goto_Stmt; Goto_Stmt_F_Label_Name : Bare_Name);

   function Goto_Stmt_F_Label_Name (Node : Bare_Goto_Stmt) return Bare_Name;

   function Goto_Stmt_P_Xref_Equation

     (Node   : Bare_Goto_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Label
     (Self : Bare_Label; Label_F_Decl : Bare_Label_Decl);

   function Label_F_Decl (Node : Bare_Label) return Bare_Label_Decl;

   function Label_P_Xref_Equation

     (Node   : Bare_Label; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Null_Stmt_P_Xref_Equation

     (Node   : Bare_Null_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Raise_Stmt
     (Self : Bare_Raise_Stmt; Raise_Stmt_F_Exception_Name : Bare_Name;
      Raise_Stmt_F_Error_Message : Bare_Expr);

   function Raise_Stmt_F_Exception_Name
     (Node : Bare_Raise_Stmt) return Bare_Name;

   function Raise_Stmt_F_Error_Message
     (Node : Bare_Raise_Stmt) return Bare_Expr;

   function Raise_Stmt_P_Xref_Equation

     (Node   : Bare_Raise_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Requeue_Stmt
     (Self : Bare_Requeue_Stmt; Requeue_Stmt_F_Call_Name : Bare_Name;
      Requeue_Stmt_F_Has_Abort : Bare_Abort_Node);

   function Requeue_Stmt_F_Call_Name
     (Node : Bare_Requeue_Stmt) return Bare_Name;

   function Requeue_Stmt_F_Has_Abort
     (Node : Bare_Requeue_Stmt) return Bare_Abort_Node;

   function Requeue_Stmt_P_Innermost_Entry_Or_Accept_Stmt_Params

     (Node   : Bare_Requeue_Stmt;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Holder;

   function Requeue_Stmt_P_Xref_Equation

     (Node   : Bare_Requeue_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Return_Stmt
     (Self : Bare_Return_Stmt; Return_Stmt_F_Return_Expr : Bare_Expr);

   function Return_Stmt_F_Return_Expr
     (Node : Bare_Return_Stmt) return Bare_Expr;

   function Return_Stmt_P_Subp

     (Node : Bare_Return_Stmt; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Subp_Body;
--  Returns the subprogram this return statement belongs to

   function Return_Stmt_P_Xref_Equation

     (Node   : Bare_Return_Stmt; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Terminate_Alternative_P_Xref_Equation

     (Node   : Bare_Terminate_Alternative; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   procedure Initialize_Fields_For_Subunit
     (Self           : Bare_Subunit; Subunit_F_Name : Bare_Name;
      Subunit_F_Body : Bare_Body_Node);

   function Subunit_F_Name (Node : Bare_Subunit) return Bare_Name;

   function Subunit_F_Body (Node : Bare_Subunit) return Bare_Body_Node;

   function Subunit_P_Env_Hook_Subunit
(Node : Bare_Subunit)
return Boolean;
--  Helper for AdaNode.env_hook. Handle sub-units (separates).

   function Subunit_P_Body_Root

     (Node : Bare_Subunit)
return Internal_Entity_Basic_Decl;
--  Return the body in which this subunit is rooted.

   function Dispatcher_Synchronized_Node_P_As_Bool

     (Node : Bare_Synchronized_Node)
return Boolean;
--  Return whether this is an instance of SynchronizedPresent

   function Synchronized_Absent_P_As_Bool

     (Node : Bare_Synchronized_Absent)
return Boolean;

   function Synchronized_Present_P_As_Bool

     (Node : Bare_Synchronized_Present)
return Boolean;

   function Dispatcher_Tagged_Node_P_As_Bool

     (Node : Bare_Tagged_Node)
return Boolean;
--  Return whether this is an instance of TaggedPresent

   function Tagged_Absent_P_As_Bool
(Node : Bare_Tagged_Absent)
return Boolean;

   function Tagged_Present_P_As_Bool

     (Node : Bare_Tagged_Present)
return Boolean;

   procedure Initialize_Fields_For_Task_Def
     (Self : Bare_Task_Def; Task_Def_F_Interfaces : Bare_Parent_List;
      Task_Def_F_Public_Part  : Bare_Public_Part;
      Task_Def_F_Private_Part : Bare_Private_Part;
      Task_Def_F_End_Name     : Bare_End_Name);

   function Task_Def_F_Interfaces
     (Node : Bare_Task_Def) return Bare_Parent_List;

   function Task_Def_F_Public_Part
     (Node : Bare_Task_Def) return Bare_Public_Part;

   function Task_Def_F_Private_Part
     (Node : Bare_Task_Def) return Bare_Private_Part;

   function Task_Def_F_End_Name (Node : Bare_Task_Def) return Bare_End_Name;

   function Dispatcher_Type_Def_P_Discrete_Range

     (Node : Bare_Type_Def; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;
--  Return the discrete range for this type def, if applicable.

   function Dispatcher_Type_Def_P_Array_Ndims

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;
--  If this designates an array type, return its number of dimensions. Return 0
--  otherwise.

   function Type_Def_P_Is_Real_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a real type or not.

   function Dispatcher_Type_Def_P_Is_Float_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a float type or not.

   function Dispatcher_Type_Def_P_Is_Fixed_Point

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a fixed point type or not.

   function Dispatcher_Type_Def_P_Is_Discrete_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Type_Def_P_Is_Int_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is an integer type or not.

   function Dispatcher_Type_Def_P_Is_Access_Type

     (Node : Bare_Type_Def; Origin : Bare_Ada_Node)
return Boolean;
--  Whether type is an access type or not.

   function Dispatcher_Type_Def_P_Is_Char_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Type_Def_P_Is_Enum_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Type_Def_P_Is_Record_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Dispatcher_Type_Def_P_Accessed_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Dispatcher_Type_Def_P_Is_Tagged_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is tagged.

   function Dispatcher_Type_Def_P_Base_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the base type entity for this derived type definition.

   function Type_Def_P_Base_Types

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Return all the base types for this type (base type + base interfaces)

   function Dispatcher_Type_Def_P_Base_Interfaces

     (Node : Bare_Type_Def; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Return the interfaces this type derives from

   function Type_Def_P_Defining_Env

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Type_Def_P_Containing_Type

     (Node : Bare_Type_Def; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Type_Decl;
--  Return the TypeDecl containing this TypeDef

   function Type_Def_P_Previous_Part

     (Node : Bare_Type_Def; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Def_P_Previous_Part_Env

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Dispatcher_Type_Def_P_Is_Static

     (Node   : Bare_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Def_P_Discrete_Range

     (Node : Bare_Type_Def; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;
--  Return the discrete range for this type def, if applicable.

   function Type_Def_P_Array_Ndims

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;
--  If this designates an array type, return its number of dimensions. Return 0
--  otherwise.

   function Type_Def_P_Is_Float_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a float type or not.

   function Type_Def_P_Is_Fixed_Point

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is a fixed point type or not.

   function Type_Def_P_Is_Discrete_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Def_P_Is_Int_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Whether type is an integer type or not.

   function Type_Def_P_Is_Access_Type

     (Node : Bare_Type_Def; Origin : Bare_Ada_Node)
return Boolean;
--  Whether type is an access type or not.

   function Type_Def_P_Is_Char_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Def_P_Is_Enum_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Def_P_Is_Record_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Def_P_Accessed_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Def_P_Is_Tagged_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Return whether this type is tagged.

   function Type_Def_P_Base_Type

     (Node   : Bare_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the base type entity for this derived type definition.

   function Type_Def_P_Base_Interfaces

     (Node : Bare_Type_Def; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;
--  Return the interfaces this type derives from

   function Type_Def_P_Is_Static

     (Node   : Bare_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Access_Def
     (Self : Bare_Access_Def; Access_Def_F_Has_Not_Null : Bare_Not_Null);

   function Access_Def_F_Has_Not_Null
     (Node : Bare_Access_Def) return Bare_Not_Null;

   function Access_Def_P_Is_Access_Type

     (Node : Bare_Access_Def; Origin : Bare_Ada_Node)
return Boolean;

   procedure Initialize_Fields_For_Access_To_Subp_Def
     (Self                               : Bare_Access_To_Subp_Def;
      Access_Def_F_Has_Not_Null          : Bare_Not_Null;
      Access_To_Subp_Def_F_Has_Protected : Bare_Protected_Node;
      Access_To_Subp_Def_F_Subp_Spec     : Bare_Subp_Spec);

   function Access_To_Subp_Def_F_Has_Protected
     (Node : Bare_Access_To_Subp_Def) return Bare_Protected_Node;

   function Access_To_Subp_Def_F_Subp_Spec
     (Node : Bare_Access_To_Subp_Def) return Bare_Subp_Spec;

   function Access_To_Subp_Def_P_Xref_Equation

     (Node   : Bare_Access_To_Subp_Def; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Access_To_Subp_Def_P_Accessed_Type

     (Node   : Bare_Access_To_Subp_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Env_Trans_Parent_13

     (Node : Bare_Access_To_Subp_Def)
return Boolean;

   function Bare_Access_To_Subp_Def_Pre_Env_Actions
     (Self                : Bare_Access_To_Subp_Def;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Base_Type_Access_Def
     (Self                      : Bare_Base_Type_Access_Def;
      Access_Def_F_Has_Not_Null : Bare_Not_Null);

   procedure Initialize_Fields_For_Anonymous_Type_Access_Def
     (Self                                  : Bare_Anonymous_Type_Access_Def;
      Access_Def_F_Has_Not_Null             : Bare_Not_Null;
      Anonymous_Type_Access_Def_F_Type_Decl : Bare_Base_Type_Decl);

   function Anonymous_Type_Access_Def_F_Type_Decl
     (Node : Bare_Anonymous_Type_Access_Def) return Bare_Base_Type_Decl;

   function Anonymous_Type_Access_Def_P_Accessed_Type

     (Node   : Bare_Anonymous_Type_Access_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   procedure Initialize_Fields_For_Type_Access_Def
     (Self : Bare_Type_Access_Def; Access_Def_F_Has_Not_Null : Bare_Not_Null;
      Type_Access_Def_F_Has_All            : Bare_All_Node;
      Type_Access_Def_F_Has_Constant       : Bare_Constant_Node;
      Type_Access_Def_F_Subtype_Indication : Bare_Subtype_Indication);

   function Type_Access_Def_F_Has_All
     (Node : Bare_Type_Access_Def) return Bare_All_Node;

   function Type_Access_Def_F_Has_Constant
     (Node : Bare_Type_Access_Def) return Bare_Constant_Node;

   function Type_Access_Def_F_Subtype_Indication
     (Node : Bare_Type_Access_Def) return Bare_Subtype_Indication;

   function Type_Access_Def_P_Accessed_Type

     (Node   : Bare_Type_Access_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Access_Def_P_Xref_Equation

     (Node   : Bare_Type_Access_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Array_Type_Def
     (Self                            : Bare_Array_Type_Def;
      Array_Type_Def_F_Indices        : Bare_Array_Indices;
      Array_Type_Def_F_Component_Type : Bare_Component_Def);

   function Array_Type_Def_F_Indices
     (Node : Bare_Array_Type_Def) return Bare_Array_Indices;

   function Array_Type_Def_F_Component_Type
     (Node : Bare_Array_Type_Def) return Bare_Component_Def;

   function Array_Type_Def_P_Comp_Type

     (Node   : Bare_Array_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Returns the type stored as a component in the array.

   function Array_Type_Def_P_Index_Type

     (Node   : Bare_Array_Type_Def; Dim : Integer; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Array_Type_Def_P_Array_Ndims

     (Node   : Bare_Array_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Array_Type_Def_P_Xref_Equation

     (Node   : Bare_Array_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Array_Type_Def_P_Xref_Entry_Point

     (Node : Bare_Array_Type_Def)
return Boolean;

   procedure Initialize_Fields_For_Derived_Type_Def
     (Self                                  : Bare_Derived_Type_Def;
      Derived_Type_Def_F_Has_Abstract       : Bare_Abstract_Node;
      Derived_Type_Def_F_Has_Limited        : Bare_Limited_Node;
      Derived_Type_Def_F_Has_Synchronized   : Bare_Synchronized_Node;
      Derived_Type_Def_F_Subtype_Indication : Bare_Subtype_Indication;
      Derived_Type_Def_F_Interfaces         : Bare_Parent_List;
      Derived_Type_Def_F_Record_Extension   : Bare_Base_Record_Def;
      Derived_Type_Def_F_Has_With_Private   : Bare_With_Private);

   function Derived_Type_Def_F_Has_Abstract
     (Node : Bare_Derived_Type_Def) return Bare_Abstract_Node;

   function Derived_Type_Def_F_Has_Limited
     (Node : Bare_Derived_Type_Def) return Bare_Limited_Node;

   function Derived_Type_Def_F_Has_Synchronized
     (Node : Bare_Derived_Type_Def) return Bare_Synchronized_Node;

   function Derived_Type_Def_F_Subtype_Indication
     (Node : Bare_Derived_Type_Def) return Bare_Subtype_Indication;

   function Derived_Type_Def_F_Interfaces
     (Node : Bare_Derived_Type_Def) return Bare_Parent_List;

   function Derived_Type_Def_F_Record_Extension
     (Node : Bare_Derived_Type_Def) return Bare_Base_Record_Def;

   function Derived_Type_Def_F_Has_With_Private
     (Node : Bare_Derived_Type_Def) return Bare_With_Private;

   function Derived_Type_Def_P_Array_Ndims

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Integer;

   function Derived_Type_Def_P_Base_Type

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Derived_Type_Def_P_Base_Interfaces

     (Node   : Bare_Derived_Type_Def;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Derived_Type_Def_P_Is_Int_Type

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Is_Access_Type

     (Node : Bare_Derived_Type_Def; Origin : Bare_Ada_Node)
return Boolean;

   function Derived_Type_Def_P_Is_Char_Type

     (Node : Bare_Derived_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Is_Float_Type

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Is_Fixed_Point

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Accessed_Type

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Derived_Type_Def_P_Is_Tagged_Type

     (Node : Bare_Derived_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Is_Enum_Type

     (Node : Bare_Derived_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Is_Record_Type

     (Node   : Bare_Derived_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Is_Static

     (Node   : Bare_Derived_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Derived_Type_Def_P_Xref_Equation

     (Node : Bare_Derived_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Derived_Type_Def_P_Discrete_Range

     (Node   : Bare_Derived_Type_Def;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;

   procedure Initialize_Fields_For_Enum_Type_Def
     (Self                          : Bare_Enum_Type_Def;
      Enum_Type_Def_F_Enum_Literals : Bare_Enum_Literal_Decl_List);

   function Enum_Type_Def_F_Enum_Literals
     (Node : Bare_Enum_Type_Def) return Bare_Enum_Literal_Decl_List;

   function Enum_Type_Def_P_Is_Char_Type

     (Node   : Bare_Enum_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Enum_Type_Def_P_Is_Enum_Type

     (Node   : Bare_Enum_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Enum_Type_Def_P_Xref_Equation

     (Node   : Bare_Enum_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Enum_Type_Def_P_Is_Static

     (Node   : Bare_Enum_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Formal_Discrete_Type_Def_P_Xref_Equation

     (Node   : Bare_Formal_Discrete_Type_Def; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Formal_Discrete_Type_Def_P_Is_Discrete_Type

     (Node   : Bare_Formal_Discrete_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Interface_Type_Def
     (Self                                : Bare_Interface_Type_Def;
      Interface_Type_Def_F_Interface_Kind : Bare_Interface_Kind;
      Interface_Type_Def_F_Interfaces     : Bare_Parent_List);

   function Interface_Type_Def_F_Interface_Kind
     (Node : Bare_Interface_Type_Def) return Bare_Interface_Kind;

   function Interface_Type_Def_F_Interfaces
     (Node : Bare_Interface_Type_Def) return Bare_Parent_List;

   function Interface_Type_Def_P_Is_Tagged_Type

     (Node   : Bare_Interface_Type_Def;
      Origin : Bare_Ada_Node        := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Interface_Type_Def_P_Base_Interfaces

     (Node   : Bare_Interface_Type_Def;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl_Array_Access;

   function Interface_Type_Def_P_Xref_Equation

     (Node   : Bare_Interface_Type_Def; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   procedure Initialize_Fields_For_Mod_Int_Type_Def
     (Self : Bare_Mod_Int_Type_Def; Mod_Int_Type_Def_F_Expr : Bare_Expr);

   function Mod_Int_Type_Def_F_Expr
     (Node : Bare_Mod_Int_Type_Def) return Bare_Expr;

   function Mod_Int_Type_Def_P_Is_Int_Type

     (Node   : Bare_Mod_Int_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Mod_Int_Type_Def_P_Xref_Equation

     (Node : Bare_Mod_Int_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Mod_Int_Type_Def_P_Is_Static

     (Node   : Bare_Mod_Int_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Mod_Int_Type_Def_P_Discrete_Range

     (Node   : Bare_Mod_Int_Type_Def;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;

   procedure Initialize_Fields_For_Private_Type_Def
     (Self                            : Bare_Private_Type_Def;
      Private_Type_Def_F_Has_Abstract : Bare_Abstract_Node;
      Private_Type_Def_F_Has_Tagged   : Bare_Tagged_Node;
      Private_Type_Def_F_Has_Limited  : Bare_Limited_Node);

   function Private_Type_Def_F_Has_Abstract
     (Node : Bare_Private_Type_Def) return Bare_Abstract_Node;

   function Private_Type_Def_F_Has_Tagged
     (Node : Bare_Private_Type_Def) return Bare_Tagged_Node;

   function Private_Type_Def_F_Has_Limited
     (Node : Bare_Private_Type_Def) return Bare_Limited_Node;

   function Private_Type_Def_P_Is_Tagged_Type

     (Node : Bare_Private_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Private_Type_Def_P_Xref_Equation

     (Node : Bare_Private_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Real_Type_Def_P_Xref_Equation

     (Node   : Bare_Real_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Real_Type_Def_P_Is_Static

     (Node   : Bare_Real_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Decimal_Fixed_Point_Def
     (Self                             : Bare_Decimal_Fixed_Point_Def;
      Decimal_Fixed_Point_Def_F_Delta  : Bare_Expr;
      Decimal_Fixed_Point_Def_F_Digits : Bare_Expr;
      Decimal_Fixed_Point_Def_F_Range  : Bare_Range_Spec);

   function Decimal_Fixed_Point_Def_F_Delta
     (Node : Bare_Decimal_Fixed_Point_Def) return Bare_Expr;

   function Decimal_Fixed_Point_Def_F_Digits
     (Node : Bare_Decimal_Fixed_Point_Def) return Bare_Expr;

   function Decimal_Fixed_Point_Def_F_Range
     (Node : Bare_Decimal_Fixed_Point_Def) return Bare_Range_Spec;

   function Decimal_Fixed_Point_Def_P_Is_Fixed_Point

     (Node   : Bare_Decimal_Fixed_Point_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Floating_Point_Def
     (Self                            : Bare_Floating_Point_Def;
      Floating_Point_Def_F_Num_Digits : Bare_Expr;
      Floating_Point_Def_F_Range      : Bare_Range_Spec);

   function Floating_Point_Def_F_Num_Digits
     (Node : Bare_Floating_Point_Def) return Bare_Expr;

   function Floating_Point_Def_F_Range
     (Node : Bare_Floating_Point_Def) return Bare_Range_Spec;

   function Floating_Point_Def_P_Is_Float_Type

     (Node   : Bare_Floating_Point_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Ordinary_Fixed_Point_Def
     (Self                             : Bare_Ordinary_Fixed_Point_Def;
      Ordinary_Fixed_Point_Def_F_Delta : Bare_Expr;
      Ordinary_Fixed_Point_Def_F_Range : Bare_Range_Spec);

   function Ordinary_Fixed_Point_Def_F_Delta
     (Node : Bare_Ordinary_Fixed_Point_Def) return Bare_Expr;

   function Ordinary_Fixed_Point_Def_F_Range
     (Node : Bare_Ordinary_Fixed_Point_Def) return Bare_Range_Spec;

   function Ordinary_Fixed_Point_Def_P_Is_Fixed_Point

     (Node   : Bare_Ordinary_Fixed_Point_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   procedure Initialize_Fields_For_Record_Type_Def
     (Self                           : Bare_Record_Type_Def;
      Record_Type_Def_F_Has_Abstract : Bare_Abstract_Node;
      Record_Type_Def_F_Has_Tagged   : Bare_Tagged_Node;
      Record_Type_Def_F_Has_Limited  : Bare_Limited_Node;
      Record_Type_Def_F_Record_Def   : Bare_Base_Record_Def);

   function Record_Type_Def_F_Has_Abstract
     (Node : Bare_Record_Type_Def) return Bare_Abstract_Node;

   function Record_Type_Def_F_Has_Tagged
     (Node : Bare_Record_Type_Def) return Bare_Tagged_Node;

   function Record_Type_Def_F_Has_Limited
     (Node : Bare_Record_Type_Def) return Bare_Limited_Node;

   function Record_Type_Def_F_Record_Def
     (Node : Bare_Record_Type_Def) return Bare_Base_Record_Def;

   function Record_Type_Def_P_Is_Tagged_Type

     (Node : Bare_Record_Type_Def; Origin : Bare_Ada_Node := No_Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Record_Type_Def_P_Is_Record_Type

     (Node   : Bare_Record_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Record_Type_Def_P_Xref_Equation

     (Node   : Bare_Record_Type_Def; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   procedure Initialize_Fields_For_Signed_Int_Type_Def
     (Self                        : Bare_Signed_Int_Type_Def;
      Signed_Int_Type_Def_F_Range : Bare_Range_Spec);

   function Signed_Int_Type_Def_F_Range
     (Node : Bare_Signed_Int_Type_Def) return Bare_Range_Spec;

   function Signed_Int_Type_Def_P_Is_Int_Type

     (Node   : Bare_Signed_Int_Type_Def; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Signed_Int_Type_Def_P_Xref_Equation

     (Node   : Bare_Signed_Int_Type_Def; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Signed_Int_Type_Def_P_Discrete_Range

     (Node   : Bare_Signed_Int_Type_Def;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;

   function Signed_Int_Type_Def_P_Is_Static

     (Node   : Bare_Signed_Int_Type_Def; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;

   function Type_Expr_P_Array_Ndims

     (Node : Bare_Type_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Integer;

   function Type_Expr_P_Type_Name

     (Node : Bare_Type_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Name;
--  Return the name node for this type expression, if applicable, else null

   function Type_Expr_P_Accessed_Type

     (Node   : Bare_Type_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Type_Expr_P_Defining_Env

     (Node   : Bare_Type_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Lexical_Env;

   function Dispatcher_Type_Expr_P_Designated_Type

     (Node   : Bare_Type_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type designated by this type expression.

   function Type_Expr_P_Designated_Type_Decl

     (Node : Bare_Type_Expr; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Returns the type declaration designated by this type expression.

   function Type_Expr_P_Designated_Type_Decl_From

     (Node   : Bare_Type_Expr; Origin_Node : Internal_Entity;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  Return the type declaration designated by this type expression as viewed
--  from the node given by origin_node.

   function Type_Expr_P_Element_Type

     (Node   : Bare_Type_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;
--  If self is an anonymous access, return the accessed type. Otherwise, return
--  the designated type.

   function Type_Expr_P_Canonical_Type

     (Node   : Bare_Type_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   procedure Initialize_Fields_For_Anonymous_Type
     (Self                       : Bare_Anonymous_Type;
      Anonymous_Type_F_Type_Decl : Bare_Anonymous_Type_Decl);

   function Anonymous_Type_F_Type_Decl
     (Node : Bare_Anonymous_Type) return Bare_Anonymous_Type_Decl;

   function Anonymous_Type_P_Designated_Type

     (Node   : Bare_Anonymous_Type; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Anonymous_Type_P_Xref_Equation

     (Node   : Bare_Anonymous_Type; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Anonymous_Type_P_Custom_Id_Text

     (Node   : Bare_Anonymous_Type;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;

   function Enum_Lit_Synth_Type_Expr_P_Designated_Type

     (Node   : Bare_Enum_Lit_Synth_Type_Expr; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Enum_Lit_Synth_Type_Expr_P_Custom_Id_Text

     (Node   : Bare_Enum_Lit_Synth_Type_Expr;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;

   procedure Initialize_Fields_For_Subtype_Indication
     (Self                              : Bare_Subtype_Indication;
      Subtype_Indication_F_Has_Not_Null : Bare_Not_Null;
      Subtype_Indication_F_Name         : Bare_Name;
      Subtype_Indication_F_Constraint   : Bare_Constraint);

   function Subtype_Indication_F_Has_Not_Null
     (Node : Bare_Subtype_Indication) return Bare_Not_Null;

   function Subtype_Indication_F_Name
     (Node : Bare_Subtype_Indication) return Bare_Name;

   function Subtype_Indication_F_Constraint
     (Node : Bare_Subtype_Indication) return Bare_Constraint;

   function Subtype_Indication_P_Designated_Type

     (Node   : Bare_Subtype_Indication; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Subtype_Indication_P_Xref_Equation

     (Node   : Bare_Subtype_Indication; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Subtype_Indication_P_Discrete_Range

     (Node   : Bare_Subtype_Indication;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Discrete_Range;

   function Subtype_Indication_P_Is_Static_Subtype

     (Node   : Bare_Subtype_Indication; Imprecise_Fallback : Boolean := False;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Returns whether Self denotes a static subtype or not.

   function Subtype_Indication_P_Custom_Id_Text

     (Node   : Bare_Subtype_Indication;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Character_Type_Array_Access;

   procedure Initialize_Fields_For_Constrained_Subtype_Indication
     (Self                              : Bare_Constrained_Subtype_Indication;
      Subtype_Indication_F_Has_Not_Null : Bare_Not_Null;
      Subtype_Indication_F_Name         : Bare_Name;
      Subtype_Indication_F_Constraint   : Bare_Constraint);

   procedure Initialize_Fields_For_Discrete_Subtype_Indication
     (Self                              : Bare_Discrete_Subtype_Indication;
      Subtype_Indication_F_Has_Not_Null : Bare_Not_Null;
      Subtype_Indication_F_Name         : Bare_Name;
      Subtype_Indication_F_Constraint   : Bare_Constraint);

   procedure Initialize_Fields_For_Unconstrained_Array_Index
     (Self : Bare_Unconstrained_Array_Index;
      Unconstrained_Array_Index_F_Subtype_Indication : Bare_Subtype_Indication);

   function Unconstrained_Array_Index_F_Subtype_Indication
     (Node : Bare_Unconstrained_Array_Index) return Bare_Subtype_Indication;

   function Unconstrained_Array_Index_P_Designated_Type

     (Node   : Bare_Unconstrained_Array_Index; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Type_Decl;

   function Dispatcher_Until_Node_P_As_Bool

     (Node : Bare_Until_Node)
return Boolean;
--  Return whether this is an instance of UntilPresent

   function Until_Absent_P_As_Bool
(Node : Bare_Until_Absent)
return Boolean;

   function Until_Present_P_As_Bool
(Node : Bare_Until_Present)
return Boolean;

   function Use_Clause_P_Xref_Entry_Point

     (Node : Bare_Use_Clause)
return Boolean;

   procedure Initialize_Fields_For_Use_Package_Clause
     (Self                          : Bare_Use_Package_Clause;
      Use_Package_Clause_F_Packages : Bare_Name_List);

   function Use_Package_Clause_F_Packages
     (Node : Bare_Use_Package_Clause) return Bare_Name_List;

   function Use_Package_Clause_P_Designated_Envs

     (Node : Bare_Use_Package_Clause)
return Lexical_Env_Array_Access;
--  Return the array of designated envs corresponding to each package name.
--
--  It is very important for this property to be memoized, as it is used a lot
--  during lexical environment lookups.

   function Use_Package_Clause_P_Xref_Equation

     (Node   : Bare_Use_Package_Clause; Env : Lexical_Env;
      Origin : Bare_Ada_Node; E_Info : Internal_Entity_Info := No_Entity_Info)

      return Logic_Equation;

   function Ref_Env_Nodes_14

     (Node : Bare_Use_Package_Clause)
return Bare_Ada_Node_Array_Access;

   function Ref_Cond_15
(Node : Bare_Use_Package_Clause)
return Boolean;

   function Bare_Use_Package_Clause_Pre_Env_Actions
     (Self                : Bare_Use_Package_Clause;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Initialize_Fields_For_Use_Type_Clause
     (Self : Bare_Use_Type_Clause; Use_Type_Clause_F_Has_All : Bare_All_Node;
      Use_Type_Clause_F_Types : Bare_Name_List);

   function Use_Type_Clause_F_Has_All
     (Node : Bare_Use_Type_Clause) return Bare_All_Node;

   function Use_Type_Clause_F_Types
     (Node : Bare_Use_Type_Clause) return Bare_Name_List;

   function Use_Type_Clause_P_Xref_Equation

     (Node   : Bare_Use_Type_Clause; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Ref_Env_Nodes_16

     (Node : Bare_Use_Type_Clause)
return Bare_Ada_Node_Array_Access;

   function Env_Dest_17
(Node : Bare_Use_Type_Clause)
return Lexical_Env;

   function Ref_Cond_18
(Node : Bare_Use_Type_Clause)
return Boolean;

   function Bare_Use_Type_Clause_Pre_Env_Actions
     (Self : Bare_Use_Type_Clause; Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only : Boolean := False) return AST_Envs.Lexical_Env;

   procedure Bare_Use_Type_Clause_Post_Env_Actions
     (Self : Bare_Use_Type_Clause; Bound_Env, Root_Env : AST_Envs.Lexical_Env);

   procedure Initialize_Fields_For_Variant
     (Self : Bare_Variant; Variant_F_Choices : Bare_Alternatives_List;
      Variant_F_Components : Bare_Component_List);

   function Variant_F_Choices
     (Node : Bare_Variant) return Bare_Alternatives_List;

   function Variant_F_Components
     (Node : Bare_Variant) return Bare_Component_List;

   function Variant_P_Matches

     (Node   : Bare_Variant; Expr : Internal_Entity_Expr;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Boolean;
--  Check if any choice in the choice list matches expr's value.

   procedure Initialize_Fields_For_Variant_Part
     (Self : Bare_Variant_Part; Variant_Part_F_Discr_Name : Bare_Identifier;
      Variant_Part_F_Variant : Bare_Variant_List);

   function Variant_Part_F_Discr_Name
     (Node : Bare_Variant_Part) return Bare_Identifier;

   function Variant_Part_F_Variant
     (Node : Bare_Variant_Part) return Bare_Variant_List;

   function Variant_Part_P_Xref_Entry_Point

     (Node : Bare_Variant_Part)
return Boolean;

   function Variant_Part_P_Xref_Equation

     (Node   : Bare_Variant_Part; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Variant_Part_P_Get_Components

     (Node          : Bare_Variant_Part;
      Discriminants : Internal_Param_Match_Array_Access;
      E_Info        : Internal_Entity_Info := No_Entity_Info)

      return Internal_Entity_Base_Formal_Param_Decl_Array_Access;
--  Get components for this variant part, depending on the values of
--  discriminants.

   procedure Initialize_Fields_For_With_Clause
     (Self : Bare_With_Clause; With_Clause_F_Has_Limited : Bare_Limited_Node;
      With_Clause_F_Has_Private : Bare_Private_Node;
      With_Clause_F_Packages    : Bare_Name_List);

   function With_Clause_F_Has_Limited
     (Node : Bare_With_Clause) return Bare_Limited_Node;

   function With_Clause_F_Has_Private
     (Node : Bare_With_Clause) return Bare_Private_Node;

   function With_Clause_F_Packages
     (Node : Bare_With_Clause) return Bare_Name_List;

   function With_Clause_P_Xref_Entry_Point

     (Node : Bare_With_Clause)
return Boolean;

   function With_Clause_P_Xref_Equation

     (Node   : Bare_With_Clause; Env : Lexical_Env; Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
return Logic_Equation;

   function Dispatcher_With_Private_P_As_Bool

     (Node : Bare_With_Private)
return Boolean;
--  Return whether this is an instance of WithPrivatePresent

   function With_Private_Absent_P_As_Bool

     (Node : Bare_With_Private_Absent)
return Boolean;

   function With_Private_Present_P_As_Bool

     (Node : Bare_With_Private_Present)
return Boolean;

   function "<" (Left, Right : Internal_Unit) return Boolean;

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : Bare_Ada_Node;
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical environment
   --  rerooting machinery: see Remove_Exiled_Entries and Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Langkit_Support.Vectors (Exiled_Entry);

   type Foreign_Node_Entry is record
      Node : Bare_Ada_Node;
      --  The foreign node that has been added to an analysis unit's lexical
      --  environment.

      Unit : Internal_Unit;
      --  Analysis unit that owns Node
   end record;

   package Foreign_Node_Entry_Vectors is new Langkit_Support.Vectors
     (Foreign_Node_Entry);

   procedure Register_Destroyable (Unit : Internal_Unit; Node : Bare_Ada_Node);
   --  Register Node to be destroyed when Unit is deallocated/reparsed

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access);
   --  Register Env to be destroyed when Unit is deallocated/reparsed

   ------------------------
   --  Memoization state --
   ------------------------

   type Mmz_Property is
     (Mmz_Bare_Ada_Node_Ada_Node_P_Resolve_Names,
      Mmz_Bare_Ada_Node_Ada_Node_P_Resolve_Own_Names,
      Mmz_Bare_Ada_Node_Ada_Node_P_Std_Entity_Implem,
      Mmz_Bare_Base_Aggregate_Base_Aggregate_P_All_Components,
      Mmz_Bare_Base_Aggregate_Base_Aggregate_P_All_Discriminants,
      Mmz_Bare_Base_Aggregate_Base_Aggregate_P_Matched_Components,
      Mmz_Bare_Base_Aggregate_Base_Aggregate_P_Matched_Discriminants,
      Mmz_Bare_Base_Aggregate_Base_Aggregate_P_Multidim_Root_Aggregate,
      Mmz_Bare_Base_Id_Base_Id_P_Env_Elements_Baseid,
      Mmz_Bare_Base_Id_Base_Id_P_Scope,
      Mmz_Bare_Base_Subp_Spec_Base_Subp_Spec_P_Get_Primitive_Subp_Tagged_Type,
      Mmz_Bare_Base_Type_Decl_Base_Type_Decl_P_Anonymous_Access_Type,
      Mmz_Bare_Base_Type_Decl_Base_Type_Decl_P_Classwide_Type_Node,
      Mmz_Bare_Base_Type_Decl_Base_Type_Decl_P_Is_Iterator_Type,
      Mmz_Bare_Base_Type_Decl_Base_Type_Decl_P_Next_Part,
      Mmz_Bare_Base_Type_Decl_Dispatcher_Base_Type_Decl_P_Previous_Part,
      Mmz_Bare_Base_Type_Decl_Dispatcher_Base_Type_Decl_P_Primitives_Env,
      Mmz_Bare_Base_Type_Decl_Base_Type_Decl_P_Private_Completion,
      Mmz_Bare_Base_Type_Decl_Base_Type_Decl_P_Scalar_Base_Subtype_Node,
      Mmz_Bare_Basic_Decl_Basic_Decl_P_Base_Subp_Declarations,
      Mmz_Bare_Classwide_Type_Decl_Classwide_Type_Decl_P_Previous_Part,
      Mmz_Bare_Compilation_Unit_Compilation_Unit_P_Imported_Units,
      Mmz_Bare_Compilation_Unit_Compilation_Unit_P_Unit_Dependencies,
      Mmz_Bare_Compilation_Unit_Compilation_Unit_P_Withed_Units,
      Mmz_Bare_Declarative_Part_Declarative_Part_P_Types_With_Models,
      Mmz_Bare_Defining_Name_Defining_Name_P_Basic_Decl,
      Mmz_Bare_End_Name_End_Name_P_Basic_Decl,
      Mmz_Bare_Enum_Literal_Decl_Enum_Literal_Decl_P_Subp_Decl_Spec,
      Mmz_Bare_Enum_Literal_Decl_Enum_Literal_Decl_P_Synth_Type_Expr,
      Mmz_Bare_Expr_Expr_P_Create_Object_Decl_Wrapper,
      Mmz_Bare_For_Loop_Spec_For_Loop_Spec_P_Iter_Type,
      Mmz_Bare_For_Loop_Var_Decl_For_Loop_Var_Decl_P_Expr_Type,
      Mmz_Bare_Name_Name_P_Is_Prefix, Mmz_Bare_Name_Name_P_Is_Suffix,
      Mmz_Bare_Name_Name_P_Name_Designated_Type_Env,
      Mmz_Bare_Type_Decl_Type_Decl_P_Compute_Primitives_Env,
      Mmz_Bare_Type_Decl_Type_Decl_P_Primitive_Type_Accessor);
   type Mmz_Key_Kind is
     (Mmz_Bare_Ada_Node, Mmz_Bare_Base_Aggregate, Mmz_Bare_Base_Id,
      Mmz_Bare_Base_Subp_Spec, Mmz_Bare_Base_Type_Decl, Mmz_Bare_Basic_Decl,
      Mmz_Bare_Classwide_Type_Decl, Mmz_Bare_Compilation_Unit,
      Mmz_Bare_Declarative_Part, Mmz_Bare_Defining_Name, Mmz_Bare_End_Name,
      Mmz_Bare_Enum_Literal_Decl, Mmz_Bare_Expr, Mmz_Bare_For_Loop_Spec,
      Mmz_Bare_For_Loop_Var_Decl, Mmz_Bare_Name, Mmz_Bare_Type_Decl,
      Mmz_Bare_Type_Expr, Mmz_Boolean, Mmz_Env_Rebindings, Mmz_Integer,
      Mmz_Internal_Entity_Info, Mmz_Internal_Entity_Type_Expr,
      Mmz_Internal_Metadata, Mmz_Lexical_Env, Mmz_Symbol_Type);
   type Mmz_Value_Kind is
     (Mmz_Evaluating, Mmz_Property_Error, Mmz_Bare_Anonymous_Object_Decl,
      Mmz_Bare_Classwide_Type_Decl, Mmz_Bare_Discrete_Base_Subtype_Decl,
      Mmz_Bare_Prim_Type_Accessor, Mmz_Boolean, Mmz_Internal_Entity,
      Mmz_Internal_Entity_Array_Access,
      Mmz_Internal_Entity_Base_Formal_Param_Decl_Array_Access,
      Mmz_Internal_Entity_Base_Subp_Spec, Mmz_Internal_Entity_Base_Type_Decl,
      Mmz_Internal_Entity_Base_Type_Decl_Array_Access,
      Mmz_Internal_Entity_Basic_Decl,
      Mmz_Internal_Entity_Basic_Decl_Array_Access,
      Mmz_Internal_Entity_Compilation_Unit_Array_Access,
      Mmz_Internal_Entity_Enum_Lit_Synth_Type_Expr,
      Mmz_Internal_Multidim_Aggregate_Info,
      Mmz_Internal_Param_Match_Array_Access, Mmz_Lexical_Env);

   type Mmz_Key_Item (Kind : Mmz_Key_Kind := Mmz_Bare_Ada_Node) is record
      case Kind is
         when Mmz_Bare_Ada_Node =>
            As_Bare_Ada_Node : Bare_Ada_Node;
         when Mmz_Bare_Base_Aggregate =>
            As_Bare_Base_Aggregate : Bare_Base_Aggregate;
         when Mmz_Bare_Base_Id =>
            As_Bare_Base_Id : Bare_Base_Id;
         when Mmz_Bare_Base_Subp_Spec =>
            As_Bare_Base_Subp_Spec : Bare_Base_Subp_Spec;
         when Mmz_Bare_Base_Type_Decl =>
            As_Bare_Base_Type_Decl : Bare_Base_Type_Decl;
         when Mmz_Bare_Basic_Decl =>
            As_Bare_Basic_Decl : Bare_Basic_Decl;
         when Mmz_Bare_Classwide_Type_Decl =>
            As_Bare_Classwide_Type_Decl : Bare_Classwide_Type_Decl;
         when Mmz_Bare_Compilation_Unit =>
            As_Bare_Compilation_Unit : Bare_Compilation_Unit;
         when Mmz_Bare_Declarative_Part =>
            As_Bare_Declarative_Part : Bare_Declarative_Part;
         when Mmz_Bare_Defining_Name =>
            As_Bare_Defining_Name : Bare_Defining_Name;
         when Mmz_Bare_End_Name =>
            As_Bare_End_Name : Bare_End_Name;
         when Mmz_Bare_Enum_Literal_Decl =>
            As_Bare_Enum_Literal_Decl : Bare_Enum_Literal_Decl;
         when Mmz_Bare_Expr =>
            As_Bare_Expr : Bare_Expr;
         when Mmz_Bare_For_Loop_Spec =>
            As_Bare_For_Loop_Spec : Bare_For_Loop_Spec;
         when Mmz_Bare_For_Loop_Var_Decl =>
            As_Bare_For_Loop_Var_Decl : Bare_For_Loop_Var_Decl;
         when Mmz_Bare_Name =>
            As_Bare_Name : Bare_Name;
         when Mmz_Bare_Type_Decl =>
            As_Bare_Type_Decl : Bare_Type_Decl;
         when Mmz_Bare_Type_Expr =>
            As_Bare_Type_Expr : Bare_Type_Expr;
         when Mmz_Boolean =>
            As_Boolean : Boolean;
         when Mmz_Env_Rebindings =>
            As_Env_Rebindings : Env_Rebindings;
         when Mmz_Integer =>
            As_Integer : Integer;
         when Mmz_Internal_Entity_Info =>
            As_Internal_Entity_Info : Internal_Entity_Info;
         when Mmz_Internal_Entity_Type_Expr =>
            As_Internal_Entity_Type_Expr : Internal_Entity_Type_Expr;
         when Mmz_Internal_Metadata =>
            As_Internal_Metadata : Internal_Metadata;
         when Mmz_Lexical_Env =>
            As_Lexical_Env : Lexical_Env;
         when Mmz_Symbol_Type =>
            As_Symbol_Type : Symbol_Type;
      end case;
   end record;

   type Mmz_Key_Array is array (Positive range <>) of Mmz_Key_Item;
   type Mmz_Key_Array_Access is access all Mmz_Key_Array;
   type Mmz_Key is record
      Property : Mmz_Property;
      Items    : Mmz_Key_Array_Access;
   end record;

   type Mmz_Value (Kind : Mmz_Value_Kind := Mmz_Evaluating) is record
      case Kind is
         when Mmz_Evaluating | Mmz_Property_Error =>
            null;

         when Mmz_Bare_Anonymous_Object_Decl =>
            As_Bare_Anonymous_Object_Decl : Bare_Anonymous_Object_Decl;
         when Mmz_Bare_Classwide_Type_Decl =>
            As_Bare_Classwide_Type_Decl : Bare_Classwide_Type_Decl;
         when Mmz_Bare_Discrete_Base_Subtype_Decl =>
            As_Bare_Discrete_Base_Subtype_Decl : Bare_Discrete_Base_Subtype_Decl;
         when Mmz_Bare_Prim_Type_Accessor =>
            As_Bare_Prim_Type_Accessor : Bare_Prim_Type_Accessor;
         when Mmz_Boolean =>
            As_Boolean : Boolean;
         when Mmz_Internal_Entity =>
            As_Internal_Entity : Internal_Entity;
         when Mmz_Internal_Entity_Array_Access =>
            As_Internal_Entity_Array_Access : Internal_Entity_Array_Access;
         when Mmz_Internal_Entity_Base_Formal_Param_Decl_Array_Access =>
            As_Internal_Entity_Base_Formal_Param_Decl_Array_Access : Internal_Entity_Base_Formal_Param_Decl_Array_Access;
         when Mmz_Internal_Entity_Base_Subp_Spec =>
            As_Internal_Entity_Base_Subp_Spec : Internal_Entity_Base_Subp_Spec;
         when Mmz_Internal_Entity_Base_Type_Decl =>
            As_Internal_Entity_Base_Type_Decl : Internal_Entity_Base_Type_Decl;
         when Mmz_Internal_Entity_Base_Type_Decl_Array_Access =>
            As_Internal_Entity_Base_Type_Decl_Array_Access : Internal_Entity_Base_Type_Decl_Array_Access;
         when Mmz_Internal_Entity_Basic_Decl =>
            As_Internal_Entity_Basic_Decl : Internal_Entity_Basic_Decl;
         when Mmz_Internal_Entity_Basic_Decl_Array_Access =>
            As_Internal_Entity_Basic_Decl_Array_Access : Internal_Entity_Basic_Decl_Array_Access;
         when Mmz_Internal_Entity_Compilation_Unit_Array_Access =>
            As_Internal_Entity_Compilation_Unit_Array_Access : Internal_Entity_Compilation_Unit_Array_Access;
         when Mmz_Internal_Entity_Enum_Lit_Synth_Type_Expr =>
            As_Internal_Entity_Enum_Lit_Synth_Type_Expr : Internal_Entity_Enum_Lit_Synth_Type_Expr;
         when Mmz_Internal_Multidim_Aggregate_Info =>
            As_Internal_Multidim_Aggregate_Info : Internal_Multidim_Aggregate_Info;
         when Mmz_Internal_Param_Match_Array_Access =>
            As_Internal_Param_Match_Array_Access : Internal_Param_Match_Array_Access;
         when Mmz_Lexical_Env =>
            As_Lexical_Env : Lexical_Env;
      end case;
   end record;

   function Hash (Key : Mmz_Key) return Hash_Type;
   function Equivalent (L, R : Mmz_Key) return Boolean;

   package Memoization_Maps is new Ada.Containers.Hashed_Maps
     (Mmz_Key, Mmz_Value, Hash, Equivalent_Keys => Equivalent);

   procedure Destroy (Map : in out Memoization_Maps.Map);
--  Free all resources stored in a memoization map. This includes destroying
--  ref-count shares the map owns.

   type Memoization_Handle is record
      Key : Mmz_Key;
      --  Key for the memoization

      Cur : Memoization_Maps.Cursor;
      --  If the unit memoization table has an entry for Key, this holds a
      --  cursor to it.

      Cache_Version : Natural := 0;
      --  Version of the unit memoization table at the time Key/Cur were
      --  created. When using this record, if the version has changed, both
      --  Key and Cur are invalid and must be recomputed.
   end record;
--  Wrapper for memoization state, to be used in memoized properties. Please
--  use high-level functions below instead of accessing fields directly.

   function Find_Memoized_Value
     (Unit  :     Internal_Unit; Handle : out Memoization_Handle;
      Value : out Mmz_Value; Create_Key : access function return Mmz_Key)
      return Boolean;
--  Initialize Handle and look for a memoization entry in Unit.Memoization_Map
--  that corresponds to the key in Handle/Create_Key. If one is found, put it
--  in Value and return True. Create such an entry and return False otherwise.

   procedure Add_Memoized_Value
     (Unit  : Internal_Unit; Handle : in out Memoization_Handle;
      Value : Mmz_Value);
--  Insert the Handle.Key/Value entry in UNit.Memoization_Map (replacing the
--  previous entry, if present). If the key in Handle is stale (i.e. caches
--  were reset since it was created), recompute it using Create_Key.

   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   type Destroy_Procedure is access procedure (Object : System.Address);

   type Destroyable_Type is record
      Object : System.Address;
      --  Object to destroy

      Destroy : Destroy_Procedure;
      --  Procedure to destroy Object
   end record;
   --  Simple holder to associate an object to destroy and the procedure to
   --  perform the destruction.

   package Destroyable_Vectors is new Langkit_Support.Vectors
     (Destroyable_Type);

   package Analysis_Unit_Sets is new Langkit_Support.Cheap_Sets
     (Internal_Unit, null);

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => GNATCOLL.VFS.Virtual_File, Element_Type => Internal_Unit,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access;

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type;
   --  Return the given symbol text as a symbol for this context. Raise an
   --  Invalid_Symbol_Error if it is invalid.

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File; Charset : String;
      Rule                : Grammar_Rule) return Internal_Unit;
   --  Create a new special analysis unit, i.e. a unit that is not registered
   --  in Context's unit map.

   function Templates_Unit (Context : Internal_Context) return Internal_Unit;
   --  Return the analysis unit to be used to parse tree rewriting templates.
   --  This creates it if it does not exists yet.

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule);

   package Virtual_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => GNATCOLL.VFS.Virtual_File,
      Equivalent_Keys => "=", "=" => GNATCOLL.VFS."=",
      Hash            => Ada.Strings.Unbounded.Hash);

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String) return GNATCOLL.VFS
     .Virtual_File;
   --  Try to return a canonical filename. This is used to have an
   --  as-unique-as-possible analysis unit identifier.

   type Internal_Unit_Provider is limited interface;
   type Internal_Unit_Provider_Access is
     access all Internal_Unit_Provider'Class;

   procedure Inc_Ref (Provider : in out Internal_Unit_Provider) is abstract;
   --  Create an ownership share for this unit provider.

   function Dec_Ref
     (Provider : in out Internal_Unit_Provider) return Boolean is abstract;
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   function Get_Unit_Filename
     (Provider : Internal_Unit_Provider; Name : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   function Get_Unit
     (Provider : Internal_Unit_Provider; Context : Internal_Context;
      Name     : Text_Type; Kind : Analysis_Unit_Kind; Charset : String := "";
      Reparse  : Boolean := False) return Internal_Unit is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access);

   type Analysis_Context_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Serial_Number : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is re-used.

      --  End of ABI area

      Ref_Count : Natural;

      Units : Units_Maps.Map;
      --  Collection of analysis units loaded in this context

      Filenames : Virtual_File_Maps.Map;
      --  Cache for GNATCOLL.VFS.Virtual_File we create for String filenames.
      --  Re-using older Virtual_File values is useful as this reduces the need
      --  to normalize paths, which is a costly operation.

      Symbols : Symbol_Table;
      --  Symbol table used in this whole context

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      Tab_Stop : Positive;
      --  Tab stop for the lexer to correctly interpret ASCII.HT input
      --  characters.

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

      Parser : Parser_Type;
      --  Main parser type. TODO: If we want to parse in several tasks, we'll
      --  replace that by an array of parsers.

      Discard_Errors_In_Populate_Lexical_Env : Boolean;
      --  See the eponym procedure

      In_Populate_Lexical_Env : Boolean;
      --  Flag to tell whether we are running the Populate_Lexical_Env pass.
      --  When it's on, we must not use the memoization map as the hash of
      --  lexical environment changes when their content changes.

      Logic_Resolution_Timeout : Natural;
      --  If zero, inefficient. Otherwise, designates the maximal number
      --  of steps allowed in the resolution of logic equations before
      --  interrupting the resolution because of timeout. See the
      --  Set_Logic_Resolution_Timeout procedure.

      Cache_Version : Natural;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior
      --  to this, its memoization map should be cleared.

      Reparse_Cache_Version : Natural;
      --  Version number used to invalidate referenced envs caches. It is
      --  incremented only when a unit is reparsed in the context.

      Rewriting_Handle : Rewriting_Handle_Pointer :=
        No_Rewriting_Handle_Pointer;
      --  Rewriting handle for this context's current rewriting session.
      --  No_Rewriting_Handle_Pointer if there is no such session currently.

      Templates_Unit : Internal_Unit := No_Analysis_Unit;
      --  Special analysis unit used only as a containing unit to parse
      --  templates in the context of tree rewriting.

      Released : Boolean;
      --  Whether this context has been released and thus is available in
      --  Context_Pool.

      Current_Call_Depth : Natural := 0;
      --  Number of recursive property calls currently running. This counter is
      --  used as a mitigation against infinite recursions.

      Call_Depth_High_Water_Mark : Natural := 0;
      --  Maximum number of recursive calls seen in this context so far

      Max_Call_Depth : Natural := 0;
      --  Maximum number of recursive calls allowed
   end record;

   type Analysis_Unit_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Unit_Version : Version_Number := 0;
      --  Version for this particular unit. This will be incremented every time
      --  a reparse occurs.

      --  End of ABI area

      Context : Internal_Context;
      --  The owning context for this analysis unit

      AST_Root : Bare_Ada_Node;

      Filename : GNATCOLL.VFS.Virtual_File;
      --  The originating name for this analysis unit. This should be set even
      --  if the analysis unit was parsed from a buffer.

      Charset : Unbounded_String;
      --  The parsing charset for this analysis unit, as a string. If
      --  the charset used actually came from a byte order mark, this
      --  is nevertheless set to the one the user requested.

      TDH : aliased Token_Data_Handler;
      --  The token data handler that handles all token data during parsing and
      --  owns it afterwards.

      Diagnostics : Diagnostics_Vectors.Vector;
      --  The list of diagnostics produced for this analysis unit

      Is_Env_Populated : Boolean;
      --  Whether Populate_Lexical_Env was called on this unit. Used not
      --  to populate multiple times the same unit and hence avoid infinite
      --  populate recursions for circular dependencies.

      Rule : Grammar_Rule;
      --  The grammar rule used to parse this unit

      AST_Mem_Pool : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      Referenced_Units : Analysis_Unit_Sets.Set;
      --  Units that are referenced from this one. Useful for
      --  visibility/computation of the reference graph.

      Exiled_Entries : Exiled_Entry_Vectors.Vector;
      --  Lexical env population for this unit may have added AST nodes it
      --  owns to the lexical environments that belong to other units ("exiled"
      --  entries). For each of these AST nodes, this vector contains an
      --  entry that records the target environment, the AST node and the
      --  corresponding symbol.

      Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector;
      --  This unit owns a set of lexical environments. This vector contains
      --  the list of AST nodes that were added to these environments and that
      --  come from other units.

      Rebindings : aliased Env_Rebindings_Vectors.Vector;
      --  List of rebindings for which Old_Env and/or New_Env belong to this
      --  unit. When this unit gets destroyed or reparsed, these rebindings
      --  need to be destroyed too (see Destroy_Rebindings).

      Memoization_Map : Memoization_Maps.Map;
      --  Mapping of arguments tuple to property result for memoization

      Cache_Version : Natural := 0;
      --  See the eponym field in Analysis_Context_Type
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Internal_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Internal_Unit);

   type Reparsed_Unit is record
      TDH          : Token_Data_Handler;
      Diagnostics  : Diagnostics_Vectors.Vector;
      AST_Mem_Pool : Bump_Ptr_Pool;
      AST_Root     : Bare_Ada_Node;
   end record;
   --  Holder for fields affected by an analysis unit reparse. This makes it
   --  possible to separate the "reparsing" and the "replace" steps.

   procedure Destroy (Reparsed : in out Reparsed_Unit);
   --  Free all resources in Reparsed

   function Basename (Filename : String) return String;
   --  Return the base filename for String

   ----------------------------------------------------
   -- Implementation for analysis context primitives --
   ----------------------------------------------------

   function Create_Context
     (Charset        : String; Unit_Provider : Internal_Unit_Provider_Access;
      With_Trivia    : Boolean; Tab_Stop : Positive;
      Max_Call_Depth : Natural := 1_000) return Internal_Context;
   --  Implementation for Analysis.Create_Context

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File; Charset : String;
      Rule                : Grammar_Rule) return Internal_Unit with
      Pre => not Has_Unit (Context, +Normalized_Filename.Full_Name);
      --  Create a new analysis unit and register it in Context

   function Get_Unit
     (Context : Internal_Context; Filename, Charset : String;
      Reparse : Boolean; Input : Internal_Lexer_Input; Rule : Grammar_Rule)
      return Internal_Unit;
   --  Helper for Get_From_File and Get_From_Buffer. Return the resulting
   --  analysis unit.

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   --  Implementation for Analysis.Has_Unit

   function Get_From_File
     (Context : Internal_Context; Filename : String; Charset : String;
      Reparse : Boolean; Rule : Grammar_Rule) return Internal_Unit with
      Pre => not Reparse or else not Has_Rewriting_Handle (Context);
      --  Implementation for Analysis.Get_From_File

   function Get_From_Buffer
     (Context : Internal_Context; Filename : String; Charset : String;
      Buffer  : String; Rule : Grammar_Rule) return Internal_Unit with
      Pre => not Has_Rewriting_Handle (Context);
      --  Implementation for Analysis.Get_From_Buffer

   function Get_With_Error
     (Context : Internal_Context; Filename : String; Error : Text_Type;
      Charset : String; Rule : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error

   function Get_From_Provider
     (Context : Internal_Context; Name : Text_Type; Kind : Analysis_Unit_Kind;
      Charset : String; Reparse : Boolean) return Internal_Unit with
      Pre => not Reparse or else not Has_Rewriting_Handle (Context);
      --  Implementation for Analysis.Get_From_Provider

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access;
   --  Implementation for Analysis.Unit_Provider

   function Hash (Context : Internal_Context) return Hash_Type;
   --  Implementation for Analysis.Hash

   function Has_With_Trivia (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_With_Trivia

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean);
   --  Implementation for Analysis.Discard_Errors_In_Populate_Lexical_Env

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural);
   --  Implementation for Analysis.Set_Logic_Resolution_Timeout

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_Rewriting_Handle

   procedure Inc_Ref (Context : Internal_Context);
   --  Increment the ref-count of Context. This does nothing if Context is
   --  null.

   procedure Dec_Ref (Context : in out Internal_Context);
   --  Decrement the ref-count of Context, destroying it if the ref-count
   --  reaches zero. This does nothing if Context is null.

   procedure Destroy (Context : in out Internal_Context) with
      Pre => not Has_Rewriting_Handle (Context);
      --  Free all resources allocated for Context

      -------------------------------------------------
      -- Implementation for analysis unit primitives --
      -------------------------------------------------

   function Context (Unit : Internal_Unit) return Internal_Context;
   --  Implementation for Analysis.Context

   function Hash (Unit : Internal_Unit) return Hash_Type;
   --  Implementation for Analysis.Hash

   procedure Reparse (Unit : Internal_Unit; Charset : String);
   --  Implementation for Analysis.Reparse

   procedure Reparse (Unit : Internal_Unit; Charset : String; Buffer : String);
   --  Implementation for Analysis.Reparse

   procedure Populate_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Populate_Lexical_Env

   function Get_Filename (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Filename

   function Get_Charset (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Charset

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean;
   --  Implementation for Analysis.Has_Diagnostics

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array;
   --  Implementation for Analysis.Diagnostics

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String;
   --  Implementation for Analysis.Format_GNU_Diagnostic

   function Root (Unit : Internal_Unit) return Bare_Ada_Node;
   --  Implementation for Analysis.Root

   function First_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.First_Token

   function Last_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.Last_Token

   function Token_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Token_Count

   function Trivia_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Trivia_Count

   function Text (Unit : Internal_Unit) return Text_Type;
   --  Implementation for Analysis.Text

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference;
   --  Implementation for Analysis.Lookup_Token

   procedure Dump_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Dump_Lexical_Env

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean);
   --  Implementation for Analysis.Print

   procedure PP_Trivia (Unit : Internal_Unit);
   --  Implementation for Analysis.PP_Trivia

   procedure Destroy (Unit : in out Internal_Unit);
   --  TODO???

   function Basename (Unit : Internal_Unit) return String;
   --  Return the base filename for Unit

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean);
   --  Invalidate memoization caches. If Invalidate_Envs is true, also
   --  invalidate referenced envs caches.

   procedure Reset_Caches (Unit : Internal_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   procedure Reference_Unit (From, Referenced : Internal_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Is_Referenced_From (Self, Unit : Internal_Unit) return Boolean;

   procedure Do_Parsing
     (Unit   :     Internal_Unit; Input : Internal_Lexer_Input;
      Result : out Reparsed_Unit);
   --  Parse text for Unit using Input and store the result in Result. This
   --  leaves Unit unchanged.

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit);
   --  Update Unit's AST from Reparsed and update stale lexical environment
   --  data after the reparsing of Unit.

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit);
   --  Destroy all destroyables objects in Unit and clear this list in Unit

   procedure Remove_Exiled_Entries (Unit : Internal_Unit);
   --  Remove lexical environment entries referencing nodes in Unit from
   --  lexical environments Unit does not own. Remove foreign node entries
   --  in foreign units that correspond to these exiled entries. Clear
   --  Unit.Exiled_Entries afterwards.

   procedure Extract_Foreign_Nodes
     (Unit          :        Internal_Unit;
      Foreign_Nodes : in out Bare_Ada_Node_Vectors.Vector);
   --  Collect in Foreign_Nodes all foreign nodes in Unit's lexical
   --  environments (i.e. lexical env entries that refer to nodes which belongs
   --  to other analysis units). Remove the exiled entries in foreign units
   --  that correspond to these foreign nodes. Clear Unit.Foreign_Nodes
   --  afterwards.

   procedure Reroot_Foreign_Node (Node : Bare_Ada_Node);
   --  Re-create the lexical env entry for Node. This is to be used in
   --  Flush_Populate_Lexical_Env_Queue, after reparsing removed the
   --  target lexical environment.

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector);
   --  Destroy all rebindings in Rebindings, plus their child rebindings. Note
   --  that children can belong to various analysis units, so this takes care
   --  of removing the destroyed rebindings from each concerned analysis unit's
   --  Rebindings vector.
   --
   --  This require an access parameter in order to avoid aliasing issues in
   --  the body.

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer;
   --  Return the Rewriting_Handle component of Context

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer);
   --  Set the Rewriting_Handle component of Context

   type Node_Safety_Net is record
      Context        : Internal_Context;
      Context_Serial : Version_Number;
      --  Analysis context and serial number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.
   end record;
   --  Information to embed in public APIs, used to check before accessing data
   --  that the said-data is still valid.

   No_Node_Safety_Net : constant Node_Safety_Net := (null, 0, null, 0);

   procedure Check_Safety_Net (Self : Node_Safety_Net);
   --  Check that Self's node is still valid, raising a Stale_Reference_Error
   --  if it is not.

private
   --  We only have a private part to defer the initialization of struct
   --  constants. This allows us to circumvent circularity problems between
   --  arrays and structs.

   No_Entity_Expr : constant Internal_Entity_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Aspect : constant Internal_Aspect :=
     (Exists => False, Node => No_Entity, Value => No_Entity_Expr);

   No_Entity_Basic_Decl : constant Internal_Entity_Basic_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Completion_Item : constant Internal_Completion_Item :=
     (Decl => No_Entity_Basic_Decl, Is_Dot_Call => False);

   No_Discrete_Range : constant Internal_Discrete_Range :=
     (Low_Bound => No_Entity_Expr, High_Bound => No_Entity_Expr);

   No_Doc_Annotation : constant Internal_Doc_Annotation :=
     (Key   => No_Character_Type_Array_Type,
      Value => No_Character_Type_Array_Type);

   No_Entity_Abort_Node : constant Internal_Entity_Abort_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abort_Absent : constant Internal_Entity_Abort_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abort_Present : constant Internal_Entity_Abort_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Stmt : constant Internal_Entity_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Simple_Stmt : constant Internal_Entity_Simple_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abort_Stmt : constant Internal_Entity_Abort_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abstract_Node : constant Internal_Entity_Abstract_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abstract_Absent : constant Internal_Entity_Abstract_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Basic_Subp_Decl : constant Internal_Entity_Basic_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Classic_Subp_Decl : constant Internal_Entity_Classic_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Formal_Subp_Decl : constant Internal_Entity_Formal_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abstract_Formal_Subp_Decl : constant Internal_Entity_Abstract_Formal_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abstract_Present : constant Internal_Entity_Abstract_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Abstract_Subp_Decl : constant Internal_Entity_Abstract_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Composite_Stmt : constant Internal_Entity_Composite_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Accept_Stmt : constant Internal_Entity_Accept_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Accept_Stmt_With_Stmts : constant Internal_Entity_Accept_Stmt_With_Stmts :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Type_Def : constant Internal_Entity_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Access_Def : constant Internal_Entity_Access_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Access_To_Subp_Def : constant Internal_Entity_Access_To_Subp_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Ada_List : constant Internal_Entity_Ada_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Ada_Node_List : constant Internal_Entity_Ada_Node_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Aggregate : constant Internal_Entity_Base_Aggregate :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aggregate : constant Internal_Entity_Aggregate :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Basic_Assoc : constant Internal_Entity_Basic_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aggregate_Assoc : constant Internal_Entity_Aggregate_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aliased_Node : constant Internal_Entity_Aliased_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aliased_Absent : constant Internal_Entity_Aliased_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aliased_Present : constant Internal_Entity_Aliased_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_All_Node : constant Internal_Entity_All_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_All_Absent : constant Internal_Entity_All_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_All_Present : constant Internal_Entity_All_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Allocator : constant Internal_Entity_Allocator :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Alternatives_List : constant Internal_Entity_Alternatives_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Object_Decl : constant Internal_Entity_Object_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Anonymous_Object_Decl : constant Internal_Entity_Anonymous_Object_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Type_Expr : constant Internal_Entity_Type_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Anonymous_Type : constant Internal_Entity_Anonymous_Type :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Type_Access_Def : constant Internal_Entity_Base_Type_Access_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Anonymous_Type_Access_Def : constant Internal_Entity_Anonymous_Type_Access_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Type_Decl : constant Internal_Entity_Base_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Type_Decl : constant Internal_Entity_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Anonymous_Type_Decl : constant Internal_Entity_Anonymous_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Array_Indices : constant Internal_Entity_Array_Indices :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Array_Type_Def : constant Internal_Entity_Array_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aspect_Assoc : constant Internal_Entity_Aspect_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aspect_Assoc_List : constant Internal_Entity_Aspect_Assoc_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aspect_Clause : constant Internal_Entity_Aspect_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Aspect_Spec : constant Internal_Entity_Aspect_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Assign_Stmt : constant Internal_Entity_Assign_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Basic_Assoc_List : constant Internal_Entity_Basic_Assoc_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Assoc_List : constant Internal_Entity_Assoc_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_At_Clause : constant Internal_Entity_At_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Attribute_Def_Clause : constant Internal_Entity_Attribute_Def_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Name : constant Internal_Entity_Name :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Attribute_Ref : constant Internal_Entity_Attribute_Ref :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Assoc : constant Internal_Entity_Base_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Assoc_List : constant Internal_Entity_Base_Assoc_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Formal_Param_Decl : constant Internal_Entity_Base_Formal_Param_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Formal_Param_Holder : constant Internal_Entity_Base_Formal_Param_Holder :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Single_Tok_Node : constant Internal_Entity_Single_Tok_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Id : constant Internal_Entity_Base_Id :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Loop_Stmt : constant Internal_Entity_Base_Loop_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Package_Decl : constant Internal_Entity_Base_Package_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Record_Def : constant Internal_Entity_Base_Record_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Body_Node : constant Internal_Entity_Body_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Subp_Body : constant Internal_Entity_Base_Subp_Body :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Subp_Spec : constant Internal_Entity_Base_Subp_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Base_Subtype_Decl : constant Internal_Entity_Base_Subtype_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Block_Stmt : constant Internal_Entity_Block_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Begin_Block : constant Internal_Entity_Begin_Block :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Bin_Op : constant Internal_Entity_Bin_Op :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Body_Stub : constant Internal_Entity_Body_Stub :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Box_Expr : constant Internal_Entity_Box_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Call_Expr : constant Internal_Entity_Call_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Call_Stmt : constant Internal_Entity_Call_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Case_Expr : constant Internal_Entity_Case_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Case_Expr_Alternative : constant Internal_Entity_Case_Expr_Alternative :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Case_Expr_Alternative_List : constant Internal_Entity_Case_Expr_Alternative_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Case_Stmt : constant Internal_Entity_Case_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Case_Stmt_Alternative : constant Internal_Entity_Case_Stmt_Alternative :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Case_Stmt_Alternative_List : constant Internal_Entity_Case_Stmt_Alternative_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Char_Literal : constant Internal_Entity_Char_Literal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Classwide_Type_Decl : constant Internal_Entity_Classwide_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Compilation_Unit : constant Internal_Entity_Compilation_Unit :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Compilation_Unit_List : constant Internal_Entity_Compilation_Unit_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Component_Clause : constant Internal_Entity_Component_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Component_Decl : constant Internal_Entity_Component_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Component_Def : constant Internal_Entity_Component_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Component_List : constant Internal_Entity_Component_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Concrete_Formal_Subp_Decl : constant Internal_Entity_Concrete_Formal_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constant_Node : constant Internal_Entity_Constant_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constant_Absent : constant Internal_Entity_Constant_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constant_Present : constant Internal_Entity_Constant_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constrained_Array_Indices : constant Internal_Entity_Constrained_Array_Indices :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subtype_Indication : constant Internal_Entity_Subtype_Indication :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constrained_Subtype_Indication : constant Internal_Entity_Constrained_Subtype_Indication :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constraint : constant Internal_Entity_Constraint :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Constraint_List : constant Internal_Entity_Constraint_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Contract_Case_Assoc : constant Internal_Entity_Contract_Case_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Contract_Case_Assoc_List : constant Internal_Entity_Contract_Case_Assoc_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Contract_Cases : constant Internal_Entity_Contract_Cases :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Real_Type_Def : constant Internal_Entity_Real_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Decimal_Fixed_Point_Def : constant Internal_Entity_Decimal_Fixed_Point_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Decl_Block : constant Internal_Entity_Decl_Block :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Decl_List : constant Internal_Entity_Decl_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Declarative_Part : constant Internal_Entity_Declarative_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Defining_Name : constant Internal_Entity_Defining_Name :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Defining_Name_List : constant Internal_Entity_Defining_Name_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Delay_Stmt : constant Internal_Entity_Delay_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Delta_Constraint : constant Internal_Entity_Delta_Constraint :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Derived_Type_Def : constant Internal_Entity_Derived_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Digits_Constraint : constant Internal_Entity_Digits_Constraint :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discrete_Base_Subtype_Decl : constant Internal_Entity_Discrete_Base_Subtype_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discrete_Subtype_Indication : constant Internal_Entity_Discrete_Subtype_Indication :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discrete_Subtype_Name : constant Internal_Entity_Discrete_Subtype_Name :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discriminant_Assoc : constant Internal_Entity_Discriminant_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Identifier_List : constant Internal_Entity_Identifier_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discriminant_Choice_List : constant Internal_Entity_Discriminant_Choice_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discriminant_Constraint : constant Internal_Entity_Discriminant_Constraint :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discriminant_Part : constant Internal_Entity_Discriminant_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discriminant_Spec : constant Internal_Entity_Discriminant_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Discriminant_Spec_List : constant Internal_Entity_Discriminant_Spec_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Dotted_Name : constant Internal_Entity_Dotted_Name :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Elsif_Expr_Part : constant Internal_Entity_Elsif_Expr_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Elsif_Expr_Part_List : constant Internal_Entity_Elsif_Expr_Part_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Elsif_Stmt_Part : constant Internal_Entity_Elsif_Stmt_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Elsif_Stmt_Part_List : constant Internal_Entity_Elsif_Stmt_Part_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_End_Name : constant Internal_Entity_End_Name :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Entry_Body : constant Internal_Entity_Entry_Body :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Entry_Completion_Formal_Params : constant Internal_Entity_Entry_Completion_Formal_Params :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Entry_Decl : constant Internal_Entity_Entry_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Entry_Index_Spec : constant Internal_Entity_Entry_Index_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Entry_Spec : constant Internal_Entity_Entry_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Enum_Lit_Synth_Type_Expr : constant Internal_Entity_Enum_Lit_Synth_Type_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Enum_Literal_Decl : constant Internal_Entity_Enum_Literal_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Enum_Literal_Decl_List : constant Internal_Entity_Enum_Literal_Decl_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Enum_Rep_Clause : constant Internal_Entity_Enum_Rep_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Enum_Subp_Spec : constant Internal_Entity_Enum_Subp_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Enum_Type_Def : constant Internal_Entity_Enum_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Error_Decl : constant Internal_Entity_Error_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Error_Stmt : constant Internal_Entity_Error_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Exception_Decl : constant Internal_Entity_Exception_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Exception_Handler : constant Internal_Entity_Exception_Handler :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Exit_Stmt : constant Internal_Entity_Exit_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Explicit_Deref : constant Internal_Entity_Explicit_Deref :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Expr_List : constant Internal_Entity_Expr_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Expr_Alternatives_List : constant Internal_Entity_Expr_Alternatives_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Expr_Function : constant Internal_Entity_Expr_Function :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Extended_Return_Stmt : constant Internal_Entity_Extended_Return_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Extended_Return_Stmt_Object_Decl : constant Internal_Entity_Extended_Return_Stmt_Object_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Floating_Point_Def : constant Internal_Entity_Floating_Point_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Loop_Spec : constant Internal_Entity_Loop_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_For_Loop_Spec : constant Internal_Entity_For_Loop_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_For_Loop_Stmt : constant Internal_Entity_For_Loop_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_For_Loop_Var_Decl : constant Internal_Entity_For_Loop_Var_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Formal_Discrete_Type_Def : constant Internal_Entity_Formal_Discrete_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Decl : constant Internal_Entity_Generic_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Formal : constant Internal_Entity_Generic_Formal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Formal_Obj_Decl : constant Internal_Entity_Generic_Formal_Obj_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Formal_Package : constant Internal_Entity_Generic_Formal_Package :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Formal_Part : constant Internal_Entity_Generic_Formal_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Formal_Subp_Decl : constant Internal_Entity_Generic_Formal_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Formal_Type_Decl : constant Internal_Entity_Generic_Formal_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Instantiation : constant Internal_Entity_Generic_Instantiation :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Package_Decl : constant Internal_Entity_Generic_Package_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Package_Instantiation : constant Internal_Entity_Generic_Package_Instantiation :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Package_Internal : constant Internal_Entity_Generic_Package_Internal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Renaming_Decl : constant Internal_Entity_Generic_Renaming_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Package_Renaming_Decl : constant Internal_Entity_Generic_Package_Renaming_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Subp_Decl : constant Internal_Entity_Generic_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Subp_Instantiation : constant Internal_Entity_Generic_Subp_Instantiation :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Subp_Internal : constant Internal_Entity_Generic_Subp_Internal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Generic_Subp_Renaming_Decl : constant Internal_Entity_Generic_Subp_Renaming_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Goto_Stmt : constant Internal_Entity_Goto_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Handled_Stmts : constant Internal_Entity_Handled_Stmts :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Identifier : constant Internal_Entity_Identifier :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_If_Expr : constant Internal_Entity_If_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_If_Stmt : constant Internal_Entity_If_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Incomplete_Type_Decl : constant Internal_Entity_Incomplete_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Incomplete_Tagged_Type_Decl : constant Internal_Entity_Incomplete_Tagged_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Index_Constraint : constant Internal_Entity_Index_Constraint :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Num_Literal : constant Internal_Entity_Num_Literal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Int_Literal : constant Internal_Entity_Int_Literal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Interface_Kind : constant Internal_Entity_Interface_Kind :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Interface_Kind_Limited : constant Internal_Entity_Interface_Kind_Limited :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Interface_Kind_Protected : constant Internal_Entity_Interface_Kind_Protected :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Interface_Kind_Synchronized : constant Internal_Entity_Interface_Kind_Synchronized :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Interface_Kind_Task : constant Internal_Entity_Interface_Kind_Task :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Interface_Type_Def : constant Internal_Entity_Interface_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Iter_Type : constant Internal_Entity_Iter_Type :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Iter_Type_In : constant Internal_Entity_Iter_Type_In :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Iter_Type_Of : constant Internal_Entity_Iter_Type_Of :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Known_Discriminant_Part : constant Internal_Entity_Known_Discriminant_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Label : constant Internal_Entity_Label :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Label_Decl : constant Internal_Entity_Label_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Library_Item : constant Internal_Entity_Library_Item :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Limited_Node : constant Internal_Entity_Limited_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Limited_Absent : constant Internal_Entity_Limited_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Limited_Present : constant Internal_Entity_Limited_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Loop_Stmt : constant Internal_Entity_Loop_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Membership_Expr : constant Internal_Entity_Membership_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Mod_Int_Type_Def : constant Internal_Entity_Mod_Int_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Mode : constant Internal_Entity_Mode :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Mode_Default : constant Internal_Entity_Mode_Default :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Mode_In : constant Internal_Entity_Mode_In :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Mode_In_Out : constant Internal_Entity_Mode_In_Out :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Mode_Out : constant Internal_Entity_Mode_Out :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Multi_Dim_Array_Assoc : constant Internal_Entity_Multi_Dim_Array_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Name_List : constant Internal_Entity_Name_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Named_Stmt : constant Internal_Entity_Named_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Named_Stmt_Decl : constant Internal_Entity_Named_Stmt_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Not_Null : constant Internal_Entity_Not_Null :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Not_Null_Absent : constant Internal_Entity_Not_Null_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Not_Null_Present : constant Internal_Entity_Not_Null_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Null_Component_Decl : constant Internal_Entity_Null_Component_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Null_Literal : constant Internal_Entity_Null_Literal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Null_Record_Aggregate : constant Internal_Entity_Null_Record_Aggregate :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Null_Record_Def : constant Internal_Entity_Null_Record_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Null_Stmt : constant Internal_Entity_Null_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Null_Subp_Decl : constant Internal_Entity_Null_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Number_Decl : constant Internal_Entity_Number_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op : constant Internal_Entity_Op :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Abs : constant Internal_Entity_Op_Abs :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_And : constant Internal_Entity_Op_And :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_And_Then : constant Internal_Entity_Op_And_Then :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Concat : constant Internal_Entity_Op_Concat :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Div : constant Internal_Entity_Op_Div :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Double_Dot : constant Internal_Entity_Op_Double_Dot :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Eq : constant Internal_Entity_Op_Eq :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Gt : constant Internal_Entity_Op_Gt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Gte : constant Internal_Entity_Op_Gte :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_In : constant Internal_Entity_Op_In :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Lt : constant Internal_Entity_Op_Lt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Lte : constant Internal_Entity_Op_Lte :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Minus : constant Internal_Entity_Op_Minus :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Mod : constant Internal_Entity_Op_Mod :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Mult : constant Internal_Entity_Op_Mult :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Neq : constant Internal_Entity_Op_Neq :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Not : constant Internal_Entity_Op_Not :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Not_In : constant Internal_Entity_Op_Not_In :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Or : constant Internal_Entity_Op_Or :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Or_Else : constant Internal_Entity_Op_Or_Else :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Plus : constant Internal_Entity_Op_Plus :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Pow : constant Internal_Entity_Op_Pow :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Rem : constant Internal_Entity_Op_Rem :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Op_Xor : constant Internal_Entity_Op_Xor :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Ordinary_Fixed_Point_Def : constant Internal_Entity_Ordinary_Fixed_Point_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Others_Designator : constant Internal_Entity_Others_Designator :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Overriding_Node : constant Internal_Entity_Overriding_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Overriding_Not_Overriding : constant Internal_Entity_Overriding_Not_Overriding :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Overriding_Overriding : constant Internal_Entity_Overriding_Overriding :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Overriding_Unspecified : constant Internal_Entity_Overriding_Unspecified :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Package_Body : constant Internal_Entity_Package_Body :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Package_Body_Stub : constant Internal_Entity_Package_Body_Stub :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Package_Decl : constant Internal_Entity_Package_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Package_Renaming_Decl : constant Internal_Entity_Package_Renaming_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Param_Assoc : constant Internal_Entity_Param_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Param_Spec : constant Internal_Entity_Param_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Param_Spec_List : constant Internal_Entity_Param_Spec_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Params : constant Internal_Entity_Params :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Paren_Expr : constant Internal_Entity_Paren_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Parent_List : constant Internal_Entity_Parent_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Pragma_Argument_Assoc : constant Internal_Entity_Pragma_Argument_Assoc :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Pragma_Node : constant Internal_Entity_Pragma_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Pragma_Node_List : constant Internal_Entity_Pragma_Node_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Prim_Type_Accessor : constant Internal_Entity_Prim_Type_Accessor :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Private_Node : constant Internal_Entity_Private_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Private_Absent : constant Internal_Entity_Private_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Private_Part : constant Internal_Entity_Private_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Private_Present : constant Internal_Entity_Private_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Private_Type_Def : constant Internal_Entity_Private_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Node : constant Internal_Entity_Protected_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Absent : constant Internal_Entity_Protected_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Body : constant Internal_Entity_Protected_Body :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Body_Stub : constant Internal_Entity_Protected_Body_Stub :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Def : constant Internal_Entity_Protected_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Present : constant Internal_Entity_Protected_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Protected_Type_Decl : constant Internal_Entity_Protected_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Public_Part : constant Internal_Entity_Public_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Qual_Expr : constant Internal_Entity_Qual_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Quantified_Expr : constant Internal_Entity_Quantified_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Quantifier : constant Internal_Entity_Quantifier :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Quantifier_All : constant Internal_Entity_Quantifier_All :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Quantifier_Some : constant Internal_Entity_Quantifier_Some :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Raise_Expr : constant Internal_Entity_Raise_Expr :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Raise_Stmt : constant Internal_Entity_Raise_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Range_Constraint : constant Internal_Entity_Range_Constraint :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Range_Spec : constant Internal_Entity_Range_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Real_Literal : constant Internal_Entity_Real_Literal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Record_Def : constant Internal_Entity_Record_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Record_Rep_Clause : constant Internal_Entity_Record_Rep_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Record_Type_Def : constant Internal_Entity_Record_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Relation_Op : constant Internal_Entity_Relation_Op :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Renaming_Clause : constant Internal_Entity_Renaming_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Requeue_Stmt : constant Internal_Entity_Requeue_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Return_Stmt : constant Internal_Entity_Return_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Reverse_Node : constant Internal_Entity_Reverse_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Reverse_Absent : constant Internal_Entity_Reverse_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Reverse_Present : constant Internal_Entity_Reverse_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Select_Stmt : constant Internal_Entity_Select_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Select_When_Part : constant Internal_Entity_Select_When_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Select_When_Part_List : constant Internal_Entity_Select_When_Part_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Signed_Int_Type_Def : constant Internal_Entity_Signed_Int_Type_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Single_Protected_Decl : constant Internal_Entity_Single_Protected_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Single_Task_Decl : constant Internal_Entity_Single_Task_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Task_Type_Decl : constant Internal_Entity_Task_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Single_Task_Type_Decl : constant Internal_Entity_Single_Task_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Stmt_List : constant Internal_Entity_Stmt_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_String_Literal : constant Internal_Entity_String_Literal :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Body : constant Internal_Entity_Subp_Body :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Body_Stub : constant Internal_Entity_Subp_Body_Stub :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Decl : constant Internal_Entity_Subp_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Kind : constant Internal_Entity_Subp_Kind :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Kind_Function : constant Internal_Entity_Subp_Kind_Function :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Kind_Procedure : constant Internal_Entity_Subp_Kind_Procedure :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Renaming_Decl : constant Internal_Entity_Subp_Renaming_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subp_Spec : constant Internal_Entity_Subp_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subtype_Decl : constant Internal_Entity_Subtype_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Subunit : constant Internal_Entity_Subunit :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Synchronized_Node : constant Internal_Entity_Synchronized_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Synchronized_Absent : constant Internal_Entity_Synchronized_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Synchronized_Present : constant Internal_Entity_Synchronized_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Synth_Anonymous_Type_Decl : constant Internal_Entity_Synth_Anonymous_Type_Decl :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Synthetic_Renaming_Clause : constant Internal_Entity_Synthetic_Renaming_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Tagged_Node : constant Internal_Entity_Tagged_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Tagged_Absent : constant Internal_Entity_Tagged_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Tagged_Present : constant Internal_Entity_Tagged_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Target_Name : constant Internal_Entity_Target_Name :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Task_Body : constant Internal_Entity_Task_Body :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Task_Body_Stub : constant Internal_Entity_Task_Body_Stub :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Task_Def : constant Internal_Entity_Task_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Terminate_Alternative : constant Internal_Entity_Terminate_Alternative :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Type_Access_Def : constant Internal_Entity_Type_Access_Def :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Un_Op : constant Internal_Entity_Un_Op :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Unconstrained_Array_Index : constant Internal_Entity_Unconstrained_Array_Index :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Unconstrained_Array_Index_List : constant Internal_Entity_Unconstrained_Array_Index_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Unconstrained_Array_Indices : constant Internal_Entity_Unconstrained_Array_Indices :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Unknown_Discriminant_Part : constant Internal_Entity_Unknown_Discriminant_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Until_Node : constant Internal_Entity_Until_Node :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Until_Absent : constant Internal_Entity_Until_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Until_Present : constant Internal_Entity_Until_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Update_Attribute_Ref : constant Internal_Entity_Update_Attribute_Ref :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Use_Clause : constant Internal_Entity_Use_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Use_Package_Clause : constant Internal_Entity_Use_Package_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Use_Type_Clause : constant Internal_Entity_Use_Type_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Variant : constant Internal_Entity_Variant :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Variant_List : constant Internal_Entity_Variant_List :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_Variant_Part : constant Internal_Entity_Variant_Part :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_While_Loop_Spec : constant Internal_Entity_While_Loop_Spec :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_While_Loop_Stmt : constant Internal_Entity_While_Loop_Stmt :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_With_Clause : constant Internal_Entity_With_Clause :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_With_Private : constant Internal_Entity_With_Private :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_With_Private_Absent : constant Internal_Entity_With_Private_Absent :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Entity_With_Private_Present : constant Internal_Entity_With_Private_Present :=
     (Node => No_Bare_Ada_Node, Info => No_Entity_Info);

   No_Env_Assoc : constant Internal_Env_Assoc :=
     (Key      => null, Val => No_Bare_Ada_Node, Dest_Env => Empty_Env,
      Metadata => No_Metadata);

   No_Eval_Discrete_Range : constant Internal_Eval_Discrete_Range :=
     (Low_Bound => No_Big_Integer, High_Bound => No_Big_Integer);

   No_Expected_Type_For_Expr : constant Internal_Expected_Type_For_Expr :=
     (Expected_Type => No_Entity_Type_Expr, Expr => No_Entity_Expr);

   No_Logic_Val_Result : constant Internal_Logic_Val_Result :=
     (Success => False, Value => No_Entity);

   No_Multidim_Aggregate_Info : constant Internal_Multidim_Aggregate_Info :=
     (Agg  => No_Entity_Base_Aggregate, Typ => No_Entity_Base_Type_Decl,
      Rank => 0);

   No_Param_Actual : constant Internal_Param_Actual :=
     (Param => No_Entity_Defining_Name, Actual => No_Entity_Expr);

   No_Single_Actual : constant Internal_Single_Actual :=
     (Name => No_Bare_Ada_Node, Assoc => No_Entity_Basic_Assoc);

   No_Single_Formal : constant Internal_Single_Formal :=
     (Name => No_Entity_Defining_Name,
      Spec => No_Entity_Base_Formal_Param_Decl);

   No_Param_Match : constant Internal_Param_Match :=
     (Has_Matched => False, Actual => No_Single_Actual,
      Formal      => No_Single_Formal);

   No_Ref_Result : constant Internal_Ref_Result :=
     (Ref => No_Entity_Base_Id, Kind => Noref);

   No_Refd_Decl : constant Internal_Refd_Decl :=
     (Decl => No_Entity_Basic_Decl, Kind => Noref);

   No_Refd_Def : constant Internal_Refd_Def :=
     (Def_Name => No_Entity_Defining_Name, Kind => Noref);

   No_Substitution : constant Internal_Substitution :=
     (From_Decl  => No_Entity_Basic_Decl, To_Value => No_Big_Integer,
      Value_Type => No_Entity_Base_Type_Decl);

end Libadalang.Implementation;
