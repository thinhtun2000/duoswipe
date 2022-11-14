with Ada.Containers;
private with Ada.Finalization;
with Ada.Strings.Unbounded;
private with Ada.Unchecked_Deallocation;

with GNATCOLL.Refcount;

private with Langkit_Support.Boxes;

with Libadalang.Common; use Libadalang.Common;
use Libadalang.Common.Token_Data_Handlers;
private with Libadalang.Implementation;
private with Libadalang.Debug;

--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process a unit: first create an
--  analysis context with ``Create_Context``, then get analysis units out of
--  it using the ``Get_From_*`` functions.

package Libadalang.Analysis is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context is tagged private;
   --  This type represents a context for all source analysis. This is the
   --  first type you need to create to use Libadalang. It will contain the
   --  results of all analysis, and is the main holder for all the data.
   --
   --  You can create several analysis contexts if you need to, which enables
   --  you, for example to:
   --
   --  * analyze several different projects at the same time;
   --
   --  * analyze different parts of the same projects in parallel.
   --
   --  In the current design, contexts always keep all of their analysis units
   --  allocated. If you need to get this memory released, the only option at
   --  your disposal is to destroy your analysis context instance.

   type Analysis_Unit is tagged private;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.

   No_Analysis_Context : constant Analysis_Context;
   --  Special value to mean the absence of analysis context

   No_Analysis_Unit : constant Analysis_Unit;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   ---------------
   -- AST nodes --
   ---------------

   type Ada_Node is tagged private;
   --  Data type for all nodes. Nodes are assembled to make up a tree. See the
   --  node primitives below to inspect such trees.
   --
   --  Unlike for contexts and units, this type has weak-reference semantics:
   --  keeping a reference to a node has no effect on the decision to keep the
   --  unit that it owns allocated. This means that once all references to the
   --  context and units related to a node are dropped, the context and its
   --  units are deallocated and the node becomes a stale reference: most
   --  operations on it will raise a ``Stale_Reference_Error``.
   --
   --  Note that since reparsing an analysis unit deallocates all the nodes
   --  it contains, this operation makes all reference to these nodes stale
   --  as well.
   --
   --  Root node class for the Ada syntax tree.
   type Expr is new Ada_Node with private;
   --  Base class for expressions.
   type Basic_Decl is new Ada_Node with private;
   --  Root class for an Ada declaration (RM 3.1). A declaration associates a
   --  name with a language entity, for example a type or a variable.
   type Abort_Node is new Ada_Node with private;
   --  Qualifier for the ``abort`` keyword.
   type Abort_Absent is new Abort_Node with private;

   type Abort_Present is new Abort_Node with private;

   type Stmt is new Ada_Node with private;
   --  Bass class for statements.
   type Simple_Stmt is new Stmt with private;
   --  Base class for simple statements.
   type Abort_Stmt is new Simple_Stmt with private;
   --  ``abort`` statement.
   type Abstract_Node is new Ada_Node with private;
   --  Qualifier for the ``abstract`` keyword.
   type Abstract_Absent is new Abstract_Node with private;

   type Basic_Subp_Decl is new Basic_Decl with private;
   --  Base class for subprogram declarations.
   type Classic_Subp_Decl is new Basic_Subp_Decl with private;
   --  This is an intermediate abstract class for subprogram declarations with
   --  a common structure: overriding indicator, ``SubpSpec``, aspects, <other
   --  fields>.
   type Formal_Subp_Decl is new Classic_Subp_Decl with private;
   --  Formal subprogram declarations, in generic declarations formal parts.
   type Abstract_Formal_Subp_Decl is new Formal_Subp_Decl with private;
   --  Formal declaration for an abstract subprogram.
   type Abstract_Present is new Abstract_Node with private;

   type Abstract_Subp_Decl is new Classic_Subp_Decl with private;
   --  Declaration for an abstract subprogram.
   type Composite_Stmt is new Stmt with private;
   --  Base class for composite statements.
   type Accept_Stmt is new Composite_Stmt with private;
   --  ``accept`` statement.
   type Accept_Stmt_With_Stmts is new Accept_Stmt with private;
   --  Extended ``accept`` statement.
   type Type_Def is new Ada_Node with private;
   --  Base class for type definitions.
   type Access_Def is new Type_Def with private;
   --  Base class for access type definitions.
   type Access_To_Subp_Def is new Access_Def with private;
   --  Type definition for accesses to subprograms.
   type Ada_List is new Ada_Node with private;

   type Ada_Node_List is new Ada_List with private with
      Iterable => (First => Ada_Node_List_First, Next => Ada_Node_List_Next,
       Has_Element => Ada_Node_List_Has_Element,
       Element     => Ada_Node_List_Element);
      --  List of AdaNode.
      --
      --  This list node can contain one of the following nodes:
      --
      --  * Abstract_Subp_Decl
      --
      --  * Allocator
      --
      --  * Aspect_Clause
      --
      --  * Attribute_Ref
      --
      --  * Base_Aggregate
      --
      --  * Bin_Op
      --
      --  * Body_Node
      --
      --  * Call_Expr
      --
      --  * Case_Expr
      --
      --  * Char_Literal
      --
      --  * Component_Clause
      --
      --  * Component_Decl
      --
      --  * Dotted_Name
      --
      --  * Entry_Decl
      --
      --  * Error_Decl
      --
      --  * Exception_Decl
      --
      --  * Exception_Handler
      --
      --  * Explicit_Deref
      --
      --  * Generic_Decl
      --
      --  * Generic_Formal
      --
      --  * Generic_Instantiation
      --
      --  * Generic_Renaming_Decl
      --
      --  * Identifier
      --
      --  * If_Expr
      --
      --  * Incomplete_Type_Decl
      --
      --  * Membership_Expr
      --
      --  * Null_Component_Decl
      --
      --  * Null_Literal
      --
      --  * Num_Literal
      --
      --  * Number_Decl
      --
      --  * Object_Decl
      --
      --  * Others_Designator
      --
      --  * Package_Decl
      --
      --  * Package_Renaming_Decl
      --
      --  * Paren_Expr
      --
      --  * Pragma_Node
      --
      --  * Protected_Type_Decl
      --
      --  * Qual_Expr
      --
      --  * Quantified_Expr
      --
      --  * Raise_Expr
      --
      --  * Single_Protected_Decl
      --
      --  * Single_Task_Decl
      --
      --  * Stmt
      --
      --  * String_Literal
      --
      --  * Subp_Decl
      --
      --  * Subtype_Decl
      --
      --  * Subtype_Indication
      --
      --  * Target_Name
      --
      --  * Task_Type_Decl
      --
      --  * Type_Decl
      --
      --  * Un_Op
      --
      --  * Use_Clause
      --
      --  * With_Clause
   type Base_Aggregate is new Expr with private;
   --  Base class for aggregates.
   type Aggregate is new Base_Aggregate with private;
   --  Aggregate that is not a ``null record`` aggregate.
   type Basic_Assoc is new Ada_Node with private;
   --  Association of one or several names to an expression.
   type Aggregate_Assoc is new Basic_Assoc with private;
   --  Assocation (X => Y) used for aggregates and parameter associations.
   type Aliased_Node is new Ada_Node with private;
   --  Qualifier for the ``aliased`` keyword.
   type Aliased_Absent is new Aliased_Node with private;

   type Aliased_Present is new Aliased_Node with private;

   type All_Node is new Ada_Node with private;
   --  Qualifier for the ``all`` keyword.
   type All_Absent is new All_Node with private;

   type All_Present is new All_Node with private;

   type Allocator is new Expr with private;
   --  Allocator expression (``new ...``).
   type Alternatives_List is new Ada_Node_List with private;
   --  List of alternatives in a ``when ...`` clause.
   --
   --  This list node can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Others_Designator
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op
   type Object_Decl is new Basic_Decl with private;
   --  Base class for Ada object declarations (RM 3.3.1). Ada object
   --  declarations are variables/constants declarations that can be
   --  declared in any declarative scope.
   type Anonymous_Object_Decl is new Object_Decl with private;

   type Type_Expr is new Ada_Node with private;
   --  A type expression is an abstract node that embodies the concept of a
   --  reference to a type.
   --
   --  Since Ada has both subtype_indications and anonymous (inline) type
   --  declarations, a type expression contains one or the other.
   type Anonymous_Type is new Type_Expr with private;
   --  Container for inline anonymous array and access types declarations.
   type Base_Type_Access_Def is new Access_Def with private;
   --  Base class for access type definitions.
   type Anonymous_Type_Access_Def is new Base_Type_Access_Def with private;
   --  Synthetic type access, that will directly reference a type decl. It is
   --  used to generate synthetic anonymous access types.
   type Base_Type_Decl is new Basic_Decl with private;
   --  Base class for type declarations.
   type Type_Decl is new Base_Type_Decl with private;
   --  Type declarations that embed a type definition node.
   type Anonymous_Type_Decl is new Type_Decl with private;
   --  Anonymous type declaration (for anonymous array or access types).
   type Array_Indices is new Ada_Node with private;
   --  Specification for array indexes.
   type Array_Type_Def is new Type_Def with private;
   --  Type definition for an array.
   type Aspect_Assoc is new Ada_Node with private;
   --  Name/expression association in an aspect.
   type Aspect_Assoc_List is new Ada_List with private with
      Iterable => (First => Aspect_Assoc_List_First,
       Next        => Aspect_Assoc_List_Next,
       Has_Element => Aspect_Assoc_List_Has_Element,
       Element     => Aspect_Assoc_List_Element);
      --  List of AspectAssoc.
   type Aspect_Clause is new Ada_Node with private;
   --  Base class for aspect clauses.
   type Aspect_Spec is new Ada_Node with private;
   --  List of aspects in a declaration.
   type Assign_Stmt is new Simple_Stmt with private;
   --  Statement for assignments.
   type Basic_Assoc_List is new Ada_List with private with
      Iterable => (First => Basic_Assoc_List_First,
       Next        => Basic_Assoc_List_Next,
       Has_Element => Basic_Assoc_List_Has_Element,
       Element     => Basic_Assoc_List_Element);
      --  List of BasicAssoc.
   type Assoc_List is new Basic_Assoc_List with private;
   --  List of associations.
   type At_Clause is new Aspect_Clause with private;
   --  Representation clause (``for .. use at ...;``).
   type Attribute_Def_Clause is new Aspect_Clause with private;
   --  Clause for an attribute definition (``for ...'Attribute use ...;``).
   type Name is new Expr with private;
   --  Base class for names.
   type Attribute_Ref is new Name with private;
   --  Expression to reference an attribute.
   type Base_Assoc is new Ada_Node with private;
   --  Abstract class for a key/value association, where the value is an
   --  expression.
   type Base_Assoc_List is new Ada_List with private with
      Iterable => (First => Base_Assoc_List_First,
       Next        => Base_Assoc_List_Next,
       Has_Element => Base_Assoc_List_Has_Element,
       Element     => Base_Assoc_List_Element);
      --  List of BaseAssoc.
   type Base_Formal_Param_Decl is new Basic_Decl with private;
   --  Base class for formal parameter declarations. This is used both for
   --  records components and for subprogram parameters.
   --
   --  This is a Libadalang abstraction, that has no ARM existence.
   type Base_Formal_Param_Holder is new Ada_Node with private;
   --  Base class for lists of formal parameters. This is used both for
   --  subprogram specifications and for records, so that we can share
   --  the matching and unpacking logic.
   type Single_Tok_Node is new Name with private;
   --  Base class for nodes that are made up of a single token.
   type Base_Id is new Single_Tok_Node with private;
   --  Base class for identifiers.
   type Base_Loop_Stmt is new Composite_Stmt with private;
   --  Base class for loop statements.
   type Base_Package_Decl is new Basic_Decl with private;
   --  Package declarations. Concrete instances of this class will be created
   --  in generic package declarations. Other non-generic package declarations
   --  will be instances of PackageDecl.
   --
   --  The behavior is the same, the only difference is that BasePackageDecl
   --  and PackageDecl have different behavior regarding lexical environments.
   --  In the case of generic package declarations, we use BasePackageDecl
   --  which has no env_spec, and the environment behavior is handled by the
   --  GenericPackageDecl instance.
   type Base_Record_Def is new Ada_Node with private;
   --  Base class for record definitions.
   type Body_Node is new Basic_Decl with private;
   --  Base class for an Ada body (RM 3.11). A body is the completion of a
   --  declaration.
   type Base_Subp_Body is new Body_Node with private;
   --  Base class for subprogram bodies.
   type Base_Subp_Spec is new Base_Formal_Param_Holder with private;
   --  Base class for subprogram specifications.
   type Base_Subtype_Decl is new Base_Type_Decl with private;
   --  Base class for subtype declarations.
   type Block_Stmt is new Composite_Stmt with private;
   --  Base class for statement blocks.
   type Begin_Block is new Block_Stmt with private;
   --  Statement block with no declarative part.
   type Bin_Op is new Expr with private;
   --  Binary expression.
   type Body_Stub is new Body_Node with private;
   --  Base class for a body stub (RM 10.1.3). A body stub is meant to be
   --  completed by .
   type Box_Expr is new Expr with private;
   --  Box expression (``<>``).
   type Call_Expr is new Name with private;
   --  Represent a syntactic call expression.
   --
   --  At the semantic level, this can be either a subprogram call, an array
   --  subcomponent access expression, an array slice or a type conversion.
   type Call_Stmt is new Simple_Stmt with private;
   --  Statement for entry or procedure calls.
   type Case_Expr is new Expr with private;
   --  ``case`` expression.
   type Case_Expr_Alternative is new Expr with private;
   --  Alternative in a ``case`` expression (``when ... => ...``).
   type Case_Expr_Alternative_List is new Ada_List with private with
      Iterable => (First => Case_Expr_Alternative_List_First,
       Next        => Case_Expr_Alternative_List_Next,
       Has_Element => Case_Expr_Alternative_List_Has_Element,
       Element     => Case_Expr_Alternative_List_Element);
      --  List of CaseExprAlternative.
   type Case_Stmt is new Composite_Stmt with private;
   --  ``case`` statement.
   type Case_Stmt_Alternative is new Ada_Node with private;
   --  Alternative in a ``case`` statement (``when ... => ...``).
   type Case_Stmt_Alternative_List is new Ada_List with private with
      Iterable => (First => Case_Stmt_Alternative_List_First,
       Next        => Case_Stmt_Alternative_List_Next,
       Has_Element => Case_Stmt_Alternative_List_Has_Element,
       Element     => Case_Stmt_Alternative_List_Element);
      --  List of CaseStmtAlternative.
   type Char_Literal is new Base_Id with private;
   --  Character literal.
   type Classwide_Type_Decl is new Base_Type_Decl with private;
   --  Synthetic node (not parsed, generated from a property call). Refers
   --  to the classwide type for a given tagged type. The aim is that those
   --  be mostly equivalent to their non-classwide type, except for some
   --  resolution rules.
   type Compilation_Unit is new Ada_Node with private;
   --  Root node for all Ada analysis units.
   type Compilation_Unit_List is new Ada_List with private with
      Iterable => (First => Compilation_Unit_List_First,
       Next        => Compilation_Unit_List_Next,
       Has_Element => Compilation_Unit_List_Has_Element,
       Element     => Compilation_Unit_List_Element);
      --  List of CompilationUnit.
   type Component_Clause is new Ada_Node with private;
   --  Representation clause for a single component.
   type Component_Decl is new Base_Formal_Param_Decl with private;
   --  Declaration for a component.
   type Component_Def is new Ada_Node with private;
   --  Definition for a component.
   type Component_List is new Base_Formal_Param_Holder with private;
   --  List of component declarations.
   type Concrete_Formal_Subp_Decl is new Formal_Subp_Decl with private;
   --  Formal declaration for a concrete subprogram.
   type Constant_Node is new Ada_Node with private;
   --  Qualifier for the ``constant`` keyword.
   type Constant_Absent is new Constant_Node with private;

   type Constant_Present is new Constant_Node with private;

   type Constrained_Array_Indices is new Array_Indices with private;
   --  Constrained specification for array indexes.
   type Subtype_Indication is new Type_Expr with private;
   --  Reference to a type by name.
   type Constrained_Subtype_Indication is new Subtype_Indication with private;
   --  Reference to a type with a range constraint.
   type Constraint is new Ada_Node with private;
   --  Base class for type constraints.
   type Constraint_List is new Ada_Node_List with private;
   --  List of constraints.
   --
   --  This list node can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Subtype_Indication
   --
   --  * Target_Name
   type Contract_Case_Assoc is new Base_Assoc with private;
   --  Single association for the ``Contract_Case`` aspect.
   type Contract_Case_Assoc_List is new Ada_List with private with
      Iterable => (First => Contract_Case_Assoc_List_First,
       Next        => Contract_Case_Assoc_List_Next,
       Has_Element => Contract_Case_Assoc_List_Has_Element,
       Element     => Contract_Case_Assoc_List_Element);
      --  List of ContractCaseAssoc.
   type Contract_Cases is new Expr with private;
   --  List of associations for the ``Contract_Case`` aspect.
   type Real_Type_Def is new Type_Def with private;
   --  Type definition for real numbers.
   type Decimal_Fixed_Point_Def is new Real_Type_Def with private;
   --  Type definition for decimal fixed-point numbers.
   type Decl_Block is new Block_Stmt with private;
   --  Statement block with a declarative part.
   type Decl_List is new Ada_Node_List with private;
   --  List of declarations.
   --
   --  This list node can contain one of the following nodes:
   --
   --  * Abstract_Subp_Decl
   --
   --  * Aspect_Clause
   --
   --  * Component_Decl
   --
   --  * Entry_Decl
   --
   --  * Expr_Function
   --
   --  * Null_Subp_Decl
   --
   --  * Pragma_Node
   --
   --  * Subp_Decl
   --
   --  * Subp_Renaming_Decl
   type Declarative_Part is new Ada_Node with private;
   --  List of declarations.
   type Defining_Name is new Name with private;
   --  Name that defines an entity.
   type Defining_Name_List is new Ada_List with private with
      Iterable => (First => Defining_Name_List_First,
       Next        => Defining_Name_List_Next,
       Has_Element => Defining_Name_List_Has_Element,
       Element     => Defining_Name_List_Element);
      --  List of DefiningName.
   type Delay_Stmt is new Simple_Stmt with private;
   --  ``delay`` statement.
   type Delta_Constraint is new Constraint with private;
   --  Delta and range type constraint.
   type Derived_Type_Def is new Type_Def with private;
   --  Type definition for a derived type.
   type Digits_Constraint is new Constraint with private;
   --  Digits and range type constraint.
   type Discrete_Base_Subtype_Decl is new Base_Subtype_Decl with private;
   --  Specific ``BaseSubtypeDecl`` synthetic subclass for the base type of
   --  scalar types.
   type Discrete_Subtype_Indication is new Subtype_Indication with private;
   --  Reference to a type with a general constraint.
   type Discrete_Subtype_Name is new Name with private;
   --  Subtype name for membership test expressions.
   type Discriminant_Assoc is new Basic_Assoc with private;
   --  Association of discriminant names to an expression.
   type Identifier_List is new Ada_List with private with
      Iterable => (First => Identifier_List_First,
       Next        => Identifier_List_Next,
       Has_Element => Identifier_List_Has_Element,
       Element     => Identifier_List_Element);
      --  List of Identifier.
   type Discriminant_Choice_List is new Identifier_List with private;
   --  List of discriminant associations.
   type Discriminant_Constraint is new Constraint with private;
   --  List of constraints that relate to type discriminants.
   type Discriminant_Part is new Base_Formal_Param_Holder with private;
   --  Specification for discriminants in type declarations.
   type Discriminant_Spec is new Base_Formal_Param_Decl with private;
   --  Known list of discriminants in type declarations.
   type Discriminant_Spec_List is new Ada_List with private with
      Iterable => (First => Discriminant_Spec_List_First,
       Next        => Discriminant_Spec_List_Next,
       Has_Element => Discriminant_Spec_List_Has_Element,
       Element     => Discriminant_Spec_List_Element);
      --  List of DiscriminantSpec.
   type Dotted_Name is new Name with private;
   --  Name to select a suffix in a prefix.
   type Elsif_Expr_Part is new Ada_Node with private;
   --  ``elsif`` block, part of an ``if`` expression.
   type Elsif_Expr_Part_List is new Ada_List with private with
      Iterable => (First => Elsif_Expr_Part_List_First,
       Next        => Elsif_Expr_Part_List_Next,
       Has_Element => Elsif_Expr_Part_List_Has_Element,
       Element     => Elsif_Expr_Part_List_Element);
      --  List of ElsifExprPart.
   type Elsif_Stmt_Part is new Ada_Node with private;
   --  ``elsif`` part in an ``if`` statement block.
   type Elsif_Stmt_Part_List is new Ada_List with private with
      Iterable => (First => Elsif_Stmt_Part_List_First,
       Next        => Elsif_Stmt_Part_List_Next,
       Has_Element => Elsif_Stmt_Part_List_Has_Element,
       Element     => Elsif_Stmt_Part_List_Element);
      --  List of ElsifStmtPart.
   type End_Name is new Name with private;
   --  Entity name in ``end ...;`` syntactic constructs.
   type Entry_Body is new Body_Node with private;
   --  Entry body.
   type Entry_Completion_Formal_Params is
     new Base_Formal_Param_Holder with private;
   --  Formal parameters for the completion of an ``EntryDecl`` (either an
   --  ``EntryBody`` or an ``AcceptStmt``).
   type Entry_Decl is new Basic_Subp_Decl with private;
   --  Entry declaration.
   type Entry_Index_Spec is new Basic_Decl with private;
   --  Index specification for an entry body.
   type Entry_Spec is new Base_Subp_Spec with private;
   --  Entry specification.
   type Enum_Lit_Synth_Type_Expr is new Type_Expr with private;
   --  Synthetic node. Represents the type expression for an enum literal.
   type Enum_Literal_Decl is new Basic_Subp_Decl with private;
   --  Declaration for an enumeration literal.
   type Enum_Literal_Decl_List is new Ada_List with private with
      Iterable => (First => Enum_Literal_Decl_List_First,
       Next        => Enum_Literal_Decl_List_Next,
       Has_Element => Enum_Literal_Decl_List_Has_Element,
       Element     => Enum_Literal_Decl_List_Element);
      --  List of EnumLiteralDecl.
   type Enum_Rep_Clause is new Aspect_Clause with private;
   --  Representation clause for enumeration types.
   type Enum_Subp_Spec is new Base_Subp_Spec with private;
   --  Synthetic node for the abstract subprogram spec of an enum literal.
   --
   --  NOTE: This has no existence in the ARM. While enum literals are
   --  functions semantically, they're not such syntactically.
   type Enum_Type_Def is new Type_Def with private;
   --  Type definition for enumerations.
   type Error_Decl is new Basic_Decl with private;
   --  Placeholder node for syntax errors in lists of declarations.
   type Error_Stmt is new Stmt with private;
   --  Placeholder node for syntax errors in lists of statements.
   type Exception_Decl is new Basic_Decl with private;
   --  Exception declarations.
   type Exception_Handler is new Basic_Decl with private;
   --  Exception handler.
   type Exit_Stmt is new Simple_Stmt with private;
   --  ``exit`` statement.
   type Explicit_Deref is new Name with private;
   --  Explicit dereference expression (``.all``).
   type Expr_List is new Ada_List with private with
      Iterable => (First => Expr_List_First, Next => Expr_List_Next,
       Has_Element => Expr_List_Has_Element, Element => Expr_List_Element);
      --  List of Expr.
      --
      --  This list node can contain one of the following nodes:
      --
      --  * Allocator
      --
      --  * Attribute_Ref
      --
      --  * Base_Aggregate
      --
      --  * Bin_Op
      --
      --  * Call_Expr
      --
      --  * Case_Expr
      --
      --  * Char_Literal
      --
      --  * Discrete_Subtype_Name
      --
      --  * Dotted_Name
      --
      --  * Explicit_Deref
      --
      --  * Identifier
      --
      --  * If_Expr
      --
      --  * Null_Literal
      --
      --  * Num_Literal
      --
      --  * Paren_Expr
      --
      --  * Qual_Expr
      --
      --  * Quantified_Expr
      --
      --  * Raise_Expr
      --
      --  * String_Literal
      --
      --  * Target_Name
      --
      --  * Un_Op
   type Expr_Alternatives_List is new Expr_List with private;
   --  List of alternatives in a membership test expression.
   --
   --  This list node can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Name
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op
   type Expr_Function is new Base_Subp_Body with private;
   --  Expression function.
   type Extended_Return_Stmt is new Composite_Stmt with private;
   --  Extended ``return`` statement.
   type Extended_Return_Stmt_Object_Decl is new Object_Decl with private;
   --  Object declaration that is part of an extended return statement.
   type Floating_Point_Def is new Real_Type_Def with private;
   --  Type definition for floating-point numbers.
   type Loop_Spec is new Ada_Node with private;
   --  Base class for loop specifications.
   type For_Loop_Spec is new Loop_Spec with private;
   --  Specification for a ``for`` loop.
   type For_Loop_Stmt is new Base_Loop_Stmt with private;
   --  Statement for ``for`` loops (``for ... loop ... end loop;``).
   type For_Loop_Var_Decl is new Basic_Decl with private;
   --  Declaration for the controlling variable in a ``for`` loop.
   type Formal_Discrete_Type_Def is new Type_Def with private;
   --  Type definition for discrete types in generic formals.
   type Generic_Decl is new Basic_Decl with private;
   --  Base class for generic declarations.
   type Generic_Formal is new Base_Formal_Param_Decl with private;
   --  Enclosing declaration for a generic formal. The real declaration is
   --  accessible via the ``decl`` field.
   type Generic_Formal_Obj_Decl is new Generic_Formal with private;
   --  Formal declaration for an object.
   type Generic_Formal_Package is new Generic_Formal with private;
   --  Formal declaration for a package.
   type Generic_Formal_Part is new Base_Formal_Param_Holder with private;
   --  List of declaration for generic formals.
   type Generic_Formal_Subp_Decl is new Generic_Formal with private;
   --  Formal declaration for a subprogram.
   type Generic_Formal_Type_Decl is new Generic_Formal with private;
   --  Formal declaration for a type.
   type Generic_Instantiation is new Basic_Decl with private;
   --  Instantiations of generics.
   type Generic_Package_Decl is new Generic_Decl with private;
   --  Generic package declaration.
   type Generic_Package_Instantiation is
     new Generic_Instantiation with private;
   --  Instantiations of a generic package.
   type Generic_Package_Internal is new Base_Package_Decl with private;
   --  This class denotes the internal package contained by a
   --  GenericPackageDecl.
   type Generic_Renaming_Decl is new Basic_Decl with private;
   --  Base node for all generic renaming declarations.
   type Generic_Package_Renaming_Decl is
     new Generic_Renaming_Decl with private;
   --  Declaration for a generic package renaming.
   type Generic_Subp_Decl is new Generic_Decl with private;
   --  Generic subprogram declaration.
   type Generic_Subp_Instantiation is new Generic_Instantiation with private;
   --  Instantiations of a generic subprogram.
   type Generic_Subp_Internal is new Basic_Subp_Decl with private;
   --  Internal node for generic subprograms.
   type Generic_Subp_Renaming_Decl is new Generic_Renaming_Decl with private;
   --  Declaration for a generic subprogram renaming.
   type Goto_Stmt is new Simple_Stmt with private;
   --  ``goto`` statement.
   type Handled_Stmts is new Ada_Node with private;
   --  List of statements, with optional exception handlers.
   type Identifier is new Base_Id with private;
   --  Regular identifier.
   type If_Expr is new Expr with private;
   --  ``if`` expression.
   type If_Stmt is new Composite_Stmt with private;
   --  ``if`` statement block.
   type Incomplete_Type_Decl is new Base_Type_Decl with private;
   --  Incomplete declaration for a type.
   type Incomplete_Tagged_Type_Decl is new Incomplete_Type_Decl with private;
   --  Incomplete declaration for a tagged type.
   type Index_Constraint is new Constraint with private;
   --  List of type constraints.
   type Num_Literal is new Single_Tok_Node with private;
   --  Base class for number literals.
   type Int_Literal is new Num_Literal with private;
   --  Literal for an integer.
   type Interface_Kind is new Ada_Node with private;
   --  Kind of interface type.
   type Interface_Kind_Limited is new Interface_Kind with private;

   type Interface_Kind_Protected is new Interface_Kind with private;

   type Interface_Kind_Synchronized is new Interface_Kind with private;

   type Interface_Kind_Task is new Interface_Kind with private;

   type Interface_Type_Def is new Type_Def with private;
   --  Type definition for an interface.
   type Iter_Type is new Ada_Node with private;
   --  Iteration type for ``for`` loops.
   type Iter_Type_In is new Iter_Type with private;

   type Iter_Type_Of is new Iter_Type with private;

   type Known_Discriminant_Part is new Discriminant_Part with private;
   --  Known list of discriminants in type declarations.
   type Label is new Simple_Stmt with private;
   --  Statement to declare a code label.
   type Label_Decl is new Basic_Decl with private;
   --  Declaration for a code label.
   type Library_Item is new Ada_Node with private;
   --  Library item in a compilation unit.
   type Limited_Node is new Ada_Node with private;
   --  Qualifier for the ``limited`` keyword.
   type Limited_Absent is new Limited_Node with private;

   type Limited_Present is new Limited_Node with private;

   type Loop_Stmt is new Base_Loop_Stmt with private;
   --  Statement for simple loops (``loop ... end loop;``).
   type Membership_Expr is new Expr with private;
   --  Represent a membership test (in/not in operators).
   --
   --  Note that we don't consider them as binary operators since multiple
   --  expressions on the right hand side are allowed.
   type Mod_Int_Type_Def is new Type_Def with private;
   --  Type definition for a modular integer type.
   type Mode is new Ada_Node with private;
   --  Syntactic indicators for passing modes in formals.
   type Mode_Default is new Mode with private;

   type Mode_In is new Mode with private;

   type Mode_In_Out is new Mode with private;

   type Mode_Out is new Mode with private;

   type Multi_Dim_Array_Assoc is new Aggregate_Assoc with private;
   --  Association used for multi-dimension array aggregates.
   type Name_List is new Ada_List with private with
      Iterable => (First => Name_List_First, Next => Name_List_Next,
       Has_Element => Name_List_Has_Element, Element => Name_List_Element);
      --  List of Name.
      --
      --  This list node can contain one of the following nodes:
      --
      --  * Attribute_Ref
      --
      --  * Call_Expr
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Explicit_Deref
      --
      --  * Identifier
      --
      --  * Qual_Expr
      --
      --  * String_Literal
      --
      --  * Target_Name
   type Named_Stmt is new Composite_Stmt with private;
   --  Wrapper class, used for composite statements that can be named (declare
   --  blocks, loops). This allows to both have a BasicDecl for the named
   --  entity declared, and a CompositeStmt for the statement hierarchy.
   type Named_Stmt_Decl is new Basic_Decl with private;
   --  BasicDecl that is always the declaration inside a named statement.
   type Not_Null is new Ada_Node with private;
   --  Qualifier for the ``not null`` keywords.
   type Not_Null_Absent is new Not_Null with private;

   type Not_Null_Present is new Not_Null with private;

   type Null_Component_Decl is new Ada_Node with private;
   --  Placeholder for the ``null`` in lists of components.
   type Null_Literal is new Single_Tok_Node with private;
   --  The ``null`` literal.
   type Null_Record_Aggregate is new Base_Aggregate with private;
   --  Aggregate for ``null record``.
   type Null_Record_Def is new Base_Record_Def with private;
   --  Record definition for ``null record``.
   type Null_Stmt is new Simple_Stmt with private;
   --  ``null;`` statement.
   type Null_Subp_Decl is new Base_Subp_Body with private;
   --  Declaration for a null subprogram.
   type Number_Decl is new Basic_Decl with private;
   --  Declaration for a static constant number.
   type Op is new Base_Id with private;
   --  Operation in a binary expression.
   --
   --  Note that the ARM does not consider "double_dot" ("..") as a binary
   --  operator, but we process it this way here anyway to keep things simple.
   type Op_Abs is new Op with private;

   type Op_And is new Op with private;

   type Op_And_Then is new Op with private;

   type Op_Concat is new Op with private;

   type Op_Div is new Op with private;

   type Op_Double_Dot is new Op with private;

   type Op_Eq is new Op with private;

   type Op_Gt is new Op with private;

   type Op_Gte is new Op with private;

   type Op_In is new Op with private;

   type Op_Lt is new Op with private;

   type Op_Lte is new Op with private;

   type Op_Minus is new Op with private;

   type Op_Mod is new Op with private;

   type Op_Mult is new Op with private;

   type Op_Neq is new Op with private;

   type Op_Not is new Op with private;

   type Op_Not_In is new Op with private;

   type Op_Or is new Op with private;

   type Op_Or_Else is new Op with private;

   type Op_Plus is new Op with private;

   type Op_Pow is new Op with private;

   type Op_Rem is new Op with private;

   type Op_Xor is new Op with private;

   type Ordinary_Fixed_Point_Def is new Real_Type_Def with private;
   --  Type definition for ordinary fixed-point numbers.
   type Others_Designator is new Ada_Node with private;
   --  ``other`` designator.
   type Overriding_Node is new Ada_Node with private;
   --  Syntactic indicators for subprogram overriding modes.
   type Overriding_Not_Overriding is new Overriding_Node with private;

   type Overriding_Overriding is new Overriding_Node with private;

   type Overriding_Unspecified is new Overriding_Node with private;

   type Package_Body is new Body_Node with private;
   --  Package body.
   type Package_Body_Stub is new Body_Stub with private;
   --  Stub for a package body (``is separate``).
   type Package_Decl is new Base_Package_Decl with private;
   --  Non-generic package declarations.
   type Package_Renaming_Decl is new Basic_Decl with private;
   --  Declaration for a package renaming.
   type Param_Assoc is new Basic_Assoc with private;
   --  Assocation (X => Y) used for aggregates and parameter associations.
   type Param_Spec is new Base_Formal_Param_Decl with private;
   --  Specification for a parameter.
   type Param_Spec_List is new Ada_List with private with
      Iterable => (First => Param_Spec_List_First,
       Next        => Param_Spec_List_Next,
       Has_Element => Param_Spec_List_Has_Element,
       Element     => Param_Spec_List_Element);
      --  List of ParamSpec.
   type Params is new Ada_Node with private;
   --  List of parameter specifications.
   type Paren_Expr is new Expr with private;
   --  Parenthesized expression.
   type Parent_List is new Name_List with private;
   --  List of parents in a type declaration.
   --
   --  This list node can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal
   type Pragma_Argument_Assoc is new Base_Assoc with private;
   --  Argument assocation in a pragma.
   type Pragma_Node is new Ada_Node with private;
   --  Class for pragmas (RM 2.8). Pragmas are compiler directives, that can be
   --  language or compiler defined.
   type Pragma_Node_List is new Ada_List with private with
      Iterable => (First => Pragma_Node_List_First,
       Next        => Pragma_Node_List_Next,
       Has_Element => Pragma_Node_List_Has_Element,
       Element     => Pragma_Node_List_Element);
      --  List of Pragma.
   type Prim_Type_Accessor is new Ada_Node with private;
   --  Synthetic node wrapping around a primitive type entity. Used in
   --  metadata.
   type Private_Node is new Ada_Node with private;
   --  Qualifier for the ``private`` keyword.
   type Private_Absent is new Private_Node with private;

   type Private_Part is new Declarative_Part with private;
   --  List of declarations in a private part.
   type Private_Present is new Private_Node with private;

   type Private_Type_Def is new Type_Def with private;
   --  Type definition for a private type.
   type Protected_Node is new Ada_Node with private;
   --  Qualifier for the ``protected`` keyword.
   type Protected_Absent is new Protected_Node with private;

   type Protected_Body is new Body_Node with private;
   --  Protected object body.
   type Protected_Body_Stub is new Body_Stub with private;
   --  Stub for a protected object body (``is separate``).
   type Protected_Def is new Ada_Node with private;
   --  Type definition for a protected object.
   type Protected_Present is new Protected_Node with private;

   type Protected_Type_Decl is new Base_Type_Decl with private;
   --  Declaration for a protected type.
   type Public_Part is new Declarative_Part with private;
   --  List of declarations in a public part.
   type Qual_Expr is new Name with private;
   --  Qualified expression (``...'(...)``).
   type Quantified_Expr is new Expr with private;
   --  Quantified expression.
   type Quantifier is new Ada_Node with private;
   --  Type for quantified expressions.
   type Quantifier_All is new Quantifier with private;

   type Quantifier_Some is new Quantifier with private;

   type Raise_Expr is new Expr with private;
   --  Expression to raise an exception.
   type Raise_Stmt is new Simple_Stmt with private;
   --  ``raise`` statement.
   type Range_Constraint is new Constraint with private;
   --  Range-based type constraint.
   type Range_Spec is new Ada_Node with private;
   --  Range specification.
   type Real_Literal is new Num_Literal with private;
   --  Literal for a real number.
   type Record_Def is new Base_Record_Def with private;
   --  Record definition that contains components (``record ... end record``).
   type Record_Rep_Clause is new Aspect_Clause with private;
   --  Representation clause for a record type.
   type Record_Type_Def is new Type_Def with private;
   --  Type definition for a record.
   type Relation_Op is new Bin_Op with private;
   --  Binary operation that compares two value, producing a boolean.
   type Renaming_Clause is new Ada_Node with private;
   --  Renaming clause, used everywhere renamings are valid.
   type Requeue_Stmt is new Simple_Stmt with private;
   --  ``requeue`` statement.
   type Return_Stmt is new Simple_Stmt with private;
   --  ``return`` statement.
   type Reverse_Node is new Ada_Node with private;
   --  Qualifier for the ``reverse`` keyword.
   type Reverse_Absent is new Reverse_Node with private;

   type Reverse_Present is new Reverse_Node with private;

   type Select_Stmt is new Composite_Stmt with private;
   --  ``select`` statements block.
   type Select_When_Part is new Ada_Node with private;
   --  Alternative part in a ``select`` statements block.
   type Select_When_Part_List is new Ada_List with private with
      Iterable => (First => Select_When_Part_List_First,
       Next        => Select_When_Part_List_Next,
       Has_Element => Select_When_Part_List_Has_Element,
       Element     => Select_When_Part_List_Element);
      --  List of SelectWhenPart.
   type Signed_Int_Type_Def is new Type_Def with private;
   --  Type definition for a signed integer type.
   type Single_Protected_Decl is new Basic_Decl with private;
   --  Declaration for a single protected object.
   type Single_Task_Decl is new Basic_Decl with private;
   --  Declaration for a single task.
   type Task_Type_Decl is new Base_Type_Decl with private;
   --  Declaration for a task type.
   type Single_Task_Type_Decl is new Task_Type_Decl with private;
   --  Type declaration for a single task.
   type Stmt_List is new Ada_Node_List with private;
   --  List of statements.
   --
   --  This list node can contain one of the following nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt
   type String_Literal is new Base_Id with private;
   --  String literal.
   type Subp_Body is new Base_Subp_Body with private;
   --  Subprogram body.
   type Subp_Body_Stub is new Body_Stub with private;
   --  Stub for a subprogram body (``is separate``).
   type Subp_Decl is new Classic_Subp_Decl with private;
   --  Regular subprogram declaration.
   type Subp_Kind is new Ada_Node with private;
   --  Qualifier for a subprogram kind.
   type Subp_Kind_Function is new Subp_Kind with private;

   type Subp_Kind_Procedure is new Subp_Kind with private;

   type Subp_Renaming_Decl is new Base_Subp_Body with private;
   --  Declaration for a subprogram renaming.
   type Subp_Spec is new Base_Subp_Spec with private;
   --  Subprogram specification.
   type Subtype_Decl is new Base_Subtype_Decl with private;
   --  Subtype declaration.
   type Subunit is new Ada_Node with private;
   --  Subunit (``separate``).
   type Synchronized_Node is new Ada_Node with private;
   --  Qualifier for the ``synchronized`` keyword.
   type Synchronized_Absent is new Synchronized_Node with private;

   type Synchronized_Present is new Synchronized_Node with private;

   type Synth_Anonymous_Type_Decl is new Anonymous_Type_Decl with private;
   --  Synthetic anonymous type decl. Used to generate anonymous access types.
   type Synthetic_Renaming_Clause is new Renaming_Clause with private;
   --  Synthetic renaming clause. Used to synthesize object decls with
   --  renamings. (See to_anonymous_object_decl).
   type Tagged_Node is new Ada_Node with private;
   --  Qualifier for the ``tagged`` keyword.
   type Tagged_Absent is new Tagged_Node with private;

   type Tagged_Present is new Tagged_Node with private;

   type Target_Name is new Name with private;
   --  Name for Ada 2020's ``@``.
   type Task_Body is new Body_Node with private;
   --  Task body.
   type Task_Body_Stub is new Body_Stub with private;
   --  Stub for a task body (``is separate``).
   type Task_Def is new Ada_Node with private;
   --  Type definition for a task type.
   type Terminate_Alternative is new Simple_Stmt with private;
   --  ``terminate`` alternative in a ``select`` statement.
   type Type_Access_Def is new Base_Type_Access_Def with private;
   --  Syntactic type definition for accesses.
   type Un_Op is new Expr with private;
   --  Unary expression.
   type Unconstrained_Array_Index is new Ada_Node with private;
   --  List of unconstrained array indexes.
   type Unconstrained_Array_Index_List is new Ada_List with private with
      Iterable => (First => Unconstrained_Array_Index_List_First,
       Next        => Unconstrained_Array_Index_List_Next,
       Has_Element => Unconstrained_Array_Index_List_Has_Element,
       Element     => Unconstrained_Array_Index_List_Element);
      --  List of UnconstrainedArrayIndex.
   type Unconstrained_Array_Indices is new Array_Indices with private;
   --  Unconstrained specification for array indexes.
   type Unknown_Discriminant_Part is new Discriminant_Part with private;
   --  Unknown list of discriminants in type declarations.
   type Until_Node is new Ada_Node with private;
   --  Qualifier for the ``until`` keyword.
   type Until_Absent is new Until_Node with private;

   type Until_Present is new Until_Node with private;

   type Update_Attribute_Ref is new Attribute_Ref with private;
   --  Reference to the ``Update`` attribute.
   type Use_Clause is new Ada_Node with private;
   --  Base class for use clauses.
   type Use_Package_Clause is new Use_Clause with private;
   --  Use clause for packages.
   type Use_Type_Clause is new Use_Clause with private;
   --  Use clause for types.
   type Variant is new Ada_Node with private;
   --  Single variant in a discriminated type record declaration.
   --
   --  This corresponds to a ``when ... => ...`` section in a variant part.
   type Variant_List is new Ada_List with private with
      Iterable => (First => Variant_List_First, Next => Variant_List_Next,
       Has_Element => Variant_List_Has_Element,
       Element     => Variant_List_Element);
      --  List of Variant.
   type Variant_Part is new Ada_Node with private;
   --  Variant part in a discriminated type record declaration.
   --
   --  This corresponds to the whole ``case ... is ... end case;`` block.
   type While_Loop_Spec is new Loop_Spec with private;
   --  Specification for a ``while`` loop.
   type While_Loop_Stmt is new Base_Loop_Stmt with private;
   --  Statement for ``while`` loops (``while ... loop ... end loop;``).
   type With_Clause is new Ada_Node with private;
   --  With clause.
   type With_Private is new Ada_Node with private;
   --  Qualifier for the ``private`` keyword in ``with private`` record
   --  clauses.
   type With_Private_Absent is new With_Private with private;

   type With_Private_Present is new With_Private with private;

   No_Ada_Node : constant Ada_Node;
   --  Special value to represent the absence of a node. Note that every node
   --  type derived from the root type has a similar ``No_Node`` constant.
   No_Expr : constant Expr;
      --% no-document: True
   No_Basic_Decl : constant Basic_Decl;
      --% no-document: True
   No_Abort_Node : constant Abort_Node;
      --% no-document: True
   No_Abort_Absent : constant Abort_Absent;
      --% no-document: True
   No_Abort_Present : constant Abort_Present;
      --% no-document: True
   No_Stmt : constant Stmt;
      --% no-document: True
   No_Simple_Stmt : constant Simple_Stmt;
      --% no-document: True
   No_Abort_Stmt : constant Abort_Stmt;
      --% no-document: True
   No_Abstract_Node : constant Abstract_Node;
      --% no-document: True
   No_Abstract_Absent : constant Abstract_Absent;
      --% no-document: True
   No_Basic_Subp_Decl : constant Basic_Subp_Decl;
      --% no-document: True
   No_Classic_Subp_Decl : constant Classic_Subp_Decl;
      --% no-document: True
   No_Formal_Subp_Decl : constant Formal_Subp_Decl;
      --% no-document: True
   No_Abstract_Formal_Subp_Decl : constant Abstract_Formal_Subp_Decl;
      --% no-document: True
   No_Abstract_Present : constant Abstract_Present;
      --% no-document: True
   No_Abstract_Subp_Decl : constant Abstract_Subp_Decl;
      --% no-document: True
   No_Composite_Stmt : constant Composite_Stmt;
      --% no-document: True
   No_Accept_Stmt : constant Accept_Stmt;
      --% no-document: True
   No_Accept_Stmt_With_Stmts : constant Accept_Stmt_With_Stmts;
      --% no-document: True
   No_Type_Def : constant Type_Def;
      --% no-document: True
   No_Access_Def : constant Access_Def;
      --% no-document: True
   No_Access_To_Subp_Def : constant Access_To_Subp_Def;
      --% no-document: True
   No_Ada_List : constant Ada_List;
      --% no-document: True
   No_Ada_Node_List : constant Ada_Node_List;
      --% no-document: True
   No_Base_Aggregate : constant Base_Aggregate;
      --% no-document: True
   No_Aggregate : constant Aggregate;
      --% no-document: True
   No_Basic_Assoc : constant Basic_Assoc;
      --% no-document: True
   No_Aggregate_Assoc : constant Aggregate_Assoc;
      --% no-document: True
   No_Aliased_Node : constant Aliased_Node;
      --% no-document: True
   No_Aliased_Absent : constant Aliased_Absent;
      --% no-document: True
   No_Aliased_Present : constant Aliased_Present;
      --% no-document: True
   No_All_Node : constant All_Node;
      --% no-document: True
   No_All_Absent : constant All_Absent;
      --% no-document: True
   No_All_Present : constant All_Present;
      --% no-document: True
   No_Allocator : constant Allocator;
      --% no-document: True
   No_Alternatives_List : constant Alternatives_List;
      --% no-document: True
   No_Object_Decl : constant Object_Decl;
      --% no-document: True
   No_Anonymous_Object_Decl : constant Anonymous_Object_Decl;
      --% no-document: True
   No_Type_Expr : constant Type_Expr;
      --% no-document: True
   No_Anonymous_Type : constant Anonymous_Type;
      --% no-document: True
   No_Base_Type_Access_Def : constant Base_Type_Access_Def;
      --% no-document: True
   No_Anonymous_Type_Access_Def : constant Anonymous_Type_Access_Def;
      --% no-document: True
   No_Base_Type_Decl : constant Base_Type_Decl;
      --% no-document: True
   No_Type_Decl : constant Type_Decl;
      --% no-document: True
   No_Anonymous_Type_Decl : constant Anonymous_Type_Decl;
      --% no-document: True
   No_Array_Indices : constant Array_Indices;
      --% no-document: True
   No_Array_Type_Def : constant Array_Type_Def;
      --% no-document: True
   No_Aspect_Assoc : constant Aspect_Assoc;
      --% no-document: True
   No_Aspect_Assoc_List : constant Aspect_Assoc_List;
      --% no-document: True
   No_Aspect_Clause : constant Aspect_Clause;
      --% no-document: True
   No_Aspect_Spec : constant Aspect_Spec;
      --% no-document: True
   No_Assign_Stmt : constant Assign_Stmt;
      --% no-document: True
   No_Basic_Assoc_List : constant Basic_Assoc_List;
      --% no-document: True
   No_Assoc_List : constant Assoc_List;
      --% no-document: True
   No_At_Clause : constant At_Clause;
      --% no-document: True
   No_Attribute_Def_Clause : constant Attribute_Def_Clause;
      --% no-document: True
   No_Name : constant Name;
      --% no-document: True
   No_Attribute_Ref : constant Attribute_Ref;
      --% no-document: True
   No_Base_Assoc : constant Base_Assoc;
      --% no-document: True
   No_Base_Assoc_List : constant Base_Assoc_List;
      --% no-document: True
   No_Base_Formal_Param_Decl : constant Base_Formal_Param_Decl;
      --% no-document: True
   No_Base_Formal_Param_Holder : constant Base_Formal_Param_Holder;
      --% no-document: True
   No_Single_Tok_Node : constant Single_Tok_Node;
      --% no-document: True
   No_Base_Id : constant Base_Id;
      --% no-document: True
   No_Base_Loop_Stmt : constant Base_Loop_Stmt;
      --% no-document: True
   No_Base_Package_Decl : constant Base_Package_Decl;
      --% no-document: True
   No_Base_Record_Def : constant Base_Record_Def;
      --% no-document: True
   No_Body_Node : constant Body_Node;
      --% no-document: True
   No_Base_Subp_Body : constant Base_Subp_Body;
      --% no-document: True
   No_Base_Subp_Spec : constant Base_Subp_Spec;
      --% no-document: True
   No_Base_Subtype_Decl : constant Base_Subtype_Decl;
      --% no-document: True
   No_Block_Stmt : constant Block_Stmt;
      --% no-document: True
   No_Begin_Block : constant Begin_Block;
      --% no-document: True
   No_Bin_Op : constant Bin_Op;
      --% no-document: True
   No_Body_Stub : constant Body_Stub;
      --% no-document: True
   No_Box_Expr : constant Box_Expr;
      --% no-document: True
   No_Call_Expr : constant Call_Expr;
      --% no-document: True
   No_Call_Stmt : constant Call_Stmt;
      --% no-document: True
   No_Case_Expr : constant Case_Expr;
      --% no-document: True
   No_Case_Expr_Alternative : constant Case_Expr_Alternative;
      --% no-document: True
   No_Case_Expr_Alternative_List : constant Case_Expr_Alternative_List;
      --% no-document: True
   No_Case_Stmt : constant Case_Stmt;
      --% no-document: True
   No_Case_Stmt_Alternative : constant Case_Stmt_Alternative;
      --% no-document: True
   No_Case_Stmt_Alternative_List : constant Case_Stmt_Alternative_List;
      --% no-document: True
   No_Char_Literal : constant Char_Literal;
      --% no-document: True
   No_Classwide_Type_Decl : constant Classwide_Type_Decl;
      --% no-document: True
   No_Compilation_Unit : constant Compilation_Unit;
      --% no-document: True
   No_Compilation_Unit_List : constant Compilation_Unit_List;
      --% no-document: True
   No_Component_Clause : constant Component_Clause;
      --% no-document: True
   No_Component_Decl : constant Component_Decl;
      --% no-document: True
   No_Component_Def : constant Component_Def;
      --% no-document: True
   No_Component_List : constant Component_List;
      --% no-document: True
   No_Concrete_Formal_Subp_Decl : constant Concrete_Formal_Subp_Decl;
      --% no-document: True
   No_Constant_Node : constant Constant_Node;
      --% no-document: True
   No_Constant_Absent : constant Constant_Absent;
      --% no-document: True
   No_Constant_Present : constant Constant_Present;
      --% no-document: True
   No_Constrained_Array_Indices : constant Constrained_Array_Indices;
      --% no-document: True
   No_Subtype_Indication : constant Subtype_Indication;
      --% no-document: True
   No_Constrained_Subtype_Indication : constant Constrained_Subtype_Indication;
      --% no-document: True
   No_Constraint : constant Constraint;
      --% no-document: True
   No_Constraint_List : constant Constraint_List;
      --% no-document: True
   No_Contract_Case_Assoc : constant Contract_Case_Assoc;
      --% no-document: True
   No_Contract_Case_Assoc_List : constant Contract_Case_Assoc_List;
      --% no-document: True
   No_Contract_Cases : constant Contract_Cases;
      --% no-document: True
   No_Real_Type_Def : constant Real_Type_Def;
      --% no-document: True
   No_Decimal_Fixed_Point_Def : constant Decimal_Fixed_Point_Def;
      --% no-document: True
   No_Decl_Block : constant Decl_Block;
      --% no-document: True
   No_Decl_List : constant Decl_List;
      --% no-document: True
   No_Declarative_Part : constant Declarative_Part;
      --% no-document: True
   No_Defining_Name : constant Defining_Name;
      --% no-document: True
   No_Defining_Name_List : constant Defining_Name_List;
      --% no-document: True
   No_Delay_Stmt : constant Delay_Stmt;
      --% no-document: True
   No_Delta_Constraint : constant Delta_Constraint;
      --% no-document: True
   No_Derived_Type_Def : constant Derived_Type_Def;
      --% no-document: True
   No_Digits_Constraint : constant Digits_Constraint;
      --% no-document: True
   No_Discrete_Base_Subtype_Decl : constant Discrete_Base_Subtype_Decl;
      --% no-document: True
   No_Discrete_Subtype_Indication : constant Discrete_Subtype_Indication;
      --% no-document: True
   No_Discrete_Subtype_Name : constant Discrete_Subtype_Name;
      --% no-document: True
   No_Discriminant_Assoc : constant Discriminant_Assoc;
      --% no-document: True
   No_Identifier_List : constant Identifier_List;
      --% no-document: True
   No_Discriminant_Choice_List : constant Discriminant_Choice_List;
      --% no-document: True
   No_Discriminant_Constraint : constant Discriminant_Constraint;
      --% no-document: True
   No_Discriminant_Part : constant Discriminant_Part;
      --% no-document: True
   No_Discriminant_Spec : constant Discriminant_Spec;
      --% no-document: True
   No_Discriminant_Spec_List : constant Discriminant_Spec_List;
      --% no-document: True
   No_Dotted_Name : constant Dotted_Name;
      --% no-document: True
   No_Elsif_Expr_Part : constant Elsif_Expr_Part;
      --% no-document: True
   No_Elsif_Expr_Part_List : constant Elsif_Expr_Part_List;
      --% no-document: True
   No_Elsif_Stmt_Part : constant Elsif_Stmt_Part;
      --% no-document: True
   No_Elsif_Stmt_Part_List : constant Elsif_Stmt_Part_List;
      --% no-document: True
   No_End_Name : constant End_Name;
      --% no-document: True
   No_Entry_Body : constant Entry_Body;
      --% no-document: True
   No_Entry_Completion_Formal_Params : constant Entry_Completion_Formal_Params;
      --% no-document: True
   No_Entry_Decl : constant Entry_Decl;
      --% no-document: True
   No_Entry_Index_Spec : constant Entry_Index_Spec;
      --% no-document: True
   No_Entry_Spec : constant Entry_Spec;
      --% no-document: True
   No_Enum_Lit_Synth_Type_Expr : constant Enum_Lit_Synth_Type_Expr;
      --% no-document: True
   No_Enum_Literal_Decl : constant Enum_Literal_Decl;
      --% no-document: True
   No_Enum_Literal_Decl_List : constant Enum_Literal_Decl_List;
      --% no-document: True
   No_Enum_Rep_Clause : constant Enum_Rep_Clause;
      --% no-document: True
   No_Enum_Subp_Spec : constant Enum_Subp_Spec;
      --% no-document: True
   No_Enum_Type_Def : constant Enum_Type_Def;
      --% no-document: True
   No_Error_Decl : constant Error_Decl;
      --% no-document: True
   No_Error_Stmt : constant Error_Stmt;
      --% no-document: True
   No_Exception_Decl : constant Exception_Decl;
      --% no-document: True
   No_Exception_Handler : constant Exception_Handler;
      --% no-document: True
   No_Exit_Stmt : constant Exit_Stmt;
      --% no-document: True
   No_Explicit_Deref : constant Explicit_Deref;
      --% no-document: True
   No_Expr_List : constant Expr_List;
      --% no-document: True
   No_Expr_Alternatives_List : constant Expr_Alternatives_List;
      --% no-document: True
   No_Expr_Function : constant Expr_Function;
      --% no-document: True
   No_Extended_Return_Stmt : constant Extended_Return_Stmt;
      --% no-document: True
   No_Extended_Return_Stmt_Object_Decl : constant Extended_Return_Stmt_Object_Decl;
      --% no-document: True
   No_Floating_Point_Def : constant Floating_Point_Def;
      --% no-document: True
   No_Loop_Spec : constant Loop_Spec;
      --% no-document: True
   No_For_Loop_Spec : constant For_Loop_Spec;
      --% no-document: True
   No_For_Loop_Stmt : constant For_Loop_Stmt;
      --% no-document: True
   No_For_Loop_Var_Decl : constant For_Loop_Var_Decl;
      --% no-document: True
   No_Formal_Discrete_Type_Def : constant Formal_Discrete_Type_Def;
      --% no-document: True
   No_Generic_Decl : constant Generic_Decl;
      --% no-document: True
   No_Generic_Formal : constant Generic_Formal;
      --% no-document: True
   No_Generic_Formal_Obj_Decl : constant Generic_Formal_Obj_Decl;
      --% no-document: True
   No_Generic_Formal_Package : constant Generic_Formal_Package;
      --% no-document: True
   No_Generic_Formal_Part : constant Generic_Formal_Part;
      --% no-document: True
   No_Generic_Formal_Subp_Decl : constant Generic_Formal_Subp_Decl;
      --% no-document: True
   No_Generic_Formal_Type_Decl : constant Generic_Formal_Type_Decl;
      --% no-document: True
   No_Generic_Instantiation : constant Generic_Instantiation;
      --% no-document: True
   No_Generic_Package_Decl : constant Generic_Package_Decl;
      --% no-document: True
   No_Generic_Package_Instantiation : constant Generic_Package_Instantiation;
      --% no-document: True
   No_Generic_Package_Internal : constant Generic_Package_Internal;
      --% no-document: True
   No_Generic_Renaming_Decl : constant Generic_Renaming_Decl;
      --% no-document: True
   No_Generic_Package_Renaming_Decl : constant Generic_Package_Renaming_Decl;
      --% no-document: True
   No_Generic_Subp_Decl : constant Generic_Subp_Decl;
      --% no-document: True
   No_Generic_Subp_Instantiation : constant Generic_Subp_Instantiation;
      --% no-document: True
   No_Generic_Subp_Internal : constant Generic_Subp_Internal;
      --% no-document: True
   No_Generic_Subp_Renaming_Decl : constant Generic_Subp_Renaming_Decl;
      --% no-document: True
   No_Goto_Stmt : constant Goto_Stmt;
      --% no-document: True
   No_Handled_Stmts : constant Handled_Stmts;
      --% no-document: True
   No_Identifier : constant Identifier;
      --% no-document: True
   No_If_Expr : constant If_Expr;
      --% no-document: True
   No_If_Stmt : constant If_Stmt;
      --% no-document: True
   No_Incomplete_Type_Decl : constant Incomplete_Type_Decl;
      --% no-document: True
   No_Incomplete_Tagged_Type_Decl : constant Incomplete_Tagged_Type_Decl;
      --% no-document: True
   No_Index_Constraint : constant Index_Constraint;
      --% no-document: True
   No_Num_Literal : constant Num_Literal;
      --% no-document: True
   No_Int_Literal : constant Int_Literal;
      --% no-document: True
   No_Interface_Kind : constant Interface_Kind;
      --% no-document: True
   No_Interface_Kind_Limited : constant Interface_Kind_Limited;
      --% no-document: True
   No_Interface_Kind_Protected : constant Interface_Kind_Protected;
      --% no-document: True
   No_Interface_Kind_Synchronized : constant Interface_Kind_Synchronized;
      --% no-document: True
   No_Interface_Kind_Task : constant Interface_Kind_Task;
      --% no-document: True
   No_Interface_Type_Def : constant Interface_Type_Def;
      --% no-document: True
   No_Iter_Type : constant Iter_Type;
      --% no-document: True
   No_Iter_Type_In : constant Iter_Type_In;
      --% no-document: True
   No_Iter_Type_Of : constant Iter_Type_Of;
      --% no-document: True
   No_Known_Discriminant_Part : constant Known_Discriminant_Part;
      --% no-document: True
   No_Label : constant Label;
      --% no-document: True
   No_Label_Decl : constant Label_Decl;
      --% no-document: True
   No_Library_Item : constant Library_Item;
      --% no-document: True
   No_Limited_Node : constant Limited_Node;
      --% no-document: True
   No_Limited_Absent : constant Limited_Absent;
      --% no-document: True
   No_Limited_Present : constant Limited_Present;
      --% no-document: True
   No_Loop_Stmt : constant Loop_Stmt;
      --% no-document: True
   No_Membership_Expr : constant Membership_Expr;
      --% no-document: True
   No_Mod_Int_Type_Def : constant Mod_Int_Type_Def;
      --% no-document: True
   No_Mode : constant Mode;
      --% no-document: True
   No_Mode_Default : constant Mode_Default;
      --% no-document: True
   No_Mode_In : constant Mode_In;
      --% no-document: True
   No_Mode_In_Out : constant Mode_In_Out;
      --% no-document: True
   No_Mode_Out : constant Mode_Out;
      --% no-document: True
   No_Multi_Dim_Array_Assoc : constant Multi_Dim_Array_Assoc;
      --% no-document: True
   No_Name_List : constant Name_List;
      --% no-document: True
   No_Named_Stmt : constant Named_Stmt;
      --% no-document: True
   No_Named_Stmt_Decl : constant Named_Stmt_Decl;
      --% no-document: True
   No_Not_Null : constant Not_Null;
      --% no-document: True
   No_Not_Null_Absent : constant Not_Null_Absent;
      --% no-document: True
   No_Not_Null_Present : constant Not_Null_Present;
      --% no-document: True
   No_Null_Component_Decl : constant Null_Component_Decl;
      --% no-document: True
   No_Null_Literal : constant Null_Literal;
      --% no-document: True
   No_Null_Record_Aggregate : constant Null_Record_Aggregate;
      --% no-document: True
   No_Null_Record_Def : constant Null_Record_Def;
      --% no-document: True
   No_Null_Stmt : constant Null_Stmt;
      --% no-document: True
   No_Null_Subp_Decl : constant Null_Subp_Decl;
      --% no-document: True
   No_Number_Decl : constant Number_Decl;
      --% no-document: True
   No_Op : constant Op;
      --% no-document: True
   No_Op_Abs : constant Op_Abs;
      --% no-document: True
   No_Op_And : constant Op_And;
      --% no-document: True
   No_Op_And_Then : constant Op_And_Then;
      --% no-document: True
   No_Op_Concat : constant Op_Concat;
      --% no-document: True
   No_Op_Div : constant Op_Div;
      --% no-document: True
   No_Op_Double_Dot : constant Op_Double_Dot;
      --% no-document: True
   No_Op_Eq : constant Op_Eq;
      --% no-document: True
   No_Op_Gt : constant Op_Gt;
      --% no-document: True
   No_Op_Gte : constant Op_Gte;
      --% no-document: True
   No_Op_In : constant Op_In;
      --% no-document: True
   No_Op_Lt : constant Op_Lt;
      --% no-document: True
   No_Op_Lte : constant Op_Lte;
      --% no-document: True
   No_Op_Minus : constant Op_Minus;
      --% no-document: True
   No_Op_Mod : constant Op_Mod;
      --% no-document: True
   No_Op_Mult : constant Op_Mult;
      --% no-document: True
   No_Op_Neq : constant Op_Neq;
      --% no-document: True
   No_Op_Not : constant Op_Not;
      --% no-document: True
   No_Op_Not_In : constant Op_Not_In;
      --% no-document: True
   No_Op_Or : constant Op_Or;
      --% no-document: True
   No_Op_Or_Else : constant Op_Or_Else;
      --% no-document: True
   No_Op_Plus : constant Op_Plus;
      --% no-document: True
   No_Op_Pow : constant Op_Pow;
      --% no-document: True
   No_Op_Rem : constant Op_Rem;
      --% no-document: True
   No_Op_Xor : constant Op_Xor;
      --% no-document: True
   No_Ordinary_Fixed_Point_Def : constant Ordinary_Fixed_Point_Def;
      --% no-document: True
   No_Others_Designator : constant Others_Designator;
      --% no-document: True
   No_Overriding_Node : constant Overriding_Node;
      --% no-document: True
   No_Overriding_Not_Overriding : constant Overriding_Not_Overriding;
      --% no-document: True
   No_Overriding_Overriding : constant Overriding_Overriding;
      --% no-document: True
   No_Overriding_Unspecified : constant Overriding_Unspecified;
      --% no-document: True
   No_Package_Body : constant Package_Body;
      --% no-document: True
   No_Package_Body_Stub : constant Package_Body_Stub;
      --% no-document: True
   No_Package_Decl : constant Package_Decl;
      --% no-document: True
   No_Package_Renaming_Decl : constant Package_Renaming_Decl;
      --% no-document: True
   No_Param_Assoc : constant Param_Assoc;
      --% no-document: True
   No_Param_Spec : constant Param_Spec;
      --% no-document: True
   No_Param_Spec_List : constant Param_Spec_List;
      --% no-document: True
   No_Params : constant Params;
      --% no-document: True
   No_Paren_Expr : constant Paren_Expr;
      --% no-document: True
   No_Parent_List : constant Parent_List;
      --% no-document: True
   No_Pragma_Argument_Assoc : constant Pragma_Argument_Assoc;
      --% no-document: True
   No_Pragma_Node : constant Pragma_Node;
      --% no-document: True
   No_Pragma_Node_List : constant Pragma_Node_List;
      --% no-document: True
   No_Prim_Type_Accessor : constant Prim_Type_Accessor;
      --% no-document: True
   No_Private_Node : constant Private_Node;
      --% no-document: True
   No_Private_Absent : constant Private_Absent;
      --% no-document: True
   No_Private_Part : constant Private_Part;
      --% no-document: True
   No_Private_Present : constant Private_Present;
      --% no-document: True
   No_Private_Type_Def : constant Private_Type_Def;
      --% no-document: True
   No_Protected_Node : constant Protected_Node;
      --% no-document: True
   No_Protected_Absent : constant Protected_Absent;
      --% no-document: True
   No_Protected_Body : constant Protected_Body;
      --% no-document: True
   No_Protected_Body_Stub : constant Protected_Body_Stub;
      --% no-document: True
   No_Protected_Def : constant Protected_Def;
      --% no-document: True
   No_Protected_Present : constant Protected_Present;
      --% no-document: True
   No_Protected_Type_Decl : constant Protected_Type_Decl;
      --% no-document: True
   No_Public_Part : constant Public_Part;
      --% no-document: True
   No_Qual_Expr : constant Qual_Expr;
      --% no-document: True
   No_Quantified_Expr : constant Quantified_Expr;
      --% no-document: True
   No_Quantifier : constant Quantifier;
      --% no-document: True
   No_Quantifier_All : constant Quantifier_All;
      --% no-document: True
   No_Quantifier_Some : constant Quantifier_Some;
      --% no-document: True
   No_Raise_Expr : constant Raise_Expr;
      --% no-document: True
   No_Raise_Stmt : constant Raise_Stmt;
      --% no-document: True
   No_Range_Constraint : constant Range_Constraint;
      --% no-document: True
   No_Range_Spec : constant Range_Spec;
      --% no-document: True
   No_Real_Literal : constant Real_Literal;
      --% no-document: True
   No_Record_Def : constant Record_Def;
      --% no-document: True
   No_Record_Rep_Clause : constant Record_Rep_Clause;
      --% no-document: True
   No_Record_Type_Def : constant Record_Type_Def;
      --% no-document: True
   No_Relation_Op : constant Relation_Op;
      --% no-document: True
   No_Renaming_Clause : constant Renaming_Clause;
      --% no-document: True
   No_Requeue_Stmt : constant Requeue_Stmt;
      --% no-document: True
   No_Return_Stmt : constant Return_Stmt;
      --% no-document: True
   No_Reverse_Node : constant Reverse_Node;
      --% no-document: True
   No_Reverse_Absent : constant Reverse_Absent;
      --% no-document: True
   No_Reverse_Present : constant Reverse_Present;
      --% no-document: True
   No_Select_Stmt : constant Select_Stmt;
      --% no-document: True
   No_Select_When_Part : constant Select_When_Part;
      --% no-document: True
   No_Select_When_Part_List : constant Select_When_Part_List;
      --% no-document: True
   No_Signed_Int_Type_Def : constant Signed_Int_Type_Def;
      --% no-document: True
   No_Single_Protected_Decl : constant Single_Protected_Decl;
      --% no-document: True
   No_Single_Task_Decl : constant Single_Task_Decl;
      --% no-document: True
   No_Task_Type_Decl : constant Task_Type_Decl;
      --% no-document: True
   No_Single_Task_Type_Decl : constant Single_Task_Type_Decl;
      --% no-document: True
   No_Stmt_List : constant Stmt_List;
      --% no-document: True
   No_String_Literal : constant String_Literal;
      --% no-document: True
   No_Subp_Body : constant Subp_Body;
      --% no-document: True
   No_Subp_Body_Stub : constant Subp_Body_Stub;
      --% no-document: True
   No_Subp_Decl : constant Subp_Decl;
      --% no-document: True
   No_Subp_Kind : constant Subp_Kind;
      --% no-document: True
   No_Subp_Kind_Function : constant Subp_Kind_Function;
      --% no-document: True
   No_Subp_Kind_Procedure : constant Subp_Kind_Procedure;
      --% no-document: True
   No_Subp_Renaming_Decl : constant Subp_Renaming_Decl;
      --% no-document: True
   No_Subp_Spec : constant Subp_Spec;
      --% no-document: True
   No_Subtype_Decl : constant Subtype_Decl;
      --% no-document: True
   No_Subunit : constant Subunit;
      --% no-document: True
   No_Synchronized_Node : constant Synchronized_Node;
      --% no-document: True
   No_Synchronized_Absent : constant Synchronized_Absent;
      --% no-document: True
   No_Synchronized_Present : constant Synchronized_Present;
      --% no-document: True
   No_Synth_Anonymous_Type_Decl : constant Synth_Anonymous_Type_Decl;
      --% no-document: True
   No_Synthetic_Renaming_Clause : constant Synthetic_Renaming_Clause;
      --% no-document: True
   No_Tagged_Node : constant Tagged_Node;
      --% no-document: True
   No_Tagged_Absent : constant Tagged_Absent;
      --% no-document: True
   No_Tagged_Present : constant Tagged_Present;
      --% no-document: True
   No_Target_Name : constant Target_Name;
      --% no-document: True
   No_Task_Body : constant Task_Body;
      --% no-document: True
   No_Task_Body_Stub : constant Task_Body_Stub;
      --% no-document: True
   No_Task_Def : constant Task_Def;
      --% no-document: True
   No_Terminate_Alternative : constant Terminate_Alternative;
      --% no-document: True
   No_Type_Access_Def : constant Type_Access_Def;
      --% no-document: True
   No_Un_Op : constant Un_Op;
      --% no-document: True
   No_Unconstrained_Array_Index : constant Unconstrained_Array_Index;
      --% no-document: True
   No_Unconstrained_Array_Index_List : constant Unconstrained_Array_Index_List;
      --% no-document: True
   No_Unconstrained_Array_Indices : constant Unconstrained_Array_Indices;
      --% no-document: True
   No_Unknown_Discriminant_Part : constant Unknown_Discriminant_Part;
      --% no-document: True
   No_Until_Node : constant Until_Node;
      --% no-document: True
   No_Until_Absent : constant Until_Absent;
      --% no-document: True
   No_Until_Present : constant Until_Present;
      --% no-document: True
   No_Update_Attribute_Ref : constant Update_Attribute_Ref;
      --% no-document: True
   No_Use_Clause : constant Use_Clause;
      --% no-document: True
   No_Use_Package_Clause : constant Use_Package_Clause;
      --% no-document: True
   No_Use_Type_Clause : constant Use_Type_Clause;
      --% no-document: True
   No_Variant : constant Variant;
      --% no-document: True
   No_Variant_List : constant Variant_List;
      --% no-document: True
   No_Variant_Part : constant Variant_Part;
      --% no-document: True
   No_While_Loop_Spec : constant While_Loop_Spec;
      --% no-document: True
   No_While_Loop_Stmt : constant While_Loop_Stmt;
      --% no-document: True
   No_With_Clause : constant With_Clause;
      --% no-document: True
   No_With_Private : constant With_Private;
      --% no-document: True
   No_With_Private_Absent : constant With_Private_Absent;
      --% no-document: True
   No_With_Private_Present : constant With_Private_Present;
      --% no-document: True

   function Is_Null (Node : Ada_Node'Class) return Boolean;
   --  Return whether this node is a null node reference.

   function Is_Token_Node (Node : Ada_Node'Class) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic (Node : Ada_Node'Class) return Boolean;
   --  Return whether this node is synthetic.

   function "=" (L, R : Ada_Node'Class) return Boolean;
   --  Return whether L and R designate the same entity

   function Short_Text_Image (Node : Ada_Node'Class) return Text_Type;
   function Short_Image (Node : Ada_Node'Class) return String;
   --  Return a short string describing Node, or "None" if Node.Is_Null is
   --  true.

   function Text_Image (Node : Ada_Node'Class) return Text_Type;
   function Image (Node : Ada_Node'Class) return String;
   --  Like Short_Image, also including its rebinding metadata

   --------------------
   -- Unit providers --
   --------------------

   type Unit_Provider_Interface is interface;
   --  Interface to fetch analysis units from a name and a unit kind.
   --
   --  The unit provider mechanism provides an abstraction which assumes that
   --  to any couple (unit name, unit kind) we can associate at most one source
   --  file. This means that several couples can be associated to the same
   --  source file, but on the other hand, only one one source file can be
   --  associated to a couple.
   --
   --  This is used to make the semantic analysis able to switch from one
   --  analysis units to another.

   function Get_Unit_Filename
     (Provider : Unit_Provider_Interface; Name : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   function Get_Unit
     (Provider : Unit_Provider_Interface; Context : Analysis_Context'Class;
      Name     : Text_Type; Kind : Analysis_Unit_Kind; Charset : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Release (Provider : in out Unit_Provider_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Provider

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class);
   --  Helper for the instantiation below

   package Unit_Provider_References is new GNATCOLL.Refcount.Shared_Pointers
     (Unit_Provider_Interface'Class, Do_Release);

   subtype Unit_Provider_Reference is Unit_Provider_References.Ref;
   No_Unit_Provider_Reference : Unit_Provider_Reference renames
     Unit_Provider_References.Null_Ref;

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create unit provider
   --  references.

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create_Context
     (Charset       : String                  := Default_Charset;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      With_Trivia   : Boolean := True; Tab_Stop : Positive := 8)
      return Analysis_Context;
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources
   --  in analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, ``"iso-8859-1"`` is
   --  the default.
   --
   --  .. todo:: Passing an unsupported charset here is not guaranteed to raise
   --     an error right here, but this would be really helpful for users.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``Unit_Provider`` will be used to query the file name
   --  that corresponds to a unit reference during semantic analysis. If it
   --  is ``null``, the default one is used instead.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.
   --% belongs-to: Analysis_Context

   function Has_Unit
     (Context : Analysis_Context'Class; Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit correponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String       := ""; Reparse : Boolean := False;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit with
      Pre => not Reparse or else not Has_Rewriting_Handle (Context);
      --  Create a new analysis unit for ``Filename`` or return the existing
      --  one if any. If ``Reparse`` is true and the analysis unit already
      --  exists, reparse it from ``Filename``.
      --
      --  ``Rule`` controls which grammar rule is used to parse the unit.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If any failure occurs, such as file opening, decoding, lexing or
      --  parsing failure, return an analysis unit anyway: errors are described
      --  as diagnostics of the returned analysis unit.

   function Get_From_Buffer
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String       := ""; Buffer : String;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit with
      Pre => not Has_Rewriting_Handle (Context);
      --  Create a new analysis unit for ``Filename`` or return the existing
      --  one if any. Whether the analysis unit already exists or not,
      --  (re)parse it from the source code in ``Buffer``.
      --
      --  ``Rule`` controls which grammar rule is used to parse the unit.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If any failure occurs, such as file opening, decoding, lexing or
      --  parsing failure, return an analysis unit anyway: errors are described
      --  as diagnostics of the returned analysis unit.

   function Get_From_Buffer
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String := ""; Buffer : Ada.Strings.Unbounded.Unbounded_String;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit with
      Pre => not Has_Rewriting_Handle (Context);
      --  Likewise, but working on an unbounded string

   function Get_With_Error
     (Context : Analysis_Context'Class; Filename : String; Error : Text_Type;
      Charset : String := ""; Rule : Grammar_Rule := Default_Grammar_Rule)
      return Analysis_Unit;
   --  If a Unit for ``Filename`` already exists, return it unchanged.
   --  Otherwise, create an empty analysis unit for ``Filename`` with a
   --  diagnostic that contains the ``Error`` message.

   function Get_From_Provider
     (Context : Analysis_Context'Class; Name : Text_Type;
      Kind    : Analysis_Unit_Kind; Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit with
      Pre => not Reparse or else not Has_Rewriting_Handle (Context);
      --  Create a new analysis unit for ``Name``/``Kind`` or return the
      --  existing one if any. If ``Reparse`` is true and the analysis
      --  unit already exists, reparse it from ``Filename``.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If the unit name cannot be tuned into a file name, raise an
      --  ``Invalid_Unit_Name_Error`` exception. If any other failure occurs,
      --  such as file opening, decoding, lexing or parsing failure, return
      --  an analysis unit anyway: errors are described as diagnostics of the
      --  returned analysis unit.

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference;
   --  Return the unit provider for ``Context``
   --
   --% belongs-to: Analysis_Context

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type;
   --  Return a hash for this context, to be used in hash tables.

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` keeps trivia when parsing units

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean);
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural);
   --  If ``Timeout`` is greater than zero, set a timeout for the resolution of
   --  logic equations. The unit is the number of steps in ANY/ALL relations.
   --  If ``Timeout`` is zero, disable the timeout. By default, the timeout is
   --  ``100 000`` steps.

   procedure Disable_Lookup_Cache (Disable : Boolean := True);
   --  Debug helper: if ``Disable`` is true, disable the use of caches in
   --  lexical environment lookups. Otherwise, activate it.

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` has a rewriting handler (see
   --  ``Libadalang.Rewriting``), i.e. whether it is in the process of
   --  rewriting. If true, this means that the set of currently loaded
   --  analysis units is frozen until the rewriting process is done.

   ------------------------------
   -- Analysis unit primitives --
   ------------------------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context;
   --  Return the context that owns this unit.

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type;
   --  Return a hash for this unit, to be used in hash tables.

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "");
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer : String);
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), raise a
   --  ``Property_Error`` on failure.

   function Get_Filename (Unit : Analysis_Unit'Class) return String;
   --  Return the filename this unit is associated to.

   function Get_Charset (Unit : Analysis_Unit'Class) return String;
   --  Return the charset that was used to parse Unit

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean;
   --  Return whether this unit has associated diagnostics.

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array;
   --  Return an array that contains the diagnostics associated to this unit.

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   pragma Warnings (Off, "defined after private extension");
   function Root (Unit : Analysis_Unit'Class) return Ada_Node;
   --  Return the root node for this unit, or ``null`` if there is none.
   pragma Warnings (On, "defined after private extension");

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the first token scanned in this unit.

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the last token scanned in this unit.

   function Token_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of tokens in this unit.

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   function Text (Unit : Analysis_Unit'Class) return Text_Type;
   --  Return the source buffer associated to this unit.

   function Debug_Text (Unit : Analysis_Unit'Class) return String;
   --  Like ``Text``, to get the source buffer slice as a string

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference;
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Debug helper: output the lexical envs for the given analysis unit.

   procedure Trigger_Envs_Debug (Is_Active : Boolean);
   --  Debug helper: activate debug traces for lexical envs lookups

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.

   procedure PP_Trivia (Unit : Analysis_Unit'Class);
   --  Debug helper: output a minimal AST with mixed trivias

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Ada_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   type Children_Array is array (Positive range <>) of Child_Record;

   function Children_With_Trivia (Node : Ada_Node'Class) return Children_Array;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --
   --  - Every trivia contained between ``Node.Start_Token`` and
   --    ``Node.End_Token - 1`` will be part of the returned array.
   --
   --  - Nodes and trivias will be lexically ordered.

   ---------------------
   -- Composite types --
   ---------------------

   type Aspect is private;
   --  Composite field representing the aspect of an entity (RM 13).

   function Exists (Self : Aspect) return Boolean;
   --  Whether the aspect is defined or not

   function Node (Self : Aspect) return Ada_Node'Class;
   --  Syntactic node that defines the aspect

   function Value (Self : Aspect) return Expr'Class;
   --  Expr node defining the value of the aspect

   function Create_Aspect
     (Exists : Boolean; Node : Ada_Node'Class; Value : Expr'Class)
      return Aspect;

   type Completion_Item is private;

   function Decl (Self : Completion_Item) return Basic_Decl'Class;

   function Is_Dot_Call (Self : Completion_Item) return Boolean;

   function Create_Completion_Item
     (Decl : Basic_Decl'Class; Is_Dot_Call : Boolean) return Completion_Item;

   type Completion_Item_Array is array (Positive range <>) of Completion_Item;

   type Discrete_Range is private;
   --  Represent the range of a discrete type or subtype. The bounds are not
   --  evaluated, you need to call ``eval_as_int`` on them, if they're static,
   --  to get their value.

   function Low_Bound (Self : Discrete_Range) return Expr'Class;

   function High_Bound (Self : Discrete_Range) return Expr'Class;

   function Create_Discrete_Range
     (Low_Bound : Expr'Class; High_Bound : Expr'Class) return Discrete_Range;

   type Doc_Annotation is private;
   --  Documentation annotation.

   function Key (Self : Doc_Annotation) return Text_Type;
   --  Annotation key

   function Value (Self : Doc_Annotation) return Text_Type;
   --  Annotation value

   function Create_Doc_Annotation
     (Key : Text_Type; Value : Text_Type) return Doc_Annotation;

   type Doc_Annotation_Array is array (Positive range <>) of Doc_Annotation;

   type Ada_Node_Array is array (Positive range <>) of Ada_Node;

   type Base_Formal_Param_Decl_Array is
     array (Positive range <>) of Base_Formal_Param_Decl;

   type Base_Type_Decl_Array is array (Positive range <>) of Base_Type_Decl;

   type Basic_Decl_Array is array (Positive range <>) of Basic_Decl;

   type Compilation_Unit_Array is
     array (Positive range <>) of Compilation_Unit;

   type Defining_Name_Array is array (Positive range <>) of Defining_Name;

   type Generic_Instantiation_Array is
     array (Positive range <>) of Generic_Instantiation;

   type Param_Spec_Array is array (Positive range <>) of Param_Spec;

   type Type_Decl_Array is array (Positive range <>) of Type_Decl;

   type Param_Actual is private;
   --  Data structure used by zip_with_params property. Associates an
   --  expression (the actual) to a formal param declaration (the parameter).

   function Param (Self : Param_Actual) return Defining_Name'Class;

   function Actual (Self : Param_Actual) return Expr'Class;

   function Create_Param_Actual
     (Param : Defining_Name'Class; Actual : Expr'Class) return Param_Actual;

   type Param_Actual_Array is array (Positive range <>) of Param_Actual;

   type Ref_Result is private;
   --  Result for a cross reference query returning a reference.

   function Ref (Self : Ref_Result) return Base_Id'Class;

   function Kind (Self : Ref_Result) return Ref_Result_Kind;

   function Create_Ref_Result
     (Ref : Base_Id'Class; Kind : Ref_Result_Kind) return Ref_Result;

   type Ref_Result_Array is array (Positive range <>) of Ref_Result;

   type Refd_Decl is private;
   --  Result for a cross reference query returning a referenced decl.

   function Decl (Self : Refd_Decl) return Basic_Decl'Class;

   function Kind (Self : Refd_Decl) return Ref_Result_Kind;

   function Create_Refd_Decl
     (Decl : Basic_Decl'Class; Kind : Ref_Result_Kind) return Refd_Decl;

   type Refd_Def is private;
   --  Result for a cross reference query returning a referenced defining name.

   function Def_Name (Self : Refd_Def) return Defining_Name'Class;

   function Kind (Self : Refd_Def) return Ref_Result_Kind;

   function Create_Refd_Def
     (Def_Name : Defining_Name'Class; Kind : Ref_Result_Kind) return Refd_Def;

   type Substitution is private;
   --  Represent a substitution of a BasicDecl by a given value. This can then
   --  be used as part of an environment in the eval_as_*_in_env property. See
   --  the declaration of those properties for more details.

   function From_Decl (Self : Substitution) return Basic_Decl'Class;
   --  The declaration to substitute.

   function To_Value (Self : Substitution) return Big_Integer;
   --  The value by which to substitute the declaration.

   function Value_Type (Self : Substitution) return Base_Type_Decl'Class;
   --  The type of the substituted value.

   function Create_Substitution
     (From_Decl  : Basic_Decl'Class; To_Value : Big_Integer;
      Value_Type : Base_Type_Decl'Class) return Substitution;

   type Substitution_Array is array (Positive range <>) of Substitution;

   type Analysis_Unit_Array is array (Positive range <>) of Analysis_Unit;

   type Unbounded_Text_Type_Array is
     array (Positive range <>) of Unbounded_Text_Type;

   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private with
      Iterable => (First => First_Token, Next => Next_Token,
       Has_Element => Has_Element, Element => Element);
      --  Allow iteration on a range of tokens corresponding to a node

   function First_Token (Self : Token_Iterator) return Token_Reference;
   --  Return the first token corresponding to the node

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Return the token that follows Tok in the token stream

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean;
   --  Return if Tok is in Self's iteration range

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Identity function: helper for the Iterable aspect

   -------------------------
   -- AST Node primitives --
   -------------------------

   function Kind (Node : Ada_Node'Class) return Ada_Node_Kind_Type;
   function Kind_Name (Node : Ada_Node'Class) return String;
   --  Return the concrete kind for Node

   pragma Warnings (Off, "defined after private extension");

   function P_Declarative_Scope
     (Node : Ada_Node'Class) return Declarative_Part;
   --  Return the scope of definition of this basic declaration.

   function P_Complete (Node : Ada_Node'Class) return Completion_Item_Array;
   --  Return possible completions at this point in the file.

   function P_Valid_Keywords
     (Node : Ada_Node'Class) return Unbounded_Text_Type_Array;
   --  Return the list of keywords that are valid at this point in the file.
   --
   --  .. note:: This is work in progress. It will return all keywords for now,
   --     without looking at the context.

   function P_Generic_Instantiations
     (Node : Ada_Node'Class) return Generic_Instantiation_Array;
   --  Return the potentially empty list of generic package/subprogram
   --  instantiations that led to the creation of this entity. Outer-most
   --  instantiations appear last.

   function P_Semantic_Parent (Node : Ada_Node'Class) return Ada_Node;
   --  Return the semantic parent for this node, if applicable, null otherwise.

   function P_Parent_Basic_Decl (Node : Ada_Node'Class) return Basic_Decl;
   --  Return the parent basic decl for this node, if applicable, null
   --  otherwise.
   --
   --  .. note:: If the parent BasicDecl of the given node is a generic
   --     declaration, this call will return the instantiation from which the
   --     node was retrieved instead, if any.

   function P_Filter_Is_Imported_By
     (Node : Ada_Node'Class; Units : Analysis_Unit_Array; Transitive : Boolean)
      return Analysis_Unit_Array;
   --  Filters out among the list of given units those that cannot refer to
   --  the unit in which this node lies. If transitive is True, the whole
   --  transitive closure of imports will be used to find a reference to
   --  the unit of this node.

   function P_Xref_Entry_Point (Node : Ada_Node'Class) return Boolean;
   --  Designates entities that are entry point for the xref solving
   --  infrastructure. If this returns true, then resolve_names can be
   --  called on it.
   --
   --  .. note:: For convenience, and unlike what is defined in the ARM wrt.
   --     complete contexts for name resolution, ``xref_entry_points`` can be
   --     nested.

   function P_Resolve_Names (Node : Ada_Node'Class) return Boolean;
   --  This will resolve names for this node. If the operation is successful,
   --  then type_var and ref_var will be bound on appropriate subnodes of the
   --  statement.

   function P_Standard_Unit (Node : Ada_Node'Class) return Analysis_Unit;
   --  Static method. Return the analysis unit corresponding to the Standard
   --  package.

   function P_Std_Entity
     (Node : Ada_Node'Class; Sym : Unbounded_Text_Type) return Ada_Node;
   --  Static property. Return an entity from the standard package with name
   --  `sym`.

   function P_Bool_Type (Node : Ada_Node'Class) return Ada_Node;
   --  Static method. Return the standard Boolean type.

   function P_Int_Type (Node : Ada_Node'Class) return Ada_Node;
   --  Static method. Return the standard Integer type.

   function P_Universal_Int_Type (Node : Ada_Node'Class) return Ada_Node;
   --  Static method. Return the standard Universal Integer type.

   function P_Universal_Real_Type (Node : Ada_Node'Class) return Ada_Node;
   --  Static method. Return the standard Universal Real type.

   function P_Top_Level_Decl
     (Node : Ada_Node'Class; Unit : Analysis_Unit'Class) return Basic_Decl;
   --  Static method. Get the top-level decl in ``unit``. This is the body of a
   --  Subunit, or the item of a ``LibraryItem``.

   function P_Choice_Match
     (Node : Ada_Node'Class; Value : Big_Integer) return Boolean;
   --  Assuming that self is a choice expression (such as what can appear
   --  in an alternative of a case statement or in the RHS of a membership
   --  expression, this property returns whether the given value satisfies it.
   --
   --  .. ATTENTION:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

   function P_Gnat_Xref
     (Node : Ada_Node'Class; Imprecise_Fallback : Boolean := False)
      return Defining_Name;
   --  Return a cross reference from this name to a defining identifier, trying
   --  to mimic GNAT's xrefs as much as possible.

   function Parent (Node : Ada_Node'Class) return Ada_Node;
   --  Return the lexical parent for this node. Return null for the root AST
   --  node or for AST nodes for which no one has a reference to the parent.

   function Parents (Node : Ada_Node'Class) return Ada_Node_Array;
   --  Return an array that contains the lexical parents (this node included).
   --  Nearer parents are first in the list.

   function Children (Node : Ada_Node'Class) return Ada_Node_Array;
   --  Return an array that contains the direct lexical children.

   function Token_Start (Node : Ada_Node'Class) return Token_Reference;
   --  Return the first token used to parse this node.

   function Token_End (Node : Ada_Node'Class) return Token_Reference;
   --  Return the last token used to parse this node.

   function Child_Index (Node : Ada_Node'Class) return Integer;
   --  Return the 0-based index for Node in its parent's children.

   function Previous_Sibling (Node : Ada_Node'Class) return Ada_Node;
   --  Return the node's previous sibling, if there is one.

   function Next_Sibling (Node : Ada_Node'Class) return Ada_Node;
   --  Return the node's next sibling, if there is one.

   function Unit (Node : Ada_Node'Class) return Analysis_Unit;
   --  Return the analysis unit owning this node.

   function Is_Ghost (Node : Ada_Node'Class) return Boolean;
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. The "token_first"
   --  of all ghost nodes is the token right after this logical position, while
   --  they have no "token_last".

   function Full_Sloc_Image (Node : Ada_Node'Class) return Text_Type;
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.

   function P_Expression_Type (Node : Expr'Class) return Base_Type_Decl;
   --  Return the declaration corresponding to the type of this expression
   --  after name resolution.

   function P_Is_Static_Expr
     (Node : Expr'Class; Imprecise_Fallback : Boolean := False) return Boolean;
   --  Return whether this expression is static according to the ARM definition
   --  of static. See RM 4.9.

   function P_First_Corresponding_Decl (Node : Expr'Class) return Basic_Decl;
   --  Return the first decl that is lexically named like self in self's scope.

   function P_Eval_As_Int (Node : Expr'Class) return Big_Integer;
   --  Statically evaluates self, and returns the value of the evaluation as an
   --  integer.
   --
   --  .. note:: In order for a call to this not to raise, the expression needs
   --     to be a static expression, as specified in the ARM section 4.9. You
   --     can verify whether an expression is static with the
   --     ``is_static_expr`` property.
   --
   --  .. ATTENTION:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

   function P_Eval_As_Int_In_Env
     (Node : Expr'Class; Env : Substitution_Array) return Big_Integer;
   --  Statically evaluates self, and returns the value of the evaluation as
   --  an integer. The given environment is used to substitute references to
   --  declarations by actual values.
   --
   --  .. note:: In order for a call to this not to raise, the expression needs
   --     to be a static expression, as specified in the ARM section 4.9. You
   --     can verify whether an expression is static with the
   --     ``is_static_expr`` property.
   --
   --  .. ATTENTION:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

   function P_Matching_Nodes (Node : Expr'Class) return Ada_Node_Array;
   --  Return the list of AST nodes that can be a match for this expression
   --  before overloading analysis.

   function F_Aspects (Node : Basic_Decl'Class) return Aspect_Spec;
   --  Return the list of aspects that are attached to this node.

   function P_Is_Formal (Node : Basic_Decl'Class) return Boolean;
   --  Whether this decl is the nested decl of a generic formal declaration.

   function P_Doc_Annotations
     (Node : Basic_Decl'Class) return Doc_Annotation_Array;
   --  Return the documentation annotations associated with this decl.
   --  Annotations are any comment line of the form::
   --
   --  --% [annotation_name]: [annotation]
   --
   --  Raises a property error if the doc is incorrectly formatted.
   --
   --  .. ATTENTION:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

   function P_Doc (Node : Basic_Decl'Class) return Text_Type;
   --  Return the documentation associated with this decl. Raises a property
   --  error if the doc is incorrectly formatted.
   --
   --  .. ATTENTION:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

   function P_Previous_Part_For_Decl
     (Node : Basic_Decl'Class) return Basic_Decl;
   --  Return the previous part for this decl, if applicable.
   --
   --  .. note:: It is not named previous_part, because BaseTypeDecl has a more
   --     precise version of previous_part that returns a BaseTypeDecl.
   --     Probably, we want to rename the specific versions, and have the root
   --     property be named previous_part. (TODO R925-008)

   function P_Canonical_Part (Node : Basic_Decl'Class) return Basic_Decl;
   --  Return the canonical part for this decl. In the case of decls composed
   --  of several parts, the canonical part will be the first part.

   function P_Is_Static_Decl
     (Node : Basic_Decl'Class; Imprecise_Fallback : Boolean := False)
      return Boolean;
   --  Return whether this declaration is static.

   function P_Is_Imported (Node : Basic_Decl'Class) return Boolean;
   --  Whether this declaration is imported from another language.

   function P_Get_Aspect_Assoc
     (Node : Basic_Decl'Class; Name : Unbounded_Text_Type) return Aspect_Assoc;
   --  Return the aspect with name ``name`` for this entity.

   function P_Get_Aspect_Spec_Expr
     (Node : Basic_Decl'Class; Name : Unbounded_Text_Type) return Expr;
   --  Return the expression associated to the aspect with name ``name`` for
   --  this entity.

   function P_Get_Aspect
     (Node               : Basic_Decl'Class; Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Aspect;
   --  Return the aspect with name ``name`` associated to this entity.
   --
   --  Aspects are properties of entities that can be specified by the Ada
   --  program, either via aspect specifications, pragmas, or attributes.
   --
   --  This will return the syntactic node corresponding to attribute directly.

   function P_Has_Aspect
     (Node               : Basic_Decl'Class; Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Boolean;
   --  Returns whether the boolean aspect named ``name`` is set on the entity
   --  represented by this node.
   --
   --  "Aspect" is used as in RM terminology (see RM 13).

   function P_Get_Pragma
     (Node : Basic_Decl'Class; Name : Unbounded_Text_Type) return Pragma_Node;
   --  Return the pragma with name ``name`` associated to this entity.

   function P_Get_Representation_Clause
     (Node               : Basic_Decl'Class; Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Attribute_Def_Clause;
   --  Return the representation clause associated to this type decl that
   --  defines the given attribute name.

   function P_Is_Compilation_Unit_Root
     (Node : Basic_Decl'Class) return Boolean;
   --  Whether a BasicDecl is the root decl for its unit.

   function P_Is_Visible
     (Node : Basic_Decl'Class; From_Node : Ada_Node'Class) return Boolean;
   --  Return whether this declaration is visible from the point of view of the
   --  given ``origin`` node.
   --
   --  .. ATTENTION:: Only package-level (public or private) declarations are
   --     supported for now.

   function P_Base_Subp_Declarations
     (Node : Basic_Decl'Class) return Basic_Decl_Array;
   --  If Self declares a primitive subprogram of some tagged type T, return
   --  the set of all subprogram declarations that it overrides (including
   --  itself).

   function P_Root_Subp_Declarations
     (Node : Basic_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Basic_Decl_Array;
   --  If Self declares a primitive subprogram of some tagged type T, return
   --  the root subprogram declarations that it overrides. There can be
   --  several, as in the following scenario:
   --
   --  - package Root defines the root tagged type T and subprogram Foo.
   --
   --  - package Itf defines interface I and abstract subprogram Foo.
   --
   --  - package D defines "type U is new Root.T and Itf.I" and an overriding
   --  subprogram Foo.
   --
   --  Here, root_subp_declarations of Foo defined in package D will return
   --  both Foo from package Root and Foo from package Itf.

   function P_Find_All_Overrides
     (Node               : Basic_Decl'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Basic_Decl_Array;
   --  If Self is the declaration of a primitive of some type T, return the
   --  list of all subprogram that override this subprogram among the given
   --  units.

   function P_Defining_Names
     (Node : Basic_Decl'Class) return Defining_Name_Array;
   --  Get all the names of this basic declaration.

   function P_Defining_Name (Node : Basic_Decl'Class) return Defining_Name;
   --  Get the name of this declaration. If this declaration has several names,
   --  it will return the first one.

   function P_Type_Expression (Node : Basic_Decl'Class) return Type_Expr;
   --  Return the type expression for this BasicDecl if applicable, a null
   --  otherwise.

   function P_Subp_Spec_Or_Null
     (Node : Basic_Decl'Class; Follow_Generic : Boolean := False)
      return Base_Subp_Spec;
   --  If Self is a Subp, returns the specification of this subprogram.
   --
   --  If ``follow_generic`` is True, will also work for instances of
   --  ``GenericSubpDecl``.

   function P_Is_Subprogram (Node : Basic_Decl'Class) return Boolean;
   --  Return True if self is a subprogram node in the general sense (which is,
   --  an entity that can be called). This includes separates and entries.

   function P_Relative_Name (Node : Basic_Decl'Class) return Single_Tok_Node;
   --  Return the relative name for Self. If Self's defining name is ``A.B.C``,
   --  return C as a node.

   function P_Relative_Name_Text
     (Node : Basic_Decl'Class) return Unbounded_Text_Type;
   --  Return the relative name for Self, as text.

   function P_Next_Part_For_Decl (Node : Basic_Decl'Class) return Basic_Decl;
   --  Return the next part of this declaration, if applicable.
   --
   --  .. note:: It is not named next_part, because BaseTypeDecl has a more
   --     precise version of next_part that returns a BaseTypeDecl. Probably,
   --     we want to rename the specific versions, and have the root property
   --     be named next_part. (TODO R925-008)

   function P_Body_Part_For_Decl (Node : Basic_Decl'Class) return Body_Node;
   --  Return the body corresponding to this declaration, if applicable.
   --
   --  .. note:: It is not named body_part, subclasses have more precise
   --     versions named body_part and returning a more precise result.
   --     Probably, we want to rename the specific versions, and have the root
   --     property be named previous_part. (TODO R925-008)

   function P_Fully_Qualified_Name_Array
     (Node : Basic_Decl'Class) return Unbounded_Text_Type_Array;
   --  Return the fully qualified name corresponding to this declaration, as an
   --  array of symbols.

   function P_Fully_Qualified_Name (Node : Basic_Decl'Class) return Text_Type;
   --  Return the fully qualified name corresponding to this declaration.

   function P_Canonical_Fully_Qualified_Name
     (Node : Basic_Decl'Class) return Text_Type;
   --  Return a canonical representation of the fully qualified name
   --  corresponding to this declaration.

   function P_Unique_Identifying_Name
     (Node : Basic_Decl'Class) return Text_Type;
   --  Return a unique identifying name for this declaration, provided this
   --  declaration is a public declaration. In the case of subprograms, this
   --  will include the profile.
   --
   --  .. attention:: This will only return a unique name for public
   --     declarations. Notably, anything nested in an unnamed declare block
   --     won't be handled correctly.

   function P_As_Bool (Node : Abort_Node'Class) return Boolean;
   --  Return whether this is an instance of AbortPresent

   function F_Names (Node : Abort_Stmt'Class) return Name_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function P_As_Bool (Node : Abstract_Node'Class) return Boolean;
   --  Return whether this is an instance of AbstractPresent

   function P_Subp_Decl_Spec
     (Node : Basic_Subp_Decl'Class) return Base_Subp_Spec;
   --  Return the specification for this subprogram

   function P_Body_Part (Node : Basic_Subp_Decl'Class) return Base_Subp_Body;
   --  Return the BaseSubpBody corresponding to this node.

   function F_Overriding
     (Node : Classic_Subp_Decl'Class) return Overriding_Node;

   function F_Overriding
     (Node : Classic_Subp_Decl'Class) return Ada_Overriding_Node;

   function F_Subp_Spec (Node : Classic_Subp_Decl'Class) return Subp_Spec;

   function F_Default_Expr (Node : Formal_Subp_Decl'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Null_Literal
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Name (Node : Accept_Stmt'Class) return Identifier;

   function F_Entry_Index_Expr (Node : Accept_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Params
     (Node : Accept_Stmt'Class) return Entry_Completion_Formal_Params;

   function F_Stmts (Node : Accept_Stmt_With_Stmts'Class) return Handled_Stmts;

   function F_End_Name (Node : Accept_Stmt_With_Stmts'Class) return End_Name;

   function F_Has_Not_Null (Node : Access_Def'Class) return Not_Null;

   function F_Has_Not_Null (Node : Access_Def'Class) return Boolean;

   function F_Has_Protected
     (Node : Access_To_Subp_Def'Class) return Protected_Node;

   function F_Has_Protected (Node : Access_To_Subp_Def'Class) return Boolean;

   function F_Subp_Spec (Node : Access_To_Subp_Def'Class) return Subp_Spec;

   function Ada_Node_List_First (Node : Ada_Node_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Ada_Node_List_Next
     (Node : Ada_Node_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Ada_Node_List_Has_Element
     (Node : Ada_Node_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Ada_Node_List_Element
     (Node : Ada_Node_List; Cursor : Positive) return Ada_Node'Class;
   --  Implementation detail for the Iterable aspect

   function F_Ancestor_Expr (Node : Base_Aggregate'Class) return Expr;

   function F_Assocs (Node : Base_Aggregate'Class) return Assoc_List;

   function P_Get_Params
     (Node : Basic_Assoc'Class; Imprecise_Fallback : Boolean := False)
      return Defining_Name_Array;
   --  Return the list of parameters that this association refers to.

   function F_Designators
     (Node : Aggregate_Assoc'Class) return Alternatives_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Others_Designator
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_R_Expr (Node : Aggregate_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function P_As_Bool (Node : Aliased_Node'Class) return Boolean;
   --  Return whether this is an instance of AliasedPresent

   function P_As_Bool (Node : All_Node'Class) return Boolean;
   --  Return whether this is an instance of AllPresent

   function F_Subpool (Node : Allocator'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Type_Or_Expr (Node : Allocator'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Qual_Expr
   --
   --  * Subtype_Indication

   function P_Get_Allocated_Type
     (Node : Allocator'Class) return Base_Type_Decl;
   --  Return the allocated type for this allocator.

   function F_Ids (Node : Object_Decl'Class) return Defining_Name_List;

   function F_Has_Aliased (Node : Object_Decl'Class) return Aliased_Node;

   function F_Has_Aliased (Node : Object_Decl'Class) return Boolean;

   function F_Has_Constant (Node : Object_Decl'Class) return Constant_Node;

   function F_Has_Constant (Node : Object_Decl'Class) return Boolean;

   function F_Mode (Node : Object_Decl'Class) return Mode;

   function F_Mode (Node : Object_Decl'Class) return Ada_Mode;

   function F_Type_Expr (Node : Object_Decl'Class) return Type_Expr;

   function F_Default_Expr (Node : Object_Decl'Class) return Expr;

   function F_Renaming_Clause
     (Node : Object_Decl'Class) return Renaming_Clause;

   function P_Public_Part_Decl (Node : Object_Decl'Class) return Basic_Decl;
   --  If this object decl is the constant completion of an object decl in the
   --  public part, return the object decl from the public part.

   function P_Type_Name (Node : Type_Expr'Class) return Name;
   --  Return the name node for this type expression, if applicable, else null

   function P_Designated_Type_Decl
     (Node : Type_Expr'Class) return Base_Type_Decl;
   --  Returns the type declaration designated by this type expression.

   function P_Designated_Type_Decl_From
     (Node : Type_Expr'Class; Origin_Node : Ada_Node'Class)
      return Base_Type_Decl;
   --  Return the type declaration designated by this type expression as viewed
   --  from the node given by origin_node.

   function F_Type_Decl
     (Node : Anonymous_Type'Class) return Anonymous_Type_Decl;

   function F_Type_Decl
     (Node : Anonymous_Type_Access_Def'Class) return Base_Type_Decl;

   function F_Name (Node : Base_Type_Decl'Class) return Defining_Name;

   function P_Base_Subtype
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl;
   --  If this type decl is a subtype decl, return the base subtype. If not,
   --  return ``Self``.

   function P_Private_Completion
     (Node : Base_Type_Decl'Class) return Base_Type_Decl;
   --  Return the private completion for this type, if there is one.

   function P_Get_Record_Representation_Clause
     (Node : Base_Type_Decl'Class; Imprecise_Fallback : Boolean := False)
      return Record_Rep_Clause;
   --  Return the record representation clause associated to this type decl, if
   --  applicable (i.e. this type decl defines a record type).

   function P_Get_Enum_Representation_Clause
     (Node : Base_Type_Decl'Class; Imprecise_Fallback : Boolean := False)
      return Enum_Rep_Clause;
   --  Return the enum representation clause associated to this type decl, if
   --  applicable (i.e. this type decl defines an enum type).

   function P_Is_Record_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Return whether this type is a record type.

   function P_Is_Array_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Return whether this type is an array type.

   function P_Find_Derived_Types
     (Node   : Base_Type_Decl'Class; Root : Ada_Node'Class;
      Origin : Ada_Node'Class; Imprecise_Fallback : Boolean := False)
      return Type_Decl_Array;
   --  Find types derived from self in the given ``root`` and its children.

   function P_Is_Real_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is a real type or not.

   function P_Is_Float_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is a float type or not.

   function P_Is_Fixed_Point
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is a fixed point type or not.

   function P_Is_Enum_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is an enum type

   function P_Is_Access_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether Self is an access type or not

   function P_Is_Char_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is a character type or not

   function P_Discrete_Range
     (Node : Base_Type_Decl'Class) return Discrete_Range;
   --  Return the discrete range for this type decl, if applicable.

   function P_Is_Discrete_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is a discrete type or not.

   function P_Is_Int_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is an integer type or not.

   function P_Accessed_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl;
   --  If this type is an access type, or a type with an Implicit_Dereference
   --  aspect, return the type of a dereference of an instance of this type.

   function P_Is_Tagged_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Whether type is tagged or not

   function P_Base_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl;
   --  Return the base type entity for this derived type declaration

   function P_Base_Types
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl_Array;
   --  Return the list of base types for Self.

   function P_Find_All_Derived_Types
     (Node               : Base_Type_Decl'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Type_Decl_Array;
   --  Return the list of all types that inherit (directly or inderictly) from
   --  Self among the given units.

   function P_Comp_Type
     (Node   : Base_Type_Decl'Class; Is_Subscript : Boolean := False;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl;
   --  Return the component type of `Self`, if applicable. The component type
   --  is the type you'll get if you call a value whose type is `Self`. So it
   --  can either be:
   --
   --  1. The component type for an array. 2. The return type for an access to
   --  function.

   function P_Index_Type
     (Node   : Base_Type_Decl'Class; Dim : Integer;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl;
   --  Return the index type for dimension ``dim`` for this type, if
   --  applicable.

   function P_Is_Derived_Type
     (Node   : Base_Type_Decl'Class; Other_Type : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean;
   --  Whether Self is derived from other_type.

   function P_Is_Interface_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Return True iff this type declaration is an interface definition.

   function P_Matching_Type
     (Node   : Base_Type_Decl'Class; Expected_Type : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean;
   --  Return whether ``self`` matches ``expected_type``.

   function P_Canonical_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl;
   --  Return the canonical type declaration for this type declaration. For
   --  subtypes, it will return the base type declaration.

   function P_Previous_Part
     (Node : Base_Type_Decl'Class; Go_To_Incomplete : Boolean := True)
      return Base_Type_Decl;
   --  Returns the previous part for this type decl.

   function P_Next_Part (Node : Base_Type_Decl'Class) return Base_Type_Decl;
   --  Returns the next part for this type decl.

   function P_Full_View (Node : Base_Type_Decl'Class) return Base_Type_Decl;
   --  Return the full completion of this type.

   function P_Is_Definite_Subtype
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean;
   --  Returns whether this is a definite subtype.
   --
   --  For convenience, this will return ``False`` for incomplete types, even
   --  though the correct answer is more akin to "non applicable".

   function P_Is_Private (Node : Base_Type_Decl'Class) return Boolean;
   --  Whether node is a private view of corresponding type.

   function P_Root_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl;
   --  Return the type that is at the root of the derivation hierarchy
   --  (ignoring secondary interfaces derivations for tagged types)

   function F_Discriminants (Node : Type_Decl'Class) return Discriminant_Part;

   function F_Type_Def (Node : Type_Decl'Class) return Type_Def;

   function P_Get_Primitives
     (Node : Type_Decl'Class; Only_Inherited : Boolean := False)
      return Basic_Decl_Array;
   --  Return the list of all primitive operations that are available on this
   --  type. If `only_inherited` is True, it will only return the primitives
   --  that are implicitly inherited by this type, discarding those explicitly
   --  defined on this type.

   function F_Indices (Node : Array_Type_Def'Class) return Array_Indices;

   function F_Component_Type
     (Node : Array_Type_Def'Class) return Component_Def;

   function F_Id (Node : Aspect_Assoc'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Expr (Node : Aspect_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Contract_Cases
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Aspect_Assoc_List'Class; Index : Positive) return Aspect_Assoc;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Aspect_Assoc_List_First (Node : Aspect_Assoc_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Aspect_Assoc_List_Next
     (Node : Aspect_Assoc_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Aspect_Assoc_List_Has_Element
     (Node : Aspect_Assoc_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Aspect_Assoc_List_Element
     (Node : Aspect_Assoc_List; Cursor : Positive) return Aspect_Assoc'Class;
   --  Implementation detail for the Iterable aspect

   function F_Aspect_Assocs
     (Node : Aspect_Spec'Class) return Aspect_Assoc_List;

   function F_Dest (Node : Assign_Stmt'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Expr (Node : Assign_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Basic_Assoc_List'Class; Index : Positive) return Basic_Assoc;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Basic_Assoc_List_First (Node : Basic_Assoc_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Basic_Assoc_List_Next
     (Node : Basic_Assoc_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Basic_Assoc_List_Has_Element
     (Node : Basic_Assoc_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Basic_Assoc_List_Element
     (Node : Basic_Assoc_List; Cursor : Positive) return Basic_Assoc'Class;
   --  Implementation detail for the Iterable aspect

   function P_Zip_With_Params
     (Node : Assoc_List'Class; Imprecise_Fallback : Boolean := False)
      return Param_Actual_Array;
   --  Returns an array of pairs, associating formal parameters to actual
   --  expressions. The formals to match are retrieved by resolving the
   --  call which this AssocList represents the actuals of.

   function F_Name (Node : At_Clause'Class) return Base_Id;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Expr (Node : At_Clause'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Attribute_Expr (Node : Attribute_Def_Clause'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Expr (Node : Attribute_Def_Clause'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function P_Enclosing_Defining_Name (Node : Name'Class) return Defining_Name;
   --  If this name is part of a defining name, return the enclosing defining
   --  name node.

   function P_Is_Defining (Node : Name'Class) return Boolean;
   --  Return True if this name is part of a defining name.

   function P_Name_Is
     (Node : Name'Class; Sym : Unbounded_Text_Type) return Boolean;
   --  Helper. Check that this name matches ``sym``.

   function P_Is_Direct_Call (Node : Name'Class) return Boolean;
   --  Return True iff this name represents a call to a subprogram which is
   --  referred by its defining name. (i.e. not through a subprogram access).

   function P_Is_Access_Call (Node : Name'Class) return Boolean;
   --  Return True iff this name represents a call to subprogram through an
   --  access type.

   function P_Is_Call (Node : Name'Class) return Boolean;
   --  Returns True if this Name corresponds to a call.

   function P_Is_Dot_Call
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean;
   --  Returns True if this Name corresponds to a dot notation call.

   function P_Failsafe_Referenced_Def_Name
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Refd_Def;
   --  Failsafe version of ``referenced_defining_name``. Returns a ``RefdDef``,
   --  which can be precise, imprecise, or error.

   function P_Referenced_Defining_Name
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Defining_Name;
   --  Like ``referenced_decl``, but will return the defining identifier for
   --  the decl, rather than the basic declaration node itself.

   function P_All_Env_Elements
     (Node     : Name'Class; Seq : Boolean := True;
      Seq_From : Ada_Node'Class := No_Ada_Node) return Ada_Node_Array;
   --  Return all elements in self's scope that are lexically named like Self.

   function P_Called_Subp_Spec
     (Node : Name'Class) return Base_Formal_Param_Holder;
   --  Return the subprogram specification of the subprogram or subprogram
   --  access that is being called by this exact Name, if relevant.

   function P_Referenced_Decl
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Basic_Decl;
   --  Return the declaration this node references after name resolution. If
   --  imprecise_fallback is True, errors raised during resolution of the xref
   --  equation are catched and a fallback mechanism is triggered, which tries
   --  to find the referenced declaration in an ad-hoc way.

   function P_Failsafe_Referenced_Decl
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Refd_Decl;
   --  Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which
   --  can be precise, imprecise, or error.

   function P_Referenced_Decl_Internal
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Refd_Decl;
   --  Return the declaration this node references. Try not to run name res if
   --  already resolved. INTERNAL USE ONLY.

   function P_Name_Designated_Type (Node : Name'Class) return Base_Type_Decl;
   --  Like SubtypeIndication.designated_type, but on names, since because
   --  of Ada's ambiguous grammar, some subtype indications will be parsed
   --  as names.

   function P_Is_Static_Subtype
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean;
   --  Returns whether Self denotes a static subtype or not.

   function P_Name_Matches (Node : Name'Class; N : Name'Class) return Boolean;
   --  Return whether two names match each other.
   --
   --  This compares the symbol for Identifier and StringLiteral nodes. We
   --  consider that there is no match for all other node kinds.

   function P_Relative_Name (Node : Name'Class) return Single_Tok_Node;
   --  Returns the relative name of this instance. For example, for a prefix
   --  A.B.C, this will return C.

   function P_Is_Operator_Name (Node : Name'Class) return Boolean;
   --  Return whether the name that Self designates is an operator.

   function P_Is_Write_Reference
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean;
   --  Whether this name is a write reference.
   --
   --  For example, `X` is a write reference in the following cases::
   --
   --  1. `X := 2;` 2. `X (2) := 2;` 3. `P(F => X)` where F is declared `out`
   --  or `in out`. 4. `X'Access`. 5. `X.C := 2`, `R.X := 2`
   --
   --  .. note:: This is an experimental feature. There might be some
   --     discrepancy with the GNAT concept of "write reference".

   function P_Is_Dispatching_Call
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean;
   --  Returns True if this Name corresponds to a dispatching call, including:
   --
   --  - calls done through subprogram access types.
   --
   --  - calls to dispatching subprograms, in the object-oriented sense.
   --
   --  .. note:: This is an experimental feature. There might be some
   --     discrepancy with the GNAT concept of "dispatching call".

   function P_Is_Static_Call
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean;
   --  Returns True if this Name corresponds to a static non-dispatching call.
   --  In other words, this will return True if and only if the target of the
   --  call is known statically.
   --
   --  .. note:: This is an experimental feature. There might be some
   --     discrepancy with the GNAT concept of "static call".

   function P_As_Symbol_Array
     (Node : Name'Class) return Unbounded_Text_Type_Array;
   --  Turn this name into an array of symbols.
   --
   --  For instance, a node with name ``A.B.C`` is turned into ``['A', 'B',
   --  'C']``.

   function F_Prefix (Node : Attribute_Ref'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Attribute (Node : Attribute_Ref'Class) return Identifier;

   function F_Args (Node : Attribute_Ref'Class) return Ada_Node;

   function P_Assoc_Expr (Node : Base_Assoc'Class) return Expr;
   --  Returns the expression side of this assoc node.

   function List_Child
     (Node : Base_Assoc_List'Class; Index : Positive) return Base_Assoc;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Base_Assoc_List_First (Node : Base_Assoc_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Base_Assoc_List_Next
     (Node : Base_Assoc_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Base_Assoc_List_Has_Element
     (Node : Base_Assoc_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Base_Assoc_List_Element
     (Node : Base_Assoc_List; Cursor : Positive) return Base_Assoc'Class;
   --  Implementation detail for the Iterable aspect

   function P_Formal_Type
     (Node   : Base_Formal_Param_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl;
   --  Return the type for this formal.

   function P_Abstract_Formal_Params
     (Node : Base_Formal_Param_Holder'Class)
      return Base_Formal_Param_Decl_Array;
   --  Return the list of abstract formal parameters for this holder.

   function P_Nb_Min_Params
     (Node : Base_Formal_Param_Holder'Class) return Integer;
   --  Return the minimum number of parameters this subprogram can be called
   --  while still being a legal call.

   function P_Nb_Max_Params
     (Node : Base_Formal_Param_Holder'Class) return Integer;
   --  Return the maximum number of parameters this subprogram can be called
   --  while still being a legal call.

   function P_Param_Types
     (Node   : Base_Formal_Param_Holder'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl_Array;
   --  Returns the type of each parameter of Self.

   function P_Canonical_Text
     (Node : Single_Tok_Node'Class) return Unbounded_Text_Type;
   --  Return a canonicalized version of this node's text.

   function F_Spec (Node : Base_Loop_Stmt'Class) return Loop_Spec;

   function F_Stmts (Node : Base_Loop_Stmt'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_End_Name (Node : Base_Loop_Stmt'Class) return End_Name;

   function F_Package_Name
     (Node : Base_Package_Decl'Class) return Defining_Name;

   function F_Public_Part (Node : Base_Package_Decl'Class) return Public_Part;

   function F_Private_Part
     (Node : Base_Package_Decl'Class) return Private_Part;

   function F_End_Name (Node : Base_Package_Decl'Class) return End_Name;

   function P_Body_Part (Node : Base_Package_Decl'Class) return Package_Body;
   --  Return the PackageBody corresponding to this node.

   function F_Components (Node : Base_Record_Def'Class) return Component_List;

   function P_Previous_Part (Node : Body_Node'Class) return Basic_Decl;
   --  Return the previous part for this body. Might be a declaration or a body
   --  stub.

   function P_Decl_Part (Node : Body_Node'Class) return Basic_Decl;
   --  Return the decl corresponding to this node if applicable.

   function P_Subunit_Root (Node : Body_Node'Class) return Basic_Decl;
   --  If self is a subunit, return the body in which it is rooted.

   function F_Overriding (Node : Base_Subp_Body'Class) return Overriding_Node;

   function F_Overriding
     (Node : Base_Subp_Body'Class) return Ada_Overriding_Node;

   function F_Subp_Spec (Node : Base_Subp_Body'Class) return Subp_Spec;

   function P_Returns (Node : Base_Subp_Spec'Class) return Type_Expr;
   --  Syntax property. Return the type expression node corresponding to the
   --  return of this subprogram spec.

   function P_Params (Node : Base_Subp_Spec'Class) return Param_Spec_Array;
   --  Returns the array of parameters specification for this subprogram spec.

   function P_Primitive_Subp_Types
     (Node : Base_Subp_Spec'Class) return Base_Type_Decl_Array;
   --  Return the types of which this subprogram is a primitive of.

   function P_Primitive_Subp_First_Type
     (Node : Base_Subp_Spec'Class) return Base_Type_Decl;
   --  Return the first type of which this subprogram is a primitive of.

   function P_Primitive_Subp_Tagged_Type
     (Node : Base_Subp_Spec'Class) return Base_Type_Decl;
   --  If this subprogram is a primitive for a tagged type, then return this
   --  type.

   function P_Return_Type
     (Node : Base_Subp_Spec'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl;
   --  Returns the return type of Self, if applicable (e.g. if Self is a
   --  subprogram). Else, returns null.

   function F_Stmts (Node : Begin_Block'Class) return Handled_Stmts;

   function F_End_Name (Node : Begin_Block'Class) return End_Name;

   function F_Left (Node : Bin_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Op (Node : Bin_Op'Class) return Op;
   --  This field can contain one of the following nodes:
   --
   --  * Op_And
   --
   --  * Op_And_Then
   --
   --  * Op_Concat
   --
   --  * Op_Div
   --
   --  * Op_Double_Dot
   --
   --  * Op_Eq
   --
   --  * Op_Gt
   --
   --  * Op_Gte
   --
   --  * Op_Lt
   --
   --  * Op_Lte
   --
   --  * Op_Minus
   --
   --  * Op_Mod
   --
   --  * Op_Mult
   --
   --  * Op_Neq
   --
   --  * Op_Or
   --
   --  * Op_Or_Else
   --
   --  * Op_Plus
   --
   --  * Op_Pow
   --
   --  * Op_Rem
   --
   --  * Op_Xor

   function F_Op (Node : Bin_Op'Class) return Ada_Op;

   function F_Right (Node : Bin_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Name (Node : Call_Expr'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Suffix (Node : Call_Expr'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Basic_Assoc_List
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function P_Is_Array_Slice (Node : Call_Expr'Class) return Boolean;
   --  Return whether this CallExpr is actually an access to a slice of the
   --  array denoted by the prefix of this CallExpr.

   function F_Call (Node : Call_Stmt'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Expr (Node : Case_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Cases (Node : Case_Expr'Class) return Case_Expr_Alternative_List;

   function F_Choices
     (Node : Case_Expr_Alternative'Class) return Alternatives_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Others_Designator
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Expr (Node : Case_Expr_Alternative'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Case_Expr_Alternative_List'Class; Index : Positive)
      return Case_Expr_Alternative;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Case_Expr_Alternative_List_First
     (Node : Case_Expr_Alternative_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Case_Expr_Alternative_List_Next
     (Node : Case_Expr_Alternative_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Case_Expr_Alternative_List_Has_Element
     (Node : Case_Expr_Alternative_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Case_Expr_Alternative_List_Element
     (Node : Case_Expr_Alternative_List; Cursor : Positive)
      return Case_Expr_Alternative'Class;
   --  Implementation detail for the Iterable aspect

   function F_Expr (Node : Case_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Alternatives
     (Node : Case_Stmt'Class) return Case_Stmt_Alternative_List;

   function F_Choices
     (Node : Case_Stmt_Alternative'Class) return Alternatives_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Others_Designator
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Stmts (Node : Case_Stmt_Alternative'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function List_Child
     (Node : Case_Stmt_Alternative_List'Class; Index : Positive)
      return Case_Stmt_Alternative;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Case_Stmt_Alternative_List_First
     (Node : Case_Stmt_Alternative_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Case_Stmt_Alternative_List_Next
     (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Case_Stmt_Alternative_List_Has_Element
     (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Case_Stmt_Alternative_List_Element
     (Node : Case_Stmt_Alternative_List; Cursor : Positive)
      return Case_Stmt_Alternative'Class;
   --  Implementation detail for the Iterable aspect

   function P_Denoted_Value (Node : Char_Literal'Class) return Character_Type;
   --  Return the value that this literal denotes.

   function F_Prelude (Node : Compilation_Unit'Class) return Ada_Node_List;
   --  ``with``, ``use`` or ``pragma`` statements.
   --
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Use_Clause
   --
   --  * With_Clause

   function F_Body (Node : Compilation_Unit'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Library_Item
   --
   --  * Subunit

   function F_Pragmas (Node : Compilation_Unit'Class) return Pragma_Node_List;

   function P_Syntactic_Fully_Qualified_Name
     (Node : Compilation_Unit'Class) return Unbounded_Text_Type_Array;
   --  Return the syntactic fully qualified name of this compilation unit.

   function P_Unit_Kind
     (Node : Compilation_Unit'Class) return Analysis_Unit_Kind;
   --  Return the kind corresponding to this analysis unit.

   function P_Withed_Units
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array;
   --  Look for all "with" clauses at the top of this compilation unit and
   --  return all the compilation units designated by them.

   function P_Imported_Units
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array;
   --  Return all the compilation units that are directly imported by this one.
   --  This includes "with"ed units as well as the direct parent unit.

   function P_Unit_Dependencies
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array;
   --  Return the list of all the compilation units that are (direct and
   --  indirect) dependencies of this one.

   function P_Decl (Node : Compilation_Unit'Class) return Basic_Decl;
   --  Get the root basic decl defined in this compilation unit.

   function P_Is_Preelaborable (Node : Compilation_Unit'Class) return Boolean;
   --  Whether this compilation unit is preelaborable or not.

   function List_Child
     (Node : Compilation_Unit_List'Class; Index : Positive)
      return Compilation_Unit;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Compilation_Unit_List_First
     (Node : Compilation_Unit_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Compilation_Unit_List_Next
     (Node : Compilation_Unit_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Compilation_Unit_List_Has_Element
     (Node : Compilation_Unit_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Compilation_Unit_List_Element
     (Node : Compilation_Unit_List; Cursor : Positive)
      return Compilation_Unit'Class;
   --  Implementation detail for the Iterable aspect

   function F_Id (Node : Component_Clause'Class) return Identifier;

   function F_Position (Node : Component_Clause'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Range (Node : Component_Clause'Class) return Range_Spec;

   function F_Ids (Node : Component_Decl'Class) return Defining_Name_List;

   function F_Component_Def (Node : Component_Decl'Class) return Component_Def;

   function F_Default_Expr (Node : Component_Decl'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Has_Aliased (Node : Component_Def'Class) return Aliased_Node;

   function F_Has_Aliased (Node : Component_Def'Class) return Boolean;

   function F_Has_Constant (Node : Component_Def'Class) return Constant_Node;

   function F_Has_Constant (Node : Component_Def'Class) return Boolean;

   function F_Type_Expr (Node : Component_Def'Class) return Type_Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Anonymous_Type
   --
   --  * Subtype_Indication

   function F_Components (Node : Component_List'Class) return Ada_Node_List;

   function F_Variant_Part (Node : Component_List'Class) return Variant_Part;

   function P_As_Bool (Node : Constant_Node'Class) return Boolean;
   --  Return whether this is an instance of ConstantPresent

   function F_List
     (Node : Constrained_Array_Indices'Class) return Constraint_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Subtype_Indication
   --
   --  * Target_Name

   function F_Has_Not_Null (Node : Subtype_Indication'Class) return Not_Null;

   function F_Has_Not_Null (Node : Subtype_Indication'Class) return Boolean;

   function F_Name (Node : Subtype_Indication'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Constraint (Node : Subtype_Indication'Class) return Constraint;

   function P_Is_Static_Subtype
     (Node : Subtype_Indication'Class; Imprecise_Fallback : Boolean := False)
      return Boolean;
   --  Returns whether Self denotes a static subtype or not.

   function F_Guard (Node : Contract_Case_Assoc'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Others_Designator
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Consequence (Node : Contract_Case_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Contract_Case_Assoc_List'Class; Index : Positive)
      return Contract_Case_Assoc;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Contract_Case_Assoc_List_First
     (Node : Contract_Case_Assoc_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Contract_Case_Assoc_List_Next
     (Node : Contract_Case_Assoc_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Contract_Case_Assoc_List_Has_Element
     (Node : Contract_Case_Assoc_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Contract_Case_Assoc_List_Element
     (Node : Contract_Case_Assoc_List; Cursor : Positive)
      return Contract_Case_Assoc'Class;
   --  Implementation detail for the Iterable aspect

   function F_Contract_Cases
     (Node : Contract_Cases'Class) return Contract_Case_Assoc_List;

   function F_Delta (Node : Decimal_Fixed_Point_Def'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Digits (Node : Decimal_Fixed_Point_Def'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Range (Node : Decimal_Fixed_Point_Def'Class) return Range_Spec;

   function F_Decls (Node : Decl_Block'Class) return Declarative_Part;

   function F_Stmts (Node : Decl_Block'Class) return Handled_Stmts;

   function F_End_Name (Node : Decl_Block'Class) return End_Name;

   function F_Decls (Node : Declarative_Part'Class) return Ada_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Abstract_Subp_Decl
   --
   --  * Aspect_Clause
   --
   --  * Body_Node
   --
   --  * Component_Decl
   --
   --  * Entry_Decl
   --
   --  * Error_Decl
   --
   --  * Exception_Decl
   --
   --  * Generic_Decl
   --
   --  * Generic_Instantiation
   --
   --  * Generic_Renaming_Decl
   --
   --  * Incomplete_Type_Decl
   --
   --  * Number_Decl
   --
   --  * Object_Decl
   --
   --  * Package_Decl
   --
   --  * Package_Renaming_Decl
   --
   --  * Pragma_Node
   --
   --  * Protected_Type_Decl
   --
   --  * Single_Protected_Decl
   --
   --  * Single_Task_Decl
   --
   --  * Subp_Decl
   --
   --  * Subtype_Decl
   --
   --  * Task_Type_Decl
   --
   --  * Type_Decl
   --
   --  * Use_Clause

   function F_Name (Node : Defining_Name'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function P_Basic_Decl (Node : Defining_Name'Class) return Basic_Decl;
   --  Returns this DefiningName's basic declaration

   function P_Find_Refs
     (Node   : Defining_Name'Class; Root : Ada_Node'Class;
      Origin : Ada_Node'Class; Imprecise_Fallback : Boolean := False)
      return Ref_Result_Array;
   --  Find all references to this defining name in the given ``root`` and its
   --  children.

   function P_Find_All_References
     (Node               : Defining_Name'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array;
   --  Searches all references to this defining name in the given list of
   --  units.

   function P_Find_All_Calls
     (Node               : Defining_Name'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array;
   --  Return the list of all possible calls to the subprogram which Self is
   --  the defining name of.
   --
   --  This will return the name corresponding to the call, excluding the
   --  parameters if there are any. For instance, it will return `A` for the
   --  `A (B)` call.
   --
   --  .. note:: This does not yet support calls done inside generics.

   function P_Next_Part (Node : Defining_Name'Class) return Defining_Name;
   --  Like ``BasicDecl.next_part_for_decl`` on a defining name

   function P_Previous_Part (Node : Defining_Name'Class) return Defining_Name;
   --  Like ``BasicDecl.previous_part_for_decl`` on a defining name

   function P_Canonical_Part (Node : Defining_Name'Class) return Defining_Name;
   --  Like ``BasicDecl.canonical_part`` on a defining name

   function List_Child
     (Node : Defining_Name_List'Class; Index : Positive) return Defining_Name;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Defining_Name_List_First
     (Node : Defining_Name_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Defining_Name_List_Next
     (Node : Defining_Name_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Defining_Name_List_Has_Element
     (Node : Defining_Name_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Defining_Name_List_Element
     (Node : Defining_Name_List; Cursor : Positive) return Defining_Name'Class;
   --  Implementation detail for the Iterable aspect

   function F_Has_Until (Node : Delay_Stmt'Class) return Until_Node;

   function F_Has_Until (Node : Delay_Stmt'Class) return Boolean;

   function F_Expr (Node : Delay_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Digits (Node : Delta_Constraint'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Range (Node : Delta_Constraint'Class) return Range_Spec;

   function F_Has_Abstract
     (Node : Derived_Type_Def'Class) return Abstract_Node;

   function F_Has_Abstract (Node : Derived_Type_Def'Class) return Boolean;

   function F_Has_Limited (Node : Derived_Type_Def'Class) return Limited_Node;

   function F_Has_Limited (Node : Derived_Type_Def'Class) return Boolean;

   function F_Has_Synchronized
     (Node : Derived_Type_Def'Class) return Synchronized_Node;

   function F_Has_Synchronized (Node : Derived_Type_Def'Class) return Boolean;

   function F_Subtype_Indication
     (Node : Derived_Type_Def'Class) return Subtype_Indication;

   function F_Interfaces (Node : Derived_Type_Def'Class) return Parent_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Record_Extension
     (Node : Derived_Type_Def'Class) return Base_Record_Def;

   function F_Has_With_Private
     (Node : Derived_Type_Def'Class) return With_Private;

   function F_Has_With_Private (Node : Derived_Type_Def'Class) return Boolean;

   function F_Digits (Node : Digits_Constraint'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Range (Node : Digits_Constraint'Class) return Range_Spec;

   function F_Subtype
     (Node : Discrete_Subtype_Name'Class) return Discrete_Subtype_Indication;

   function F_Ids
     (Node : Discriminant_Assoc'Class) return Discriminant_Choice_List;

   function F_Discr_Expr (Node : Discriminant_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Identifier_List'Class; Index : Positive) return Identifier;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Identifier_List_First (Node : Identifier_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Identifier_List_Next
     (Node : Identifier_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Identifier_List_Has_Element
     (Node : Identifier_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Identifier_List_Element
     (Node : Identifier_List; Cursor : Positive) return Identifier'Class;
   --  Implementation detail for the Iterable aspect

   function F_Constraints
     (Node : Discriminant_Constraint'Class) return Assoc_List;

   function F_Ids (Node : Discriminant_Spec'Class) return Defining_Name_List;

   function F_Type_Expr (Node : Discriminant_Spec'Class) return Type_Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Anonymous_Type
   --
   --  * Subtype_Indication

   function F_Default_Expr (Node : Discriminant_Spec'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Discriminant_Spec_List'Class; Index : Positive)
      return Discriminant_Spec;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Discriminant_Spec_List_First
     (Node : Discriminant_Spec_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Discriminant_Spec_List_Next
     (Node : Discriminant_Spec_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Discriminant_Spec_List_Has_Element
     (Node : Discriminant_Spec_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Discriminant_Spec_List_Element
     (Node : Discriminant_Spec_List; Cursor : Positive)
      return Discriminant_Spec'Class;
   --  Implementation detail for the Iterable aspect

   function F_Prefix (Node : Dotted_Name'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Suffix (Node : Dotted_Name'Class) return Base_Id;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Cond_Expr (Node : Elsif_Expr_Part'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Then_Expr (Node : Elsif_Expr_Part'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Elsif_Expr_Part_List'Class; Index : Positive)
      return Elsif_Expr_Part;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Elsif_Expr_Part_List_First
     (Node : Elsif_Expr_Part_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Elsif_Expr_Part_List_Next
     (Node : Elsif_Expr_Part_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Elsif_Expr_Part_List_Has_Element
     (Node : Elsif_Expr_Part_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Elsif_Expr_Part_List_Element
     (Node : Elsif_Expr_Part_List; Cursor : Positive)
      return Elsif_Expr_Part'Class;
   --  Implementation detail for the Iterable aspect

   function F_Cond_Expr (Node : Elsif_Stmt_Part'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Stmts (Node : Elsif_Stmt_Part'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function List_Child
     (Node : Elsif_Stmt_Part_List'Class; Index : Positive)
      return Elsif_Stmt_Part;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Elsif_Stmt_Part_List_First
     (Node : Elsif_Stmt_Part_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Elsif_Stmt_Part_List_Next
     (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Elsif_Stmt_Part_List_Has_Element
     (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Elsif_Stmt_Part_List_Element
     (Node : Elsif_Stmt_Part_List; Cursor : Positive)
      return Elsif_Stmt_Part'Class;
   --  Implementation detail for the Iterable aspect

   function F_Name (Node : End_Name'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function P_Basic_Decl (Node : End_Name'Class) return Basic_Decl;
   --  Returns this EndName's basic declaration

   function F_Entry_Name (Node : Entry_Body'Class) return Defining_Name;

   function F_Index_Spec (Node : Entry_Body'Class) return Entry_Index_Spec;

   function F_Params
     (Node : Entry_Body'Class) return Entry_Completion_Formal_Params;

   function F_Barrier (Node : Entry_Body'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Decls (Node : Entry_Body'Class) return Declarative_Part;

   function F_Stmts (Node : Entry_Body'Class) return Handled_Stmts;

   function F_End_Name (Node : Entry_Body'Class) return End_Name;

   function F_Params
     (Node : Entry_Completion_Formal_Params'Class) return Params;

   function F_Overriding (Node : Entry_Decl'Class) return Overriding_Node;

   function F_Overriding (Node : Entry_Decl'Class) return Ada_Overriding_Node;

   function F_Spec (Node : Entry_Decl'Class) return Entry_Spec;

   function F_Id (Node : Entry_Index_Spec'Class) return Defining_Name;

   function F_Subtype (Node : Entry_Index_Spec'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Subtype_Indication
   --
   --  * Target_Name

   function F_Entry_Name (Node : Entry_Spec'Class) return Defining_Name;

   function F_Family_Type (Node : Entry_Spec'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Subtype_Indication
   --
   --  * Target_Name

   function F_Entry_Params (Node : Entry_Spec'Class) return Params;

   function F_Name (Node : Enum_Literal_Decl'Class) return Defining_Name;

   function P_Enum_Type (Node : Enum_Literal_Decl'Class) return Type_Decl;
   --  Return the enum type corresponding to this enum literal.

   function List_Child
     (Node : Enum_Literal_Decl_List'Class; Index : Positive)
      return Enum_Literal_Decl;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Enum_Literal_Decl_List_First
     (Node : Enum_Literal_Decl_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Enum_Literal_Decl_List_Next
     (Node : Enum_Literal_Decl_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Enum_Literal_Decl_List_Has_Element
     (Node : Enum_Literal_Decl_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Enum_Literal_Decl_List_Element
     (Node : Enum_Literal_Decl_List; Cursor : Positive)
      return Enum_Literal_Decl'Class;
   --  Implementation detail for the Iterable aspect

   function F_Type_Name (Node : Enum_Rep_Clause'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Aggregate (Node : Enum_Rep_Clause'Class) return Base_Aggregate;

   function F_Enum_Literals
     (Node : Enum_Type_Def'Class) return Enum_Literal_Decl_List;

   function F_Ids (Node : Exception_Decl'Class) return Defining_Name_List;

   function F_Renames (Node : Exception_Decl'Class) return Renaming_Clause;

   function F_Exception_Name
     (Node : Exception_Handler'Class) return Defining_Name;

   function F_Handled_Exceptions
     (Node : Exception_Handler'Class) return Alternatives_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Others_Designator
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Stmts (Node : Exception_Handler'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_Loop_Name (Node : Exit_Stmt'Class) return Identifier;

   function F_Cond_Expr (Node : Exit_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Prefix (Node : Explicit_Deref'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function List_Child (Node : Expr_List'Class; Index : Positive) return Expr;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Expr_List_First (Node : Expr_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Expr_List_Next
     (Node : Expr_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Expr_List_Has_Element
     (Node : Expr_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Expr_List_Element
     (Node : Expr_List; Cursor : Positive) return Expr'Class;
   --  Implementation detail for the Iterable aspect

   function F_Expr (Node : Expr_Function'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Base_Aggregate
   --
   --  * Paren_Expr

   function F_Decl
     (Node : Extended_Return_Stmt'Class)
      return Extended_Return_Stmt_Object_Decl;

   function F_Stmts (Node : Extended_Return_Stmt'Class) return Handled_Stmts;

   function F_Num_Digits (Node : Floating_Point_Def'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Range (Node : Floating_Point_Def'Class) return Range_Spec;

   function F_Var_Decl (Node : For_Loop_Spec'Class) return For_Loop_Var_Decl;

   function F_Loop_Type (Node : For_Loop_Spec'Class) return Iter_Type;

   function F_Loop_Type (Node : For_Loop_Spec'Class) return Ada_Iter_Type;

   function F_Has_Reverse (Node : For_Loop_Spec'Class) return Reverse_Node;

   function F_Has_Reverse (Node : For_Loop_Spec'Class) return Boolean;

   function F_Iter_Expr (Node : For_Loop_Spec'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Id (Node : For_Loop_Var_Decl'Class) return Defining_Name;

   function F_Id_Type
     (Node : For_Loop_Var_Decl'Class) return Subtype_Indication;

   function F_Formal_Part
     (Node : Generic_Decl'Class) return Generic_Formal_Part;

   function F_Decl (Node : Generic_Formal'Class) return Basic_Decl;
   --  This field can contain one of the following nodes:
   --
   --  * Formal_Subp_Decl
   --
   --  * Generic_Instantiation
   --
   --  * Incomplete_Type_Decl
   --
   --  * Number_Decl
   --
   --  * Object_Decl
   --
   --  * Single_Protected_Decl
   --
   --  * Single_Task_Decl
   --
   --  * Type_Decl

   function F_Decls (Node : Generic_Formal_Part'Class) return Ada_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Generic_Formal
   --
   --  * Pragma_Node
   --
   --  * Use_Clause

   function P_Designated_Generic_Decl
     (Node : Generic_Instantiation'Class) return Basic_Decl;
   --  Return the generic decl entity designated by this instantiation,
   --  containing the generic context. This is equivalent to the expanded
   --  generic unit in GNAT.

   function F_Package_Decl
     (Node : Generic_Package_Decl'Class) return Generic_Package_Internal;

   function P_Body_Part
     (Node : Generic_Package_Decl'Class) return Package_Body;
   --  Return the PackageBody corresponding to this node, or null if there is
   --  none.

   function F_Name
     (Node : Generic_Package_Instantiation'Class) return Defining_Name;

   function F_Generic_Pkg_Name
     (Node : Generic_Package_Instantiation'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Params
     (Node : Generic_Package_Instantiation'Class) return Assoc_List;

   function F_Name
     (Node : Generic_Package_Renaming_Decl'Class) return Defining_Name;

   function F_Renames (Node : Generic_Package_Renaming_Decl'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Subp_Decl
     (Node : Generic_Subp_Decl'Class) return Generic_Subp_Internal;

   function P_Body_Part (Node : Generic_Subp_Decl'Class) return Base_Subp_Body;
   --  Return the BaseSubpBody corresponding to this node.

   function F_Overriding
     (Node : Generic_Subp_Instantiation'Class) return Overriding_Node;

   function F_Overriding
     (Node : Generic_Subp_Instantiation'Class) return Ada_Overriding_Node;

   function F_Kind (Node : Generic_Subp_Instantiation'Class) return Subp_Kind;

   function F_Kind
     (Node : Generic_Subp_Instantiation'Class) return Ada_Subp_Kind;

   function F_Subp_Name
     (Node : Generic_Subp_Instantiation'Class) return Defining_Name;

   function F_Generic_Subp_Name
     (Node : Generic_Subp_Instantiation'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Params
     (Node : Generic_Subp_Instantiation'Class) return Assoc_List;

   function P_Designated_Subp
     (Node : Generic_Subp_Instantiation'Class) return Ada_Node;
   --  Return the subprogram decl designated by this instantiation.

   function F_Subp_Spec (Node : Generic_Subp_Internal'Class) return Subp_Spec;

   function F_Kind (Node : Generic_Subp_Renaming_Decl'Class) return Subp_Kind;

   function F_Kind
     (Node : Generic_Subp_Renaming_Decl'Class) return Ada_Subp_Kind;

   function F_Name
     (Node : Generic_Subp_Renaming_Decl'Class) return Defining_Name;

   function F_Renames (Node : Generic_Subp_Renaming_Decl'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Label_Name (Node : Goto_Stmt'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Stmts (Node : Handled_Stmts'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_Exceptions (Node : Handled_Stmts'Class) return Ada_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Exception_Handler
   --
   --  * Pragma_Node

   function F_Cond_Expr (Node : If_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Then_Expr (Node : If_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Alternatives (Node : If_Expr'Class) return Elsif_Expr_Part_List;

   function F_Else_Expr (Node : If_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Cond_Expr (Node : If_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Then_Stmts (Node : If_Stmt'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_Alternatives (Node : If_Stmt'Class) return Elsif_Stmt_Part_List;

   function F_Else_Stmts (Node : If_Stmt'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_Discriminants
     (Node : Incomplete_Type_Decl'Class) return Discriminant_Part;

   function F_Has_Abstract
     (Node : Incomplete_Tagged_Type_Decl'Class) return Abstract_Node;

   function F_Has_Abstract
     (Node : Incomplete_Tagged_Type_Decl'Class) return Boolean;

   function F_Constraints
     (Node : Index_Constraint'Class) return Constraint_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Subtype_Indication
   --
   --  * Target_Name

   function P_Denoted_Value (Node : Int_Literal'Class) return Big_Integer;
   --  Return the value that this literal denotes.

   function F_Interface_Kind
     (Node : Interface_Type_Def'Class) return Interface_Kind;

   function F_Interface_Kind
     (Node : Interface_Type_Def'Class) return Ada_Interface_Kind;

   function F_Interfaces (Node : Interface_Type_Def'Class) return Parent_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Discr_Specs
     (Node : Known_Discriminant_Part'Class) return Discriminant_Spec_List;

   function F_Decl (Node : Label'Class) return Label_Decl;

   function F_Name (Node : Label_Decl'Class) return Defining_Name;

   function F_Has_Private (Node : Library_Item'Class) return Private_Node;

   function F_Has_Private (Node : Library_Item'Class) return Boolean;

   function F_Item (Node : Library_Item'Class) return Basic_Decl;
   --  This field can contain one of the following nodes:
   --
   --  * Abstract_Subp_Decl
   --
   --  * Base_Subp_Body
   --
   --  * Error_Decl
   --
   --  * Generic_Decl
   --
   --  * Generic_Instantiation
   --
   --  * Generic_Renaming_Decl
   --
   --  * Package_Body
   --
   --  * Package_Decl
   --
   --  * Package_Renaming_Decl
   --
   --  * Subp_Decl

   function P_As_Bool (Node : Limited_Node'Class) return Boolean;
   --  Return whether this is an instance of LimitedPresent

   function F_Expr (Node : Membership_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Op (Node : Membership_Expr'Class) return Op;
   --  This field can contain one of the following nodes:
   --
   --  * Op_In
   --
   --  * Op_Not_In

   function F_Op (Node : Membership_Expr'Class) return Ada_Op;

   function F_Membership_Exprs
     (Node : Membership_Expr'Class) return Expr_Alternatives_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Name
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Expr (Node : Mod_Int_Type_Def'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child (Node : Name_List'Class; Index : Positive) return Name;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Name_List_First (Node : Name_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Name_List_Next
     (Node : Name_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Name_List_Has_Element
     (Node : Name_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Name_List_Element
     (Node : Name_List; Cursor : Positive) return Name'Class;
   --  Implementation detail for the Iterable aspect

   function F_Decl (Node : Named_Stmt'Class) return Named_Stmt_Decl;

   function F_Stmt (Node : Named_Stmt'Class) return Composite_Stmt;
   --  This field can contain one of the following nodes:
   --
   --  * Base_Loop_Stmt
   --
   --  * Block_Stmt

   function F_Name (Node : Named_Stmt_Decl'Class) return Defining_Name;

   function P_As_Bool (Node : Not_Null'Class) return Boolean;
   --  Return whether this is an instance of NotNullPresent

   function F_Ids (Node : Number_Decl'Class) return Defining_Name_List;

   function F_Expr (Node : Number_Decl'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Delta (Node : Ordinary_Fixed_Point_Def'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Range (Node : Ordinary_Fixed_Point_Def'Class) return Range_Spec;

   function F_Package_Name (Node : Package_Body'Class) return Defining_Name;

   function F_Decls (Node : Package_Body'Class) return Declarative_Part;

   function F_Stmts (Node : Package_Body'Class) return Handled_Stmts;

   function F_End_Name (Node : Package_Body'Class) return End_Name;

   function F_Name (Node : Package_Body_Stub'Class) return Defining_Name;

   function F_Name (Node : Package_Renaming_Decl'Class) return Defining_Name;

   function F_Renames
     (Node : Package_Renaming_Decl'Class) return Renaming_Clause;

   function P_Renamed_Package
     (Node : Package_Renaming_Decl'Class) return Basic_Decl;
   --  Return the declaration of the package that is renamed by Self.

   function P_Final_Renamed_Package
     (Node : Package_Renaming_Decl'Class) return Basic_Decl;
   --  Return the declaration of the package that is ultimately renamed by
   --  Self, skipping through all intermediate package renamings.

   function F_Designator (Node : Param_Assoc'Class) return Ada_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Identifier
   --
   --  * Others_Designator
   --
   --  * String_Literal

   function F_R_Expr (Node : Param_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Ids (Node : Param_Spec'Class) return Defining_Name_List;

   function F_Has_Aliased (Node : Param_Spec'Class) return Aliased_Node;

   function F_Has_Aliased (Node : Param_Spec'Class) return Boolean;

   function F_Mode (Node : Param_Spec'Class) return Mode;

   function F_Mode (Node : Param_Spec'Class) return Ada_Mode;

   function F_Type_Expr (Node : Param_Spec'Class) return Type_Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Anonymous_Type
   --
   --  * Subtype_Indication

   function F_Default_Expr (Node : Param_Spec'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function List_Child
     (Node : Param_Spec_List'Class; Index : Positive) return Param_Spec;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Param_Spec_List_First (Node : Param_Spec_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Param_Spec_List_Next
     (Node : Param_Spec_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Param_Spec_List_Has_Element
     (Node : Param_Spec_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Param_Spec_List_Element
     (Node : Param_Spec_List; Cursor : Positive) return Param_Spec'Class;
   --  Implementation detail for the Iterable aspect

   function F_Params (Node : Params'Class) return Param_Spec_List;

   function F_Expr (Node : Paren_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Id (Node : Pragma_Argument_Assoc'Class) return Identifier;

   function F_Expr (Node : Pragma_Argument_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Id (Node : Pragma_Node'Class) return Identifier;

   function F_Args (Node : Pragma_Node'Class) return Base_Assoc_List;

   function P_Associated_Decls
     (Node : Pragma_Node'Class) return Basic_Decl_Array;
   --  Return an array of ``BasicDecl`` instances associated with this pragma,
   --  or an empty array if non applicable.

   function List_Child
     (Node : Pragma_Node_List'Class; Index : Positive) return Pragma_Node;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Pragma_Node_List_First (Node : Pragma_Node_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Pragma_Node_List_Next
     (Node : Pragma_Node_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Pragma_Node_List_Has_Element
     (Node : Pragma_Node_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Pragma_Node_List_Element
     (Node : Pragma_Node_List; Cursor : Positive) return Pragma_Node'Class;
   --  Implementation detail for the Iterable aspect

   function P_As_Bool (Node : Private_Node'Class) return Boolean;
   --  Return whether this is an instance of PrivatePresent

   function F_Has_Abstract
     (Node : Private_Type_Def'Class) return Abstract_Node;

   function F_Has_Abstract (Node : Private_Type_Def'Class) return Boolean;

   function F_Has_Tagged (Node : Private_Type_Def'Class) return Tagged_Node;

   function F_Has_Tagged (Node : Private_Type_Def'Class) return Boolean;

   function F_Has_Limited (Node : Private_Type_Def'Class) return Limited_Node;

   function F_Has_Limited (Node : Private_Type_Def'Class) return Boolean;

   function P_As_Bool (Node : Protected_Node'Class) return Boolean;
   --  Return whether this is an instance of ProtectedPresent

   function F_Name (Node : Protected_Body'Class) return Defining_Name;

   function F_Decls (Node : Protected_Body'Class) return Declarative_Part;

   function F_End_Name (Node : Protected_Body'Class) return End_Name;

   function F_Name (Node : Protected_Body_Stub'Class) return Defining_Name;

   function F_Public_Part (Node : Protected_Def'Class) return Public_Part;

   function F_Private_Part (Node : Protected_Def'Class) return Private_Part;

   function F_End_Name (Node : Protected_Def'Class) return End_Name;

   function F_Discriminants
     (Node : Protected_Type_Decl'Class) return Discriminant_Part;

   function F_Interfaces (Node : Protected_Type_Decl'Class) return Parent_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Definition
     (Node : Protected_Type_Decl'Class) return Protected_Def;

   function F_Prefix (Node : Qual_Expr'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Suffix (Node : Qual_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Base_Aggregate
   --
   --  * Paren_Expr

   function F_Quantifier (Node : Quantified_Expr'Class) return Quantifier;

   function F_Quantifier (Node : Quantified_Expr'Class) return Ada_Quantifier;

   function F_Loop_Spec (Node : Quantified_Expr'Class) return For_Loop_Spec;

   function F_Expr (Node : Quantified_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Exception_Name (Node : Raise_Expr'Class) return Name;

   function F_Error_Message (Node : Raise_Expr'Class) return Expr;

   function F_Exception_Name (Node : Raise_Stmt'Class) return Name;

   function F_Error_Message (Node : Raise_Stmt'Class) return Expr;

   function F_Range (Node : Range_Constraint'Class) return Range_Spec;

   function F_Range (Node : Range_Spec'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Bin_Op
   --
   --  * Box_Expr
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Name (Node : Record_Rep_Clause'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_At_Expr (Node : Record_Rep_Clause'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Components (Node : Record_Rep_Clause'Class) return Ada_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Component_Clause
   --
   --  * Pragma_Node

   function F_Has_Abstract (Node : Record_Type_Def'Class) return Abstract_Node;

   function F_Has_Abstract (Node : Record_Type_Def'Class) return Boolean;

   function F_Has_Tagged (Node : Record_Type_Def'Class) return Tagged_Node;

   function F_Has_Tagged (Node : Record_Type_Def'Class) return Boolean;

   function F_Has_Limited (Node : Record_Type_Def'Class) return Limited_Node;

   function F_Has_Limited (Node : Record_Type_Def'Class) return Boolean;

   function F_Record_Def (Node : Record_Type_Def'Class) return Base_Record_Def;

   function F_Renamed_Object (Node : Renaming_Clause'Class) return Name;

   function F_Call_Name (Node : Requeue_Stmt'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Has_Abort (Node : Requeue_Stmt'Class) return Abort_Node;

   function F_Has_Abort (Node : Requeue_Stmt'Class) return Boolean;

   function F_Return_Expr (Node : Return_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function P_As_Bool (Node : Reverse_Node'Class) return Boolean;
   --  Return whether this is an instance of ReversePresent

   function F_Guards (Node : Select_Stmt'Class) return Select_When_Part_List;

   function F_Else_Stmts (Node : Select_Stmt'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_Abort_Stmts (Node : Select_Stmt'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function F_Cond_Expr (Node : Select_When_Part'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Stmts (Node : Select_When_Part'Class) return Stmt_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Pragma_Node
   --
   --  * Stmt

   function List_Child
     (Node : Select_When_Part_List'Class; Index : Positive)
      return Select_When_Part;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Select_When_Part_List_First
     (Node : Select_When_Part_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Select_When_Part_List_Next
     (Node : Select_When_Part_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Select_When_Part_List_Has_Element
     (Node : Select_When_Part_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Select_When_Part_List_Element
     (Node : Select_When_Part_List; Cursor : Positive)
      return Select_When_Part'Class;
   --  Implementation detail for the Iterable aspect

   function F_Range (Node : Signed_Int_Type_Def'Class) return Range_Spec;

   function F_Name (Node : Single_Protected_Decl'Class) return Defining_Name;

   function F_Interfaces
     (Node : Single_Protected_Decl'Class) return Parent_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Definition
     (Node : Single_Protected_Decl'Class) return Protected_Def;

   function F_Task_Type
     (Node : Single_Task_Decl'Class) return Single_Task_Type_Decl;

   function F_Discriminants
     (Node : Task_Type_Decl'Class) return Discriminant_Part;

   function F_Definition (Node : Task_Type_Decl'Class) return Task_Def;

   function P_Denoted_Value (Node : String_Literal'Class) return Text_Type;
   --  Return the value that this literal denotes.

   function F_Decls (Node : Subp_Body'Class) return Declarative_Part;

   function F_Stmts (Node : Subp_Body'Class) return Handled_Stmts;

   function F_End_Name (Node : Subp_Body'Class) return End_Name;

   function F_Overriding (Node : Subp_Body_Stub'Class) return Overriding_Node;

   function F_Overriding
     (Node : Subp_Body_Stub'Class) return Ada_Overriding_Node;

   function F_Subp_Spec (Node : Subp_Body_Stub'Class) return Subp_Spec;

   function F_Renames (Node : Subp_Renaming_Decl'Class) return Renaming_Clause;

   function F_Subp_Kind (Node : Subp_Spec'Class) return Subp_Kind;

   function F_Subp_Kind (Node : Subp_Spec'Class) return Ada_Subp_Kind;

   function F_Subp_Name (Node : Subp_Spec'Class) return Defining_Name;

   function F_Subp_Params (Node : Subp_Spec'Class) return Params;

   function F_Subp_Returns (Node : Subp_Spec'Class) return Type_Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Anonymous_Type
   --
   --  * Subtype_Indication

   function F_Subtype (Node : Subtype_Decl'Class) return Subtype_Indication;

   function F_Name (Node : Subunit'Class) return Name;
   --  This field can contain one of the following nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Body (Node : Subunit'Class) return Body_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Package_Body
   --
   --  * Protected_Body
   --
   --  * Subp_Body
   --
   --  * Task_Body

   function P_Body_Root (Node : Subunit'Class) return Basic_Decl;
   --  Return the body in which this subunit is rooted.

   function P_As_Bool (Node : Synchronized_Node'Class) return Boolean;
   --  Return whether this is an instance of SynchronizedPresent

   function P_As_Bool (Node : Tagged_Node'Class) return Boolean;
   --  Return whether this is an instance of TaggedPresent

   function F_Name (Node : Task_Body'Class) return Defining_Name;

   function F_Decls (Node : Task_Body'Class) return Declarative_Part;

   function F_Stmts (Node : Task_Body'Class) return Handled_Stmts;

   function F_End_Name (Node : Task_Body'Class) return End_Name;

   function F_Name (Node : Task_Body_Stub'Class) return Defining_Name;

   function F_Interfaces (Node : Task_Def'Class) return Parent_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Public_Part (Node : Task_Def'Class) return Public_Part;

   function F_Private_Part (Node : Task_Def'Class) return Private_Part;

   function F_End_Name (Node : Task_Def'Class) return End_Name;

   function F_Has_All (Node : Type_Access_Def'Class) return All_Node;

   function F_Has_All (Node : Type_Access_Def'Class) return Boolean;

   function F_Has_Constant (Node : Type_Access_Def'Class) return Constant_Node;

   function F_Has_Constant (Node : Type_Access_Def'Class) return Boolean;

   function F_Subtype_Indication
     (Node : Type_Access_Def'Class) return Subtype_Indication;

   function F_Op (Node : Un_Op'Class) return Op;
   --  This field can contain one of the following nodes:
   --
   --  * Op_Abs
   --
   --  * Op_Minus
   --
   --  * Op_Not
   --
   --  * Op_Plus

   function F_Op (Node : Un_Op'Class) return Ada_Op;

   function F_Expr (Node : Un_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Subtype_Indication
     (Node : Unconstrained_Array_Index'Class) return Subtype_Indication;

   function List_Child
     (Node : Unconstrained_Array_Index_List'Class; Index : Positive)
      return Unconstrained_Array_Index;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Unconstrained_Array_Index_List_First
     (Node : Unconstrained_Array_Index_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Unconstrained_Array_Index_List_Next
     (Node : Unconstrained_Array_Index_List; Cursor : Positive)
      return Positive;
   --  Implementation detail for the Iterable aspect

   function Unconstrained_Array_Index_List_Has_Element
     (Node : Unconstrained_Array_Index_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Unconstrained_Array_Index_List_Element
     (Node : Unconstrained_Array_Index_List; Cursor : Positive)
      return Unconstrained_Array_Index'Class;
   --  Implementation detail for the Iterable aspect

   function F_Types
     (Node : Unconstrained_Array_Indices'Class)
      return Unconstrained_Array_Index_List;

   function P_As_Bool (Node : Until_Node'Class) return Boolean;
   --  Return whether this is an instance of UntilPresent

   function F_Packages (Node : Use_Package_Clause'Class) return Name_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function F_Has_All (Node : Use_Type_Clause'Class) return All_Node;

   function F_Has_All (Node : Use_Type_Clause'Class) return Boolean;

   function F_Types (Node : Use_Type_Clause'Class) return Name_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Attribute_Ref
   --
   --  * Call_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * Qual_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name

   function F_Choices (Node : Variant'Class) return Alternatives_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Discrete_Subtype_Indication
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Others_Designator
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Components (Node : Variant'Class) return Component_List;

   function List_Child
     (Node : Variant_List'Class; Index : Positive) return Variant;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Variant_List_First (Node : Variant_List) return Positive;
   --  Implementation detail for the Iterable aspect

   function Variant_List_Next
     (Node : Variant_List; Cursor : Positive) return Positive;
   --  Implementation detail for the Iterable aspect

   function Variant_List_Has_Element
     (Node : Variant_List; Cursor : Positive) return Boolean;
   --  Implementation detail for the Iterable aspect

   function Variant_List_Element
     (Node : Variant_List; Cursor : Positive) return Variant'Class;
   --  Implementation detail for the Iterable aspect

   function F_Discr_Name (Node : Variant_Part'Class) return Identifier;

   function F_Variant (Node : Variant_Part'Class) return Variant_List;

   function F_Expr (Node : While_Loop_Spec'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Allocator
   --
   --  * Attribute_Ref
   --
   --  * Base_Aggregate
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Case_Expr
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Explicit_Deref
   --
   --  * Identifier
   --
   --  * If_Expr
   --
   --  * Membership_Expr
   --
   --  * Null_Literal
   --
   --  * Num_Literal
   --
   --  * Paren_Expr
   --
   --  * Qual_Expr
   --
   --  * Quantified_Expr
   --
   --  * Raise_Expr
   --
   --  * String_Literal
   --
   --  * Target_Name
   --
   --  * Un_Op

   function F_Has_Limited (Node : With_Clause'Class) return Limited_Node;

   function F_Has_Limited (Node : With_Clause'Class) return Boolean;

   function F_Has_Private (Node : With_Clause'Class) return Private_Node;

   function F_Has_Private (Node : With_Clause'Class) return Boolean;

   function F_Packages (Node : With_Clause'Class) return Name_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Char_Literal
   --
   --  * Dotted_Name
   --
   --  * Identifier
   --
   --  * String_Literal

   function P_As_Bool (Node : With_Private'Class) return Boolean;
   --  Return whether this is an instance of WithPrivatePresent

   pragma Warnings (On, "defined after private extension");

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Children_Count (Node : Ada_Node'Class) return Natural;
   --  Return the number of children Node has

   function First_Child_Index (Node : Ada_Node'Class) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index (Node : Ada_Node'Class) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   pragma Warnings (Off, "defined after private extension");
   procedure Get_Child
     (Node   : Ada_Node'Class; Index : Positive; Index_In_Bounds : out Boolean;
      Result : out Ada_Node);
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. Index is out-of-bounds), the content of
   --  Result is undefined.

   function Child (Node : Ada_Node'Class; Index : Positive) return Ada_Node;
   --  Return the Index'th child of Node, or null if Node has no such child
   pragma Warnings (On, "defined after private extension");

   function Traverse
     (Node  : Ada_Node'Class;
      Visit : access function (Node : Ada_Node'Class) return Visit_Status)
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
     (Node  : Ada_Node'Class;
      Visit : access function (Node : Ada_Node'Class) return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range (Node : Ada_Node'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Ada_Node'Class; Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   pragma Warnings (Off, "defined after private extension");
   function Lookup
     (Node : Ada_Node'Class; Sloc : Source_Location) return Ada_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   pragma Warnings (On, "defined after private extension");

   -----------------------
   -- Lexical utilities --
   -----------------------

   function Text (Node : Ada_Node'Class) return Text_Type;
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   function Debug_Text (Node : Ada_Node'Class) return String;
   --  Like ``Text``, to get the source buffer slice as a string

   function Token_Range (Node : Ada_Node'Class) return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   function Is_Keyword
     (Token : Token_Reference; Version : Language_Version) return Boolean;
--  Given an Ada language version, return whether ``Token`` is an Ada keyword.
--
--  Due to the way Libadalang works, every token added after Ada 83 is lexed
--  as a regular identifier, and then treated specially by the parser in some
--  circumstances (being akin to the notion of reserved word).
--
--  This function returns True for regular lexer keywords, as well as for those
--  identifiers.

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : Ada_Node'Class; Show_Slocs : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.
   --
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia (Node : Ada_Node'Class; Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Ada_Node'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   --  The following As_* functions convert references to nodes from one
   --  type to another (Ada_Node can refer to any node type). They raise
   --  a Constraint_Error if the conversion is invalid.

   pragma Warnings (Off, "defined after private extension");
   function As_Ada_Node (Node : Ada_Node'Class) return Ada_Node;
      --% no-document: True
   function As_Expr (Node : Ada_Node'Class) return Expr;
      --% no-document: True
   function As_Basic_Decl (Node : Ada_Node'Class) return Basic_Decl;
      --% no-document: True
   function As_Abort_Node (Node : Ada_Node'Class) return Abort_Node;
      --% no-document: True
   function As_Abort_Absent (Node : Ada_Node'Class) return Abort_Absent;
      --% no-document: True
   function As_Abort_Present (Node : Ada_Node'Class) return Abort_Present;
      --% no-document: True
   function As_Stmt (Node : Ada_Node'Class) return Stmt;
      --% no-document: True
   function As_Simple_Stmt (Node : Ada_Node'Class) return Simple_Stmt;
      --% no-document: True
   function As_Abort_Stmt (Node : Ada_Node'Class) return Abort_Stmt;
      --% no-document: True
   function As_Abstract_Node (Node : Ada_Node'Class) return Abstract_Node;
      --% no-document: True
   function As_Abstract_Absent (Node : Ada_Node'Class) return Abstract_Absent;
      --% no-document: True
   function As_Basic_Subp_Decl (Node : Ada_Node'Class) return Basic_Subp_Decl;
      --% no-document: True
   function As_Classic_Subp_Decl
     (Node : Ada_Node'Class) return Classic_Subp_Decl;
      --% no-document: True
   function As_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Formal_Subp_Decl;
      --% no-document: True
   function As_Abstract_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Abstract_Formal_Subp_Decl;
      --% no-document: True
   function As_Abstract_Present
     (Node : Ada_Node'Class) return Abstract_Present;
      --% no-document: True
   function As_Abstract_Subp_Decl
     (Node : Ada_Node'Class) return Abstract_Subp_Decl;
      --% no-document: True
   function As_Composite_Stmt (Node : Ada_Node'Class) return Composite_Stmt;
      --% no-document: True
   function As_Accept_Stmt (Node : Ada_Node'Class) return Accept_Stmt;
      --% no-document: True
   function As_Accept_Stmt_With_Stmts
     (Node : Ada_Node'Class) return Accept_Stmt_With_Stmts;
      --% no-document: True
   function As_Type_Def (Node : Ada_Node'Class) return Type_Def;
      --% no-document: True
   function As_Access_Def (Node : Ada_Node'Class) return Access_Def;
      --% no-document: True
   function As_Access_To_Subp_Def
     (Node : Ada_Node'Class) return Access_To_Subp_Def;
      --% no-document: True
   function As_Ada_List (Node : Ada_Node'Class) return Ada_List;
      --% no-document: True
   function As_Ada_Node_List (Node : Ada_Node'Class) return Ada_Node_List;
      --% no-document: True
   function As_Base_Aggregate (Node : Ada_Node'Class) return Base_Aggregate;
      --% no-document: True
   function As_Aggregate (Node : Ada_Node'Class) return Aggregate;
      --% no-document: True
   function As_Basic_Assoc (Node : Ada_Node'Class) return Basic_Assoc;
      --% no-document: True
   function As_Aggregate_Assoc (Node : Ada_Node'Class) return Aggregate_Assoc;
      --% no-document: True
   function As_Aliased_Node (Node : Ada_Node'Class) return Aliased_Node;
      --% no-document: True
   function As_Aliased_Absent (Node : Ada_Node'Class) return Aliased_Absent;
      --% no-document: True
   function As_Aliased_Present (Node : Ada_Node'Class) return Aliased_Present;
      --% no-document: True
   function As_All_Node (Node : Ada_Node'Class) return All_Node;
      --% no-document: True
   function As_All_Absent (Node : Ada_Node'Class) return All_Absent;
      --% no-document: True
   function As_All_Present (Node : Ada_Node'Class) return All_Present;
      --% no-document: True
   function As_Allocator (Node : Ada_Node'Class) return Allocator;
      --% no-document: True
   function As_Alternatives_List
     (Node : Ada_Node'Class) return Alternatives_List;
      --% no-document: True
   function As_Object_Decl (Node : Ada_Node'Class) return Object_Decl;
      --% no-document: True
   function As_Anonymous_Object_Decl
     (Node : Ada_Node'Class) return Anonymous_Object_Decl;
      --% no-document: True
   function As_Type_Expr (Node : Ada_Node'Class) return Type_Expr;
      --% no-document: True
   function As_Anonymous_Type (Node : Ada_Node'Class) return Anonymous_Type;
      --% no-document: True
   function As_Base_Type_Access_Def
     (Node : Ada_Node'Class) return Base_Type_Access_Def;
      --% no-document: True
   function As_Anonymous_Type_Access_Def
     (Node : Ada_Node'Class) return Anonymous_Type_Access_Def;
      --% no-document: True
   function As_Base_Type_Decl (Node : Ada_Node'Class) return Base_Type_Decl;
      --% no-document: True
   function As_Type_Decl (Node : Ada_Node'Class) return Type_Decl;
      --% no-document: True
   function As_Anonymous_Type_Decl
     (Node : Ada_Node'Class) return Anonymous_Type_Decl;
      --% no-document: True
   function As_Array_Indices (Node : Ada_Node'Class) return Array_Indices;
      --% no-document: True
   function As_Array_Type_Def (Node : Ada_Node'Class) return Array_Type_Def;
      --% no-document: True
   function As_Aspect_Assoc (Node : Ada_Node'Class) return Aspect_Assoc;
      --% no-document: True
   function As_Aspect_Assoc_List
     (Node : Ada_Node'Class) return Aspect_Assoc_List;
      --% no-document: True
   function As_Aspect_Clause (Node : Ada_Node'Class) return Aspect_Clause;
      --% no-document: True
   function As_Aspect_Spec (Node : Ada_Node'Class) return Aspect_Spec;
      --% no-document: True
   function As_Assign_Stmt (Node : Ada_Node'Class) return Assign_Stmt;
      --% no-document: True
   function As_Basic_Assoc_List
     (Node : Ada_Node'Class) return Basic_Assoc_List;
      --% no-document: True
   function As_Assoc_List (Node : Ada_Node'Class) return Assoc_List;
      --% no-document: True
   function As_At_Clause (Node : Ada_Node'Class) return At_Clause;
      --% no-document: True
   function As_Attribute_Def_Clause
     (Node : Ada_Node'Class) return Attribute_Def_Clause;
      --% no-document: True
   function As_Name (Node : Ada_Node'Class) return Name;
      --% no-document: True
   function As_Attribute_Ref (Node : Ada_Node'Class) return Attribute_Ref;
      --% no-document: True
   function As_Base_Assoc (Node : Ada_Node'Class) return Base_Assoc;
      --% no-document: True
   function As_Base_Assoc_List (Node : Ada_Node'Class) return Base_Assoc_List;
      --% no-document: True
   function As_Base_Formal_Param_Decl
     (Node : Ada_Node'Class) return Base_Formal_Param_Decl;
      --% no-document: True
   function As_Base_Formal_Param_Holder
     (Node : Ada_Node'Class) return Base_Formal_Param_Holder;
      --% no-document: True
   function As_Single_Tok_Node (Node : Ada_Node'Class) return Single_Tok_Node;
      --% no-document: True
   function As_Base_Id (Node : Ada_Node'Class) return Base_Id;
      --% no-document: True
   function As_Base_Loop_Stmt (Node : Ada_Node'Class) return Base_Loop_Stmt;
      --% no-document: True
   function As_Base_Package_Decl
     (Node : Ada_Node'Class) return Base_Package_Decl;
      --% no-document: True
   function As_Base_Record_Def (Node : Ada_Node'Class) return Base_Record_Def;
      --% no-document: True
   function As_Body_Node (Node : Ada_Node'Class) return Body_Node;
      --% no-document: True
   function As_Base_Subp_Body (Node : Ada_Node'Class) return Base_Subp_Body;
      --% no-document: True
   function As_Base_Subp_Spec (Node : Ada_Node'Class) return Base_Subp_Spec;
      --% no-document: True
   function As_Base_Subtype_Decl
     (Node : Ada_Node'Class) return Base_Subtype_Decl;
      --% no-document: True
   function As_Block_Stmt (Node : Ada_Node'Class) return Block_Stmt;
      --% no-document: True
   function As_Begin_Block (Node : Ada_Node'Class) return Begin_Block;
      --% no-document: True
   function As_Bin_Op (Node : Ada_Node'Class) return Bin_Op;
      --% no-document: True
   function As_Body_Stub (Node : Ada_Node'Class) return Body_Stub;
      --% no-document: True
   function As_Box_Expr (Node : Ada_Node'Class) return Box_Expr;
      --% no-document: True
   function As_Call_Expr (Node : Ada_Node'Class) return Call_Expr;
      --% no-document: True
   function As_Call_Stmt (Node : Ada_Node'Class) return Call_Stmt;
      --% no-document: True
   function As_Case_Expr (Node : Ada_Node'Class) return Case_Expr;
      --% no-document: True
   function As_Case_Expr_Alternative
     (Node : Ada_Node'Class) return Case_Expr_Alternative;
      --% no-document: True
   function As_Case_Expr_Alternative_List
     (Node : Ada_Node'Class) return Case_Expr_Alternative_List;
      --% no-document: True
   function As_Case_Stmt (Node : Ada_Node'Class) return Case_Stmt;
      --% no-document: True
   function As_Case_Stmt_Alternative
     (Node : Ada_Node'Class) return Case_Stmt_Alternative;
      --% no-document: True
   function As_Case_Stmt_Alternative_List
     (Node : Ada_Node'Class) return Case_Stmt_Alternative_List;
      --% no-document: True
   function As_Char_Literal (Node : Ada_Node'Class) return Char_Literal;
      --% no-document: True
   function As_Classwide_Type_Decl
     (Node : Ada_Node'Class) return Classwide_Type_Decl;
      --% no-document: True
   function As_Compilation_Unit
     (Node : Ada_Node'Class) return Compilation_Unit;
      --% no-document: True
   function As_Compilation_Unit_List
     (Node : Ada_Node'Class) return Compilation_Unit_List;
      --% no-document: True
   function As_Component_Clause
     (Node : Ada_Node'Class) return Component_Clause;
      --% no-document: True
   function As_Component_Decl (Node : Ada_Node'Class) return Component_Decl;
      --% no-document: True
   function As_Component_Def (Node : Ada_Node'Class) return Component_Def;
      --% no-document: True
   function As_Component_List (Node : Ada_Node'Class) return Component_List;
      --% no-document: True
   function As_Concrete_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Concrete_Formal_Subp_Decl;
      --% no-document: True
   function As_Constant_Node (Node : Ada_Node'Class) return Constant_Node;
      --% no-document: True
   function As_Constant_Absent (Node : Ada_Node'Class) return Constant_Absent;
      --% no-document: True
   function As_Constant_Present
     (Node : Ada_Node'Class) return Constant_Present;
      --% no-document: True
   function As_Constrained_Array_Indices
     (Node : Ada_Node'Class) return Constrained_Array_Indices;
      --% no-document: True
   function As_Subtype_Indication
     (Node : Ada_Node'Class) return Subtype_Indication;
      --% no-document: True
   function As_Constrained_Subtype_Indication
     (Node : Ada_Node'Class) return Constrained_Subtype_Indication;
      --% no-document: True
   function As_Constraint (Node : Ada_Node'Class) return Constraint;
      --% no-document: True
   function As_Constraint_List (Node : Ada_Node'Class) return Constraint_List;
      --% no-document: True
   function As_Contract_Case_Assoc
     (Node : Ada_Node'Class) return Contract_Case_Assoc;
      --% no-document: True
   function As_Contract_Case_Assoc_List
     (Node : Ada_Node'Class) return Contract_Case_Assoc_List;
      --% no-document: True
   function As_Contract_Cases (Node : Ada_Node'Class) return Contract_Cases;
      --% no-document: True
   function As_Real_Type_Def (Node : Ada_Node'Class) return Real_Type_Def;
      --% no-document: True
   function As_Decimal_Fixed_Point_Def
     (Node : Ada_Node'Class) return Decimal_Fixed_Point_Def;
      --% no-document: True
   function As_Decl_Block (Node : Ada_Node'Class) return Decl_Block;
      --% no-document: True
   function As_Decl_List (Node : Ada_Node'Class) return Decl_List;
      --% no-document: True
   function As_Declarative_Part
     (Node : Ada_Node'Class) return Declarative_Part;
      --% no-document: True
   function As_Defining_Name (Node : Ada_Node'Class) return Defining_Name;
      --% no-document: True
   function As_Defining_Name_List
     (Node : Ada_Node'Class) return Defining_Name_List;
      --% no-document: True
   function As_Delay_Stmt (Node : Ada_Node'Class) return Delay_Stmt;
      --% no-document: True
   function As_Delta_Constraint
     (Node : Ada_Node'Class) return Delta_Constraint;
      --% no-document: True
   function As_Derived_Type_Def
     (Node : Ada_Node'Class) return Derived_Type_Def;
      --% no-document: True
   function As_Digits_Constraint
     (Node : Ada_Node'Class) return Digits_Constraint;
      --% no-document: True
   function As_Discrete_Base_Subtype_Decl
     (Node : Ada_Node'Class) return Discrete_Base_Subtype_Decl;
      --% no-document: True
   function As_Discrete_Subtype_Indication
     (Node : Ada_Node'Class) return Discrete_Subtype_Indication;
      --% no-document: True
   function As_Discrete_Subtype_Name
     (Node : Ada_Node'Class) return Discrete_Subtype_Name;
      --% no-document: True
   function As_Discriminant_Assoc
     (Node : Ada_Node'Class) return Discriminant_Assoc;
      --% no-document: True
   function As_Identifier_List (Node : Ada_Node'Class) return Identifier_List;
      --% no-document: True
   function As_Discriminant_Choice_List
     (Node : Ada_Node'Class) return Discriminant_Choice_List;
      --% no-document: True
   function As_Discriminant_Constraint
     (Node : Ada_Node'Class) return Discriminant_Constraint;
      --% no-document: True
   function As_Discriminant_Part
     (Node : Ada_Node'Class) return Discriminant_Part;
      --% no-document: True
   function As_Discriminant_Spec
     (Node : Ada_Node'Class) return Discriminant_Spec;
      --% no-document: True
   function As_Discriminant_Spec_List
     (Node : Ada_Node'Class) return Discriminant_Spec_List;
      --% no-document: True
   function As_Dotted_Name (Node : Ada_Node'Class) return Dotted_Name;
      --% no-document: True
   function As_Elsif_Expr_Part (Node : Ada_Node'Class) return Elsif_Expr_Part;
      --% no-document: True
   function As_Elsif_Expr_Part_List
     (Node : Ada_Node'Class) return Elsif_Expr_Part_List;
      --% no-document: True
   function As_Elsif_Stmt_Part (Node : Ada_Node'Class) return Elsif_Stmt_Part;
      --% no-document: True
   function As_Elsif_Stmt_Part_List
     (Node : Ada_Node'Class) return Elsif_Stmt_Part_List;
      --% no-document: True
   function As_End_Name (Node : Ada_Node'Class) return End_Name;
      --% no-document: True
   function As_Entry_Body (Node : Ada_Node'Class) return Entry_Body;
      --% no-document: True
   function As_Entry_Completion_Formal_Params
     (Node : Ada_Node'Class) return Entry_Completion_Formal_Params;
      --% no-document: True
   function As_Entry_Decl (Node : Ada_Node'Class) return Entry_Decl;
      --% no-document: True
   function As_Entry_Index_Spec
     (Node : Ada_Node'Class) return Entry_Index_Spec;
      --% no-document: True
   function As_Entry_Spec (Node : Ada_Node'Class) return Entry_Spec;
      --% no-document: True
   function As_Enum_Lit_Synth_Type_Expr
     (Node : Ada_Node'Class) return Enum_Lit_Synth_Type_Expr;
      --% no-document: True
   function As_Enum_Literal_Decl
     (Node : Ada_Node'Class) return Enum_Literal_Decl;
      --% no-document: True
   function As_Enum_Literal_Decl_List
     (Node : Ada_Node'Class) return Enum_Literal_Decl_List;
      --% no-document: True
   function As_Enum_Rep_Clause (Node : Ada_Node'Class) return Enum_Rep_Clause;
      --% no-document: True
   function As_Enum_Subp_Spec (Node : Ada_Node'Class) return Enum_Subp_Spec;
      --% no-document: True
   function As_Enum_Type_Def (Node : Ada_Node'Class) return Enum_Type_Def;
      --% no-document: True
   function As_Error_Decl (Node : Ada_Node'Class) return Error_Decl;
      --% no-document: True
   function As_Error_Stmt (Node : Ada_Node'Class) return Error_Stmt;
      --% no-document: True
   function As_Exception_Decl (Node : Ada_Node'Class) return Exception_Decl;
      --% no-document: True
   function As_Exception_Handler
     (Node : Ada_Node'Class) return Exception_Handler;
      --% no-document: True
   function As_Exit_Stmt (Node : Ada_Node'Class) return Exit_Stmt;
      --% no-document: True
   function As_Explicit_Deref (Node : Ada_Node'Class) return Explicit_Deref;
      --% no-document: True
   function As_Expr_List (Node : Ada_Node'Class) return Expr_List;
      --% no-document: True
   function As_Expr_Alternatives_List
     (Node : Ada_Node'Class) return Expr_Alternatives_List;
      --% no-document: True
   function As_Expr_Function (Node : Ada_Node'Class) return Expr_Function;
      --% no-document: True
   function As_Extended_Return_Stmt
     (Node : Ada_Node'Class) return Extended_Return_Stmt;
      --% no-document: True
   function As_Extended_Return_Stmt_Object_Decl
     (Node : Ada_Node'Class) return Extended_Return_Stmt_Object_Decl;
      --% no-document: True
   function As_Floating_Point_Def
     (Node : Ada_Node'Class) return Floating_Point_Def;
      --% no-document: True
   function As_Loop_Spec (Node : Ada_Node'Class) return Loop_Spec;
      --% no-document: True
   function As_For_Loop_Spec (Node : Ada_Node'Class) return For_Loop_Spec;
      --% no-document: True
   function As_For_Loop_Stmt (Node : Ada_Node'Class) return For_Loop_Stmt;
      --% no-document: True
   function As_For_Loop_Var_Decl
     (Node : Ada_Node'Class) return For_Loop_Var_Decl;
      --% no-document: True
   function As_Formal_Discrete_Type_Def
     (Node : Ada_Node'Class) return Formal_Discrete_Type_Def;
      --% no-document: True
   function As_Generic_Decl (Node : Ada_Node'Class) return Generic_Decl;
      --% no-document: True
   function As_Generic_Formal (Node : Ada_Node'Class) return Generic_Formal;
      --% no-document: True
   function As_Generic_Formal_Obj_Decl
     (Node : Ada_Node'Class) return Generic_Formal_Obj_Decl;
      --% no-document: True
   function As_Generic_Formal_Package
     (Node : Ada_Node'Class) return Generic_Formal_Package;
      --% no-document: True
   function As_Generic_Formal_Part
     (Node : Ada_Node'Class) return Generic_Formal_Part;
      --% no-document: True
   function As_Generic_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Generic_Formal_Subp_Decl;
      --% no-document: True
   function As_Generic_Formal_Type_Decl
     (Node : Ada_Node'Class) return Generic_Formal_Type_Decl;
      --% no-document: True
   function As_Generic_Instantiation
     (Node : Ada_Node'Class) return Generic_Instantiation;
      --% no-document: True
   function As_Generic_Package_Decl
     (Node : Ada_Node'Class) return Generic_Package_Decl;
      --% no-document: True
   function As_Generic_Package_Instantiation
     (Node : Ada_Node'Class) return Generic_Package_Instantiation;
      --% no-document: True
   function As_Generic_Package_Internal
     (Node : Ada_Node'Class) return Generic_Package_Internal;
      --% no-document: True
   function As_Generic_Renaming_Decl
     (Node : Ada_Node'Class) return Generic_Renaming_Decl;
      --% no-document: True
   function As_Generic_Package_Renaming_Decl
     (Node : Ada_Node'Class) return Generic_Package_Renaming_Decl;
      --% no-document: True
   function As_Generic_Subp_Decl
     (Node : Ada_Node'Class) return Generic_Subp_Decl;
      --% no-document: True
   function As_Generic_Subp_Instantiation
     (Node : Ada_Node'Class) return Generic_Subp_Instantiation;
      --% no-document: True
   function As_Generic_Subp_Internal
     (Node : Ada_Node'Class) return Generic_Subp_Internal;
      --% no-document: True
   function As_Generic_Subp_Renaming_Decl
     (Node : Ada_Node'Class) return Generic_Subp_Renaming_Decl;
      --% no-document: True
   function As_Goto_Stmt (Node : Ada_Node'Class) return Goto_Stmt;
      --% no-document: True
   function As_Handled_Stmts (Node : Ada_Node'Class) return Handled_Stmts;
      --% no-document: True
   function As_Identifier (Node : Ada_Node'Class) return Identifier;
      --% no-document: True
   function As_If_Expr (Node : Ada_Node'Class) return If_Expr;
      --% no-document: True
   function As_If_Stmt (Node : Ada_Node'Class) return If_Stmt;
      --% no-document: True
   function As_Incomplete_Type_Decl
     (Node : Ada_Node'Class) return Incomplete_Type_Decl;
      --% no-document: True
   function As_Incomplete_Tagged_Type_Decl
     (Node : Ada_Node'Class) return Incomplete_Tagged_Type_Decl;
      --% no-document: True
   function As_Index_Constraint
     (Node : Ada_Node'Class) return Index_Constraint;
      --% no-document: True
   function As_Num_Literal (Node : Ada_Node'Class) return Num_Literal;
      --% no-document: True
   function As_Int_Literal (Node : Ada_Node'Class) return Int_Literal;
      --% no-document: True
   function As_Interface_Kind (Node : Ada_Node'Class) return Interface_Kind;
      --% no-document: True
   function As_Interface_Kind_Limited
     (Node : Ada_Node'Class) return Interface_Kind_Limited;
      --% no-document: True
   function As_Interface_Kind_Protected
     (Node : Ada_Node'Class) return Interface_Kind_Protected;
      --% no-document: True
   function As_Interface_Kind_Synchronized
     (Node : Ada_Node'Class) return Interface_Kind_Synchronized;
      --% no-document: True
   function As_Interface_Kind_Task
     (Node : Ada_Node'Class) return Interface_Kind_Task;
      --% no-document: True
   function As_Interface_Type_Def
     (Node : Ada_Node'Class) return Interface_Type_Def;
      --% no-document: True
   function As_Iter_Type (Node : Ada_Node'Class) return Iter_Type;
      --% no-document: True
   function As_Iter_Type_In (Node : Ada_Node'Class) return Iter_Type_In;
      --% no-document: True
   function As_Iter_Type_Of (Node : Ada_Node'Class) return Iter_Type_Of;
      --% no-document: True
   function As_Known_Discriminant_Part
     (Node : Ada_Node'Class) return Known_Discriminant_Part;
      --% no-document: True
   function As_Label (Node : Ada_Node'Class) return Label;
      --% no-document: True
   function As_Label_Decl (Node : Ada_Node'Class) return Label_Decl;
      --% no-document: True
   function As_Library_Item (Node : Ada_Node'Class) return Library_Item;
      --% no-document: True
   function As_Limited_Node (Node : Ada_Node'Class) return Limited_Node;
      --% no-document: True
   function As_Limited_Absent (Node : Ada_Node'Class) return Limited_Absent;
      --% no-document: True
   function As_Limited_Present (Node : Ada_Node'Class) return Limited_Present;
      --% no-document: True
   function As_Loop_Stmt (Node : Ada_Node'Class) return Loop_Stmt;
      --% no-document: True
   function As_Membership_Expr (Node : Ada_Node'Class) return Membership_Expr;
      --% no-document: True
   function As_Mod_Int_Type_Def
     (Node : Ada_Node'Class) return Mod_Int_Type_Def;
      --% no-document: True
   function As_Mode (Node : Ada_Node'Class) return Mode;
      --% no-document: True
   function As_Mode_Default (Node : Ada_Node'Class) return Mode_Default;
      --% no-document: True
   function As_Mode_In (Node : Ada_Node'Class) return Mode_In;
      --% no-document: True
   function As_Mode_In_Out (Node : Ada_Node'Class) return Mode_In_Out;
      --% no-document: True
   function As_Mode_Out (Node : Ada_Node'Class) return Mode_Out;
      --% no-document: True
   function As_Multi_Dim_Array_Assoc
     (Node : Ada_Node'Class) return Multi_Dim_Array_Assoc;
      --% no-document: True
   function As_Name_List (Node : Ada_Node'Class) return Name_List;
      --% no-document: True
   function As_Named_Stmt (Node : Ada_Node'Class) return Named_Stmt;
      --% no-document: True
   function As_Named_Stmt_Decl (Node : Ada_Node'Class) return Named_Stmt_Decl;
      --% no-document: True
   function As_Not_Null (Node : Ada_Node'Class) return Not_Null;
      --% no-document: True
   function As_Not_Null_Absent (Node : Ada_Node'Class) return Not_Null_Absent;
      --% no-document: True
   function As_Not_Null_Present
     (Node : Ada_Node'Class) return Not_Null_Present;
      --% no-document: True
   function As_Null_Component_Decl
     (Node : Ada_Node'Class) return Null_Component_Decl;
      --% no-document: True
   function As_Null_Literal (Node : Ada_Node'Class) return Null_Literal;
      --% no-document: True
   function As_Null_Record_Aggregate
     (Node : Ada_Node'Class) return Null_Record_Aggregate;
      --% no-document: True
   function As_Null_Record_Def (Node : Ada_Node'Class) return Null_Record_Def;
      --% no-document: True
   function As_Null_Stmt (Node : Ada_Node'Class) return Null_Stmt;
      --% no-document: True
   function As_Null_Subp_Decl (Node : Ada_Node'Class) return Null_Subp_Decl;
      --% no-document: True
   function As_Number_Decl (Node : Ada_Node'Class) return Number_Decl;
      --% no-document: True
   function As_Op (Node : Ada_Node'Class) return Op;
      --% no-document: True
   function As_Op_Abs (Node : Ada_Node'Class) return Op_Abs;
      --% no-document: True
   function As_Op_And (Node : Ada_Node'Class) return Op_And;
      --% no-document: True
   function As_Op_And_Then (Node : Ada_Node'Class) return Op_And_Then;
      --% no-document: True
   function As_Op_Concat (Node : Ada_Node'Class) return Op_Concat;
      --% no-document: True
   function As_Op_Div (Node : Ada_Node'Class) return Op_Div;
      --% no-document: True
   function As_Op_Double_Dot (Node : Ada_Node'Class) return Op_Double_Dot;
      --% no-document: True
   function As_Op_Eq (Node : Ada_Node'Class) return Op_Eq;
      --% no-document: True
   function As_Op_Gt (Node : Ada_Node'Class) return Op_Gt;
      --% no-document: True
   function As_Op_Gte (Node : Ada_Node'Class) return Op_Gte;
      --% no-document: True
   function As_Op_In (Node : Ada_Node'Class) return Op_In;
      --% no-document: True
   function As_Op_Lt (Node : Ada_Node'Class) return Op_Lt;
      --% no-document: True
   function As_Op_Lte (Node : Ada_Node'Class) return Op_Lte;
      --% no-document: True
   function As_Op_Minus (Node : Ada_Node'Class) return Op_Minus;
      --% no-document: True
   function As_Op_Mod (Node : Ada_Node'Class) return Op_Mod;
      --% no-document: True
   function As_Op_Mult (Node : Ada_Node'Class) return Op_Mult;
      --% no-document: True
   function As_Op_Neq (Node : Ada_Node'Class) return Op_Neq;
      --% no-document: True
   function As_Op_Not (Node : Ada_Node'Class) return Op_Not;
      --% no-document: True
   function As_Op_Not_In (Node : Ada_Node'Class) return Op_Not_In;
      --% no-document: True
   function As_Op_Or (Node : Ada_Node'Class) return Op_Or;
      --% no-document: True
   function As_Op_Or_Else (Node : Ada_Node'Class) return Op_Or_Else;
      --% no-document: True
   function As_Op_Plus (Node : Ada_Node'Class) return Op_Plus;
      --% no-document: True
   function As_Op_Pow (Node : Ada_Node'Class) return Op_Pow;
      --% no-document: True
   function As_Op_Rem (Node : Ada_Node'Class) return Op_Rem;
      --% no-document: True
   function As_Op_Xor (Node : Ada_Node'Class) return Op_Xor;
      --% no-document: True
   function As_Ordinary_Fixed_Point_Def
     (Node : Ada_Node'Class) return Ordinary_Fixed_Point_Def;
      --% no-document: True
   function As_Others_Designator
     (Node : Ada_Node'Class) return Others_Designator;
      --% no-document: True
   function As_Overriding_Node (Node : Ada_Node'Class) return Overriding_Node;
      --% no-document: True
   function As_Overriding_Not_Overriding
     (Node : Ada_Node'Class) return Overriding_Not_Overriding;
      --% no-document: True
   function As_Overriding_Overriding
     (Node : Ada_Node'Class) return Overriding_Overriding;
      --% no-document: True
   function As_Overriding_Unspecified
     (Node : Ada_Node'Class) return Overriding_Unspecified;
      --% no-document: True
   function As_Package_Body (Node : Ada_Node'Class) return Package_Body;
      --% no-document: True
   function As_Package_Body_Stub
     (Node : Ada_Node'Class) return Package_Body_Stub;
      --% no-document: True
   function As_Package_Decl (Node : Ada_Node'Class) return Package_Decl;
      --% no-document: True
   function As_Package_Renaming_Decl
     (Node : Ada_Node'Class) return Package_Renaming_Decl;
      --% no-document: True
   function As_Param_Assoc (Node : Ada_Node'Class) return Param_Assoc;
      --% no-document: True
   function As_Param_Spec (Node : Ada_Node'Class) return Param_Spec;
      --% no-document: True
   function As_Param_Spec_List (Node : Ada_Node'Class) return Param_Spec_List;
      --% no-document: True
   function As_Params (Node : Ada_Node'Class) return Params;
      --% no-document: True
   function As_Paren_Expr (Node : Ada_Node'Class) return Paren_Expr;
      --% no-document: True
   function As_Parent_List (Node : Ada_Node'Class) return Parent_List;
      --% no-document: True
   function As_Pragma_Argument_Assoc
     (Node : Ada_Node'Class) return Pragma_Argument_Assoc;
      --% no-document: True
   function As_Pragma_Node (Node : Ada_Node'Class) return Pragma_Node;
      --% no-document: True
   function As_Pragma_Node_List
     (Node : Ada_Node'Class) return Pragma_Node_List;
      --% no-document: True
   function As_Prim_Type_Accessor
     (Node : Ada_Node'Class) return Prim_Type_Accessor;
      --% no-document: True
   function As_Private_Node (Node : Ada_Node'Class) return Private_Node;
      --% no-document: True
   function As_Private_Absent (Node : Ada_Node'Class) return Private_Absent;
      --% no-document: True
   function As_Private_Part (Node : Ada_Node'Class) return Private_Part;
      --% no-document: True
   function As_Private_Present (Node : Ada_Node'Class) return Private_Present;
      --% no-document: True
   function As_Private_Type_Def
     (Node : Ada_Node'Class) return Private_Type_Def;
      --% no-document: True
   function As_Protected_Node (Node : Ada_Node'Class) return Protected_Node;
      --% no-document: True
   function As_Protected_Absent
     (Node : Ada_Node'Class) return Protected_Absent;
      --% no-document: True
   function As_Protected_Body (Node : Ada_Node'Class) return Protected_Body;
      --% no-document: True
   function As_Protected_Body_Stub
     (Node : Ada_Node'Class) return Protected_Body_Stub;
      --% no-document: True
   function As_Protected_Def (Node : Ada_Node'Class) return Protected_Def;
      --% no-document: True
   function As_Protected_Present
     (Node : Ada_Node'Class) return Protected_Present;
      --% no-document: True
   function As_Protected_Type_Decl
     (Node : Ada_Node'Class) return Protected_Type_Decl;
      --% no-document: True
   function As_Public_Part (Node : Ada_Node'Class) return Public_Part;
      --% no-document: True
   function As_Qual_Expr (Node : Ada_Node'Class) return Qual_Expr;
      --% no-document: True
   function As_Quantified_Expr (Node : Ada_Node'Class) return Quantified_Expr;
      --% no-document: True
   function As_Quantifier (Node : Ada_Node'Class) return Quantifier;
      --% no-document: True
   function As_Quantifier_All (Node : Ada_Node'Class) return Quantifier_All;
      --% no-document: True
   function As_Quantifier_Some (Node : Ada_Node'Class) return Quantifier_Some;
      --% no-document: True
   function As_Raise_Expr (Node : Ada_Node'Class) return Raise_Expr;
      --% no-document: True
   function As_Raise_Stmt (Node : Ada_Node'Class) return Raise_Stmt;
      --% no-document: True
   function As_Range_Constraint
     (Node : Ada_Node'Class) return Range_Constraint;
      --% no-document: True
   function As_Range_Spec (Node : Ada_Node'Class) return Range_Spec;
      --% no-document: True
   function As_Real_Literal (Node : Ada_Node'Class) return Real_Literal;
      --% no-document: True
   function As_Record_Def (Node : Ada_Node'Class) return Record_Def;
      --% no-document: True
   function As_Record_Rep_Clause
     (Node : Ada_Node'Class) return Record_Rep_Clause;
      --% no-document: True
   function As_Record_Type_Def (Node : Ada_Node'Class) return Record_Type_Def;
      --% no-document: True
   function As_Relation_Op (Node : Ada_Node'Class) return Relation_Op;
      --% no-document: True
   function As_Renaming_Clause (Node : Ada_Node'Class) return Renaming_Clause;
      --% no-document: True
   function As_Requeue_Stmt (Node : Ada_Node'Class) return Requeue_Stmt;
      --% no-document: True
   function As_Return_Stmt (Node : Ada_Node'Class) return Return_Stmt;
      --% no-document: True
   function As_Reverse_Node (Node : Ada_Node'Class) return Reverse_Node;
      --% no-document: True
   function As_Reverse_Absent (Node : Ada_Node'Class) return Reverse_Absent;
      --% no-document: True
   function As_Reverse_Present (Node : Ada_Node'Class) return Reverse_Present;
      --% no-document: True
   function As_Select_Stmt (Node : Ada_Node'Class) return Select_Stmt;
      --% no-document: True
   function As_Select_When_Part
     (Node : Ada_Node'Class) return Select_When_Part;
      --% no-document: True
   function As_Select_When_Part_List
     (Node : Ada_Node'Class) return Select_When_Part_List;
      --% no-document: True
   function As_Signed_Int_Type_Def
     (Node : Ada_Node'Class) return Signed_Int_Type_Def;
      --% no-document: True
   function As_Single_Protected_Decl
     (Node : Ada_Node'Class) return Single_Protected_Decl;
      --% no-document: True
   function As_Single_Task_Decl
     (Node : Ada_Node'Class) return Single_Task_Decl;
      --% no-document: True
   function As_Task_Type_Decl (Node : Ada_Node'Class) return Task_Type_Decl;
      --% no-document: True
   function As_Single_Task_Type_Decl
     (Node : Ada_Node'Class) return Single_Task_Type_Decl;
      --% no-document: True
   function As_Stmt_List (Node : Ada_Node'Class) return Stmt_List;
      --% no-document: True
   function As_String_Literal (Node : Ada_Node'Class) return String_Literal;
      --% no-document: True
   function As_Subp_Body (Node : Ada_Node'Class) return Subp_Body;
      --% no-document: True
   function As_Subp_Body_Stub (Node : Ada_Node'Class) return Subp_Body_Stub;
      --% no-document: True
   function As_Subp_Decl (Node : Ada_Node'Class) return Subp_Decl;
      --% no-document: True
   function As_Subp_Kind (Node : Ada_Node'Class) return Subp_Kind;
      --% no-document: True
   function As_Subp_Kind_Function
     (Node : Ada_Node'Class) return Subp_Kind_Function;
      --% no-document: True
   function As_Subp_Kind_Procedure
     (Node : Ada_Node'Class) return Subp_Kind_Procedure;
      --% no-document: True
   function As_Subp_Renaming_Decl
     (Node : Ada_Node'Class) return Subp_Renaming_Decl;
      --% no-document: True
   function As_Subp_Spec (Node : Ada_Node'Class) return Subp_Spec;
      --% no-document: True
   function As_Subtype_Decl (Node : Ada_Node'Class) return Subtype_Decl;
      --% no-document: True
   function As_Subunit (Node : Ada_Node'Class) return Subunit;
      --% no-document: True
   function As_Synchronized_Node
     (Node : Ada_Node'Class) return Synchronized_Node;
      --% no-document: True
   function As_Synchronized_Absent
     (Node : Ada_Node'Class) return Synchronized_Absent;
      --% no-document: True
   function As_Synchronized_Present
     (Node : Ada_Node'Class) return Synchronized_Present;
      --% no-document: True
   function As_Synth_Anonymous_Type_Decl
     (Node : Ada_Node'Class) return Synth_Anonymous_Type_Decl;
      --% no-document: True
   function As_Synthetic_Renaming_Clause
     (Node : Ada_Node'Class) return Synthetic_Renaming_Clause;
      --% no-document: True
   function As_Tagged_Node (Node : Ada_Node'Class) return Tagged_Node;
      --% no-document: True
   function As_Tagged_Absent (Node : Ada_Node'Class) return Tagged_Absent;
      --% no-document: True
   function As_Tagged_Present (Node : Ada_Node'Class) return Tagged_Present;
      --% no-document: True
   function As_Target_Name (Node : Ada_Node'Class) return Target_Name;
      --% no-document: True
   function As_Task_Body (Node : Ada_Node'Class) return Task_Body;
      --% no-document: True
   function As_Task_Body_Stub (Node : Ada_Node'Class) return Task_Body_Stub;
      --% no-document: True
   function As_Task_Def (Node : Ada_Node'Class) return Task_Def;
      --% no-document: True
   function As_Terminate_Alternative
     (Node : Ada_Node'Class) return Terminate_Alternative;
      --% no-document: True
   function As_Type_Access_Def (Node : Ada_Node'Class) return Type_Access_Def;
      --% no-document: True
   function As_Un_Op (Node : Ada_Node'Class) return Un_Op;
      --% no-document: True
   function As_Unconstrained_Array_Index
     (Node : Ada_Node'Class) return Unconstrained_Array_Index;
      --% no-document: True
   function As_Unconstrained_Array_Index_List
     (Node : Ada_Node'Class) return Unconstrained_Array_Index_List;
      --% no-document: True
   function As_Unconstrained_Array_Indices
     (Node : Ada_Node'Class) return Unconstrained_Array_Indices;
      --% no-document: True
   function As_Unknown_Discriminant_Part
     (Node : Ada_Node'Class) return Unknown_Discriminant_Part;
      --% no-document: True
   function As_Until_Node (Node : Ada_Node'Class) return Until_Node;
      --% no-document: True
   function As_Until_Absent (Node : Ada_Node'Class) return Until_Absent;
      --% no-document: True
   function As_Until_Present (Node : Ada_Node'Class) return Until_Present;
      --% no-document: True
   function As_Update_Attribute_Ref
     (Node : Ada_Node'Class) return Update_Attribute_Ref;
      --% no-document: True
   function As_Use_Clause (Node : Ada_Node'Class) return Use_Clause;
      --% no-document: True
   function As_Use_Package_Clause
     (Node : Ada_Node'Class) return Use_Package_Clause;
      --% no-document: True
   function As_Use_Type_Clause (Node : Ada_Node'Class) return Use_Type_Clause;
      --% no-document: True
   function As_Variant (Node : Ada_Node'Class) return Variant;
      --% no-document: True
   function As_Variant_List (Node : Ada_Node'Class) return Variant_List;
      --% no-document: True
   function As_Variant_Part (Node : Ada_Node'Class) return Variant_Part;
      --% no-document: True
   function As_While_Loop_Spec (Node : Ada_Node'Class) return While_Loop_Spec;
      --% no-document: True
   function As_While_Loop_Stmt (Node : Ada_Node'Class) return While_Loop_Stmt;
      --% no-document: True
   function As_With_Clause (Node : Ada_Node'Class) return With_Clause;
      --% no-document: True
   function As_With_Private (Node : Ada_Node'Class) return With_Private;
      --% no-document: True
   function As_With_Private_Absent
     (Node : Ada_Node'Class) return With_Private_Absent;
      --% no-document: True
   function As_With_Private_Present
     (Node : Ada_Node'Class) return With_Private_Present;
      --% no-document: True

   function Hash (Node : Ada_Node) return Ada.Containers.Hash_Type;
   --  Generic hash function, to be used for nodes as keys in hash tables
   pragma Warnings (On, "defined after private extension");

private

   type Internal_Context_Access is
     access all Implementation.Analysis_Context_Type;
   type Internal_Unit_Access is access all Implementation.Analysis_Unit_Type;

   type Analysis_Context is new Ada.Finalization.Controlled with record
      Internal : Internal_Context_Access;
   end record;

   overriding procedure Initialize (Context : in out Analysis_Context);
   overriding procedure Adjust (Context : in out Analysis_Context);
   overriding procedure Finalize (Context : in out Analysis_Context);

   type Analysis_Unit is tagged record
      Internal : Internal_Unit_Access;

      Context : Analysis_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   No_Analysis_Context : constant Analysis_Context :=
     (Ada.Finalization.Controlled with Internal => null);
   No_Analysis_Unit : constant Analysis_Unit :=
     (Internal => null,
      Context  => (Ada.Finalization.Controlled with Internal => null));

   --------------------------
   -- AST nodes (internal) --
   --------------------------

   type Ada_Node is tagged record
      Internal   : Implementation.AST_Envs.Entity;
      Safety_Net : Implementation.Node_Safety_Net;
   end record;
   No_Ada_Node : constant Ada_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Expr is new Ada_Node with null record;
   No_Expr : constant Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Basic_Decl is new Ada_Node with null record;
   No_Basic_Decl : constant Basic_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abort_Node is new Ada_Node with null record;
   No_Abort_Node : constant Abort_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abort_Absent is new Abort_Node with null record;
   No_Abort_Absent : constant Abort_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abort_Present is new Abort_Node with null record;
   No_Abort_Present : constant Abort_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Stmt is new Ada_Node with null record;
   No_Stmt : constant Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Simple_Stmt is new Stmt with null record;
   No_Simple_Stmt : constant Simple_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abort_Stmt is new Simple_Stmt with null record;
   No_Abort_Stmt : constant Abort_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abstract_Node is new Ada_Node with null record;
   No_Abstract_Node : constant Abstract_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abstract_Absent is new Abstract_Node with null record;
   No_Abstract_Absent : constant Abstract_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Basic_Subp_Decl is new Basic_Decl with null record;
   No_Basic_Subp_Decl : constant Basic_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Classic_Subp_Decl is new Basic_Subp_Decl with null record;
   No_Classic_Subp_Decl : constant Classic_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Formal_Subp_Decl is new Classic_Subp_Decl with null record;
   No_Formal_Subp_Decl : constant Formal_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abstract_Formal_Subp_Decl is new Formal_Subp_Decl with null record;
   No_Abstract_Formal_Subp_Decl : constant Abstract_Formal_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abstract_Present is new Abstract_Node with null record;
   No_Abstract_Present : constant Abstract_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Abstract_Subp_Decl is new Classic_Subp_Decl with null record;
   No_Abstract_Subp_Decl : constant Abstract_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Composite_Stmt is new Stmt with null record;
   No_Composite_Stmt : constant Composite_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Accept_Stmt is new Composite_Stmt with null record;
   No_Accept_Stmt : constant Accept_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Accept_Stmt_With_Stmts is new Accept_Stmt with null record;
   No_Accept_Stmt_With_Stmts : constant Accept_Stmt_With_Stmts :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Type_Def is new Ada_Node with null record;
   No_Type_Def : constant Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Access_Def is new Type_Def with null record;
   No_Access_Def : constant Access_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Access_To_Subp_Def is new Access_Def with null record;
   No_Access_To_Subp_Def : constant Access_To_Subp_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Ada_List is new Ada_Node with null record;
   No_Ada_List : constant Ada_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Ada_Node_List is new Ada_List with null record;
   No_Ada_Node_List : constant Ada_Node_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Aggregate is new Expr with null record;
   No_Base_Aggregate : constant Base_Aggregate :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aggregate is new Base_Aggregate with null record;
   No_Aggregate : constant Aggregate :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Basic_Assoc is new Ada_Node with null record;
   No_Basic_Assoc : constant Basic_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aggregate_Assoc is new Basic_Assoc with null record;
   No_Aggregate_Assoc : constant Aggregate_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aliased_Node is new Ada_Node with null record;
   No_Aliased_Node : constant Aliased_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aliased_Absent is new Aliased_Node with null record;
   No_Aliased_Absent : constant Aliased_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aliased_Present is new Aliased_Node with null record;
   No_Aliased_Present : constant Aliased_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type All_Node is new Ada_Node with null record;
   No_All_Node : constant All_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type All_Absent is new All_Node with null record;
   No_All_Absent : constant All_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type All_Present is new All_Node with null record;
   No_All_Present : constant All_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Allocator is new Expr with null record;
   No_Allocator : constant Allocator :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Alternatives_List is new Ada_Node_List with null record;
   No_Alternatives_List : constant Alternatives_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Object_Decl is new Basic_Decl with null record;
   No_Object_Decl : constant Object_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Anonymous_Object_Decl is new Object_Decl with null record;
   No_Anonymous_Object_Decl : constant Anonymous_Object_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Type_Expr is new Ada_Node with null record;
   No_Type_Expr : constant Type_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Anonymous_Type is new Type_Expr with null record;
   No_Anonymous_Type : constant Anonymous_Type :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Type_Access_Def is new Access_Def with null record;
   No_Base_Type_Access_Def : constant Base_Type_Access_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Anonymous_Type_Access_Def is new Base_Type_Access_Def with null record;
   No_Anonymous_Type_Access_Def : constant Anonymous_Type_Access_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Type_Decl is new Basic_Decl with null record;
   No_Base_Type_Decl : constant Base_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Type_Decl is new Base_Type_Decl with null record;
   No_Type_Decl : constant Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Anonymous_Type_Decl is new Type_Decl with null record;
   No_Anonymous_Type_Decl : constant Anonymous_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Array_Indices is new Ada_Node with null record;
   No_Array_Indices : constant Array_Indices :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Array_Type_Def is new Type_Def with null record;
   No_Array_Type_Def : constant Array_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aspect_Assoc is new Ada_Node with null record;
   No_Aspect_Assoc : constant Aspect_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aspect_Assoc_List is new Ada_List with null record;
   No_Aspect_Assoc_List : constant Aspect_Assoc_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aspect_Clause is new Ada_Node with null record;
   No_Aspect_Clause : constant Aspect_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Aspect_Spec is new Ada_Node with null record;
   No_Aspect_Spec : constant Aspect_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Assign_Stmt is new Simple_Stmt with null record;
   No_Assign_Stmt : constant Assign_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Basic_Assoc_List is new Ada_List with null record;
   No_Basic_Assoc_List : constant Basic_Assoc_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Assoc_List is new Basic_Assoc_List with null record;
   No_Assoc_List : constant Assoc_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type At_Clause is new Aspect_Clause with null record;
   No_At_Clause : constant At_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Attribute_Def_Clause is new Aspect_Clause with null record;
   No_Attribute_Def_Clause : constant Attribute_Def_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Name is new Expr with null record;
   No_Name : constant Name :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Attribute_Ref is new Name with null record;
   No_Attribute_Ref : constant Attribute_Ref :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Assoc is new Ada_Node with null record;
   No_Base_Assoc : constant Base_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Assoc_List is new Ada_List with null record;
   No_Base_Assoc_List : constant Base_Assoc_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Formal_Param_Decl is new Basic_Decl with null record;
   No_Base_Formal_Param_Decl : constant Base_Formal_Param_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Formal_Param_Holder is new Ada_Node with null record;
   No_Base_Formal_Param_Holder : constant Base_Formal_Param_Holder :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Single_Tok_Node is new Name with null record;
   No_Single_Tok_Node : constant Single_Tok_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Id is new Single_Tok_Node with null record;
   No_Base_Id : constant Base_Id :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Loop_Stmt is new Composite_Stmt with null record;
   No_Base_Loop_Stmt : constant Base_Loop_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Package_Decl is new Basic_Decl with null record;
   No_Base_Package_Decl : constant Base_Package_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Record_Def is new Ada_Node with null record;
   No_Base_Record_Def : constant Base_Record_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Body_Node is new Basic_Decl with null record;
   No_Body_Node : constant Body_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Subp_Body is new Body_Node with null record;
   No_Base_Subp_Body : constant Base_Subp_Body :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Subp_Spec is new Base_Formal_Param_Holder with null record;
   No_Base_Subp_Spec : constant Base_Subp_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Base_Subtype_Decl is new Base_Type_Decl with null record;
   No_Base_Subtype_Decl : constant Base_Subtype_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Block_Stmt is new Composite_Stmt with null record;
   No_Block_Stmt : constant Block_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Begin_Block is new Block_Stmt with null record;
   No_Begin_Block : constant Begin_Block :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Bin_Op is new Expr with null record;
   No_Bin_Op : constant Bin_Op :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Body_Stub is new Body_Node with null record;
   No_Body_Stub : constant Body_Stub :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Box_Expr is new Expr with null record;
   No_Box_Expr : constant Box_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Call_Expr is new Name with null record;
   No_Call_Expr : constant Call_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Call_Stmt is new Simple_Stmt with null record;
   No_Call_Stmt : constant Call_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Case_Expr is new Expr with null record;
   No_Case_Expr : constant Case_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Case_Expr_Alternative is new Expr with null record;
   No_Case_Expr_Alternative : constant Case_Expr_Alternative :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Case_Expr_Alternative_List is new Ada_List with null record;
   No_Case_Expr_Alternative_List : constant Case_Expr_Alternative_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Case_Stmt is new Composite_Stmt with null record;
   No_Case_Stmt : constant Case_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Case_Stmt_Alternative is new Ada_Node with null record;
   No_Case_Stmt_Alternative : constant Case_Stmt_Alternative :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Case_Stmt_Alternative_List is new Ada_List with null record;
   No_Case_Stmt_Alternative_List : constant Case_Stmt_Alternative_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Char_Literal is new Base_Id with null record;
   No_Char_Literal : constant Char_Literal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Classwide_Type_Decl is new Base_Type_Decl with null record;
   No_Classwide_Type_Decl : constant Classwide_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Compilation_Unit is new Ada_Node with null record;
   No_Compilation_Unit : constant Compilation_Unit :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Compilation_Unit_List is new Ada_List with null record;
   No_Compilation_Unit_List : constant Compilation_Unit_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Component_Clause is new Ada_Node with null record;
   No_Component_Clause : constant Component_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Component_Decl is new Base_Formal_Param_Decl with null record;
   No_Component_Decl : constant Component_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Component_Def is new Ada_Node with null record;
   No_Component_Def : constant Component_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Component_List is new Base_Formal_Param_Holder with null record;
   No_Component_List : constant Component_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Concrete_Formal_Subp_Decl is new Formal_Subp_Decl with null record;
   No_Concrete_Formal_Subp_Decl : constant Concrete_Formal_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constant_Node is new Ada_Node with null record;
   No_Constant_Node : constant Constant_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constant_Absent is new Constant_Node with null record;
   No_Constant_Absent : constant Constant_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constant_Present is new Constant_Node with null record;
   No_Constant_Present : constant Constant_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constrained_Array_Indices is new Array_Indices with null record;
   No_Constrained_Array_Indices : constant Constrained_Array_Indices :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subtype_Indication is new Type_Expr with null record;
   No_Subtype_Indication : constant Subtype_Indication :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constrained_Subtype_Indication is new Subtype_Indication with
   null record;
   No_Constrained_Subtype_Indication : constant Constrained_Subtype_Indication :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constraint is new Ada_Node with null record;
   No_Constraint : constant Constraint :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Constraint_List is new Ada_Node_List with null record;
   No_Constraint_List : constant Constraint_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Contract_Case_Assoc is new Base_Assoc with null record;
   No_Contract_Case_Assoc : constant Contract_Case_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Contract_Case_Assoc_List is new Ada_List with null record;
   No_Contract_Case_Assoc_List : constant Contract_Case_Assoc_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Contract_Cases is new Expr with null record;
   No_Contract_Cases : constant Contract_Cases :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Real_Type_Def is new Type_Def with null record;
   No_Real_Type_Def : constant Real_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Decimal_Fixed_Point_Def is new Real_Type_Def with null record;
   No_Decimal_Fixed_Point_Def : constant Decimal_Fixed_Point_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Decl_Block is new Block_Stmt with null record;
   No_Decl_Block : constant Decl_Block :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Decl_List is new Ada_Node_List with null record;
   No_Decl_List : constant Decl_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Declarative_Part is new Ada_Node with null record;
   No_Declarative_Part : constant Declarative_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Defining_Name is new Name with null record;
   No_Defining_Name : constant Defining_Name :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Defining_Name_List is new Ada_List with null record;
   No_Defining_Name_List : constant Defining_Name_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Delay_Stmt is new Simple_Stmt with null record;
   No_Delay_Stmt : constant Delay_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Delta_Constraint is new Constraint with null record;
   No_Delta_Constraint : constant Delta_Constraint :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Derived_Type_Def is new Type_Def with null record;
   No_Derived_Type_Def : constant Derived_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Digits_Constraint is new Constraint with null record;
   No_Digits_Constraint : constant Digits_Constraint :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discrete_Base_Subtype_Decl is new Base_Subtype_Decl with null record;
   No_Discrete_Base_Subtype_Decl : constant Discrete_Base_Subtype_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discrete_Subtype_Indication is new Subtype_Indication with null record;
   No_Discrete_Subtype_Indication : constant Discrete_Subtype_Indication :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discrete_Subtype_Name is new Name with null record;
   No_Discrete_Subtype_Name : constant Discrete_Subtype_Name :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discriminant_Assoc is new Basic_Assoc with null record;
   No_Discriminant_Assoc : constant Discriminant_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Identifier_List is new Ada_List with null record;
   No_Identifier_List : constant Identifier_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discriminant_Choice_List is new Identifier_List with null record;
   No_Discriminant_Choice_List : constant Discriminant_Choice_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discriminant_Constraint is new Constraint with null record;
   No_Discriminant_Constraint : constant Discriminant_Constraint :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discriminant_Part is new Base_Formal_Param_Holder with null record;
   No_Discriminant_Part : constant Discriminant_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discriminant_Spec is new Base_Formal_Param_Decl with null record;
   No_Discriminant_Spec : constant Discriminant_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Discriminant_Spec_List is new Ada_List with null record;
   No_Discriminant_Spec_List : constant Discriminant_Spec_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Dotted_Name is new Name with null record;
   No_Dotted_Name : constant Dotted_Name :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Elsif_Expr_Part is new Ada_Node with null record;
   No_Elsif_Expr_Part : constant Elsif_Expr_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Elsif_Expr_Part_List is new Ada_List with null record;
   No_Elsif_Expr_Part_List : constant Elsif_Expr_Part_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Elsif_Stmt_Part is new Ada_Node with null record;
   No_Elsif_Stmt_Part : constant Elsif_Stmt_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Elsif_Stmt_Part_List is new Ada_List with null record;
   No_Elsif_Stmt_Part_List : constant Elsif_Stmt_Part_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type End_Name is new Name with null record;
   No_End_Name : constant End_Name :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Entry_Body is new Body_Node with null record;
   No_Entry_Body : constant Entry_Body :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Entry_Completion_Formal_Params is new Base_Formal_Param_Holder with
   null record;
   No_Entry_Completion_Formal_Params : constant Entry_Completion_Formal_Params :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Entry_Decl is new Basic_Subp_Decl with null record;
   No_Entry_Decl : constant Entry_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Entry_Index_Spec is new Basic_Decl with null record;
   No_Entry_Index_Spec : constant Entry_Index_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Entry_Spec is new Base_Subp_Spec with null record;
   No_Entry_Spec : constant Entry_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Enum_Lit_Synth_Type_Expr is new Type_Expr with null record;
   No_Enum_Lit_Synth_Type_Expr : constant Enum_Lit_Synth_Type_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Enum_Literal_Decl is new Basic_Subp_Decl with null record;
   No_Enum_Literal_Decl : constant Enum_Literal_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Enum_Literal_Decl_List is new Ada_List with null record;
   No_Enum_Literal_Decl_List : constant Enum_Literal_Decl_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Enum_Rep_Clause is new Aspect_Clause with null record;
   No_Enum_Rep_Clause : constant Enum_Rep_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Enum_Subp_Spec is new Base_Subp_Spec with null record;
   No_Enum_Subp_Spec : constant Enum_Subp_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Enum_Type_Def is new Type_Def with null record;
   No_Enum_Type_Def : constant Enum_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Error_Decl is new Basic_Decl with null record;
   No_Error_Decl : constant Error_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Error_Stmt is new Stmt with null record;
   No_Error_Stmt : constant Error_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Exception_Decl is new Basic_Decl with null record;
   No_Exception_Decl : constant Exception_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Exception_Handler is new Basic_Decl with null record;
   No_Exception_Handler : constant Exception_Handler :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Exit_Stmt is new Simple_Stmt with null record;
   No_Exit_Stmt : constant Exit_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Explicit_Deref is new Name with null record;
   No_Explicit_Deref : constant Explicit_Deref :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Expr_List is new Ada_List with null record;
   No_Expr_List : constant Expr_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Expr_Alternatives_List is new Expr_List with null record;
   No_Expr_Alternatives_List : constant Expr_Alternatives_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Expr_Function is new Base_Subp_Body with null record;
   No_Expr_Function : constant Expr_Function :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Extended_Return_Stmt is new Composite_Stmt with null record;
   No_Extended_Return_Stmt : constant Extended_Return_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Extended_Return_Stmt_Object_Decl is new Object_Decl with null record;
   No_Extended_Return_Stmt_Object_Decl : constant Extended_Return_Stmt_Object_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Floating_Point_Def is new Real_Type_Def with null record;
   No_Floating_Point_Def : constant Floating_Point_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Loop_Spec is new Ada_Node with null record;
   No_Loop_Spec : constant Loop_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type For_Loop_Spec is new Loop_Spec with null record;
   No_For_Loop_Spec : constant For_Loop_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type For_Loop_Stmt is new Base_Loop_Stmt with null record;
   No_For_Loop_Stmt : constant For_Loop_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type For_Loop_Var_Decl is new Basic_Decl with null record;
   No_For_Loop_Var_Decl : constant For_Loop_Var_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Formal_Discrete_Type_Def is new Type_Def with null record;
   No_Formal_Discrete_Type_Def : constant Formal_Discrete_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Decl is new Basic_Decl with null record;
   No_Generic_Decl : constant Generic_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Formal is new Base_Formal_Param_Decl with null record;
   No_Generic_Formal : constant Generic_Formal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Formal_Obj_Decl is new Generic_Formal with null record;
   No_Generic_Formal_Obj_Decl : constant Generic_Formal_Obj_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Formal_Package is new Generic_Formal with null record;
   No_Generic_Formal_Package : constant Generic_Formal_Package :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Formal_Part is new Base_Formal_Param_Holder with null record;
   No_Generic_Formal_Part : constant Generic_Formal_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Formal_Subp_Decl is new Generic_Formal with null record;
   No_Generic_Formal_Subp_Decl : constant Generic_Formal_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Formal_Type_Decl is new Generic_Formal with null record;
   No_Generic_Formal_Type_Decl : constant Generic_Formal_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Instantiation is new Basic_Decl with null record;
   No_Generic_Instantiation : constant Generic_Instantiation :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Package_Decl is new Generic_Decl with null record;
   No_Generic_Package_Decl : constant Generic_Package_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Package_Instantiation is new Generic_Instantiation with
   null record;
   No_Generic_Package_Instantiation : constant Generic_Package_Instantiation :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Package_Internal is new Base_Package_Decl with null record;
   No_Generic_Package_Internal : constant Generic_Package_Internal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Renaming_Decl is new Basic_Decl with null record;
   No_Generic_Renaming_Decl : constant Generic_Renaming_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Package_Renaming_Decl is new Generic_Renaming_Decl with
   null record;
   No_Generic_Package_Renaming_Decl : constant Generic_Package_Renaming_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Subp_Decl is new Generic_Decl with null record;
   No_Generic_Subp_Decl : constant Generic_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Subp_Instantiation is new Generic_Instantiation with
   null record;
   No_Generic_Subp_Instantiation : constant Generic_Subp_Instantiation :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Subp_Internal is new Basic_Subp_Decl with null record;
   No_Generic_Subp_Internal : constant Generic_Subp_Internal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Generic_Subp_Renaming_Decl is new Generic_Renaming_Decl with
   null record;
   No_Generic_Subp_Renaming_Decl : constant Generic_Subp_Renaming_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Goto_Stmt is new Simple_Stmt with null record;
   No_Goto_Stmt : constant Goto_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Handled_Stmts is new Ada_Node with null record;
   No_Handled_Stmts : constant Handled_Stmts :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Identifier is new Base_Id with null record;
   No_Identifier : constant Identifier :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type If_Expr is new Expr with null record;
   No_If_Expr : constant If_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type If_Stmt is new Composite_Stmt with null record;
   No_If_Stmt : constant If_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Incomplete_Type_Decl is new Base_Type_Decl with null record;
   No_Incomplete_Type_Decl : constant Incomplete_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Incomplete_Tagged_Type_Decl is new Incomplete_Type_Decl with
   null record;
   No_Incomplete_Tagged_Type_Decl : constant Incomplete_Tagged_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Index_Constraint is new Constraint with null record;
   No_Index_Constraint : constant Index_Constraint :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Num_Literal is new Single_Tok_Node with null record;
   No_Num_Literal : constant Num_Literal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Int_Literal is new Num_Literal with null record;
   No_Int_Literal : constant Int_Literal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Interface_Kind is new Ada_Node with null record;
   No_Interface_Kind : constant Interface_Kind :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Interface_Kind_Limited is new Interface_Kind with null record;
   No_Interface_Kind_Limited : constant Interface_Kind_Limited :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Interface_Kind_Protected is new Interface_Kind with null record;
   No_Interface_Kind_Protected : constant Interface_Kind_Protected :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Interface_Kind_Synchronized is new Interface_Kind with null record;
   No_Interface_Kind_Synchronized : constant Interface_Kind_Synchronized :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Interface_Kind_Task is new Interface_Kind with null record;
   No_Interface_Kind_Task : constant Interface_Kind_Task :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Interface_Type_Def is new Type_Def with null record;
   No_Interface_Type_Def : constant Interface_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Iter_Type is new Ada_Node with null record;
   No_Iter_Type : constant Iter_Type :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Iter_Type_In is new Iter_Type with null record;
   No_Iter_Type_In : constant Iter_Type_In :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Iter_Type_Of is new Iter_Type with null record;
   No_Iter_Type_Of : constant Iter_Type_Of :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Known_Discriminant_Part is new Discriminant_Part with null record;
   No_Known_Discriminant_Part : constant Known_Discriminant_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Label is new Simple_Stmt with null record;
   No_Label : constant Label :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Label_Decl is new Basic_Decl with null record;
   No_Label_Decl : constant Label_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Library_Item is new Ada_Node with null record;
   No_Library_Item : constant Library_Item :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Limited_Node is new Ada_Node with null record;
   No_Limited_Node : constant Limited_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Limited_Absent is new Limited_Node with null record;
   No_Limited_Absent : constant Limited_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Limited_Present is new Limited_Node with null record;
   No_Limited_Present : constant Limited_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Loop_Stmt is new Base_Loop_Stmt with null record;
   No_Loop_Stmt : constant Loop_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Membership_Expr is new Expr with null record;
   No_Membership_Expr : constant Membership_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Mod_Int_Type_Def is new Type_Def with null record;
   No_Mod_Int_Type_Def : constant Mod_Int_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Mode is new Ada_Node with null record;
   No_Mode : constant Mode :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Mode_Default is new Mode with null record;
   No_Mode_Default : constant Mode_Default :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Mode_In is new Mode with null record;
   No_Mode_In : constant Mode_In :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Mode_In_Out is new Mode with null record;
   No_Mode_In_Out : constant Mode_In_Out :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Mode_Out is new Mode with null record;
   No_Mode_Out : constant Mode_Out :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Multi_Dim_Array_Assoc is new Aggregate_Assoc with null record;
   No_Multi_Dim_Array_Assoc : constant Multi_Dim_Array_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Name_List is new Ada_List with null record;
   No_Name_List : constant Name_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Named_Stmt is new Composite_Stmt with null record;
   No_Named_Stmt : constant Named_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Named_Stmt_Decl is new Basic_Decl with null record;
   No_Named_Stmt_Decl : constant Named_Stmt_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Not_Null is new Ada_Node with null record;
   No_Not_Null : constant Not_Null :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Not_Null_Absent is new Not_Null with null record;
   No_Not_Null_Absent : constant Not_Null_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Not_Null_Present is new Not_Null with null record;
   No_Not_Null_Present : constant Not_Null_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Null_Component_Decl is new Ada_Node with null record;
   No_Null_Component_Decl : constant Null_Component_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Null_Literal is new Single_Tok_Node with null record;
   No_Null_Literal : constant Null_Literal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Null_Record_Aggregate is new Base_Aggregate with null record;
   No_Null_Record_Aggregate : constant Null_Record_Aggregate :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Null_Record_Def is new Base_Record_Def with null record;
   No_Null_Record_Def : constant Null_Record_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Null_Stmt is new Simple_Stmt with null record;
   No_Null_Stmt : constant Null_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Null_Subp_Decl is new Base_Subp_Body with null record;
   No_Null_Subp_Decl : constant Null_Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Number_Decl is new Basic_Decl with null record;
   No_Number_Decl : constant Number_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op is new Base_Id with null record;
   No_Op : constant Op :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Abs is new Op with null record;
   No_Op_Abs : constant Op_Abs :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_And is new Op with null record;
   No_Op_And : constant Op_And :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_And_Then is new Op with null record;
   No_Op_And_Then : constant Op_And_Then :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Concat is new Op with null record;
   No_Op_Concat : constant Op_Concat :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Div is new Op with null record;
   No_Op_Div : constant Op_Div :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Double_Dot is new Op with null record;
   No_Op_Double_Dot : constant Op_Double_Dot :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Eq is new Op with null record;
   No_Op_Eq : constant Op_Eq :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Gt is new Op with null record;
   No_Op_Gt : constant Op_Gt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Gte is new Op with null record;
   No_Op_Gte : constant Op_Gte :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_In is new Op with null record;
   No_Op_In : constant Op_In :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Lt is new Op with null record;
   No_Op_Lt : constant Op_Lt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Lte is new Op with null record;
   No_Op_Lte : constant Op_Lte :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Minus is new Op with null record;
   No_Op_Minus : constant Op_Minus :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Mod is new Op with null record;
   No_Op_Mod : constant Op_Mod :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Mult is new Op with null record;
   No_Op_Mult : constant Op_Mult :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Neq is new Op with null record;
   No_Op_Neq : constant Op_Neq :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Not is new Op with null record;
   No_Op_Not : constant Op_Not :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Not_In is new Op with null record;
   No_Op_Not_In : constant Op_Not_In :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Or is new Op with null record;
   No_Op_Or : constant Op_Or :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Or_Else is new Op with null record;
   No_Op_Or_Else : constant Op_Or_Else :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Plus is new Op with null record;
   No_Op_Plus : constant Op_Plus :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Pow is new Op with null record;
   No_Op_Pow : constant Op_Pow :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Rem is new Op with null record;
   No_Op_Rem : constant Op_Rem :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Op_Xor is new Op with null record;
   No_Op_Xor : constant Op_Xor :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Ordinary_Fixed_Point_Def is new Real_Type_Def with null record;
   No_Ordinary_Fixed_Point_Def : constant Ordinary_Fixed_Point_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Others_Designator is new Ada_Node with null record;
   No_Others_Designator : constant Others_Designator :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Overriding_Node is new Ada_Node with null record;
   No_Overriding_Node : constant Overriding_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Overriding_Not_Overriding is new Overriding_Node with null record;
   No_Overriding_Not_Overriding : constant Overriding_Not_Overriding :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Overriding_Overriding is new Overriding_Node with null record;
   No_Overriding_Overriding : constant Overriding_Overriding :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Overriding_Unspecified is new Overriding_Node with null record;
   No_Overriding_Unspecified : constant Overriding_Unspecified :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Package_Body is new Body_Node with null record;
   No_Package_Body : constant Package_Body :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Package_Body_Stub is new Body_Stub with null record;
   No_Package_Body_Stub : constant Package_Body_Stub :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Package_Decl is new Base_Package_Decl with null record;
   No_Package_Decl : constant Package_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Package_Renaming_Decl is new Basic_Decl with null record;
   No_Package_Renaming_Decl : constant Package_Renaming_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Param_Assoc is new Basic_Assoc with null record;
   No_Param_Assoc : constant Param_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Param_Spec is new Base_Formal_Param_Decl with null record;
   No_Param_Spec : constant Param_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Param_Spec_List is new Ada_List with null record;
   No_Param_Spec_List : constant Param_Spec_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Params is new Ada_Node with null record;
   No_Params : constant Params :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Paren_Expr is new Expr with null record;
   No_Paren_Expr : constant Paren_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Parent_List is new Name_List with null record;
   No_Parent_List : constant Parent_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Pragma_Argument_Assoc is new Base_Assoc with null record;
   No_Pragma_Argument_Assoc : constant Pragma_Argument_Assoc :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Pragma_Node is new Ada_Node with null record;
   No_Pragma_Node : constant Pragma_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Pragma_Node_List is new Ada_List with null record;
   No_Pragma_Node_List : constant Pragma_Node_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Prim_Type_Accessor is new Ada_Node with null record;
   No_Prim_Type_Accessor : constant Prim_Type_Accessor :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Private_Node is new Ada_Node with null record;
   No_Private_Node : constant Private_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Private_Absent is new Private_Node with null record;
   No_Private_Absent : constant Private_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Private_Part is new Declarative_Part with null record;
   No_Private_Part : constant Private_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Private_Present is new Private_Node with null record;
   No_Private_Present : constant Private_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Private_Type_Def is new Type_Def with null record;
   No_Private_Type_Def : constant Private_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Node is new Ada_Node with null record;
   No_Protected_Node : constant Protected_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Absent is new Protected_Node with null record;
   No_Protected_Absent : constant Protected_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Body is new Body_Node with null record;
   No_Protected_Body : constant Protected_Body :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Body_Stub is new Body_Stub with null record;
   No_Protected_Body_Stub : constant Protected_Body_Stub :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Def is new Ada_Node with null record;
   No_Protected_Def : constant Protected_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Present is new Protected_Node with null record;
   No_Protected_Present : constant Protected_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Protected_Type_Decl is new Base_Type_Decl with null record;
   No_Protected_Type_Decl : constant Protected_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Public_Part is new Declarative_Part with null record;
   No_Public_Part : constant Public_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Qual_Expr is new Name with null record;
   No_Qual_Expr : constant Qual_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Quantified_Expr is new Expr with null record;
   No_Quantified_Expr : constant Quantified_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Quantifier is new Ada_Node with null record;
   No_Quantifier : constant Quantifier :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Quantifier_All is new Quantifier with null record;
   No_Quantifier_All : constant Quantifier_All :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Quantifier_Some is new Quantifier with null record;
   No_Quantifier_Some : constant Quantifier_Some :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Raise_Expr is new Expr with null record;
   No_Raise_Expr : constant Raise_Expr :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Raise_Stmt is new Simple_Stmt with null record;
   No_Raise_Stmt : constant Raise_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Range_Constraint is new Constraint with null record;
   No_Range_Constraint : constant Range_Constraint :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Range_Spec is new Ada_Node with null record;
   No_Range_Spec : constant Range_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Real_Literal is new Num_Literal with null record;
   No_Real_Literal : constant Real_Literal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Record_Def is new Base_Record_Def with null record;
   No_Record_Def : constant Record_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Record_Rep_Clause is new Aspect_Clause with null record;
   No_Record_Rep_Clause : constant Record_Rep_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Record_Type_Def is new Type_Def with null record;
   No_Record_Type_Def : constant Record_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Relation_Op is new Bin_Op with null record;
   No_Relation_Op : constant Relation_Op :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Renaming_Clause is new Ada_Node with null record;
   No_Renaming_Clause : constant Renaming_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Requeue_Stmt is new Simple_Stmt with null record;
   No_Requeue_Stmt : constant Requeue_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Return_Stmt is new Simple_Stmt with null record;
   No_Return_Stmt : constant Return_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Reverse_Node is new Ada_Node with null record;
   No_Reverse_Node : constant Reverse_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Reverse_Absent is new Reverse_Node with null record;
   No_Reverse_Absent : constant Reverse_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Reverse_Present is new Reverse_Node with null record;
   No_Reverse_Present : constant Reverse_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Select_Stmt is new Composite_Stmt with null record;
   No_Select_Stmt : constant Select_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Select_When_Part is new Ada_Node with null record;
   No_Select_When_Part : constant Select_When_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Select_When_Part_List is new Ada_List with null record;
   No_Select_When_Part_List : constant Select_When_Part_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Signed_Int_Type_Def is new Type_Def with null record;
   No_Signed_Int_Type_Def : constant Signed_Int_Type_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Single_Protected_Decl is new Basic_Decl with null record;
   No_Single_Protected_Decl : constant Single_Protected_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Single_Task_Decl is new Basic_Decl with null record;
   No_Single_Task_Decl : constant Single_Task_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Task_Type_Decl is new Base_Type_Decl with null record;
   No_Task_Type_Decl : constant Task_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Single_Task_Type_Decl is new Task_Type_Decl with null record;
   No_Single_Task_Type_Decl : constant Single_Task_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Stmt_List is new Ada_Node_List with null record;
   No_Stmt_List : constant Stmt_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type String_Literal is new Base_Id with null record;
   No_String_Literal : constant String_Literal :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Body is new Base_Subp_Body with null record;
   No_Subp_Body : constant Subp_Body :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Body_Stub is new Body_Stub with null record;
   No_Subp_Body_Stub : constant Subp_Body_Stub :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Decl is new Classic_Subp_Decl with null record;
   No_Subp_Decl : constant Subp_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Kind is new Ada_Node with null record;
   No_Subp_Kind : constant Subp_Kind :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Kind_Function is new Subp_Kind with null record;
   No_Subp_Kind_Function : constant Subp_Kind_Function :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Kind_Procedure is new Subp_Kind with null record;
   No_Subp_Kind_Procedure : constant Subp_Kind_Procedure :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Renaming_Decl is new Base_Subp_Body with null record;
   No_Subp_Renaming_Decl : constant Subp_Renaming_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subp_Spec is new Base_Subp_Spec with null record;
   No_Subp_Spec : constant Subp_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subtype_Decl is new Base_Subtype_Decl with null record;
   No_Subtype_Decl : constant Subtype_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Subunit is new Ada_Node with null record;
   No_Subunit : constant Subunit :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Synchronized_Node is new Ada_Node with null record;
   No_Synchronized_Node : constant Synchronized_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Synchronized_Absent is new Synchronized_Node with null record;
   No_Synchronized_Absent : constant Synchronized_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Synchronized_Present is new Synchronized_Node with null record;
   No_Synchronized_Present : constant Synchronized_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Synth_Anonymous_Type_Decl is new Anonymous_Type_Decl with null record;
   No_Synth_Anonymous_Type_Decl : constant Synth_Anonymous_Type_Decl :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Synthetic_Renaming_Clause is new Renaming_Clause with null record;
   No_Synthetic_Renaming_Clause : constant Synthetic_Renaming_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Tagged_Node is new Ada_Node with null record;
   No_Tagged_Node : constant Tagged_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Tagged_Absent is new Tagged_Node with null record;
   No_Tagged_Absent : constant Tagged_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Tagged_Present is new Tagged_Node with null record;
   No_Tagged_Present : constant Tagged_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Target_Name is new Name with null record;
   No_Target_Name : constant Target_Name :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Task_Body is new Body_Node with null record;
   No_Task_Body : constant Task_Body :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Task_Body_Stub is new Body_Stub with null record;
   No_Task_Body_Stub : constant Task_Body_Stub :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Task_Def is new Ada_Node with null record;
   No_Task_Def : constant Task_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Terminate_Alternative is new Simple_Stmt with null record;
   No_Terminate_Alternative : constant Terminate_Alternative :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Type_Access_Def is new Base_Type_Access_Def with null record;
   No_Type_Access_Def : constant Type_Access_Def :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Un_Op is new Expr with null record;
   No_Un_Op : constant Un_Op :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Unconstrained_Array_Index is new Ada_Node with null record;
   No_Unconstrained_Array_Index : constant Unconstrained_Array_Index :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Unconstrained_Array_Index_List is new Ada_List with null record;
   No_Unconstrained_Array_Index_List : constant Unconstrained_Array_Index_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Unconstrained_Array_Indices is new Array_Indices with null record;
   No_Unconstrained_Array_Indices : constant Unconstrained_Array_Indices :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Unknown_Discriminant_Part is new Discriminant_Part with null record;
   No_Unknown_Discriminant_Part : constant Unknown_Discriminant_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Until_Node is new Ada_Node with null record;
   No_Until_Node : constant Until_Node :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Until_Absent is new Until_Node with null record;
   No_Until_Absent : constant Until_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Until_Present is new Until_Node with null record;
   No_Until_Present : constant Until_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Update_Attribute_Ref is new Attribute_Ref with null record;
   No_Update_Attribute_Ref : constant Update_Attribute_Ref :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Use_Clause is new Ada_Node with null record;
   No_Use_Clause : constant Use_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Use_Package_Clause is new Use_Clause with null record;
   No_Use_Package_Clause : constant Use_Package_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Use_Type_Clause is new Use_Clause with null record;
   No_Use_Type_Clause : constant Use_Type_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Variant is new Ada_Node with null record;
   No_Variant : constant Variant :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Variant_List is new Ada_List with null record;
   No_Variant_List : constant Variant_List :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type Variant_Part is new Ada_Node with null record;
   No_Variant_Part : constant Variant_Part :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type While_Loop_Spec is new Loop_Spec with null record;
   No_While_Loop_Spec : constant While_Loop_Spec :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type While_Loop_Stmt is new Base_Loop_Stmt with null record;
   No_While_Loop_Stmt : constant While_Loop_Stmt :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type With_Clause is new Ada_Node with null record;
   No_With_Clause : constant With_Clause :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type With_Private is new Ada_Node with null record;
   No_With_Private : constant With_Private :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type With_Private_Absent is new With_Private with null record;
   No_With_Private_Absent : constant With_Private_Absent :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);
   type With_Private_Present is new With_Private with null record;
   No_With_Private_Present : constant With_Private_Present :=
     (Internal   => Implementation.No_Entity,
      Safety_Net => Implementation.No_Node_Safety_Net);

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : Ada_Node;
      Last : Token_Index;
   end record;

   ---------------------------------
   -- Composite types (internals) --
   ---------------------------------

   type Text_Type_Access is access all Text_Type;
   procedure Free is new Ada.Unchecked_Deallocation
     (Text_Type, Text_Type_Access);

   type Internal_Aspect_Record is limited record
      Internal_Exists : Boolean;
      Internal_Node   : Ada_Node;
      Internal_Value  : Expr;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Aspect_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Aspect_Record; Count : Positive);
   procedure Release (Self : in out Internal_Aspect_Record) is null;

   package Boxed_Aspect is new Langkit_Support.Boxes
     (Internal_Aspect_Record, Refcount, Set_Refcount, Release);

   type Aspect is new Boxed_Aspect.Reference;

   type Internal_Completion_Item_Record is limited record
      Internal_Decl        : Basic_Decl;
      Internal_Is_Dot_Call : Boolean;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Completion_Item_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Completion_Item_Record; Count : Positive);
   procedure Release (Self : in out Internal_Completion_Item_Record) is null;

   package Boxed_Completion_Item is new Langkit_Support.Boxes
     (Internal_Completion_Item_Record, Refcount, Set_Refcount, Release);

   type Completion_Item is new Boxed_Completion_Item.Reference;

   type Internal_Discrete_Range_Record is limited record
      Internal_Low_Bound  : Expr;
      Internal_High_Bound : Expr;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Discrete_Range_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Discrete_Range_Record; Count : Positive);
   procedure Release (Self : in out Internal_Discrete_Range_Record) is null;

   package Boxed_Discrete_Range is new Langkit_Support.Boxes
     (Internal_Discrete_Range_Record, Refcount, Set_Refcount, Release);

   type Discrete_Range is new Boxed_Discrete_Range.Reference;

   type Internal_Doc_Annotation_Record is limited record
      Internal_Key   : Text_Type_Access;
      Internal_Value : Text_Type_Access;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Doc_Annotation_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Doc_Annotation_Record; Count : Positive);
   procedure Release (Self : in out Internal_Doc_Annotation_Record);

   package Boxed_Doc_Annotation is new Langkit_Support.Boxes
     (Internal_Doc_Annotation_Record, Refcount, Set_Refcount, Release);

   type Doc_Annotation is new Boxed_Doc_Annotation.Reference;

   type Internal_Param_Actual_Record is limited record
      Internal_Param  : Defining_Name;
      Internal_Actual : Expr;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Param_Actual_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Param_Actual_Record; Count : Positive);
   procedure Release (Self : in out Internal_Param_Actual_Record) is null;

   package Boxed_Param_Actual is new Langkit_Support.Boxes
     (Internal_Param_Actual_Record, Refcount, Set_Refcount, Release);

   type Param_Actual is new Boxed_Param_Actual.Reference;

   type Internal_Ref_Result_Record is limited record
      Internal_Ref  : Base_Id;
      Internal_Kind : Ref_Result_Kind;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Ref_Result_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Ref_Result_Record; Count : Positive);
   procedure Release (Self : in out Internal_Ref_Result_Record) is null;

   package Boxed_Ref_Result is new Langkit_Support.Boxes
     (Internal_Ref_Result_Record, Refcount, Set_Refcount, Release);

   type Ref_Result is new Boxed_Ref_Result.Reference;

   type Internal_Refd_Decl_Record is limited record
      Internal_Decl : Basic_Decl;
      Internal_Kind : Ref_Result_Kind;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Refd_Decl_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Refd_Decl_Record; Count : Positive);
   procedure Release (Self : in out Internal_Refd_Decl_Record) is null;

   package Boxed_Refd_Decl is new Langkit_Support.Boxes
     (Internal_Refd_Decl_Record, Refcount, Set_Refcount, Release);

   type Refd_Decl is new Boxed_Refd_Decl.Reference;

   type Internal_Refd_Def_Record is limited record
      Internal_Def_Name : Defining_Name;
      Internal_Kind     : Ref_Result_Kind;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Refd_Def_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Refd_Def_Record; Count : Positive);
   procedure Release (Self : in out Internal_Refd_Def_Record) is null;

   package Boxed_Refd_Def is new Langkit_Support.Boxes
     (Internal_Refd_Def_Record, Refcount, Set_Refcount, Release);

   type Refd_Def is new Boxed_Refd_Def.Reference;

   type Internal_Substitution_Record is limited record
      Internal_From_Decl  : Basic_Decl;
      Internal_To_Value   : Big_Integer;
      Internal_Value_Type : Base_Type_Decl;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Substitution_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Substitution_Record; Count : Positive);
   procedure Release (Self : in out Internal_Substitution_Record) is null;

   package Boxed_Substitution is new Langkit_Support.Boxes
     (Internal_Substitution_Record, Refcount, Set_Refcount, Release);

   type Substitution is new Boxed_Substitution.Reference;

   --  The dummy references to these packages forces them to be included in
   --  statically linked builds (thanks to the binder). This benefits the GDB
   --  helpers at no cost.

   Version : String renames Libadalang.Version;
   procedure RN (Node : Libadalang.Implementation.Bare_Ada_Node) renames
     Libadalang.Debug.PN;

end Libadalang.Analysis;
