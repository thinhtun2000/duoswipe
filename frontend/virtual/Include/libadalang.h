







#ifndef LIBADALANG
#define LIBADALANG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* This type represents a context for all source analysis. This is the first
   type you need to create to use libadalang. It will contain the results of
   all analysis, and is the main holder for all the data.

   You can create several analysis contexts if you need to, which enables you,
   for example to:

   * analyze several different projects at the same time;

   * analyze different parts of the same projects in parallel.

   In the current design, contexts always keep all of their analysis units
   allocated. If you need to get this memory released, the only option at your
   disposal is to destroy your analysis context instance.

   This structure is partially opaque: some fields are exposed to allow direct
   access, for performance concerns.  */
typedef struct
{
   uint64_t serial_number;
} *ada_analysis_context;

/* This type represents the analysis of a single file.

   This type has strong-reference semantics and is ref-counted. Furthermore, a
   reference to a unit contains an implicit reference to the context that owns
   it. This means that keeping a reference to a unit will keep the context and
   all the unit it contains allocated.

   This structure is partially opaque: some fields are exposed to allow direct
   access, for performance concerns.  */
typedef struct
{
   uint64_t version_number;
} *ada_analysis_unit;

/* Data type for all nodes. Nodes are assembled to make up a tree.  See the
   node primitives below to inspect such trees.

   Unlike for contexts and units, this type has weak-reference semantics:
   keeping a reference to a node has no effect on the decision to keep the unit
   that it owns allocated. This means that once all references to the context
   and units related to a node are dropped, the context and its units are
   deallocated and the node becomes a stale reference: most operations on it
   will raise a ``Stale_Reference_Error``.

   Note that since reparsing an analysis unit deallocates all the nodes it
   contains, this operation makes all reference to these nodes stale as
   well.   */
typedef void* ada_base_node;

/* Kind of AST nodes in parse trees.  */
typedef enum {
    

        /* ada_node (abstract)  */
        /* Root node class for the Ada syntax tree.  */
    

        /* abort_node (abstract)  */
        /* Qualifier for the ``abort`` keyword.  */
    

        
        ada_abort_absent = 1,
    

        
        ada_abort_present = 2,
    

        /* abstract_node (abstract)  */
        /* Qualifier for the ``abstract`` keyword.  */
    

        
        ada_abstract_absent = 3,
    

        
        ada_abstract_present = 4,
    

        /* ada_list (abstract)  */
        
    

        /* List of AdaNode.
        
           This list node can contain one of the following nodes:
        
           * ada_abstract_subp_decl
        
           * ada_allocator
        
           * ada_aspect_clause
        
           * ada_attribute_ref
        
           * ada_base_aggregate
        
           * ada_bin_op
        
           * ada_body_node
        
           * ada_call_expr
        
           * ada_case_expr
        
           * ada_char_literal
        
           * ada_component_clause
        
           * ada_component_decl
        
           * ada_dotted_name
        
           * ada_entry_decl
        
           * ada_error_decl
        
           * ada_exception_decl
        
           * ada_exception_handler
        
           * ada_explicit_deref
        
           * ada_generic_decl
        
           * ada_generic_formal
        
           * ada_generic_instantiation
        
           * ada_generic_renaming_decl
        
           * ada_identifier
        
           * ada_if_expr
        
           * ada_incomplete_type_decl
        
           * ada_membership_expr
        
           * ada_null_component_decl
        
           * ada_null_literal
        
           * ada_num_literal
        
           * ada_number_decl
        
           * ada_object_decl
        
           * ada_others_designator
        
           * ada_package_decl
        
           * ada_package_renaming_decl
        
           * ada_paren_expr
        
           * ada_pragma_node
        
           * ada_protected_type_decl
        
           * ada_qual_expr
        
           * ada_quantified_expr
        
           * ada_raise_expr
        
           * ada_single_protected_decl
        
           * ada_single_task_decl
        
           * ada_stmt
        
           * ada_string_literal
        
           * ada_subp_decl
        
           * ada_subtype_decl
        
           * ada_subtype_indication
        
           * ada_target_name
        
           * ada_task_type_decl
        
           * ada_type_decl
        
           * ada_un_op
        
           * ada_use_clause
        
           * ada_with_clause  */
        ada_ada_node_list = 5,
    

        /* List of alternatives in a ``when ...`` clause.
        
           This list node can contain one of the following nodes:
        
           * ada_allocator
        
           * ada_attribute_ref
        
           * ada_base_aggregate
        
           * ada_bin_op
        
           * ada_call_expr
        
           * ada_case_expr
        
           * ada_char_literal
        
           * ada_discrete_subtype_indication
        
           * ada_dotted_name
        
           * ada_explicit_deref
        
           * ada_identifier
        
           * ada_if_expr
        
           * ada_membership_expr
        
           * ada_null_literal
        
           * ada_num_literal
        
           * ada_others_designator
        
           * ada_paren_expr
        
           * ada_qual_expr
        
           * ada_quantified_expr
        
           * ada_raise_expr
        
           * ada_string_literal
        
           * ada_target_name
        
           * ada_un_op  */
        ada_alternatives_list = 6,
    

        /* List of constraints.
        
           This list node can contain one of the following nodes:
        
           * ada_attribute_ref
        
           * ada_bin_op
        
           * ada_call_expr
        
           * ada_char_literal
        
           * ada_dotted_name
        
           * ada_explicit_deref
        
           * ada_identifier
        
           * ada_qual_expr
        
           * ada_string_literal
        
           * ada_subtype_indication
        
           * ada_target_name  */
        ada_constraint_list = 7,
    

        /* List of declarations.
        
           This list node can contain one of the following nodes:
        
           * ada_abstract_subp_decl
        
           * ada_aspect_clause
        
           * ada_component_decl
        
           * ada_entry_decl
        
           * ada_expr_function
        
           * ada_null_subp_decl
        
           * ada_pragma_node
        
           * ada_subp_decl
        
           * ada_subp_renaming_decl  */
        ada_decl_list = 8,
    

        /* List of statements.
        
           This list node can contain one of the following nodes:
        
           * ada_pragma_node
        
           * ada_stmt  */
        ada_stmt_list = 9,
    

        /* List of AspectAssoc.  */
        ada_aspect_assoc_list = 10,
    

        /* List of BaseAssoc.  */
        ada_base_assoc_list = 11,
    

        /* basic_assoc_list (abstract)  */
        /* List of BasicAssoc.  */
    

        /* List of associations.  */
        ada_assoc_list = 12,
    

        /* List of CaseExprAlternative.  */
        ada_case_expr_alternative_list = 13,
    

        /* List of CaseStmtAlternative.  */
        ada_case_stmt_alternative_list = 14,
    

        /* List of CompilationUnit.  */
        ada_compilation_unit_list = 15,
    

        /* List of ContractCaseAssoc.  */
        ada_contract_case_assoc_list = 16,
    

        /* List of DefiningName.  */
        ada_defining_name_list = 17,
    

        /* List of DiscriminantSpec.  */
        ada_discriminant_spec_list = 18,
    

        /* List of ElsifExprPart.  */
        ada_elsif_expr_part_list = 19,
    

        /* List of ElsifStmtPart.  */
        ada_elsif_stmt_part_list = 20,
    

        /* List of EnumLiteralDecl.  */
        ada_enum_literal_decl_list = 21,
    

        /* expr_list (abstract)  */
        /* List of Expr.
        
           This list node can contain one of the following nodes:
        
           * ada_allocator
        
           * ada_attribute_ref
        
           * ada_base_aggregate
        
           * ada_bin_op
        
           * ada_call_expr
        
           * ada_case_expr
        
           * ada_char_literal
        
           * ada_discrete_subtype_name
        
           * ada_dotted_name
        
           * ada_explicit_deref
        
           * ada_identifier
        
           * ada_if_expr
        
           * ada_null_literal
        
           * ada_num_literal
        
           * ada_paren_expr
        
           * ada_qual_expr
        
           * ada_quantified_expr
        
           * ada_raise_expr
        
           * ada_string_literal
        
           * ada_target_name
        
           * ada_un_op  */
    

        /* List of alternatives in a membership test expression.
        
           This list node can contain one of the following nodes:
        
           * ada_allocator
        
           * ada_attribute_ref
        
           * ada_base_aggregate
        
           * ada_bin_op
        
           * ada_call_expr
        
           * ada_case_expr
        
           * ada_char_literal
        
           * ada_discrete_subtype_name
        
           * ada_dotted_name
        
           * ada_explicit_deref
        
           * ada_identifier
        
           * ada_if_expr
        
           * ada_null_literal
        
           * ada_num_literal
        
           * ada_paren_expr
        
           * ada_qual_expr
        
           * ada_quantified_expr
        
           * ada_raise_expr
        
           * ada_string_literal
        
           * ada_target_name
        
           * ada_un_op  */
        ada_expr_alternatives_list = 22,
    

        /* identifier_list (abstract)  */
        /* List of Identifier.  */
    

        /* List of discriminant associations.  */
        ada_discriminant_choice_list = 23,
    

        /* List of Name.
        
           This list node can contain one of the following nodes:
        
           * ada_attribute_ref
        
           * ada_call_expr
        
           * ada_char_literal
        
           * ada_dotted_name
        
           * ada_explicit_deref
        
           * ada_identifier
        
           * ada_qual_expr
        
           * ada_string_literal
        
           * ada_target_name  */
        ada_name_list = 24,
    

        /* List of parents in a type declaration.
        
           This list node can contain one of the following nodes:
        
           * ada_char_literal
        
           * ada_dotted_name
        
           * ada_identifier
        
           * ada_string_literal  */
        ada_parent_list = 25,
    

        /* List of ParamSpec.  */
        ada_param_spec_list = 26,
    

        /* List of Pragma.  */
        ada_pragma_node_list = 27,
    

        /* List of SelectWhenPart.  */
        ada_select_when_part_list = 28,
    

        /* List of UnconstrainedArrayIndex.  */
        ada_unconstrained_array_index_list = 29,
    

        /* List of Variant.  */
        ada_variant_list = 30,
    

        /* aliased_node (abstract)  */
        /* Qualifier for the ``aliased`` keyword.  */
    

        
        ada_aliased_absent = 31,
    

        
        ada_aliased_present = 32,
    

        /* all_node (abstract)  */
        /* Qualifier for the ``all`` keyword.  */
    

        
        ada_all_absent = 33,
    

        
        ada_all_present = 34,
    

        /* array_indices (abstract)  */
        /* Specification for array indexes.  */
    

        /* Constrained specification for array indexes.  */
        ada_constrained_array_indices = 35,
    

        /* Unconstrained specification for array indexes.  */
        ada_unconstrained_array_indices = 36,
    

        /* Name/expression association in an aspect.  */
        ada_aspect_assoc = 37,
    

        /* aspect_clause (abstract)  */
        /* Base class for aspect clauses.  */
    

        /* Representation clause (``for .. use at ...;``).  */
        ada_at_clause = 38,
    

        /* Clause for an attribute definition (``for ...'Attribute use
           ...;``).   */
        ada_attribute_def_clause = 39,
    

        /* Representation clause for enumeration types.  */
        ada_enum_rep_clause = 40,
    

        /* Representation clause for a record type.  */
        ada_record_rep_clause = 41,
    

        /* List of aspects in a declaration.  */
        ada_aspect_spec = 42,
    

        /* base_assoc (abstract)  */
        /* Abstract class for a key/value association, where the value is an
           expression.  */
    

        /* Single association for the ``Contract_Case`` aspect.  */
        ada_contract_case_assoc = 43,
    

        /* Argument assocation in a pragma.  */
        ada_pragma_argument_assoc = 44,
    

        /* base_formal_param_holder (abstract)  */
        /* Base class for lists of formal parameters. This is used both for
           subprogram specifications and for records, so that we can share the
           matching and unpacking logic.  */
    

        /* base_subp_spec (abstract)  */
        /* Base class for subprogram specifications.  */
    

        /* Entry specification.  */
        ada_entry_spec = 45,
    

        /* Synthetic node for the abstract subprogram spec of an enum literal.
        
           NOTE: This has no existence in the ARM. While enum literals are
           functions semantically, they're not such syntactically.  */
        ada_enum_subp_spec = 46,
    

        /* Subprogram specification.  */
        ada_subp_spec = 47,
    

        /* List of component declarations.  */
        ada_component_list = 48,
    

        /* discriminant_part (abstract)  */
        /* Specification for discriminants in type declarations.  */
    

        /* Known list of discriminants in type declarations.  */
        ada_known_discriminant_part = 49,
    

        /* Unknown list of discriminants in type declarations.  */
        ada_unknown_discriminant_part = 50,
    

        /* Formal parameters for the completion of an ``EntryDecl`` (either an
           ``EntryBody`` or an ``AcceptStmt``).  */
        ada_entry_completion_formal_params = 51,
    

        /* List of declaration for generic formals.  */
        ada_generic_formal_part = 52,
    

        /* base_record_def (abstract)  */
        /* Base class for record definitions.  */
    

        /* Record definition for ``null record``.  */
        ada_null_record_def = 53,
    

        /* Record definition that contains components (``record ... end
           record``).  */
        ada_record_def = 54,
    

        /* basic_assoc (abstract)  */
        /* Association of one or several names to an expression.  */
    

        /* Assocation (X => Y) used for aggregates and parameter
           associations.   */
        ada_aggregate_assoc = 55,
    

        /* Association used for multi-dimension array aggregates.  */
        ada_multi_dim_array_assoc = 56,
    

        /* Association of discriminant names to an expression.  */
        ada_discriminant_assoc = 57,
    

        /* Assocation (X => Y) used for aggregates and parameter
           associations.   */
        ada_param_assoc = 58,
    

        /* basic_decl (abstract)  */
        /* Root class for an Ada declaration (RM 3.1). A declaration associates
           a name with a language entity, for example a type or a variable.  */
    

        /* base_formal_param_decl (abstract)  */
        /* Base class for formal parameter declarations. This is used both for
           records components and for subprogram parameters.
        
           This is a Libadalang abstraction, that has no ARM existence.  */
    

        /* Declaration for a component.  */
        ada_component_decl = 59,
    

        /* Known list of discriminants in type declarations.  */
        ada_discriminant_spec = 60,
    

        /* generic_formal (abstract)  */
        /* Enclosing declaration for a generic formal. The real declaration is
           accessible via the ``decl`` field.  */
    

        /* Formal declaration for an object.  */
        ada_generic_formal_obj_decl = 61,
    

        /* Formal declaration for a package.  */
        ada_generic_formal_package = 62,
    

        /* Formal declaration for a subprogram.  */
        ada_generic_formal_subp_decl = 63,
    

        /* Formal declaration for a type.  */
        ada_generic_formal_type_decl = 64,
    

        /* Specification for a parameter.  */
        ada_param_spec = 65,
    

        /* base_package_decl (abstract)  */
        /* Package declarations. Concrete instances of this class will be
           created in generic package declarations. Other non-generic package
           declarations will be instances of PackageDecl.
        
           The behavior is the same, the only difference is that
           BasePackageDecl and PackageDecl have different behavior regarding
           lexical environments. In the case of generic package declarations,
           we use BasePackageDecl which has no env_spec, and the environment
           behavior is handled by the GenericPackageDecl instance.  */
    

        /* This class denotes the internal package contained by a
           GenericPackageDecl.  */
        ada_generic_package_internal = 66,
    

        /* Non-generic package declarations.  */
        ada_package_decl = 67,
    

        /* base_type_decl (abstract)  */
        /* Base class for type declarations.  */
    

        /* base_subtype_decl (abstract)  */
        /* Base class for subtype declarations.  */
    

        /* Specific ``BaseSubtypeDecl`` synthetic subclass for the base type of
           scalar types.  */
        ada_discrete_base_subtype_decl = 68,
    

        /* Subtype declaration.  */
        ada_subtype_decl = 69,
    

        /* Synthetic node (not parsed, generated from a property call). Refers
           to the classwide type for a given tagged type. The aim is that those
           be mostly equivalent to their non-classwide type, except for some
           resolution rules.  */
        ada_classwide_type_decl = 70,
    

        /* Incomplete declaration for a type.  */
        ada_incomplete_type_decl = 71,
    

        /* Incomplete declaration for a tagged type.  */
        ada_incomplete_tagged_type_decl = 72,
    

        /* Declaration for a protected type.  */
        ada_protected_type_decl = 73,
    

        /* Declaration for a task type.  */
        ada_task_type_decl = 74,
    

        /* Type declaration for a single task.  */
        ada_single_task_type_decl = 75,
    

        /* Type declarations that embed a type definition node.  */
        ada_type_decl = 76,
    

        /* Anonymous type declaration (for anonymous array or access
           types).   */
        ada_anonymous_type_decl = 77,
    

        /* Synthetic anonymous type decl. Used to generate anonymous access
           types.  */
        ada_synth_anonymous_type_decl = 78,
    

        /* basic_subp_decl (abstract)  */
        /* Base class for subprogram declarations.  */
    

        /* classic_subp_decl (abstract)  */
        /* This is an intermediate abstract class for subprogram declarations
           with a common structure: overriding indicator, ``SubpSpec``,
           aspects, <other fields>.  */
    

        /* Declaration for an abstract subprogram.  */
        ada_abstract_subp_decl = 79,
    

        /* formal_subp_decl (abstract)  */
        /* Formal subprogram declarations, in generic declarations formal
           parts.  */
    

        /* Formal declaration for an abstract subprogram.  */
        ada_abstract_formal_subp_decl = 80,
    

        /* Formal declaration for a concrete subprogram.  */
        ada_concrete_formal_subp_decl = 81,
    

        /* Regular subprogram declaration.  */
        ada_subp_decl = 82,
    

        /* Entry declaration.  */
        ada_entry_decl = 83,
    

        /* Declaration for an enumeration literal.  */
        ada_enum_literal_decl = 84,
    

        /* Internal node for generic subprograms.  */
        ada_generic_subp_internal = 85,
    

        /* body_node (abstract)  */
        /* Base class for an Ada body (RM 3.11). A body is the completion of a
           declaration.  */
    

        /* base_subp_body (abstract)  */
        /* Base class for subprogram bodies.  */
    

        /* Expression function.  */
        ada_expr_function = 86,
    

        /* Declaration for a null subprogram.  */
        ada_null_subp_decl = 87,
    

        /* Subprogram body.  */
        ada_subp_body = 88,
    

        /* Declaration for a subprogram renaming.  */
        ada_subp_renaming_decl = 89,
    

        /* body_stub (abstract)  */
        /* Base class for a body stub (RM 10.1.3). A body stub is meant to be
           completed by .  */
    

        /* Stub for a package body (``is separate``).  */
        ada_package_body_stub = 90,
    

        /* Stub for a protected object body (``is separate``).  */
        ada_protected_body_stub = 91,
    

        /* Stub for a subprogram body (``is separate``).  */
        ada_subp_body_stub = 92,
    

        /* Stub for a task body (``is separate``).  */
        ada_task_body_stub = 93,
    

        /* Entry body.  */
        ada_entry_body = 94,
    

        /* Package body.  */
        ada_package_body = 95,
    

        /* Protected object body.  */
        ada_protected_body = 96,
    

        /* Task body.  */
        ada_task_body = 97,
    

        /* Index specification for an entry body.  */
        ada_entry_index_spec = 98,
    

        /* Placeholder node for syntax errors in lists of declarations.  */
        ada_error_decl = 99,
    

        /* Exception declarations.  */
        ada_exception_decl = 100,
    

        /* Exception handler.  */
        ada_exception_handler = 101,
    

        /* Declaration for the controlling variable in a ``for`` loop.  */
        ada_for_loop_var_decl = 102,
    

        /* generic_decl (abstract)  */
        /* Base class for generic declarations.  */
    

        /* Generic package declaration.  */
        ada_generic_package_decl = 103,
    

        /* Generic subprogram declaration.  */
        ada_generic_subp_decl = 104,
    

        /* generic_instantiation (abstract)  */
        /* Instantiations of generics.  */
    

        /* Instantiations of a generic package.  */
        ada_generic_package_instantiation = 105,
    

        /* Instantiations of a generic subprogram.  */
        ada_generic_subp_instantiation = 106,
    

        /* generic_renaming_decl (abstract)  */
        /* Base node for all generic renaming declarations.  */
    

        /* Declaration for a generic package renaming.  */
        ada_generic_package_renaming_decl = 107,
    

        /* Declaration for a generic subprogram renaming.  */
        ada_generic_subp_renaming_decl = 108,
    

        /* Declaration for a code label.  */
        ada_label_decl = 109,
    

        /* BasicDecl that is always the declaration inside a named
           statement.   */
        ada_named_stmt_decl = 110,
    

        /* Declaration for a static constant number.  */
        ada_number_decl = 111,
    

        /* Base class for Ada object declarations (RM 3.3.1). Ada object
           declarations are variables/constants declarations that can be
           declared in any declarative scope.  */
        ada_object_decl = 112,
    

        
        ada_anonymous_object_decl = 113,
    

        /* Object declaration that is part of an extended return statement.  */
        ada_extended_return_stmt_object_decl = 114,
    

        /* Declaration for a package renaming.  */
        ada_package_renaming_decl = 115,
    

        /* Declaration for a single protected object.  */
        ada_single_protected_decl = 116,
    

        /* Declaration for a single task.  */
        ada_single_task_decl = 117,
    

        /* Alternative in a ``case`` statement (``when ... => ...``).  */
        ada_case_stmt_alternative = 118,
    

        /* Root node for all Ada analysis units.  */
        ada_compilation_unit = 119,
    

        /* Representation clause for a single component.  */
        ada_component_clause = 120,
    

        /* Definition for a component.  */
        ada_component_def = 121,
    

        /* constant_node (abstract)  */
        /* Qualifier for the ``constant`` keyword.  */
    

        
        ada_constant_absent = 122,
    

        
        ada_constant_present = 123,
    

        /* constraint (abstract)  */
        /* Base class for type constraints.  */
    

        /* Delta and range type constraint.  */
        ada_delta_constraint = 124,
    

        /* Digits and range type constraint.  */
        ada_digits_constraint = 125,
    

        /* List of constraints that relate to type discriminants.  */
        ada_discriminant_constraint = 126,
    

        /* List of type constraints.  */
        ada_index_constraint = 127,
    

        /* Range-based type constraint.  */
        ada_range_constraint = 128,
    

        /* List of declarations.  */
        ada_declarative_part = 129,
    

        /* List of declarations in a private part.  */
        ada_private_part = 130,
    

        /* List of declarations in a public part.  */
        ada_public_part = 131,
    

        /* ``elsif`` block, part of an ``if`` expression.  */
        ada_elsif_expr_part = 132,
    

        /* ``elsif`` part in an ``if`` statement block.  */
        ada_elsif_stmt_part = 133,
    

        /* expr (abstract)  */
        /* Base class for expressions.  */
    

        /* Allocator expression (``new ...``).  */
        ada_allocator = 134,
    

        /* base_aggregate (abstract)  */
        /* Base class for aggregates.  */
    

        /* Aggregate that is not a ``null record`` aggregate.  */
        ada_aggregate = 135,
    

        /* Aggregate for ``null record``.  */
        ada_null_record_aggregate = 136,
    

        /* Binary expression.  */
        ada_bin_op = 137,
    

        /* Binary operation that compares two value, producing a boolean.  */
        ada_relation_op = 138,
    

        /* Box expression (``<>``).  */
        ada_box_expr = 139,
    

        /* ``case`` expression.  */
        ada_case_expr = 140,
    

        /* Alternative in a ``case`` expression (``when ... => ...``).  */
        ada_case_expr_alternative = 141,
    

        /* List of associations for the ``Contract_Case`` aspect.  */
        ada_contract_cases = 142,
    

        /* ``if`` expression.  */
        ada_if_expr = 143,
    

        /* Represent a membership test (in/not in operators).
        
           Note that we don't consider them as binary operators since multiple
           expressions on the right hand side are allowed.  */
        ada_membership_expr = 144,
    

        /* name (abstract)  */
        /* Base class for names.  */
    

        /* Expression to reference an attribute.  */
        ada_attribute_ref = 145,
    

        /* Reference to the ``Update`` attribute.  */
        ada_update_attribute_ref = 146,
    

        /* Represent a syntactic call expression.
        
           At the semantic level, this can be either a subprogram call, an
           array subcomponent access expression, an array slice or a type
           conversion.  */
        ada_call_expr = 147,
    

        /* Name that defines an entity.  */
        ada_defining_name = 148,
    

        /* Subtype name for membership test expressions.  */
        ada_discrete_subtype_name = 149,
    

        /* Name to select a suffix in a prefix.  */
        ada_dotted_name = 150,
    

        /* Entity name in ``end ...;`` syntactic constructs.  */
        ada_end_name = 151,
    

        /* Explicit dereference expression (``.all``).  */
        ada_explicit_deref = 152,
    

        /* Qualified expression (``...'(...)``).  */
        ada_qual_expr = 153,
    

        /* single_tok_node (abstract)  */
        /* Base class for nodes that are made up of a single token.  */
    

        /* base_id (abstract)  */
        /* Base class for identifiers.  */
    

        /* Character literal.  */
        ada_char_literal = 154,
    

        /* Regular identifier.  */
        ada_identifier = 155,
    

        /* op (abstract)  */
        /* Operation in a binary expression.
        
           Note that the ARM does not consider "double_dot" ("..") as a binary
           operator, but we process it this way here anyway to keep things
           simple.  */
    

        
        ada_op_abs = 156,
    

        
        ada_op_and = 157,
    

        
        ada_op_and_then = 158,
    

        
        ada_op_concat = 159,
    

        
        ada_op_div = 160,
    

        
        ada_op_double_dot = 161,
    

        
        ada_op_eq = 162,
    

        
        ada_op_gt = 163,
    

        
        ada_op_gte = 164,
    

        
        ada_op_in = 165,
    

        
        ada_op_lt = 166,
    

        
        ada_op_lte = 167,
    

        
        ada_op_minus = 168,
    

        
        ada_op_mod = 169,
    

        
        ada_op_mult = 170,
    

        
        ada_op_neq = 171,
    

        
        ada_op_not = 172,
    

        
        ada_op_not_in = 173,
    

        
        ada_op_or = 174,
    

        
        ada_op_or_else = 175,
    

        
        ada_op_plus = 176,
    

        
        ada_op_pow = 177,
    

        
        ada_op_rem = 178,
    

        
        ada_op_xor = 179,
    

        /* String literal.  */
        ada_string_literal = 180,
    

        /* The ``null`` literal.  */
        ada_null_literal = 181,
    

        /* num_literal (abstract)  */
        /* Base class for number literals.  */
    

        /* Literal for an integer.  */
        ada_int_literal = 182,
    

        /* Literal for a real number.  */
        ada_real_literal = 183,
    

        /* Name for Ada 2020's ``@``.  */
        ada_target_name = 184,
    

        /* Parenthesized expression.  */
        ada_paren_expr = 185,
    

        /* Quantified expression.  */
        ada_quantified_expr = 186,
    

        /* Expression to raise an exception.  */
        ada_raise_expr = 187,
    

        /* Unary expression.  */
        ada_un_op = 188,
    

        /* List of statements, with optional exception handlers.  */
        ada_handled_stmts = 189,
    

        /* interface_kind (abstract)  */
        /* Kind of interface type.  */
    

        
        ada_interface_kind_limited = 190,
    

        
        ada_interface_kind_protected = 191,
    

        
        ada_interface_kind_synchronized = 192,
    

        
        ada_interface_kind_task = 193,
    

        /* iter_type (abstract)  */
        /* Iteration type for ``for`` loops.  */
    

        
        ada_iter_type_in = 194,
    

        
        ada_iter_type_of = 195,
    

        /* Library item in a compilation unit.  */
        ada_library_item = 196,
    

        /* limited_node (abstract)  */
        /* Qualifier for the ``limited`` keyword.  */
    

        
        ada_limited_absent = 197,
    

        
        ada_limited_present = 198,
    

        /* loop_spec (abstract)  */
        /* Base class for loop specifications.  */
    

        /* Specification for a ``for`` loop.  */
        ada_for_loop_spec = 199,
    

        /* Specification for a ``while`` loop.  */
        ada_while_loop_spec = 200,
    

        /* mode (abstract)  */
        /* Syntactic indicators for passing modes in formals.  */
    

        
        ada_mode_default = 201,
    

        
        ada_mode_in = 202,
    

        
        ada_mode_in_out = 203,
    

        
        ada_mode_out = 204,
    

        /* not_null (abstract)  */
        /* Qualifier for the ``not null`` keywords.  */
    

        
        ada_not_null_absent = 205,
    

        
        ada_not_null_present = 206,
    

        /* Placeholder for the ``null`` in lists of components.  */
        ada_null_component_decl = 207,
    

        /* ``other`` designator.  */
        ada_others_designator = 208,
    

        /* overriding_node (abstract)  */
        /* Syntactic indicators for subprogram overriding modes.  */
    

        
        ada_overriding_not_overriding = 209,
    

        
        ada_overriding_overriding = 210,
    

        
        ada_overriding_unspecified = 211,
    

        /* List of parameter specifications.  */
        ada_params = 212,
    

        /* Class for pragmas (RM 2.8). Pragmas are compiler directives, that
           can be language or compiler defined.  */
        ada_pragma_node = 213,
    

        /* Synthetic node wrapping around a primitive type entity. Used in
           metadata.  */
        ada_prim_type_accessor = 214,
    

        /* private_node (abstract)  */
        /* Qualifier for the ``private`` keyword.  */
    

        
        ada_private_absent = 215,
    

        
        ada_private_present = 216,
    

        /* Type definition for a protected object.  */
        ada_protected_def = 217,
    

        /* protected_node (abstract)  */
        /* Qualifier for the ``protected`` keyword.  */
    

        
        ada_protected_absent = 218,
    

        
        ada_protected_present = 219,
    

        /* quantifier (abstract)  */
        /* Type for quantified expressions.  */
    

        
        ada_quantifier_all = 220,
    

        
        ada_quantifier_some = 221,
    

        /* Range specification.  */
        ada_range_spec = 222,
    

        /* Renaming clause, used everywhere renamings are valid.  */
        ada_renaming_clause = 223,
    

        /* Synthetic renaming clause. Used to synthesize object decls with
           renamings. (See to_anonymous_object_decl).  */
        ada_synthetic_renaming_clause = 224,
    

        /* reverse_node (abstract)  */
        /* Qualifier for the ``reverse`` keyword.  */
    

        
        ada_reverse_absent = 225,
    

        
        ada_reverse_present = 226,
    

        /* Alternative part in a ``select`` statements block.  */
        ada_select_when_part = 227,
    

        /* stmt (abstract)  */
        /* Bass class for statements.  */
    

        /* composite_stmt (abstract)  */
        /* Base class for composite statements.  */
    

        /* ``accept`` statement.  */
        ada_accept_stmt = 228,
    

        /* Extended ``accept`` statement.  */
        ada_accept_stmt_with_stmts = 229,
    

        /* base_loop_stmt (abstract)  */
        /* Base class for loop statements.  */
    

        /* Statement for ``for`` loops (``for ... loop ... end loop;``).  */
        ada_for_loop_stmt = 230,
    

        /* Statement for simple loops (``loop ... end loop;``).  */
        ada_loop_stmt = 231,
    

        /* Statement for ``while`` loops (``while ... loop ... end
           loop;``).   */
        ada_while_loop_stmt = 232,
    

        /* block_stmt (abstract)  */
        /* Base class for statement blocks.  */
    

        /* Statement block with no declarative part.  */
        ada_begin_block = 233,
    

        /* Statement block with a declarative part.  */
        ada_decl_block = 234,
    

        /* ``case`` statement.  */
        ada_case_stmt = 235,
    

        /* Extended ``return`` statement.  */
        ada_extended_return_stmt = 236,
    

        /* ``if`` statement block.  */
        ada_if_stmt = 237,
    

        /* Wrapper class, used for composite statements that can be named
           (declare blocks, loops). This allows to both have a BasicDecl for
           the named entity declared, and a CompositeStmt for the statement
           hierarchy.  */
        ada_named_stmt = 238,
    

        /* ``select`` statements block.  */
        ada_select_stmt = 239,
    

        /* Placeholder node for syntax errors in lists of statements.  */
        ada_error_stmt = 240,
    

        /* simple_stmt (abstract)  */
        /* Base class for simple statements.  */
    

        /* ``abort`` statement.  */
        ada_abort_stmt = 241,
    

        /* Statement for assignments.  */
        ada_assign_stmt = 242,
    

        /* Statement for entry or procedure calls.  */
        ada_call_stmt = 243,
    

        /* ``delay`` statement.  */
        ada_delay_stmt = 244,
    

        /* ``exit`` statement.  */
        ada_exit_stmt = 245,
    

        /* ``goto`` statement.  */
        ada_goto_stmt = 246,
    

        /* Statement to declare a code label.  */
        ada_label = 247,
    

        /* ``null;`` statement.  */
        ada_null_stmt = 248,
    

        /* ``raise`` statement.  */
        ada_raise_stmt = 249,
    

        /* ``requeue`` statement.  */
        ada_requeue_stmt = 250,
    

        /* ``return`` statement.  */
        ada_return_stmt = 251,
    

        /* ``terminate`` alternative in a ``select`` statement.  */
        ada_terminate_alternative = 252,
    

        /* subp_kind (abstract)  */
        /* Qualifier for a subprogram kind.  */
    

        
        ada_subp_kind_function = 253,
    

        
        ada_subp_kind_procedure = 254,
    

        /* Subunit (``separate``).  */
        ada_subunit = 255,
    

        /* synchronized_node (abstract)  */
        /* Qualifier for the ``synchronized`` keyword.  */
    

        
        ada_synchronized_absent = 256,
    

        
        ada_synchronized_present = 257,
    

        /* tagged_node (abstract)  */
        /* Qualifier for the ``tagged`` keyword.  */
    

        
        ada_tagged_absent = 258,
    

        
        ada_tagged_present = 259,
    

        /* Type definition for a task type.  */
        ada_task_def = 260,
    

        /* type_def (abstract)  */
        /* Base class for type definitions.  */
    

        /* access_def (abstract)  */
        /* Base class for access type definitions.  */
    

        /* Type definition for accesses to subprograms.  */
        ada_access_to_subp_def = 261,
    

        /* base_type_access_def (abstract)  */
        /* Base class for access type definitions.  */
    

        /* Synthetic type access, that will directly reference a type decl. It
           is used to generate synthetic anonymous access types.  */
        ada_anonymous_type_access_def = 262,
    

        /* Syntactic type definition for accesses.  */
        ada_type_access_def = 263,
    

        /* Type definition for an array.  */
        ada_array_type_def = 264,
    

        /* Type definition for a derived type.  */
        ada_derived_type_def = 265,
    

        /* Type definition for enumerations.  */
        ada_enum_type_def = 266,
    

        /* Type definition for discrete types in generic formals.  */
        ada_formal_discrete_type_def = 267,
    

        /* Type definition for an interface.  */
        ada_interface_type_def = 268,
    

        /* Type definition for a modular integer type.  */
        ada_mod_int_type_def = 269,
    

        /* Type definition for a private type.  */
        ada_private_type_def = 270,
    

        /* real_type_def (abstract)  */
        /* Type definition for real numbers.  */
    

        /* Type definition for decimal fixed-point numbers.  */
        ada_decimal_fixed_point_def = 271,
    

        /* Type definition for floating-point numbers.  */
        ada_floating_point_def = 272,
    

        /* Type definition for ordinary fixed-point numbers.  */
        ada_ordinary_fixed_point_def = 273,
    

        /* Type definition for a record.  */
        ada_record_type_def = 274,
    

        /* Type definition for a signed integer type.  */
        ada_signed_int_type_def = 275,
    

        /* type_expr (abstract)  */
        /* A type expression is an abstract node that embodies the concept of a
           reference to a type.
        
           Since Ada has both subtype_indications and anonymous (inline) type
           declarations, a type expression contains one or the other.  */
    

        /* Container for inline anonymous array and access types
           declarations.   */
        ada_anonymous_type = 276,
    

        /* Synthetic node. Represents the type expression for an enum
           literal.   */
        ada_enum_lit_synth_type_expr = 277,
    

        /* Reference to a type by name.  */
        ada_subtype_indication = 278,
    

        /* Reference to a type with a range constraint.  */
        ada_constrained_subtype_indication = 279,
    

        /* Reference to a type with a general constraint.  */
        ada_discrete_subtype_indication = 280,
    

        /* List of unconstrained array indexes.  */
        ada_unconstrained_array_index = 281,
    

        /* until_node (abstract)  */
        /* Qualifier for the ``until`` keyword.  */
    

        
        ada_until_absent = 282,
    

        
        ada_until_present = 283,
    

        /* use_clause (abstract)  */
        /* Base class for use clauses.  */
    

        /* Use clause for packages.  */
        ada_use_package_clause = 284,
    

        /* Use clause for types.  */
        ada_use_type_clause = 285,
    

        /* Single variant in a discriminated type record declaration.
        
           This corresponds to a ``when ... => ...`` section in a variant
           part.   */
        ada_variant = 286,
    

        /* Variant part in a discriminated type record declaration.
        
           This corresponds to the whole ``case ... is ... end case;``
           block.   */
        ada_variant_part = 287,
    

        /* With clause.  */
        ada_with_clause = 288,
    

        /* with_private (abstract)  */
        /* Qualifier for the ``private`` keyword in ``with private`` record
           clauses.  */
    

        
        ada_with_private_absent = 289,
    

        
        ada_with_private_present = 290,
} ada_node_kind_enum;

/* Reference to a symbol. Symbols are owned by analysis contexts, so they must
   not outlive them. This type exists only in the C API, and roughly wraps the
   corresponding Ada type (an array fat pointer).  */
typedef struct {
   void *data;
   void *bounds;
} ada_symbol_type;

/* Data type for env rebindings. For internal use only.  */
typedef void *ada_env_rebindings_type;

typedef uint8_t ada_bool;

/* Helper data structures for source location handling.  */

/* Location in a source file. Line and column numbers are one-based.  */
typedef struct {
    uint32_t line;
    uint16_t column;
} ada_source_location;

/* Location of a span of text in a source file.  */
typedef struct {
    ada_source_location start;
    ada_source_location end;
} ada_source_location_range;


/* String encoded in UTF-32 (native endianness).  */
typedef struct {
   /* Address for the content of the string.  */
    uint32_t *chars;
   /* Size of the string (in characters).  */
    size_t length;

    int is_allocated;
} ada_text;

/* Arbitrarily large integer.  */
typedef void *ada_big_integer;

/* Kind for this token.  */
typedef enum {
   
      
      ADA_TERMINATION = 0
      ,
      ADA_LEXING_FAILURE = 1
      ,
      ADA_IDENTIFIER = 2
      ,
      ADA_ALL = 3
      ,
      ADA_ABORT = 4
      ,
      ADA_ELSE = 5
      ,
      ADA_NEW = 6
      ,
      ADA_RETURN = 7
      ,
      ADA_ABS = 8
      ,
      ADA_ELSIF = 9
      ,
      ADA_NOT = 10
      ,
      ADA_REVERSE = 11
      ,
      ADA_END = 12
      ,
      ADA_NULL = 13
      ,
      ADA_ACCEPT = 14
      ,
      ADA_ENTRY = 15
      ,
      ADA_SELECT = 16
      ,
      ADA_ACCESS = 17
      ,
      ADA_EXCEPTION = 18
      ,
      ADA_OF = 19
      ,
      ADA_SEPARATE = 20
      ,
      ADA_EXIT = 21
      ,
      ADA_OR = 22
      ,
      ADA_OTHERS = 23
      ,
      ADA_SUBTYPE = 24
      ,
      ADA_AND = 25
      ,
      ADA_FOR = 26
      ,
      ADA_OUT = 27
      ,
      ADA_ARRAY = 28
      ,
      ADA_FUNCTION = 29
      ,
      ADA_AT = 30
      ,
      ADA_GENERIC = 31
      ,
      ADA_PACKAGE = 32
      ,
      ADA_TASK = 33
      ,
      ADA_BEGIN = 34
      ,
      ADA_GOTO = 35
      ,
      ADA_PRAGMA = 36
      ,
      ADA_TERMINATE = 37
      ,
      ADA_BODY = 38
      ,
      ADA_PRIVATE = 39
      ,
      ADA_THEN = 40
      ,
      ADA_IF = 41
      ,
      ADA_PROCEDURE = 42
      ,
      ADA_TYPE = 43
      ,
      ADA_CASE = 44
      ,
      ADA_IN = 45
      ,
      ADA_CONSTANT = 46
      ,
      ADA_IS = 47
      ,
      ADA_RAISE = 48
      ,
      ADA_USE = 49
      ,
      ADA_DECLARE = 50
      ,
      ADA_RANGE = 51
      ,
      ADA_DELAY = 52
      ,
      ADA_LIMITED = 53
      ,
      ADA_RECORD = 54
      ,
      ADA_WHEN = 55
      ,
      ADA_DELTA = 56
      ,
      ADA_LOOP = 57
      ,
      ADA_REM = 58
      ,
      ADA_WHILE = 59
      ,
      ADA_DIGITS = 60
      ,
      ADA_RENAMES = 61
      ,
      ADA_DO = 62
      ,
      ADA_MOD = 63
      ,
      ADA_XOR = 64
      ,
      ADA_PAR_CLOSE = 65
      ,
      ADA_PAR_OPEN = 66
      ,
      ADA_SEMICOLON = 67
      ,
      ADA_COLON = 68
      ,
      ADA_COMMA = 69
      ,
      ADA_DOUBLEDOT = 70
      ,
      ADA_DOT = 71
      ,
      ADA_DIAMOND = 72
      ,
      ADA_LTE = 73
      ,
      ADA_GTE = 74
      ,
      ADA_ARROW = 75
      ,
      ADA_EQUAL = 76
      ,
      ADA_LT = 77
      ,
      ADA_GT = 78
      ,
      ADA_PLUS = 79
      ,
      ADA_MINUS = 80
      ,
      ADA_POWER = 81
      ,
      ADA_MULT = 82
      ,
      ADA_AMP = 83
      ,
      ADA_NOTEQUAL = 84
      ,
      ADA_DIVIDE = 85
      ,
      ADA_TICK = 86
      ,
      ADA_PIPE = 87
      ,
      ADA_ASSIGN = 88
      ,
      ADA_LABEL_START = 89
      ,
      ADA_LABEL_END = 90
      ,
      ADA_TARGET = 91
      ,
      ADA_STRING = 92
      ,
      ADA_CHAR = 93
      ,
      ADA_WITH = 94
      ,
      ADA_DECIMAL = 95
      ,
      ADA_INTEGER = 96
      ,
      ADA_COMMENT = 97
      ,
      ADA_PREP_LINE = 98
      ,
      ADA_WHITESPACE = 99
} ada_token_kind;

/* Reference to a token in an analysis unit.  */
typedef struct {
    /* Private data associated to this token or NULL if this designates no
       token.  */
    void *token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;

    ada_token_kind kind;
    ada_text text;
    ada_source_location_range sloc_range;
} ada_token;


/* Diagnostic for an analysis unit: cannot open the source file, parsing error,
   ...  */
typedef struct {
    ada_source_location_range sloc_range;
    ada_text message;
} ada_diagnostic;

   typedef enum {
      ADA_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, ADA_ANALYSIS_UNIT_KIND_UNIT_BODY
   } ada_analysis_unit_kind;
   /* Specify a kind of analysis unit. Specification units provide an interface
      to the outer world while body units provide an implementation for the
      corresponding interface.  */
   typedef enum {
      ADA_LOOKUP_KIND_RECURSIVE, ADA_LOOKUP_KIND_FLAT, ADA_LOOKUP_KIND_MINIMAL
   } ada_lookup_kind;
   
   typedef enum {
      ADA_FIND_ALL_MODE_REFERENCES, ADA_FIND_ALL_MODE_DERIVEDTYPES
   } ada_find_all_mode;
   
   typedef enum {
      ADA_REF_RESULT_KIND_NOREF, ADA_REF_RESULT_KIND_PRECISE, ADA_REF_RESULT_KIND_IMPRECISE, ADA_REF_RESULT_KIND_ERROR
   } ada_ref_result_kind;
   /* Kind for the result of a cross reference operation.
   
      - ``NoRef`` is for no reference, it is the null value for this enum.
   
      - ``Precise`` is when the reference result is precise.
   
      - ``Imprecise`` is when there was an error computing the precise result,
      and a result was gotten in an imprecise fashion.
   
      - ``Error`` is for unrecoverable errors (either there is no imprecise
      path for the request you made, or the imprecise path errored out too.  */
   typedef enum {
      ADA_GRAMMAR_RULE_DECIMAL_FIXED_POINT_DEF_RULE, ADA_GRAMMAR_RULE_RELATION_RULE, ADA_GRAMMAR_RULE_ASPECT_SPEC_RULE, ADA_GRAMMAR_RULE_SIMPLE_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_ANONYMOUS_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_FORMAL_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_PARAM_SPEC_RULE, ADA_GRAMMAR_RULE_DECL_PART_RULE, ADA_GRAMMAR_RULE_ASPECT_CLAUSE_RULE, ADA_GRAMMAR_RULE_IF_EXPR_RULE, ADA_GRAMMAR_RULE_TASK_DEF_RULE, ADA_GRAMMAR_RULE_SELECT_STMT_RULE, ADA_GRAMMAR_RULE_PARAM_SPECS_RULE, ADA_GRAMMAR_RULE_ILOOP_STMT_RULE, ADA_GRAMMAR_RULE_DEFINING_ID_LIST_RULE, ADA_GRAMMAR_RULE_INT_LITERAL_RULE, ADA_GRAMMAR_RULE_PROTECTED_BODY_STUB_RULE, ADA_GRAMMAR_RULE_UPDATE_ATTR_AGGREGATE_RULE, ADA_GRAMMAR_RULE_ARRAY_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_PROTECTED_EL_RULE, ADA_GRAMMAR_RULE_SIMPLE_STMT_RULE, ADA_GRAMMAR_RULE_INTERFACE_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_COMPILATION_UNIT_RULE, ADA_GRAMMAR_RULE_COMPONENT_CLAUSE_RULE, ADA_GRAMMAR_RULE_SIGNED_INT_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_NULL_LITERAL_RULE, ADA_GRAMMAR_RULE_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_RANGE_SPEC_RULE, ADA_GRAMMAR_RULE_PAREN_EXPR_RULE, ADA_GRAMMAR_RULE_DERIVED_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_BOX_EXPR_RULE, ADA_GRAMMAR_RULE_TERM_RULE, ADA_GRAMMAR_RULE_OTHERS_DESIGNATOR_RULE, ADA_GRAMMAR_RULE_IF_STMT_RULE, ADA_GRAMMAR_RULE_GENERIC_FORMAL_PART_RULE, ADA_GRAMMAR_RULE_DISCRIMINANT_PART_RULE, ADA_GRAMMAR_RULE_PROTECTED_DEF_RULE, ADA_GRAMMAR_RULE_RENAMING_CLAUSE_RULE, ADA_GRAMMAR_RULE_SINGLE_TASK_DECL_RULE, ADA_GRAMMAR_RULE_DISCRETE_SUBTYPE_INDICATION_RULE, ADA_GRAMMAR_RULE_BOOLEAN_OP_RULE, ADA_GRAMMAR_RULE_MODE_RULE, ADA_GRAMMAR_RULE_REL_OP_RULE, ADA_GRAMMAR_RULE_ACCEPT_STMT_RULE, ADA_GRAMMAR_RULE_TASK_BODY_STUB_RULE, ADA_GRAMMAR_RULE_DISCRIMINANT_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_SUBTYPE_DECL_RULE, ADA_GRAMMAR_RULE_MEMBERSHIP_CHOICE_LIST_RULE, ADA_GRAMMAR_RULE_DELTA_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_STATIC_NAME_RULE, ADA_GRAMMAR_RULE_CHAR_LITERAL_RULE, ADA_GRAMMAR_RULE_DISCRETE_SUBTYPE_DEFINITION_RULE, ADA_GRAMMAR_RULE_OVERRIDING_INDICATOR_RULE, ADA_GRAMMAR_RULE_RANGE_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_RECOV_DECL_PART_RULE, ADA_GRAMMAR_RULE_PACKAGE_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_STRING_LITERAL_RULE, ADA_GRAMMAR_RULE_PACKAGE_BODY_RULE, ADA_GRAMMAR_RULE_CASE_STMT_RULE, ADA_GRAMMAR_RULE_RECORD_DEF_RULE, ADA_GRAMMAR_RULE_STMTS_RULE, ADA_GRAMMAR_RULE_CONTRACT_CASES_EXPR_RULE, ADA_GRAMMAR_RULE_BLOCK_STMT_RULE, ADA_GRAMMAR_RULE_LABEL_RULE, ADA_GRAMMAR_RULE_ABSTRACT_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_FOR_LOOP_PARAM_SPEC_RULE, ADA_GRAMMAR_RULE_DIRECT_NAME_OR_TARGET_NAME_RULE, ADA_GRAMMAR_RULE_ASSIGNMENT_STMT_RULE, ADA_GRAMMAR_RULE_SUB_OBJECT_DECL_RULE, ADA_GRAMMAR_RULE_SUBUNIT_RULE, ADA_GRAMMAR_RULE_PARAM_ASSOC_RULE, ADA_GRAMMAR_RULE_FLOATING_POINT_DEF_RULE, ADA_GRAMMAR_RULE_COMPOUND_STMT_RULE, ADA_GRAMMAR_RULE_PACKAGE_BODY_STUB_RULE, ADA_GRAMMAR_RULE_BODY_RULE, ADA_GRAMMAR_RULE_DISCR_SPEC_LIST_RULE, ADA_GRAMMAR_RULE_OBJECT_DECL_RULE, ADA_GRAMMAR_RULE_RECORD_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_COMPILATION_RULE, ADA_GRAMMAR_RULE_LIBRARY_UNIT_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_STMT_RULE, ADA_GRAMMAR_RULE_ENTRY_BODY_RULE, ADA_GRAMMAR_RULE_WITH_CLAUSE_RULE, ADA_GRAMMAR_RULE_TERMINATE_ALTERNATIVE_RULE, ADA_GRAMMAR_RULE_EXT_RET_STMT_OBJECT_DECL_RULE, ADA_GRAMMAR_RULE_SUBTYPE_INDICATION_RULE, ADA_GRAMMAR_RULE_IDENTIFIER_RULE, ADA_GRAMMAR_RULE_LIBRARY_UNIT_BODY_RULE, ADA_GRAMMAR_RULE_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_SEXPR_OR_BOX_RULE, ADA_GRAMMAR_RULE_USE_CLAUSE_RULE, ADA_GRAMMAR_RULE_SUBP_SPEC_RULE, ADA_GRAMMAR_RULE_UNOP_TERM_RULE, ADA_GRAMMAR_RULE_NULL_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_EXT_RETURN_STMT_RULE, ADA_GRAMMAR_RULE_USE_PACKAGE_CLAUSE_RULE, ADA_GRAMMAR_RULE_VARIANT_PART_RULE, ADA_GRAMMAR_RULE_MEMBERSHIP_CHOICE_RULE, ADA_GRAMMAR_RULE_EXCEPTION_HANDLER_RULE, ADA_GRAMMAR_RULE_PRIMARY_RULE, ADA_GRAMMAR_RULE_CONSTRAINT_LIST_RULE, ADA_GRAMMAR_RULE_RAISE_EXPR_RULE, ADA_GRAMMAR_RULE_RETURN_STMT_RULE, ADA_GRAMMAR_RULE_GENERIC_DECL_RULE, ADA_GRAMMAR_RULE_COMPONENT_DECL_RULE, ADA_GRAMMAR_RULE_ANONYMOUS_TYPE_RULE, ADA_GRAMMAR_RULE_MOD_INT_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_PRAGMA_ARGUMENT_RULE, ADA_GRAMMAR_RULE_COMPONENT_DEF_RULE, ADA_GRAMMAR_RULE_AGGREGATE_ASSOC_RULE, ADA_GRAMMAR_RULE_EXPR_FN_RULE, ADA_GRAMMAR_RULE_CALL_SUFFIX_RULE, ADA_GRAMMAR_RULE_ASPECT_ASSOC_RULE, ADA_GRAMMAR_RULE_PROTECTED_BODY_RULE, ADA_GRAMMAR_RULE_IBLOCK_STMT_RULE, ADA_GRAMMAR_RULE_NAME_RULE, ADA_GRAMMAR_RULE_CONTEXT_ITEM_RULE, ADA_GRAMMAR_RULE_TASK_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_DISCRIMINANT_SPEC_RULE, ADA_GRAMMAR_RULE_GENERIC_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_DELAY_STMT_RULE, ADA_GRAMMAR_RULE_USE_TYPE_CLAUSE_RULE, ADA_GRAMMAR_RULE_REAL_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_CALL_STMT_RULE, ADA_GRAMMAR_RULE_BODY_STUB_RULE, ADA_GRAMMAR_RULE_FACTOR_RULE, ADA_GRAMMAR_RULE_DEFINING_ID_RULE, ADA_GRAMMAR_RULE_ENTRY_DECL_RULE, ADA_GRAMMAR_RULE_PROTECTED_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_FORMAL_DISCRETE_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_DISCRETE_RANGE_RULE, ADA_GRAMMAR_RULE_PRAGMA_RULE, ADA_GRAMMAR_RULE_REQUEUE_STMT_RULE, ADA_GRAMMAR_RULE_RAISE_STMT_RULE, ADA_GRAMMAR_RULE_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_QUAL_NAME_INTERNAL_RULE, ADA_GRAMMAR_RULE_CONDITIONAL_EXPR_RULE, ADA_GRAMMAR_RULE_CONTRACT_CASE_ASSOC_RULE, ADA_GRAMMAR_RULE_BASIC_DECL_RULE, ADA_GRAMMAR_RULE_EXPR_RULE, ADA_GRAMMAR_RULE_SUBP_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_SIMPLE_EXPR_RULE, ADA_GRAMMAR_RULE_CASE_ALT_RULE, ADA_GRAMMAR_RULE_GENERIC_FORMAL_DECL_RULE, ADA_GRAMMAR_RULE_ENUM_LITERAL_DECL_RULE, ADA_GRAMMAR_RULE_DIGITS_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_GOTO_STMT_RULE, ADA_GRAMMAR_RULE_DIRECT_NAME_RULE, ADA_GRAMMAR_RULE_HANDLED_STMTS_RULE, ADA_GRAMMAR_RULE_TASK_ITEM_RULE, ADA_GRAMMAR_RULE_CASE_EXPR_RULE, ADA_GRAMMAR_RULE_TARGET_NAME_RULE, ADA_GRAMMAR_RULE_PROTECTED_DECL_RULE, ADA_GRAMMAR_RULE_TASK_BODY_RULE, ADA_GRAMMAR_RULE_ABORT_STMT_RULE, ADA_GRAMMAR_RULE_DEC_LITERAL_RULE, ADA_GRAMMAR_RULE_EXCEPTION_DECL_RULE, ADA_GRAMMAR_RULE_PACKAGE_DECL_RULE, ADA_GRAMMAR_RULE_SUBP_BODY_STUB_RULE, ADA_GRAMMAR_RULE_DISCRIMINANT_ASSOC_RULE, ADA_GRAMMAR_RULE_QUALIFIED_NAME_RULE, ADA_GRAMMAR_RULE_ENUM_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_CHOICE_LIST_RULE, ADA_GRAMMAR_RULE_ALLOCATOR_RULE, ADA_GRAMMAR_RULE_DEFINING_NAME_RULE, ADA_GRAMMAR_RULE_SUBP_BODY_RULE, ADA_GRAMMAR_RULE_UPDATE_ATTR_CONTENT_RULE, ADA_GRAMMAR_RULE_CASE_EXPR_ALT_RULE, ADA_GRAMMAR_RULE_GENERIC_INSTANTIATION_RULE, ADA_GRAMMAR_RULE_LIBRARY_ITEM_RULE, ADA_GRAMMAR_RULE_EXIT_STMT_RULE, ADA_GRAMMAR_RULE_CONSTRAINED_SUBTYPE_INDICATION_RULE, ADA_GRAMMAR_RULE_VARIANT_RULE, ADA_GRAMMAR_RULE_SUBTYPE_NAME_RULE, ADA_GRAMMAR_RULE_INDEX_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_PROTECTED_OP_RULE, ADA_GRAMMAR_RULE_LOOP_STMT_RULE, ADA_GRAMMAR_RULE_AGGREGATE_RULE, ADA_GRAMMAR_RULE_CHOICE_RULE, ADA_GRAMMAR_RULE_NUM_LITERAL_RULE, ADA_GRAMMAR_RULE_QUANTIFIED_EXPR_RULE, ADA_GRAMMAR_RULE_UNCONSTRAINED_INDEX_RULE, ADA_GRAMMAR_RULE_PARENT_LIST_RULE, ADA_GRAMMAR_RULE_COMPONENT_ITEM_RULE, ADA_GRAMMAR_RULE_TYPE_EXPR_RULE, ADA_GRAMMAR_RULE_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_COMPONENT_LIST_RULE, ADA_GRAMMAR_RULE_MULTIDIM_ARRAY_ASSOC_RULE, ADA_GRAMMAR_RULE_ACCESS_DEF_RULE, ADA_GRAMMAR_RULE_NULL_STMT_RULE, ADA_GRAMMAR_RULE_NUMBER_DECL_RULE, ADA_GRAMMAR_RULE_ORDINARY_FIXED_POINT_DEF_RULE, ADA_GRAMMAR_RULE_BASIC_DECLS_RULE
   } ada_grammar_rule;
   /* Gramar rule to use for parsing.  */

const ada_grammar_rule ada_default_grammar_rule = ADA_GRAMMAR_RULE_COMPILATION_RULE;

/* Enumerated type describing all possible exceptions that need to be handled
   in the C bindings.  */
typedef enum {
      EXCEPTION_INVALID_FIELD,
      EXCEPTION_NODE_DATA_EVALUATION_ERROR,
      EXCEPTION_INVALID_INPUT,
      EXCEPTION_INVALID_SYMBOL_ERROR,
      EXCEPTION_INVALID_UNIT_NAME_ERROR,
      EXCEPTION_NATIVE_EXCEPTION,
      EXCEPTION_PRECONDITION_FAILURE,
      EXCEPTION_PROPERTY_ERROR,
      EXCEPTION_TEMPLATE_ARGS_ERROR,
      EXCEPTION_TEMPLATE_FORMAT_ERROR,
      EXCEPTION_TEMPLATE_INSTANTIATION_ERROR,
      EXCEPTION_STALE_REFERENCE_ERROR,
      EXCEPTION_UNKNOWN_CHARSET,
} ada_exception_kind;

/* Holder for native exceptions-related information.  Memory management for
   this and all the fields is handled by the library: one just has to make sure
   not to keep references to it.

   .. todo:: For the moment, this structure contains already formatted
      information, but depending on possible future Ada runtime improvements,
      this might change.  */
typedef struct {
   /* The kind of this exception.  */
   ada_exception_kind kind;

   /* Message and context information associated with this exception.  */
   const char *information;
} ada_exception;

/*
 * Array types incomplete declarations
 */



typedef struct ada_bare_ada_node_array *ada_bare_ada_node_array;



typedef struct ada_ada_node_array *ada_ada_node_array;


        

typedef struct ada_text_type *ada_text_type;

        

typedef struct ada_completion_item_array *ada_completion_item_array;

        

typedef struct ada_doc_annotation_array *ada_doc_annotation_array;

        

typedef struct ada_param_actual_array *ada_param_actual_array;

        

typedef struct ada_ref_result_array *ada_ref_result_array;

        

typedef struct ada_substitution_array *ada_substitution_array;

        

typedef struct ada_analysis_unit_array *ada_analysis_unit_array;

        

typedef struct ada_unbounded_text_type_array *ada_unbounded_text_type_array;


/*
 * Struct types declarations
 */

        



typedef struct {
        ada_bool dottable_subp;
        ada_bool access_entity;
        ada_base_node primitive;
        ada_base_node primitive_real_type;
} ada_internal_metadata;



        



typedef struct {
        ada_internal_metadata md;
        ada_env_rebindings_type rebindings;
        ada_bool from_rebound;
} ada_internal_entity_info;



        



typedef struct {
        ada_base_node node;
        ada_internal_entity_info info;
} ada_base_entity;



        



typedef struct {
        ada_bool exists;
        ada_base_entity node;
        ada_base_entity value;
} ada_internal_aspect;



        



typedef struct {
        ada_base_entity decl;
        ada_bool is_dot_call;
} ada_internal_completion_item;



        



typedef struct {
        ada_base_entity low_bound;
        ada_base_entity high_bound;
} ada_internal_discrete_range;



        



typedef struct {
        ada_text_type key;
        ada_text_type value;
} ada_internal_doc_annotation;

    /* Increment the ref-count of all components in "r".  */
    extern void
    ada_internal_doc_annotation_inc_ref(ada_internal_doc_annotation *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    ada_internal_doc_annotation_dec_ref(ada_internal_doc_annotation *r);


        



typedef struct {
        ada_base_entity param;
        ada_base_entity actual;
} ada_internal_param_actual;



        



typedef struct {
        ada_base_entity ref;
        ada_ref_result_kind kind;
} ada_internal_ref_result;



        



typedef struct {
        ada_base_entity decl;
        ada_ref_result_kind kind;
} ada_internal_refd_decl;



        



typedef struct {
        ada_base_entity def_name;
        ada_ref_result_kind kind;
} ada_internal_refd_def;



        



typedef struct {
        ada_base_entity from_decl;
        ada_big_integer to_value;
        ada_base_entity value_type;
} ada_internal_substitution;

    /* Increment the ref-count of all components in "r".  */
    extern void
    ada_internal_substitution_inc_ref(ada_internal_substitution *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    ada_internal_substitution_dec_ref(ada_internal_substitution *r);



/*
 * Types for unit providers
 */

/* Interface to fetch analysis units from a name and a unit kind.

   The unit provider mechanism provides an abstraction which assumes that to
   any couple (unit name, unit kind) we can associate at most one source file.
   This means that several couples can be associated to the same source file,
   but on the other hand, only one one source file can be associated to a
   couple.

   This is used to make the semantic analysis able to switch from one analysis
   units to another.  */
typedef void *ada_unit_provider;

/* Callback type for functions that are called when destroying a unit file
   provider type.  */
typedef void (*ada_unit_provider_destroy_callback)(void *data);

/* Callback type for functions that are called to turn a unit reference encoded
   as a unit name into an analysis unit.  */
typedef char *(*ada_unit_provider_get_unit_filename_callback)(
   void *data,
   ada_text *name,
   ada_analysis_unit_kind kind
);

/* Callback type for functions that are called to turn a unit reference encoded
   as a unit name into an analysis unit.  */
typedef ada_analysis_unit (*ada_unit_provider_get_unit_from_name_callback)(
   void *data,
   ada_analysis_context context,
   ada_text *name,
   ada_analysis_unit_kind kind,
   const char *charset,
   int reparse
);

/* All the functions below can potentially raise an exception, so
   ada_get_last_exception must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */






struct ada_bare_ada_node_array {
   int n;
   int ref_count;
   ada_base_node items[1];
};

/* Create a length-sized array.  */
extern ada_bare_ada_node_array
ada_bare_ada_node_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_bare_ada_node_array_inc_ref(ada_bare_ada_node_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_bare_ada_node_array_dec_ref(ada_bare_ada_node_array a);







struct ada_ada_node_array {
   int n;
   int ref_count;
   ada_base_entity items[1];
};

/* Create a length-sized array.  */
extern ada_ada_node_array
ada_ada_node_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_ada_node_array_inc_ref(ada_ada_node_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_ada_node_array_dec_ref(ada_ada_node_array a);



        




struct ada_text_type {
   int n;
   int ref_count;
   uint32_t items[1];
};

/* Create a length-sized array.  */
extern ada_text_type
ada_text_type_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_text_type_inc_ref(ada_text_type a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_text_type_dec_ref(ada_text_type a);


        




struct ada_completion_item_array {
   int n;
   int ref_count;
   ada_internal_completion_item items[1];
};

/* Create a length-sized array.  */
extern ada_completion_item_array
ada_completion_item_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_completion_item_array_inc_ref(ada_completion_item_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_completion_item_array_dec_ref(ada_completion_item_array a);


        




struct ada_doc_annotation_array {
   int n;
   int ref_count;
   ada_internal_doc_annotation items[1];
};

/* Create a length-sized array.  */
extern ada_doc_annotation_array
ada_doc_annotation_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_doc_annotation_array_inc_ref(ada_doc_annotation_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_doc_annotation_array_dec_ref(ada_doc_annotation_array a);


        




struct ada_param_actual_array {
   int n;
   int ref_count;
   ada_internal_param_actual items[1];
};

/* Create a length-sized array.  */
extern ada_param_actual_array
ada_param_actual_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_param_actual_array_inc_ref(ada_param_actual_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_param_actual_array_dec_ref(ada_param_actual_array a);


        




struct ada_ref_result_array {
   int n;
   int ref_count;
   ada_internal_ref_result items[1];
};

/* Create a length-sized array.  */
extern ada_ref_result_array
ada_ref_result_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_ref_result_array_inc_ref(ada_ref_result_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_ref_result_array_dec_ref(ada_ref_result_array a);


        




struct ada_substitution_array {
   int n;
   int ref_count;
   ada_internal_substitution items[1];
};

/* Create a length-sized array.  */
extern ada_substitution_array
ada_substitution_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_substitution_array_inc_ref(ada_substitution_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_substitution_array_dec_ref(ada_substitution_array a);


        




struct ada_analysis_unit_array {
   int n;
   int ref_count;
   ada_analysis_unit items[1];
};

/* Create a length-sized array.  */
extern ada_analysis_unit_array
ada_analysis_unit_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_analysis_unit_array_inc_ref(ada_analysis_unit_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_analysis_unit_array_dec_ref(ada_analysis_unit_array a);


        




struct ada_unbounded_text_type_array {
   int n;
   int ref_count;
   ada_symbol_type items[1];
};

/* Create a length-sized array.  */
extern ada_unbounded_text_type_array
ada_unbounded_text_type_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_unbounded_text_type_array_inc_ref(ada_unbounded_text_type_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_unbounded_text_type_array_dec_ref(ada_unbounded_text_type_array a);



/*
 * Analysis primitives
 */

/* Create a new analysis context.

   ``Charset`` will be used as a default charset to decode input sources in
   analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   charsets. Be careful: passing an unsupported charset is not guaranteed to
   raise an error here. If no charset is provided, ``"iso-8859-1"`` is the
   default.

   .. todo:: Passing an unsupported charset here is not guaranteed to raise an
      error right here, but this would be really helpful for users.

   When ``With_Trivia`` is true, the parsed analysis units will contain
   trivias.

   If provided, ``Unit_Provider`` will be used to query the file name that
   corresponds to a unit reference during semantic analysis. If it is ``NULL``,
   the default one is used instead.

   ``Tab_Stop`` is a positive number to describe the effect of tabulation
   characters on the column number in source files.  */
extern ada_analysis_context
ada_create_analysis_context(
   const char *charset,
   ada_unit_provider unit_provider,
   int with_trivia,
   int tab_stop
);

/* Increase the reference count to an analysis context. Return the reference
   for convenience.  */
extern ada_analysis_context
ada_context_incref(ada_analysis_context context);

/* Decrease the reference count to an analysis context. Destruction happens
   when the ref-count reaches 0.  */
extern void
ada_context_decref(ada_analysis_context context);

/* If the given string is a valid symbol, yield it as a symbol and return true.
   Otherwise, return false.  */
extern int
ada_context_symbol(ada_analysis_context context,
                                   ada_text *text,
                                   ada_symbol_type *symbol);

/* Debug helper. Set whether ``Property_Error`` exceptions raised in
   ``Populate_Lexical_Env`` should be discarded. They are by default.  */
extern void
ada_context_discard_errors_in_populate_lexical_env(
        ada_analysis_context context,
        int discard);

/* Create a new analysis unit for ``Filename`` or return the existing one if
   any. If ``Reparse`` is true and the analysis unit already exists, reparse it
   from ``Filename``.

   ``Rule`` controls which grammar rule is used to parse the unit.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as file opening, decoding, lexing or parsing
   failure, return an analysis unit anyway: errors are described as diagnostics
   of the returned analysis unit.  */
extern ada_analysis_unit
ada_get_analysis_unit_from_file(
        ada_analysis_context context,
        const char *filename,
        const char *charset,
        int reparse,
        ada_grammar_rule rule);

/* Create a new analysis unit for ``Filename`` or return the existing one if
   any. Whether the analysis unit already exists or not, (re)parse it from the
   source code in ``Buffer``.

   ``Rule`` controls which grammar rule is used to parse the unit.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as file opening, decoding, lexing or parsing
   failure, return an analysis unit anyway: errors are described as diagnostics
   of the returned analysis unit.  */
extern ada_analysis_unit
ada_get_analysis_unit_from_buffer(
        ada_analysis_context context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        ada_grammar_rule rule);

/* Create a new analysis unit for ``Name``/``Kind`` or return the existing one
   if any. If ``Reparse`` is true and the analysis unit already exists, reparse
   it from ``Filename``.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If the unit name cannot be tuned into a file name, return ``NULL``. If any
   other failure occurs, such as file opening, decoding, lexing or parsing
   failure, return an analysis unit anyway: errors are described as diagnostics
   of the returned analysis unit.  */
extern ada_analysis_unit
ada_get_analysis_unit_from_provider(
        ada_analysis_context context,
        ada_text *name,
        ada_analysis_unit_kind kind,
        const char *charset,
        int reparse);

/* Return the root node for this unit, or ``NULL`` if there is none.  */
extern void
ada_unit_root(ada_analysis_unit unit,
                              ada_base_entity *result_p);

/* Return a reference to the first token scanned in this unit.  */
extern void
ada_unit_first_token(ada_analysis_unit unit,
                                     ada_token *token);

/* Return a reference to the last token scanned in this unit.  */
extern void
ada_unit_last_token(ada_analysis_unit unit,
                                    ada_token *token);

/* Return the number of tokens in this unit.  */
extern int
ada_unit_token_count(ada_analysis_unit unit);

/* Return the number of trivias in this unit. This is 0 for units that were
   parsed with trivia analysis disabled.  */
extern int
ada_unit_trivia_count(ada_analysis_unit unit);

/* Debug helper: output the lexical envs for the given analysis unit.  */
extern void
ada_unit_dump_lexical_env(ada_analysis_unit unit);

/* Return the filename this unit is associated to.

   The returned string is dynamically allocated and the caller must free it
   when done with it.  */
extern char *
ada_unit_filename(ada_analysis_unit unit);

/* Return the number of diagnostics associated to this unit.  */
extern unsigned
ada_unit_diagnostic_count(ada_analysis_unit unit);

/* Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P. Return
   zero on failure (when N is too big).  */
extern int
ada_unit_diagnostic(ada_analysis_unit unit,
                                    unsigned n,
                                    ada_diagnostic *diagnostic_p);

/* Return the context that owns this unit.  */
extern ada_analysis_context
ada_unit_context(ada_analysis_unit context);

/* Reparse an analysis unit from the associated file.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as decoding, lexing or parsing failure,
   diagnostic are emitted to explain what happened.  */
extern void
ada_unit_reparse_from_file(ada_analysis_unit unit,
                                           const char *charset);

/* Reparse an analysis unit from a buffer.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as decoding, lexing or parsing failure,
   diagnostic are emitted to explain what happened.  */
extern void
ada_unit_reparse_from_buffer (ada_analysis_unit unit,
                                              const char *charset,
                                              const char *buffer,
                                              size_t buffer_size);

/* Create lexical environments for this analysis unit, according to the
   specifications given in the language spec.

   If not done before, it will be automatically called during semantic
   analysis. Calling it before enables one to control where the latency occurs.

   Depending on whether errors are discarded (see
   ``Discard_Errors_In_Populate_Lexical_Env``), return 0 on failure and 1 on
   success.  */
extern int
ada_unit_populate_lexical_env(ada_analysis_unit unit);

/*
 * General AST node primitives
 */

/* Return whether this node is a null node reference.  */
static inline int
ada_node_is_null(ada_base_entity *node) {
    return node->node == NULL;
}

/* Return the kind of this node.  */
extern ada_node_kind_enum
ada_node_kind(ada_base_entity *node);

/* Helper for textual dump: return the kind name for this node. The returned
   string is a copy and thus must be free'd by the caller.  */
extern void
ada_kind_name(ada_node_kind_enum kind, ada_text *result);

/* Return the analysis unit that owns this node.  */
extern int
ada_node_unit(ada_base_entity *node,
                              ada_analysis_unit *unit_p);

/* Return whether this node is a node that contains only a single token.  */
extern int
ada_node_is_token_node(ada_base_entity *node);

/* Return whether this node is synthetic.  */
extern int
ada_node_is_synthetic(ada_base_entity *node);

/* Return a representation of this node as a string.  */
extern void
ada_node_short_image(ada_base_entity *node,
                                     ada_text *result);

/* Return the source buffer slice corresponding to the text that spans between
   the first and the last tokens of this node.

   Note that this returns the empty string for synthetic nodes.  */
extern void
ada_node_text(ada_base_entity *node,
                              ada_text *text);

/* Return the spanning source location range for this node.

   Note that this returns the sloc of the parent for synthetic nodes.  */
extern void
ada_node_sloc_range(ada_base_entity *node,
                                    ada_source_location_range *sloc_range);

/* Return the bottom-most node from in ``Node`` and its children which contains
   ``Sloc``, or ``NULL`` if there is none.  */
extern void
ada_lookup_in_node(ada_base_entity *node,
                                   const ada_source_location *sloc,
                                   ada_base_entity *result_p);

/* Return the number of children in this node.  */
extern unsigned
ada_node_children_count(ada_base_entity *node);

/* Return the Nth child for in this node's fields and store it into *CHILD_P.
   Return zero on failure (when N is too big).  */
extern int
ada_node_child(ada_base_entity *node,
                               unsigned n,
                               ada_base_entity* child_p);

/* Encode some text using the current locale. The result is dynamically
   allocated: it is up to the caller to free it when done with it.

   This is a development helper to make it quick and easy to print token and
   diagnostic text: it ignores errors (when the locale does not support some
   characters). Production code should use real conversion routines such as
   libiconv's in order to deal with UTF-32 texts.  */
extern char *
ada_text_to_locale_string(ada_text *text);

/* Free dynamically allocated memory.

   This is a helper to free objects from dynamic languages.  */
extern void
ada_free(void *address);

/* If this text object owns the buffer it references, free this buffer.

   Note that even though this accepts a pointer to a text object, it does not
   deallocates the text object itself but rather the buffer it references.  */
extern void
ada_destroy_text(ada_text *text);

/* Return the text associated to this symbol.  */
extern void
ada_symbol_text(ada_symbol_type *symbol,
                                ada_text *text);

/* Create a big integer from its string representation (in base 10).  */
extern ada_big_integer
ada_create_big_integer(ada_text *text);

/* Return the string representation (in base 10) of this big integer.  */
extern void
ada_big_integer_text(ada_big_integer bigint,
                                     ada_text *text);

/* Decrease the reference count for this big integer.  */
extern void
ada_big_integer_decref(ada_big_integer bigint);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

        



/* Return the scope of definition of this basic declaration.  */
extern int ada_ada_node_p_declarative_scope(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return possible completions at this point in the file.  */
extern int ada_ada_node_p_complete(
    ada_base_entity *node,


    ada_completion_item_array *value_p
);


        



/* Return the list of keywords that are valid at this point in the file.

   .. note:: This is work in progress. It will return all keywords for now,
      without looking at the context.  */
extern int ada_ada_node_p_valid_keywords(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/* Return the potentially empty list of generic package/subprogram
   instantiations that led to the creation of this entity. Outer-most
   instantiations appear last.  */
extern int ada_ada_node_p_generic_instantiations(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return the semantic parent for this node, if applicable, null otherwise.  */
extern int ada_ada_node_p_semantic_parent(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the parent basic decl for this node, if applicable, null otherwise.

   .. note:: If the parent BasicDecl of the given node is a generic
      declaration, this call will return the instantiation from which the node
      was retrieved instead, if any.  */
extern int ada_ada_node_p_parent_basic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Filters out among the list of given units those that cannot refer to the
   unit in which this node lies. If transitive is True, the whole transitive
   closure of imports will be used to find a reference to the unit of this
   node.  */
extern int ada_ada_node_p_filter_is_imported_by(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        transitive,

    ada_analysis_unit_array *value_p
);


        



/* Designates entities that are entry point for the xref solving
   infrastructure. If this returns true, then resolve_names can be called on
   it.

   .. note:: For convenience, and unlike what is defined in the ARM wrt.
      complete contexts for name resolution, ``xref_entry_points`` can be
      nested.  */
extern int ada_ada_node_p_xref_entry_point(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This will resolve names for this node. If the operation is successful, then
   type_var and ref_var will be bound on appropriate subnodes of the
   statement.   */
extern int ada_ada_node_p_resolve_names(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Static method. Return the analysis unit corresponding to the Standard
   package.  */
extern int ada_ada_node_p_standard_unit(
    ada_base_entity *node,


    ada_analysis_unit *value_p
);


        



/* Static property. Return an entity from the standard package with name
   `sym`.   */
extern int ada_ada_node_p_std_entity(
    ada_base_entity *node,

        
        const ada_symbol_type*
        sym,

    ada_base_entity *value_p
);


        



/* Static method. Return the standard Boolean type.  */
extern int ada_ada_node_p_bool_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Static method. Return the standard Integer type.  */
extern int ada_ada_node_p_int_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Static method. Return the standard Universal Integer type.  */
extern int ada_ada_node_p_universal_int_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Static method. Return the standard Universal Real type.  */
extern int ada_ada_node_p_universal_real_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Static method. Get the top-level decl in ``unit``.  This is the body of a
   Subunit, or the item of a ``LibraryItem``.  */
extern int ada_ada_node_p_top_level_decl(
    ada_base_entity *node,

        
        ada_analysis_unit
        unit,

    ada_base_entity *value_p
);


        



/* Assuming that self is a choice expression (such as what can appear in an
   alternative of a case statement or in the RHS of a membership expression,
   this property returns whether the given value satisfies it.

   .. ATTENTION:: This is an experimental feature, so even if it is exposed to
      allow experiments, it is totally unsupported and the API and behavior are
      very likely to change in the future.  */
extern int ada_ada_node_p_choice_match(
    ada_base_entity *node,

        
        const ada_big_integer*
        value,

    ada_bool *value_p
);


        



/* Return a cross reference from this name to a defining identifier, trying to
   mimic GNAT's xrefs as much as possible.  */
extern int ada_ada_node_p_gnat_xref(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/* Return the lexical parent for this node. Return null for the root AST node
   or for AST nodes for which no one has a reference to the parent.  */
extern int ada_ada_node_parent(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return an array that contains the lexical parents (this node included).
   Nearer parents are first in the list.  */
extern int ada_ada_node_parents(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return an array that contains the direct lexical children.  */
extern int ada_ada_node_children(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return the first token used to parse this node.  */
extern int ada_ada_node_token_start(
    ada_base_entity *node,


    ada_token *value_p
);


        



/* Return the last token used to parse this node.  */
extern int ada_ada_node_token_end(
    ada_base_entity *node,


    ada_token *value_p
);


        



/* Return the 0-based index for Node in its parent's children.  */
extern int ada_ada_node_child_index(
    ada_base_entity *node,


    int *value_p
);


        



/* Return the node's previous sibling, if there is one.  */
extern int ada_ada_node_previous_sibling(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the node's next sibling, if there is one.  */
extern int ada_ada_node_next_sibling(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the analysis unit owning this node.  */
extern int ada_ada_node_unit(
    ada_base_entity *node,


    ada_analysis_unit *value_p
);


        



/* Return whether the node is a ghost.

   Unlike regular nodes, ghost nodes cover no token in the input source: they
   are logically located instead between two tokens. The "token_first" of all
   ghost nodes is the token right after this logical position, while they have
   no "token_last".  */
extern int ada_ada_node_is_ghost(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return a string containing the filename + the sloc in GNU conformant format.
   Useful to create diagnostics from a node.  */
extern int ada_ada_node_full_sloc_image(
    ada_base_entity *node,


    ada_text_type *value_p
);


        



/* Return whether this is an instance of AbortPresent  */
extern int ada_abort_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return whether this is an instance of AbstractPresent  */
extern int ada_abstract_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Returns an array of pairs, associating formal parameters to actual
   expressions. The formals to match are retrieved by resolving the call which
   this AssocList represents the actuals of.  */
extern int ada_assoc_list_p_zip_with_params(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_param_actual_array *value_p
);


        



/* Return whether this is an instance of AliasedPresent  */
extern int ada_aliased_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return whether this is an instance of AllPresent  */
extern int ada_all_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_attribute_ref

   * ada_bin_op

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_subtype_indication

   * ada_target_name  */
extern int ada_constrained_array_indices_f_list(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_unconstrained_array_indices_f_types(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_aspect_assoc_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_contract_cases

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_aspect_assoc_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_identifier

   * ada_string_literal  */
extern int ada_at_clause_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_at_clause_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_attribute_def_clause_f_attribute_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_attribute_def_clause_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_enum_rep_clause_f_type_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_enum_rep_clause_f_aggregate(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_record_rep_clause_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_record_rep_clause_f_at_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_component_clause

   * ada_pragma_node  */
extern int ada_record_rep_clause_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_aspect_spec_f_aspect_assocs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns the expression side of this assoc node.  */
extern int ada_base_assoc_p_assoc_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_others_designator

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_contract_case_assoc_f_guard(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_contract_case_assoc_f_consequence(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_pragma_argument_assoc_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_pragma_argument_assoc_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the list of abstract formal parameters for this holder.  */
extern int ada_base_formal_param_holder_p_abstract_formal_params(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return the minimum number of parameters this subprogram can be called while
   still being a legal call.  */
extern int ada_base_formal_param_holder_p_nb_min_params(
    ada_base_entity *node,


    int *value_p
);


        



/* Return the maximum number of parameters this subprogram can be called while
   still being a legal call.  */
extern int ada_base_formal_param_holder_p_nb_max_params(
    ada_base_entity *node,


    int *value_p
);


        



/* Returns the type of each parameter of Self.  */
extern int ada_base_formal_param_holder_p_param_types(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_ada_node_array *value_p
);


        



/* Syntax property. Return the type expression node corresponding to the return
   of this subprogram spec.  */
extern int ada_base_subp_spec_p_returns(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns the array of parameters specification for this subprogram spec.  */
extern int ada_base_subp_spec_p_params(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return the types of which this subprogram is a primitive of.  */
extern int ada_base_subp_spec_p_primitive_subp_types(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return the first type of which this subprogram is a primitive of.  */
extern int ada_base_subp_spec_p_primitive_subp_first_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* If this subprogram is a primitive for a tagged type, then return this
   type.   */
extern int ada_base_subp_spec_p_primitive_subp_tagged_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns the return type of Self, if applicable (e.g. if Self is a
   subprogram). Else, returns null.  */
extern int ada_base_subp_spec_p_return_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        




extern int ada_entry_spec_f_entry_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_bin_op

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_subtype_indication

   * ada_target_name  */
extern int ada_entry_spec_f_family_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_spec_f_entry_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_spec_f_subp_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_spec_f_subp_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_spec_f_subp_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_anonymous_type

   * ada_subtype_indication  */
extern int ada_subp_spec_f_subp_returns(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_component_list_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_component_list_f_variant_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_known_discriminant_part_f_discr_specs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_completion_formal_params_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_generic_formal

   * ada_pragma_node

   * ada_use_clause  */
extern int ada_generic_formal_part_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_record_def_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the list of parameters that this association refers to.  */
extern int ada_basic_assoc_p_get_params(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_discrete_subtype_indication

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_others_designator

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_aggregate_assoc_f_designators(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_aggregate_assoc_f_r_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_discriminant_assoc_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_discriminant_assoc_f_discr_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_identifier

   * ada_others_designator

   * ada_string_literal  */
extern int ada_param_assoc_f_designator(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_param_assoc_f_r_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Whether this decl is the nested decl of a generic formal declaration.  */
extern int ada_basic_decl_p_is_formal(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return the documentation annotations associated with this decl. Annotations
   are any comment line of the form::

   --% [annotation_name]: [annotation]

   Raises a property error if the doc is incorrectly formatted.

   .. ATTENTION:: This is an experimental feature, so even if it is exposed to
      allow experiments, it is totally unsupported and the API and behavior are
      very likely to change in the future.  */
extern int ada_basic_decl_p_doc_annotations(
    ada_base_entity *node,


    ada_doc_annotation_array *value_p
);


        



/* Return the documentation associated with this decl. Raises a property error
   if the doc is incorrectly formatted.

   .. ATTENTION:: This is an experimental feature, so even if it is exposed to
      allow experiments, it is totally unsupported and the API and behavior are
      very likely to change in the future.  */
extern int ada_basic_decl_p_doc(
    ada_base_entity *node,


    ada_text_type *value_p
);


        



/* Return the previous part for this decl, if applicable.

   .. note:: It is not named previous_part, because BaseTypeDecl has a more
      precise version of previous_part that returns a BaseTypeDecl. Probably,
      we want to rename the specific versions, and have the root property be
      named previous_part. (TODO R925-008)  */
extern int ada_basic_decl_p_previous_part_for_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the canonical part for this decl. In the case of decls composed of
   several parts, the canonical part will be the first part.  */
extern int ada_basic_decl_p_canonical_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this declaration is static.  */
extern int ada_basic_decl_p_is_static_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Whether this declaration is imported from another language.  */
extern int ada_basic_decl_p_is_imported(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return the list of aspects that are attached to this node.  */
extern int ada_basic_decl_f_aspects(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the aspect with name ``name`` for this entity.  */
extern int ada_basic_decl_p_get_aspect_assoc(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/* Return the expression associated to the aspect with name ``name`` for this
   entity.  */
extern int ada_basic_decl_p_get_aspect_spec_expr(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/* Return the aspect with name ``name`` associated to this entity.

   Aspects are properties of entities that can be specified by the Ada program,
   either via aspect specifications, pragmas, or attributes.

   This will return the syntactic node corresponding to attribute directly.  */
extern int ada_basic_decl_p_get_aspect(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_internal_aspect *value_p
);


        



/* Returns whether the boolean aspect named ``name`` is set on the entity
   represented by this node.

   "Aspect" is used as in RM terminology (see RM 13).  */
extern int ada_basic_decl_p_has_aspect(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Return the pragma with name ``name`` associated to this entity.  */
extern int ada_basic_decl_p_get_pragma(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/* Return the representation clause associated to this type decl that defines
   the given attribute name.  */
extern int ada_basic_decl_p_get_representation_clause(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/* Whether a BasicDecl is the root decl for its unit.  */
extern int ada_basic_decl_p_is_compilation_unit_root(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return whether this declaration is visible from the point of view of the
   given ``origin`` node.

   .. ATTENTION:: Only package-level (public or private) declarations are
      supported for now.  */
extern int ada_basic_decl_p_is_visible(
    ada_base_entity *node,

        
        const ada_base_entity*
        from_node,

    ada_bool *value_p
);


        



/* If Self declares a primitive subprogram of some tagged type T, return the
   set of all subprogram declarations that it overrides (including itself).  */
extern int ada_basic_decl_p_base_subp_declarations(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* If Self declares a primitive subprogram of some tagged type T, return the
   root subprogram declarations that it overrides. There can be several, as in
   the following scenario:

   - package Root defines the root tagged type T and subprogram Foo.

   - package Itf defines interface I and abstract subprogram Foo.

   - package D defines "type U is new Root.T and Itf.I" and an overriding
   subprogram Foo.

   Here, root_subp_declarations of Foo defined in package D will return both
   Foo from package Root and Foo from package Itf.  */
extern int ada_basic_decl_p_root_subp_declarations(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_ada_node_array *value_p
);


        



/* If Self is the declaration of a primitive of some type T, return the list of
   all subprogram that override this subprogram among the given units.  */
extern int ada_basic_decl_p_find_all_overrides(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/* Get all the names of this basic declaration.  */
extern int ada_basic_decl_p_defining_names(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Get the name of this declaration. If this declaration has several names, it
   will return the first one.  */
extern int ada_basic_decl_p_defining_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the type expression for this BasicDecl if applicable, a null
   otherwise.  */
extern int ada_basic_decl_p_type_expression(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* If Self is a Subp, returns the specification of this subprogram.

   If ``follow_generic`` is True, will also work for instances of
   ``GenericSubpDecl``.  */
extern int ada_basic_decl_p_subp_spec_or_null(
    ada_base_entity *node,

        
        ada_bool
        follow_generic,

    ada_base_entity *value_p
);


        



/* Return True if self is a subprogram node in the general sense (which is, an
   entity that can be called). This includes separates and entries.  */
extern int ada_basic_decl_p_is_subprogram(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return the relative name for Self. If Self's defining name is ``A.B.C``,
   return C as a node.  */
extern int ada_basic_decl_p_relative_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the relative name for Self, as text.  */
extern int ada_basic_decl_p_relative_name_text(
    ada_base_entity *node,


    ada_symbol_type *value_p
);


        



/* Return the next part of this declaration, if applicable.

   .. note:: It is not named next_part, because BaseTypeDecl has a more precise
      version of next_part that returns a BaseTypeDecl. Probably, we want to
      rename the specific versions, and have the root property be named
      next_part. (TODO R925-008)  */
extern int ada_basic_decl_p_next_part_for_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the body corresponding to this declaration, if applicable.

   .. note:: It is not named body_part, subclasses have more precise versions
      named body_part and returning a more precise result. Probably, we want to
      rename the specific versions, and have the root property be named
      previous_part. (TODO R925-008)  */
extern int ada_basic_decl_p_body_part_for_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the fully qualified name corresponding to this declaration, as an
   array of symbols.  */
extern int ada_basic_decl_p_fully_qualified_name_array(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/* Return the fully qualified name corresponding to this declaration.  */
extern int ada_basic_decl_p_fully_qualified_name(
    ada_base_entity *node,


    ada_text_type *value_p
);


        



/* Return a canonical representation of the fully qualified name corresponding
   to this declaration.  */
extern int ada_basic_decl_p_canonical_fully_qualified_name(
    ada_base_entity *node,


    ada_text_type *value_p
);


        



/* Return a unique identifying name for this declaration, provided this
   declaration is a public declaration. In the case of subprograms, this will
   include the profile.

   .. attention:: This will only return a unique name for public declarations.
      Notably, anything nested in an unnamed declare block won't be handled
      correctly.  */
extern int ada_basic_decl_p_unique_identifying_name(
    ada_base_entity *node,


    ada_text_type *value_p
);


        



/* Return the type for this formal.  */
extern int ada_base_formal_param_decl_p_formal_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        




extern int ada_component_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_component_decl_f_component_def(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_component_decl_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_discriminant_spec_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_anonymous_type

   * ada_subtype_indication  */
extern int ada_discriminant_spec_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_discriminant_spec_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_formal_subp_decl

   * ada_generic_instantiation

   * ada_incomplete_type_decl

   * ada_number_decl

   * ada_object_decl

   * ada_single_protected_decl

   * ada_single_task_decl

   * ada_type_decl  */
extern int ada_generic_formal_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_param_spec_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_param_spec_f_has_aliased(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_param_spec_f_mode(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_anonymous_type

   * ada_subtype_indication  */
extern int ada_param_spec_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_param_spec_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_package_decl_f_package_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_package_decl_f_public_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_package_decl_f_private_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_package_decl_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the PackageBody corresponding to this node.  */
extern int ada_base_package_decl_p_body_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_type_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* If this type decl is a subtype decl, return the base subtype. If not, return
   ``Self``.  */
extern int ada_base_type_decl_p_base_subtype(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/* Return the private completion for this type, if there is one.  */
extern int ada_base_type_decl_p_private_completion(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the record representation clause associated to this type decl, if
   applicable (i.e. this type decl defines a record type).  */
extern int ada_base_type_decl_p_get_record_representation_clause(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/* Return the enum representation clause associated to this type decl, if
   applicable (i.e. this type decl defines an enum type).  */
extern int ada_base_type_decl_p_get_enum_representation_clause(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/* Return whether this type is a record type.  */
extern int ada_base_type_decl_p_is_record_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Return whether this type is an array type.  */
extern int ada_base_type_decl_p_is_array_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Find types derived from self in the given ``root`` and its children.  */
extern int ada_base_type_decl_p_find_derived_types(
    ada_base_entity *node,

        
        const ada_base_entity*
        root,
        
        const ada_base_entity*
        origin,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/* Whether type is a real type or not.  */
extern int ada_base_type_decl_p_is_real_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether type is a float type or not.  */
extern int ada_base_type_decl_p_is_float_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether type is a fixed point type or not.  */
extern int ada_base_type_decl_p_is_fixed_point(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether type is an enum type  */
extern int ada_base_type_decl_p_is_enum_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether Self is an access type or not  */
extern int ada_base_type_decl_p_is_access_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether type is a character type or not  */
extern int ada_base_type_decl_p_is_char_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Return the discrete range for this type decl, if applicable.  */
extern int ada_base_type_decl_p_discrete_range(
    ada_base_entity *node,


    ada_internal_discrete_range *value_p
);


        



/* Whether type is a discrete type or not.  */
extern int ada_base_type_decl_p_is_discrete_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether type is an integer type or not.  */
extern int ada_base_type_decl_p_is_int_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* If this type is an access type, or a type with an Implicit_Dereference
   aspect, return the type of a dereference of an instance of this type.  */
extern int ada_base_type_decl_p_accessed_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/* Whether type is tagged or not  */
extern int ada_base_type_decl_p_is_tagged_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Return the base type entity for this derived type declaration  */
extern int ada_base_type_decl_p_base_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/* Return the list of base types for Self.  */
extern int ada_base_type_decl_p_base_types(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_ada_node_array *value_p
);


        



/* Return the list of all types that inherit (directly or inderictly) from Self
   among the given units.  */
extern int ada_base_type_decl_p_find_all_derived_types(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/* Return the component type of `Self`, if applicable. The component type is
   the type you'll get if you call a value whose type is `Self`.  So it can
   either be:

   1. The component type for an array. 2. The return type for an access to
   function.  */
extern int ada_base_type_decl_p_comp_type(
    ada_base_entity *node,

        
        ada_bool
        is_subscript,
        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/* Return the index type for dimension ``dim`` for this type, if
   applicable.   */
extern int ada_base_type_decl_p_index_type(
    ada_base_entity *node,

        
        int
        dim,
        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/* Whether Self is derived from other_type.  */
extern int ada_base_type_decl_p_is_derived_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        other_type,
        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Return True iff this type declaration is an interface definition.  */
extern int ada_base_type_decl_p_is_interface_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Return whether ``self`` matches ``expected_type``.  */
extern int ada_base_type_decl_p_matching_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        expected_type,
        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Return the canonical type declaration for this type declaration. For
   subtypes, it will return the base type declaration.  */
extern int ada_base_type_decl_p_canonical_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/* Returns the previous part for this type decl.  */
extern int ada_base_type_decl_p_previous_part(
    ada_base_entity *node,

        
        ada_bool
        go_to_incomplete,

    ada_base_entity *value_p
);


        



/* Returns the next part for this type decl.  */
extern int ada_base_type_decl_p_next_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the full completion of this type.  */
extern int ada_base_type_decl_p_full_view(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns whether this is a definite subtype.

   For convenience, this will return ``False`` for incomplete types, even
   though the correct answer is more akin to "non applicable".  */
extern int ada_base_type_decl_p_is_definite_subtype(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/* Whether node is a private view of corresponding type.  */
extern int ada_base_type_decl_p_is_private(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return the type that is at the root of the derivation hierarchy (ignoring
   secondary interfaces derivations for tagged types)  */
extern int ada_base_type_decl_p_root_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        




extern int ada_subtype_decl_f_subtype(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_incomplete_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_incomplete_tagged_type_decl_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_protected_type_decl_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_type_decl_f_definition(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_type_decl_f_definition(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_type_decl_f_type_def(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the list of all primitive operations that are available on this type.
   If `only_inherited` is True, it will only return the primitives that are
   implicitly inherited by this type, discarding those explicitly defined on
   this type.  */
extern int ada_type_decl_p_get_primitives(
    ada_base_entity *node,

        
        ada_bool
        only_inherited,

    ada_ada_node_array *value_p
);


        



/* Return the specification for this subprogram  */
extern int ada_basic_subp_decl_p_subp_decl_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the BaseSubpBody corresponding to this node.  */
extern int ada_basic_subp_decl_p_body_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_classic_subp_decl_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_classic_subp_decl_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_box_expr

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_null_literal

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_formal_subp_decl_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_decl_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_decl_f_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_enum_literal_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the enum type corresponding to this enum literal.  */
extern int ada_enum_literal_decl_p_enum_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_internal_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the previous part for this body. Might be a declaration or a body
   stub.  */
extern int ada_body_node_p_previous_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the decl corresponding to this node if applicable.  */
extern int ada_body_node_p_decl_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* If self is a subunit, return the body in which it is rooted.  */
extern int ada_body_node_p_subunit_root(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_subp_body_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_subp_body_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_base_aggregate

   * ada_paren_expr  */
extern int ada_expr_function_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_body_stub_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_body_stub_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_body_stub_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subp_body_stub_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_body_stub_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_body_f_entry_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_body_f_index_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_body_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_entry_body_f_barrier(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_body_f_package_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_body_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_body_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_entry_index_spec_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_bin_op

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_subtype_indication

   * ada_target_name  */
extern int ada_entry_index_spec_f_subtype(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_exception_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_exception_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_exception_handler_f_exception_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_others_designator

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_exception_handler_f_handled_exceptions(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_exception_handler_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_for_loop_var_decl_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_for_loop_var_decl_f_id_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_decl_f_formal_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_package_decl_f_package_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the PackageBody corresponding to this node, or null if there is
   none.   */
extern int ada_generic_package_decl_p_body_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_decl_f_subp_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the BaseSubpBody corresponding to this node.  */
extern int ada_generic_subp_decl_p_body_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the generic decl entity designated by this instantiation, containing
   the generic context. This is equivalent to the expanded generic unit in
   GNAT.  */
extern int ada_generic_instantiation_p_designated_generic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_package_instantiation_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_generic_package_instantiation_f_generic_pkg_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_package_instantiation_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_instantiation_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_instantiation_f_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_instantiation_f_subp_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_generic_subp_instantiation_f_generic_subp_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_instantiation_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the subprogram decl designated by this instantiation.  */
extern int ada_generic_subp_instantiation_p_designated_subp(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_package_renaming_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_generic_package_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_renaming_decl_f_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_generic_subp_renaming_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_generic_subp_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_label_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_named_stmt_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_number_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_number_decl_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_has_aliased(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_has_constant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_mode(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_object_decl_f_renaming_clause(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* If this object decl is the constant completion of an object decl in the
   public part, return the object decl from the public part.  */
extern int ada_object_decl_p_public_part_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_renaming_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_package_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the declaration of the package that is renamed by Self.  */
extern int ada_package_renaming_decl_p_renamed_package(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the declaration of the package that is ultimately renamed by Self,
   skipping through all intermediate package renamings.  */
extern int ada_package_renaming_decl_p_final_renamed_package(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_single_protected_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_single_protected_decl_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_single_protected_decl_f_definition(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_single_task_decl_f_task_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_discrete_subtype_indication

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_others_designator

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_case_stmt_alternative_f_choices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_case_stmt_alternative_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* ``with``, ``use`` or ``pragma`` statements.

   This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_use_clause

   * ada_with_clause  */
extern int ada_compilation_unit_f_prelude(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_library_item

   * ada_subunit  */
extern int ada_compilation_unit_f_body(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_compilation_unit_f_pragmas(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the syntactic fully qualified name of this compilation unit.  */
extern int ada_compilation_unit_p_syntactic_fully_qualified_name(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/* Return the kind corresponding to this analysis unit.  */
extern int ada_compilation_unit_p_unit_kind(
    ada_base_entity *node,


    ada_analysis_unit_kind *value_p
);


        



/* Look for all "with" clauses at the top of this compilation unit and return
   all the compilation units designated by them.  */
extern int ada_compilation_unit_p_withed_units(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return all the compilation units that are directly imported by this one.
   This includes "with"ed units as well as the direct parent unit.  */
extern int ada_compilation_unit_p_imported_units(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return the list of all the compilation units that are (direct and indirect)
   dependencies of this one.  */
extern int ada_compilation_unit_p_unit_dependencies(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Get the root basic decl defined in this compilation unit.  */
extern int ada_compilation_unit_p_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Whether this compilation unit is preelaborable or not.  */
extern int ada_compilation_unit_p_is_preelaborable(
    ada_base_entity *node,


    ada_bool *value_p
);


        




extern int ada_component_clause_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_component_clause_f_position(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_component_clause_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_component_def_f_has_aliased(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_component_def_f_has_constant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_anonymous_type

   * ada_subtype_indication  */
extern int ada_component_def_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of ConstantPresent  */
extern int ada_constant_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_delta_constraint_f_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_delta_constraint_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_digits_constraint_f_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_digits_constraint_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_discriminant_constraint_f_constraints(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_attribute_ref

   * ada_bin_op

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_subtype_indication

   * ada_target_name  */
extern int ada_index_constraint_f_constraints(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_range_constraint_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_abstract_subp_decl

   * ada_aspect_clause

   * ada_body_node

   * ada_component_decl

   * ada_entry_decl

   * ada_error_decl

   * ada_exception_decl

   * ada_generic_decl

   * ada_generic_instantiation

   * ada_generic_renaming_decl

   * ada_incomplete_type_decl

   * ada_number_decl

   * ada_object_decl

   * ada_package_decl

   * ada_package_renaming_decl

   * ada_pragma_node

   * ada_protected_type_decl

   * ada_single_protected_decl

   * ada_single_task_decl

   * ada_subp_decl

   * ada_subtype_decl

   * ada_task_type_decl

   * ada_type_decl

   * ada_use_clause  */
extern int ada_declarative_part_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_elsif_expr_part_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_elsif_expr_part_f_then_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_elsif_stmt_part_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_elsif_stmt_part_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the declaration corresponding to the type of this expression after
   name resolution.  */
extern int ada_expr_p_expression_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this expression is static according to the ARM definition of
   static. See RM 4.9.  */
extern int ada_expr_p_is_static_expr(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Return the first decl that is lexically named like self in self's scope.  */
extern int ada_expr_p_first_corresponding_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Statically evaluates self, and returns the value of the evaluation as an
   integer.

   .. note:: In order for a call to this not to raise, the expression needs to
      be a static expression, as specified in the ARM section 4.9. You can
      verify whether an expression is static with the ``is_static_expr``
      property.

   .. ATTENTION:: This is an experimental feature, so even if it is exposed to
      allow experiments, it is totally unsupported and the API and behavior are
      very likely to change in the future.  */
extern int ada_expr_p_eval_as_int(
    ada_base_entity *node,


    ada_big_integer *value_p
);


        



/* Statically evaluates self, and returns the value of the evaluation as an
   integer. The given environment is used to substitute references to
   declarations by actual values.

   .. note:: In order for a call to this not to raise, the expression needs to
      be a static expression, as specified in the ARM section 4.9. You can
      verify whether an expression is static with the ``is_static_expr``
      property.

   .. ATTENTION:: This is an experimental feature, so even if it is exposed to
      allow experiments, it is totally unsupported and the API and behavior are
      very likely to change in the future.  */
extern int ada_expr_p_eval_as_int_in_env(
    ada_base_entity *node,

        
        ada_substitution_array
        env,

    ada_big_integer *value_p
);


        



/* Return the list of AST nodes that can be a match for this expression before
   overloading analysis.  */
extern int ada_expr_p_matching_nodes(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_allocator_f_subpool(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_qual_expr

   * ada_subtype_indication  */
extern int ada_allocator_f_type_or_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the allocated type for this allocator.  */
extern int ada_allocator_p_get_allocated_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_aggregate_f_ancestor_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_aggregate_f_assocs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_bin_op_f_left(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_op_and

   * ada_op_and_then

   * ada_op_concat

   * ada_op_div

   * ada_op_double_dot

   * ada_op_eq

   * ada_op_gt

   * ada_op_gte

   * ada_op_lt

   * ada_op_lte

   * ada_op_minus

   * ada_op_mod

   * ada_op_mult

   * ada_op_neq

   * ada_op_or

   * ada_op_or_else

   * ada_op_plus

   * ada_op_pow

   * ada_op_rem

   * ada_op_xor  */
extern int ada_bin_op_f_op(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_bin_op_f_right(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_case_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_case_expr_f_cases(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_discrete_subtype_indication

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_others_designator

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_case_expr_alternative_f_choices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_case_expr_alternative_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_contract_cases_f_contract_cases(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_if_expr_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_if_expr_f_then_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_if_expr_f_alternatives(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_if_expr_f_else_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_membership_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_op_in

   * ada_op_not_in  */
extern int ada_membership_expr_f_op(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_discrete_subtype_name

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_membership_expr_f_membership_exprs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* If this name is part of a defining name, return the enclosing defining name
   node.  */
extern int ada_name_p_enclosing_defining_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return True if this name is part of a defining name.  */
extern int ada_name_p_is_defining(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Helper. Check that this name matches ``sym``.  */
extern int ada_name_p_name_is(
    ada_base_entity *node,

        
        const ada_symbol_type*
        sym,

    ada_bool *value_p
);


        



/* Return True iff this name represents a call to a subprogram which is
   referred by its defining name. (i.e. not through a subprogram access).  */
extern int ada_name_p_is_direct_call(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return True iff this name represents a call to subprogram through an access
   type.  */
extern int ada_name_p_is_access_call(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Returns True if this Name corresponds to a call.  */
extern int ada_name_p_is_call(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Returns True if this Name corresponds to a dot notation call.  */
extern int ada_name_p_is_dot_call(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Failsafe version of ``referenced_defining_name``. Returns a ``RefdDef``,
   which can be precise, imprecise, or error.  */
extern int ada_name_p_failsafe_referenced_def_name(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_internal_refd_def *value_p
);


        



/* Like ``referenced_decl``, but will return the defining identifier for the
   decl, rather than the basic declaration node itself.  */
extern int ada_name_p_referenced_defining_name(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/* Return all elements in self's scope that are lexically named like Self.  */
extern int ada_name_p_all_env_elements(
    ada_base_entity *node,

        
        ada_bool
        seq,
        
        const ada_base_entity*
        seq_from,

    ada_ada_node_array *value_p
);


        



/* Return the subprogram specification of the subprogram or subprogram access
   that is being called by this exact Name, if relevant.  */
extern int ada_name_p_called_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the declaration this node references after name resolution. If
   imprecise_fallback is True, errors raised during resolution of the xref
   equation are catched and a fallback mechanism is triggered, which tries to
   find the referenced declaration in an ad-hoc way.  */
extern int ada_name_p_referenced_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/* Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which can
   be precise, imprecise, or error.  */
extern int ada_name_p_failsafe_referenced_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_internal_refd_decl *value_p
);


        



/* Return the declaration this node references. Try not to run name res if
   already resolved. INTERNAL USE ONLY.  */
extern int ada_name_p_referenced_decl_internal(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_internal_refd_decl *value_p
);


        



/* Like SubtypeIndication.designated_type, but on names, since because of Ada's
   ambiguous grammar, some subtype indications will be parsed as names.  */
extern int ada_name_p_name_designated_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns whether Self denotes a static subtype or not.  */
extern int ada_name_p_is_static_subtype(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Return whether two names match each other.

   This compares the symbol for Identifier and StringLiteral nodes. We consider
   that there is no match for all other node kinds.  */
extern int ada_name_p_name_matches(
    ada_base_entity *node,

        
        const ada_base_entity*
        n,

    ada_bool *value_p
);


        



/* Returns the relative name of this instance. For example, for a prefix A.B.C,
   this will return C.  */
extern int ada_name_p_relative_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether the name that Self designates is an operator.  */
extern int ada_name_p_is_operator_name(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Whether this name is a write reference.

   For example, `X` is a write reference in the following cases::

   1. `X := 2;` 2. `X (2) := 2;` 3. `P(F => X)` where F is declared `out` or
   `in out`. 4. `X'Access`. 5. `X.C := 2`, `R.X := 2`

   .. note:: This is an experimental feature. There might be some discrepancy
      with the GNAT concept of "write reference".  */
extern int ada_name_p_is_write_reference(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Returns True if this Name corresponds to a dispatching call, including:

   - calls done through subprogram access types.

   - calls to dispatching subprograms, in the object-oriented sense.

   .. note:: This is an experimental feature. There might be some discrepancy
      with the GNAT concept of "dispatching call".  */
extern int ada_name_p_is_dispatching_call(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Returns True if this Name corresponds to a static non-dispatching call. In
   other words, this will return True if and only if the target of the call is
   known statically.

   .. note:: This is an experimental feature. There might be some discrepancy
      with the GNAT concept of "static call".  */
extern int ada_name_p_is_static_call(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/* Turn this name into an array of symbols.

   For instance, a node with name ``A.B.C`` is turned into ``['A', 'B',
   'C']``.   */
extern int ada_name_p_as_symbol_array(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_attribute_ref_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_attribute_ref_f_attribute(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_attribute_ref_f_args(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_call_expr_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_basic_assoc_list

   * ada_bin_op

   * ada_call_expr

   * ada_char_literal

   * ada_discrete_subtype_indication

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_call_expr_f_suffix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this CallExpr is actually an access to a slice of the array
   denoted by the prefix of this CallExpr.  */
extern int ada_call_expr_p_is_array_slice(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_defining_name_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns this DefiningName's basic declaration  */
extern int ada_defining_name_p_basic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Find all references to this defining name in the given ``root`` and its
   children.  */
extern int ada_defining_name_p_find_refs(
    ada_base_entity *node,

        
        const ada_base_entity*
        root,
        
        const ada_base_entity*
        origin,
        
        ada_bool
        imprecise_fallback,

    ada_ref_result_array *value_p
);


        



/* Searches all references to this defining name in the given list of
   units.   */
extern int ada_defining_name_p_find_all_references(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        imprecise_fallback,

    ada_ref_result_array *value_p
);


        



/* Return the list of all possible calls to the subprogram which Self is the
   defining name of.

   This will return the name corresponding to the call, excluding the
   parameters if there are any. For instance, it will return `A` for the `A
   (B)` call.

   .. note:: This does not yet support calls done inside generics.  */
extern int ada_defining_name_p_find_all_calls(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        imprecise_fallback,

    ada_ref_result_array *value_p
);


        



/* Like ``BasicDecl.next_part_for_decl`` on a defining name  */
extern int ada_defining_name_p_next_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Like ``BasicDecl.previous_part_for_decl`` on a defining name  */
extern int ada_defining_name_p_previous_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Like ``BasicDecl.canonical_part`` on a defining name  */
extern int ada_defining_name_p_canonical_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_discrete_subtype_name_f_subtype(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_dotted_name_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_identifier

   * ada_string_literal  */
extern int ada_dotted_name_f_suffix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_end_name_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns this EndName's basic declaration  */
extern int ada_end_name_p_basic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_explicit_deref_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_qual_expr_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_base_aggregate

   * ada_paren_expr  */
extern int ada_qual_expr_f_suffix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return a canonicalized version of this node's text.  */
extern int ada_single_tok_node_p_canonical_text(
    ada_base_entity *node,


    ada_symbol_type *value_p
);


        



/* Return the value that this literal denotes.  */
extern int ada_char_literal_p_denoted_value(
    ada_base_entity *node,


    uint32_t *value_p
);


        



/* Return the value that this literal denotes.  */
extern int ada_string_literal_p_denoted_value(
    ada_base_entity *node,


    ada_text_type *value_p
);


        



/* Return the value that this literal denotes.  */
extern int ada_int_literal_p_denoted_value(
    ada_base_entity *node,


    ada_big_integer *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_paren_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_quantified_expr_f_quantifier(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_quantified_expr_f_loop_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_quantified_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_raise_expr_f_exception_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_raise_expr_f_error_message(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_op_abs

   * ada_op_minus

   * ada_op_not

   * ada_op_plus  */
extern int ada_un_op_f_op(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_un_op_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_handled_stmts_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_exception_handler

   * ada_pragma_node  */
extern int ada_handled_stmts_f_exceptions(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_library_item_f_has_private(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_abstract_subp_decl

   * ada_base_subp_body

   * ada_error_decl

   * ada_generic_decl

   * ada_generic_instantiation

   * ada_generic_renaming_decl

   * ada_package_body

   * ada_package_decl

   * ada_package_renaming_decl

   * ada_subp_decl  */
extern int ada_library_item_f_item(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of LimitedPresent  */
extern int ada_limited_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        




extern int ada_for_loop_spec_f_var_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_for_loop_spec_f_loop_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_for_loop_spec_f_has_reverse(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_bin_op

   * ada_call_expr

   * ada_char_literal

   * ada_discrete_subtype_indication

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_for_loop_spec_f_iter_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_while_loop_spec_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of NotNullPresent  */
extern int ada_not_null_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        




extern int ada_params_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_pragma_node_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_pragma_node_f_args(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return an array of ``BasicDecl`` instances associated with this pragma, or
   an empty array if non applicable.  */
extern int ada_pragma_node_p_associated_decls(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/* Return whether this is an instance of PrivatePresent  */
extern int ada_private_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        




extern int ada_protected_def_f_public_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_def_f_private_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_protected_def_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of ProtectedPresent  */
extern int ada_protected_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_range_spec_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_renaming_clause_f_renamed_object(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of ReversePresent  */
extern int ada_reverse_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_select_when_part_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_select_when_part_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_accept_stmt_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_accept_stmt_f_entry_index_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_accept_stmt_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_accept_stmt_with_stmts_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_accept_stmt_with_stmts_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_loop_stmt_f_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_base_loop_stmt_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_base_loop_stmt_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_begin_block_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_begin_block_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_decl_block_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_decl_block_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_decl_block_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_case_stmt_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_case_stmt_f_alternatives(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_extended_return_stmt_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_extended_return_stmt_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_if_stmt_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_if_stmt_f_then_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_if_stmt_f_alternatives(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_if_stmt_f_else_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_named_stmt_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_base_loop_stmt

   * ada_block_stmt  */
extern int ada_named_stmt_f_stmt(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_select_stmt_f_guards(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_select_stmt_f_else_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_pragma_node

   * ada_stmt  */
extern int ada_select_stmt_f_abort_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_abort_stmt_f_names(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_assign_stmt_f_dest(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_assign_stmt_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_call_stmt_f_call(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_delay_stmt_f_has_until(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_delay_stmt_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_exit_stmt_f_loop_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_exit_stmt_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_goto_stmt_f_label_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_label_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_raise_stmt_f_exception_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_raise_stmt_f_error_message(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_requeue_stmt_f_call_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_requeue_stmt_f_has_abort(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_return_stmt_f_return_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_subunit_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_package_body

   * ada_protected_body

   * ada_subp_body

   * ada_task_body  */
extern int ada_subunit_f_body(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the body in which this subunit is rooted.  */
extern int ada_subunit_p_body_root(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of SynchronizedPresent  */
extern int ada_synchronized_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* Return whether this is an instance of TaggedPresent  */
extern int ada_tagged_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_task_def_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_def_f_public_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_def_f_private_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_task_def_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_access_def_f_has_not_null(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_access_to_subp_def_f_has_protected(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_access_to_subp_def_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_anonymous_type_access_def_f_type_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_type_access_def_f_has_all(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_type_access_def_f_has_constant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_type_access_def_f_subtype_indication(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_array_type_def_f_indices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_array_type_def_f_component_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_derived_type_def_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_derived_type_def_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_derived_type_def_f_has_synchronized(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_derived_type_def_f_subtype_indication(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_derived_type_def_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_derived_type_def_f_record_extension(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_derived_type_def_f_has_with_private(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_enum_type_def_f_enum_literals(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_interface_type_def_f_interface_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_interface_type_def_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_mod_int_type_def_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_private_type_def_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_private_type_def_f_has_tagged(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_private_type_def_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_decimal_fixed_point_def_f_delta(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_decimal_fixed_point_def_f_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_decimal_fixed_point_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_floating_point_def_f_num_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_floating_point_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_box_expr

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_null_literal

   * ada_num_literal

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_ordinary_fixed_point_def_f_delta(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_ordinary_fixed_point_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_record_type_def_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_record_type_def_f_has_tagged(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_record_type_def_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_record_type_def_f_record_def(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_signed_int_type_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the name node for this type expression, if applicable, else null  */
extern int ada_type_expr_p_type_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns the type declaration designated by this type expression.  */
extern int ada_type_expr_p_designated_type_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return the type declaration designated by this type expression as viewed
   from the node given by origin_node.  */
extern int ada_type_expr_p_designated_type_decl_from(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin_node,

    ada_base_entity *value_p
);


        




extern int ada_anonymous_type_f_type_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subtype_indication_f_has_not_null(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * ada_attribute_ref

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_subtype_indication_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_subtype_indication_f_constraint(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Returns whether Self denotes a static subtype or not.  */
extern int ada_subtype_indication_p_is_static_subtype(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        




extern int ada_unconstrained_array_index_f_subtype_indication(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of UntilPresent  */
extern int ada_until_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_use_package_clause_f_packages(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_use_type_clause_f_has_all(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_attribute_ref

   * ada_call_expr

   * ada_char_literal

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_qual_expr

   * ada_string_literal

   * ada_target_name  */
extern int ada_use_type_clause_f_types(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_allocator

   * ada_attribute_ref

   * ada_base_aggregate

   * ada_bin_op

   * ada_call_expr

   * ada_case_expr

   * ada_char_literal

   * ada_discrete_subtype_indication

   * ada_dotted_name

   * ada_explicit_deref

   * ada_identifier

   * ada_if_expr

   * ada_membership_expr

   * ada_null_literal

   * ada_num_literal

   * ada_others_designator

   * ada_paren_expr

   * ada_qual_expr

   * ada_quantified_expr

   * ada_raise_expr

   * ada_string_literal

   * ada_target_name

   * ada_un_op  */
extern int ada_variant_f_choices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_variant_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_variant_part_f_discr_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_variant_part_f_variant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_with_clause_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        




extern int ada_with_clause_f_has_private(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * ada_char_literal

   * ada_dotted_name

   * ada_identifier

   * ada_string_literal  */
extern int ada_with_clause_f_packages(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/* Return whether this is an instance of WithPrivatePresent  */
extern int ada_with_private_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);



/*
 * Unit providers
 */

/* Create a unit provider. When done with it, the result must be passed to
   ``ada_destroy_unit_provider``.

   Pass as ``data`` a pointer to hold your private data: it will be passed to
   all callbacks below.

   ``destroy`` is a callback that is called by ``ada_destroy_unit_provider`` to
   leave a chance to free resources that ``data`` may hold.

   ``get_unit_from_node`` is a callback. It turns an analysis unit reference
   represented as a node into an analysis unit. It should return ``NULL`` if
   the node is not a valid unit name representation.

   ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
   except it takes an analysis unit reference represented as a string.  */
extern ada_unit_provider
ada_create_unit_provider(
   void *data,
   ada_unit_provider_destroy_callback destroy_func,
   ada_unit_provider_get_unit_filename_callback get_unit_filename_func,
   ada_unit_provider_get_unit_from_name_callback get_unit_from_name_func
);

/* Release an ownership share for this unit provider. This destroys the unit
   provider if there are no shares left.  */
extern void
ada_dec_ref_unit_provider(void *data);


      


/* Couple name/value to define a scenario variable for a project.  */
typedef struct {
   char *name;
   char *value;
} ada_project_scenario_variable;

/* Load the project file at ``Project_File`` and return a unit provider that
   uses it.

   If ``Project`` is passed, use it to provide units, otherwise, try use the
   whole project tree.

   As unit providers must guarantee that there exists at most one source file
   for each couple (unit name, unit kind), aggregate projects that contains
   several conflicting units are not supported: trying to load one will yield
   an error (see below).

   If not ``NULL``, ``Scenario_Vars`` must point to an array of
   ``ada_project_scenario_variable`` couples to provide scenario variables for
   this project. The last element of this array must end with a ``{ NULL, NULL
   }`` couple.

   If not ``NULL``, ``target`` and ``runtime`` must point to valid NULL-
   terminated strings.

   When done with it, the result must be free'd with
   ``ada_destroy_unit_provider``.

   If the requested project is invalid (error while opening the file, error
   while analysing its syntax, ...), or if it is an unsupported aggregate
   project, this returns ``NULL``.  */
extern ada_unit_provider
ada_create_project_unit_provider(
   char *project_file,
   char *project,
   ada_project_scenario_variable *scenario_vars,
   char *target,
   char *runtime
);

/* Return a unit provider that knows which compilation units are to be found in
   the given list of source files.

   This knowledge is built trying to parse all given input files as Ada source
   files and listing the compilation units found there. Files that cannot be
   parsed properly are discarded. If two compilation units are found for the
   same unit, the first that is found in the given input files is taken and the
   other ones are discarded.

   Source files are decoded using the given charset. If it is ``NULL``, the
   default charset (ISO-8859-1) is used.

   `input_files` must point to a ``NULL``-terminated array of filenames.  Once
   this function returns, this array and the strings it contains can be
   deallocated.

   When done with it, the result must be free'd with
   ``ada_destroy_unit_provider``.

   .. todo:: Find a way to report discarded source files/compilation units.  */
extern ada_unit_provider
ada_create_auto_provider(
   const char **input_files,
   const char *charset
);



/*
 * Misc
 */

/* Return exception information for the last error that happened in the current
   thread. Will be automatically allocated on error and free'd on the next
   error.  */
extern const ada_exception *
ada_get_last_exception(void);

/* Return a human-readable name for a token kind.

   The returned string is dynamically allocated and the caller must free it
   when done with it.

   If the given kind is invalid, return ``NULL`` and set the last exception
   accordingly.  */
extern char *
ada_token_kind_name(ada_token_kind kind);

/* Return a reference to the next token in the corresponding analysis unit.  */
extern void
ada_token_next(ada_token *token,
                               ada_token *next_token);

/* Return a reference to the previous token in the corresponding analysis
   unit.   */
extern void
ada_token_previous(ada_token *token,
                                   ada_token *previous_token);

/* Compute the source buffer slice corresponding to the text that spans between
   the ``First`` and ``Last`` tokens (both included). This yields an empty
   slice if ``Last`` actually appears before ``First``. Put the result in
   ``RESULT``.

   This returns 0 if ``First`` and ``Last`` don't belong to the same analysis
   unit. Return 1 if successful.  */
extern int
ada_token_range_text(ada_token *first,
                                     ada_token *last,
                                     ada_text *result);

/* Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   means that their position in the stream won't be taken into account, only
   the kind and text of the token.  */
extern void
ada_token_is_equivalent(ada_token *left,
                                        ada_token *right);

/* Return a representation of this entity as a string.  */
extern void
ada_entity_image(ada_base_entity ent, ada_text *result);

#ifdef __cplusplus
}
#endif

#endif
