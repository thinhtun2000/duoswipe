with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

private package Libadalang.Implementation.C is

   subtype ada_analysis_context is Internal_Context;
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
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   subtype ada_analysis_unit is Internal_Unit;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   type ada_base_node is new System.Address;
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

   type ada_node_kind_enum is new int;
   --  Kind of AST nodes in parse trees.

   subtype ada_base_entity is Internal_Entity;
   type ada_base_entity_Ptr is access Internal_Entity;

   type ada_symbol_type is record
      Data, Bounds : System.Address;
   end record with
      Convention => C;
      --  Reference to a symbol. Symbols are owned by analysis contexts, so
      --  they must not outlive them. This type exists only in the C API, and
      --  roughly wraps the corresponding Ada type (an array fat pointer).

      --  Helper data structures for source location handling

   type ada_source_location is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record with
      Convention => C;

   type ada_source_location_range is record
      Start_S, End_S : ada_source_location;
   end record with
      Convention => C;

   type ada_text is record
      Chars : System.Address;
      --  Address for the content of the string.

      Length : size_t;
      --  Size of the string (in characters).

      Is_Allocated : int;
   end record with
      Convention => C;
      --  String encoded in UTF-32 (native endianness).

   type ada_big_integer is new System.Address;
   --  Arbitrarily large integer.

   type ada_token is record
      Token_Data                : System.Address;
      Token_Index, Trivia_Index : int;

      Kind       : int;
      Text       : ada_text;
      Sloc_Range : ada_source_location_range;
   end record with
      Convention => C;
      --  Reference to a token in an analysis unit.

   type ada_diagnostic is record
      Sloc_Range : ada_source_location_range;
      Message    : ada_text;
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record with
      Convention => C;
      --  Diagnostic for an analysis unit: cannot open the source file, parsing
      --  error, ...

   type ada_exception_kind is
     (Exception_Invalid_Field, Exception_Node_Data_Evaluation_Error,
      Exception_Invalid_Input, Exception_Invalid_Symbol_Error,
      Exception_Invalid_Unit_Name_Error, Exception_Native_Exception,
      Exception_Precondition_Failure, Exception_Property_Error,
      Exception_Template_Args_Error, Exception_Template_Format_Error,
      Exception_Template_Instantiation_Error, Exception_Stale_Reference_Error,
      Exception_Unknown_Charset) with
      Convention => C;
      --  Enumerated type describing all possible exceptions that need to be
      --  handled in the C bindings.

   type ada_exception is record
      Kind : ada_exception_kind;
      --  The kind of this exception.

      Information : chars_ptr;
      --  Message and context information associated with this exception.
   end record;
   --  Holder for native exceptions-related information. Memory management for
   --  this and all the fields is handled by the library: one just has to make
   --  sure not to keep references to it.
   --
   --  .. todo:: For the moment, this structure contains already formatted
   --     information, but depending on possible future Ada runtime
   --     improvements, this might change.

   type ada_exception_Ptr is access ada_exception;

   type ada_bool is new Unsigned_8;
   subtype uint32_t is Unsigned_32;

   subtype ada_analysis_unit_kind is Analysis_Unit_Kind;
   subtype ada_lookup_kind is Lookup_Kind;
   subtype ada_find_all_mode is Find_All_Mode;
   subtype ada_ref_result_kind is Ref_Result_Kind;
   subtype ada_grammar_rule is Grammar_Rule;

   procedure Free (Address : System.Address) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_free";
      --  Free dynamically allocated memory.
      --
      --  This is a helper to free objects from dynamic languages. Helper to
      --  free objects in dynamic languages

   procedure ada_destroy_text (T : access ada_text) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_destroy_text";
      --  If this text object owns the buffer it references, free this buffer.
      --
      --  Note that even though this accepts a pointer to a text object, it
      --  does not deallocates the text object itself but rather the buffer
      --  it references.

   procedure ada_symbol_text
     (Symbol : access ada_symbol_type; Text : access ada_text) with
      Export,
      Convention    => C,
      External_Name => "ada_symbol_text";
      --  Return the text associated to this symbol.

   function ada_create_big_integer
     (Text : access ada_text) return ada_big_integer with
      Export,
      Convention    => C,
      External_Name => "ada_create_big_integer";
      --  Create a big integer from its string representation (in base 10).

   procedure ada_big_integer_text
     (Bigint : ada_big_integer; Text : access ada_text) with
      Export,
      Convention    => C,
      External_Name => "ada_big_integer_text";
      --  Return the string representation (in base 10) of this big integer.

   procedure ada_big_integer_decref (Bigint : ada_big_integer) with
      Export,
      Convention    => C,
      External_Name => "ada_big_integer_decref";
      --  Decrease the reference count for this big integer.

      --  Types for unit providers

   type ada_unit_provider is new System.Address;
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

   type ada_unit_provider_destroy_callback is access procedure
     (Data : System.Address) with
      Convention => C;
      --  Callback type for functions that are called when destroying a unit
      --  file provider type.

   type ada_unit_provider_get_unit_filename_callback is access function
     (Data : System.Address; Name : ada_text; Kind : ada_analysis_unit_kind)
      return chars_ptr with
      Convention => C;
      --  Callback type for functions that are called to turn a unit reference
      --  encoded as a unit name into an analysis unit.

   type ada_unit_provider_get_unit_from_name_callback is access function
     (Data : System.Address; Context : ada_analysis_context; Name : ada_text;
      Kind : ada_analysis_unit_kind; Charset : chars_ptr; Reparse : int)
      return ada_analysis_unit with
      Convention => C;
      --  Callback type for functions that are called to turn a unit reference
      --  encoded as a unit name into an analysis unit.

      -------------------------
      -- Analysis primitives --
      -------------------------

   function ada_create_analysis_context
     (Charset     : chars_ptr; Unit_Provider : ada_unit_provider;
      With_Trivia : int; Tab_Stop : int) return ada_analysis_context with
      Export        => True,
      Convention    => C,
      External_Name => "ada_create_analysis_context";
      --  Create a new analysis context.
      --
      --  ``Charset`` will be used as a default charset to decode input
      --  sources in analysis units. Please see ``GNATCOLL.Iconv`` for several
      --  supported charsets. Be careful: passing an unsupported charset is
      --  not guaranteed to raise an error here. If no charset is provided,
      --  ``"iso-8859-1"`` is the default.
      --
      --  .. todo:: Passing an unsupported charset here is not guaranteed to
      --  raise
      --     an error right here, but this would be really helpful for users.
      --
      --  When ``With_Trivia`` is true, the parsed analysis units will contain
      --  trivias.
      --
      --  If provided, ``Unit_Provider`` will be used to query the file name
      --  that corresponds to a unit reference during semantic analysis. If
      --  it is ``NULL``, the default one is used instead.
      --
      --  ``Tab_Stop`` is a positive number to describe the effect of
      --  tabulation characters on the column number in source files.

   function ada_context_incref
     (Context : ada_analysis_context) return ada_analysis_context with
      Export        => True,
      Convention    => C,
      External_Name => "ada_context_incref";
      --  Increase the reference count to an analysis context. Return the
      --  reference for convenience.

   procedure ada_context_decref (Context : ada_analysis_context) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_context_decref";
      --  Decrease the reference count to an analysis context. Destruction
      --  happens when the ref-count reaches 0.

   function ada_context_symbol
     (Context : ada_analysis_context; Text : access ada_text;
      Symbol  : access ada_symbol_type) return int with
      Export,
      Convention    => C,
      External_Name => "ada_context_symbol";
      --  If the given string is a valid symbol, yield it as a symbol and
      --  return true. Otherwise, return false.

   procedure ada_context_discard_errors_in_populate_lexical_env
     (Context : ada_analysis_context; Discard : int) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_context_discard_errors_in_populate_lexical_env";
      --  Debug helper. Set whether ``Property_Error`` exceptions raised in
      --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   function ada_get_analysis_unit_from_file
     (Context : ada_analysis_context; Filename, Charset : chars_ptr;
      Reparse : int; Rule : ada_grammar_rule) return ada_analysis_unit with
      Export        => True,
      Convention    => C,
      External_Name => "ada_get_analysis_unit_from_file";
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

   function ada_get_analysis_unit_from_buffer
     (Context : ada_analysis_context; Filename, Charset : chars_ptr;
      Buffer  : chars_ptr; Buffer_Size : size_t; Rule : ada_grammar_rule)
      return ada_analysis_unit with
      Export        => True,
      Convention    => C,
      External_Name => "ada_get_analysis_unit_from_buffer";
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

   function ada_get_analysis_unit_from_provider
     (Context : ada_analysis_context; Name : ada_text;
      Kind    : ada_analysis_unit_kind; Charset : chars_ptr; Reparse : int)
      return ada_analysis_unit with
      Export        => True,
      Convention    => C,
      External_Name => "ada_get_analysis_unit_from_provider";
      --  Create a new analysis unit for ``Name``/``Kind`` or return the
      --  existing one if any. If ``Reparse`` is true and the analysis unit
      --  already exists, reparse it from ``Filename``.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If the unit name cannot be tuned into a file name, return ``NULL``.
      --  If any other failure occurs, such as file opening, decoding, lexing
      --  or parsing failure, return an analysis unit anyway: errors are
      --  described as diagnostics of the returned analysis unit.

   procedure ada_unit_root
     (Unit : ada_analysis_unit; Result_P : ada_base_entity_Ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_root";
      --  Return the root node for this unit, or ``NULL`` if there is none.

   procedure ada_unit_first_token
     (Unit : ada_analysis_unit; Token : access ada_token) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_first_token";
      --  Return a reference to the first token scanned in this unit.

   procedure ada_unit_last_token
     (Unit : ada_analysis_unit; Token : access ada_token) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_last_token";
      --  Return a reference to the last token scanned in this unit.

   function ada_unit_token_count (Unit : ada_analysis_unit) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_token_count";
      --  Return the number of tokens in this unit.

   function ada_unit_trivia_count (Unit : ada_analysis_unit) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_trivia_count";
      --  Return the number of trivias in this unit. This is 0 for units that
      --  were parsed with trivia analysis disabled.

   procedure ada_unit_lookup_token
     (Unit   : ada_analysis_unit; Sloc : access ada_source_location;
      Result : access ada_token) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_lookup_token";
      --  Look for a token in this unit that contains the given source
      --  location. If this falls before the first token, return the first
      --  token. If this falls between two tokens, return the token that
      --  appears before. If this falls after the last token, return the
      --  last token. If there is no token in this unit, return no token.

   procedure ada_unit_dump_lexical_env (Unit : ada_analysis_unit) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_dump_lexical_env";

   function ada_unit_filename (Unit : ada_analysis_unit) return chars_ptr with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_filename";
      --  Return the filename this unit is associated to.
      --
      --  The returned string is dynamically allocated and the caller must free
      --  it when done with it.

   function ada_unit_diagnostic_count
     (Unit : ada_analysis_unit) return unsigned with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_diagnostic_count";
      --  Return the number of diagnostics associated to this unit.

   function ada_unit_diagnostic
     (Unit         : ada_analysis_unit; N : unsigned;
      Diagnostic_P : access ada_diagnostic) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_diagnostic";
      --  Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P.
      --  Return zero on failure (when N is too big).

   function ada_unit_context
     (Unit : ada_analysis_unit) return ada_analysis_context with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_context";
      --  Return the context that owns this unit.

   procedure ada_unit_reparse_from_file
     (Unit : ada_analysis_unit; Charset : chars_ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_reparse_from_file";
      --  Reparse an analysis unit from the associated file.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If any failure occurs, such as decoding, lexing or parsing failure,
      --  diagnostic are emitted to explain what happened.

   procedure ada_unit_reparse_from_buffer
     (Unit        : ada_analysis_unit; Charset : chars_ptr; Buffer : chars_ptr;
      Buffer_Size : size_t) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_reparse_from_buffer";
      --  Reparse an analysis unit from a buffer.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If any failure occurs, such as decoding, lexing or parsing failure,
      --  diagnostic are emitted to explain what happened.

   function ada_unit_populate_lexical_env
     (Unit : ada_analysis_unit) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unit_populate_lexical_env";
      --  Create lexical environments for this analysis unit, according to the
      --  specifications given in the language spec.
      --
      --  If not done before, it will be automatically called during semantic
      --  analysis. Calling it before enables one to control where the latency
      --  occurs.
      --
      --  Depending on whether errors are discarded (see
      --  ``Discard_Errors_In_Populate_Lexical_Env``), return 0 on failure
      --  and 1 on success.

      ---------------------------------
      -- General AST node primitives --
      ---------------------------------

   function ada_node_kind
     (Node : ada_base_entity_Ptr) return ada_node_kind_enum with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_kind";
      --  Return the kind of this node.

   procedure ada_kind_name
     (Kind : ada_node_kind_enum; Result : access ada_text) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_kind_name";
      --  Helper for textual dump: return the kind name for this node. The
      --  returned string is a copy and thus must be free'd by the caller.

   function ada_node_unit
     (Node : ada_base_entity_Ptr) return ada_analysis_unit with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_unit";
      --  Return the analysis unit that owns this node.

   function ada_is_token_node (Node : ada_base_entity_Ptr) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_is_token_node";
      --  Return whether this node is a node that contains only a single token.

   function ada_is_synthetic (Node : ada_base_entity_Ptr) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_is_synthetic";
      --  Return whether this node is synthetic.

   procedure ada_node_short_image
     (Node : ada_base_entity_Ptr; Result : access ada_text) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_short_image";
      --  Return a representation of this node as a string.

   procedure ada_node_text
     (Node : ada_base_entity_Ptr; Text : access ada_text) with
      Export,
      Convention    => C,
      External_Name => "ada_node_text";
      --  Return the source buffer slice corresponding to the text that spans
      --  between the first and the last tokens of this node.
      --
      --  Note that this returns the empty string for synthetic nodes.

   procedure ada_node_sloc_range
     (Node         : ada_base_entity_Ptr;
      Sloc_Range_P : access ada_source_location_range) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_sloc_range";
      --  Return the spanning source location range for this node.
      --
      --  Note that this returns the sloc of the parent for synthetic nodes.

   procedure ada_lookup_in_node
     (Node   : ada_base_entity_Ptr; Sloc : ada_source_location;
      Result : ada_base_entity_Ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_lookup_in_node";
      --  Return the bottom-most node from in ``Node`` and its children which
      --  contains ``Sloc``, or ``NULL`` if there is none.

   function ada_node_children_count
     (Node : ada_base_entity_Ptr) return unsigned with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_children_count";
      --  Return the number of children in this node.

   function ada_node_child
     (Node : ada_base_entity_Ptr; N : unsigned; Child_P : ada_base_entity_Ptr)
      return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_node_child";
      --  Return the Nth child for in this node's fields and store it into
      --  *CHILD_P. Return zero on failure (when N is too big).

   function ada_text_to_locale_string
     (Text : ada_text) return System.Address with
      Export        => True,
      Convention    => C,
      External_Name => "ada_text_to_locale_string";
      --  Encode some text using the current locale. The result is dynamically
      --  allocated: it is up to the caller to free it when done with it.
      --
      --  This is a development helper to make it quick and easy to print token
      --  and diagnostic text: it ignores errors (when the locale does not
      --  support some characters). Production code should use real conversion
      --  routines such as libiconv's in order to deal with UTF-32 texts.

   subtype ada_bare_ada_node_array is Bare_Ada_Node_Array_Access;
   type ada_bare_ada_node_array_Ptr is access Bare_Ada_Node_Array_Access;

   function ada_bare_ada_node_array_create
     (Length : int) return Bare_Ada_Node_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_bare_ada_node_array_create";

   procedure ada_bare_ada_node_array_inc_ref
     (A : Bare_Ada_Node_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_bare_ada_node_array_inc_ref";

   procedure ada_bare_ada_node_array_dec_ref
     (A : Bare_Ada_Node_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_bare_ada_node_array_dec_ref";

   subtype ada_ada_node_array is Internal_Entity_Array_Access;
   type ada_ada_node_array_Ptr is access Internal_Entity_Array_Access;

   function ada_ada_node_array_create
     (Length : int) return Internal_Entity_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_array_create";

   procedure ada_ada_node_array_inc_ref (A : Internal_Entity_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_array_inc_ref";

   procedure ada_ada_node_array_dec_ref (A : Internal_Entity_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_array_dec_ref";

      --------------------
      -- Unit providers --
      --------------------

   function ada_create_unit_provider
     (Data : System.Address; Destroy_Func : ada_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : ada_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : ada_unit_provider_get_unit_from_name_callback)
      return ada_unit_provider with
      Export        => True,
      Convention    => C,
      External_Name => "ada_create_unit_provider";
      --  Create a unit provider. When done with it, the result must be passed
      --  to ``ada_destroy_unit_provider``.
      --
      --  Pass as ``data`` a pointer to hold your private data: it will be
      --  passed to all callbacks below.
      --
      --  ``destroy`` is a callback that is called by
      --  ``ada_destroy_unit_provider`` to leave a chance to free
      --  resources that ``data`` may hold.
      --
      --  ``get_unit_from_node`` is a callback. It turns an analysis unit
      --  reference represented as a node into an analysis unit. It should
      --  return ``NULL`` if the node is not a valid unit name representation.
      --
      --  ``get_unit_from_name`` is a callback similar to
      --  ``get_unit_from_node`` except it takes an analysis unit
      --  reference represented as a string.

   procedure ada_dec_ref_unit_provider (Provider : ada_unit_provider) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_dec_ref_unit_provider";
      --  Release an ownership share for this unit provider. This destroys the
      --  unit provider if there are no shares left.

      ------------------
      -- Struct types --
      ------------------

   subtype ada_internal_aspect is Internal_Aspect;
   type ada_internal_aspect_Ptr is access Internal_Aspect;

   subtype ada_internal_completion_item is Internal_Completion_Item;
   type ada_internal_completion_item_Ptr is access Internal_Completion_Item;

   subtype ada_internal_discrete_range is Internal_Discrete_Range;
   type ada_internal_discrete_range_Ptr is access Internal_Discrete_Range;

   subtype ada_internal_doc_annotation is Internal_Doc_Annotation;
   type ada_internal_doc_annotation_Ptr is access Internal_Doc_Annotation;

   procedure ada_internal_doc_annotation_inc_ref
     (R : ada_internal_doc_annotation_Ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_internal_doc_annotation_inc_ref";
   procedure ada_internal_doc_annotation_dec_ref
     (R : ada_internal_doc_annotation_Ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_internal_doc_annotation_dec_ref";

   subtype ada_internal_param_actual is Internal_Param_Actual;
   type ada_internal_param_actual_Ptr is access Internal_Param_Actual;

   subtype ada_internal_ref_result is Internal_Ref_Result;
   type ada_internal_ref_result_Ptr is access Internal_Ref_Result;

   subtype ada_internal_refd_decl is Internal_Refd_Decl;
   type ada_internal_refd_decl_Ptr is access Internal_Refd_Decl;

   subtype ada_internal_refd_def is Internal_Refd_Def;
   type ada_internal_refd_def_Ptr is access Internal_Refd_Def;

   subtype ada_internal_substitution is Internal_Substitution;
   type ada_internal_substitution_Ptr is access Internal_Substitution;

   procedure ada_internal_substitution_inc_ref
     (R : ada_internal_substitution_Ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_internal_substitution_inc_ref";
   procedure ada_internal_substitution_dec_ref
     (R : ada_internal_substitution_Ptr) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_internal_substitution_dec_ref";

      -----------------
      -- Array types --
      -----------------

   subtype ada_text_type is Character_Type_Array_Access;
   type ada_text_type_Ptr is access Character_Type_Array_Access;

   function ada_text_type_create
     (Length : int) return Character_Type_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_text_type_create";

   procedure ada_text_type_inc_ref (A : Character_Type_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_text_type_inc_ref";

   procedure ada_text_type_dec_ref (A : Character_Type_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_text_type_dec_ref";

   subtype ada_completion_item_array is Internal_Completion_Item_Array_Access;
   type ada_completion_item_array_Ptr is
     access Internal_Completion_Item_Array_Access;

   function ada_completion_item_array_create
     (Length : int) return Internal_Completion_Item_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_completion_item_array_create";

   procedure ada_completion_item_array_inc_ref
     (A : Internal_Completion_Item_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_completion_item_array_inc_ref";

   procedure ada_completion_item_array_dec_ref
     (A : Internal_Completion_Item_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_completion_item_array_dec_ref";

   subtype ada_doc_annotation_array is Internal_Doc_Annotation_Array_Access;
   type ada_doc_annotation_array_Ptr is
     access Internal_Doc_Annotation_Array_Access;

   function ada_doc_annotation_array_create
     (Length : int) return Internal_Doc_Annotation_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_doc_annotation_array_create";

   procedure ada_doc_annotation_array_inc_ref
     (A : Internal_Doc_Annotation_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_doc_annotation_array_inc_ref";

   procedure ada_doc_annotation_array_dec_ref
     (A : Internal_Doc_Annotation_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_doc_annotation_array_dec_ref";

   subtype ada_param_actual_array is Internal_Param_Actual_Array_Access;
   type ada_param_actual_array_Ptr is
     access Internal_Param_Actual_Array_Access;

   function ada_param_actual_array_create
     (Length : int) return Internal_Param_Actual_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_actual_array_create";

   procedure ada_param_actual_array_inc_ref
     (A : Internal_Param_Actual_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_actual_array_inc_ref";

   procedure ada_param_actual_array_dec_ref
     (A : Internal_Param_Actual_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_actual_array_dec_ref";

   subtype ada_ref_result_array is Internal_Ref_Result_Array_Access;
   type ada_ref_result_array_Ptr is access Internal_Ref_Result_Array_Access;

   function ada_ref_result_array_create
     (Length : int) return Internal_Ref_Result_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ref_result_array_create";

   procedure ada_ref_result_array_inc_ref
     (A : Internal_Ref_Result_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ref_result_array_inc_ref";

   procedure ada_ref_result_array_dec_ref
     (A : Internal_Ref_Result_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ref_result_array_dec_ref";

   subtype ada_substitution_array is Internal_Substitution_Array_Access;
   type ada_substitution_array_Ptr is
     access Internal_Substitution_Array_Access;

   function ada_substitution_array_create
     (Length : int) return Internal_Substitution_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_substitution_array_create";

   procedure ada_substitution_array_inc_ref
     (A : Internal_Substitution_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_substitution_array_inc_ref";

   procedure ada_substitution_array_dec_ref
     (A : Internal_Substitution_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_substitution_array_dec_ref";

   subtype ada_analysis_unit_array is Internal_Unit_Array_Access;
   type ada_analysis_unit_array_Ptr is access Internal_Unit_Array_Access;

   function ada_analysis_unit_array_create
     (Length : int) return Internal_Unit_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_analysis_unit_array_create";

   procedure ada_analysis_unit_array_inc_ref
     (A : Internal_Unit_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_analysis_unit_array_inc_ref";

   procedure ada_analysis_unit_array_dec_ref
     (A : Internal_Unit_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_analysis_unit_array_dec_ref";

   subtype ada_unbounded_text_type_array is Symbol_Type_Array_Access;
   type ada_unbounded_text_type_array_Ptr is access Symbol_Type_Array_Access;

   function ada_unbounded_text_type_array_create
     (Length : int) return Symbol_Type_Array_Access with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unbounded_text_type_array_create";

   procedure ada_unbounded_text_type_array_inc_ref
     (A : Symbol_Type_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unbounded_text_type_array_inc_ref";

   procedure ada_unbounded_text_type_array_dec_ref
     (A : Symbol_Type_Array_Access) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unbounded_text_type_array_dec_ref";

      ----------
      -- Misc --
      ----------

   function ada_get_last_exception return ada_exception_Ptr with
      Export        => True,
      Convention    => C,
      External_Name => "ada_get_last_exception";
      --  Return exception information for the last error that happened in the
      --  current thread. Will be automatically allocated on error and free'd
      --  on the next error.

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   function ada_token_kind_name (Kind : int) return chars_ptr with
      Export        => True,
      Convention    => C,
      External_Name => "ada_token_kind_name";
      --  Return a human-readable name for a token kind.
      --
      --  The returned string is dynamically allocated and the caller must free
      --  it when done with it.
      --
      --  If the given kind is invalid, return ``NULL`` and set the last
      --  exception accordingly.

   procedure ada_token_next
     (Token : ada_token; Next_Token : access ada_token) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_token_next";
      --  Return a reference to the next token in the corresponding analysis
      --  unit.

   procedure ada_token_previous
     (Token : ada_token; Previous_Token : access ada_token) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_token_previous";
      --  Return a reference to the previous token in the corresponding
      --  analysis unit.

   function ada_token_range_text
     (First, Last : ada_token; Text : access ada_text) return int with
      Export        => True,
      Convention    => C,
      External_Name => "ada_token_range_text";
      --  Compute the source buffer slice corresponding to the text that spans
      --  between the ``First`` and ``Last`` tokens (both included). This
      --  yields an empty slice if ``Last`` actually appears before ``First``.
      --  Put the result in ``RESULT``.
      --
      --  This returns 0 if ``First`` and ``Last`` don't belong to the same
      --  analysis unit. Return 1 if successful.

   function ada_token_is_equivalent
     (Left : ada_token; Right : ada_token) return ada_bool with
      Export        => True,
      Convention    => C,
      External_Name => "ada_token_is_equivalent";
      --  Return whether ``L`` and ``R`` are structurally equivalent tokens.
      --  This means that their position in the stream won't be taken into
      --  account, only the kind and text of the token.

   procedure ada_entity_image
     (Ent : ada_base_entity_Ptr; Result : access ada_text) with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entity_image";
      --  Return a representation of this entity as a string.

      ---------------------------------------
      -- Kind-specific AST node primitives --
      ---------------------------------------

      --  All these primitives return their result through an OUT parameter.
      --  They return a boolean telling whether the operation was successful
      --  (it can fail if the node does not have the proper type, for
      --  instance). When an AST node is returned, its ref-count is left as-is.

   function ada_ada_node_p_declarative_scope
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_declarative_scope";
      --  Return the scope of definition of this basic declaration.

   function ada_ada_node_p_complete
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_completion_item_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_complete";
      --  Return possible completions at this point in the file.

   function ada_ada_node_p_valid_keywords
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_valid_keywords";
      --  Return the list of keywords that are valid at this point in the file.
      --
      --  .. note:: This is work in progress. It will return all keywords for
      --  now,
      --     without looking at the context.

   function ada_ada_node_p_generic_instantiations
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_generic_instantiations";
      --  Return the potentially empty list of generic package/subprogram
      --  instantiations that led to the creation of this entity. Outer-most
      --  instantiations appear last.

   function ada_ada_node_p_semantic_parent
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_semantic_parent";
      --  Return the semantic parent for this node, if applicable, null
      --  otherwise.

   function ada_ada_node_p_parent_basic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_parent_basic_decl";
      --  Return the parent basic decl for this node, if applicable, null
      --  otherwise.
      --
      --  .. note:: If the parent BasicDecl of the given node is a generic
   --     declaration, this call will return the instantiation from which the
      --     node was retrieved instead, if any.

   function ada_ada_node_p_filter_is_imported_by
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Transitive :
ada_bool;
Value_P : access ada_analysis_unit_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_filter_is_imported_by";
      --  Filters out among the list of given units those that cannot refer to
      --  the unit in which this node lies. If transitive is True, the whole
      --  transitive closure of imports will be used to find a reference to
      --  the unit of this node.

   function ada_ada_node_p_xref_entry_point
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_xref_entry_point";
      --  Designates entities that are entry point for the xref solving
      --  infrastructure. If this returns true, then resolve_names can
      --  be called on it.
      --
      --  .. note:: For convenience, and unlike what is defined in the ARM wrt.
   --     complete contexts for name resolution, ``xref_entry_points`` can be
      --     nested.

   function ada_ada_node_p_resolve_names
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_resolve_names";
      --  This will resolve names for this node. If the operation is
      --  successful, then type_var and ref_var will be bound on
      --  appropriate subnodes of the statement.

   function ada_ada_node_p_standard_unit
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_analysis_unit)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_standard_unit";
      --  Static method. Return the analysis unit corresponding to the Standard
      --  package.

   function ada_ada_node_p_std_entity
     (Node : ada_base_entity_Ptr;
Sym        : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_std_entity";
      --  Static property. Return an entity from the standard package with name
      --  `sym`.

   function ada_ada_node_p_bool_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_bool_type";
      --  Static method. Return the standard Boolean type.

   function ada_ada_node_p_int_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_int_type";
      --  Static method. Return the standard Integer type.

   function ada_ada_node_p_universal_int_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_universal_int_type";
      --  Static method. Return the standard Universal Integer type.

   function ada_ada_node_p_universal_real_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_universal_real_type";
      --  Static method. Return the standard Universal Real type.

   function ada_ada_node_p_top_level_decl
     (Node : ada_base_entity_Ptr;
Unit :
ada_analysis_unit;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_top_level_decl";
      --  Static method. Get the top-level decl in ``unit``. This is the body
      --  of a Subunit, or the item of a ``LibraryItem``.

   function ada_ada_node_p_choice_match
     (Node : ada_base_entity_Ptr;
Value      : access constant ada_big_integer;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_choice_match";
      --  Assuming that self is a choice expression (such as what can appear
      --  in an alternative of a case statement or in the RHS of a membership
      --  expression, this property returns whether the given value satisfies
      --  it.
      --
      --  .. ATTENTION:: This is an experimental feature, so even if it is
      --  exposed
      --     to allow experiments, it is totally unsupported and the API and
      --     behavior are very likely to change in the future.

   function ada_ada_node_p_gnat_xref
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_p_gnat_xref";
      --  Return a cross reference from this name to a defining identifier,
      --  trying to mimic GNAT's xrefs as much as possible.

   function ada_ada_node_parent
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_parent";
      --  Return the lexical parent for this node. Return null for the root AST
      --  node or for AST nodes for which no one has a reference to the parent.

   function ada_ada_node_parents
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_parents";
      --  Return an array that contains the lexical parents (this node
      --  included). Nearer parents are first in the list.

   function ada_ada_node_children
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_children";
      --  Return an array that contains the direct lexical children.

   function ada_ada_node_token_start
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_token) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_token_start";
      --  Return the first token used to parse this node.

   function ada_ada_node_token_end
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_token) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_token_end";
      --  Return the last token used to parse this node.

   function ada_ada_node_child_index
     (Node : ada_base_entity_Ptr;
Value_P    : access int) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_child_index";
      --  Return the 0-based index for Node in its parent's children.

   function ada_ada_node_previous_sibling
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_previous_sibling";
      --  Return the node's previous sibling, if there is one.

   function ada_ada_node_next_sibling
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_next_sibling";
      --  Return the node's next sibling, if there is one.

   function ada_ada_node_unit
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_analysis_unit)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_unit";
      --  Return the analysis unit owning this node.

   function ada_ada_node_is_ghost
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_is_ghost";
      --  Return whether the node is a ghost.
      --
      --  Unlike regular nodes, ghost nodes cover no token in the input
      --  source: they are logically located instead between two tokens.
      --  The "token_first" of all ghost nodes is the token right after
      --  this logical position, while they have no "token_last".

   function ada_ada_node_full_sloc_image
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ada_node_full_sloc_image";
      --  Return a string containing the filename + the sloc in GNU conformant
      --  format. Useful to create diagnostics from a node.

   function ada_abort_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_abort_node_p_as_bool";
      --  Return whether this is an instance of AbortPresent

   function ada_abstract_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_abstract_node_p_as_bool";
      --  Return whether this is an instance of AbstractPresent

   function ada_assoc_list_p_zip_with_params
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_param_actual_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_assoc_list_p_zip_with_params";
      --  Returns an array of pairs, associating formal parameters to actual
      --  expressions. The formals to match are retrieved by resolving the
      --  call which this AssocList represents the actuals of.

   function ada_aliased_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_aliased_node_p_as_bool";
      --  Return whether this is an instance of AliasedPresent

   function ada_all_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_all_node_p_as_bool";
      --  Return whether this is an instance of AllPresent

   function ada_constrained_array_indices_f_list
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_constrained_array_indices_f_list";
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

   function ada_unconstrained_array_indices_f_types
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unconstrained_array_indices_f_types";

   function ada_aspect_assoc_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_aspect_assoc_f_id";
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

   function ada_aspect_assoc_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_aspect_assoc_f_expr";
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

   function ada_at_clause_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_at_clause_f_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_at_clause_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_at_clause_f_expr";
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

   function ada_attribute_def_clause_f_attribute_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_attribute_def_clause_f_attribute_expr";
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

   function ada_attribute_def_clause_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_attribute_def_clause_f_expr";
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

   function ada_enum_rep_clause_f_type_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_enum_rep_clause_f_type_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_enum_rep_clause_f_aggregate
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_enum_rep_clause_f_aggregate";

   function ada_record_rep_clause_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_rep_clause_f_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_record_rep_clause_f_at_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_rep_clause_f_at_expr";
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

   function ada_record_rep_clause_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_rep_clause_f_components";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Component_Clause
      --
      --  * Pragma_Node

   function ada_aspect_spec_f_aspect_assocs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_aspect_spec_f_aspect_assocs";

   function ada_base_assoc_p_assoc_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_assoc_p_assoc_expr";
      --  Returns the expression side of this assoc node.

   function ada_contract_case_assoc_f_guard
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_contract_case_assoc_f_guard";
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

   function ada_contract_case_assoc_f_consequence
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_contract_case_assoc_f_consequence";
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

   function ada_pragma_argument_assoc_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_pragma_argument_assoc_f_id";

   function ada_pragma_argument_assoc_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_pragma_argument_assoc_f_expr";
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

   function ada_base_formal_param_holder_p_abstract_formal_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_formal_param_holder_p_abstract_formal_params";
      --  Return the list of abstract formal parameters for this holder.

   function ada_base_formal_param_holder_p_nb_min_params
     (Node : ada_base_entity_Ptr;
Value_P    : access int) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_formal_param_holder_p_nb_min_params";
      --  Return the minimum number of parameters this subprogram can be called
      --  while still being a legal call.

   function ada_base_formal_param_holder_p_nb_max_params
     (Node : ada_base_entity_Ptr;
Value_P    : access int) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_formal_param_holder_p_nb_max_params";
      --  Return the maximum number of parameters this subprogram can be called
      --  while still being a legal call.

   function ada_base_formal_param_holder_p_param_types
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_formal_param_holder_p_param_types";
      --  Returns the type of each parameter of Self.

   function ada_base_subp_spec_p_returns
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_spec_p_returns";
      --  Syntax property. Return the type expression node corresponding to the
      --  return of this subprogram spec.

   function ada_base_subp_spec_p_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_spec_p_params";
      --  Returns the array of parameters specification for this subprogram
      --  spec.

   function ada_base_subp_spec_p_primitive_subp_types
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_spec_p_primitive_subp_types";
      --  Return the types of which this subprogram is a primitive of.

   function ada_base_subp_spec_p_primitive_subp_first_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_spec_p_primitive_subp_first_type";
      --  Return the first type of which this subprogram is a primitive of.

   function ada_base_subp_spec_p_primitive_subp_tagged_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_spec_p_primitive_subp_tagged_type";
      --  If this subprogram is a primitive for a tagged type, then return this
      --  type.

   function ada_base_subp_spec_p_return_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_spec_p_return_type";
      --  Returns the return type of Self, if applicable (e.g. if Self is a
      --  subprogram). Else, returns null.

   function ada_entry_spec_f_entry_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_spec_f_entry_name";

   function ada_entry_spec_f_family_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_spec_f_family_type";
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

   function ada_entry_spec_f_entry_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_spec_f_entry_params";

   function ada_subp_spec_f_subp_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_spec_f_subp_kind";

   function ada_subp_spec_f_subp_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_spec_f_subp_name";

   function ada_subp_spec_f_subp_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_spec_f_subp_params";

   function ada_subp_spec_f_subp_returns
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_spec_f_subp_returns";
      --  This field can contain one of the following nodes:
      --
      --  * Anonymous_Type
      --
      --  * Subtype_Indication

   function ada_component_list_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_list_f_components";

   function ada_component_list_f_variant_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_list_f_variant_part";

   function ada_known_discriminant_part_f_discr_specs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_known_discriminant_part_f_discr_specs";

   function ada_entry_completion_formal_params_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_completion_formal_params_f_params";

   function ada_generic_formal_part_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_formal_part_f_decls";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Generic_Formal
      --
      --  * Pragma_Node
      --
      --  * Use_Clause

   function ada_base_record_def_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_record_def_f_components";

   function ada_basic_assoc_p_get_params
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_assoc_p_get_params";
      --  Return the list of parameters that this association refers to.

   function ada_aggregate_assoc_f_designators
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_aggregate_assoc_f_designators";
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

   function ada_aggregate_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_aggregate_assoc_f_r_expr";
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

   function ada_discriminant_assoc_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discriminant_assoc_f_ids";

   function ada_discriminant_assoc_f_discr_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discriminant_assoc_f_discr_expr";
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

   function ada_param_assoc_f_designator
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_assoc_f_designator";
      --  This field can contain one of the following nodes:
      --
      --  * Identifier
      --
      --  * Others_Designator
      --
      --  * String_Literal

   function ada_param_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_assoc_f_r_expr";
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

   function ada_basic_decl_p_is_formal
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_is_formal";
      --  Whether this decl is the nested decl of a generic formal declaration.

   function ada_basic_decl_p_doc_annotations
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_doc_annotation_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_doc_annotations";
      --  Return the documentation annotations associated with this decl.
      --  Annotations are any comment line of the form::
      --
      --  --% [annotation_name]: [annotation]
      --
      --  Raises a property error if the doc is incorrectly formatted.
      --
      --  .. ATTENTION:: This is an experimental feature, so even if it is
      --  exposed
      --     to allow experiments, it is totally unsupported and the API and
      --     behavior are very likely to change in the future.

   function ada_basic_decl_p_doc
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_doc";
      --  Return the documentation associated with this decl. Raises a property
      --  error if the doc is incorrectly formatted.
      --
      --  .. ATTENTION:: This is an experimental feature, so even if it is
      --  exposed
      --     to allow experiments, it is totally unsupported and the API and
      --     behavior are very likely to change in the future.

   function ada_basic_decl_p_previous_part_for_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_previous_part_for_decl";
      --  Return the previous part for this decl, if applicable.
      --
      --  .. note:: It is not named previous_part, because BaseTypeDecl has a
      --  more
      --     precise version of previous_part that returns a BaseTypeDecl.
   --     Probably, we want to rename the specific versions, and have the root
      --     property be named previous_part. (TODO R925-008)

   function ada_basic_decl_p_canonical_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_canonical_part";
      --  Return the canonical part for this decl. In the case of decls
      --  composed of several parts, the canonical part will be the first part.

   function ada_basic_decl_p_is_static_decl
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_is_static_decl";
      --  Return whether this declaration is static.

   function ada_basic_decl_p_is_imported
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_is_imported";
      --  Whether this declaration is imported from another language.

   function ada_basic_decl_f_aspects
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_f_aspects";
      --  Return the list of aspects that are attached to this node.

   function ada_basic_decl_p_get_aspect_assoc
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_get_aspect_assoc";
      --  Return the aspect with name ``name`` for this entity.

   function ada_basic_decl_p_get_aspect_spec_expr
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_get_aspect_spec_expr";
      --  Return the expression associated to the aspect with name ``name`` for
      --  this entity.

   function ada_basic_decl_p_get_aspect
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_internal_aspect)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_get_aspect";
      --  Return the aspect with name ``name`` associated to this entity.
      --
      --  Aspects are properties of entities that can be specified by the Ada
      --  program, either via aspect specifications, pragmas, or attributes.
      --
      --  This will return the syntactic node corresponding to attribute
      --  directly.

   function ada_basic_decl_p_has_aspect
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_has_aspect";
      --  Returns whether the boolean aspect named ``name`` is set on the
      --  entity represented by this node.
      --
      --  "Aspect" is used as in RM terminology (see RM 13).

   function ada_basic_decl_p_get_pragma
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_get_pragma";
      --  Return the pragma with name ``name`` associated to this entity.

   function ada_basic_decl_p_get_representation_clause
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_get_representation_clause";
      --  Return the representation clause associated to this type decl that
      --  defines the given attribute name.

   function ada_basic_decl_p_is_compilation_unit_root
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_is_compilation_unit_root";
      --  Whether a BasicDecl is the root decl for its unit.

   function ada_basic_decl_p_is_visible
     (Node : ada_base_entity_Ptr;
From_Node  : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_is_visible";
      --  Return whether this declaration is visible from the point of view of
      --  the given ``origin`` node.
      --
      --  .. ATTENTION:: Only package-level (public or private) declarations
      --  are
      --     supported for now.

   function ada_basic_decl_p_base_subp_declarations
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_base_subp_declarations";
      --  If Self declares a primitive subprogram of some tagged type T, return
      --  the set of all subprogram declarations that it overrides (including
      --  itself).

   function ada_basic_decl_p_root_subp_declarations
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_root_subp_declarations";
      --  If Self declares a primitive subprogram of some tagged type T, return
      --  the root subprogram declarations that it overrides. There can be
      --  several, as in the following scenario:
      --
      --  - package Root defines the root tagged type T and subprogram Foo.
      --
      --  - package Itf defines interface I and abstract subprogram Foo.
      --
      --  - package D defines "type U is new Root.T and Itf.I" and an
      --  overriding subprogram Foo.
      --
      --  Here, root_subp_declarations of Foo defined in package D will return
      --  both Foo from package Root and Foo from package Itf.

   function ada_basic_decl_p_find_all_overrides
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_find_all_overrides";
      --  If Self is the declaration of a primitive of some type T, return the
      --  list of all subprogram that override this subprogram among the given
      --  units.

   function ada_basic_decl_p_defining_names
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_defining_names";
      --  Get all the names of this basic declaration.

   function ada_basic_decl_p_defining_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_defining_name";
      --  Get the name of this declaration. If this declaration has several
      --  names, it will return the first one.

   function ada_basic_decl_p_type_expression
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_type_expression";
      --  Return the type expression for this BasicDecl if applicable, a null
      --  otherwise.

   function ada_basic_decl_p_subp_spec_or_null
     (Node : ada_base_entity_Ptr;
Follow_Generic :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_subp_spec_or_null";
      --  If Self is a Subp, returns the specification of this subprogram.
      --
      --  If ``follow_generic`` is True, will also work for instances of
      --  ``GenericSubpDecl``.

   function ada_basic_decl_p_is_subprogram
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_is_subprogram";
      --  Return True if self is a subprogram node in the general sense (which
      --  is, an entity that can be called). This includes separates and
      --  entries.

   function ada_basic_decl_p_relative_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_relative_name";
      --  Return the relative name for Self. If Self's defining name is
      --  ``A.B.C``, return C as a node.

   function ada_basic_decl_p_relative_name_text
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_symbol_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_relative_name_text";
      --  Return the relative name for Self, as text.

   function ada_basic_decl_p_next_part_for_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_next_part_for_decl";
      --  Return the next part of this declaration, if applicable.
      --
      --  .. note:: It is not named next_part, because BaseTypeDecl has a more
   --     precise version of next_part that returns a BaseTypeDecl. Probably,
   --     we want to rename the specific versions, and have the root property
      --     be named next_part. (TODO R925-008)

   function ada_basic_decl_p_body_part_for_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_body_part_for_decl";
      --  Return the body corresponding to this declaration, if applicable.
      --
      --  .. note:: It is not named body_part, subclasses have more precise
      --     versions named body_part and returning a more precise result.
   --     Probably, we want to rename the specific versions, and have the root
      --     property be named previous_part. (TODO R925-008)

   function ada_basic_decl_p_fully_qualified_name_array
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_fully_qualified_name_array";
      --  Return the fully qualified name corresponding to this declaration, as
      --  an array of symbols.

   function ada_basic_decl_p_fully_qualified_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_fully_qualified_name";
      --  Return the fully qualified name corresponding to this declaration.

   function ada_basic_decl_p_canonical_fully_qualified_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_canonical_fully_qualified_name";
      --  Return a canonical representation of the fully qualified name
      --  corresponding to this declaration.

   function ada_basic_decl_p_unique_identifying_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_decl_p_unique_identifying_name";
      --  Return a unique identifying name for this declaration, provided this
      --  declaration is a public declaration. In the case of subprograms, this
      --  will include the profile.
      --
      --  .. attention:: This will only return a unique name for public
      --     declarations. Notably, anything nested in an unnamed declare block
      --     won't be handled correctly.

   function ada_base_formal_param_decl_p_formal_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_formal_param_decl_p_formal_type";
      --  Return the type for this formal.

   function ada_component_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_decl_f_ids";

   function ada_component_decl_f_component_def
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_decl_f_component_def";

   function ada_component_decl_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_decl_f_default_expr";
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

   function ada_discriminant_spec_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discriminant_spec_f_ids";

   function ada_discriminant_spec_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discriminant_spec_f_type_expr";
      --  This field can contain one of the following nodes:
      --
      --  * Anonymous_Type
      --
      --  * Subtype_Indication

   function ada_discriminant_spec_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discriminant_spec_f_default_expr";
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

   function ada_generic_formal_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_formal_f_decl";
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

   function ada_param_spec_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_spec_f_ids";

   function ada_param_spec_f_has_aliased
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_spec_f_has_aliased";

   function ada_param_spec_f_mode
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_spec_f_mode";

   function ada_param_spec_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_spec_f_type_expr";
      --  This field can contain one of the following nodes:
      --
      --  * Anonymous_Type
      --
      --  * Subtype_Indication

   function ada_param_spec_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_param_spec_f_default_expr";
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

   function ada_base_package_decl_f_package_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_package_decl_f_package_name";

   function ada_base_package_decl_f_public_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_package_decl_f_public_part";

   function ada_base_package_decl_f_private_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_package_decl_f_private_part";

   function ada_base_package_decl_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_package_decl_f_end_name";

   function ada_base_package_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_package_decl_p_body_part";
      --  Return the PackageBody corresponding to this node.

   function ada_base_type_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_f_name";

   function ada_base_type_decl_p_base_subtype
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_base_subtype";
      --  If this type decl is a subtype decl, return the base subtype. If not,
      --  return ``Self``.

   function ada_base_type_decl_p_private_completion
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_private_completion";
      --  Return the private completion for this type, if there is one.

   function ada_base_type_decl_p_get_record_representation_clause
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_get_record_representation_clause";
      --  Return the record representation clause associated to this type decl,
      --  if applicable (i.e. this type decl defines a record type).

   function ada_base_type_decl_p_get_enum_representation_clause
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_get_enum_representation_clause";
      --  Return the enum representation clause associated to this type decl,
      --  if applicable (i.e. this type decl defines an enum type).

   function ada_base_type_decl_p_is_record_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_record_type";
      --  Return whether this type is a record type.

   function ada_base_type_decl_p_is_array_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_array_type";
      --  Return whether this type is an array type.

   function ada_base_type_decl_p_find_derived_types
     (Node   : ada_base_entity_Ptr;
Root         : access constant ada_base_entity;
      Origin : access constant ada_base_entity; Imprecise_Fallback :
ada_bool;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_find_derived_types";
      --  Find types derived from self in the given ``root`` and its children.

   function ada_base_type_decl_p_is_real_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_real_type";
      --  Whether type is a real type or not.

   function ada_base_type_decl_p_is_float_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_float_type";
      --  Whether type is a float type or not.

   function ada_base_type_decl_p_is_fixed_point
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_fixed_point";
      --  Whether type is a fixed point type or not.

   function ada_base_type_decl_p_is_enum_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_enum_type";
      --  Whether type is an enum type

   function ada_base_type_decl_p_is_access_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_access_type";
      --  Whether Self is an access type or not

   function ada_base_type_decl_p_is_char_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_char_type";
      --  Whether type is a character type or not

   function ada_base_type_decl_p_discrete_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_internal_discrete_range)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_discrete_range";
      --  Return the discrete range for this type decl, if applicable.

   function ada_base_type_decl_p_is_discrete_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_discrete_type";
      --  Whether type is a discrete type or not.

   function ada_base_type_decl_p_is_int_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_int_type";
      --  Whether type is an integer type or not.

   function ada_base_type_decl_p_accessed_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_accessed_type";
      --  If this type is an access type, or a type with an
      --  Implicit_Dereference aspect, return the type of a dereference of
      --  an instance of this type.

   function ada_base_type_decl_p_is_tagged_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_tagged_type";
      --  Whether type is tagged or not

   function ada_base_type_decl_p_base_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_base_type";
      --  Return the base type entity for this derived type declaration

   function ada_base_type_decl_p_base_types
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_base_types";
      --  Return the list of base types for Self.

   function ada_base_type_decl_p_find_all_derived_types
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_find_all_derived_types";
      --  Return the list of all types that inherit (directly or inderictly)
      --  from Self among the given units.

   function ada_base_type_decl_p_comp_type
     (Node : ada_base_entity_Ptr;
Is_Subscript :
ada_bool;
      Origin : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_comp_type";
      --  Return the component type of `Self`, if applicable. The component
      --  type is the type you'll get if you call a value whose type is `Self`.
      --  So it can either be:
      --
      --  1. The component type for an array. 2. The return type for an access
      --  to function.

   function ada_base_type_decl_p_index_type
     (Node : ada_base_entity_Ptr;
Dim :
int;
      Origin : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_index_type";
      --  Return the index type for dimension ``dim`` for this type, if
      --  applicable.

   function ada_base_type_decl_p_is_derived_type
     (Node   : ada_base_entity_Ptr;
Other_Type   : access constant ada_base_entity;
      Origin : access constant ada_base_entity;
Value_P      : access ada_bool)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_derived_type";
      --  Whether Self is derived from other_type.

   function ada_base_type_decl_p_is_interface_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_interface_type";
      --  Return True iff this type declaration is an interface definition.

   function ada_base_type_decl_p_matching_type
     (Node : ada_base_entity_Ptr;

      Expected_Type : access constant ada_base_entity;
      Origin        : access constant ada_base_entity;
Value_P             : access ada_bool)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_matching_type";
      --  Return whether ``self`` matches ``expected_type``.

   function ada_base_type_decl_p_canonical_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_canonical_type";
      --  Return the canonical type declaration for this type declaration. For
      --  subtypes, it will return the base type declaration.

   function ada_base_type_decl_p_previous_part
     (Node : ada_base_entity_Ptr;
Go_To_Incomplete :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_previous_part";
      --  Returns the previous part for this type decl.

   function ada_base_type_decl_p_next_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_next_part";
      --  Returns the next part for this type decl.

   function ada_base_type_decl_p_full_view
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_full_view";
      --  Return the full completion of this type.

   function ada_base_type_decl_p_is_definite_subtype
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_definite_subtype";
      --  Returns whether this is a definite subtype.
      --
      --  For convenience, this will return ``False`` for incomplete types,
      --  even though the correct answer is more akin to "non applicable".

   function ada_base_type_decl_p_is_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_is_private";
      --  Whether node is a private view of corresponding type.

   function ada_base_type_decl_p_root_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_type_decl_p_root_type";
      --  Return the type that is at the root of the derivation hierarchy
      --  (ignoring secondary interfaces derivations for tagged types)

   function ada_subtype_decl_f_subtype
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subtype_decl_f_subtype";

   function ada_incomplete_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_incomplete_type_decl_f_discriminants";

   function ada_incomplete_tagged_type_decl_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_incomplete_tagged_type_decl_f_has_abstract";

   function ada_protected_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_type_decl_f_discriminants";

   function ada_protected_type_decl_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_type_decl_f_interfaces";
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

   function ada_protected_type_decl_f_definition
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_type_decl_f_definition";

   function ada_task_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_type_decl_f_discriminants";

   function ada_task_type_decl_f_definition
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_type_decl_f_definition";

   function ada_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_decl_f_discriminants";

   function ada_type_decl_f_type_def
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_decl_f_type_def";

   function ada_type_decl_p_get_primitives
     (Node : ada_base_entity_Ptr;
Only_Inherited :
ada_bool;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_decl_p_get_primitives";
      --  Return the list of all primitive operations that are available on
      --  this type. If `only_inherited` is True, it will only return the
      --  primitives that are implicitly inherited by this type, discarding
      --  those explicitly defined on this type.

   function ada_basic_subp_decl_p_subp_decl_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_subp_decl_p_subp_decl_spec";
      --  Return the specification for this subprogram

   function ada_basic_subp_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_basic_subp_decl_p_body_part";
      --  Return the BaseSubpBody corresponding to this node.

   function ada_classic_subp_decl_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_classic_subp_decl_f_overriding";

   function ada_classic_subp_decl_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_classic_subp_decl_f_subp_spec";

   function ada_formal_subp_decl_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_formal_subp_decl_f_default_expr";
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

   function ada_entry_decl_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_decl_f_overriding";

   function ada_entry_decl_f_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_decl_f_spec";

   function ada_enum_literal_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_enum_literal_decl_f_name";

   function ada_enum_literal_decl_p_enum_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_enum_literal_decl_p_enum_type";
      --  Return the enum type corresponding to this enum literal.

   function ada_generic_subp_internal_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_internal_f_subp_spec";

   function ada_body_node_p_previous_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_body_node_p_previous_part";
      --  Return the previous part for this body. Might be a declaration or a
      --  body stub.

   function ada_body_node_p_decl_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_body_node_p_decl_part";
      --  Return the decl corresponding to this node if applicable.

   function ada_body_node_p_subunit_root
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_body_node_p_subunit_root";
      --  If self is a subunit, return the body in which it is rooted.

   function ada_base_subp_body_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_body_f_overriding";

   function ada_base_subp_body_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_subp_body_f_subp_spec";

   function ada_expr_function_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_function_f_expr";
      --  This field can contain one of the following nodes:
      --
      --  * Base_Aggregate
      --
      --  * Paren_Expr

   function ada_subp_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_body_f_decls";

   function ada_subp_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_body_f_stmts";

   function ada_subp_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_body_f_end_name";

   function ada_subp_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_renaming_decl_f_renames";

   function ada_package_body_stub_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_body_stub_f_name";

   function ada_protected_body_stub_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_body_stub_f_name";

   function ada_subp_body_stub_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_body_stub_f_overriding";

   function ada_subp_body_stub_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subp_body_stub_f_subp_spec";

   function ada_task_body_stub_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_body_stub_f_name";

   function ada_entry_body_f_entry_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_entry_name";

   function ada_entry_body_f_index_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_index_spec";

   function ada_entry_body_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_params";

   function ada_entry_body_f_barrier
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_barrier";
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

   function ada_entry_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_decls";

   function ada_entry_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_stmts";

   function ada_entry_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_body_f_end_name";

   function ada_package_body_f_package_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_body_f_package_name";

   function ada_package_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_body_f_decls";

   function ada_package_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_body_f_stmts";

   function ada_package_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_body_f_end_name";

   function ada_protected_body_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_body_f_name";

   function ada_protected_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_body_f_decls";

   function ada_protected_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_body_f_end_name";

   function ada_task_body_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_body_f_name";

   function ada_task_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_body_f_decls";

   function ada_task_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_body_f_stmts";

   function ada_task_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_body_f_end_name";

   function ada_entry_index_spec_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_index_spec_f_id";

   function ada_entry_index_spec_f_subtype
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_entry_index_spec_f_subtype";
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

   function ada_exception_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exception_decl_f_ids";

   function ada_exception_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exception_decl_f_renames";

   function ada_exception_handler_f_exception_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exception_handler_f_exception_name";

   function ada_exception_handler_f_handled_exceptions
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exception_handler_f_handled_exceptions";
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

   function ada_exception_handler_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exception_handler_f_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_for_loop_var_decl_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_for_loop_var_decl_f_id";

   function ada_for_loop_var_decl_f_id_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_for_loop_var_decl_f_id_type";

   function ada_generic_decl_f_formal_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_decl_f_formal_part";

   function ada_generic_package_decl_f_package_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_decl_f_package_decl";

   function ada_generic_package_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_decl_p_body_part";
      --  Return the PackageBody corresponding to this node, or null if there
      --  is none.

   function ada_generic_subp_decl_f_subp_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_decl_f_subp_decl";

   function ada_generic_subp_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_decl_p_body_part";
      --  Return the BaseSubpBody corresponding to this node.

   function ada_generic_instantiation_p_designated_generic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_instantiation_p_designated_generic_decl";
      --  Return the generic decl entity designated by this instantiation,
      --  containing the generic context. This is equivalent to the expanded
      --  generic unit in GNAT.

   function ada_generic_package_instantiation_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_instantiation_f_name";

   function ada_generic_package_instantiation_f_generic_pkg_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_instantiation_f_generic_pkg_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_generic_package_instantiation_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_instantiation_f_params";

   function ada_generic_subp_instantiation_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_instantiation_f_overriding";

   function ada_generic_subp_instantiation_f_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_instantiation_f_kind";

   function ada_generic_subp_instantiation_f_subp_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_instantiation_f_subp_name";

   function ada_generic_subp_instantiation_f_generic_subp_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_instantiation_f_generic_subp_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_generic_subp_instantiation_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_instantiation_f_params";

   function ada_generic_subp_instantiation_p_designated_subp
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_instantiation_p_designated_subp";
      --  Return the subprogram decl designated by this instantiation.

   function ada_generic_package_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_renaming_decl_f_name";

   function ada_generic_package_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_package_renaming_decl_f_renames";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_generic_subp_renaming_decl_f_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_renaming_decl_f_kind";

   function ada_generic_subp_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_renaming_decl_f_name";

   function ada_generic_subp_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_generic_subp_renaming_decl_f_renames";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_label_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_label_decl_f_name";

   function ada_named_stmt_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_named_stmt_decl_f_name";

   function ada_number_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_number_decl_f_ids";

   function ada_number_decl_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_number_decl_f_expr";
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

   function ada_object_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_ids";

   function ada_object_decl_f_has_aliased
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_has_aliased";

   function ada_object_decl_f_has_constant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_has_constant";

   function ada_object_decl_f_mode
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_mode";

   function ada_object_decl_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_type_expr";

   function ada_object_decl_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_default_expr";

   function ada_object_decl_f_renaming_clause
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_f_renaming_clause";

   function ada_object_decl_p_public_part_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_object_decl_p_public_part_decl";
      --  If this object decl is the constant completion of an object decl in
      --  the public part, return the object decl from the public part.

   function ada_package_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_renaming_decl_f_name";

   function ada_package_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_renaming_decl_f_renames";

   function ada_package_renaming_decl_p_renamed_package
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_renaming_decl_p_renamed_package";
      --  Return the declaration of the package that is renamed by Self.

   function ada_package_renaming_decl_p_final_renamed_package
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_package_renaming_decl_p_final_renamed_package";
      --  Return the declaration of the package that is ultimately renamed by
      --  Self, skipping through all intermediate package renamings.

   function ada_single_protected_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_single_protected_decl_f_name";

   function ada_single_protected_decl_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_single_protected_decl_f_interfaces";
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

   function ada_single_protected_decl_f_definition
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_single_protected_decl_f_definition";

   function ada_single_task_decl_f_task_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_single_task_decl_f_task_type";

   function ada_case_stmt_alternative_f_choices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_stmt_alternative_f_choices";
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

   function ada_case_stmt_alternative_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_stmt_alternative_f_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_compilation_unit_f_prelude
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_f_prelude";
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

   function ada_compilation_unit_f_body
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_f_body";
      --  This field can contain one of the following nodes:
      --
      --  * Library_Item
      --
      --  * Subunit

   function ada_compilation_unit_f_pragmas
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_f_pragmas";

   function ada_compilation_unit_p_syntactic_fully_qualified_name
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_syntactic_fully_qualified_name";
      --  Return the syntactic fully qualified name of this compilation unit.

   function ada_compilation_unit_p_unit_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_analysis_unit_kind)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_unit_kind";
      --  Return the kind corresponding to this analysis unit.

   function ada_compilation_unit_p_withed_units
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_withed_units";
      --  Look for all "with" clauses at the top of this compilation unit and
      --  return all the compilation units designated by them.

   function ada_compilation_unit_p_imported_units
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_imported_units";
      --  Return all the compilation units that are directly imported by this
      --  one. This includes "with"ed units as well as the direct parent unit.

   function ada_compilation_unit_p_unit_dependencies
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_unit_dependencies";
      --  Return the list of all the compilation units that are (direct and
      --  indirect) dependencies of this one.

   function ada_compilation_unit_p_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_decl";
      --  Get the root basic decl defined in this compilation unit.

   function ada_compilation_unit_p_is_preelaborable
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_compilation_unit_p_is_preelaborable";
      --  Whether this compilation unit is preelaborable or not.

   function ada_component_clause_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_clause_f_id";

   function ada_component_clause_f_position
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_clause_f_position";
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

   function ada_component_clause_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_clause_f_range";

   function ada_component_def_f_has_aliased
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_def_f_has_aliased";

   function ada_component_def_f_has_constant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_def_f_has_constant";

   function ada_component_def_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_component_def_f_type_expr";
      --  This field can contain one of the following nodes:
      --
      --  * Anonymous_Type
      --
      --  * Subtype_Indication

   function ada_constant_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_constant_node_p_as_bool";
      --  Return whether this is an instance of ConstantPresent

   function ada_delta_constraint_f_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_delta_constraint_f_digits";
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

   function ada_delta_constraint_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_delta_constraint_f_range";

   function ada_digits_constraint_f_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_digits_constraint_f_digits";
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

   function ada_digits_constraint_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_digits_constraint_f_range";

   function ada_discriminant_constraint_f_constraints
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discriminant_constraint_f_constraints";

   function ada_index_constraint_f_constraints
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_index_constraint_f_constraints";
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

   function ada_range_constraint_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_range_constraint_f_range";

   function ada_declarative_part_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_declarative_part_f_decls";
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

   function ada_elsif_expr_part_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_elsif_expr_part_f_cond_expr";
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

   function ada_elsif_expr_part_f_then_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_elsif_expr_part_f_then_expr";
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

   function ada_elsif_stmt_part_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_elsif_stmt_part_f_cond_expr";
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

   function ada_elsif_stmt_part_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_elsif_stmt_part_f_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_expr_p_expression_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_p_expression_type";
      --  Return the declaration corresponding to the type of this expression
      --  after name resolution.

   function ada_expr_p_is_static_expr
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_p_is_static_expr";
      --  Return whether this expression is static according to the ARM
      --  definition of static. See RM 4.9.

   function ada_expr_p_first_corresponding_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_p_first_corresponding_decl";
      --  Return the first decl that is lexically named like self in self's
      --  scope.

   function ada_expr_p_eval_as_int
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_big_integer)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_p_eval_as_int";
      --  Statically evaluates self, and returns the value of the evaluation as
      --  an integer.
      --
      --  .. note:: In order for a call to this not to raise, the expression
      --  needs
   --     to be a static expression, as specified in the ARM section 4.9. You
      --     can verify whether an expression is static with the
      --     ``is_static_expr`` property.
      --
      --  .. ATTENTION:: This is an experimental feature, so even if it is
      --  exposed
      --     to allow experiments, it is totally unsupported and the API and
      --     behavior are very likely to change in the future.

   function ada_expr_p_eval_as_int_in_env
     (Node : ada_base_entity_Ptr;
Env :
ada_substitution_array;

      Value_P : access ada_big_integer) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_p_eval_as_int_in_env";
      --  Statically evaluates self, and returns the value of the evaluation as
      --  an integer. The given environment is used to substitute references to
      --  declarations by actual values.
      --
      --  .. note:: In order for a call to this not to raise, the expression
      --  needs
   --     to be a static expression, as specified in the ARM section 4.9. You
      --     can verify whether an expression is static with the
      --     ``is_static_expr`` property.
      --
      --  .. ATTENTION:: This is an experimental feature, so even if it is
      --  exposed
      --     to allow experiments, it is totally unsupported and the API and
      --     behavior are very likely to change in the future.

   function ada_expr_p_matching_nodes
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_expr_p_matching_nodes";
      --  Return the list of AST nodes that can be a match for this expression
      --  before overloading analysis.

   function ada_allocator_f_subpool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_allocator_f_subpool";
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

   function ada_allocator_f_type_or_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_allocator_f_type_or_expr";
      --  This field can contain one of the following nodes:
      --
      --  * Qual_Expr
      --
      --  * Subtype_Indication

   function ada_allocator_p_get_allocated_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_allocator_p_get_allocated_type";
      --  Return the allocated type for this allocator.

   function ada_base_aggregate_f_ancestor_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_aggregate_f_ancestor_expr";

   function ada_base_aggregate_f_assocs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_aggregate_f_assocs";

   function ada_bin_op_f_left
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_bin_op_f_left";
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

   function ada_bin_op_f_op
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_bin_op_f_op";
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

   function ada_bin_op_f_right
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_bin_op_f_right";
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

   function ada_case_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_expr_f_expr";
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

   function ada_case_expr_f_cases
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_expr_f_cases";

   function ada_case_expr_alternative_f_choices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_expr_alternative_f_choices";
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

   function ada_case_expr_alternative_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_expr_alternative_f_expr";
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

   function ada_contract_cases_f_contract_cases
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_contract_cases_f_contract_cases";

   function ada_if_expr_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_expr_f_cond_expr";
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

   function ada_if_expr_f_then_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_expr_f_then_expr";
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

   function ada_if_expr_f_alternatives
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_expr_f_alternatives";

   function ada_if_expr_f_else_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_expr_f_else_expr";
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

   function ada_membership_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_membership_expr_f_expr";
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

   function ada_membership_expr_f_op
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_membership_expr_f_op";
      --  This field can contain one of the following nodes:
      --
      --  * Op_In
      --
      --  * Op_Not_In

   function ada_membership_expr_f_membership_exprs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_membership_expr_f_membership_exprs";
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

   function ada_name_p_enclosing_defining_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_enclosing_defining_name";
      --  If this name is part of a defining name, return the enclosing
      --  defining name node.

   function ada_name_p_is_defining
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_defining";
      --  Return True if this name is part of a defining name.

   function ada_name_p_name_is
     (Node : ada_base_entity_Ptr;
Sym        : access constant ada_symbol_type;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_name_is";
      --  Helper. Check that this name matches ``sym``.

   function ada_name_p_is_direct_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_direct_call";
      --  Return True iff this name represents a call to a subprogram which
      --  is referred by its defining name. (i.e. not through a subprogram
      --  access).

   function ada_name_p_is_access_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_access_call";
      --  Return True iff this name represents a call to subprogram through an
      --  access type.

   function ada_name_p_is_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_call";
      --  Returns True if this Name corresponds to a call.

   function ada_name_p_is_dot_call
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_dot_call";
      --  Returns True if this Name corresponds to a dot notation call.

   function ada_name_p_failsafe_referenced_def_name
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_internal_refd_def) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_failsafe_referenced_def_name";
      --  Failsafe version of ``referenced_defining_name``. Returns a
      --  ``RefdDef``, which can be precise, imprecise, or error.

   function ada_name_p_referenced_defining_name
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_referenced_defining_name";
      --  Like ``referenced_decl``, but will return the defining identifier for
      --  the decl, rather than the basic declaration node itself.

   function ada_name_p_all_env_elements
     (Node : ada_base_entity_Ptr;
Seq :
ada_bool;
      Seq_From : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_all_env_elements";
      --  Return all elements in self's scope that are lexically named like
      --  Self.

   function ada_name_p_called_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_called_subp_spec";
      --  Return the subprogram specification of the subprogram or subprogram
      --  access that is being called by this exact Name, if relevant.

   function ada_name_p_referenced_decl
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_referenced_decl";
      --  Return the declaration this node references after name resolution.
      --  If imprecise_fallback is True, errors raised during resolution of
      --  the xref equation are catched and a fallback mechanism is triggered,
      --  which tries to find the referenced declaration in an ad-hoc way.

   function ada_name_p_failsafe_referenced_decl
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_internal_refd_decl) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_failsafe_referenced_decl";
      --  Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``,
      --  which can be precise, imprecise, or error.

   function ada_name_p_referenced_decl_internal
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_internal_refd_decl) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_referenced_decl_internal";
      --  Return the declaration this node references. Try not to run name res
      --  if already resolved. INTERNAL USE ONLY.

   function ada_name_p_name_designated_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_name_designated_type";
      --  Like SubtypeIndication.designated_type, but on names, since because
      --  of Ada's ambiguous grammar, some subtype indications will be parsed
      --  as names.

   function ada_name_p_is_static_subtype
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_static_subtype";
      --  Returns whether Self denotes a static subtype or not.

   function ada_name_p_name_matches
     (Node : ada_base_entity_Ptr;
N          : access constant ada_base_entity;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_name_matches";
      --  Return whether two names match each other.
      --
      --  This compares the symbol for Identifier and StringLiteral nodes. We
      --  consider that there is no match for all other node kinds.

   function ada_name_p_relative_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_relative_name";
      --  Returns the relative name of this instance. For example, for a prefix
      --  A.B.C, this will return C.

   function ada_name_p_is_operator_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_operator_name";
      --  Return whether the name that Self designates is an operator.

   function ada_name_p_is_write_reference
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_write_reference";
      --  Whether this name is a write reference.
      --
      --  For example, `X` is a write reference in the following cases::
      --
      --  1. `X := 2;` 2. `X (2) := 2;` 3. `P(F => X)` where F is declared
      --  `out` or `in out`. 4. `X'Access`. 5. `X.C := 2`, `R.X := 2`
      --
      --  .. note:: This is an experimental feature. There might be some
      --     discrepancy with the GNAT concept of "write reference".

   function ada_name_p_is_dispatching_call
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_dispatching_call";
      --  Returns True if this Name corresponds to a dispatching call,
      --  including:
      --
      --  - calls done through subprogram access types.
      --
      --  - calls to dispatching subprograms, in the object-oriented sense.
      --
      --  .. note:: This is an experimental feature. There might be some
      --     discrepancy with the GNAT concept of "dispatching call".

   function ada_name_p_is_static_call
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_is_static_call";
      --  Returns True if this Name corresponds to a static non-dispatching
      --  call. In other words, this will return True if and only if the
      --  target of the call is known statically.
      --
      --  .. note:: This is an experimental feature. There might be some
      --     discrepancy with the GNAT concept of "static call".

   function ada_name_p_as_symbol_array
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_name_p_as_symbol_array";
      --  Turn this name into an array of symbols.
      --
      --  For instance, a node with name ``A.B.C`` is turned into ``['A', 'B',
      --  'C']``.

   function ada_attribute_ref_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_attribute_ref_f_prefix";
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

   function ada_attribute_ref_f_attribute
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_attribute_ref_f_attribute";

   function ada_attribute_ref_f_args
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_attribute_ref_f_args";

   function ada_call_expr_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_call_expr_f_name";
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

   function ada_call_expr_f_suffix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_call_expr_f_suffix";
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

   function ada_call_expr_p_is_array_slice
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_call_expr_p_is_array_slice";
      --  Return whether this CallExpr is actually an access to a slice of the
      --  array denoted by the prefix of this CallExpr.

   function ada_defining_name_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_f_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_defining_name_p_basic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_basic_decl";
      --  Returns this DefiningName's basic declaration

   function ada_defining_name_p_find_refs
     (Node   : ada_base_entity_Ptr;
Root         : access constant ada_base_entity;
      Origin : access constant ada_base_entity; Imprecise_Fallback :
ada_bool;

      Value_P : access ada_ref_result_array) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_find_refs";
      --  Find all references to this defining name in the given ``root`` and
      --  its children.

   function ada_defining_name_p_find_all_references
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ref_result_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_find_all_references";
      --  Searches all references to this defining name in the given list of
      --  units.

   function ada_defining_name_p_find_all_calls
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ref_result_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_find_all_calls";
      --  Return the list of all possible calls to the subprogram which Self is
      --  the defining name of.
      --
      --  This will return the name corresponding to the call, excluding the
      --  parameters if there are any. For instance, it will return `A` for
      --  the `A (B)` call.
      --
      --  .. note:: This does not yet support calls done inside generics.

   function ada_defining_name_p_next_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_next_part";
      --  Like ``BasicDecl.next_part_for_decl`` on a defining name

   function ada_defining_name_p_previous_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_previous_part";
      --  Like ``BasicDecl.previous_part_for_decl`` on a defining name

   function ada_defining_name_p_canonical_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_defining_name_p_canonical_part";
      --  Like ``BasicDecl.canonical_part`` on a defining name

   function ada_discrete_subtype_name_f_subtype
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_discrete_subtype_name_f_subtype";

   function ada_dotted_name_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_dotted_name_f_prefix";
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

   function ada_dotted_name_f_suffix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_dotted_name_f_suffix";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_end_name_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_end_name_f_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_end_name_p_basic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_end_name_p_basic_decl";
      --  Returns this EndName's basic declaration

   function ada_explicit_deref_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_explicit_deref_f_prefix";
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

   function ada_qual_expr_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_qual_expr_f_prefix";
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

   function ada_qual_expr_f_suffix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_qual_expr_f_suffix";
      --  This field can contain one of the following nodes:
      --
      --  * Base_Aggregate
      --
      --  * Paren_Expr

   function ada_single_tok_node_p_canonical_text
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_symbol_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_single_tok_node_p_canonical_text";
      --  Return a canonicalized version of this node's text.

   function ada_char_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;
Value_P    : access uint32_t) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_char_literal_p_denoted_value";
      --  Return the value that this literal denotes.

   function ada_string_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_string_literal_p_denoted_value";
      --  Return the value that this literal denotes.

   function ada_int_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_big_integer)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_int_literal_p_denoted_value";
      --  Return the value that this literal denotes.

   function ada_paren_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_paren_expr_f_expr";
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

   function ada_quantified_expr_f_quantifier
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_quantified_expr_f_quantifier";

   function ada_quantified_expr_f_loop_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_quantified_expr_f_loop_spec";

   function ada_quantified_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_quantified_expr_f_expr";
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

   function ada_raise_expr_f_exception_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_raise_expr_f_exception_name";

   function ada_raise_expr_f_error_message
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_raise_expr_f_error_message";

   function ada_un_op_f_op
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_un_op_f_op";
      --  This field can contain one of the following nodes:
      --
      --  * Op_Abs
      --
      --  * Op_Minus
      --
      --  * Op_Not
      --
      --  * Op_Plus

   function ada_un_op_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_un_op_f_expr";
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

   function ada_handled_stmts_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_handled_stmts_f_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_handled_stmts_f_exceptions
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_handled_stmts_f_exceptions";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Exception_Handler
      --
      --  * Pragma_Node

   function ada_library_item_f_has_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_library_item_f_has_private";

   function ada_library_item_f_item
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_library_item_f_item";
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

   function ada_limited_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_limited_node_p_as_bool";
      --  Return whether this is an instance of LimitedPresent

   function ada_for_loop_spec_f_var_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_for_loop_spec_f_var_decl";

   function ada_for_loop_spec_f_loop_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_for_loop_spec_f_loop_type";

   function ada_for_loop_spec_f_has_reverse
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_for_loop_spec_f_has_reverse";

   function ada_for_loop_spec_f_iter_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_for_loop_spec_f_iter_expr";
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

   function ada_while_loop_spec_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_while_loop_spec_f_expr";
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

   function ada_not_null_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_not_null_p_as_bool";
      --  Return whether this is an instance of NotNullPresent

   function ada_params_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_params_f_params";

   function ada_pragma_node_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_pragma_node_f_id";

   function ada_pragma_node_f_args
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_pragma_node_f_args";

   function ada_pragma_node_p_associated_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_pragma_node_p_associated_decls";
      --  Return an array of ``BasicDecl`` instances associated with this
      --  pragma, or an empty array if non applicable.

   function ada_private_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_private_node_p_as_bool";
      --  Return whether this is an instance of PrivatePresent

   function ada_protected_def_f_public_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_def_f_public_part";

   function ada_protected_def_f_private_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_def_f_private_part";

   function ada_protected_def_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_def_f_end_name";

   function ada_protected_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_protected_node_p_as_bool";
      --  Return whether this is an instance of ProtectedPresent

   function ada_range_spec_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_range_spec_f_range";
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

   function ada_renaming_clause_f_renamed_object
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_renaming_clause_f_renamed_object";

   function ada_reverse_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_reverse_node_p_as_bool";
      --  Return whether this is an instance of ReversePresent

   function ada_select_when_part_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_select_when_part_f_cond_expr";
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

   function ada_select_when_part_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_select_when_part_f_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_accept_stmt_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_accept_stmt_f_name";

   function ada_accept_stmt_f_entry_index_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_accept_stmt_f_entry_index_expr";
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

   function ada_accept_stmt_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_accept_stmt_f_params";

   function ada_accept_stmt_with_stmts_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_accept_stmt_with_stmts_f_stmts";

   function ada_accept_stmt_with_stmts_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_accept_stmt_with_stmts_f_end_name";

   function ada_base_loop_stmt_f_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_loop_stmt_f_spec";

   function ada_base_loop_stmt_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_loop_stmt_f_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_base_loop_stmt_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_base_loop_stmt_f_end_name";

   function ada_begin_block_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_begin_block_f_stmts";

   function ada_begin_block_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_begin_block_f_end_name";

   function ada_decl_block_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_decl_block_f_decls";

   function ada_decl_block_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_decl_block_f_stmts";

   function ada_decl_block_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_decl_block_f_end_name";

   function ada_case_stmt_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_stmt_f_expr";
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

   function ada_case_stmt_f_alternatives
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_case_stmt_f_alternatives";

   function ada_extended_return_stmt_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_extended_return_stmt_f_decl";

   function ada_extended_return_stmt_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_extended_return_stmt_f_stmts";

   function ada_if_stmt_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_stmt_f_cond_expr";
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

   function ada_if_stmt_f_then_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_stmt_f_then_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_if_stmt_f_alternatives
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_stmt_f_alternatives";

   function ada_if_stmt_f_else_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_if_stmt_f_else_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_named_stmt_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_named_stmt_f_decl";

   function ada_named_stmt_f_stmt
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_named_stmt_f_stmt";
      --  This field can contain one of the following nodes:
      --
      --  * Base_Loop_Stmt
      --
      --  * Block_Stmt

   function ada_select_stmt_f_guards
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_select_stmt_f_guards";

   function ada_select_stmt_f_else_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_select_stmt_f_else_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_select_stmt_f_abort_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_select_stmt_f_abort_stmts";
      --  This field contains a list that itself contains one of the following
      --  nodes:
      --
      --  * Pragma_Node
      --
      --  * Stmt

   function ada_abort_stmt_f_names
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_abort_stmt_f_names";
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

   function ada_assign_stmt_f_dest
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_assign_stmt_f_dest";
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

   function ada_assign_stmt_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_assign_stmt_f_expr";
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

   function ada_call_stmt_f_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_call_stmt_f_call";
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

   function ada_delay_stmt_f_has_until
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_delay_stmt_f_has_until";

   function ada_delay_stmt_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_delay_stmt_f_expr";
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

   function ada_exit_stmt_f_loop_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exit_stmt_f_loop_name";

   function ada_exit_stmt_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_exit_stmt_f_cond_expr";
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

   function ada_goto_stmt_f_label_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_goto_stmt_f_label_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_label_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_label_f_decl";

   function ada_raise_stmt_f_exception_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_raise_stmt_f_exception_name";

   function ada_raise_stmt_f_error_message
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_raise_stmt_f_error_message";

   function ada_requeue_stmt_f_call_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_requeue_stmt_f_call_name";
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

   function ada_requeue_stmt_f_has_abort
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_requeue_stmt_f_has_abort";

   function ada_return_stmt_f_return_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_return_stmt_f_return_expr";
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

   function ada_subunit_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subunit_f_name";
      --  This field can contain one of the following nodes:
      --
      --  * Char_Literal
      --
      --  * Dotted_Name
      --
      --  * Identifier
      --
      --  * String_Literal

   function ada_subunit_f_body
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subunit_f_body";
      --  This field can contain one of the following nodes:
      --
      --  * Package_Body
      --
      --  * Protected_Body
      --
      --  * Subp_Body
      --
      --  * Task_Body

   function ada_subunit_p_body_root
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subunit_p_body_root";
      --  Return the body in which this subunit is rooted.

   function ada_synchronized_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_synchronized_node_p_as_bool";
      --  Return whether this is an instance of SynchronizedPresent

   function ada_tagged_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_tagged_node_p_as_bool";
      --  Return whether this is an instance of TaggedPresent

   function ada_task_def_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_def_f_interfaces";
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

   function ada_task_def_f_public_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_def_f_public_part";

   function ada_task_def_f_private_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_def_f_private_part";

   function ada_task_def_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_task_def_f_end_name";

   function ada_access_def_f_has_not_null
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_access_def_f_has_not_null";

   function ada_access_to_subp_def_f_has_protected
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_access_to_subp_def_f_has_protected";

   function ada_access_to_subp_def_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_access_to_subp_def_f_subp_spec";

   function ada_anonymous_type_access_def_f_type_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_anonymous_type_access_def_f_type_decl";

   function ada_type_access_def_f_has_all
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_access_def_f_has_all";

   function ada_type_access_def_f_has_constant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_access_def_f_has_constant";

   function ada_type_access_def_f_subtype_indication
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_access_def_f_subtype_indication";

   function ada_array_type_def_f_indices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_array_type_def_f_indices";

   function ada_array_type_def_f_component_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_array_type_def_f_component_type";

   function ada_derived_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_has_abstract";

   function ada_derived_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_has_limited";

   function ada_derived_type_def_f_has_synchronized
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_has_synchronized";

   function ada_derived_type_def_f_subtype_indication
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_subtype_indication";

   function ada_derived_type_def_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_interfaces";
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

   function ada_derived_type_def_f_record_extension
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_record_extension";

   function ada_derived_type_def_f_has_with_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_derived_type_def_f_has_with_private";

   function ada_enum_type_def_f_enum_literals
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_enum_type_def_f_enum_literals";

   function ada_interface_type_def_f_interface_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_interface_type_def_f_interface_kind";

   function ada_interface_type_def_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_interface_type_def_f_interfaces";
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

   function ada_mod_int_type_def_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_mod_int_type_def_f_expr";
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

   function ada_private_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_private_type_def_f_has_abstract";

   function ada_private_type_def_f_has_tagged
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_private_type_def_f_has_tagged";

   function ada_private_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_private_type_def_f_has_limited";

   function ada_decimal_fixed_point_def_f_delta
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_decimal_fixed_point_def_f_delta";
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

   function ada_decimal_fixed_point_def_f_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_decimal_fixed_point_def_f_digits";
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

   function ada_decimal_fixed_point_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_decimal_fixed_point_def_f_range";

   function ada_floating_point_def_f_num_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_floating_point_def_f_num_digits";
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

   function ada_floating_point_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_floating_point_def_f_range";

   function ada_ordinary_fixed_point_def_f_delta
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ordinary_fixed_point_def_f_delta";
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

   function ada_ordinary_fixed_point_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_ordinary_fixed_point_def_f_range";

   function ada_record_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_type_def_f_has_abstract";

   function ada_record_type_def_f_has_tagged
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_type_def_f_has_tagged";

   function ada_record_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_type_def_f_has_limited";

   function ada_record_type_def_f_record_def
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_record_type_def_f_record_def";

   function ada_signed_int_type_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_signed_int_type_def_f_range";

   function ada_type_expr_p_type_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_expr_p_type_name";
      --  Return the name node for this type expression, if applicable, else
      --  null

   function ada_type_expr_p_designated_type_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_expr_p_designated_type_decl";
      --  Returns the type declaration designated by this type expression.

   function ada_type_expr_p_designated_type_decl_from
     (Node : ada_base_entity_Ptr;

      Origin_Node : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_type_expr_p_designated_type_decl_from";
      --  Return the type declaration designated by this type expression as
      --  viewed from the node given by origin_node.

   function ada_anonymous_type_f_type_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_anonymous_type_f_type_decl";

   function ada_subtype_indication_f_has_not_null
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subtype_indication_f_has_not_null";

   function ada_subtype_indication_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subtype_indication_f_name";
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

   function ada_subtype_indication_f_constraint
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subtype_indication_f_constraint";

   function ada_subtype_indication_p_is_static_subtype
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_subtype_indication_p_is_static_subtype";
      --  Returns whether Self denotes a static subtype or not.

   function ada_unconstrained_array_index_f_subtype_indication
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_unconstrained_array_index_f_subtype_indication";

   function ada_until_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_until_node_p_as_bool";
      --  Return whether this is an instance of UntilPresent

   function ada_use_package_clause_f_packages
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_use_package_clause_f_packages";
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

   function ada_use_type_clause_f_has_all
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_use_type_clause_f_has_all";

   function ada_use_type_clause_f_types
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_use_type_clause_f_types";
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

   function ada_variant_f_choices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_variant_f_choices";
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

   function ada_variant_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_variant_f_components";

   function ada_variant_part_f_discr_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_variant_part_f_discr_name";

   function ada_variant_part_f_variant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_variant_part_f_variant";

   function ada_with_clause_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_with_clause_f_has_limited";

   function ada_with_clause_f_has_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_with_clause_f_has_private";

   function ada_with_clause_f_packages
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity)
      return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_with_clause_f_packages";
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

   function ada_with_private_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int
with
      Export        => True,
      Convention    => C,
      External_Name => "ada_with_private_p_as_bool";
      --  Return whether this is an instance of WithPrivatePresent

      ------------------------
      -- Conversion helpers --
      ------------------------

      --  The following conversion helpers are use by the various C bindings

   function Wrap (S : Source_Location) return ada_source_location is
     ((Unsigned_32 (S.Line), Unsigned_16 (S.Column)));
   function Unwrap (S : ada_source_location) return Source_Location is
     ((Line_Number (S.Line), Column_Number (S.Column)));

   function Wrap
     (S : Source_Location_Range) return ada_source_location_range is
     ((Start_S => (Unsigned_32 (S.Start_Line), Unsigned_16 (S.Start_Column)),
       End_S   => (Unsigned_32 (S.End_Line), Unsigned_16 (S.End_Column))));
   function Unwrap
     (S : ada_source_location_range) return Source_Location_Range is
     ((Line_Number (S.Start_S.Line), Line_Number (S.End_S.Line),
       Column_Number (S.Start_S.Column), Column_Number (S.End_S.Column)));

   function Wrap (S : Unbounded_Wide_Wide_String) return ada_text;

   function Wrap_Alloc (S : Text_Type) return ada_text;
   function Wrap
     (S : Text_Cst_Access; First : Positive; Last : Natural) return ada_text;

   function Wrap (T : Text_Cst_Access) return ada_text is
     (if T = null then
        (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
      else (Chars     => T.all'Address, Length => T.all'Length,
         Is_Allocated => 0));
   function Wrap (T : Text_Access) return ada_text is
     (Wrap (Text_Cst_Access (T)));

   --  The following conversions are used only at the interface between Ada and
   --  C (i.e. as parameters and return types for C entry points) for access
   --  types. All read/writes for the pointed values are made through the
   --  access values and never through the System.Address values. Thus,
   --  strict aliasing issues should not arise for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "possible aliasing problem for type");

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, ada_big_integer);
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (ada_big_integer, Big_Integer_Type);

   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, ada_symbol_type);
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (ada_symbol_type, Symbol_Type);

   function Wrap is new Ada.Unchecked_Conversion
     (Bare_Ada_Node, ada_base_node);
   function Unwrap is new Ada.Unchecked_Conversion
     (ada_base_node, Bare_Ada_Node);

   function Wrap (Token : Token_Reference) return ada_token;
   function Unwrap (Token : ada_token) return Token_Reference;

   function Wrap_Private_Provider is new Ada.Unchecked_Conversion
     (Internal_Unit_Provider_Access, ada_unit_provider);
   function Unwrap_Private_Provider is new Ada.Unchecked_Conversion
     (ada_unit_provider, Internal_Unit_Provider_Access);

   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);

   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Aspect_Assoc_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Aspect_Assoc_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Base_Formal_Param_Decl_Array_Access,
      Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access,
      Internal_Entity_Base_Formal_Param_Decl_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Base_Type_Decl_Array_Access,
      Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access,
      Internal_Entity_Base_Type_Decl_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Basic_Assoc_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Basic_Assoc_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Basic_Decl_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Basic_Decl_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Compilation_Unit_Array_Access,
      Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access,
      Internal_Entity_Compilation_Unit_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Defining_Name_Array_Access,
      Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access,
      Internal_Entity_Defining_Name_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Generic_Instantiation_Array_Access,
      Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access,
      Internal_Entity_Generic_Instantiation_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Param_Spec_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Param_Spec_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Pragma_Node_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Pragma_Node_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Type_Decl_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Type_Decl_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Type_Expr_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Type_Expr_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Variant_Array_Access, Internal_Entity_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Internal_Entity_Array_Access, Internal_Entity_Variant_Array_Access);

   pragma Warnings (On, "possible aliasing problem for type");

end Libadalang.Implementation.C;
