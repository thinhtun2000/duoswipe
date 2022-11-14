with Ada.Finalization;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;
use type System.Address;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libadalang.Private_Converters; use Libadalang.Private_Converters;

with Libadalang.Implementation.Extensions;
use Libadalang.Implementation.Extensions;

package body Libadalang.Implementation.C is

   --  Avoid hiding from $.Lexer
   subtype Token_Data_Type is Common.Token_Data_Type;

   type C_Unit_Provider is limited new Ada.Finalization.Limited_Controlled and
     Internal_Unit_Provider with record
      Ref_Count               : Natural;
      Data                    : System.Address;
      Destroy_Func            : ada_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : ada_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : ada_unit_provider_get_unit_from_name_callback;
   end record;

   type C_Unit_Provider_Access is access all C_Unit_Provider;

   overriding procedure Finalize (Provider : in out C_Unit_Provider);
   overriding procedure Inc_Ref (Provider : in out C_Unit_Provider);
   overriding function Dec_Ref
     (Provider : in out C_Unit_Provider) return Boolean;

   overriding function Get_Unit_Filename
     (Provider : C_Unit_Provider; Name : Text_Type; Kind : Analysis_Unit_Kind)
      return String;

   overriding function Get_Unit
     (Provider : C_Unit_Provider; Context : Internal_Context; Name : Text_Type;
      Kind     : Analysis_Unit_Kind; Charset : String := "";
      Reparse  : Boolean := False) return Internal_Unit;

   function Value_Or_Empty (S : chars_ptr) return String
   --  If S is null, return an empty string. Return Value (S) otherwise.
   is
     (if S = Null_Ptr then "" else Value (S));

   Last_Exception : ada_exception_Ptr := null;

   ----------
   -- Free --
   ----------

   procedure Free (Address : System.Address) is
      procedure C_Free (Address : System.Address) with
         Import        => True,
         Convention    => C,
         External_Name => "free";
   begin
      C_Free (Address);
   end Free;

   -------------------------
   -- Analysis primitives --
   -------------------------

   function ada_create_analysis_context
     (Charset     : chars_ptr; Unit_Provider : ada_unit_provider;
      With_Trivia : int; Tab_Stop : int) return ada_analysis_context
   is
   begin
      Clear_Last_Exception;

      declare
         C : constant String :=
           (if Charset = Null_Ptr then "iso-8859-1" else Value (Charset));
      begin
         return Create_Context
             (Charset       => C,
              Unit_Provider => Unwrap_Private_Provider (Unit_Provider),
              With_Trivia => With_Trivia /= 0, Tab_Stop => Natural (Tab_Stop));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_create_analysis_context;

   function ada_context_incref
     (Context : ada_analysis_context) return ada_analysis_context
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (Context);
      return Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_context_incref;

   procedure ada_context_decref (Context : ada_analysis_context) is
      Context_Var : Internal_Context := Context;
   begin
      Clear_Last_Exception;
      Dec_Ref (Context_Var);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_context_decref;

   function ada_context_symbol
     (Context : ada_analysis_context; Text : access ada_text;
      Symbol  : access ada_symbol_type) return int
   is
      Raw_Text : Text_Type (1 .. Natural (Text.Length)) with
         Import,
         Address => Text.Chars;
   begin
      Clear_Last_Exception;
      Symbol.all := Wrap_Symbol (Lookup_Symbol (Context, Raw_Text));
      return 1;
   exception
      when Invalid_Symbol_Error =>
         return 0;
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_context_symbol;

   procedure ada_context_discard_errors_in_populate_lexical_env
     (Context : ada_analysis_context; Discard : int)
   is
   begin
      Clear_Last_Exception;
      Discard_Errors_In_Populate_Lexical_Env (Context, Discard /= 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_context_discard_errors_in_populate_lexical_env;

   function ada_get_analysis_unit_from_file
     (Context : ada_analysis_context; Filename, Charset : chars_ptr;
      Reparse : int; Rule : ada_grammar_rule) return ada_analysis_unit
   is
   begin
      Clear_Last_Exception;

      return Get_From_File
          (Context, Value (Filename), Value_Or_Empty (Charset), Reparse /= 0,
           Rule);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_get_analysis_unit_from_file;

   function ada_get_analysis_unit_from_buffer
     (Context : ada_analysis_context; Filename, Charset : chars_ptr;
      Buffer  : chars_ptr; Buffer_Size : size_t; Rule : ada_grammar_rule)
      return ada_analysis_unit
   is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size)) with
            Import,
            Address => Convert (Buffer);
      begin
         return Get_From_Buffer
             (Context, Value (Filename), Value_Or_Empty (Charset), Buffer_Str,
              Rule);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_get_analysis_unit_from_buffer;

   function ada_get_analysis_unit_from_provider
     (Context : ada_analysis_context; Name : ada_text;
      Kind    : ada_analysis_unit_kind; Charset : chars_ptr; Reparse : int)
      return ada_analysis_unit
   is
   begin
      Clear_Last_Exception;

      declare
         Text_Name : Text_Type (1 .. Natural (Name.Length)) with
            Import,
            Address => Name.Chars;
      begin
         return Get_From_Provider
             (Context, Text_Name, Kind, Value_Or_Empty (Charset),
              Reparse /= 0);
      end;
   exception
      when Invalid_Unit_Name_Error =>
         return null;
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_get_analysis_unit_from_provider;

   procedure ada_unit_root
     (Unit : ada_analysis_unit; Result_P : ada_base_entity_Ptr)
   is
   begin
      Clear_Last_Exception;

      Result_P.all := (Unit.AST_Root, No_Entity_Info);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_root;

   procedure ada_unit_first_token
     (Unit : ada_analysis_unit; Token : access ada_token)
   is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := First_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_first_token;

   procedure ada_unit_last_token
     (Unit : ada_analysis_unit; Token : access ada_token)
   is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := Last_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_last_token;

   function ada_unit_token_count (Unit : ada_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Token_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end ada_unit_token_count;

   function ada_unit_trivia_count (Unit : ada_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Trivia_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end ada_unit_trivia_count;

   procedure ada_unit_lookup_token
     (Unit   : ada_analysis_unit; Sloc : access ada_source_location;
      Result : access ada_token)
   is
   begin
      Clear_Last_Exception;

      declare
         S   : constant Source_Location := Unwrap (Sloc.all);
         Tok : constant Token_Reference := Lookup_Token (Unit, S);
      begin
         Result.all := Wrap (Tok);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_lookup_token;

   procedure ada_unit_dump_lexical_env (Unit : ada_analysis_unit) is
   begin
      Clear_Last_Exception;
      Dump_Lexical_Env (Unit);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_dump_lexical_env;

   function ada_unit_filename (Unit : ada_analysis_unit) return chars_ptr is
   begin
      Clear_Last_Exception;

      return New_String (Get_Filename (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return Null_Ptr;
   end ada_unit_filename;

   function ada_unit_diagnostic_count
     (Unit : ada_analysis_unit) return unsigned
   is
   begin
      Clear_Last_Exception;

      return unsigned (Unit.Diagnostics.Length);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_unit_diagnostic_count;

   function ada_unit_diagnostic
     (Unit         : ada_analysis_unit; N : unsigned;
      Diagnostic_P : access ada_diagnostic) return int
   is
   begin
      Clear_Last_Exception;

      if N < unsigned (Unit.Diagnostics.Length) then
         declare
            D_In  : Diagnostic renames Unit.Diagnostics (Natural (N) + 1);
            D_Out : ada_diagnostic renames Diagnostic_P.all;
         begin
            D_Out.Sloc_Range := Wrap (D_In.Sloc_Range);
            D_Out.Message    := Wrap (D_In.Message);
            return 1;
         end;
      else
         return 0;
      end if;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_unit_diagnostic;

   function ada_unit_context
     (Unit : ada_analysis_unit) return ada_analysis_context
   is
   begin
      Clear_Last_Exception;
      return Unit.Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_unit_context;

   procedure ada_unit_reparse_from_file
     (Unit : ada_analysis_unit; Charset : chars_ptr)
   is
   begin
      Clear_Last_Exception;

      Reparse (Unit, Value_Or_Empty (Charset));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_reparse_from_file;

   procedure ada_unit_reparse_from_buffer
     (Unit        : ada_analysis_unit; Charset : chars_ptr; Buffer : chars_ptr;
      Buffer_Size : size_t)
   is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size)) with
            Import,
            Address => Convert (Buffer);
      begin
         Reparse (Unit, Value_Or_Empty (Charset), Buffer_Str);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unit_reparse_from_buffer;

   function ada_unit_populate_lexical_env (Unit : ada_analysis_unit) return int
   is
   begin
      Clear_Last_Exception;
      Populate_Lexical_Env (Unit);
      return 1;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_unit_populate_lexical_env;

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (Ada_Node_Kind_Type) of Text_Access :=
     (Ada_Abort_Absent => new Text_Type'(To_Text ("AbortAbsent")),
      Ada_Abort_Present => new Text_Type'(To_Text ("AbortPresent")),
      Ada_Abstract_Absent => new Text_Type'(To_Text ("AbstractAbsent")),
      Ada_Abstract_Present => new Text_Type'(To_Text ("AbstractPresent")),
      Ada_Ada_Node_List => new Text_Type'(To_Text ("AdaNodeList")),
      Ada_Alternatives_List => new Text_Type'(To_Text ("AlternativesList")),
      Ada_Constraint_List => new Text_Type'(To_Text ("ConstraintList")),
      Ada_Decl_List                  => new Text_Type'(To_Text ("DeclList")),
      Ada_Stmt_List                  => new Text_Type'(To_Text ("StmtList")),
      Ada_Aspect_Assoc_List => new Text_Type'(To_Text ("AspectAssocList")),
      Ada_Base_Assoc_List => new Text_Type'(To_Text ("BaseAssocList")),
      Ada_Assoc_List                 => new Text_Type'(To_Text ("AssocList")),
      Ada_Case_Expr_Alternative_List =>
        new Text_Type'(To_Text ("CaseExprAlternativeList")),
      Ada_Case_Stmt_Alternative_List =>
        new Text_Type'(To_Text ("CaseStmtAlternativeList")),
      Ada_Compilation_Unit_List =>
        new Text_Type'(To_Text ("CompilationUnitList")),
      Ada_Contract_Case_Assoc_List =>
        new Text_Type'(To_Text ("ContractCaseAssocList")),
      Ada_Defining_Name_List => new Text_Type'(To_Text ("DefiningNameList")),
      Ada_Discriminant_Spec_List =>
        new Text_Type'(To_Text ("DiscriminantSpecList")),
      Ada_Elsif_Expr_Part_List =>
        new Text_Type'(To_Text ("ElsifExprPartList")),
      Ada_Elsif_Stmt_Part_List =>
        new Text_Type'(To_Text ("ElsifStmtPartList")),
      Ada_Enum_Literal_Decl_List =>
        new Text_Type'(To_Text ("EnumLiteralDeclList")),
      Ada_Expr_Alternatives_List =>
        new Text_Type'(To_Text ("ExprAlternativesList")),
      Ada_Discriminant_Choice_List =>
        new Text_Type'(To_Text ("DiscriminantChoiceList")),
      Ada_Name_List             => new Text_Type'(To_Text ("NameList")),
      Ada_Parent_List           => new Text_Type'(To_Text ("ParentList")),
      Ada_Param_Spec_List       => new Text_Type'(To_Text ("ParamSpecList")),
      Ada_Pragma_Node_List      => new Text_Type'(To_Text ("PragmaNodeList")),
      Ada_Select_When_Part_List =>
        new Text_Type'(To_Text ("SelectWhenPartList")),
      Ada_Unconstrained_Array_Index_List =>
        new Text_Type'(To_Text ("UnconstrainedArrayIndexList")),
      Ada_Variant_List              => new Text_Type'(To_Text ("VariantList")),
      Ada_Aliased_Absent => new Text_Type'(To_Text ("AliasedAbsent")),
      Ada_Aliased_Present => new Text_Type'(To_Text ("AliasedPresent")),
      Ada_All_Absent                => new Text_Type'(To_Text ("AllAbsent")),
      Ada_All_Present               => new Text_Type'(To_Text ("AllPresent")),
      Ada_Constrained_Array_Indices =>
        new Text_Type'(To_Text ("ConstrainedArrayIndices")),
      Ada_Unconstrained_Array_Indices =>
        new Text_Type'(To_Text ("UnconstrainedArrayIndices")),
      Ada_Aspect_Assoc         => new Text_Type'(To_Text ("AspectAssoc")),
      Ada_At_Clause            => new Text_Type'(To_Text ("AtClause")),
      Ada_Attribute_Def_Clause =>
        new Text_Type'(To_Text ("AttributeDefClause")),
      Ada_Enum_Rep_Clause       => new Text_Type'(To_Text ("EnumRepClause")),
      Ada_Record_Rep_Clause     => new Text_Type'(To_Text ("RecordRepClause")),
      Ada_Aspect_Spec           => new Text_Type'(To_Text ("AspectSpec")),
      Ada_Contract_Case_Assoc => new Text_Type'(To_Text ("ContractCaseAssoc")),
      Ada_Pragma_Argument_Assoc =>
        new Text_Type'(To_Text ("PragmaArgumentAssoc")),
      Ada_Entry_Spec              => new Text_Type'(To_Text ("EntrySpec")),
      Ada_Enum_Subp_Spec          => new Text_Type'(To_Text ("EnumSubpSpec")),
      Ada_Subp_Spec               => new Text_Type'(To_Text ("SubpSpec")),
      Ada_Component_List          => new Text_Type'(To_Text ("ComponentList")),
      Ada_Known_Discriminant_Part =>
        new Text_Type'(To_Text ("KnownDiscriminantPart")),
      Ada_Unknown_Discriminant_Part =>
        new Text_Type'(To_Text ("UnknownDiscriminantPart")),
      Ada_Entry_Completion_Formal_Params =>
        new Text_Type'(To_Text ("EntryCompletionFormalParams")),
      Ada_Generic_Formal_Part => new Text_Type'(To_Text ("GenericFormalPart")),
      Ada_Null_Record_Def       => new Text_Type'(To_Text ("NullRecordDef")),
      Ada_Record_Def            => new Text_Type'(To_Text ("RecordDef")),
      Ada_Aggregate_Assoc       => new Text_Type'(To_Text ("AggregateAssoc")),
      Ada_Multi_Dim_Array_Assoc =>
        new Text_Type'(To_Text ("MultiDimArrayAssoc")),
      Ada_Discriminant_Assoc => new Text_Type'(To_Text ("DiscriminantAssoc")),
      Ada_Param_Assoc             => new Text_Type'(To_Text ("ParamAssoc")),
      Ada_Component_Decl          => new Text_Type'(To_Text ("ComponentDecl")),
      Ada_Discriminant_Spec => new Text_Type'(To_Text ("DiscriminantSpec")),
      Ada_Generic_Formal_Obj_Decl =>
        new Text_Type'(To_Text ("GenericFormalObjDecl")),
      Ada_Generic_Formal_Package =>
        new Text_Type'(To_Text ("GenericFormalPackage")),
      Ada_Generic_Formal_Subp_Decl =>
        new Text_Type'(To_Text ("GenericFormalSubpDecl")),
      Ada_Generic_Formal_Type_Decl =>
        new Text_Type'(To_Text ("GenericFormalTypeDecl")),
      Ada_Param_Spec               => new Text_Type'(To_Text ("ParamSpec")),
      Ada_Generic_Package_Internal =>
        new Text_Type'(To_Text ("GenericPackageInternal")),
      Ada_Package_Decl => new Text_Type'(To_Text ("PackageDecl")),
      Ada_Discrete_Base_Subtype_Decl =>
        new Text_Type'(To_Text ("DiscreteBaseSubtypeDecl")),
      Ada_Subtype_Decl         => new Text_Type'(To_Text ("SubtypeDecl")),
      Ada_Classwide_Type_Decl => new Text_Type'(To_Text ("ClasswideTypeDecl")),
      Ada_Incomplete_Type_Decl =>
        new Text_Type'(To_Text ("IncompleteTypeDecl")),
      Ada_Incomplete_Tagged_Type_Decl =>
        new Text_Type'(To_Text ("IncompleteTaggedTypeDecl")),
      Ada_Protected_Type_Decl => new Text_Type'(To_Text ("ProtectedTypeDecl")),
      Ada_Task_Type_Decl        => new Text_Type'(To_Text ("TaskTypeDecl")),
      Ada_Single_Task_Type_Decl =>
        new Text_Type'(To_Text ("SingleTaskTypeDecl")),
      Ada_Type_Decl                 => new Text_Type'(To_Text ("TypeDecl")),
      Ada_Anonymous_Type_Decl => new Text_Type'(To_Text ("AnonymousTypeDecl")),
      Ada_Synth_Anonymous_Type_Decl =>
        new Text_Type'(To_Text ("SynthAnonymousTypeDecl")),
      Ada_Abstract_Subp_Decl => new Text_Type'(To_Text ("AbstractSubpDecl")),
      Ada_Abstract_Formal_Subp_Decl =>
        new Text_Type'(To_Text ("AbstractFormalSubpDecl")),
      Ada_Concrete_Formal_Subp_Decl =>
        new Text_Type'(To_Text ("ConcreteFormalSubpDecl")),
      Ada_Subp_Decl             => new Text_Type'(To_Text ("SubpDecl")),
      Ada_Entry_Decl            => new Text_Type'(To_Text ("EntryDecl")),
      Ada_Enum_Literal_Decl     => new Text_Type'(To_Text ("EnumLiteralDecl")),
      Ada_Generic_Subp_Internal =>
        new Text_Type'(To_Text ("GenericSubpInternal")),
      Ada_Expr_Function        => new Text_Type'(To_Text ("ExprFunction")),
      Ada_Null_Subp_Decl       => new Text_Type'(To_Text ("NullSubpDecl")),
      Ada_Subp_Body            => new Text_Type'(To_Text ("SubpBody")),
      Ada_Subp_Renaming_Decl   => new Text_Type'(To_Text ("SubpRenamingDecl")),
      Ada_Package_Body_Stub    => new Text_Type'(To_Text ("PackageBodyStub")),
      Ada_Protected_Body_Stub => new Text_Type'(To_Text ("ProtectedBodyStub")),
      Ada_Subp_Body_Stub       => new Text_Type'(To_Text ("SubpBodyStub")),
      Ada_Task_Body_Stub       => new Text_Type'(To_Text ("TaskBodyStub")),
      Ada_Entry_Body           => new Text_Type'(To_Text ("EntryBody")),
      Ada_Package_Body         => new Text_Type'(To_Text ("PackageBody")),
      Ada_Protected_Body       => new Text_Type'(To_Text ("ProtectedBody")),
      Ada_Task_Body            => new Text_Type'(To_Text ("TaskBody")),
      Ada_Entry_Index_Spec     => new Text_Type'(To_Text ("EntryIndexSpec")),
      Ada_Error_Decl           => new Text_Type'(To_Text ("ErrorDecl")),
      Ada_Exception_Decl       => new Text_Type'(To_Text ("ExceptionDecl")),
      Ada_Exception_Handler    => new Text_Type'(To_Text ("ExceptionHandler")),
      Ada_For_Loop_Var_Decl    => new Text_Type'(To_Text ("ForLoopVarDecl")),
      Ada_Generic_Package_Decl =>
        new Text_Type'(To_Text ("GenericPackageDecl")),
      Ada_Generic_Subp_Decl => new Text_Type'(To_Text ("GenericSubpDecl")),
      Ada_Generic_Package_Instantiation =>
        new Text_Type'(To_Text ("GenericPackageInstantiation")),
      Ada_Generic_Subp_Instantiation =>
        new Text_Type'(To_Text ("GenericSubpInstantiation")),
      Ada_Generic_Package_Renaming_Decl =>
        new Text_Type'(To_Text ("GenericPackageRenamingDecl")),
      Ada_Generic_Subp_Renaming_Decl =>
        new Text_Type'(To_Text ("GenericSubpRenamingDecl")),
      Ada_Label_Decl            => new Text_Type'(To_Text ("LabelDecl")),
      Ada_Named_Stmt_Decl       => new Text_Type'(To_Text ("NamedStmtDecl")),
      Ada_Number_Decl           => new Text_Type'(To_Text ("NumberDecl")),
      Ada_Object_Decl           => new Text_Type'(To_Text ("ObjectDecl")),
      Ada_Anonymous_Object_Decl =>
        new Text_Type'(To_Text ("AnonymousObjectDecl")),
      Ada_Extended_Return_Stmt_Object_Decl =>
        new Text_Type'(To_Text ("ExtendedReturnStmtObjectDecl")),
      Ada_Package_Renaming_Decl =>
        new Text_Type'(To_Text ("PackageRenamingDecl")),
      Ada_Single_Protected_Decl =>
        new Text_Type'(To_Text ("SingleProtectedDecl")),
      Ada_Single_Task_Decl      => new Text_Type'(To_Text ("SingleTaskDecl")),
      Ada_Case_Stmt_Alternative =>
        new Text_Type'(To_Text ("CaseStmtAlternative")),
      Ada_Compilation_Unit => new Text_Type'(To_Text ("CompilationUnit")),
      Ada_Component_Clause => new Text_Type'(To_Text ("ComponentClause")),
      Ada_Component_Def           => new Text_Type'(To_Text ("ComponentDef")),
      Ada_Constant_Absent => new Text_Type'(To_Text ("ConstantAbsent")),
      Ada_Constant_Present => new Text_Type'(To_Text ("ConstantPresent")),
      Ada_Delta_Constraint => new Text_Type'(To_Text ("DeltaConstraint")),
      Ada_Digits_Constraint => new Text_Type'(To_Text ("DigitsConstraint")),
      Ada_Discriminant_Constraint =>
        new Text_Type'(To_Text ("DiscriminantConstraint")),
      Ada_Index_Constraint      => new Text_Type'(To_Text ("IndexConstraint")),
      Ada_Range_Constraint      => new Text_Type'(To_Text ("RangeConstraint")),
      Ada_Declarative_Part      => new Text_Type'(To_Text ("DeclarativePart")),
      Ada_Private_Part          => new Text_Type'(To_Text ("PrivatePart")),
      Ada_Public_Part           => new Text_Type'(To_Text ("PublicPart")),
      Ada_Elsif_Expr_Part       => new Text_Type'(To_Text ("ElsifExprPart")),
      Ada_Elsif_Stmt_Part       => new Text_Type'(To_Text ("ElsifStmtPart")),
      Ada_Allocator             => new Text_Type'(To_Text ("Allocator")),
      Ada_Aggregate             => new Text_Type'(To_Text ("Aggregate")),
      Ada_Null_Record_Aggregate =>
        new Text_Type'(To_Text ("NullRecordAggregate")),
      Ada_Bin_Op                => new Text_Type'(To_Text ("BinOp")),
      Ada_Relation_Op           => new Text_Type'(To_Text ("RelationOp")),
      Ada_Box_Expr              => new Text_Type'(To_Text ("BoxExpr")),
      Ada_Case_Expr             => new Text_Type'(To_Text ("CaseExpr")),
      Ada_Case_Expr_Alternative =>
        new Text_Type'(To_Text ("CaseExprAlternative")),
      Ada_Contract_Cases       => new Text_Type'(To_Text ("ContractCases")),
      Ada_If_Expr              => new Text_Type'(To_Text ("IfExpr")),
      Ada_Membership_Expr      => new Text_Type'(To_Text ("MembershipExpr")),
      Ada_Attribute_Ref        => new Text_Type'(To_Text ("AttributeRef")),
      Ada_Update_Attribute_Ref =>
        new Text_Type'(To_Text ("UpdateAttributeRef")),
      Ada_Call_Expr             => new Text_Type'(To_Text ("CallExpr")),
      Ada_Defining_Name         => new Text_Type'(To_Text ("DefiningName")),
      Ada_Discrete_Subtype_Name =>
        new Text_Type'(To_Text ("DiscreteSubtypeName")),
      Ada_Dotted_Name            => new Text_Type'(To_Text ("DottedName")),
      Ada_End_Name               => new Text_Type'(To_Text ("EndName")),
      Ada_Explicit_Deref         => new Text_Type'(To_Text ("ExplicitDeref")),
      Ada_Qual_Expr              => new Text_Type'(To_Text ("QualExpr")),
      Ada_Char_Literal           => new Text_Type'(To_Text ("CharLiteral")),
      Ada_Identifier             => new Text_Type'(To_Text ("Identifier")),
      Ada_Op_Abs                 => new Text_Type'(To_Text ("OpAbs")),
      Ada_Op_And                 => new Text_Type'(To_Text ("OpAnd")),
      Ada_Op_And_Then            => new Text_Type'(To_Text ("OpAndThen")),
      Ada_Op_Concat              => new Text_Type'(To_Text ("OpConcat")),
      Ada_Op_Div                 => new Text_Type'(To_Text ("OpDiv")),
      Ada_Op_Double_Dot          => new Text_Type'(To_Text ("OpDoubleDot")),
      Ada_Op_Eq                  => new Text_Type'(To_Text ("OpEq")),
      Ada_Op_Gt                  => new Text_Type'(To_Text ("OpGt")),
      Ada_Op_Gte                 => new Text_Type'(To_Text ("OpGte")),
      Ada_Op_In                  => new Text_Type'(To_Text ("OpIn")),
      Ada_Op_Lt                  => new Text_Type'(To_Text ("OpLt")),
      Ada_Op_Lte                 => new Text_Type'(To_Text ("OpLte")),
      Ada_Op_Minus               => new Text_Type'(To_Text ("OpMinus")),
      Ada_Op_Mod                 => new Text_Type'(To_Text ("OpMod")),
      Ada_Op_Mult                => new Text_Type'(To_Text ("OpMult")),
      Ada_Op_Neq                 => new Text_Type'(To_Text ("OpNeq")),
      Ada_Op_Not                 => new Text_Type'(To_Text ("OpNot")),
      Ada_Op_Not_In              => new Text_Type'(To_Text ("OpNotIn")),
      Ada_Op_Or                  => new Text_Type'(To_Text ("OpOr")),
      Ada_Op_Or_Else             => new Text_Type'(To_Text ("OpOrElse")),
      Ada_Op_Plus                => new Text_Type'(To_Text ("OpPlus")),
      Ada_Op_Pow                 => new Text_Type'(To_Text ("OpPow")),
      Ada_Op_Rem                 => new Text_Type'(To_Text ("OpRem")),
      Ada_Op_Xor                 => new Text_Type'(To_Text ("OpXor")),
      Ada_String_Literal         => new Text_Type'(To_Text ("StringLiteral")),
      Ada_Null_Literal           => new Text_Type'(To_Text ("NullLiteral")),
      Ada_Int_Literal            => new Text_Type'(To_Text ("IntLiteral")),
      Ada_Real_Literal           => new Text_Type'(To_Text ("RealLiteral")),
      Ada_Target_Name            => new Text_Type'(To_Text ("TargetName")),
      Ada_Paren_Expr             => new Text_Type'(To_Text ("ParenExpr")),
      Ada_Quantified_Expr        => new Text_Type'(To_Text ("QuantifiedExpr")),
      Ada_Raise_Expr             => new Text_Type'(To_Text ("RaiseExpr")),
      Ada_Un_Op                  => new Text_Type'(To_Text ("UnOp")),
      Ada_Handled_Stmts          => new Text_Type'(To_Text ("HandledStmts")),
      Ada_Interface_Kind_Limited =>
        new Text_Type'(To_Text ("InterfaceKindLimited")),
      Ada_Interface_Kind_Protected =>
        new Text_Type'(To_Text ("InterfaceKindProtected")),
      Ada_Interface_Kind_Synchronized =>
        new Text_Type'(To_Text ("InterfaceKindSynchronized")),
      Ada_Interface_Kind_Task => new Text_Type'(To_Text ("InterfaceKindTask")),
      Ada_Iter_Type_In              => new Text_Type'(To_Text ("IterTypeIn")),
      Ada_Iter_Type_Of              => new Text_Type'(To_Text ("IterTypeOf")),
      Ada_Library_Item              => new Text_Type'(To_Text ("LibraryItem")),
      Ada_Limited_Absent => new Text_Type'(To_Text ("LimitedAbsent")),
      Ada_Limited_Present => new Text_Type'(To_Text ("LimitedPresent")),
      Ada_For_Loop_Spec             => new Text_Type'(To_Text ("ForLoopSpec")),
      Ada_While_Loop_Spec => new Text_Type'(To_Text ("WhileLoopSpec")),
      Ada_Mode_Default              => new Text_Type'(To_Text ("ModeDefault")),
      Ada_Mode_In                   => new Text_Type'(To_Text ("ModeIn")),
      Ada_Mode_In_Out               => new Text_Type'(To_Text ("ModeInOut")),
      Ada_Mode_Out                  => new Text_Type'(To_Text ("ModeOut")),
      Ada_Not_Null_Absent => new Text_Type'(To_Text ("NotNullAbsent")),
      Ada_Not_Null_Present => new Text_Type'(To_Text ("NotNullPresent")),
      Ada_Null_Component_Decl => new Text_Type'(To_Text ("NullComponentDecl")),
      Ada_Others_Designator => new Text_Type'(To_Text ("OthersDesignator")),
      Ada_Overriding_Not_Overriding =>
        new Text_Type'(To_Text ("OverridingNotOverriding")),
      Ada_Overriding_Overriding =>
        new Text_Type'(To_Text ("OverridingOverriding")),
      Ada_Overriding_Unspecified =>
        new Text_Type'(To_Text ("OverridingUnspecified")),
      Ada_Params                    => new Text_Type'(To_Text ("Params")),
      Ada_Pragma_Node               => new Text_Type'(To_Text ("PragmaNode")),
      Ada_Prim_Type_Accessor => new Text_Type'(To_Text ("PrimTypeAccessor")),
      Ada_Private_Absent => new Text_Type'(To_Text ("PrivateAbsent")),
      Ada_Private_Present => new Text_Type'(To_Text ("PrivatePresent")),
      Ada_Protected_Def => new Text_Type'(To_Text ("ProtectedDef")),
      Ada_Protected_Absent => new Text_Type'(To_Text ("ProtectedAbsent")),
      Ada_Protected_Present => new Text_Type'(To_Text ("ProtectedPresent")),
      Ada_Quantifier_All => new Text_Type'(To_Text ("QuantifierAll")),
      Ada_Quantifier_Some => new Text_Type'(To_Text ("QuantifierSome")),
      Ada_Range_Spec                => new Text_Type'(To_Text ("RangeSpec")),
      Ada_Renaming_Clause => new Text_Type'(To_Text ("RenamingClause")),
      Ada_Synthetic_Renaming_Clause =>
        new Text_Type'(To_Text ("SyntheticRenamingClause")),
      Ada_Reverse_Absent         => new Text_Type'(To_Text ("ReverseAbsent")),
      Ada_Reverse_Present        => new Text_Type'(To_Text ("ReversePresent")),
      Ada_Select_When_Part       => new Text_Type'(To_Text ("SelectWhenPart")),
      Ada_Accept_Stmt            => new Text_Type'(To_Text ("AcceptStmt")),
      Ada_Accept_Stmt_With_Stmts =>
        new Text_Type'(To_Text ("AcceptStmtWithStmts")),
      Ada_For_Loop_Stmt        => new Text_Type'(To_Text ("ForLoopStmt")),
      Ada_Loop_Stmt            => new Text_Type'(To_Text ("LoopStmt")),
      Ada_While_Loop_Stmt      => new Text_Type'(To_Text ("WhileLoopStmt")),
      Ada_Begin_Block          => new Text_Type'(To_Text ("BeginBlock")),
      Ada_Decl_Block           => new Text_Type'(To_Text ("DeclBlock")),
      Ada_Case_Stmt            => new Text_Type'(To_Text ("CaseStmt")),
      Ada_Extended_Return_Stmt =>
        new Text_Type'(To_Text ("ExtendedReturnStmt")),
      Ada_If_Stmt               => new Text_Type'(To_Text ("IfStmt")),
      Ada_Named_Stmt            => new Text_Type'(To_Text ("NamedStmt")),
      Ada_Select_Stmt           => new Text_Type'(To_Text ("SelectStmt")),
      Ada_Error_Stmt            => new Text_Type'(To_Text ("ErrorStmt")),
      Ada_Abort_Stmt            => new Text_Type'(To_Text ("AbortStmt")),
      Ada_Assign_Stmt           => new Text_Type'(To_Text ("AssignStmt")),
      Ada_Call_Stmt             => new Text_Type'(To_Text ("CallStmt")),
      Ada_Delay_Stmt            => new Text_Type'(To_Text ("DelayStmt")),
      Ada_Exit_Stmt             => new Text_Type'(To_Text ("ExitStmt")),
      Ada_Goto_Stmt             => new Text_Type'(To_Text ("GotoStmt")),
      Ada_Label                 => new Text_Type'(To_Text ("Label")),
      Ada_Null_Stmt             => new Text_Type'(To_Text ("NullStmt")),
      Ada_Raise_Stmt            => new Text_Type'(To_Text ("RaiseStmt")),
      Ada_Requeue_Stmt          => new Text_Type'(To_Text ("RequeueStmt")),
      Ada_Return_Stmt           => new Text_Type'(To_Text ("ReturnStmt")),
      Ada_Terminate_Alternative =>
        new Text_Type'(To_Text ("TerminateAlternative")),
      Ada_Subp_Kind_Function  => new Text_Type'(To_Text ("SubpKindFunction")),
      Ada_Subp_Kind_Procedure => new Text_Type'(To_Text ("SubpKindProcedure")),
      Ada_Subunit             => new Text_Type'(To_Text ("Subunit")),
      Ada_Synchronized_Absent =>
        new Text_Type'(To_Text ("SynchronizedAbsent")),
      Ada_Synchronized_Present =>
        new Text_Type'(To_Text ("SynchronizedPresent")),
      Ada_Tagged_Absent => new Text_Type'(To_Text ("TaggedAbsent")),
      Ada_Tagged_Present => new Text_Type'(To_Text ("TaggedPresent")),
      Ada_Task_Def                  => new Text_Type'(To_Text ("TaskDef")),
      Ada_Access_To_Subp_Def => new Text_Type'(To_Text ("AccessToSubpDef")),
      Ada_Anonymous_Type_Access_Def =>
        new Text_Type'(To_Text ("AnonymousTypeAccessDef")),
      Ada_Type_Access_Def => new Text_Type'(To_Text ("TypeAccessDef")),
      Ada_Array_Type_Def           => new Text_Type'(To_Text ("ArrayTypeDef")),
      Ada_Derived_Type_Def => new Text_Type'(To_Text ("DerivedTypeDef")),
      Ada_Enum_Type_Def            => new Text_Type'(To_Text ("EnumTypeDef")),
      Ada_Formal_Discrete_Type_Def =>
        new Text_Type'(To_Text ("FormalDiscreteTypeDef")),
      Ada_Interface_Type_Def => new Text_Type'(To_Text ("InterfaceTypeDef")),
      Ada_Mod_Int_Type_Def        => new Text_Type'(To_Text ("ModIntTypeDef")),
      Ada_Private_Type_Def => new Text_Type'(To_Text ("PrivateTypeDef")),
      Ada_Decimal_Fixed_Point_Def =>
        new Text_Type'(To_Text ("DecimalFixedPointDef")),
      Ada_Floating_Point_Def => new Text_Type'(To_Text ("FloatingPointDef")),
      Ada_Ordinary_Fixed_Point_Def =>
        new Text_Type'(To_Text ("OrdinaryFixedPointDef")),
      Ada_Record_Type_Def => new Text_Type'(To_Text ("RecordTypeDef")),
      Ada_Signed_Int_Type_Def => new Text_Type'(To_Text ("SignedIntTypeDef")),
      Ada_Anonymous_Type => new Text_Type'(To_Text ("AnonymousType")),
      Ada_Enum_Lit_Synth_Type_Expr =>
        new Text_Type'(To_Text ("EnumLitSynthTypeExpr")),
      Ada_Subtype_Indication => new Text_Type'(To_Text ("SubtypeIndication")),
      Ada_Constrained_Subtype_Indication =>
        new Text_Type'(To_Text ("ConstrainedSubtypeIndication")),
      Ada_Discrete_Subtype_Indication =>
        new Text_Type'(To_Text ("DiscreteSubtypeIndication")),
      Ada_Unconstrained_Array_Index =>
        new Text_Type'(To_Text ("UnconstrainedArrayIndex")),
      Ada_Until_Absent         => new Text_Type'(To_Text ("UntilAbsent")),
      Ada_Until_Present        => new Text_Type'(To_Text ("UntilPresent")),
      Ada_Use_Package_Clause   => new Text_Type'(To_Text ("UsePackageClause")),
      Ada_Use_Type_Clause      => new Text_Type'(To_Text ("UseTypeClause")),
      Ada_Variant              => new Text_Type'(To_Text ("Variant")),
      Ada_Variant_Part         => new Text_Type'(To_Text ("VariantPart")),
      Ada_With_Clause          => new Text_Type'(To_Text ("WithClause")),
      Ada_With_Private_Absent => new Text_Type'(To_Text ("WithPrivateAbsent")),
      Ada_With_Private_Present =>
        new Text_Type'(To_Text ("WithPrivatePresent")));

   function ada_node_kind
     (Node : ada_base_entity_Ptr) return ada_node_kind_enum
   is
   begin
      Clear_Last_Exception;

      declare
         K : constant Ada_Node_Kind_Type := Node.Node.Kind;
      begin
         return ada_node_kind_enum (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ada_node_kind_enum'First;
   end ada_node_kind;

   procedure ada_kind_name
     (Kind : ada_node_kind_enum; Result : access ada_text)
   is
   begin
      Clear_Last_Exception;

      declare
         K : constant Ada_Node_Kind_Type := Ada_Node_Kind_Type'Enum_Val (Kind);
         Name : Text_Access renames Node_Kind_Names (K);
      begin
         Result.all :=
           (Chars        => Name.all'Address, Length => Name'Length,
            Is_Allocated => 0);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_kind_name;

   function ada_node_unit (Node : ada_base_entity_Ptr) return ada_analysis_unit
   is
   begin
      Clear_Last_Exception;
      return Node.Node.Unit;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_node_unit;

   function ada_is_token_node (Node : ada_base_entity_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Token_Node (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_is_token_node;

   function ada_is_synthetic (Node : ada_base_entity_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Synthetic (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_is_synthetic;

   procedure ada_node_short_image
     (Node : ada_base_entity_Ptr; Result : access ada_text)
   is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Short_Text_Image (Node.Node);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_node_short_image;

   procedure ada_node_text (Node : ada_base_entity_Ptr; Text : access ada_text)
   is
   begin
      Clear_Last_Exception;
      Text.all := Wrap_Alloc (Implementation.Text (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_node_text;

   procedure ada_node_sloc_range
     (Node         : ada_base_entity_Ptr;
      Sloc_Range_P : access ada_source_location_range)
   is
   begin
      Clear_Last_Exception;

      Sloc_Range_P.all := Wrap (Sloc_Range (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_node_sloc_range;

   procedure ada_lookup_in_node
     (Node   : ada_base_entity_Ptr; Sloc : ada_source_location;
      Result : ada_base_entity_Ptr)
   is
   begin
      Clear_Last_Exception;

      declare
         S : constant Source_Location := Unwrap (Sloc);
      begin
         Result.all := (Lookup (Node.Node, S), Node.Info);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_lookup_in_node;

   function ada_node_children_count
     (Node : ada_base_entity_Ptr) return unsigned
   is
   begin
      Clear_Last_Exception;
      return unsigned (Children_Count (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_node_children_count;

   function ada_node_child
     (Node : ada_base_entity_Ptr; N : unsigned; Child_P : ada_base_entity_Ptr)
      return int
   is
   begin
      Clear_Last_Exception;

      declare
         Result : Bare_Ada_Node;
         Exists : Boolean;
      begin
         if N > unsigned (Natural'Last) then
            return 0;
         end if;
         Get_Child (Node.Node, Natural (N) + 1, Exists, Result);
         if Exists then
            Child_P.all := (Result, Node.Info);
            return 1;
         else
            return 0;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_node_child;

   function ada_text_to_locale_string (Text : ada_text) return System.Address
   is
   begin
      Clear_Last_Exception;

      declare
         use GNATCOLL.Iconv;

         Input_Byte_Size : constant size_t := 4 * Text.Length;

         Output_Byte_Size : constant size_t := Input_Byte_Size + 1;
         --  Assuming no encoding will take more than 4 bytes per character,
         --  4 times the size of the input text plus one null byte should be
         --  enough to hold the result. This is a development helper anyway,
         --  so we don't have performance concerns.

         Result : constant System.Address :=
           System.Memory.Alloc (System.Memory.size_t (Output_Byte_Size));
         --  Buffer we are going to return to the caller. We use
         --  System.Memory.Alloc so that users can call C's "free" function
         --  in order to free it.

         Input : String (1 .. Natural (Input_Byte_Size));
         for Input'Address use Text.Chars;

         Output : String (1 .. Natural (Output_Byte_Size));
         for Output'Address use Result;

         State                     : Iconv_T;
         Input_Index, Output_Index : Positive := 1;
         Status                    : Iconv_Result;

         From_Code : constant String :=
           (if System."=" (System.Default_Bit_Order, System.Low_Order_First)
            then UTF32LE
            else UTF32BE);

      begin
         --  GNATCOLL.Iconv raises Constraint_Error exceptions for empty
         --  strings, so handle them ourselves.

         if Input_Byte_Size = 0 then
            Output (1) := ASCII.NUL;
         end if;

         --  Encode to the locale. Don't bother with error checking...

         Set_Locale;
         State :=
           Iconv_Open
             (To_Code         => Locale, From_Code => From_Code,
              Transliteration => True, Ignore => True);
         Iconv (State, Input, Input_Index, Output, Output_Index, Status);
         Iconv_Close (State);

         --  Don't forget the trailing NULL character to keep C programs happy
         Output (Output_Index) := ASCII.NUL;

         return Result;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return System.Null_Address;
   end ada_text_to_locale_string;

   ----------
   -- Wrap --
   ----------

   function Wrap (S : Unbounded_Wide_Wide_String) return ada_text is
      Chars  : Big_Wide_Wide_String_Access;
      Length : Natural;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      return (Chars.all'Address, size_t (Length), 0);
   end Wrap;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception (Exc : Exception_Occurrence) is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new ada_exception;

         --  If it is not the first time, free memory allocated for the last
         --  exception.

      elsif Last_Exception.Information /= Null_Ptr then
         Free (Last_Exception.Information);
      end if;

      --  Get the kind corresponding to Exc

      declare
         Id : constant Exception_Id := Exception_Identity (Exc);
      begin
         if Id = Invalid_Field'Identity then
            Last_Exception.Kind        := Exception_Invalid_Field;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Node_Data_Evaluation_Error'Identity then
            Last_Exception.Kind        := Exception_Node_Data_Evaluation_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Invalid_Input'Identity then
            Last_Exception.Kind        := Exception_Invalid_Input;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Invalid_Symbol_Error'Identity then
            Last_Exception.Kind        := Exception_Invalid_Symbol_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Invalid_Unit_Name_Error'Identity then
            Last_Exception.Kind        := Exception_Invalid_Unit_Name_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Native_Exception'Identity then
            Last_Exception.Kind        := Exception_Native_Exception;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Precondition_Failure'Identity then
            Last_Exception.Kind        := Exception_Precondition_Failure;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Property_Error'Identity then
            Last_Exception.Kind        := Exception_Property_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Template_Args_Error'Identity then
            Last_Exception.Kind        := Exception_Template_Args_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Template_Format_Error'Identity then
            Last_Exception.Kind        := Exception_Template_Format_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Template_Instantiation_Error'Identity then
            Last_Exception.Kind := Exception_Template_Instantiation_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Stale_Reference_Error'Identity then
            Last_Exception.Kind        := Exception_Stale_Reference_Error;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         elsif Id = Unknown_Charset'Identity then
            Last_Exception.Kind        := Exception_Unknown_Charset;
            Last_Exception.Information := New_String (Exception_Message (Exc));
         else
            Last_Exception.Kind        := Exception_Native_Exception;
            Last_Exception.Information :=
              New_String (Exception_Information (Exc));
         end if;
      end;

   end Set_Last_Exception;

   --------------------------
   -- Clear_Last_Exception --
   --------------------------

   procedure Clear_Last_Exception is
   begin
      if Last_Exception /= null then
         Free (Last_Exception.Information);
      end if;
   end Clear_Last_Exception;

   function ada_get_last_exception return ada_exception_Ptr is
   begin
      if Last_Exception = null or else Last_Exception.Information = Null_Ptr
      then
         return null;
      else
         return Last_Exception;
      end if;
   end ada_get_last_exception;

   function ada_token_kind_name (Kind : int) return chars_ptr is
      K : Token_Kind;
   begin
      begin
         K := Token_Kind'Enum_Val (Kind);
      exception
         when Exc : Constraint_Error =>
            Set_Last_Exception (Exc);
            return Null_Ptr;
      end;

      return New_String (Token_Kind_Name (K));
   end ada_token_kind_name;

   procedure ada_token_next (Token : ada_token; Next_Token : access ada_token)
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         NT : constant Token_Reference := Next (T);
      begin
         Next_Token.all := Wrap (NT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_token_next;

   procedure ada_token_previous
     (Token : ada_token; Previous_Token : access ada_token)
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         PT : constant Token_Reference := Previous (T);
      begin
         Previous_Token.all := Wrap (PT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_token_previous;

   function ada_token_range_text
     (First, Last : ada_token; Text : access ada_text) return int
   is
   begin
      Clear_Last_Exception;
      declare
         FD : constant Token_Data_Type := Data (Unwrap (First));
         LD : constant Token_Data_Type := Data (Unwrap (Last));

         First_Source_Buffer, Last_Source_Buffer : Text_Cst_Access;
         First_Index, Ignored_First              : Positive;
         Last_Index, Ignored_Last                : Natural;
      begin
         Extract_Token_Text
           (FD, First_Source_Buffer, First_Index, Ignored_Last);
         Extract_Token_Text
           (LD, Last_Source_Buffer, Ignored_First, Last_Index);
         if First_Source_Buffer /= Last_Source_Buffer then
            return 0;
         end if;
         Text.all := Wrap (First_Source_Buffer, First_Index, Last_Index);
         return 1;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_token_range_text;

   function ada_token_is_equivalent
     (Left : ada_token; Right : ada_token) return ada_bool
   is
   begin
      Clear_Last_Exception;
      declare
         L : constant Token_Reference := Unwrap (Left);
         R : constant Token_Reference := Unwrap (Right);
      begin
         return ada_bool (Boolean'Pos (Is_Equivalent (L, R)));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_token_is_equivalent;

   procedure ada_entity_image
     (Ent : ada_base_entity_Ptr; Result : access ada_text)
   is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Text_Image (Ent.all);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_entity_image;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Text_Type) return ada_text is
      T : Text_Access := new Text_Type'(S);
   begin
      return ada_text'(T.all'Address, T.all'Length, Is_Allocated => 1);
   end Wrap_Alloc;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (S : Text_Cst_Access; First : Positive; Last : Natural) return ada_text
   is
      Substring : Text_Type renames S (First .. Last);
   begin
      return
        (if First > Last then
           (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
         else (Chars     => S (First)'Address, Length => Substring'Length,
            Is_Allocated => 0));
   end Wrap;

   procedure ada_destroy_text (T : access ada_text) is
   begin
      Clear_Last_Exception;
      declare
         use System;
      begin
         if T.Is_Allocated /= 0 and then T.Chars /= System.Null_Address then
            declare
               TT : Text_Type (1 .. Natural (T.Length));
               for TT'Address use T.Chars;
               TA : Text_Access := TT'Unrestricted_Access;
            begin
               Free (TA);
            end;
            T.Chars := System.Null_Address;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_destroy_text;

   procedure ada_symbol_text
     (Symbol : access ada_symbol_type; Text : access ada_text)
   is
   begin
      Clear_Last_Exception;
      declare
         Sym    : constant Symbol_Type := Unwrap_Symbol (Symbol.all);
         Result : constant Text_Type   :=
           (if Sym = null then "" else Image (Sym));
      begin
         Text.all := Wrap_Alloc (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_symbol_text;

   function ada_create_big_integer
     (Text : access ada_text) return ada_big_integer
   is
   begin
      Clear_Last_Exception;
      declare
         T : Text_Type (1 .. Natural (Text.Length)) with
            Import,
            Address => Text.Chars;
         Image  : constant String           := Langkit_Support.Text.Image (T);
         Result : constant Big_Integer_Type := Create_Big_Integer (Image);
      begin
         return Wrap_Big_Integer (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ada_big_integer (System.Null_Address);
   end ada_create_big_integer;

   procedure ada_big_integer_text
     (Bigint : ada_big_integer; Text : access ada_text)
   is
   begin
      Clear_Last_Exception;
      declare
         BI    : constant Big_Integer_Type := Unwrap_Big_Integer (Bigint);
         Image : constant String           := BI.Value.Image;
      begin
         Text.all := Wrap_Alloc (To_Text (Image));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_big_integer_text;

   procedure ada_big_integer_decref (Bigint : ada_big_integer) is
   begin
      Clear_Last_Exception;
      declare
         BI : Big_Integer_Type := Unwrap_Big_Integer (Bigint);
      begin
         Dec_Ref (BI);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_big_integer_decref;

   function ada_create_unit_provider
     (Data : System.Address; Destroy_Func : ada_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : ada_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : ada_unit_provider_get_unit_from_name_callback)
      return ada_unit_provider
   is
   begin
      Clear_Last_Exception;
      declare
         Result : constant C_Unit_Provider_Access :=
           new C_Unit_Provider'
             (Ada.Finalization.Limited_Controlled with Ref_Count => 1,
              Data => Data, Destroy_Func => Destroy_Func,
              Get_Unit_Filename_Func => Get_Unit_Filename_Func,
              Get_Unit_From_Name_Func => Get_Unit_From_Name_Func);
      begin
         return Wrap_Private_Provider (Internal_Unit_Provider_Access (Result));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ada_unit_provider (System.Null_Address);
   end ada_create_unit_provider;

   procedure ada_dec_ref_unit_provider (Provider : ada_unit_provider) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Unit_Provider_Access :=
           Unwrap_Private_Provider (Provider);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_dec_ref_unit_provider;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Provider : in out C_Unit_Provider) is
   begin
      Provider.Destroy_Func (Provider.Data);
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Provider : in out C_Unit_Provider) is
   begin
      Provider.Ref_Count := Provider.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref
     (Provider : in out C_Unit_Provider) return Boolean
   is
   begin
      Provider.Ref_Count := Provider.Ref_Count - 1;
      if Provider.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : C_Unit_Provider; Name : Text_Type; Kind : Analysis_Unit_Kind)
      return String
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;

      C_Result : chars_ptr :=
        Provider.Get_Unit_Filename_Func
          (Provider.Data, Wrap (Name_Access), Kind);
   begin
      if C_Result = Null_Ptr then
         raise Property_Error with "invalid AST node for unit name";
      else
         declare
            Result : constant String := Value (C_Result);
         begin
            Free (C_Result);
            return Result;
         end;
      end if;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider : C_Unit_Provider; Context : Internal_Context; Name : Text_Type;
      Kind     : Analysis_Unit_Kind; Charset : String := "";
      Reparse  : Boolean := False) return Internal_Unit
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;
      C_Charset   : chars_ptr                :=
        (if Charset'Length = 0 then Null_Ptr else New_String (Charset));
   begin
      return
        C_Result : constant ada_analysis_unit :=
          Provider.Get_Unit_From_Name_Func
            (Provider.Data, Context, Wrap (Name_Access), Kind, C_Charset,
             Boolean'Pos (Reparse))
      do
         Free (C_Charset);
         if C_Result = null then
            raise Property_Error with "invalid AST node for unit name";
         end if;
      end return;
   end Get_Unit;

   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Reference) return ada_token is
      function Convert is new Ada.Unchecked_Conversion
        (Token_Data_Handler_Access, System.Address);
   begin
      if Token = No_Token then
         return (Token_Data => System.Null_Address, Token_Index => -1,
            Trivia_Index    => -1, others => <>);
      end if;

      declare
         D : constant Token_Data_Type := Data (Token);
         K : constant Token_Kind      := Kind (D);

         Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);

         Source_Buffer : Text_Cst_Access;
         First         : Positive;
         Last          : Natural;
      begin
         Extract_Token_Text (D, Source_Buffer, First, Last);
         return (Token_Data => Convert (Get_Token_TDH (Token)),
            Token_Index     => int (Index.Token),
            Trivia_Index    => int (Index.Trivia), Kind => K'Enum_Rep,
            Text            => Wrap (Source_Buffer, First, Last),
            Sloc_Range      => Wrap (Sloc_Range (D)));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : ada_token) return Token_Reference is
      use System;

      --  The following unchecked conversion makes it possible to restore
      --  the Ada type of token data handler accesses from the C API. All
      --  read/writes for the pointed values are made in Ada through values of
      --  the same access type. Thus, strict aliasing issues should not arise
      --  for these.
      --
      --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
      --       Optimization-and-Strict-Aliasing.html>.

      pragma Warnings (Off, "possible aliasing problem for type");
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Token_Data_Handler_Access);
      pragma Warnings (On, "possible aliasing problem for type");

   begin
      return
        (if Token.Token_Data = Null_Address then No_Token
         else Wrap_Token_Reference
             (Convert (Token.Token_Data),
              (Token  => Token_Index (Token.Token_Index),
               Trivia => Token_Index (Token.Trivia_Index))));
   end Unwrap;

   function ada_bare_ada_node_array_create
     (Length : int) return Bare_Ada_Node_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Bare_Ada_Node_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_bare_ada_node_array_create;

   procedure ada_bare_ada_node_array_inc_ref (A : Bare_Ada_Node_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_bare_ada_node_array_inc_ref;

   procedure ada_bare_ada_node_array_dec_ref (A : Bare_Ada_Node_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Bare_Ada_Node_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_bare_ada_node_array_dec_ref;

   function ada_ada_node_array_create
     (Length : int) return Internal_Entity_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Entity_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_ada_node_array_create;

   procedure ada_ada_node_array_inc_ref (A : Internal_Entity_Array_Access) is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_ada_node_array_inc_ref;

   procedure ada_ada_node_array_dec_ref (A : Internal_Entity_Array_Access) is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Entity_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_ada_node_array_dec_ref;

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   function ada_ada_node_p_declarative_scope
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Bare_Declarative_Part;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Declarative_Scope
             (Unwrapped_Node);

         Value_P.all := (Result, Node.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_declarative_scope;

   function ada_ada_node_p_complete
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_completion_item_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Completion_Item_Array_Access;
      begin
         Result :=
           Libadalang.Implementation.Dispatcher_Ada_Node_P_Complete
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_complete;

   function ada_ada_node_p_valid_keywords
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Symbol_Type_Array_Access;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Valid_Keywords
             (Unwrapped_Node);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_valid_keywords;

   function ada_ada_node_p_generic_instantiations
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity_Generic_Instantiation_Array_Access;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Generic_Instantiations
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := Convert (Result);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_generic_instantiations;

   function ada_ada_node_p_semantic_parent
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Semantic_Parent
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_semantic_parent;

   function ada_ada_node_p_parent_basic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity_Basic_Decl;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Parent_Basic_Decl
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_parent_basic_decl;

   function ada_ada_node_p_filter_is_imported_by
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Transitive :
ada_bool;
Value_P : access ada_analysis_unit_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Units : constant Internal_Unit_Array_Access := Units;

      Unwrapped_Transitive : constant Boolean := Transitive /= 0;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Unit_Array_Access;
      begin
         Result :=
           Libadalang.Implementation.Extensions
             .Ada_Node_P_Filter_Is_Imported_By
             (Unwrapped_Node, Units => Unwrapped_Units,
              Transitive            => Unwrapped_Transitive);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_filter_is_imported_by;

   function ada_ada_node_p_xref_entry_point
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Boolean;
      begin
         Result :=
           Libadalang.Implementation.Dispatcher_Ada_Node_P_Xref_Entry_Point
             (Unwrapped_Node);

         Value_P.all := ada_bool (Boolean'Pos (Result));

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_xref_entry_point;

   function ada_ada_node_p_resolve_names
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Boolean;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Resolve_Names
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := ada_bool (Boolean'Pos (Result));

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_resolve_names;

   function ada_ada_node_p_standard_unit
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_analysis_unit)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Unit;
      begin
         Result :=
           Libadalang.Implementation.Extensions.Ada_Node_P_Standard_Unit
             (Unwrapped_Node);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_standard_unit;

   function ada_ada_node_p_std_entity
     (Node : ada_base_entity_Ptr;
Sym        : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Sym : constant Symbol_Type := Unwrap_Symbol (Sym.all);
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Std_Entity
             (Unwrapped_Node, Sym => Unwrapped_Sym);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_std_entity;

   function ada_ada_node_p_bool_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Bool_Type (Unwrapped_Node);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_bool_type;

   function ada_ada_node_p_int_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Int_Type (Unwrapped_Node);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_int_type;

   function ada_ada_node_p_universal_int_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Universal_Int_Type
             (Unwrapped_Node);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_universal_int_type;

   function ada_ada_node_p_universal_real_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Universal_Real_Type
             (Unwrapped_Node);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_universal_real_type;

   function ada_ada_node_p_top_level_decl
     (Node : ada_base_entity_Ptr;
Unit :
ada_analysis_unit;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Unit : constant Internal_Unit := Unit;
   begin
      Clear_Last_Exception;

      declare

         Result : Bare_Basic_Decl;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Top_Level_Decl
             (Unwrapped_Node, Unit => Unwrapped_Unit);

         Value_P.all := (Result, Node.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_top_level_decl;

   function ada_ada_node_p_choice_match
     (Node : ada_base_entity_Ptr;
Value      : access constant ada_big_integer;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Value : constant Big_Integer_Type :=
        Unwrap_Big_Integer (Value.all);
   begin
      Clear_Last_Exception;

      declare

         Result : Boolean;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Choice_Match
             (Unwrapped_Node, Value => Unwrapped_Value, E_Info => Node.Info);

         Value_P.all := ada_bool (Boolean'Pos (Result));

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_choice_match;

   function ada_ada_node_p_gnat_xref
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity_Defining_Name;
      begin
         Result :=
           Libadalang.Implementation.Ada_Node_P_Gnat_Xref
             (Unwrapped_Node,
              Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
              E_Info             => Node.Info);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_p_gnat_xref;

   function ada_ada_node_parent
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Parent
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_parent;

   function ada_ada_node_parents
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity_Array_Access;
      begin
         Result :=
           Libadalang.Implementation.Parents
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_parents;

   function ada_ada_node_children
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity_Array_Access;
      begin
         Result :=
           Libadalang.Implementation.Children
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_children;

   function ada_ada_node_token_start
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_token) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Token_Reference;
      begin
         Result := Libadalang.Implementation.Token_Start (Unwrapped_Node);

         Value_P.all := Wrap (Result);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_token_start;

   function ada_ada_node_token_end
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_token) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Token_Reference;
      begin
         Result := Libadalang.Implementation.Token_End (Unwrapped_Node);

         Value_P.all := Wrap (Result);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_token_end;

   function ada_ada_node_child_index
     (Node : ada_base_entity_Ptr;
Value_P    : access int) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Integer;
      begin
         Result := Libadalang.Implementation.Child_Index (Unwrapped_Node);

         Value_P.all := int (Result);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_child_index;

   function ada_ada_node_previous_sibling
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Previous_Sibling
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_previous_sibling;

   function ada_ada_node_next_sibling
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Entity;
      begin
         Result :=
           Libadalang.Implementation.Next_Sibling
             (Unwrapped_Node, E_Info => Node.Info);

         Value_P.all := (Result.Node, Result.Info);

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_next_sibling;

   function ada_ada_node_unit
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_analysis_unit)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Internal_Unit;
      begin
         Result := Libadalang.Implementation.Unit (Unwrapped_Node);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_unit;

   function ada_ada_node_is_ghost
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Boolean;
      begin
         Result := Libadalang.Implementation.Is_Ghost (Unwrapped_Node);

         Value_P.all := ada_bool (Boolean'Pos (Result));

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_is_ghost;

   function ada_ada_node_full_sloc_image
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      declare

         Result : Character_Type_Array_Access;
      begin
         Result := Libadalang.Implementation.Full_Sloc_Image (Unwrapped_Node);

         Value_P.all := Result;

         return 1;
      exception
         when Exc : Property_Error =>
            Set_Last_Exception (Exc);
            return 0;
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ada_node_full_sloc_image;

   function ada_abort_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Abort_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Abort_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_abort_node_p_as_bool;

   function ada_abstract_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Abstract_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Abstract_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_abstract_node_p_as_bool;

   function ada_assoc_list_p_zip_with_params
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_param_actual_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Assoc_List_Range then

         declare

            Result : Internal_Param_Actual_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Assoc_List_P_Zip_With_Params
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_assoc_list_p_zip_with_params;

   function ada_aliased_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Aliased_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Aliased_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_aliased_node_p_as_bool;

   function ada_all_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_All_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_All_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_all_node_p_as_bool;

   function ada_constrained_array_indices_f_list
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Constrained_Array_Indices_Range then

         declare

            Result : Bare_Constraint_List;
         begin
            Result := Constrained_Array_Indices_F_List (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_constrained_array_indices_f_list;

   function ada_unconstrained_array_indices_f_types
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Unconstrained_Array_Indices_Range then

         declare

            Result : Bare_Unconstrained_Array_Index_List;
         begin
            Result := Unconstrained_Array_Indices_F_Types (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_unconstrained_array_indices_f_types;

   function ada_aspect_assoc_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Aspect_Assoc_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Aspect_Assoc_F_Id (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_aspect_assoc_f_id;

   function ada_aspect_assoc_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Aspect_Assoc_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Aspect_Assoc_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_aspect_assoc_f_expr;

   function ada_at_clause_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_At_Clause_Range then

         declare

            Result : Bare_Base_Id;
         begin
            Result := At_Clause_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_at_clause_f_name;

   function ada_at_clause_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_At_Clause_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := At_Clause_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_at_clause_f_expr;

   function ada_attribute_def_clause_f_attribute_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Attribute_Def_Clause_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Attribute_Def_Clause_F_Attribute_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_attribute_def_clause_f_attribute_expr;

   function ada_attribute_def_clause_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Attribute_Def_Clause_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Attribute_Def_Clause_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_attribute_def_clause_f_expr;

   function ada_enum_rep_clause_f_type_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Enum_Rep_Clause_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Enum_Rep_Clause_F_Type_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_enum_rep_clause_f_type_name;

   function ada_enum_rep_clause_f_aggregate
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Enum_Rep_Clause_Range then

         declare

            Result : Bare_Base_Aggregate;
         begin
            Result := Enum_Rep_Clause_F_Aggregate (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_enum_rep_clause_f_aggregate;

   function ada_record_rep_clause_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Rep_Clause_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Record_Rep_Clause_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_rep_clause_f_name;

   function ada_record_rep_clause_f_at_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Rep_Clause_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Record_Rep_Clause_F_At_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_rep_clause_f_at_expr;

   function ada_record_rep_clause_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Rep_Clause_Range then

         declare

            Result : Bare_Ada_Node_List;
         begin
            Result := Record_Rep_Clause_F_Components (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_rep_clause_f_components;

   function ada_aspect_spec_f_aspect_assocs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Aspect_Spec_Range then

         declare

            Result : Bare_Aspect_Assoc_List;
         begin
            Result := Aspect_Spec_F_Aspect_Assocs (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_aspect_spec_f_aspect_assocs;

   function ada_base_assoc_p_assoc_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Assoc then

         declare

            Result : Internal_Entity_Expr;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Assoc_P_Assoc_Expr
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_assoc_p_assoc_expr;

   function ada_contract_case_assoc_f_guard
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Contract_Case_Assoc_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Contract_Case_Assoc_F_Guard (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_contract_case_assoc_f_guard;

   function ada_contract_case_assoc_f_consequence
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Contract_Case_Assoc_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Contract_Case_Assoc_F_Consequence (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_contract_case_assoc_f_consequence;

   function ada_pragma_argument_assoc_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Pragma_Argument_Assoc_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Pragma_Argument_Assoc_F_Id (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_pragma_argument_assoc_f_id;

   function ada_pragma_argument_assoc_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Pragma_Argument_Assoc_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Pragma_Argument_Assoc_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_pragma_argument_assoc_f_expr;

   function ada_base_formal_param_holder_p_abstract_formal_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Formal_Param_Holder then

         declare

            Result : Internal_Entity_Base_Formal_Param_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Formal_Param_Holder_P_Abstract_Formal_Params
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_formal_param_holder_p_abstract_formal_params;

   function ada_base_formal_param_holder_p_nb_min_params
     (Node : ada_base_entity_Ptr;
Value_P    : access int) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Formal_Param_Holder then

         declare

            Result : Integer;
         begin
            Result :=
              Libadalang.Implementation
                .Base_Formal_Param_Holder_P_Nb_Min_Params
                (Unwrapped_Node);

            Value_P.all := int (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_formal_param_holder_p_nb_min_params;

   function ada_base_formal_param_holder_p_nb_max_params
     (Node : ada_base_entity_Ptr;
Value_P    : access int) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Formal_Param_Holder then

         declare

            Result : Integer;
         begin
            Result :=
              Libadalang.Implementation
                .Base_Formal_Param_Holder_P_Nb_Max_Params
                (Unwrapped_Node);

            Value_P.all := int (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_formal_param_holder_p_nb_max_params;

   function ada_base_formal_param_holder_p_param_types
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Formal_Param_Holder then

         declare

            Result : Internal_Entity_Base_Type_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Base_Formal_Param_Holder_P_Param_Types
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_formal_param_holder_p_param_types;

   function ada_base_subp_spec_p_returns
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Spec then

         declare

            Result : Internal_Entity_Type_Expr;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Subp_Spec_P_Returns
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_spec_p_returns;

   function ada_base_subp_spec_p_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Spec then

         declare

            Result : Internal_Entity_Param_Spec_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Subp_Spec_P_Params
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_spec_p_params;

   function ada_base_subp_spec_p_primitive_subp_types
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Spec then

         declare

            Result : Internal_Entity_Base_Type_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Base_Subp_Spec_P_Primitive_Subp_Types
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_spec_p_primitive_subp_types;

   function ada_base_subp_spec_p_primitive_subp_first_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Spec then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Base_Subp_Spec_P_Primitive_Subp_First_Type
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_spec_p_primitive_subp_first_type;

   function ada_base_subp_spec_p_primitive_subp_tagged_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Spec then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Base_Subp_Spec_P_Primitive_Subp_Tagged_Type
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_spec_p_primitive_subp_tagged_type;

   function ada_base_subp_spec_p_return_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Spec then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Subp_Spec_P_Return_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_spec_p_return_type;

   function ada_entry_spec_f_entry_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Spec_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Entry_Spec_F_Entry_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_spec_f_entry_name;

   function ada_entry_spec_f_family_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Spec_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Entry_Spec_F_Family_Type (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_spec_f_family_type;

   function ada_entry_spec_f_entry_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Spec_Range then

         declare

            Result : Bare_Params;
         begin
            Result := Entry_Spec_F_Entry_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_spec_f_entry_params;

   function ada_subp_spec_f_subp_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Spec_Range then

         declare

            Result : Bare_Subp_Kind;
         begin
            Result := Subp_Spec_F_Subp_Kind (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_spec_f_subp_kind;

   function ada_subp_spec_f_subp_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Spec_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Subp_Spec_F_Subp_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_spec_f_subp_name;

   function ada_subp_spec_f_subp_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Spec_Range then

         declare

            Result : Bare_Params;
         begin
            Result := Subp_Spec_F_Subp_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_spec_f_subp_params;

   function ada_subp_spec_f_subp_returns
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Spec_Range then

         declare

            Result : Bare_Type_Expr;
         begin
            Result := Subp_Spec_F_Subp_Returns (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_spec_f_subp_returns;

   function ada_component_list_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_List_Range then

         declare

            Result : Bare_Ada_Node_List;
         begin
            Result := Component_List_F_Components (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_list_f_components;

   function ada_component_list_f_variant_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_List_Range then

         declare

            Result : Bare_Variant_Part;
         begin
            Result := Component_List_F_Variant_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_list_f_variant_part;

   function ada_known_discriminant_part_f_discr_specs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Known_Discriminant_Part_Range then

         declare

            Result : Bare_Discriminant_Spec_List;
         begin
            Result := Known_Discriminant_Part_F_Discr_Specs (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_known_discriminant_part_f_discr_specs;

   function ada_entry_completion_formal_params_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Completion_Formal_Params_Range then

         declare

            Result : Bare_Params;
         begin
            Result := Entry_Completion_Formal_Params_F_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_completion_formal_params_f_params;

   function ada_generic_formal_part_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Formal_Part_Range then

         declare

            Result : Bare_Ada_Node_List;
         begin
            Result := Generic_Formal_Part_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_formal_part_f_decls;

   function ada_base_record_def_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Record_Def then

         declare

            Result : Bare_Component_List;
         begin
            Result := Base_Record_Def_F_Components (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_record_def_f_components;

   function ada_basic_assoc_p_get_params
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Assoc then

         declare

            Result : Internal_Entity_Defining_Name_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Assoc_P_Get_Params
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_assoc_p_get_params;

   function ada_aggregate_assoc_f_designators
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Aggregate_Assoc_Range then

         declare

            Result : Bare_Alternatives_List;
         begin
            Result := Aggregate_Assoc_F_Designators (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_aggregate_assoc_f_designators;

   function ada_aggregate_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Aggregate_Assoc_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Aggregate_Assoc_F_R_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_aggregate_assoc_f_r_expr;

   function ada_discriminant_assoc_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discriminant_Assoc_Range then

         declare

            Result : Bare_Discriminant_Choice_List;
         begin
            Result := Discriminant_Assoc_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discriminant_assoc_f_ids;

   function ada_discriminant_assoc_f_discr_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discriminant_Assoc_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Discriminant_Assoc_F_Discr_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discriminant_assoc_f_discr_expr;

   function ada_param_assoc_f_designator
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Assoc_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Param_Assoc_F_Designator (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_assoc_f_designator;

   function ada_param_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Assoc_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Param_Assoc_F_R_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_assoc_f_r_expr;

   function ada_basic_decl_p_is_formal
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Is_Formal
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_is_formal;

   function ada_basic_decl_p_doc_annotations
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_doc_annotation_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Doc_Annotation_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Extensions.Basic_Decl_P_Doc_Annotations
                (Unwrapped_Node);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_doc_annotations;

   function ada_basic_decl_p_doc
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Character_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Extensions.Basic_Decl_P_Doc
                (Unwrapped_Node);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_doc;

   function ada_basic_decl_p_previous_part_for_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Basic_Decl_P_Previous_Part_For_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_previous_part_for_decl;

   function ada_basic_decl_p_canonical_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Canonical_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_canonical_part;

   function ada_basic_decl_p_is_static_decl
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Static_Decl
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_is_static_decl;

   function ada_basic_decl_p_is_imported
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Imported
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_is_imported;

   function ada_basic_decl_f_aspects
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Bare_Aspect_Spec;
         begin
            Result := Basic_Decl_F_Aspects (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_f_aspects;

   function ada_basic_decl_p_get_aspect_assoc
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Name : constant Symbol_Type := Unwrap_Symbol (Name.all);
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Aspect_Assoc;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Basic_Decl_P_Get_Aspect_Assoc
                (Unwrapped_Node, Name => Unwrapped_Name, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_get_aspect_assoc;

   function ada_basic_decl_p_get_aspect_spec_expr
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Name : constant Symbol_Type := Unwrap_Symbol (Name.all);
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Expr;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Get_Aspect_Spec_Expr
                (Unwrapped_Node, Name => Unwrapped_Name, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_get_aspect_spec_expr;

   function ada_basic_decl_p_get_aspect
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_internal_aspect)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Name : constant Symbol_Type := Unwrap_Symbol (Name.all);

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Aspect;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Get_Aspect
                (Unwrapped_Node, Name => Unwrapped_Name,
                 Imprecise_Fallback   => Unwrapped_Imprecise_Fallback,
                 E_Info               => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_get_aspect;

   function ada_basic_decl_p_has_aspect
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Name : constant Symbol_Type := Unwrap_Symbol (Name.all);

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Has_Aspect
                (Unwrapped_Node, Name => Unwrapped_Name,
                 Imprecise_Fallback   => Unwrapped_Imprecise_Fallback,
                 E_Info               => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_has_aspect;

   function ada_basic_decl_p_get_pragma
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Name : constant Symbol_Type := Unwrap_Symbol (Name.all);
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Pragma_Node;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Get_Pragma
                (Unwrapped_Node, Name => Unwrapped_Name, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_get_pragma;

   function ada_basic_decl_p_get_representation_clause
     (Node : ada_base_entity_Ptr;
Name       : access constant ada_symbol_type;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_base_entity)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Name : constant Symbol_Type := Unwrap_Symbol (Name.all);

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Attribute_Def_Clause;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Get_Representation_Clause
                (Unwrapped_Node, Name => Unwrapped_Name,
                 Imprecise_Fallback   => Unwrapped_Imprecise_Fallback,
                 E_Info               => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_get_representation_clause;

   function ada_basic_decl_p_is_compilation_unit_root
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Is_Compilation_Unit_Root
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_is_compilation_unit_root;

   function ada_basic_decl_p_is_visible
     (Node : ada_base_entity_Ptr;
From_Node  : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_From_Node : constant Internal_Entity :=
        (if From_Node.all.Node = null then No_Entity
         else (From_Node.all.Node, From_Node.all.Info));
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Is_Visible
                (Unwrapped_Node, From_Node => Unwrapped_From_Node,
                 E_Info                    => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_is_visible;

   function ada_basic_decl_p_base_subp_declarations
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Basic_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Base_Subp_Declarations
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_base_subp_declarations;

   function ada_basic_decl_p_root_subp_declarations
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Basic_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Root_Subp_Declarations
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_root_subp_declarations;

   function ada_basic_decl_p_find_all_overrides
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Units : constant Internal_Unit_Array_Access := Units;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Basic_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Find_All_Overrides
                (Unwrapped_Node, Units => Unwrapped_Units,
                 Imprecise_Fallback    => Unwrapped_Imprecise_Fallback,
                 E_Info                => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_find_all_overrides;

   function ada_basic_decl_p_defining_names
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Defining_Name_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Basic_Decl_P_Defining_Names
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_defining_names;

   function ada_basic_decl_p_defining_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Defining_Name;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Defining_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_defining_name;

   function ada_basic_decl_p_type_expression
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Type_Expr;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Basic_Decl_P_Type_Expression
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_type_expression;

   function ada_basic_decl_p_subp_spec_or_null
     (Node : ada_base_entity_Ptr;
Follow_Generic :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Follow_Generic : constant Boolean := Follow_Generic /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Base_Subp_Spec;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Subp_Spec_Or_Null
                (Unwrapped_Node, Follow_Generic => Unwrapped_Follow_Generic,
                 E_Info                         => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_subp_spec_or_null;

   function ada_basic_decl_p_is_subprogram
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Is_Subprogram
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_is_subprogram;

   function ada_basic_decl_p_relative_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Single_Tok_Node;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Relative_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_relative_name;

   function ada_basic_decl_p_relative_name_text
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_symbol_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Symbol_Type;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Relative_Name_Text
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Wrap_Symbol (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_relative_name_text;

   function ada_basic_decl_p_next_part_for_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Basic_Decl_P_Next_Part_For_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_next_part_for_decl;

   function ada_basic_decl_p_body_part_for_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Internal_Entity_Body_Node;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Body_Part_For_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_body_part_for_decl;

   function ada_basic_decl_p_fully_qualified_name_array
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Symbol_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Fully_Qualified_Name_Array
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_fully_qualified_name_array;

   function ada_basic_decl_p_fully_qualified_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Character_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Fully_Qualified_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_fully_qualified_name;

   function ada_basic_decl_p_canonical_fully_qualified_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Character_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation
                .Basic_Decl_P_Canonical_Fully_Qualified_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_canonical_fully_qualified_name;

   function ada_basic_decl_p_unique_identifying_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Decl then

         declare

            Result : Character_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Basic_Decl_P_Unique_Identifying_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_decl_p_unique_identifying_name;

   function ada_base_formal_param_decl_p_formal_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Formal_Param_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Formal_Param_Decl_P_Formal_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_formal_param_decl_p_formal_type;

   function ada_component_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Decl_Range then

         declare

            Result : Bare_Defining_Name_List;
         begin
            Result := Component_Decl_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_decl_f_ids;

   function ada_component_decl_f_component_def
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Decl_Range then

         declare

            Result : Bare_Component_Def;
         begin
            Result := Component_Decl_F_Component_Def (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_decl_f_component_def;

   function ada_component_decl_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Decl_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Component_Decl_F_Default_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_decl_f_default_expr;

   function ada_discriminant_spec_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discriminant_Spec_Range then

         declare

            Result : Bare_Defining_Name_List;
         begin
            Result := Discriminant_Spec_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discriminant_spec_f_ids;

   function ada_discriminant_spec_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discriminant_Spec_Range then

         declare

            Result : Bare_Type_Expr;
         begin
            Result := Discriminant_Spec_F_Type_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discriminant_spec_f_type_expr;

   function ada_discriminant_spec_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discriminant_Spec_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Discriminant_Spec_F_Default_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discriminant_spec_f_default_expr;

   function ada_generic_formal_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Formal then

         declare

            Result : Bare_Basic_Decl;
         begin
            Result := Generic_Formal_F_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_formal_f_decl;

   function ada_param_spec_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Spec_Range then

         declare

            Result : Bare_Defining_Name_List;
         begin
            Result := Param_Spec_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_spec_f_ids;

   function ada_param_spec_f_has_aliased
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Spec_Range then

         declare

            Result : Bare_Aliased_Node;
         begin
            Result := Param_Spec_F_Has_Aliased (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_spec_f_has_aliased;

   function ada_param_spec_f_mode
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Spec_Range then

         declare

            Result : Bare_Mode;
         begin
            Result := Param_Spec_F_Mode (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_spec_f_mode;

   function ada_param_spec_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Spec_Range then

         declare

            Result : Bare_Type_Expr;
         begin
            Result := Param_Spec_F_Type_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_spec_f_type_expr;

   function ada_param_spec_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Param_Spec_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Param_Spec_F_Default_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_param_spec_f_default_expr;

   function ada_base_package_decl_f_package_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Package_Decl then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Base_Package_Decl_F_Package_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_package_decl_f_package_name;

   function ada_base_package_decl_f_public_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Package_Decl then

         declare

            Result : Bare_Public_Part;
         begin
            Result := Base_Package_Decl_F_Public_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_package_decl_f_public_part;

   function ada_base_package_decl_f_private_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Package_Decl then

         declare

            Result : Bare_Private_Part;
         begin
            Result := Base_Package_Decl_F_Private_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_package_decl_f_private_part;

   function ada_base_package_decl_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Package_Decl then

         declare

            Result : Bare_End_Name;
         begin
            Result := Base_Package_Decl_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_package_decl_f_end_name;

   function ada_base_package_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Package_Decl then

         declare

            Result : Internal_Entity_Package_Body;
         begin
            Result :=
              Libadalang.Implementation.Base_Package_Decl_P_Body_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_package_decl_p_body_part;

   function ada_base_type_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Base_Type_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_f_name;

   function ada_base_type_decl_p_base_subtype
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Base_Subtype
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_base_subtype;

   function ada_base_type_decl_p_private_completion
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Private_Completion
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_private_completion;

   function ada_base_type_decl_p_get_record_representation_clause
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Record_Rep_Clause;
         begin
            Result :=
              Libadalang.Implementation
                .Base_Type_Decl_P_Get_Record_Representation_Clause
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_get_record_representation_clause;

   function ada_base_type_decl_p_get_enum_representation_clause
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Enum_Rep_Clause;
         begin
            Result :=
              Libadalang.Implementation
                .Base_Type_Decl_P_Get_Enum_Representation_Clause
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_get_enum_representation_clause;

   function ada_base_type_decl_p_is_record_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Record_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_record_type;

   function ada_base_type_decl_p_is_array_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Is_Array_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_array_type;

   function ada_base_type_decl_p_find_derived_types
     (Node   : ada_base_entity_Ptr;
Root         : access constant ada_base_entity;
      Origin : access constant ada_base_entity; Imprecise_Fallback :
ada_bool;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Root : constant Internal_Entity :=
        (if Root.all.Node = null then No_Entity
         else (Root.all.Node, Root.all.Info));

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Type_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Find_Derived_Types
                (Unwrapped_Node, Root => Unwrapped_Root,
                 Origin               => Unwrapped_Origin,
                 Imprecise_Fallback   => Unwrapped_Imprecise_Fallback,
                 E_Info               => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_find_derived_types;

   function ada_base_type_decl_p_is_real_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Real_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_real_type;

   function ada_base_type_decl_p_is_float_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Float_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_float_type;

   function ada_base_type_decl_p_is_fixed_point
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Fixed_Point
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_fixed_point;

   function ada_base_type_decl_p_is_enum_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Enum_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_enum_type;

   function ada_base_type_decl_p_is_access_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Access_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_access_type;

   function ada_base_type_decl_p_is_char_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Char_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_char_type;

   function ada_base_type_decl_p_discrete_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_internal_discrete_range)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Discrete_Range;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Discrete_Range
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_discrete_range;

   function ada_base_type_decl_p_is_discrete_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Discrete_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_discrete_type;

   function ada_base_type_decl_p_is_int_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Int_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_int_type;

   function ada_base_type_decl_p_accessed_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Accessed_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_accessed_type;

   function ada_base_type_decl_p_is_tagged_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Is_Tagged_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_tagged_type;

   function ada_base_type_decl_p_base_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Base_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_base_type;

   function ada_base_type_decl_p_base_types
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Base_Types
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_base_types;

   function ada_base_type_decl_p_find_all_derived_types
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Units : constant Internal_Unit_Array_Access := Units;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Type_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Find_All_Derived_Types
                (Unwrapped_Node, Units => Unwrapped_Units,
                 Imprecise_Fallback    => Unwrapped_Imprecise_Fallback,
                 E_Info                => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_find_all_derived_types;

   function ada_base_type_decl_p_comp_type
     (Node : ada_base_entity_Ptr;
Is_Subscript :
ada_bool;
      Origin : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Is_Subscript : constant Boolean := Is_Subscript /= 0;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Comp_Type
                (Unwrapped_Node, Is_Subscript => Unwrapped_Is_Subscript,
                 Origin => Unwrapped_Origin, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_comp_type;

   function ada_base_type_decl_p_index_type
     (Node : ada_base_entity_Ptr;
Dim :
int;
      Origin : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Dim : constant Integer := Integer (Dim);

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Index_Type
                (Unwrapped_Node, Dim => Unwrapped_Dim,
                 Origin              => Unwrapped_Origin, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_index_type;

   function ada_base_type_decl_p_is_derived_type
     (Node   : ada_base_entity_Ptr;
Other_Type   : access constant ada_base_entity;
      Origin : access constant ada_base_entity;
Value_P      : access ada_bool)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Other_Type : constant Internal_Entity_Base_Type_Decl :=
        (if Other_Type.all.Node = null then No_Entity_Base_Type_Decl
         else (Other_Type.all.Node, Other_Type.all.Info));

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Is_Derived_Type
                (Unwrapped_Node, Other_Type => Unwrapped_Other_Type,
                 Origin => Unwrapped_Origin, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_derived_type;

   function ada_base_type_decl_p_is_interface_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Is_Interface_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_interface_type;

   function ada_base_type_decl_p_matching_type
     (Node : ada_base_entity_Ptr;

      Expected_Type : access constant ada_base_entity;
      Origin        : access constant ada_base_entity;
Value_P             : access ada_bool)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Expected_Type : constant Internal_Entity_Base_Type_Decl :=
        (if Expected_Type.all.Node = null then No_Entity_Base_Type_Decl
         else (Expected_Type.all.Node, Expected_Type.all.Info));

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Matching_Type
                (Unwrapped_Node, Expected_Type => Unwrapped_Expected_Type,
                 Origin => Unwrapped_Origin, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_matching_type;

   function ada_base_type_decl_p_canonical_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Canonical_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_canonical_type;

   function ada_base_type_decl_p_previous_part
     (Node : ada_base_entity_Ptr;
Go_To_Incomplete :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Go_To_Incomplete : constant Boolean := Go_To_Incomplete /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Base_Type_Decl_P_Previous_Part
                (Unwrapped_Node,
                 Go_To_Incomplete => Unwrapped_Go_To_Incomplete,
                 E_Info           => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_previous_part;

   function ada_base_type_decl_p_next_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Next_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_next_part;

   function ada_base_type_decl_p_full_view
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Full_View
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_full_view;

   function ada_base_type_decl_p_is_definite_subtype
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Base_Type_Decl_P_Is_Definite_Subtype
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_definite_subtype;

   function ada_base_type_decl_p_is_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Private
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_is_private;

   function ada_base_type_decl_p_root_type
     (Node : ada_base_entity_Ptr;
Origin     : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Type_Decl then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Root_Type
                (Unwrapped_Node, Origin => Unwrapped_Origin,
                 E_Info                 => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_type_decl_p_root_type;

   function ada_subtype_decl_f_subtype
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subtype_Decl_Range then

         declare

            Result : Bare_Subtype_Indication;
         begin
            Result := Subtype_Decl_F_Subtype (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subtype_decl_f_subtype;

   function ada_incomplete_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Incomplete_Type_Decl_Range then

         declare

            Result : Bare_Discriminant_Part;
         begin
            Result := Incomplete_Type_Decl_F_Discriminants (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_incomplete_type_decl_f_discriminants;

   function ada_incomplete_tagged_type_decl_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Incomplete_Tagged_Type_Decl_Range then

         declare

            Result : Bare_Abstract_Node;
         begin
            Result :=
              Incomplete_Tagged_Type_Decl_F_Has_Abstract (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_incomplete_tagged_type_decl_f_has_abstract;

   function ada_protected_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Type_Decl_Range then

         declare

            Result : Bare_Discriminant_Part;
         begin
            Result := Protected_Type_Decl_F_Discriminants (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_type_decl_f_discriminants;

   function ada_protected_type_decl_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Type_Decl_Range then

         declare

            Result : Bare_Parent_List;
         begin
            Result := Protected_Type_Decl_F_Interfaces (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_type_decl_f_interfaces;

   function ada_protected_type_decl_f_definition
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Type_Decl_Range then

         declare

            Result : Bare_Protected_Def;
         begin
            Result := Protected_Type_Decl_F_Definition (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_type_decl_f_definition;

   function ada_task_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Type_Decl_Range then

         declare

            Result : Bare_Discriminant_Part;
         begin
            Result := Task_Type_Decl_F_Discriminants (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_type_decl_f_discriminants;

   function ada_task_type_decl_f_definition
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Type_Decl_Range then

         declare

            Result : Bare_Task_Def;
         begin
            Result := Task_Type_Decl_F_Definition (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_type_decl_f_definition;

   function ada_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Decl_Range then

         declare

            Result : Bare_Discriminant_Part;
         begin
            Result := Type_Decl_F_Discriminants (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_decl_f_discriminants;

   function ada_type_decl_f_type_def
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Decl_Range then

         declare

            Result : Bare_Type_Def;
         begin
            Result := Type_Decl_F_Type_Def (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_decl_f_type_def;

   function ada_type_decl_p_get_primitives
     (Node : ada_base_entity_Ptr;
Only_Inherited :
ada_bool;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Only_Inherited : constant Boolean := Only_Inherited /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Decl_Range then

         declare

            Result : Internal_Entity_Basic_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Type_Decl_P_Get_Primitives
                (Unwrapped_Node, Only_Inherited => Unwrapped_Only_Inherited,
                 E_Info                         => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_decl_p_get_primitives;

   function ada_basic_subp_decl_p_subp_decl_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Subp_Decl then

         declare

            Result : Internal_Entity_Base_Subp_Spec;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Basic_Subp_Decl_P_Subp_Decl_Spec
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_subp_decl_p_subp_decl_spec;

   function ada_basic_subp_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Basic_Subp_Decl then

         declare

            Result : Internal_Entity_Base_Subp_Body;
         begin
            Result :=
              Libadalang.Implementation.Basic_Subp_Decl_P_Body_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_basic_subp_decl_p_body_part;

   function ada_classic_subp_decl_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Classic_Subp_Decl then

         declare

            Result : Bare_Overriding_Node;
         begin
            Result := Classic_Subp_Decl_F_Overriding (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_classic_subp_decl_f_overriding;

   function ada_classic_subp_decl_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Classic_Subp_Decl then

         declare

            Result : Bare_Subp_Spec;
         begin
            Result := Classic_Subp_Decl_F_Subp_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_classic_subp_decl_f_subp_spec;

   function ada_formal_subp_decl_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Formal_Subp_Decl then

         declare

            Result : Bare_Expr;
         begin
            Result := Formal_Subp_Decl_F_Default_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_formal_subp_decl_f_default_expr;

   function ada_entry_decl_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Decl_Range then

         declare

            Result : Bare_Overriding_Node;
         begin
            Result := Entry_Decl_F_Overriding (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_decl_f_overriding;

   function ada_entry_decl_f_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Decl_Range then

         declare

            Result : Bare_Entry_Spec;
         begin
            Result := Entry_Decl_F_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_decl_f_spec;

   function ada_enum_literal_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Enum_Literal_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Enum_Literal_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_enum_literal_decl_f_name;

   function ada_enum_literal_decl_p_enum_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Enum_Literal_Decl_Range then

         declare

            Result : Internal_Entity_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Enum_Literal_Decl_P_Enum_Type
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_enum_literal_decl_p_enum_type;

   function ada_generic_subp_internal_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Internal_Range then

         declare

            Result : Bare_Subp_Spec;
         begin
            Result := Generic_Subp_Internal_F_Subp_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_internal_f_subp_spec;

   function ada_body_node_p_previous_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Body_Node then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Body_Node_P_Previous_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_body_node_p_previous_part;

   function ada_body_node_p_decl_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Body_Node then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Body_Node_P_Decl_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_body_node_p_decl_part;

   function ada_body_node_p_subunit_root
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Body_Node then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Body_Node_P_Subunit_Root
                (Unwrapped_Node);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_body_node_p_subunit_root;

   function ada_base_subp_body_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Body then

         declare

            Result : Bare_Overriding_Node;
         begin
            Result := Base_Subp_Body_F_Overriding (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_body_f_overriding;

   function ada_base_subp_body_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Subp_Body then

         declare

            Result : Bare_Subp_Spec;
         begin
            Result := Base_Subp_Body_F_Subp_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_subp_body_f_subp_spec;

   function ada_expr_function_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr_Function_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Expr_Function_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_function_f_expr;

   function ada_subp_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Body_Range then

         declare

            Result : Bare_Declarative_Part;
         begin
            Result := Subp_Body_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_body_f_decls;

   function ada_subp_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Body_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Subp_Body_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_body_f_stmts;

   function ada_subp_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Body_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Subp_Body_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_body_f_end_name;

   function ada_subp_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Renaming_Decl_Range then

         declare

            Result : Bare_Renaming_Clause;
         begin
            Result := Subp_Renaming_Decl_F_Renames (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_renaming_decl_f_renames;

   function ada_package_body_stub_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Body_Stub_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Package_Body_Stub_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_body_stub_f_name;

   function ada_protected_body_stub_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Body_Stub_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Protected_Body_Stub_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_body_stub_f_name;

   function ada_subp_body_stub_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Body_Stub_Range then

         declare

            Result : Bare_Overriding_Node;
         begin
            Result := Subp_Body_Stub_F_Overriding (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_body_stub_f_overriding;

   function ada_subp_body_stub_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subp_Body_Stub_Range then

         declare

            Result : Bare_Subp_Spec;
         begin
            Result := Subp_Body_Stub_F_Subp_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subp_body_stub_f_subp_spec;

   function ada_task_body_stub_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Body_Stub_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Task_Body_Stub_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_body_stub_f_name;

   function ada_entry_body_f_entry_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Entry_Body_F_Entry_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_entry_name;

   function ada_entry_body_f_index_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_Entry_Index_Spec;
         begin
            Result := Entry_Body_F_Index_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_index_spec;

   function ada_entry_body_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_Entry_Completion_Formal_Params;
         begin
            Result := Entry_Body_F_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_params;

   function ada_entry_body_f_barrier
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Entry_Body_F_Barrier (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_barrier;

   function ada_entry_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_Declarative_Part;
         begin
            Result := Entry_Body_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_decls;

   function ada_entry_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Entry_Body_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_stmts;

   function ada_entry_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Body_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Entry_Body_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_body_f_end_name;

   function ada_package_body_f_package_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Body_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Package_Body_F_Package_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_body_f_package_name;

   function ada_package_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Body_Range then

         declare

            Result : Bare_Declarative_Part;
         begin
            Result := Package_Body_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_body_f_decls;

   function ada_package_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Body_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Package_Body_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_body_f_stmts;

   function ada_package_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Body_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Package_Body_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_body_f_end_name;

   function ada_protected_body_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Body_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Protected_Body_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_body_f_name;

   function ada_protected_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Body_Range then

         declare

            Result : Bare_Declarative_Part;
         begin
            Result := Protected_Body_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_body_f_decls;

   function ada_protected_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Body_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Protected_Body_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_body_f_end_name;

   function ada_task_body_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Body_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Task_Body_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_body_f_name;

   function ada_task_body_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Body_Range then

         declare

            Result : Bare_Declarative_Part;
         begin
            Result := Task_Body_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_body_f_decls;

   function ada_task_body_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Body_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Task_Body_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_body_f_stmts;

   function ada_task_body_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Body_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Task_Body_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_body_f_end_name;

   function ada_entry_index_spec_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Index_Spec_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Entry_Index_Spec_F_Id (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_index_spec_f_id;

   function ada_entry_index_spec_f_subtype
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Entry_Index_Spec_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Entry_Index_Spec_F_Subtype (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_entry_index_spec_f_subtype;

   function ada_exception_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exception_Decl_Range then

         declare

            Result : Bare_Defining_Name_List;
         begin
            Result := Exception_Decl_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exception_decl_f_ids;

   function ada_exception_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exception_Decl_Range then

         declare

            Result : Bare_Renaming_Clause;
         begin
            Result := Exception_Decl_F_Renames (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exception_decl_f_renames;

   function ada_exception_handler_f_exception_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exception_Handler_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Exception_Handler_F_Exception_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exception_handler_f_exception_name;

   function ada_exception_handler_f_handled_exceptions
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exception_Handler_Range then

         declare

            Result : Bare_Alternatives_List;
         begin
            Result := Exception_Handler_F_Handled_Exceptions (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exception_handler_f_handled_exceptions;

   function ada_exception_handler_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exception_Handler_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Exception_Handler_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exception_handler_f_stmts;

   function ada_for_loop_var_decl_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_For_Loop_Var_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := For_Loop_Var_Decl_F_Id (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_for_loop_var_decl_f_id;

   function ada_for_loop_var_decl_f_id_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_For_Loop_Var_Decl_Range then

         declare

            Result : Bare_Subtype_Indication;
         begin
            Result := For_Loop_Var_Decl_F_Id_Type (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_for_loop_var_decl_f_id_type;

   function ada_generic_decl_f_formal_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Decl then

         declare

            Result : Bare_Generic_Formal_Part;
         begin
            Result := Generic_Decl_F_Formal_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_decl_f_formal_part;

   function ada_generic_package_decl_f_package_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Decl_Range then

         declare

            Result : Bare_Generic_Package_Internal;
         begin
            Result := Generic_Package_Decl_F_Package_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_decl_f_package_decl;

   function ada_generic_package_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Decl_Range then

         declare

            Result : Internal_Entity_Package_Body;
         begin
            Result :=
              Libadalang.Implementation.Generic_Package_Decl_P_Body_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_decl_p_body_part;

   function ada_generic_subp_decl_f_subp_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Decl_Range then

         declare

            Result : Bare_Generic_Subp_Internal;
         begin
            Result := Generic_Subp_Decl_F_Subp_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_decl_f_subp_decl;

   function ada_generic_subp_decl_p_body_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Decl_Range then

         declare

            Result : Internal_Entity_Base_Subp_Body;
         begin
            Result :=
              Libadalang.Implementation.Generic_Subp_Decl_P_Body_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_decl_p_body_part;

   function ada_generic_instantiation_p_designated_generic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Instantiation then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Generic_Instantiation_P_Designated_Generic_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_instantiation_p_designated_generic_decl;

   function ada_generic_package_instantiation_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Instantiation_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Generic_Package_Instantiation_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_instantiation_f_name;

   function ada_generic_package_instantiation_f_generic_pkg_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Instantiation_Range then

         declare

            Result : Bare_Name;
         begin
            Result :=
              Generic_Package_Instantiation_F_Generic_Pkg_Name
                (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_instantiation_f_generic_pkg_name;

   function ada_generic_package_instantiation_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Instantiation_Range then

         declare

            Result : Bare_Assoc_List;
         begin
            Result := Generic_Package_Instantiation_F_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_instantiation_f_params;

   function ada_generic_subp_instantiation_f_overriding
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Instantiation_Range then

         declare

            Result : Bare_Overriding_Node;
         begin
            Result := Generic_Subp_Instantiation_F_Overriding (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_instantiation_f_overriding;

   function ada_generic_subp_instantiation_f_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Instantiation_Range then

         declare

            Result : Bare_Subp_Kind;
         begin
            Result := Generic_Subp_Instantiation_F_Kind (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_instantiation_f_kind;

   function ada_generic_subp_instantiation_f_subp_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Instantiation_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Generic_Subp_Instantiation_F_Subp_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_instantiation_f_subp_name;

   function ada_generic_subp_instantiation_f_generic_subp_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Instantiation_Range then

         declare

            Result : Bare_Name;
         begin
            Result :=
              Generic_Subp_Instantiation_F_Generic_Subp_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_instantiation_f_generic_subp_name;

   function ada_generic_subp_instantiation_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Instantiation_Range then

         declare

            Result : Bare_Assoc_List;
         begin
            Result := Generic_Subp_Instantiation_F_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_instantiation_f_params;

   function ada_generic_subp_instantiation_p_designated_subp
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Instantiation_Range then

         declare

            Result : Internal_Entity;
         begin
            Result :=
              Libadalang.Implementation
                .Generic_Subp_Instantiation_P_Designated_Subp
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_instantiation_p_designated_subp;

   function ada_generic_package_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Renaming_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Generic_Package_Renaming_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_renaming_decl_f_name;

   function ada_generic_package_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Package_Renaming_Decl_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Generic_Package_Renaming_Decl_F_Renames (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_package_renaming_decl_f_renames;

   function ada_generic_subp_renaming_decl_f_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Renaming_Decl_Range then

         declare

            Result : Bare_Subp_Kind;
         begin
            Result := Generic_Subp_Renaming_Decl_F_Kind (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_renaming_decl_f_kind;

   function ada_generic_subp_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Renaming_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Generic_Subp_Renaming_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_renaming_decl_f_name;

   function ada_generic_subp_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Generic_Subp_Renaming_Decl_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Generic_Subp_Renaming_Decl_F_Renames (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_generic_subp_renaming_decl_f_renames;

   function ada_label_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Label_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Label_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_label_decl_f_name;

   function ada_named_stmt_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Named_Stmt_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Named_Stmt_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_named_stmt_decl_f_name;

   function ada_number_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Number_Decl_Range then

         declare

            Result : Bare_Defining_Name_List;
         begin
            Result := Number_Decl_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_number_decl_f_ids;

   function ada_number_decl_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Number_Decl_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Number_Decl_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_number_decl_f_expr;

   function ada_object_decl_f_ids
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Defining_Name_List;
         begin
            Result := Object_Decl_F_Ids (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_ids;

   function ada_object_decl_f_has_aliased
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Aliased_Node;
         begin
            Result := Object_Decl_F_Has_Aliased (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_has_aliased;

   function ada_object_decl_f_has_constant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Constant_Node;
         begin
            Result := Object_Decl_F_Has_Constant (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_has_constant;

   function ada_object_decl_f_mode
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Mode;
         begin
            Result := Object_Decl_F_Mode (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_mode;

   function ada_object_decl_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Type_Expr;
         begin
            Result := Object_Decl_F_Type_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_type_expr;

   function ada_object_decl_f_default_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Object_Decl_F_Default_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_default_expr;

   function ada_object_decl_f_renaming_clause
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Bare_Renaming_Clause;
         begin
            Result := Object_Decl_F_Renaming_Clause (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_f_renaming_clause;

   function ada_object_decl_p_public_part_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Object_Decl_Range then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Object_Decl_P_Public_Part_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_object_decl_p_public_part_decl;

   function ada_package_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Renaming_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Package_Renaming_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_renaming_decl_f_name;

   function ada_package_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Renaming_Decl_Range then

         declare

            Result : Bare_Renaming_Clause;
         begin
            Result := Package_Renaming_Decl_F_Renames (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_renaming_decl_f_renames;

   function ada_package_renaming_decl_p_renamed_package
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Renaming_Decl_Range then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Package_Renaming_Decl_P_Renamed_Package
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_renaming_decl_p_renamed_package;

   function ada_package_renaming_decl_p_final_renamed_package
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Package_Renaming_Decl_Range then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Package_Renaming_Decl_P_Final_Renamed_Package
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_package_renaming_decl_p_final_renamed_package;

   function ada_single_protected_decl_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Single_Protected_Decl_Range then

         declare

            Result : Bare_Defining_Name;
         begin
            Result := Single_Protected_Decl_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_single_protected_decl_f_name;

   function ada_single_protected_decl_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Single_Protected_Decl_Range then

         declare

            Result : Bare_Parent_List;
         begin
            Result := Single_Protected_Decl_F_Interfaces (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_single_protected_decl_f_interfaces;

   function ada_single_protected_decl_f_definition
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Single_Protected_Decl_Range then

         declare

            Result : Bare_Protected_Def;
         begin
            Result := Single_Protected_Decl_F_Definition (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_single_protected_decl_f_definition;

   function ada_single_task_decl_f_task_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Single_Task_Decl_Range then

         declare

            Result : Bare_Single_Task_Type_Decl;
         begin
            Result := Single_Task_Decl_F_Task_Type (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_single_task_decl_f_task_type;

   function ada_case_stmt_alternative_f_choices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Stmt_Alternative_Range then

         declare

            Result : Bare_Alternatives_List;
         begin
            Result := Case_Stmt_Alternative_F_Choices (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_stmt_alternative_f_choices;

   function ada_case_stmt_alternative_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Stmt_Alternative_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Case_Stmt_Alternative_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_stmt_alternative_f_stmts;

   function ada_compilation_unit_f_prelude
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Bare_Ada_Node_List;
         begin
            Result := Compilation_Unit_F_Prelude (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_f_prelude;

   function ada_compilation_unit_f_body
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Compilation_Unit_F_Body (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_f_body;

   function ada_compilation_unit_f_pragmas
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Bare_Pragma_Node_List;
         begin
            Result := Compilation_Unit_F_Pragmas (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_f_pragmas;

   function ada_compilation_unit_p_syntactic_fully_qualified_name
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Symbol_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation
                .Compilation_Unit_P_Syntactic_Fully_Qualified_Name
                (Unwrapped_Node);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_syntactic_fully_qualified_name;

   function ada_compilation_unit_p_unit_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_analysis_unit_kind)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Analysis_Unit_Kind;
         begin
            Result :=
              Libadalang.Implementation.Compilation_Unit_P_Unit_Kind
                (Unwrapped_Node);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_unit_kind;

   function ada_compilation_unit_p_withed_units
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Internal_Entity_Compilation_Unit_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Compilation_Unit_P_Withed_Units
                (Unwrapped_Node);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_withed_units;

   function ada_compilation_unit_p_imported_units
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Internal_Entity_Compilation_Unit_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Compilation_Unit_P_Imported_Units
                (Unwrapped_Node);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_imported_units;

   function ada_compilation_unit_p_unit_dependencies
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Internal_Entity_Compilation_Unit_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Compilation_Unit_P_Unit_Dependencies
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_unit_dependencies;

   function ada_compilation_unit_p_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Bare_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Compilation_Unit_P_Decl
                (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_decl;

   function ada_compilation_unit_p_is_preelaborable
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Compilation_Unit_Range then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Compilation_Unit_P_Is_Preelaborable
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_compilation_unit_p_is_preelaborable;

   function ada_component_clause_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Clause_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Component_Clause_F_Id (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_clause_f_id;

   function ada_component_clause_f_position
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Clause_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Component_Clause_F_Position (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_clause_f_position;

   function ada_component_clause_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Clause_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Component_Clause_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_clause_f_range;

   function ada_component_def_f_has_aliased
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Def_Range then

         declare

            Result : Bare_Aliased_Node;
         begin
            Result := Component_Def_F_Has_Aliased (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_def_f_has_aliased;

   function ada_component_def_f_has_constant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Def_Range then

         declare

            Result : Bare_Constant_Node;
         begin
            Result := Component_Def_F_Has_Constant (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_def_f_has_constant;

   function ada_component_def_f_type_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Component_Def_Range then

         declare

            Result : Bare_Type_Expr;
         begin
            Result := Component_Def_F_Type_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_component_def_f_type_expr;

   function ada_constant_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Constant_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Constant_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_constant_node_p_as_bool;

   function ada_delta_constraint_f_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Delta_Constraint_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Delta_Constraint_F_Digits (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_delta_constraint_f_digits;

   function ada_delta_constraint_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Delta_Constraint_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Delta_Constraint_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_delta_constraint_f_range;

   function ada_digits_constraint_f_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Digits_Constraint_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Digits_Constraint_F_Digits (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_digits_constraint_f_digits;

   function ada_digits_constraint_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Digits_Constraint_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Digits_Constraint_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_digits_constraint_f_range;

   function ada_discriminant_constraint_f_constraints
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discriminant_Constraint_Range then

         declare

            Result : Bare_Assoc_List;
         begin
            Result := Discriminant_Constraint_F_Constraints (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discriminant_constraint_f_constraints;

   function ada_index_constraint_f_constraints
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Index_Constraint_Range then

         declare

            Result : Bare_Constraint_List;
         begin
            Result := Index_Constraint_F_Constraints (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_index_constraint_f_constraints;

   function ada_range_constraint_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Range_Constraint_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Range_Constraint_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_range_constraint_f_range;

   function ada_declarative_part_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Declarative_Part_Range then

         declare

            Result : Bare_Ada_Node_List;
         begin
            Result := Declarative_Part_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_declarative_part_f_decls;

   function ada_elsif_expr_part_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Elsif_Expr_Part_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Elsif_Expr_Part_F_Cond_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_elsif_expr_part_f_cond_expr;

   function ada_elsif_expr_part_f_then_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Elsif_Expr_Part_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Elsif_Expr_Part_F_Then_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_elsif_expr_part_f_then_expr;

   function ada_elsif_stmt_part_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Elsif_Stmt_Part_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Elsif_Stmt_Part_F_Cond_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_elsif_stmt_part_f_cond_expr;

   function ada_elsif_stmt_part_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Elsif_Stmt_Part_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Elsif_Stmt_Part_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_elsif_stmt_part_f_stmts;

   function ada_expr_p_expression_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Expr_P_Expression_Type
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_p_expression_type;

   function ada_expr_p_is_static_expr
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Expr_P_Is_Static_Expr
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_p_is_static_expr;

   function ada_expr_p_first_corresponding_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation
                .Dispatcher_Expr_P_First_Corresponding_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_p_first_corresponding_decl;

   function ada_expr_p_eval_as_int
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_big_integer) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr then

         declare

            Result : Big_Integer_Type;
         begin
            Result :=
              Libadalang.Implementation.Expr_P_Eval_As_Int
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Wrap_Big_Integer (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_p_eval_as_int;

   function ada_expr_p_eval_as_int_in_env
     (Node : ada_base_entity_Ptr;
Env :
ada_substitution_array;

      Value_P : access ada_big_integer) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Env : constant Internal_Substitution_Array_Access := Env;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr then

         declare

            Result : Big_Integer_Type;
         begin
            Result :=
              Libadalang.Implementation.Extensions.Expr_P_Eval_As_Int_In_Env
                (Unwrapped_Node, Env => Unwrapped_Env, E_Info => Node.Info);

            Value_P.all := Wrap_Big_Integer (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_p_eval_as_int_in_env;

   function ada_expr_p_matching_nodes
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Expr then

         declare

            Result : Internal_Entity_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Expr_P_Matching_Nodes
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_expr_p_matching_nodes;

   function ada_allocator_f_subpool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Allocator_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Allocator_F_Subpool (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_allocator_f_subpool;

   function ada_allocator_f_type_or_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Allocator_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Allocator_F_Type_Or_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_allocator_f_type_or_expr;

   function ada_allocator_p_get_allocated_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Allocator_Range then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Allocator_P_Get_Allocated_Type
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_allocator_p_get_allocated_type;

   function ada_base_aggregate_f_ancestor_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Aggregate then

         declare

            Result : Bare_Expr;
         begin
            Result := Base_Aggregate_F_Ancestor_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_aggregate_f_ancestor_expr;

   function ada_base_aggregate_f_assocs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Aggregate then

         declare

            Result : Bare_Assoc_List;
         begin
            Result := Base_Aggregate_F_Assocs (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_aggregate_f_assocs;

   function ada_bin_op_f_left
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Bin_Op_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Bin_Op_F_Left (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_bin_op_f_left;

   function ada_bin_op_f_op
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Bin_Op_Range then

         declare

            Result : Bare_Op;
         begin
            Result := Bin_Op_F_Op (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_bin_op_f_op;

   function ada_bin_op_f_right
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Bin_Op_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Bin_Op_F_Right (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_bin_op_f_right;

   function ada_case_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Case_Expr_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_expr_f_expr;

   function ada_case_expr_f_cases
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Expr_Range then

         declare

            Result : Bare_Case_Expr_Alternative_List;
         begin
            Result := Case_Expr_F_Cases (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_expr_f_cases;

   function ada_case_expr_alternative_f_choices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Expr_Alternative_Range then

         declare

            Result : Bare_Alternatives_List;
         begin
            Result := Case_Expr_Alternative_F_Choices (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_expr_alternative_f_choices;

   function ada_case_expr_alternative_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Expr_Alternative_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Case_Expr_Alternative_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_expr_alternative_f_expr;

   function ada_contract_cases_f_contract_cases
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Contract_Cases_Range then

         declare

            Result : Bare_Contract_Case_Assoc_List;
         begin
            Result := Contract_Cases_F_Contract_Cases (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_contract_cases_f_contract_cases;

   function ada_if_expr_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Cond_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_expr_f_cond_expr;

   function ada_if_expr_f_then_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Then_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_expr_f_then_expr;

   function ada_if_expr_f_alternatives
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Expr_Range then

         declare

            Result : Bare_Elsif_Expr_Part_List;
         begin
            Result := If_Expr_F_Alternatives (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_expr_f_alternatives;

   function ada_if_expr_f_else_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Else_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_expr_f_else_expr;

   function ada_membership_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Membership_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Membership_Expr_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_membership_expr_f_expr;

   function ada_membership_expr_f_op
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Membership_Expr_Range then

         declare

            Result : Bare_Op;
         begin
            Result := Membership_Expr_F_Op (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_membership_expr_f_op;

   function ada_membership_expr_f_membership_exprs
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Membership_Expr_Range then

         declare

            Result : Bare_Expr_Alternatives_List;
         begin
            Result := Membership_Expr_F_Membership_Exprs (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_membership_expr_f_membership_exprs;

   function ada_name_p_enclosing_defining_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Defining_Name;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Enclosing_Defining_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_enclosing_defining_name;

   function ada_name_p_is_defining
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Defining
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_defining;

   function ada_name_p_name_is
     (Node : ada_base_entity_Ptr;
Sym        : access constant ada_symbol_type;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Sym : constant Symbol_Type := Unwrap_Symbol (Sym.all);
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Name_Is
                (Unwrapped_Node, Sym => Unwrapped_Sym);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_name_is;

   function ada_name_p_is_direct_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Direct_Call
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_direct_call;

   function ada_name_p_is_access_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Access_Call
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_access_call;

   function ada_name_p_is_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Call
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_call;

   function ada_name_p_is_dot_call
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Dot_Call
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_dot_call;

   function ada_name_p_failsafe_referenced_def_name
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_internal_refd_def) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Refd_Def;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Failsafe_Referenced_Def_Name
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_failsafe_referenced_def_name;

   function ada_name_p_referenced_defining_name
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Defining_Name;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Referenced_Defining_Name
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_referenced_defining_name;

   function ada_name_p_all_env_elements
     (Node : ada_base_entity_Ptr;
Seq :
ada_bool;
      Seq_From : access constant ada_base_entity;

      Value_P : access ada_ada_node_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Seq : constant Boolean := Seq /= 0;

      Unwrapped_Seq_From : constant Bare_Ada_Node := Seq_From.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Name_P_All_Env_Elements
                (Unwrapped_Node, Seq => Unwrapped_Seq,
                 Seq_From => Unwrapped_Seq_From, E_Info => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_all_env_elements;

   function ada_name_p_called_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Base_Formal_Param_Holder;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Called_Subp_Spec
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_called_subp_spec;

   function ada_name_p_referenced_decl
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Referenced_Decl
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_referenced_decl;

   function ada_name_p_failsafe_referenced_decl
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_internal_refd_decl) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Refd_Decl;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Failsafe_Referenced_Decl
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_failsafe_referenced_decl;

   function ada_name_p_referenced_decl_internal
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_internal_refd_decl) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Refd_Decl;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Referenced_Decl_Internal
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_referenced_decl_internal;

   function ada_name_p_name_designated_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Name_Designated_Type
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_name_designated_type;

   function ada_name_p_is_static_subtype
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Static_Subtype
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_static_subtype;

   function ada_name_p_name_matches
     (Node : ada_base_entity_Ptr;
N          : access constant ada_base_entity;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_N : constant Internal_Entity_Name :=
        (if N.all.Node = null then No_Entity_Name
         else (N.all.Node, N.all.Info));
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Name_Matches
                (Unwrapped_Node, N => Unwrapped_N);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_name_matches;

   function ada_name_p_relative_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Internal_Entity_Single_Tok_Node;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Name_P_Relative_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_relative_name;

   function ada_name_p_is_operator_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Operator_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_operator_name;

   function ada_name_p_is_write_reference
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Write_Reference
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_write_reference;

   function ada_name_p_is_dispatching_call
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Dispatching_Call
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_dispatching_call;

   function ada_name_p_is_static_call
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Name_P_Is_Static_Call
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_is_static_call;

   function ada_name_p_as_symbol_array
     (Node : ada_base_entity_Ptr;

      Value_P : access ada_unbounded_text_type_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Name then

         declare

            Result : Symbol_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Name_P_As_Symbol_Array
                (Unwrapped_Node);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_name_p_as_symbol_array;

   function ada_attribute_ref_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Attribute_Ref_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Attribute_Ref_F_Prefix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_attribute_ref_f_prefix;

   function ada_attribute_ref_f_attribute
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Attribute_Ref_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Attribute_Ref_F_Attribute (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_attribute_ref_f_attribute;

   function ada_attribute_ref_f_args
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Attribute_Ref_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Attribute_Ref_F_Args (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_attribute_ref_f_args;

   function ada_call_expr_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Call_Expr_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Call_Expr_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_call_expr_f_name;

   function ada_call_expr_f_suffix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Call_Expr_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := Call_Expr_F_Suffix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_call_expr_f_suffix;

   function ada_call_expr_p_is_array_slice
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Call_Expr_Range then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Call_Expr_P_Is_Array_Slice
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_call_expr_p_is_array_slice;

   function ada_defining_name_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Defining_Name_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_f_name;

   function ada_defining_name_p_basic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Basic_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_basic_decl;

   function ada_defining_name_p_find_refs
     (Node   : ada_base_entity_Ptr;
Root         : access constant ada_base_entity;
      Origin : access constant ada_base_entity; Imprecise_Fallback :
ada_bool;

      Value_P : access ada_ref_result_array) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Root : constant Internal_Entity :=
        (if Root.all.Node = null then No_Entity
         else (Root.all.Node, Root.all.Info));

      Unwrapped_Origin : constant Bare_Ada_Node := Origin.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Ref_Result_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Find_Refs
                (Unwrapped_Node, Root => Unwrapped_Root,
                 Origin               => Unwrapped_Origin,
                 Imprecise_Fallback   => Unwrapped_Imprecise_Fallback,
                 E_Info               => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_find_refs;

   function ada_defining_name_p_find_all_references
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ref_result_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Units : constant Internal_Unit_Array_Access := Units;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Ref_Result_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Find_All_References
                (Unwrapped_Node, Units => Unwrapped_Units,
                 Imprecise_Fallback    => Unwrapped_Imprecise_Fallback,
                 E_Info                => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_find_all_references;

   function ada_defining_name_p_find_all_calls
     (Node : ada_base_entity_Ptr;
Units :
ada_analysis_unit_array;
      Imprecise_Fallback :
ada_bool;
Value_P : access ada_ref_result_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Units : constant Internal_Unit_Array_Access := Units;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Ref_Result_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Find_All_Calls
                (Unwrapped_Node, Units => Unwrapped_Units,
                 Imprecise_Fallback    => Unwrapped_Imprecise_Fallback,
                 E_Info                => Node.Info);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_find_all_calls;

   function ada_defining_name_p_next_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Entity_Defining_Name;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Next_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_next_part;

   function ada_defining_name_p_previous_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Entity_Defining_Name;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Previous_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_previous_part;

   function ada_defining_name_p_canonical_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Defining_Name_Range then

         declare

            Result : Internal_Entity_Defining_Name;
         begin
            Result :=
              Libadalang.Implementation.Defining_Name_P_Canonical_Part
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_defining_name_p_canonical_part;

   function ada_discrete_subtype_name_f_subtype
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Discrete_Subtype_Name_Range then

         declare

            Result : Bare_Discrete_Subtype_Indication;
         begin
            Result := Discrete_Subtype_Name_F_Subtype (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_discrete_subtype_name_f_subtype;

   function ada_dotted_name_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Dotted_Name_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Dotted_Name_F_Prefix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_dotted_name_f_prefix;

   function ada_dotted_name_f_suffix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Dotted_Name_Range then

         declare

            Result : Bare_Base_Id;
         begin
            Result := Dotted_Name_F_Suffix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_dotted_name_f_suffix;

   function ada_end_name_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_End_Name_Range then

         declare

            Result : Bare_Name;
         begin
            Result := End_Name_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_end_name_f_name;

   function ada_end_name_p_basic_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_End_Name_Range then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.End_Name_P_Basic_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_end_name_p_basic_decl;

   function ada_explicit_deref_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Explicit_Deref_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Explicit_Deref_F_Prefix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_explicit_deref_f_prefix;

   function ada_qual_expr_f_prefix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Qual_Expr_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Qual_Expr_F_Prefix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_qual_expr_f_prefix;

   function ada_qual_expr_f_suffix
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Qual_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Qual_Expr_F_Suffix (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_qual_expr_f_suffix;

   function ada_single_tok_node_p_canonical_text
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_symbol_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Single_Tok_Node then

         declare

            Result : Symbol_Type;
         begin
            Result :=
              Libadalang.Implementation.Single_Tok_Node_P_Canonical_Text
                (Unwrapped_Node);

            Value_P.all := Wrap_Symbol (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_single_tok_node_p_canonical_text;

   function ada_char_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;
Value_P    : access uint32_t) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Char_Literal_Range then

         declare

            Result : Character_Type;
         begin
            Result :=
              Libadalang.Implementation.Extensions.Char_Literal_P_Denoted_Value
                (Unwrapped_Node);

            Value_P.all := Character_Type'Pos (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_char_literal_p_denoted_value;

   function ada_string_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_text_type) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_String_Literal_Range then

         declare

            Result : Character_Type_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Extensions
                .String_Literal_P_Denoted_Value
                (Unwrapped_Node);

            Value_P.all := Result;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_string_literal_p_denoted_value;

   function ada_int_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_big_integer) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Int_Literal_Range then

         declare

            Result : Big_Integer_Type;
         begin
            Result :=
              Libadalang.Implementation.Extensions.Int_Literal_P_Denoted_Value
                (Unwrapped_Node);

            Value_P.all := Wrap_Big_Integer (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_int_literal_p_denoted_value;

   function ada_paren_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Paren_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Paren_Expr_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_paren_expr_f_expr;

   function ada_quantified_expr_f_quantifier
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Quantified_Expr_Range then

         declare

            Result : Bare_Quantifier;
         begin
            Result := Quantified_Expr_F_Quantifier (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_quantified_expr_f_quantifier;

   function ada_quantified_expr_f_loop_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Quantified_Expr_Range then

         declare

            Result : Bare_For_Loop_Spec;
         begin
            Result := Quantified_Expr_F_Loop_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_quantified_expr_f_loop_spec;

   function ada_quantified_expr_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Quantified_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Quantified_Expr_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_quantified_expr_f_expr;

   function ada_raise_expr_f_exception_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Raise_Expr_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Raise_Expr_F_Exception_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_raise_expr_f_exception_name;

   function ada_raise_expr_f_error_message
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Raise_Expr_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Raise_Expr_F_Error_Message (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_raise_expr_f_error_message;

   function ada_un_op_f_op
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Un_Op_Range then

         declare

            Result : Bare_Op;
         begin
            Result := Un_Op_F_Op (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_un_op_f_op;

   function ada_un_op_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Un_Op_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Un_Op_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_un_op_f_expr;

   function ada_handled_stmts_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Handled_Stmts_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Handled_Stmts_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_handled_stmts_f_stmts;

   function ada_handled_stmts_f_exceptions
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Handled_Stmts_Range then

         declare

            Result : Bare_Ada_Node_List;
         begin
            Result := Handled_Stmts_F_Exceptions (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_handled_stmts_f_exceptions;

   function ada_library_item_f_has_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Library_Item_Range then

         declare

            Result : Bare_Private_Node;
         begin
            Result := Library_Item_F_Has_Private (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_library_item_f_has_private;

   function ada_library_item_f_item
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Library_Item_Range then

         declare

            Result : Bare_Basic_Decl;
         begin
            Result := Library_Item_F_Item (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_library_item_f_item;

   function ada_limited_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Limited_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Limited_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_limited_node_p_as_bool;

   function ada_for_loop_spec_f_var_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_For_Loop_Spec_Range then

         declare

            Result : Bare_For_Loop_Var_Decl;
         begin
            Result := For_Loop_Spec_F_Var_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_for_loop_spec_f_var_decl;

   function ada_for_loop_spec_f_loop_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_For_Loop_Spec_Range then

         declare

            Result : Bare_Iter_Type;
         begin
            Result := For_Loop_Spec_F_Loop_Type (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_for_loop_spec_f_loop_type;

   function ada_for_loop_spec_f_has_reverse
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_For_Loop_Spec_Range then

         declare

            Result : Bare_Reverse_Node;
         begin
            Result := For_Loop_Spec_F_Has_Reverse (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_for_loop_spec_f_has_reverse;

   function ada_for_loop_spec_f_iter_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_For_Loop_Spec_Range then

         declare

            Result : Bare_Ada_Node;
         begin
            Result := For_Loop_Spec_F_Iter_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_for_loop_spec_f_iter_expr;

   function ada_while_loop_spec_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_While_Loop_Spec_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := While_Loop_Spec_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_while_loop_spec_f_expr;

   function ada_not_null_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Not_Null then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Not_Null_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_not_null_p_as_bool;

   function ada_params_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Params_Range then

         declare

            Result : Bare_Param_Spec_List;
         begin
            Result := Params_F_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_params_f_params;

   function ada_pragma_node_f_id
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Pragma_Node_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Pragma_Node_F_Id (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_pragma_node_f_id;

   function ada_pragma_node_f_args
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Pragma_Node_Range then

         declare

            Result : Bare_Base_Assoc_List;
         begin
            Result := Pragma_Node_F_Args (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_pragma_node_f_args;

   function ada_pragma_node_p_associated_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_ada_node_array)
      return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Pragma_Node_Range then

         declare

            Result : Internal_Entity_Basic_Decl_Array_Access;
         begin
            Result :=
              Libadalang.Implementation.Pragma_Node_P_Associated_Decls
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := Convert (Result);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_pragma_node_p_associated_decls;

   function ada_private_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Private_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Private_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_private_node_p_as_bool;

   function ada_protected_def_f_public_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Def_Range then

         declare

            Result : Bare_Public_Part;
         begin
            Result := Protected_Def_F_Public_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_def_f_public_part;

   function ada_protected_def_f_private_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Def_Range then

         declare

            Result : Bare_Private_Part;
         begin
            Result := Protected_Def_F_Private_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_def_f_private_part;

   function ada_protected_def_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Def_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Protected_Def_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_def_f_end_name;

   function ada_protected_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Protected_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Protected_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_protected_node_p_as_bool;

   function ada_range_spec_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Range_Spec_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Range_Spec_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_range_spec_f_range;

   function ada_renaming_clause_f_renamed_object
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Renaming_Clause_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Renaming_Clause_F_Renamed_Object (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_renaming_clause_f_renamed_object;

   function ada_reverse_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Reverse_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Reverse_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_reverse_node_p_as_bool;

   function ada_select_when_part_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Select_When_Part_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Select_When_Part_F_Cond_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_select_when_part_f_cond_expr;

   function ada_select_when_part_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Select_When_Part_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Select_When_Part_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_select_when_part_f_stmts;

   function ada_accept_stmt_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Accept_Stmt_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Accept_Stmt_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_accept_stmt_f_name;

   function ada_accept_stmt_f_entry_index_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Accept_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Accept_Stmt_F_Entry_Index_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_accept_stmt_f_entry_index_expr;

   function ada_accept_stmt_f_params
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Accept_Stmt_Range then

         declare

            Result : Bare_Entry_Completion_Formal_Params;
         begin
            Result := Accept_Stmt_F_Params (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_accept_stmt_f_params;

   function ada_accept_stmt_with_stmts_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Accept_Stmt_With_Stmts_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Accept_Stmt_With_Stmts_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_accept_stmt_with_stmts_f_stmts;

   function ada_accept_stmt_with_stmts_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Accept_Stmt_With_Stmts_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Accept_Stmt_With_Stmts_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_accept_stmt_with_stmts_f_end_name;

   function ada_base_loop_stmt_f_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Loop_Stmt then

         declare

            Result : Bare_Loop_Spec;
         begin
            Result := Base_Loop_Stmt_F_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_loop_stmt_f_spec;

   function ada_base_loop_stmt_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Loop_Stmt then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Base_Loop_Stmt_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_loop_stmt_f_stmts;

   function ada_base_loop_stmt_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Base_Loop_Stmt then

         declare

            Result : Bare_End_Name;
         begin
            Result := Base_Loop_Stmt_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_base_loop_stmt_f_end_name;

   function ada_begin_block_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Begin_Block_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Begin_Block_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_begin_block_f_stmts;

   function ada_begin_block_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Begin_Block_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Begin_Block_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_begin_block_f_end_name;

   function ada_decl_block_f_decls
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Decl_Block_Range then

         declare

            Result : Bare_Declarative_Part;
         begin
            Result := Decl_Block_F_Decls (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_decl_block_f_decls;

   function ada_decl_block_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Decl_Block_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Decl_Block_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_decl_block_f_stmts;

   function ada_decl_block_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Decl_Block_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Decl_Block_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_decl_block_f_end_name;

   function ada_case_stmt_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Case_Stmt_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_stmt_f_expr;

   function ada_case_stmt_f_alternatives
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Case_Stmt_Range then

         declare

            Result : Bare_Case_Stmt_Alternative_List;
         begin
            Result := Case_Stmt_F_Alternatives (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_case_stmt_f_alternatives;

   function ada_extended_return_stmt_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Extended_Return_Stmt_Range then

         declare

            Result : Bare_Extended_Return_Stmt_Object_Decl;
         begin
            Result := Extended_Return_Stmt_F_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_extended_return_stmt_f_decl;

   function ada_extended_return_stmt_f_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Extended_Return_Stmt_Range then

         declare

            Result : Bare_Handled_Stmts;
         begin
            Result := Extended_Return_Stmt_F_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_extended_return_stmt_f_stmts;

   function ada_if_stmt_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := If_Stmt_F_Cond_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_stmt_f_cond_expr;

   function ada_if_stmt_f_then_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Stmt_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := If_Stmt_F_Then_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_stmt_f_then_stmts;

   function ada_if_stmt_f_alternatives
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Stmt_Range then

         declare

            Result : Bare_Elsif_Stmt_Part_List;
         begin
            Result := If_Stmt_F_Alternatives (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_stmt_f_alternatives;

   function ada_if_stmt_f_else_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_If_Stmt_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := If_Stmt_F_Else_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_if_stmt_f_else_stmts;

   function ada_named_stmt_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Named_Stmt_Range then

         declare

            Result : Bare_Named_Stmt_Decl;
         begin
            Result := Named_Stmt_F_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_named_stmt_f_decl;

   function ada_named_stmt_f_stmt
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Named_Stmt_Range then

         declare

            Result : Bare_Composite_Stmt;
         begin
            Result := Named_Stmt_F_Stmt (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_named_stmt_f_stmt;

   function ada_select_stmt_f_guards
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Select_Stmt_Range then

         declare

            Result : Bare_Select_When_Part_List;
         begin
            Result := Select_Stmt_F_Guards (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_select_stmt_f_guards;

   function ada_select_stmt_f_else_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Select_Stmt_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Select_Stmt_F_Else_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_select_stmt_f_else_stmts;

   function ada_select_stmt_f_abort_stmts
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Select_Stmt_Range then

         declare

            Result : Bare_Stmt_List;
         begin
            Result := Select_Stmt_F_Abort_Stmts (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_select_stmt_f_abort_stmts;

   function ada_abort_stmt_f_names
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Abort_Stmt_Range then

         declare

            Result : Bare_Name_List;
         begin
            Result := Abort_Stmt_F_Names (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_abort_stmt_f_names;

   function ada_assign_stmt_f_dest
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Assign_Stmt_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Assign_Stmt_F_Dest (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_assign_stmt_f_dest;

   function ada_assign_stmt_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Assign_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Assign_Stmt_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_assign_stmt_f_expr;

   function ada_call_stmt_f_call
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Call_Stmt_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Call_Stmt_F_Call (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_call_stmt_f_call;

   function ada_delay_stmt_f_has_until
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Delay_Stmt_Range then

         declare

            Result : Bare_Until_Node;
         begin
            Result := Delay_Stmt_F_Has_Until (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_delay_stmt_f_has_until;

   function ada_delay_stmt_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Delay_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Delay_Stmt_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_delay_stmt_f_expr;

   function ada_exit_stmt_f_loop_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exit_Stmt_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Exit_Stmt_F_Loop_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exit_stmt_f_loop_name;

   function ada_exit_stmt_f_cond_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Exit_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Exit_Stmt_F_Cond_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_exit_stmt_f_cond_expr;

   function ada_goto_stmt_f_label_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Goto_Stmt_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Goto_Stmt_F_Label_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_goto_stmt_f_label_name;

   function ada_label_f_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Label_Range then

         declare

            Result : Bare_Label_Decl;
         begin
            Result := Label_F_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_label_f_decl;

   function ada_raise_stmt_f_exception_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Raise_Stmt_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Raise_Stmt_F_Exception_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_raise_stmt_f_exception_name;

   function ada_raise_stmt_f_error_message
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Raise_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Raise_Stmt_F_Error_Message (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_raise_stmt_f_error_message;

   function ada_requeue_stmt_f_call_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Requeue_Stmt_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Requeue_Stmt_F_Call_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_requeue_stmt_f_call_name;

   function ada_requeue_stmt_f_has_abort
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Requeue_Stmt_Range then

         declare

            Result : Bare_Abort_Node;
         begin
            Result := Requeue_Stmt_F_Has_Abort (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_requeue_stmt_f_has_abort;

   function ada_return_stmt_f_return_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Return_Stmt_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Return_Stmt_F_Return_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_return_stmt_f_return_expr;

   function ada_subunit_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subunit_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Subunit_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subunit_f_name;

   function ada_subunit_f_body
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subunit_Range then

         declare

            Result : Bare_Body_Node;
         begin
            Result := Subunit_F_Body (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subunit_f_body;

   function ada_subunit_p_body_root
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subunit_Range then

         declare

            Result : Internal_Entity_Basic_Decl;
         begin
            Result :=
              Libadalang.Implementation.Subunit_P_Body_Root (Unwrapped_Node);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subunit_p_body_root;

   function ada_synchronized_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Synchronized_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Synchronized_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_synchronized_node_p_as_bool;

   function ada_tagged_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Tagged_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Tagged_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_tagged_node_p_as_bool;

   function ada_task_def_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Def_Range then

         declare

            Result : Bare_Parent_List;
         begin
            Result := Task_Def_F_Interfaces (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_def_f_interfaces;

   function ada_task_def_f_public_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Def_Range then

         declare

            Result : Bare_Public_Part;
         begin
            Result := Task_Def_F_Public_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_def_f_public_part;

   function ada_task_def_f_private_part
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Def_Range then

         declare

            Result : Bare_Private_Part;
         begin
            Result := Task_Def_F_Private_Part (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_def_f_private_part;

   function ada_task_def_f_end_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Task_Def_Range then

         declare

            Result : Bare_End_Name;
         begin
            Result := Task_Def_F_End_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_task_def_f_end_name;

   function ada_access_def_f_has_not_null
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Access_Def then

         declare

            Result : Bare_Not_Null;
         begin
            Result := Access_Def_F_Has_Not_Null (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_access_def_f_has_not_null;

   function ada_access_to_subp_def_f_has_protected
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Access_To_Subp_Def_Range then

         declare

            Result : Bare_Protected_Node;
         begin
            Result := Access_To_Subp_Def_F_Has_Protected (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_access_to_subp_def_f_has_protected;

   function ada_access_to_subp_def_f_subp_spec
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Access_To_Subp_Def_Range then

         declare

            Result : Bare_Subp_Spec;
         begin
            Result := Access_To_Subp_Def_F_Subp_Spec (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_access_to_subp_def_f_subp_spec;

   function ada_anonymous_type_access_def_f_type_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Anonymous_Type_Access_Def_Range then

         declare

            Result : Bare_Base_Type_Decl;
         begin
            Result := Anonymous_Type_Access_Def_F_Type_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_anonymous_type_access_def_f_type_decl;

   function ada_type_access_def_f_has_all
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Access_Def_Range then

         declare

            Result : Bare_All_Node;
         begin
            Result := Type_Access_Def_F_Has_All (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_access_def_f_has_all;

   function ada_type_access_def_f_has_constant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Access_Def_Range then

         declare

            Result : Bare_Constant_Node;
         begin
            Result := Type_Access_Def_F_Has_Constant (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_access_def_f_has_constant;

   function ada_type_access_def_f_subtype_indication
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Access_Def_Range then

         declare

            Result : Bare_Subtype_Indication;
         begin
            Result := Type_Access_Def_F_Subtype_Indication (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_access_def_f_subtype_indication;

   function ada_array_type_def_f_indices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Array_Type_Def_Range then

         declare

            Result : Bare_Array_Indices;
         begin
            Result := Array_Type_Def_F_Indices (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_array_type_def_f_indices;

   function ada_array_type_def_f_component_type
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Array_Type_Def_Range then

         declare

            Result : Bare_Component_Def;
         begin
            Result := Array_Type_Def_F_Component_Type (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_array_type_def_f_component_type;

   function ada_derived_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_Abstract_Node;
         begin
            Result := Derived_Type_Def_F_Has_Abstract (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_has_abstract;

   function ada_derived_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_Limited_Node;
         begin
            Result := Derived_Type_Def_F_Has_Limited (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_has_limited;

   function ada_derived_type_def_f_has_synchronized
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_Synchronized_Node;
         begin
            Result := Derived_Type_Def_F_Has_Synchronized (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_has_synchronized;

   function ada_derived_type_def_f_subtype_indication
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_Subtype_Indication;
         begin
            Result := Derived_Type_Def_F_Subtype_Indication (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_subtype_indication;

   function ada_derived_type_def_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_Parent_List;
         begin
            Result := Derived_Type_Def_F_Interfaces (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_interfaces;

   function ada_derived_type_def_f_record_extension
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_Base_Record_Def;
         begin
            Result := Derived_Type_Def_F_Record_Extension (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_record_extension;

   function ada_derived_type_def_f_has_with_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Derived_Type_Def_Range then

         declare

            Result : Bare_With_Private;
         begin
            Result := Derived_Type_Def_F_Has_With_Private (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_derived_type_def_f_has_with_private;

   function ada_enum_type_def_f_enum_literals
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Enum_Type_Def_Range then

         declare

            Result : Bare_Enum_Literal_Decl_List;
         begin
            Result := Enum_Type_Def_F_Enum_Literals (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_enum_type_def_f_enum_literals;

   function ada_interface_type_def_f_interface_kind
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Interface_Type_Def_Range then

         declare

            Result : Bare_Interface_Kind;
         begin
            Result := Interface_Type_Def_F_Interface_Kind (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_interface_type_def_f_interface_kind;

   function ada_interface_type_def_f_interfaces
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Interface_Type_Def_Range then

         declare

            Result : Bare_Parent_List;
         begin
            Result := Interface_Type_Def_F_Interfaces (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_interface_type_def_f_interfaces;

   function ada_mod_int_type_def_f_expr
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Mod_Int_Type_Def_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Mod_Int_Type_Def_F_Expr (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_mod_int_type_def_f_expr;

   function ada_private_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Private_Type_Def_Range then

         declare

            Result : Bare_Abstract_Node;
         begin
            Result := Private_Type_Def_F_Has_Abstract (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_private_type_def_f_has_abstract;

   function ada_private_type_def_f_has_tagged
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Private_Type_Def_Range then

         declare

            Result : Bare_Tagged_Node;
         begin
            Result := Private_Type_Def_F_Has_Tagged (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_private_type_def_f_has_tagged;

   function ada_private_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Private_Type_Def_Range then

         declare

            Result : Bare_Limited_Node;
         begin
            Result := Private_Type_Def_F_Has_Limited (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_private_type_def_f_has_limited;

   function ada_decimal_fixed_point_def_f_delta
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Decimal_Fixed_Point_Def_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Decimal_Fixed_Point_Def_F_Delta (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_decimal_fixed_point_def_f_delta;

   function ada_decimal_fixed_point_def_f_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Decimal_Fixed_Point_Def_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Decimal_Fixed_Point_Def_F_Digits (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_decimal_fixed_point_def_f_digits;

   function ada_decimal_fixed_point_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Decimal_Fixed_Point_Def_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Decimal_Fixed_Point_Def_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_decimal_fixed_point_def_f_range;

   function ada_floating_point_def_f_num_digits
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Floating_Point_Def_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Floating_Point_Def_F_Num_Digits (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_floating_point_def_f_num_digits;

   function ada_floating_point_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Floating_Point_Def_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Floating_Point_Def_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_floating_point_def_f_range;

   function ada_ordinary_fixed_point_def_f_delta
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Ordinary_Fixed_Point_Def_Range then

         declare

            Result : Bare_Expr;
         begin
            Result := Ordinary_Fixed_Point_Def_F_Delta (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ordinary_fixed_point_def_f_delta;

   function ada_ordinary_fixed_point_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Ordinary_Fixed_Point_Def_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Ordinary_Fixed_Point_Def_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_ordinary_fixed_point_def_f_range;

   function ada_record_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Type_Def_Range then

         declare

            Result : Bare_Abstract_Node;
         begin
            Result := Record_Type_Def_F_Has_Abstract (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_type_def_f_has_abstract;

   function ada_record_type_def_f_has_tagged
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Type_Def_Range then

         declare

            Result : Bare_Tagged_Node;
         begin
            Result := Record_Type_Def_F_Has_Tagged (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_type_def_f_has_tagged;

   function ada_record_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Type_Def_Range then

         declare

            Result : Bare_Limited_Node;
         begin
            Result := Record_Type_Def_F_Has_Limited (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_type_def_f_has_limited;

   function ada_record_type_def_f_record_def
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Record_Type_Def_Range then

         declare

            Result : Bare_Base_Record_Def;
         begin
            Result := Record_Type_Def_F_Record_Def (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_record_type_def_f_record_def;

   function ada_signed_int_type_def_f_range
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Signed_Int_Type_Def_Range then

         declare

            Result : Bare_Range_Spec;
         begin
            Result := Signed_Int_Type_Def_F_Range (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_signed_int_type_def_f_range;

   function ada_type_expr_p_type_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Expr then

         declare

            Result : Internal_Entity_Name;
         begin
            Result :=
              Libadalang.Implementation.Type_Expr_P_Type_Name
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_expr_p_type_name;

   function ada_type_expr_p_designated_type_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Expr then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Type_Expr_P_Designated_Type_Decl
                (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_expr_p_designated_type_decl;

   function ada_type_expr_p_designated_type_decl_from
     (Node : ada_base_entity_Ptr;

      Origin_Node : access constant ada_base_entity;

      Value_P : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Origin_Node : constant Internal_Entity :=
        (if Origin_Node.all.Node = null then No_Entity
         else (Origin_Node.all.Node, Origin_Node.all.Info));
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Type_Expr then

         declare

            Result : Internal_Entity_Base_Type_Decl;
         begin
            Result :=
              Libadalang.Implementation.Type_Expr_P_Designated_Type_Decl_From
                (Unwrapped_Node, Origin_Node => Unwrapped_Origin_Node,
                 E_Info                      => Node.Info);

            Value_P.all := (Result.Node, Result.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_type_expr_p_designated_type_decl_from;

   function ada_anonymous_type_f_type_decl
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Anonymous_Type_Range then

         declare

            Result : Bare_Anonymous_Type_Decl;
         begin
            Result := Anonymous_Type_F_Type_Decl (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_anonymous_type_f_type_decl;

   function ada_subtype_indication_f_has_not_null
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subtype_Indication_Range then

         declare

            Result : Bare_Not_Null;
         begin
            Result := Subtype_Indication_F_Has_Not_Null (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subtype_indication_f_has_not_null;

   function ada_subtype_indication_f_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subtype_Indication_Range then

         declare

            Result : Bare_Name;
         begin
            Result := Subtype_Indication_F_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subtype_indication_f_name;

   function ada_subtype_indication_f_constraint
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subtype_Indication_Range then

         declare

            Result : Bare_Constraint;
         begin
            Result := Subtype_Indication_F_Constraint (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subtype_indication_f_constraint;

   function ada_subtype_indication_p_is_static_subtype
     (Node : ada_base_entity_Ptr;
Imprecise_Fallback :
ada_bool;

      Value_P : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;

      Unwrapped_Imprecise_Fallback : constant Boolean :=
        Imprecise_Fallback /= 0;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Subtype_Indication_Range then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Subtype_Indication_P_Is_Static_Subtype
                (Unwrapped_Node,
                 Imprecise_Fallback => Unwrapped_Imprecise_Fallback,
                 E_Info             => Node.Info);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_subtype_indication_p_is_static_subtype;

   function ada_unconstrained_array_index_f_subtype_indication
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Unconstrained_Array_Index_Range then

         declare

            Result : Bare_Subtype_Indication;
         begin
            Result :=
              Unconstrained_Array_Index_F_Subtype_Indication (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_unconstrained_array_index_f_subtype_indication;

   function ada_until_node_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Until_Node then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_Until_Node_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_until_node_p_as_bool;

   function ada_use_package_clause_f_packages
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Use_Package_Clause_Range then

         declare

            Result : Bare_Name_List;
         begin
            Result := Use_Package_Clause_F_Packages (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_use_package_clause_f_packages;

   function ada_use_type_clause_f_has_all
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Use_Type_Clause_Range then

         declare

            Result : Bare_All_Node;
         begin
            Result := Use_Type_Clause_F_Has_All (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_use_type_clause_f_has_all;

   function ada_use_type_clause_f_types
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Use_Type_Clause_Range then

         declare

            Result : Bare_Name_List;
         begin
            Result := Use_Type_Clause_F_Types (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_use_type_clause_f_types;

   function ada_variant_f_choices
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Variant_Range then

         declare

            Result : Bare_Alternatives_List;
         begin
            Result := Variant_F_Choices (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_variant_f_choices;

   function ada_variant_f_components
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Variant_Range then

         declare

            Result : Bare_Component_List;
         begin
            Result := Variant_F_Components (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_variant_f_components;

   function ada_variant_part_f_discr_name
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Variant_Part_Range then

         declare

            Result : Bare_Identifier;
         begin
            Result := Variant_Part_F_Discr_Name (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_variant_part_f_discr_name;

   function ada_variant_part_f_variant
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_Variant_Part_Range then

         declare

            Result : Bare_Variant_List;
         begin
            Result := Variant_Part_F_Variant (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_variant_part_f_variant;

   function ada_with_clause_f_has_limited
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_With_Clause_Range then

         declare

            Result : Bare_Limited_Node;
         begin
            Result := With_Clause_F_Has_Limited (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_with_clause_f_has_limited;

   function ada_with_clause_f_has_private
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_With_Clause_Range then

         declare

            Result : Bare_Private_Node;
         begin
            Result := With_Clause_F_Has_Private (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_with_clause_f_has_private;

   function ada_with_clause_f_packages
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_With_Clause_Range then

         declare

            Result : Bare_Name_List;
         begin
            Result := With_Clause_F_Packages (Unwrapped_Node);

            Value_P.all := (Result, Node.Info);

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_with_clause_f_packages;

   function ada_with_private_p_as_bool
     (Node : ada_base_entity_Ptr;
Value_P    : access ada_bool) return int

   is
      Unwrapped_Node : constant Bare_Ada_Node := Node.Node;
   begin
      Clear_Last_Exception;

      if Unwrapped_Node.Kind in Ada_With_Private then

         declare

            Result : Boolean;
         begin
            Result :=
              Libadalang.Implementation.Dispatcher_With_Private_P_As_Bool
                (Unwrapped_Node);

            Value_P.all := ada_bool (Boolean'Pos (Result));

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ada_with_private_p_as_bool;

   procedure ada_internal_doc_annotation_inc_ref
     (R : ada_internal_doc_annotation_Ptr)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (R.all);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_internal_doc_annotation_inc_ref;

   procedure ada_internal_doc_annotation_dec_ref
     (R : ada_internal_doc_annotation_Ptr)
   is
   begin
      Clear_Last_Exception;
      Dec_Ref (R.all);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_internal_doc_annotation_dec_ref;

   procedure ada_internal_substitution_inc_ref
     (R : ada_internal_substitution_Ptr)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (R.all);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_internal_substitution_inc_ref;

   procedure ada_internal_substitution_dec_ref
     (R : ada_internal_substitution_Ptr)
   is
   begin
      Clear_Last_Exception;
      Dec_Ref (R.all);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_internal_substitution_dec_ref;

   function ada_text_type_create
     (Length : int) return Character_Type_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Character_Type_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_text_type_create;

   procedure ada_text_type_inc_ref (A : Character_Type_Array_Access) is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_text_type_inc_ref;

   procedure ada_text_type_dec_ref (A : Character_Type_Array_Access) is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Character_Type_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_text_type_dec_ref;

   function ada_completion_item_array_create
     (Length : int) return Internal_Completion_Item_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Completion_Item_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_completion_item_array_create;

   procedure ada_completion_item_array_inc_ref
     (A : Internal_Completion_Item_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_completion_item_array_inc_ref;

   procedure ada_completion_item_array_dec_ref
     (A : Internal_Completion_Item_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Completion_Item_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_completion_item_array_dec_ref;

   function ada_doc_annotation_array_create
     (Length : int) return Internal_Doc_Annotation_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Doc_Annotation_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_doc_annotation_array_create;

   procedure ada_doc_annotation_array_inc_ref
     (A : Internal_Doc_Annotation_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_doc_annotation_array_inc_ref;

   procedure ada_doc_annotation_array_dec_ref
     (A : Internal_Doc_Annotation_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Doc_Annotation_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_doc_annotation_array_dec_ref;

   function ada_param_actual_array_create
     (Length : int) return Internal_Param_Actual_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Param_Actual_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_param_actual_array_create;

   procedure ada_param_actual_array_inc_ref
     (A : Internal_Param_Actual_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_param_actual_array_inc_ref;

   procedure ada_param_actual_array_dec_ref
     (A : Internal_Param_Actual_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Param_Actual_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_param_actual_array_dec_ref;

   function ada_ref_result_array_create
     (Length : int) return Internal_Ref_Result_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Ref_Result_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_ref_result_array_create;

   procedure ada_ref_result_array_inc_ref
     (A : Internal_Ref_Result_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_ref_result_array_inc_ref;

   procedure ada_ref_result_array_dec_ref
     (A : Internal_Ref_Result_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Ref_Result_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_ref_result_array_dec_ref;

   function ada_substitution_array_create
     (Length : int) return Internal_Substitution_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Substitution_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_substitution_array_create;

   procedure ada_substitution_array_inc_ref
     (A : Internal_Substitution_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_substitution_array_inc_ref;

   procedure ada_substitution_array_dec_ref
     (A : Internal_Substitution_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Substitution_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_substitution_array_dec_ref;

   function ada_analysis_unit_array_create
     (Length : int) return Internal_Unit_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Internal_Unit_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_analysis_unit_array_create;

   procedure ada_analysis_unit_array_inc_ref (A : Internal_Unit_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_analysis_unit_array_inc_ref;

   procedure ada_analysis_unit_array_dec_ref (A : Internal_Unit_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Internal_Unit_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_analysis_unit_array_dec_ref;

   function ada_unbounded_text_type_array_create
     (Length : int) return Symbol_Type_Array_Access
   is
   begin
      Clear_Last_Exception;
      return Create_Symbol_Type_Array (Natural (Length));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end ada_unbounded_text_type_array_create;

   procedure ada_unbounded_text_type_array_inc_ref
     (A : Symbol_Type_Array_Access)
   is
   begin
      Clear_Last_Exception;
      Inc_Ref (A);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unbounded_text_type_array_inc_ref;

   procedure ada_unbounded_text_type_array_dec_ref
     (A : Symbol_Type_Array_Access)
   is
   begin
      Clear_Last_Exception;
      declare
         A_Var : Symbol_Type_Array_Access := A;
      begin
         Dec_Ref (A_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ada_unbounded_text_type_array_dec_ref;

end Libadalang.Implementation.C;
