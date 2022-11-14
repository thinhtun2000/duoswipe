with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Iconv;

with Libadalang.Lexer_Implementation; use Libadalang.Lexer_Implementation;
with Libadalang.Private_Converters;

with Libadalang.Sources;

package body Libadalang.Common is

   Is_Token_Node_Kind : constant array (Ada_Node_Kind_Type) of Boolean :=
     (Ada_Abort_Absent => False, Ada_Abort_Present => False,
      Ada_Abstract_Absent => False, Ada_Abstract_Present => False,
      Ada_Ada_Node_List => False, Ada_Alternatives_List => False,
      Ada_Constraint_List                  => False, Ada_Decl_List => False,
      Ada_Stmt_List => False, Ada_Aspect_Assoc_List => False,
      Ada_Base_Assoc_List                  => False, Ada_Assoc_List => False,
      Ada_Case_Expr_Alternative_List       => False,
      Ada_Case_Stmt_Alternative_List       => False,
      Ada_Compilation_Unit_List            => False,
      Ada_Contract_Case_Assoc_List => False, Ada_Defining_Name_List => False,
      Ada_Discriminant_Spec_List => False, Ada_Elsif_Expr_Part_List => False,
      Ada_Elsif_Stmt_Part_List => False, Ada_Enum_Literal_Decl_List => False,
      Ada_Expr_Alternatives_List           => False,
      Ada_Discriminant_Choice_List         => False, Ada_Name_List => False,
      Ada_Parent_List => False, Ada_Param_Spec_List => False,
      Ada_Pragma_Node_List => False, Ada_Select_When_Part_List => False,
      Ada_Unconstrained_Array_Index_List   => False, Ada_Variant_List => False,
      Ada_Aliased_Absent => False, Ada_Aliased_Present => False,
      Ada_All_Absent                       => False, Ada_All_Present => False,
      Ada_Constrained_Array_Indices        => False,
      Ada_Unconstrained_Array_Indices      => False, Ada_Aspect_Assoc => False,
      Ada_At_Clause => False, Ada_Attribute_Def_Clause => False,
      Ada_Enum_Rep_Clause => False, Ada_Record_Rep_Clause => False,
      Ada_Aspect_Spec => False, Ada_Contract_Case_Assoc => False,
      Ada_Pragma_Argument_Assoc            => False, Ada_Entry_Spec => False,
      Ada_Enum_Subp_Spec                   => False, Ada_Subp_Spec => False,
      Ada_Component_List => False, Ada_Known_Discriminant_Part => False,
      Ada_Unknown_Discriminant_Part        => False,
      Ada_Entry_Completion_Formal_Params   => False,
      Ada_Generic_Formal_Part => False, Ada_Null_Record_Def => False,
      Ada_Record_Def => False, Ada_Aggregate_Assoc => False,
      Ada_Multi_Dim_Array_Assoc => False, Ada_Discriminant_Assoc => False,
      Ada_Param_Assoc => False, Ada_Component_Decl => False,
      Ada_Discriminant_Spec => False, Ada_Generic_Formal_Obj_Decl => False,
      Ada_Generic_Formal_Package           => False,
      Ada_Generic_Formal_Subp_Decl         => False,
      Ada_Generic_Formal_Type_Decl         => False, Ada_Param_Spec => False,
      Ada_Generic_Package_Internal         => False, Ada_Package_Decl => False,
      Ada_Discrete_Base_Subtype_Decl       => False, Ada_Subtype_Decl => False,
      Ada_Classwide_Type_Decl => False, Ada_Incomplete_Type_Decl => False,
      Ada_Incomplete_Tagged_Type_Decl      => False,
      Ada_Protected_Type_Decl => False, Ada_Task_Type_Decl => False,
      Ada_Single_Task_Type_Decl            => False, Ada_Type_Decl => False,
      Ada_Anonymous_Type_Decl => False, Ada_Synth_Anonymous_Type_Decl => False,
      Ada_Abstract_Subp_Decl => False, Ada_Abstract_Formal_Subp_Decl => False,
      Ada_Concrete_Formal_Subp_Decl        => False, Ada_Subp_Decl => False,
      Ada_Entry_Decl => False, Ada_Enum_Literal_Decl => False,
      Ada_Generic_Subp_Internal => False, Ada_Expr_Function => False,
      Ada_Null_Subp_Decl                   => False, Ada_Subp_Body => False,
      Ada_Subp_Renaming_Decl => False, Ada_Package_Body_Stub => False,
      Ada_Protected_Body_Stub => False, Ada_Subp_Body_Stub => False,
      Ada_Task_Body_Stub                   => False, Ada_Entry_Body => False,
      Ada_Package_Body => False, Ada_Protected_Body => False,
      Ada_Task_Body => False, Ada_Entry_Index_Spec => False,
      Ada_Error_Decl => False, Ada_Exception_Decl => False,
      Ada_Exception_Handler => False, Ada_For_Loop_Var_Decl => False,
      Ada_Generic_Package_Decl => False, Ada_Generic_Subp_Decl => False,
      Ada_Generic_Package_Instantiation    => False,
      Ada_Generic_Subp_Instantiation       => False,
      Ada_Generic_Package_Renaming_Decl    => False,
      Ada_Generic_Subp_Renaming_Decl       => False, Ada_Label_Decl => False,
      Ada_Named_Stmt_Decl                  => False, Ada_Number_Decl => False,
      Ada_Object_Decl => False, Ada_Anonymous_Object_Decl => False,
      Ada_Extended_Return_Stmt_Object_Decl => False,
      Ada_Package_Renaming_Decl => False, Ada_Single_Protected_Decl => False,
      Ada_Single_Task_Decl => False, Ada_Case_Stmt_Alternative => False,
      Ada_Compilation_Unit => False, Ada_Component_Clause => False,
      Ada_Component_Def => False, Ada_Constant_Absent => False,
      Ada_Constant_Present => False, Ada_Delta_Constraint => False,
      Ada_Digits_Constraint => False, Ada_Discriminant_Constraint => False,
      Ada_Index_Constraint => False, Ada_Range_Constraint => False,
      Ada_Declarative_Part                 => False, Ada_Private_Part => False,
      Ada_Public_Part => False, Ada_Elsif_Expr_Part => False,
      Ada_Elsif_Stmt_Part                  => False, Ada_Allocator => False,
      Ada_Aggregate => False, Ada_Null_Record_Aggregate => False,
      Ada_Bin_Op => False, Ada_Relation_Op => False, Ada_Box_Expr => False,
      Ada_Case_Expr => False, Ada_Case_Expr_Alternative => False,
      Ada_Contract_Cases                   => False, Ada_If_Expr => False,
      Ada_Membership_Expr => False, Ada_Attribute_Ref => False,
      Ada_Update_Attribute_Ref             => False, Ada_Call_Expr => False,
      Ada_Defining_Name => False, Ada_Discrete_Subtype_Name => False,
      Ada_Dotted_Name                      => False, Ada_End_Name => False,
      Ada_Explicit_Deref                   => False, Ada_Qual_Expr => False,
      Ada_Char_Literal => True, Ada_Identifier => True, Ada_Op_Abs => False,
      Ada_Op_And => False, Ada_Op_And_Then => False, Ada_Op_Concat => False,
      Ada_Op_Div => False, Ada_Op_Double_Dot => False, Ada_Op_Eq => False,
      Ada_Op_Gt => False, Ada_Op_Gte => False, Ada_Op_In => False,
      Ada_Op_Lt => False, Ada_Op_Lte => False, Ada_Op_Minus => False,
      Ada_Op_Mod => False, Ada_Op_Mult => False, Ada_Op_Neq => False,
      Ada_Op_Not => False, Ada_Op_Not_In => False, Ada_Op_Or => False,
      Ada_Op_Or_Else => False, Ada_Op_Plus => False, Ada_Op_Pow => False,
      Ada_Op_Rem => False, Ada_Op_Xor => False, Ada_String_Literal => True,
      Ada_Null_Literal                     => True, Ada_Int_Literal => True,
      Ada_Real_Literal                     => True, Ada_Target_Name => False,
      Ada_Paren_Expr => False, Ada_Quantified_Expr => False,
      Ada_Raise_Expr => False, Ada_Un_Op => False, Ada_Handled_Stmts => False,
      Ada_Interface_Kind_Limited           => False,
      Ada_Interface_Kind_Protected         => False,
      Ada_Interface_Kind_Synchronized      => False,
      Ada_Interface_Kind_Task              => False, Ada_Iter_Type_In => False,
      Ada_Iter_Type_Of                     => False, Ada_Library_Item => False,
      Ada_Limited_Absent => False, Ada_Limited_Present => False,
      Ada_For_Loop_Spec => False, Ada_While_Loop_Spec => False,
      Ada_Mode_Default                     => False, Ada_Mode_In => False,
      Ada_Mode_In_Out                      => False, Ada_Mode_Out => False,
      Ada_Not_Null_Absent => False, Ada_Not_Null_Present => False,
      Ada_Null_Component_Decl => False, Ada_Others_Designator => False,
      Ada_Overriding_Not_Overriding        => False,
      Ada_Overriding_Overriding => False, Ada_Overriding_Unspecified => False,
      Ada_Params                           => False, Ada_Pragma_Node => False,
      Ada_Prim_Type_Accessor => False, Ada_Private_Absent => False,
      Ada_Private_Present => False, Ada_Protected_Def => False,
      Ada_Protected_Absent => False, Ada_Protected_Present => False,
      Ada_Quantifier_All => False, Ada_Quantifier_Some => False,
      Ada_Range_Spec => False, Ada_Renaming_Clause => False,
      Ada_Synthetic_Renaming_Clause => False, Ada_Reverse_Absent => False,
      Ada_Reverse_Present => False, Ada_Select_When_Part => False,
      Ada_Accept_Stmt => False, Ada_Accept_Stmt_With_Stmts => False,
      Ada_For_Loop_Stmt                    => False, Ada_Loop_Stmt => False,
      Ada_While_Loop_Stmt                  => False, Ada_Begin_Block => False,
      Ada_Decl_Block                       => False, Ada_Case_Stmt => False,
      Ada_Extended_Return_Stmt             => False, Ada_If_Stmt => False,
      Ada_Named_Stmt                       => False, Ada_Select_Stmt => False,
      Ada_Error_Stmt                       => False, Ada_Abort_Stmt => False,
      Ada_Assign_Stmt                      => False, Ada_Call_Stmt => False,
      Ada_Delay_Stmt => False, Ada_Exit_Stmt => False, Ada_Goto_Stmt => False,
      Ada_Label => False, Ada_Null_Stmt => False, Ada_Raise_Stmt => False,
      Ada_Requeue_Stmt                     => False, Ada_Return_Stmt => False,
      Ada_Terminate_Alternative => False, Ada_Subp_Kind_Function => False,
      Ada_Subp_Kind_Procedure              => False, Ada_Subunit => False,
      Ada_Synchronized_Absent => False, Ada_Synchronized_Present => False,
      Ada_Tagged_Absent => False, Ada_Tagged_Present => False,
      Ada_Task_Def => False, Ada_Access_To_Subp_Def => False,
      Ada_Anonymous_Type_Access_Def => False, Ada_Type_Access_Def => False,
      Ada_Array_Type_Def => False, Ada_Derived_Type_Def => False,
      Ada_Enum_Type_Def => False, Ada_Formal_Discrete_Type_Def => False,
      Ada_Interface_Type_Def => False, Ada_Mod_Int_Type_Def => False,
      Ada_Private_Type_Def => False, Ada_Decimal_Fixed_Point_Def => False,
      Ada_Floating_Point_Def => False, Ada_Ordinary_Fixed_Point_Def => False,
      Ada_Record_Type_Def => False, Ada_Signed_Int_Type_Def => False,
      Ada_Anonymous_Type => False, Ada_Enum_Lit_Synth_Type_Expr => False,
      Ada_Subtype_Indication               => False,
      Ada_Constrained_Subtype_Indication   => False,
      Ada_Discrete_Subtype_Indication      => False,
      Ada_Unconstrained_Array_Index        => False, Ada_Until_Absent => False,
      Ada_Until_Present => False, Ada_Use_Package_Clause => False,
      Ada_Use_Type_Clause                  => False, Ada_Variant => False,
      Ada_Variant_Part                     => False, Ada_With_Clause => False,
      Ada_With_Private_Absent => False, Ada_With_Private_Present => False);
   --  For each node kind, return whether it is a node that contains only a
   --  single token.

   function Wrap_Token_Reference
     (TDH : Token_Data_Handler_Access; Index : Token_Or_Trivia_Index)
      return Token_Reference;
   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access;
   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   procedure Extract_Token_Text
     (Token :     Token_Data_Type; Source_Buffer : out Text_Cst_Access;
      First : out Positive; Last : out Natural);
   --  Implementations for converters soft-links

   Token_Kind_To_Literals : constant array (Token_Kind) of Text_Access :=
     (
Ada_Limited => new Text_Type'("limited"),

      Ada_All => new Text_Type'("all"),
Ada_When      => new Text_Type'("when"),

      Ada_Terminate => new Text_Type'("terminate"),

      Ada_Assign => new Text_Type'(":="),
Ada_Par_Open     => new Text_Type'("("),

      Ada_Comma => new Text_Type'(","),
Ada_Lt          => new Text_Type'("<"),

      Ada_Then => new Text_Type'("then"),

      Ada_Return => new Text_Type'("return"),

      Ada_Arrow => new Text_Type'("=>"),
Ada_Not         => new Text_Type'("not"),

      Ada_Mod => new Text_Type'("mod"),

      Ada_Renames => new Text_Type'("renames"),

      Ada_Subtype => new Text_Type'("subtype"),

      Ada_Pipe => new Text_Type'("|"),
Ada_Out        => new Text_Type'("out"),

      Ada_Xor => new Text_Type'("xor"),
Ada_Tick      => new Text_Type'("'"),

      Ada_Plus => new Text_Type'("+"),
Ada_Divide     => new Text_Type'("/"),

      Ada_Access => new Text_Type'("access"),
Ada_Abs          => new Text_Type'("abs"),

      Ada_Exit => new Text_Type'("exit"),
Ada_Rem        => new Text_Type'("rem"),

      Ada_New => new Text_Type'("new"),
Ada_Semicolon => new Text_Type'(";"),

      Ada_Body => new Text_Type'("body"),
Ada_Goto       => new Text_Type'("goto"),

      Ada_Others => new Text_Type'("others"),

      Ada_Digits => new Text_Type'("digits"),

      Ada_Reverse => new Text_Type'("reverse"),

      Ada_Package => new Text_Type'("package"),
Ada_Of            => new Text_Type'("of"),

      Ada_Range => new Text_Type'("range"),

      Ada_Procedure => new Text_Type'("procedure"),

      Ada_Or => new Text_Type'("or"),
Ada_Gte      => new Text_Type'(">="),

      Ada_Label_End => new Text_Type'(">>"),

      Ada_Private => new Text_Type'("private"),

      Ada_Array => new Text_Type'("array"),

      Ada_Select => new Text_Type'("select"),
Ada_Use          => new Text_Type'("use"),

      Ada_Raise => new Text_Type'("raise"),

      Ada_Doubledot => new Text_Type'(".."),
Ada_Amp             => new Text_Type'("&"),

      Ada_Mult => new Text_Type'("*"),
Ada_Dot        => new Text_Type'("."),

      Ada_Colon => new Text_Type'(":"),
Ada_Type        => new Text_Type'("type"),

      Ada_Label_Start => new Text_Type'("<<"),
Ada_Gt                => new Text_Type'(">"),

      Ada_Function => new Text_Type'("function"),

      Ada_Target => new Text_Type'("@"),
Ada_Else         => new Text_Type'("else"),

      Ada_Pragma => new Text_Type'("pragma"),

      Ada_Delta => new Text_Type'("delta"),
Ada_With        => new Text_Type'("with"),

      Ada_Case => new Text_Type'("case"),

      Ada_Exception => new Text_Type'("exception"),

      Ada_Record => new Text_Type'("record"),

      Ada_While => new Text_Type'("while"),

      Ada_Declare => new Text_Type'("declare"),

      Ada_Loop => new Text_Type'("loop"),
Ada_And        => new Text_Type'("and"),

      Ada_Constant => new Text_Type'("constant"),

      Ada_Do => new Text_Type'("do"),
Ada_Diamond  => new Text_Type'("<>"),

      Ada_Is => new Text_Type'("is"),
Ada_Lte      => new Text_Type'("<="),

      Ada_Accept => new Text_Type'("accept"),

      Ada_Abort => new Text_Type'("abort"),
Ada_At          => new Text_Type'("at"),

      Ada_In => new Text_Type'("in"),
Ada_Null     => new Text_Type'("null"),

      Ada_If => new Text_Type'("if"),
Ada_End      => new Text_Type'("end"),

      Ada_Par_Close => new Text_Type'(")"),
Ada_Minus           => new Text_Type'("-"),

      Ada_Delay => new Text_Type'("delay"),
Ada_Equal       => new Text_Type'("="),

      Ada_Begin => new Text_Type'("begin"),

      Ada_Separate => new Text_Type'("separate"),

      Ada_Power => new Text_Type'("**"),
Ada_Task        => new Text_Type'("task"),

      Ada_For => new Text_Type'("for"),

      Ada_Generic => new Text_Type'("generic"),

      Ada_Notequal => new Text_Type'("/="),

      Ada_Elsif => new Text_Type'("elsif"),

      Ada_Entry => new Text_Type'("entry"),
others          => new Text_Type'(""));

   Token_Kind_Names : constant array (Token_Kind) of String_Access :=
     (Ada_Use => new String'("Use"), Ada_Raise => new String'("Raise"),
      Ada_When => new String'("When"), Ada_Access => new String'("Access"),
      Ada_Exit           => new String'("Exit"), Ada_Xor => new String'("Xor"),
      Ada_Whitespace     => new String'("Whitespace"),
      Ada_Body           => new String'("Body"),
      Ada_Doubledot      => new String'("Doubledot"),
      Ada_Prep_Line      => new String'("Prep_Line"),
      Ada_Goto => new String'("Goto"), Ada_Separate => new String'("Separate"),
      Ada_Declare => new String'("Declare"), Ada_Dot => new String'("Dot"),
      Ada_Others => new String'("Others"), Ada_Not => new String'("Not"),
      Ada_Amp => new String'("Amp"), Ada_Mult => new String'("Mult"),
      Ada_Mod => new String'("Mod"), Ada_Digits => new String'("Digits"),
      Ada_Exception      => new String'("Exception"),
      Ada_Task => new String'("Task"), Ada_Renames => new String'("Renames"),
      Ada_Notequal => new String'("Notequal"), Ada_Of => new String'("Of"),
      Ada_Decimal => new String'("Decimal"), Ada_Delta => new String'("Delta"),
      Ada_Par_Open => new String'("Par_Open"), Ada_Pipe => new String'("Pipe"),
      Ada_Subtype => new String'("Subtype"), Ada_While => new String'("While"),
      Ada_Plus => new String'("Plus"), Ada_Arrow => new String'("Arrow"),
      Ada_Integer => new String'("Integer"), Ada_Or => new String'("Or"),
      Ada_Procedure => new String'("Procedure"), Ada_And => new String'("And"),
      Ada_Label_Start    => new String'("Label_Start"),
      Ada_Gt => new String'("Gt"), Ada_Divide => new String'("Divide"),
      Ada_Semicolon      => new String'("Semicolon"),
      Ada_Abort => new String'("Abort"), Ada_Gte => new String'("Gte"),
      Ada_Select => new String'("Select"), Ada_Out => new String'("Out"),
      Ada_End            => new String'("End"), Ada_For => new String'("For"),
      Ada_Generic => new String'("Generic"), Ada_Delay => new String'("Delay"),
      Ada_Label_End => new String'("Label_End"), Ada_Lt => new String'("Lt"),
      Ada_Abs => new String'("Abs"), Ada_Private => new String'("Private"),
      Ada_Rem            => new String'("Rem"), Ada_New => new String'("New"),
      Ada_Type => new String'("Type"), Ada_Reverse => new String'("Reverse"),
      Ada_Function => new String'("Function"), Ada_Do => new String'("Do"),
      Ada_Begin => new String'("Begin"), Ada_Return => new String'("Return"),
      Ada_String => new String'("String"), Ada_Power => new String'("Power"),
      Ada_Package => new String'("Package"), Ada_Else => new String'("Else"),
      Ada_Pragma => new String'("Pragma"), Ada_Comma => new String'("Comma"),
      Ada_Assign => new String'("Assign"), Ada_Tick => new String'("Tick"),
      Ada_With => new String'("With"), Ada_Case => new String'("Case"),
      Ada_Target => new String'("Target"), Ada_Then => new String'("Then"),
      Ada_Equal => new String'("Equal"), Ada_Elsif => new String'("Elsif"),
      Ada_Record => new String'("Record"), Ada_Range => new String'("Range"),
      Ada_Diamond => new String'("Diamond"), Ada_Lte => new String'("Lte"),
      Ada_Entry          => new String'("Entry"),
      Ada_Identifier     => new String'("Identifier"),
      Ada_Loop => new String'("Loop"), Ada_Comment => new String'("Comment"),
      Ada_Limited => new String'("Limited"), Ada_All => new String'("All"),
      Ada_Null => new String'("Null"), Ada_Constant => new String'("Constant"),
      Ada_Is => new String'("Is"), Ada_Terminate => new String'("Terminate"),
      Ada_Accept => new String'("Accept"), Ada_Char => new String'("Char"),
      Ada_Colon          => new String'("Colon"), Ada_At => new String'("At"),
      Ada_In => new String'("In"), Ada_Array => new String'("Array"),
      Ada_Par_Close      => new String'("Par_Close"),
      Ada_Minus          => new String'("Minus"), Ada_If => new String'("If"),
      Ada_Lexing_Failure => new String'("Lexing_Failure"),
      Ada_Termination    => new String'("Termination"));

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type
   is
   begin
      declare
         Raw_Text : constant Text_Type :=
           (case Index is
when Precomputed_Symbol                                  => """&""",
              when Precomputed_Symbol_1                  => """&""""+""",
              when Precomputed_Symbol_10                 => """=""",
              when Precomputed_Symbol_11                 => """>""",
              when Precomputed_Symbol_12                 => """>=""",
              when Precomputed_Symbol_2                  => """*""",
              when Precomputed_Symbol_20                 => "<<>>",
              when Precomputed_Symbol_3                  => """**""",
              when Precomputed_Symbol_4                  => """+""",
              when Precomputed_Symbol_5                  => """-""",
              when Precomputed_Symbol_6                  => """/""",
              when Precomputed_Symbol_7                  => """/=""",
              when Precomputed_Symbol_8                  => """<""",
              when Precomputed_Symbol_9                  => """<=""",
              when Precomputed_Symbol_Abort              => "abort",
              when Precomputed_Symbol_Abs                => """abs""",
              when Precomputed_Symbol_Abs_158            => "abs",
              when Precomputed_Symbol_Abstract           => "abstract",
              when Precomputed_Symbol_Accept             => "accept",
              when Precomputed_Symbol_Access             => "Access",
              when Precomputed_Symbol_Access_161         => "access",
              when Precomputed_Symbol_Ada                => "Ada",
              when Precomputed_Symbol_Address            => "Address",
              when Precomputed_Symbol_Address_Size       => "Address_Size",
              when Precomputed_Symbol_Aft                => "Aft",
              when Precomputed_Symbol_Aliased            => "aliased",
              when Precomputed_Symbol_Alignment          => "Alignment",
              when Precomputed_Symbol_All                => "all",
              when Precomputed_Symbol_And                => """and""",
              when Precomputed_Symbol_And_164            => "and",
              when Precomputed_Symbol_Array              => "array",
              when Precomputed_Symbol_Assert             => "Assert",
              when Precomputed_Symbol_At                 => "at",
              when Precomputed_Symbol_Aux_Dec            => "Aux_DEC",
              when Precomputed_Symbol_Base               => "Base",
              when Precomputed_Symbol_Begin              => "begin",
              when Precomputed_Symbol_Body               => "body",
              when Precomputed_Symbol_Boolean            => "Boolean",
              when Precomputed_Symbol_Callable           => "Callable",
              when Precomputed_Symbol_Case               => "case",
              when Precomputed_Symbol_Ceiling            => "Ceiling",
              when Precomputed_Symbol_Class              => "Class",
              when Precomputed_Symbol_Compile_Time_Error =>
                "Compile_Time_Error",
              when Precomputed_Symbol_Compile_Time_Warning =>
                "Compile_Time_Warning",
              when Precomputed_Symbol_Component_Size       => "Component_Size",
              when Precomputed_Symbol_Constant             => "constant",
              when Precomputed_Symbol_Constant_Indexing => "Constant_Indexing",
              when Precomputed_Symbol_Convention           => "Convention",
              when Precomputed_Symbol_Cursor               => "Cursor",
              when Precomputed_Symbol_Declare              => "declare",
              when Precomputed_Symbol_Delay                => "delay",
              when Precomputed_Symbol_Delta                => "delta",
              when Precomputed_Symbol_Depends              => "Depends",
              when Precomputed_Symbol_Descriptor_Size => "Descriptor_Size",
              when Precomputed_Symbol_Digits               => "Digits",
              when Precomputed_Symbol_Digits_174           => "digits",
              when Precomputed_Symbol_Do                   => "do",
              when Precomputed_Symbol_Duration             => "Duration",
              when Precomputed_Symbol_Elaborate_Body       => "Elaborate_Body",
              when Precomputed_Symbol_Element              => "Element",
              when Precomputed_Symbol_Else                 => "else",
              when Precomputed_Symbol_Elsif                => "elsif",
              when Precomputed_Symbol_End                  => "end",
              when Precomputed_Symbol_Entry                => "entry",
              when Precomputed_Symbol_Epsilon              => "Epsilon",
              when Precomputed_Symbol_Exception            => "exception",
              when Precomputed_Symbol_Exception_Id         => "Exception_Id",
              when Precomputed_Symbol_Exception_Occurrence =>
                "Exception_Occurrence",
              when Precomputed_Symbol_Exceptions           => "Exceptions",
              when Precomputed_Symbol_Exit                 => "exit",
              when Precomputed_Symbol_Export               => "Export",
              when Precomputed_Symbol_Finalization_Size => "Finalization_Size",
              when Precomputed_Symbol_First                => "First",
              when Precomputed_Symbol_First_Bit            => "First_Bit",
              when Precomputed_Symbol_Floor                => "Floor",
              when Precomputed_Symbol_For                  => "for",
              when Precomputed_Symbol_Fore                 => "Fore",
              when Precomputed_Symbol_Function             => "function",
              when Precomputed_Symbol_Generic              => "generic",
              when Precomputed_Symbol_Global               => "Global",
              when Precomputed_Symbol_Goto                 => "goto",
              when Precomputed_Symbol_Identity             => "Identity",
              when Precomputed_Symbol_If                   => "if",
              when Precomputed_Symbol_Image                => "Image",
              when Precomputed_Symbol_Img                  => "Img",
              when Precomputed_Symbol_Implicit_Dereference =>
                "Implicit_Dereference",
              when Precomputed_Symbol_Import              => "Import",
              when Precomputed_Symbol_Import_Function     => "Import_Function",
              when Precomputed_Symbol_Import_Procedure => "Import_Procedure",
              when Precomputed_Symbol_In                  => "in",
              when Precomputed_Symbol_Inline              => "Inline",
              when Precomputed_Symbol_Input               => "Input",
              when Precomputed_Symbol_Integer             => "Integer",
              when Precomputed_Symbol_Interface           => "Interface",
              when Precomputed_Symbol_Interface_188       => "interface",
              when Precomputed_Symbol_Is                  => "is",
              when Precomputed_Symbol_Iterable            => "Iterable",
              when Precomputed_Symbol_Iterator_Element => "Iterator_Element",
              when Precomputed_Symbol_Iterator_Interfaces =>
                "Iterator_Interfaces",
              when Precomputed_Symbol_Large                        => "Large",
              when Precomputed_Symbol_Last                         => "Last",
              when Precomputed_Symbol_Last_Bit => "Last_Bit",
              when Precomputed_Symbol_Length                       => "Length",
              when Precomputed_Symbol_Limited => "limited",
              when Precomputed_Symbol_Loop                         => "loop",
              when Precomputed_Symbol_Loop_Entry => "Loop_Entry",
              when Precomputed_Symbol_Loop_Invariant => "Loop_Invariant",
              when Precomputed_Symbol_Machine_Mantissa => "Machine_Mantissa",
              when Precomputed_Symbol_Mantissa => "Mantissa",
              when Precomputed_Symbol_Max                          => "Max",
              when Precomputed_Symbol_Max_Size_In_Storage_Elements =>
                "Max_Size_In_Storage_Elements",
              when Precomputed_Symbol_Maximum_Alignment => "Maximum_Alignment",
              when Precomputed_Symbol_Min                   => "Min",
              when Precomputed_Symbol_Mod                   => """mod""",
              when Precomputed_Symbol_Mod_192               => "mod",
              when Precomputed_Symbol_Model                 => "Model",
              when Precomputed_Symbol_Model_Epsilon         => "Model_Epsilon",
              when Precomputed_Symbol_Model_Mantissa => "Model_Mantissa",
              when Precomputed_Symbol_Model_Of              => "Model_Of",
              when Precomputed_Symbol_Model_Post            => "Model_Post",
              when Precomputed_Symbol_Model_Pre             => "Model_Pre",
              when Precomputed_Symbol_Modulus               => "Modulus",
              when Precomputed_Symbol_New                   => "new",
              when Precomputed_Symbol_Nextpart              => "__nextpart",
              when Precomputed_Symbol_Not                   => """not""",
              when Precomputed_Symbol_Not_194               => "not",
              when Precomputed_Symbol_Null                  => "null",
              when Precomputed_Symbol_Object_Size           => "Object_Size",
              when Precomputed_Symbol_Of                    => "of",
              when Precomputed_Symbol_Old                   => "Old",
              when Precomputed_Symbol_Or                    => """or""",
              when Precomputed_Symbol_Or_197                => "or",
              when Precomputed_Symbol_Others                => "others",
              when Precomputed_Symbol_Out                   => "out",
              when Precomputed_Symbol_Output                => "Output",
              when Precomputed_Symbol_Overriding            => "overriding",
              when Precomputed_Symbol_Pack                  => "Pack",
              when Precomputed_Symbol_Package               => "package",
              when Precomputed_Symbol_Pos                   => "Pos",
              when Precomputed_Symbol_Position              => "Position",
              when Precomputed_Symbol_Post                  => "Post",
              when Precomputed_Symbol_Pragma                => "pragma",
              when Precomputed_Symbol_Pre                   => "Pre",
              when Precomputed_Symbol_Pred                  => "Pred",
              when Precomputed_Symbol_Predicate             => "Predicate",
              when Precomputed_Symbol_Preelaborate          => "Preelaborate",
              when Precomputed_Symbol_Private               => "private",
              when Precomputed_Symbol_Privatepart           => "__privatepart",
              when Precomputed_Symbol_Procedure             => "procedure",
              when Precomputed_Symbol_Protected             => "protected",
              when Precomputed_Symbol_Pure                  => "Pure",
              when Precomputed_Symbol_Raise                 => "raise",
              when Precomputed_Symbol_Range                 => "Range",
              when Precomputed_Symbol_Range_207             => "range",
              when Precomputed_Symbol_Read                  => "Read",
              when Precomputed_Symbol_Record                => "record",
              when Precomputed_Symbol_Rem                   => """rem""",
              when Precomputed_Symbol_Rem_209               => "rem",
              when Precomputed_Symbol_Remote_Call_Interface =>
                "Remote_Call_Interface",
              when Precomputed_Symbol_Remote_Types => "Remote_Types",
              when Precomputed_Symbol_Renames                    => "renames",
              when Precomputed_Symbol_Requeue                    => "requeue",
              when Precomputed_Symbol_Result                     => "Result",
              when Precomputed_Symbol_Return                     => "return",
              when Precomputed_Symbol_Reverse                    => "reverse",
              when Precomputed_Symbol_Root_Storage_Pool => "Root_Storage_Pool",
              when Precomputed_Symbol_Root_Stream_Type => "Root_Stream_Type",
              when Precomputed_Symbol_Round                      => "Round",
              when Precomputed_Symbol_Rounding                   => "Rounding",
              when Precomputed_Symbol_Safe_Large => "Safe_Large",
              when Precomputed_Symbol_Safe_Small => "Safe_Small",
              when Precomputed_Symbol_Select                     => "select",
              when Precomputed_Symbol_Separate                   => "separate",
              when Precomputed_Symbol_Shared_Passive => "Shared_Passive",
              when Precomputed_Symbol_Size                       => "Size",
              when Precomputed_Symbol_Small                      => "Small",
              when Precomputed_Symbol_Some                       => "some",
              when Precomputed_Symbol_Standard                   => "Standard",
              when Precomputed_Symbol_Static_Predicate => "Static_Predicate",
              when Precomputed_Symbol_Storage_Pool => "Storage_Pool",
              when Precomputed_Symbol_Storage_Pools => "Storage_Pools",
              when Precomputed_Symbol_Streams                    => "Streams",
              when Precomputed_Symbol_String                     => "String",
              when Precomputed_Symbol_Style_Checks => "Style_Checks",
              when Precomputed_Symbol_Subtype                    => "subtype",
              when Precomputed_Symbol_Succ                       => "Succ",
              when Precomputed_Symbol_Synchronized => "synchronized",
              when Precomputed_Symbol_System                     => "System",
              when Precomputed_Symbol_System_Allocator_Alignment =>
                "System_Allocator_Alignment",
              when Precomputed_Symbol_Tag                 => "Tag",
              when Precomputed_Symbol_Tagged              => "tagged",
              when Precomputed_Symbol_Tags                => "Tags",
              when Precomputed_Symbol_Target_Name         => "Target_Name",
              when Precomputed_Symbol_Task                => "task",
              when Precomputed_Symbol_Task_Id             => "Task_Id",
              when Precomputed_Symbol_Task_Identification =>
                "Task_Identification",
              when Precomputed_Symbol_Terminate          => "terminate",
              when Precomputed_Symbol_Then               => "then",
              when Precomputed_Symbol_Type               => "type",
              when Precomputed_Symbol_Type_Class         => "Type_Class",
              when Precomputed_Symbol_Unchecked_Access   => "Unchecked_Access",
              when Precomputed_Symbol_Universal_Int_Type =>
                "Universal_Int_Type_",
              when Precomputed_Symbol_Universal_Real_Type =>
                "Universal_Real_Type_",
              when Precomputed_Symbol_Unreferenced        => "Unreferenced",
              when Precomputed_Symbol_Unrestricted_Access =>
                "Unrestricted_Access",
              when Precomputed_Symbol_Until             => "until",
              when Precomputed_Symbol_Update            => "Update",
              when Precomputed_Symbol_Use               => "use",
              when Precomputed_Symbol_Vads_Size         => "VADS_Size",
              when Precomputed_Symbol_Val               => "Val",
              when Precomputed_Symbol_Valid             => "Valid",
              when Precomputed_Symbol_Value             => "Value",
              when Precomputed_Symbol_Value_Size        => "Value_Size",
              when Precomputed_Symbol_Variable_Indexing => "Variable_Indexing",
              when Precomputed_Symbol_Volatile          => "Volatile",
              when Precomputed_Symbol_Warnings          => "Warnings",
              when Precomputed_Symbol_When              => "when",
              when Precomputed_Symbol_While             => "while",
              when Precomputed_Symbol_Wide_Image        => "Wide_Image",
              when Precomputed_Symbol_Wide_String       => "Wide_String",
              when Precomputed_Symbol_Wide_Value        => "Wide_Value",
              when Precomputed_Symbol_Wide_Wide_Image   => "Wide_Wide_Image",
              when Precomputed_Symbol_Wide_Wide_String  => "Wide_Wide_String",
              when Precomputed_Symbol_Wide_Wide_Value   => "Wide_Wide_Value",
              when Precomputed_Symbol_Width             => "Width",
              when Precomputed_Symbol_With              => "with",
              when Precomputed_Symbol_Word_Size         => "Word_Size",
              when Precomputed_Symbol_Write             => "Write",
              when Precomputed_Symbol_Xor               => """xor""",
              when Precomputed_Symbol_Xor_229           => "xor");

         Symbol : constant Symbolization_Result :=
           Libadalang.Sources.Canonicalize (Raw_Text);
      begin
         if Symbol.Success then
            return Symbol.Symbol;
         else
            raise Program_Error
              with "Cannot canonicalize symbol literal: " & Image (Raw_Text);
         end if;
      end;
   end Precomputed_Symbol;

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

   ------------------------
   -- Token_Kind_Literal --
   ------------------------

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type is
     (Token_Kind_To_Literals (Token_Id).all);

   -----------------------
   -- Token_Error_Image --
   -----------------------

   function Token_Error_Image (Token_Id : Token_Kind) return String is
      Literal : constant Text_Type := Token_Kind_Literal (Token_Id);
   begin
      return
        (if Literal /= "" then "'" & Image (Literal) & "'"
         else Token_Kind_Name (Token_Id));
   end Token_Error_Image;

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind is
     (Token_Kind'Val (Raw));

   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind is
     (Token_Kind'Pos (Kind));

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : Ada_Node_Kind_Type) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : Ada_Node_Kind_Type) return Boolean is
   begin
      return Kind in Ada_Ada_List;
   end Is_List_Node;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Reference) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference
   is
   begin
      return
        (if Token.TDH = null then No_Token
         else Wrap_Token_Reference
             (Token.TDH, Next (Token.Index, Token.TDH.all, Exclude_Trivia)));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token : Token_Reference; Exclude_Trivia : Boolean := False)
      return Token_Reference
   is
   begin
      return
        (if Token.TDH = null then No_Token
         else Wrap_Token_Reference
             (Token.TDH,
              Previous (Token.Index, Token.TDH.all, Exclude_Trivia)));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Reference) return Symbol_Type is
   begin
      return Get_Symbol (Token.Index, Token.TDH.all);
   end Get_Symbol;

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Reference) return Token_Data_Type is
   begin
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Reference) return Text_Type is
      RD : constant Stored_Token_Data := Raw_Data (Token);
   begin
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------------
   -- Debug_Text --
   ----------------

   function Debug_Text (Token : Token_Reference) return String is
     (Image (Text (Token)));

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Reference) return Text_Type is
      FD : constant Token_Data_Type := Data (First);
      LD : constant Token_Data_Type := Data (Last);
   begin
      if First.TDH /= Last.TDH then
         raise Constraint_Error;
      end if;
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------------
   -- Debug_Text --
   ----------------

   function Debug_Text (First, Last : Token_Reference) return String is
   begin
      return Image (Text (First, Last));
   end Debug_Text;

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token : Token_Reference) return Boolean is
   begin
      return Token.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Reference) return Token_Index is
   begin
      return
        (if Token.Index.Trivia = No_Token_Index then Token.Index.Token
         else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Reference) return Boolean is
      DL : constant Token_Data_Type := Data (L);
      DR : constant Token_Data_Type := Data (R);
      TL : constant Text_Type       := Text (L);
      TR : constant Text_Type       := Text (R);
   begin
      return DL.Kind = DR.Kind and then TL = TR;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Reference) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return
        ("<Token Kind=" & Token_Kind_Name (D.Kind) & " Text=" &
         Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Reference) return Stored_Token_Data is
     (if T.Index.Trivia = No_Token_Index then
        Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
      else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler; Token : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type
   is
   begin
      return (Kind => To_Token_Kind (Raw_Data.Kind),
         Is_Trivia => Token.Index.Trivia /= No_Token_Index,
         Index     =>
           (if Token.Index.Trivia = No_Token_Index then Token.Index.Token
            else Token.Index.Trivia),
         Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
         Source_First  => Raw_Data.Source_First,
         Source_Last   => Raw_Data.Source_Last,
         Sloc_Range    => Raw_Data.Sloc_Range);
   end Convert;

   --------------------------
   -- Wrap_Token_Reference --
   --------------------------

   function Wrap_Token_Reference
     (TDH : Token_Data_Handler_Access; Index : Token_Or_Trivia_Index)
      return Token_Reference
   is
   begin
      return
        (if Index = No_Token_Or_Trivia_Index then No_Token else (TDH, Index));
   end Wrap_Token_Reference;

   -------------------
   -- Get_Token_TDH --
   -------------------

   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access
   is
   begin
      return Token.TDH;
   end Get_Token_TDH;

   ---------------------
   -- Get_Token_Index --
   ---------------------

   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index
   is
   begin
      return Token.Index;
   end Get_Token_Index;

   ------------------------
   -- Extract_Token_Text --
   ------------------------

   procedure Extract_Token_Text
     (Token :     Token_Data_Type; Source_Buffer : out Text_Cst_Access;
      First : out Positive; Last : out Natural)
   is
   begin
      Source_Buffer := Token.Source_Buffer;
      First         := Token.Source_First;
      Last          := Token.Source_Last;
   end Extract_Token_Text;

begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;

   Private_Converters.Wrap_Token_Reference := Wrap_Token_Reference'Access;
   Private_Converters.Get_Token_TDH        := Get_Token_TDH'Access;
   Private_Converters.Get_Token_Index      := Get_Token_Index'Access;
   Private_Converters.Extract_Token_Text   := Extract_Token_Text'Access;
end Libadalang.Common;
