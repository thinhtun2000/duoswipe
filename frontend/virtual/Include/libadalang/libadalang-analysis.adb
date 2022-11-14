with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");
with Ada.Unchecked_Conversion;

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

pragma Warnings (Off, "referenced");
with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Debug;      use Langkit_Support.Adalog.Debug;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;
pragma Warnings (On, "referenced");

with Libadalang.Common;             use Libadalang.Common.Symbols;
with Libadalang.Private_Converters; use Libadalang.Private_Converters;
with Libadalang.Public_Converters;  use Libadalang.Public_Converters;

with Libadalang.Implementation.Extensions;
use Libadalang.Implementation.Extensions;
with Libadalang.Lexer;

package body Libadalang.Analysis is

   use Libadalang.Implementation;
   use AST_Envs;

   function To_Public_Text_Type
     (Value : Character_Type_Array_Access) return Text_Type;

   function To_Public_Completion_Item_Array
     (Value : Internal_Completion_Item_Array_Access)
      return Completion_Item_Array;

   function To_Public_Doc_Annotation_Array
     (Value : Internal_Doc_Annotation_Array_Access)
      return Doc_Annotation_Array;

   function To_Public_Ada_Node_Array
     (Value : Internal_Entity_Array_Access) return Ada_Node_Array;

   function To_Public_Base_Formal_Param_Decl_Array
     (Value : Internal_Entity_Base_Formal_Param_Decl_Array_Access)
      return Base_Formal_Param_Decl_Array;

   function To_Public_Base_Type_Decl_Array
     (Value : Internal_Entity_Base_Type_Decl_Array_Access)
      return Base_Type_Decl_Array;

   function To_Public_Basic_Decl_Array
     (Value : Internal_Entity_Basic_Decl_Array_Access) return Basic_Decl_Array;

   function To_Public_Compilation_Unit_Array
     (Value : Internal_Entity_Compilation_Unit_Array_Access)
      return Compilation_Unit_Array;

   function To_Public_Defining_Name_Array
     (Value : Internal_Entity_Defining_Name_Array_Access)
      return Defining_Name_Array;

   function To_Public_Generic_Instantiation_Array
     (Value : Internal_Entity_Generic_Instantiation_Array_Access)
      return Generic_Instantiation_Array;

   function To_Public_Param_Spec_Array
     (Value : Internal_Entity_Param_Spec_Array_Access) return Param_Spec_Array;

   function To_Public_Type_Decl_Array
     (Value : Internal_Entity_Type_Decl_Array_Access) return Type_Decl_Array;

   function To_Public_Param_Actual_Array
     (Value : Internal_Param_Actual_Array_Access) return Param_Actual_Array;

   function To_Public_Ref_Result_Array
     (Value : Internal_Ref_Result_Array_Access) return Ref_Result_Array;

   function To_Internal_Substitution_Array
     (Value : Substitution_Array) return Internal_Substitution_Array_Access;

   function To_Public_Analysis_Unit_Array
     (Value : Internal_Unit_Array_Access) return Analysis_Unit_Array;

   function To_Internal_Analysis_Unit_Array
     (Value : Analysis_Unit_Array) return Internal_Unit_Array_Access;

   function To_Public_Unbounded_Text_Type_Array
     (Value : Symbol_Type_Array_Access) return Unbounded_Text_Type_Array;

   function To_Public_Aspect (Value : Internal_Aspect) return Aspect;

   function To_Public_Completion_Item
     (Value : Internal_Completion_Item) return Completion_Item;

   function To_Public_Discrete_Range
     (Value : Internal_Discrete_Range) return Discrete_Range;

   function To_Public_Doc_Annotation
     (Value : Internal_Doc_Annotation) return Doc_Annotation;

   function To_Public_Param_Actual
     (Value : Internal_Param_Actual) return Param_Actual;

   function To_Public_Ref_Result
     (Value : Internal_Ref_Result) return Ref_Result;

   function To_Public_Refd_Decl (Value : Internal_Refd_Decl) return Refd_Decl;

   function To_Public_Refd_Def (Value : Internal_Refd_Def) return Refd_Def;

   function To_Internal_Substitution
     (Value : Substitution) return Internal_Substitution;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class) is
   begin
      Provider.Release;
   end Do_Release;

   ------------------------------------
   -- Create_Unit_Provider_Reference --
   ------------------------------------

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference
   is
   begin
      return Result : Unit_Provider_Reference do
         Result.Set (Provider);
      end return;
   end Create_Unit_Provider_Reference;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String                  := Default_Charset;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      With_Trivia   : Boolean := True; Tab_Stop : Positive := 8)
      return Analysis_Context
   is
      use Unit_Provider_References;

      UP : Internal_Unit_Provider_Access :=
        Wrap_Public_Provider (Unit_Provider);
      Result : Internal_Context :=
        Create_Context (Charset, UP, With_Trivia, Tab_Stop);
   begin
      --  Create_Context created an owneship for itself, so don't forget to
      --  remove the share on UP.
      Dec_Ref (UP);

      return Context : constant Analysis_Context := Wrap_Context (Result) do
         --  Result has one ownership share and the call to Wrap_Context
         --  creates a new one, so don't forget to dec-ref before returning.
         Dec_Ref (Result);
      end return;
   end Create_Context;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context : Analysis_Context'Class; Unit_Filename : String) return Boolean
   is
   begin
      return Has_Unit (Unwrap_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String       := ""; Reparse : Boolean := False;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
   begin
      return Wrap_Unit
          (Get_From_File
             (Unwrap_Context (Context), Filename, Charset, Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String       := ""; Buffer : String;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
   begin
      return Wrap_Unit
          (Get_From_Buffer
             (Unwrap_Context (Context), Filename, Charset, Buffer, Rule));
   end Get_From_Buffer;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context : Analysis_Context'Class; Filename : String;
      Charset : String := ""; Buffer : Ada.Strings.Unbounded.Unbounded_String;
      Rule    : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Bytes       : Big_String_Access;
      Bytes_Count : Natural;
   begin
      Get_String (Buffer, Bytes, Bytes_Count);
      return Wrap_Unit
          (Get_From_Buffer
             (Unwrap_Context (Context), Filename, Charset,
              Bytes (1 .. Bytes_Count), Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context : Analysis_Context'Class; Filename : String; Error : Text_Type;
      Charset : String := ""; Rule : Grammar_Rule := Default_Grammar_Rule)
      return Analysis_Unit
   is
      Result : constant Internal_Unit :=
        Implementation.Get_With_Error
          (Unwrap_Context (Context), Filename, Error, Charset, Rule);
   begin
      return Wrap_Unit (Result);
   end Get_With_Error;

   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context : Analysis_Context'Class; Name : Text_Type;
      Kind    : Analysis_Unit_Kind; Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit
   is
   begin
      return Wrap_Unit
          (Get_From_Provider
             (Unwrap_Context (Context), Name, Kind, Charset, Reparse));
   end Get_From_Provider;

   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference
   is
      Provider : constant Internal_Unit_Provider_Access :=
        Unit_Provider (Unwrap_Context (Context));
   begin
      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Context (Context));
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean
   is
   begin
      return Has_With_Trivia (Unwrap_Context (Context));
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean)
   is
   begin
      Discard_Errors_In_Populate_Lexical_Env
        (Unwrap_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural)
   is
   begin
      Set_Logic_Resolution_Timeout (Unwrap_Context (Context), Timeout);
   end Set_Logic_Resolution_Timeout;

   --------------------------
   -- Disable_Lookup_Cache --
   --------------------------

   procedure Disable_Lookup_Cache (Disable : Boolean := True) is
   begin
      Implementation.AST_Envs.Activate_Lookup_Cache := not Disable;
   end Disable_Lookup_Cache;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean
   is
   begin
      return Has_Rewriting_Handle (Unwrap_Context (Context));
   end Has_Rewriting_Handle;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context is
   begin
      return Wrap_Context (Context (Unwrap_Unit (Unit)));
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type is
   begin
      return Hash (Unwrap_Unit (Unit));
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "") is
   begin
      Reparse (Unwrap_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer : String)
   is
   begin
      Reparse (Unwrap_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      Populate_Lexical_Env (Unwrap_Unit (Unit));
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit'Class) return String is
   begin
      return Get_Filename (Unwrap_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit'Class) return String is
   begin
      return Get_Charset (Unwrap_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is
   begin
      return Has_Diagnostics (Unwrap_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array
   is
   begin
      return Implementation.Diagnostics (Unwrap_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String
   is
   begin
      return Format_GNU_Diagnostic (Unwrap_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit'Class) return Ada_Node is
   begin
      return Wrap_Node (Root (Unwrap_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      return First_Token (Unwrap_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      return Last_Token (Unwrap_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      return Token_Count (Unwrap_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      return Trivia_Count (Unwrap_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit'Class) return Text_Type is
   begin
      return Implementation.Text (Unwrap_Unit (Unit));
   end Text;

   ----------------
   -- Debug_Text --
   ----------------

   function Debug_Text (Unit : Analysis_Unit'Class) return String is
   begin
      return Image (Unit.Text);
   end Debug_Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference
   is
   begin
      return Lookup_Token (Unwrap_Unit (Unit), Sloc);
   end Lookup_Token;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      Dump_Lexical_Env (Unwrap_Unit (Unit));
   end Dump_Lexical_Env;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      GNATCOLL.Traces.Set_Active (AST_Envs.Me, Is_Active);
   end Trigger_Envs_Debug;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True)
   is
   begin
      Print (Unwrap_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit'Class) is
   begin
      PP_Trivia (Unwrap_Unit (Unit));
   end PP_Trivia;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Ada_Node'Class) return Boolean is
     (Node.Internal.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Ada_Node'Class) return Boolean is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Is_Token_Node (Node.Internal.Node);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Ada_Node'Class) return Boolean is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Is_Synthetic (Node.Internal.Node);
   end Is_Synthetic;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Ada_Node'Class) return Boolean is
   begin
      Check_Safety_Net (L.Safety_Net);
      Check_Safety_Net (R.Safety_Net);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   ----------------------
   -- Short_Text_Image --
   ----------------------

   function Short_Text_Image (Node : Ada_Node'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Short_Text_Image (Node.Internal.Node);
   end Short_Text_Image;

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image (Node : Ada_Node'Class) return String is
     (Image (Node.Short_Text_Image));

   ----------------
   -- Text_Image --
   ----------------

   function Text_Image (Node : Ada_Node'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Text_Image (Node.Internal);
   end Text_Image;

   -----------
   -- Image --
   -----------

   function Image (Node : Ada_Node'Class) return String is
     (Image (Node.Text_Image));

   -----------------------
   -- Entity converters --
   -----------------------

   function As_Ada_Node (Node : Ada_Node'Class) return Ada_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Ada_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      return (Internal => (Node => N, Info => Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);

   end As_Ada_Node;
   function As_Expr (Node : Ada_Node'Class) return Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Expr then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Expr to " &
           Node.Kind_Name;

      end if;
   end As_Expr;
   function As_Basic_Decl (Node : Ada_Node'Class) return Basic_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Basic_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Basic_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BasicDecl to " &
           Node.Kind_Name;

      end if;
   end As_Basic_Decl;
   function As_Abort_Node (Node : Ada_Node'Class) return Abort_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abort_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abort_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Abort to " &
           Node.Kind_Name;

      end if;
   end As_Abort_Node;
   function As_Abort_Absent (Node : Ada_Node'Class) return Abort_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abort_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abort_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Abort.Absent to " & Node.Kind_Name;

      end if;
   end As_Abort_Absent;
   function As_Abort_Present (Node : Ada_Node'Class) return Abort_Present is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abort_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abort_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Abort.Present to " & Node.Kind_Name;

      end if;
   end As_Abort_Present;
   function As_Stmt (Node : Ada_Node'Class) return Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Stmt then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Stmt to " &
           Node.Kind_Name;

      end if;
   end As_Stmt;
   function As_Simple_Stmt (Node : Ada_Node'Class) return Simple_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Simple_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Simple_Stmt then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SimpleStmt to " & Node.Kind_Name;

      end if;
   end As_Simple_Stmt;
   function As_Abort_Stmt (Node : Ada_Node'Class) return Abort_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abort_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abort_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " AbortStmt to " &
           Node.Kind_Name;

      end if;
   end As_Abort_Stmt;
   function As_Abstract_Node (Node : Ada_Node'Class) return Abstract_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abstract_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abstract_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Abstract to " &
           Node.Kind_Name;

      end if;
   end As_Abstract_Node;
   function As_Abstract_Absent (Node : Ada_Node'Class) return Abstract_Absent
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abstract_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abstract_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Abstract.Absent to " & Node.Kind_Name;

      end if;
   end As_Abstract_Absent;
   function As_Basic_Subp_Decl (Node : Ada_Node'Class) return Basic_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Basic_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Basic_Subp_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BasicSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Basic_Subp_Decl;
   function As_Classic_Subp_Decl
     (Node : Ada_Node'Class) return Classic_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Classic_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Classic_Subp_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ClassicSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Classic_Subp_Decl;
   function As_Formal_Subp_Decl (Node : Ada_Node'Class) return Formal_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Formal_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Formal_Subp_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " FormalSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Formal_Subp_Decl;
   function As_Abstract_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Abstract_Formal_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abstract_Formal_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abstract_Formal_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AbstractFormalSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Abstract_Formal_Subp_Decl;
   function As_Abstract_Present (Node : Ada_Node'Class) return Abstract_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abstract_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abstract_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Abstract.Present to " & Node.Kind_Name;

      end if;
   end As_Abstract_Present;
   function As_Abstract_Subp_Decl
     (Node : Ada_Node'Class) return Abstract_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Abstract_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Abstract_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AbstractSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Abstract_Subp_Decl;
   function As_Composite_Stmt (Node : Ada_Node'Class) return Composite_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Composite_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Composite_Stmt then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CompositeStmt to " & Node.Kind_Name;

      end if;
   end As_Composite_Stmt;
   function As_Accept_Stmt (Node : Ada_Node'Class) return Accept_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Accept_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Accept_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AcceptStmt to " & Node.Kind_Name;

      end if;
   end As_Accept_Stmt;
   function As_Accept_Stmt_With_Stmts
     (Node : Ada_Node'Class) return Accept_Stmt_With_Stmts
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Accept_Stmt_With_Stmts;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Accept_Stmt_With_Stmts_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AcceptStmtWithStmts to " & Node.Kind_Name;

      end if;
   end As_Accept_Stmt_With_Stmts;
   function As_Type_Def (Node : Ada_Node'Class) return Type_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Type_Def then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " TypeDef to " &
           Node.Kind_Name;

      end if;
   end As_Type_Def;
   function As_Access_Def (Node : Ada_Node'Class) return Access_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Access_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Access_Def then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " AccessDef to " &
           Node.Kind_Name;

      end if;
   end As_Access_Def;
   function As_Access_To_Subp_Def
     (Node : Ada_Node'Class) return Access_To_Subp_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Access_To_Subp_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Access_To_Subp_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AccessToSubpDef to " & Node.Kind_Name;

      end if;
   end As_Access_To_Subp_Def;
   function As_Ada_List (Node : Ada_Node'Class) return Ada_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Ada_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Ada_List then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " AdaList to " &
           Node.Kind_Name;

      end if;
   end As_Ada_List;
   function As_Ada_Node_List (Node : Ada_Node'Class) return Ada_Node_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Ada_Node_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Ada_Node_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AdaNode.list to " & Node.Kind_Name;

      end if;
   end As_Ada_Node_List;
   function As_Base_Aggregate (Node : Ada_Node'Class) return Base_Aggregate is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Aggregate;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Aggregate then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseAggregate to " & Node.Kind_Name;

      end if;
   end As_Base_Aggregate;
   function As_Aggregate (Node : Ada_Node'Class) return Aggregate is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aggregate;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aggregate_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Aggregate to " &
           Node.Kind_Name;

      end if;
   end As_Aggregate;
   function As_Basic_Assoc (Node : Ada_Node'Class) return Basic_Assoc is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Basic_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Basic_Assoc then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BasicAssoc to " & Node.Kind_Name;

      end if;
   end As_Basic_Assoc;
   function As_Aggregate_Assoc (Node : Ada_Node'Class) return Aggregate_Assoc
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aggregate_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aggregate_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AggregateAssoc to " & Node.Kind_Name;

      end if;
   end As_Aggregate_Assoc;
   function As_Aliased_Node (Node : Ada_Node'Class) return Aliased_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aliased_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aliased_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Aliased to " &
           Node.Kind_Name;

      end if;
   end As_Aliased_Node;
   function As_Aliased_Absent (Node : Ada_Node'Class) return Aliased_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aliased_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aliased_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Aliased.Absent to " & Node.Kind_Name;

      end if;
   end As_Aliased_Absent;
   function As_Aliased_Present (Node : Ada_Node'Class) return Aliased_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aliased_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aliased_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Aliased.Present to " & Node.Kind_Name;

      end if;
   end As_Aliased_Present;
   function As_All_Node (Node : Ada_Node'Class) return All_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_All_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_All_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " All to " &
           Node.Kind_Name;

      end if;
   end As_All_Node;
   function As_All_Absent (Node : Ada_Node'Class) return All_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_All_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_All_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " All.Absent to " & Node.Kind_Name;

      end if;
   end As_All_Absent;
   function As_All_Present (Node : Ada_Node'Class) return All_Present is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_All_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_All_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " All.Present to " & Node.Kind_Name;

      end if;
   end As_All_Present;
   function As_Allocator (Node : Ada_Node'Class) return Allocator is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Allocator;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Allocator_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Allocator to " &
           Node.Kind_Name;

      end if;
   end As_Allocator;
   function As_Alternatives_List
     (Node : Ada_Node'Class) return Alternatives_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Alternatives_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Alternatives_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AlternativesList to " & Node.Kind_Name;

      end if;
   end As_Alternatives_List;
   function As_Object_Decl (Node : Ada_Node'Class) return Object_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Object_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Object_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ObjectDecl to " & Node.Kind_Name;

      end if;
   end As_Object_Decl;
   function As_Anonymous_Object_Decl
     (Node : Ada_Node'Class) return Anonymous_Object_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Anonymous_Object_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Anonymous_Object_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AnonymousObjectDecl to " & Node.Kind_Name;

      end if;
   end As_Anonymous_Object_Decl;
   function As_Type_Expr (Node : Ada_Node'Class) return Type_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Type_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Type_Expr then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " TypeExpr to " &
           Node.Kind_Name;

      end if;
   end As_Type_Expr;
   function As_Anonymous_Type (Node : Ada_Node'Class) return Anonymous_Type is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Anonymous_Type;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Anonymous_Type_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AnonymousType to " & Node.Kind_Name;

      end if;
   end As_Anonymous_Type;
   function As_Base_Type_Access_Def
     (Node : Ada_Node'Class) return Base_Type_Access_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Type_Access_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Type_Access_Def then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseTypeAccessDef to " & Node.Kind_Name;

      end if;
   end As_Base_Type_Access_Def;
   function As_Anonymous_Type_Access_Def
     (Node : Ada_Node'Class) return Anonymous_Type_Access_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Anonymous_Type_Access_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Anonymous_Type_Access_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AnonymousTypeAccessDef to " & Node.Kind_Name;

      end if;
   end As_Anonymous_Type_Access_Def;
   function As_Base_Type_Decl (Node : Ada_Node'Class) return Base_Type_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Type_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Base_Type_Decl;
   function As_Type_Decl (Node : Ada_Node'Class) return Type_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " TypeDecl to " &
           Node.Kind_Name;

      end if;
   end As_Type_Decl;
   function As_Anonymous_Type_Decl
     (Node : Ada_Node'Class) return Anonymous_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Anonymous_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Anonymous_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AnonymousTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Anonymous_Type_Decl;
   function As_Array_Indices (Node : Ada_Node'Class) return Array_Indices is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Array_Indices;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Array_Indices then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ArrayIndices to " & Node.Kind_Name;

      end if;
   end As_Array_Indices;
   function As_Array_Type_Def (Node : Ada_Node'Class) return Array_Type_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Array_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Array_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ArrayTypeDef to " & Node.Kind_Name;

      end if;
   end As_Array_Type_Def;
   function As_Aspect_Assoc (Node : Ada_Node'Class) return Aspect_Assoc is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aspect_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aspect_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AspectAssoc to " & Node.Kind_Name;

      end if;
   end As_Aspect_Assoc;
   function As_Aspect_Assoc_List
     (Node : Ada_Node'Class) return Aspect_Assoc_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aspect_Assoc_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aspect_Assoc_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AspectAssoc.list to " & Node.Kind_Name;

      end if;
   end As_Aspect_Assoc_List;
   function As_Aspect_Clause (Node : Ada_Node'Class) return Aspect_Clause is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aspect_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aspect_Clause then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AspectClause to " & Node.Kind_Name;

      end if;
   end As_Aspect_Clause;
   function As_Aspect_Spec (Node : Ada_Node'Class) return Aspect_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Aspect_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Aspect_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AspectSpec to " & Node.Kind_Name;

      end if;
   end As_Aspect_Spec;
   function As_Assign_Stmt (Node : Ada_Node'Class) return Assign_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Assign_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Assign_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AssignStmt to " & Node.Kind_Name;

      end if;
   end As_Assign_Stmt;
   function As_Basic_Assoc_List (Node : Ada_Node'Class) return Basic_Assoc_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Basic_Assoc_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Basic_Assoc_List then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BasicAssoc.list to " & Node.Kind_Name;

      end if;
   end As_Basic_Assoc_List;
   function As_Assoc_List (Node : Ada_Node'Class) return Assoc_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Assoc_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Assoc_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " AssocList to " &
           Node.Kind_Name;

      end if;
   end As_Assoc_List;
   function As_At_Clause (Node : Ada_Node'Class) return At_Clause is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_At_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_At_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " AtClause to " &
           Node.Kind_Name;

      end if;
   end As_At_Clause;
   function As_Attribute_Def_Clause
     (Node : Ada_Node'Class) return Attribute_Def_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Attribute_Def_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Attribute_Def_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AttributeDefClause to " & Node.Kind_Name;

      end if;
   end As_Attribute_Def_Clause;
   function As_Name (Node : Ada_Node'Class) return Name is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Name;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Name then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Name to " &
           Node.Kind_Name;

      end if;
   end As_Name;
   function As_Attribute_Ref (Node : Ada_Node'Class) return Attribute_Ref is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Attribute_Ref;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Attribute_Ref_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " AttributeRef to " & Node.Kind_Name;

      end if;
   end As_Attribute_Ref;
   function As_Base_Assoc (Node : Ada_Node'Class) return Base_Assoc is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Assoc then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BaseAssoc to " &
           Node.Kind_Name;

      end if;
   end As_Base_Assoc;
   function As_Base_Assoc_List (Node : Ada_Node'Class) return Base_Assoc_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Assoc_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Assoc_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseAssoc.list to " & Node.Kind_Name;

      end if;
   end As_Base_Assoc_List;
   function As_Base_Formal_Param_Decl
     (Node : Ada_Node'Class) return Base_Formal_Param_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Formal_Param_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Formal_Param_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseFormalParamDecl to " & Node.Kind_Name;

      end if;
   end As_Base_Formal_Param_Decl;
   function As_Base_Formal_Param_Holder
     (Node : Ada_Node'Class) return Base_Formal_Param_Holder
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Formal_Param_Holder;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Formal_Param_Holder then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseFormalParamHolder to " & Node.Kind_Name;

      end if;
   end As_Base_Formal_Param_Holder;
   function As_Single_Tok_Node (Node : Ada_Node'Class) return Single_Tok_Node
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Single_Tok_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Single_Tok_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SingleTokNode to " & Node.Kind_Name;

      end if;
   end As_Single_Tok_Node;
   function As_Base_Id (Node : Ada_Node'Class) return Base_Id is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Id;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Id then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BaseId to " &
           Node.Kind_Name;

      end if;
   end As_Base_Id;
   function As_Base_Loop_Stmt (Node : Ada_Node'Class) return Base_Loop_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Loop_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Loop_Stmt then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseLoopStmt to " & Node.Kind_Name;

      end if;
   end As_Base_Loop_Stmt;
   function As_Base_Package_Decl
     (Node : Ada_Node'Class) return Base_Package_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Package_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Package_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BasePackageDecl to " & Node.Kind_Name;

      end if;
   end As_Base_Package_Decl;
   function As_Base_Record_Def (Node : Ada_Node'Class) return Base_Record_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Record_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Record_Def then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseRecordDef to " & Node.Kind_Name;

      end if;
   end As_Base_Record_Def;
   function As_Body_Node (Node : Ada_Node'Class) return Body_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Body_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Body_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Body to " &
           Node.Kind_Name;

      end if;
   end As_Body_Node;
   function As_Base_Subp_Body (Node : Ada_Node'Class) return Base_Subp_Body is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Subp_Body;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Subp_Body then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseSubpBody to " & Node.Kind_Name;

      end if;
   end As_Base_Subp_Body;
   function As_Base_Subp_Spec (Node : Ada_Node'Class) return Base_Subp_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Subp_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Subp_Spec then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseSubpSpec to " & Node.Kind_Name;

      end if;
   end As_Base_Subp_Spec;
   function As_Base_Subtype_Decl
     (Node : Ada_Node'Class) return Base_Subtype_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Base_Subtype_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Base_Subtype_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BaseSubtypeDecl to " & Node.Kind_Name;

      end if;
   end As_Base_Subtype_Decl;
   function As_Block_Stmt (Node : Ada_Node'Class) return Block_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Block_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Block_Stmt then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BlockStmt to " &
           Node.Kind_Name;

      end if;
   end As_Block_Stmt;
   function As_Begin_Block (Node : Ada_Node'Class) return Begin_Block is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Begin_Block;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Begin_Block_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " BeginBlock to " & Node.Kind_Name;

      end if;
   end As_Begin_Block;
   function As_Bin_Op (Node : Ada_Node'Class) return Bin_Op is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Bin_Op;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Bin_Op_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BinOp to " &
           Node.Kind_Name;

      end if;
   end As_Bin_Op;
   function As_Body_Stub (Node : Ada_Node'Class) return Body_Stub is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Body_Stub;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Body_Stub then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BodyStub to " &
           Node.Kind_Name;

      end if;
   end As_Body_Stub;
   function As_Box_Expr (Node : Ada_Node'Class) return Box_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Box_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Box_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " BoxExpr to " &
           Node.Kind_Name;

      end if;
   end As_Box_Expr;
   function As_Call_Expr (Node : Ada_Node'Class) return Call_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Call_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Call_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " CallExpr to " &
           Node.Kind_Name;

      end if;
   end As_Call_Expr;
   function As_Call_Stmt (Node : Ada_Node'Class) return Call_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Call_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Call_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " CallStmt to " &
           Node.Kind_Name;

      end if;
   end As_Call_Stmt;
   function As_Case_Expr (Node : Ada_Node'Class) return Case_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Case_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Case_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " CaseExpr to " &
           Node.Kind_Name;

      end if;
   end As_Case_Expr;
   function As_Case_Expr_Alternative
     (Node : Ada_Node'Class) return Case_Expr_Alternative
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Case_Expr_Alternative;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Case_Expr_Alternative_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CaseExprAlternative to " & Node.Kind_Name;

      end if;
   end As_Case_Expr_Alternative;
   function As_Case_Expr_Alternative_List
     (Node : Ada_Node'Class) return Case_Expr_Alternative_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Case_Expr_Alternative_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Case_Expr_Alternative_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CaseExprAlternative.list to " & Node.Kind_Name;

      end if;
   end As_Case_Expr_Alternative_List;
   function As_Case_Stmt (Node : Ada_Node'Class) return Case_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Case_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Case_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " CaseStmt to " &
           Node.Kind_Name;

      end if;
   end As_Case_Stmt;
   function As_Case_Stmt_Alternative
     (Node : Ada_Node'Class) return Case_Stmt_Alternative
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Case_Stmt_Alternative;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Case_Stmt_Alternative_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CaseStmtAlternative to " & Node.Kind_Name;

      end if;
   end As_Case_Stmt_Alternative;
   function As_Case_Stmt_Alternative_List
     (Node : Ada_Node'Class) return Case_Stmt_Alternative_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Case_Stmt_Alternative_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Case_Stmt_Alternative_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CaseStmtAlternative.list to " & Node.Kind_Name;

      end if;
   end As_Case_Stmt_Alternative_List;
   function As_Char_Literal (Node : Ada_Node'Class) return Char_Literal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Char_Literal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Char_Literal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CharLiteral to " & Node.Kind_Name;

      end if;
   end As_Char_Literal;
   function As_Classwide_Type_Decl
     (Node : Ada_Node'Class) return Classwide_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Classwide_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Classwide_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ClasswideTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Classwide_Type_Decl;
   function As_Compilation_Unit (Node : Ada_Node'Class) return Compilation_Unit
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Compilation_Unit;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Compilation_Unit_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CompilationUnit to " & Node.Kind_Name;

      end if;
   end As_Compilation_Unit;
   function As_Compilation_Unit_List
     (Node : Ada_Node'Class) return Compilation_Unit_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Compilation_Unit_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Compilation_Unit_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " CompilationUnit.list to " & Node.Kind_Name;

      end if;
   end As_Compilation_Unit_List;
   function As_Component_Clause (Node : Ada_Node'Class) return Component_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Component_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Component_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ComponentClause to " & Node.Kind_Name;

      end if;
   end As_Component_Clause;
   function As_Component_Decl (Node : Ada_Node'Class) return Component_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Component_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Component_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ComponentDecl to " & Node.Kind_Name;

      end if;
   end As_Component_Decl;
   function As_Component_Def (Node : Ada_Node'Class) return Component_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Component_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Component_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ComponentDef to " & Node.Kind_Name;

      end if;
   end As_Component_Def;
   function As_Component_List (Node : Ada_Node'Class) return Component_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Component_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Component_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ComponentList to " & Node.Kind_Name;

      end if;
   end As_Component_List;
   function As_Concrete_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Concrete_Formal_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Concrete_Formal_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Concrete_Formal_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ConcreteFormalSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Concrete_Formal_Subp_Decl;
   function As_Constant_Node (Node : Ada_Node'Class) return Constant_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constant_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constant_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Constant to " &
           Node.Kind_Name;

      end if;
   end As_Constant_Node;
   function As_Constant_Absent (Node : Ada_Node'Class) return Constant_Absent
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constant_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constant_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Constant.Absent to " & Node.Kind_Name;

      end if;
   end As_Constant_Absent;
   function As_Constant_Present (Node : Ada_Node'Class) return Constant_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constant_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constant_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Constant.Present to " & Node.Kind_Name;

      end if;
   end As_Constant_Present;
   function As_Constrained_Array_Indices
     (Node : Ada_Node'Class) return Constrained_Array_Indices
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constrained_Array_Indices;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constrained_Array_Indices_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ConstrainedArrayIndices to " & Node.Kind_Name;

      end if;
   end As_Constrained_Array_Indices;
   function As_Subtype_Indication
     (Node : Ada_Node'Class) return Subtype_Indication
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subtype_Indication;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subtype_Indication_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SubtypeIndication to " & Node.Kind_Name;

      end if;
   end As_Subtype_Indication;
   function As_Constrained_Subtype_Indication
     (Node : Ada_Node'Class) return Constrained_Subtype_Indication
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constrained_Subtype_Indication;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constrained_Subtype_Indication_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ConstrainedSubtypeIndication to " & Node.Kind_Name;

      end if;
   end As_Constrained_Subtype_Indication;
   function As_Constraint (Node : Ada_Node'Class) return Constraint is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constraint;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constraint then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Constraint to " & Node.Kind_Name;

      end if;
   end As_Constraint;
   function As_Constraint_List (Node : Ada_Node'Class) return Constraint_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Constraint_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Constraint_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ConstraintList to " & Node.Kind_Name;

      end if;
   end As_Constraint_List;
   function As_Contract_Case_Assoc
     (Node : Ada_Node'Class) return Contract_Case_Assoc
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Contract_Case_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Contract_Case_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ContractCaseAssoc to " & Node.Kind_Name;

      end if;
   end As_Contract_Case_Assoc;
   function As_Contract_Case_Assoc_List
     (Node : Ada_Node'Class) return Contract_Case_Assoc_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Contract_Case_Assoc_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Contract_Case_Assoc_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ContractCaseAssoc.list to " & Node.Kind_Name;

      end if;
   end As_Contract_Case_Assoc_List;
   function As_Contract_Cases (Node : Ada_Node'Class) return Contract_Cases is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Contract_Cases;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Contract_Cases_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ContractCases to " & Node.Kind_Name;

      end if;
   end As_Contract_Cases;
   function As_Real_Type_Def (Node : Ada_Node'Class) return Real_Type_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Real_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Real_Type_Def then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RealTypeDef to " & Node.Kind_Name;

      end if;
   end As_Real_Type_Def;
   function As_Decimal_Fixed_Point_Def
     (Node : Ada_Node'Class) return Decimal_Fixed_Point_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Decimal_Fixed_Point_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Decimal_Fixed_Point_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DecimalFixedPointDef to " & Node.Kind_Name;

      end if;
   end As_Decimal_Fixed_Point_Def;
   function As_Decl_Block (Node : Ada_Node'Class) return Decl_Block is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Decl_Block;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Decl_Block_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " DeclBlock to " &
           Node.Kind_Name;

      end if;
   end As_Decl_Block;
   function As_Decl_List (Node : Ada_Node'Class) return Decl_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Decl_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Decl_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " DeclList to " &
           Node.Kind_Name;

      end if;
   end As_Decl_List;
   function As_Declarative_Part (Node : Ada_Node'Class) return Declarative_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Declarative_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Declarative_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DeclarativePart to " & Node.Kind_Name;

      end if;
   end As_Declarative_Part;
   function As_Defining_Name (Node : Ada_Node'Class) return Defining_Name is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Defining_Name;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Defining_Name_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DefiningName to " & Node.Kind_Name;

      end if;
   end As_Defining_Name;
   function As_Defining_Name_List
     (Node : Ada_Node'Class) return Defining_Name_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Defining_Name_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Defining_Name_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DefiningName.list to " & Node.Kind_Name;

      end if;
   end As_Defining_Name_List;
   function As_Delay_Stmt (Node : Ada_Node'Class) return Delay_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Delay_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Delay_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " DelayStmt to " &
           Node.Kind_Name;

      end if;
   end As_Delay_Stmt;
   function As_Delta_Constraint (Node : Ada_Node'Class) return Delta_Constraint
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Delta_Constraint;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Delta_Constraint_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DeltaConstraint to " & Node.Kind_Name;

      end if;
   end As_Delta_Constraint;
   function As_Derived_Type_Def (Node : Ada_Node'Class) return Derived_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Derived_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Derived_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DerivedTypeDef to " & Node.Kind_Name;

      end if;
   end As_Derived_Type_Def;
   function As_Digits_Constraint
     (Node : Ada_Node'Class) return Digits_Constraint
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Digits_Constraint;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Digits_Constraint_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DigitsConstraint to " & Node.Kind_Name;

      end if;
   end As_Digits_Constraint;
   function As_Discrete_Base_Subtype_Decl
     (Node : Ada_Node'Class) return Discrete_Base_Subtype_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discrete_Base_Subtype_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discrete_Base_Subtype_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscreteBaseSubtypeDecl to " & Node.Kind_Name;

      end if;
   end As_Discrete_Base_Subtype_Decl;
   function As_Discrete_Subtype_Indication
     (Node : Ada_Node'Class) return Discrete_Subtype_Indication
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discrete_Subtype_Indication;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discrete_Subtype_Indication_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscreteSubtypeIndication to " & Node.Kind_Name;

      end if;
   end As_Discrete_Subtype_Indication;
   function As_Discrete_Subtype_Name
     (Node : Ada_Node'Class) return Discrete_Subtype_Name
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discrete_Subtype_Name;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discrete_Subtype_Name_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscreteSubtypeName to " & Node.Kind_Name;

      end if;
   end As_Discrete_Subtype_Name;
   function As_Discriminant_Assoc
     (Node : Ada_Node'Class) return Discriminant_Assoc
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discriminant_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discriminant_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscriminantAssoc to " & Node.Kind_Name;

      end if;
   end As_Discriminant_Assoc;
   function As_Identifier_List (Node : Ada_Node'Class) return Identifier_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Identifier_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Identifier_List then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Identifier.list to " & Node.Kind_Name;

      end if;
   end As_Identifier_List;
   function As_Discriminant_Choice_List
     (Node : Ada_Node'Class) return Discriminant_Choice_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discriminant_Choice_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discriminant_Choice_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscriminantChoiceList to " & Node.Kind_Name;

      end if;
   end As_Discriminant_Choice_List;
   function As_Discriminant_Constraint
     (Node : Ada_Node'Class) return Discriminant_Constraint
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discriminant_Constraint;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discriminant_Constraint_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscriminantConstraint to " & Node.Kind_Name;

      end if;
   end As_Discriminant_Constraint;
   function As_Discriminant_Part
     (Node : Ada_Node'Class) return Discriminant_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discriminant_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discriminant_Part then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscriminantPart to " & Node.Kind_Name;

      end if;
   end As_Discriminant_Part;
   function As_Discriminant_Spec
     (Node : Ada_Node'Class) return Discriminant_Spec
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discriminant_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discriminant_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscriminantSpec to " & Node.Kind_Name;

      end if;
   end As_Discriminant_Spec;
   function As_Discriminant_Spec_List
     (Node : Ada_Node'Class) return Discriminant_Spec_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Discriminant_Spec_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Discriminant_Spec_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DiscriminantSpec.list to " & Node.Kind_Name;

      end if;
   end As_Discriminant_Spec_List;
   function As_Dotted_Name (Node : Ada_Node'Class) return Dotted_Name is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Dotted_Name;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Dotted_Name_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " DottedName to " & Node.Kind_Name;

      end if;
   end As_Dotted_Name;
   function As_Elsif_Expr_Part (Node : Ada_Node'Class) return Elsif_Expr_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Elsif_Expr_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Elsif_Expr_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ElsifExprPart to " & Node.Kind_Name;

      end if;
   end As_Elsif_Expr_Part;
   function As_Elsif_Expr_Part_List
     (Node : Ada_Node'Class) return Elsif_Expr_Part_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Elsif_Expr_Part_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Elsif_Expr_Part_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ElsifExprPart.list to " & Node.Kind_Name;

      end if;
   end As_Elsif_Expr_Part_List;
   function As_Elsif_Stmt_Part (Node : Ada_Node'Class) return Elsif_Stmt_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Elsif_Stmt_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Elsif_Stmt_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ElsifStmtPart to " & Node.Kind_Name;

      end if;
   end As_Elsif_Stmt_Part;
   function As_Elsif_Stmt_Part_List
     (Node : Ada_Node'Class) return Elsif_Stmt_Part_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Elsif_Stmt_Part_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Elsif_Stmt_Part_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ElsifStmtPart.list to " & Node.Kind_Name;

      end if;
   end As_Elsif_Stmt_Part_List;
   function As_End_Name (Node : Ada_Node'Class) return End_Name is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_End_Name;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_End_Name_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " EndName to " &
           Node.Kind_Name;

      end if;
   end As_End_Name;
   function As_Entry_Body (Node : Ada_Node'Class) return Entry_Body is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Entry_Body;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Entry_Body_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " EntryBody to " &
           Node.Kind_Name;

      end if;
   end As_Entry_Body;
   function As_Entry_Completion_Formal_Params
     (Node : Ada_Node'Class) return Entry_Completion_Formal_Params
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Entry_Completion_Formal_Params;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Entry_Completion_Formal_Params_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EntryCompletionFormalParams to " & Node.Kind_Name;

      end if;
   end As_Entry_Completion_Formal_Params;
   function As_Entry_Decl (Node : Ada_Node'Class) return Entry_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Entry_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Entry_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " EntryDecl to " &
           Node.Kind_Name;

      end if;
   end As_Entry_Decl;
   function As_Entry_Index_Spec (Node : Ada_Node'Class) return Entry_Index_Spec
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Entry_Index_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Entry_Index_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EntryIndexSpec to " & Node.Kind_Name;

      end if;
   end As_Entry_Index_Spec;
   function As_Entry_Spec (Node : Ada_Node'Class) return Entry_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Entry_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Entry_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " EntrySpec to " &
           Node.Kind_Name;

      end if;
   end As_Entry_Spec;
   function As_Enum_Lit_Synth_Type_Expr
     (Node : Ada_Node'Class) return Enum_Lit_Synth_Type_Expr
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Enum_Lit_Synth_Type_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Enum_Lit_Synth_Type_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EnumLitSynthTypeExpr to " & Node.Kind_Name;

      end if;
   end As_Enum_Lit_Synth_Type_Expr;
   function As_Enum_Literal_Decl
     (Node : Ada_Node'Class) return Enum_Literal_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Enum_Literal_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Enum_Literal_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EnumLiteralDecl to " & Node.Kind_Name;

      end if;
   end As_Enum_Literal_Decl;
   function As_Enum_Literal_Decl_List
     (Node : Ada_Node'Class) return Enum_Literal_Decl_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Enum_Literal_Decl_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Enum_Literal_Decl_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EnumLiteralDecl.list to " & Node.Kind_Name;

      end if;
   end As_Enum_Literal_Decl_List;
   function As_Enum_Rep_Clause (Node : Ada_Node'Class) return Enum_Rep_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Enum_Rep_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Enum_Rep_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EnumRepClause to " & Node.Kind_Name;

      end if;
   end As_Enum_Rep_Clause;
   function As_Enum_Subp_Spec (Node : Ada_Node'Class) return Enum_Subp_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Enum_Subp_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Enum_Subp_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EnumSubpSpec to " & Node.Kind_Name;

      end if;
   end As_Enum_Subp_Spec;
   function As_Enum_Type_Def (Node : Ada_Node'Class) return Enum_Type_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Enum_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Enum_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " EnumTypeDef to " & Node.Kind_Name;

      end if;
   end As_Enum_Type_Def;
   function As_Error_Decl (Node : Ada_Node'Class) return Error_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Error_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Error_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " ErrorDecl to " &
           Node.Kind_Name;

      end if;
   end As_Error_Decl;
   function As_Error_Stmt (Node : Ada_Node'Class) return Error_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Error_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Error_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " ErrorStmt to " &
           Node.Kind_Name;

      end if;
   end As_Error_Stmt;
   function As_Exception_Decl (Node : Ada_Node'Class) return Exception_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Exception_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Exception_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExceptionDecl to " & Node.Kind_Name;

      end if;
   end As_Exception_Decl;
   function As_Exception_Handler
     (Node : Ada_Node'Class) return Exception_Handler
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Exception_Handler;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Exception_Handler_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExceptionHandler to " & Node.Kind_Name;

      end if;
   end As_Exception_Handler;
   function As_Exit_Stmt (Node : Ada_Node'Class) return Exit_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Exit_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Exit_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " ExitStmt to " &
           Node.Kind_Name;

      end if;
   end As_Exit_Stmt;
   function As_Explicit_Deref (Node : Ada_Node'Class) return Explicit_Deref is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Explicit_Deref;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Explicit_Deref_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExplicitDeref to " & Node.Kind_Name;

      end if;
   end As_Explicit_Deref;
   function As_Expr_List (Node : Ada_Node'Class) return Expr_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Expr_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Expr_List then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Expr.list to " &
           Node.Kind_Name;

      end if;
   end As_Expr_List;
   function As_Expr_Alternatives_List
     (Node : Ada_Node'Class) return Expr_Alternatives_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Expr_Alternatives_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Expr_Alternatives_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExprAlternativesList to " & Node.Kind_Name;

      end if;
   end As_Expr_Alternatives_List;
   function As_Expr_Function (Node : Ada_Node'Class) return Expr_Function is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Expr_Function;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Expr_Function_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExprFunction to " & Node.Kind_Name;

      end if;
   end As_Expr_Function;
   function As_Extended_Return_Stmt
     (Node : Ada_Node'Class) return Extended_Return_Stmt
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Extended_Return_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Extended_Return_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExtendedReturnStmt to " & Node.Kind_Name;

      end if;
   end As_Extended_Return_Stmt;
   function As_Extended_Return_Stmt_Object_Decl
     (Node : Ada_Node'Class) return Extended_Return_Stmt_Object_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Extended_Return_Stmt_Object_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Extended_Return_Stmt_Object_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ExtendedReturnStmtObjectDecl to " & Node.Kind_Name;

      end if;
   end As_Extended_Return_Stmt_Object_Decl;
   function As_Floating_Point_Def
     (Node : Ada_Node'Class) return Floating_Point_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Floating_Point_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Floating_Point_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " FloatingPointDef to " & Node.Kind_Name;

      end if;
   end As_Floating_Point_Def;
   function As_Loop_Spec (Node : Ada_Node'Class) return Loop_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Loop_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Loop_Spec then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " LoopSpec to " &
           Node.Kind_Name;

      end if;
   end As_Loop_Spec;
   function As_For_Loop_Spec (Node : Ada_Node'Class) return For_Loop_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_For_Loop_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_For_Loop_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ForLoopSpec to " & Node.Kind_Name;

      end if;
   end As_For_Loop_Spec;
   function As_For_Loop_Stmt (Node : Ada_Node'Class) return For_Loop_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_For_Loop_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_For_Loop_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ForLoopStmt to " & Node.Kind_Name;

      end if;
   end As_For_Loop_Stmt;
   function As_For_Loop_Var_Decl
     (Node : Ada_Node'Class) return For_Loop_Var_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_For_Loop_Var_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_For_Loop_Var_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ForLoopVarDecl to " & Node.Kind_Name;

      end if;
   end As_For_Loop_Var_Decl;
   function As_Formal_Discrete_Type_Def
     (Node : Ada_Node'Class) return Formal_Discrete_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Formal_Discrete_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Formal_Discrete_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " FormalDiscreteTypeDef to " & Node.Kind_Name;

      end if;
   end As_Formal_Discrete_Type_Def;
   function As_Generic_Decl (Node : Ada_Node'Class) return Generic_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Decl;
   function As_Generic_Formal (Node : Ada_Node'Class) return Generic_Formal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Formal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Formal then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericFormal to " & Node.Kind_Name;

      end if;
   end As_Generic_Formal;
   function As_Generic_Formal_Obj_Decl
     (Node : Ada_Node'Class) return Generic_Formal_Obj_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Formal_Obj_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Formal_Obj_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericFormalObjDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Formal_Obj_Decl;
   function As_Generic_Formal_Package
     (Node : Ada_Node'Class) return Generic_Formal_Package
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Formal_Package;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Formal_Package_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericFormalPackage to " & Node.Kind_Name;

      end if;
   end As_Generic_Formal_Package;
   function As_Generic_Formal_Part
     (Node : Ada_Node'Class) return Generic_Formal_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Formal_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Formal_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericFormalPart to " & Node.Kind_Name;

      end if;
   end As_Generic_Formal_Part;
   function As_Generic_Formal_Subp_Decl
     (Node : Ada_Node'Class) return Generic_Formal_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Formal_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Formal_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericFormalSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Formal_Subp_Decl;
   function As_Generic_Formal_Type_Decl
     (Node : Ada_Node'Class) return Generic_Formal_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Formal_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Formal_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericFormalTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Formal_Type_Decl;
   function As_Generic_Instantiation
     (Node : Ada_Node'Class) return Generic_Instantiation
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Instantiation;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Instantiation then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericInstantiation to " & Node.Kind_Name;

      end if;
   end As_Generic_Instantiation;
   function As_Generic_Package_Decl
     (Node : Ada_Node'Class) return Generic_Package_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Package_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Package_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericPackageDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Package_Decl;
   function As_Generic_Package_Instantiation
     (Node : Ada_Node'Class) return Generic_Package_Instantiation
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Package_Instantiation;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Package_Instantiation_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericPackageInstantiation to " & Node.Kind_Name;

      end if;
   end As_Generic_Package_Instantiation;
   function As_Generic_Package_Internal
     (Node : Ada_Node'Class) return Generic_Package_Internal
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Package_Internal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Package_Internal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericPackageInternal to " & Node.Kind_Name;

      end if;
   end As_Generic_Package_Internal;
   function As_Generic_Renaming_Decl
     (Node : Ada_Node'Class) return Generic_Renaming_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Renaming_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Renaming_Decl then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericRenamingDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Renaming_Decl;
   function As_Generic_Package_Renaming_Decl
     (Node : Ada_Node'Class) return Generic_Package_Renaming_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Package_Renaming_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Package_Renaming_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericPackageRenamingDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Package_Renaming_Decl;
   function As_Generic_Subp_Decl
     (Node : Ada_Node'Class) return Generic_Subp_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Subp_Decl;
   function As_Generic_Subp_Instantiation
     (Node : Ada_Node'Class) return Generic_Subp_Instantiation
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Subp_Instantiation;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Subp_Instantiation_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericSubpInstantiation to " & Node.Kind_Name;

      end if;
   end As_Generic_Subp_Instantiation;
   function As_Generic_Subp_Internal
     (Node : Ada_Node'Class) return Generic_Subp_Internal
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Subp_Internal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Subp_Internal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericSubpInternal to " & Node.Kind_Name;

      end if;
   end As_Generic_Subp_Internal;
   function As_Generic_Subp_Renaming_Decl
     (Node : Ada_Node'Class) return Generic_Subp_Renaming_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Generic_Subp_Renaming_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Generic_Subp_Renaming_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " GenericSubpRenamingDecl to " & Node.Kind_Name;

      end if;
   end As_Generic_Subp_Renaming_Decl;
   function As_Goto_Stmt (Node : Ada_Node'Class) return Goto_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Goto_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Goto_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " GotoStmt to " &
           Node.Kind_Name;

      end if;
   end As_Goto_Stmt;
   function As_Handled_Stmts (Node : Ada_Node'Class) return Handled_Stmts is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Handled_Stmts;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Handled_Stmts_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " HandledStmts to " & Node.Kind_Name;

      end if;
   end As_Handled_Stmts;
   function As_Identifier (Node : Ada_Node'Class) return Identifier is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Identifier;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Identifier_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Identifier to " & Node.Kind_Name;

      end if;
   end As_Identifier;
   function As_If_Expr (Node : Ada_Node'Class) return If_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_If_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_If_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " IfExpr to " &
           Node.Kind_Name;

      end if;
   end As_If_Expr;
   function As_If_Stmt (Node : Ada_Node'Class) return If_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_If_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_If_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " IfStmt to " &
           Node.Kind_Name;

      end if;
   end As_If_Stmt;
   function As_Incomplete_Type_Decl
     (Node : Ada_Node'Class) return Incomplete_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Incomplete_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Incomplete_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " IncompleteTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Incomplete_Type_Decl;
   function As_Incomplete_Tagged_Type_Decl
     (Node : Ada_Node'Class) return Incomplete_Tagged_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Incomplete_Tagged_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Incomplete_Tagged_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " IncompleteTaggedTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Incomplete_Tagged_Type_Decl;
   function As_Index_Constraint (Node : Ada_Node'Class) return Index_Constraint
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Index_Constraint;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Index_Constraint_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " IndexConstraint to " & Node.Kind_Name;

      end if;
   end As_Index_Constraint;
   function As_Num_Literal (Node : Ada_Node'Class) return Num_Literal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Num_Literal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Num_Literal then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NumLiteral to " & Node.Kind_Name;

      end if;
   end As_Num_Literal;
   function As_Int_Literal (Node : Ada_Node'Class) return Int_Literal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Int_Literal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Int_Literal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " IntLiteral to " & Node.Kind_Name;

      end if;
   end As_Int_Literal;
   function As_Interface_Kind (Node : Ada_Node'Class) return Interface_Kind is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Interface_Kind;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Interface_Kind then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " InterfaceKind to " & Node.Kind_Name;

      end if;
   end As_Interface_Kind;
   function As_Interface_Kind_Limited
     (Node : Ada_Node'Class) return Interface_Kind_Limited
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Interface_Kind_Limited;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Interface_Kind_Limited_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " InterfaceKind.Limited to " & Node.Kind_Name;

      end if;
   end As_Interface_Kind_Limited;
   function As_Interface_Kind_Protected
     (Node : Ada_Node'Class) return Interface_Kind_Protected
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Interface_Kind_Protected;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Interface_Kind_Protected_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " InterfaceKind.Protected to " & Node.Kind_Name;

      end if;
   end As_Interface_Kind_Protected;
   function As_Interface_Kind_Synchronized
     (Node : Ada_Node'Class) return Interface_Kind_Synchronized
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Interface_Kind_Synchronized;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Interface_Kind_Synchronized_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " InterfaceKind.Synchronized to " & Node.Kind_Name;

      end if;
   end As_Interface_Kind_Synchronized;
   function As_Interface_Kind_Task
     (Node : Ada_Node'Class) return Interface_Kind_Task
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Interface_Kind_Task;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Interface_Kind_Task_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " InterfaceKind.Task to " & Node.Kind_Name;

      end if;
   end As_Interface_Kind_Task;
   function As_Interface_Type_Def
     (Node : Ada_Node'Class) return Interface_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Interface_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Interface_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " InterfaceTypeDef to " & Node.Kind_Name;

      end if;
   end As_Interface_Type_Def;
   function As_Iter_Type (Node : Ada_Node'Class) return Iter_Type is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Iter_Type;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Iter_Type then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " IterType to " &
           Node.Kind_Name;

      end if;
   end As_Iter_Type;
   function As_Iter_Type_In (Node : Ada_Node'Class) return Iter_Type_In is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Iter_Type_In;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Iter_Type_In_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " IterType.In to " & Node.Kind_Name;

      end if;
   end As_Iter_Type_In;
   function As_Iter_Type_Of (Node : Ada_Node'Class) return Iter_Type_Of is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Iter_Type_Of;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Iter_Type_Of_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " IterType.Of to " & Node.Kind_Name;

      end if;
   end As_Iter_Type_Of;
   function As_Known_Discriminant_Part
     (Node : Ada_Node'Class) return Known_Discriminant_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Known_Discriminant_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Known_Discriminant_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " KnownDiscriminantPart to " & Node.Kind_Name;

      end if;
   end As_Known_Discriminant_Part;
   function As_Label (Node : Ada_Node'Class) return Label is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Label;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Label_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Label to " &
           Node.Kind_Name;

      end if;
   end As_Label;
   function As_Label_Decl (Node : Ada_Node'Class) return Label_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Label_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Label_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " LabelDecl to " &
           Node.Kind_Name;

      end if;
   end As_Label_Decl;
   function As_Library_Item (Node : Ada_Node'Class) return Library_Item is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Library_Item;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Library_Item_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " LibraryItem to " & Node.Kind_Name;

      end if;
   end As_Library_Item;
   function As_Limited_Node (Node : Ada_Node'Class) return Limited_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Limited_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Limited_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Limited to " &
           Node.Kind_Name;

      end if;
   end As_Limited_Node;
   function As_Limited_Absent (Node : Ada_Node'Class) return Limited_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Limited_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Limited_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Limited.Absent to " & Node.Kind_Name;

      end if;
   end As_Limited_Absent;
   function As_Limited_Present (Node : Ada_Node'Class) return Limited_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Limited_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Limited_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Limited.Present to " & Node.Kind_Name;

      end if;
   end As_Limited_Present;
   function As_Loop_Stmt (Node : Ada_Node'Class) return Loop_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Loop_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Loop_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " LoopStmt to " &
           Node.Kind_Name;

      end if;
   end As_Loop_Stmt;
   function As_Membership_Expr (Node : Ada_Node'Class) return Membership_Expr
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Membership_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Membership_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " MembershipExpr to " & Node.Kind_Name;

      end if;
   end As_Membership_Expr;
   function As_Mod_Int_Type_Def (Node : Ada_Node'Class) return Mod_Int_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Mod_Int_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Mod_Int_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ModIntTypeDef to " & Node.Kind_Name;

      end if;
   end As_Mod_Int_Type_Def;
   function As_Mode (Node : Ada_Node'Class) return Mode is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Mode;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Mode then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Mode to " &
           Node.Kind_Name;

      end if;
   end As_Mode;
   function As_Mode_Default (Node : Ada_Node'Class) return Mode_Default is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Mode_Default;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Mode_Default_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Mode.Default to " & Node.Kind_Name;

      end if;
   end As_Mode_Default;
   function As_Mode_In (Node : Ada_Node'Class) return Mode_In is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Mode_In;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Mode_In_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Mode.In to " &
           Node.Kind_Name;

      end if;
   end As_Mode_In;
   function As_Mode_In_Out (Node : Ada_Node'Class) return Mode_In_Out is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Mode_In_Out;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Mode_In_Out_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Mode.InOut to " & Node.Kind_Name;

      end if;
   end As_Mode_In_Out;
   function As_Mode_Out (Node : Ada_Node'Class) return Mode_Out is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Mode_Out;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Mode_Out_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Mode.Out to " &
           Node.Kind_Name;

      end if;
   end As_Mode_Out;
   function As_Multi_Dim_Array_Assoc
     (Node : Ada_Node'Class) return Multi_Dim_Array_Assoc
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Multi_Dim_Array_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Multi_Dim_Array_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " MultiDimArrayAssoc to " & Node.Kind_Name;

      end if;
   end As_Multi_Dim_Array_Assoc;
   function As_Name_List (Node : Ada_Node'Class) return Name_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Name_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Name_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Name.list to " &
           Node.Kind_Name;

      end if;
   end As_Name_List;
   function As_Named_Stmt (Node : Ada_Node'Class) return Named_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Named_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Named_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " NamedStmt to " &
           Node.Kind_Name;

      end if;
   end As_Named_Stmt;
   function As_Named_Stmt_Decl (Node : Ada_Node'Class) return Named_Stmt_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Named_Stmt_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Named_Stmt_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NamedStmtDecl to " & Node.Kind_Name;

      end if;
   end As_Named_Stmt_Decl;
   function As_Not_Null (Node : Ada_Node'Class) return Not_Null is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Not_Null;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Not_Null then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " NotNull to " &
           Node.Kind_Name;

      end if;
   end As_Not_Null;
   function As_Not_Null_Absent (Node : Ada_Node'Class) return Not_Null_Absent
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Not_Null_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Not_Null_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NotNull.Absent to " & Node.Kind_Name;

      end if;
   end As_Not_Null_Absent;
   function As_Not_Null_Present (Node : Ada_Node'Class) return Not_Null_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Not_Null_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Not_Null_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NotNull.Present to " & Node.Kind_Name;

      end if;
   end As_Not_Null_Present;
   function As_Null_Component_Decl
     (Node : Ada_Node'Class) return Null_Component_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Null_Component_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Null_Component_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NullComponentDecl to " & Node.Kind_Name;

      end if;
   end As_Null_Component_Decl;
   function As_Null_Literal (Node : Ada_Node'Class) return Null_Literal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Null_Literal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Null_Literal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NullLiteral to " & Node.Kind_Name;

      end if;
   end As_Null_Literal;
   function As_Null_Record_Aggregate
     (Node : Ada_Node'Class) return Null_Record_Aggregate
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Null_Record_Aggregate;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Null_Record_Aggregate_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NullRecordAggregate to " & Node.Kind_Name;

      end if;
   end As_Null_Record_Aggregate;
   function As_Null_Record_Def (Node : Ada_Node'Class) return Null_Record_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Null_Record_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Null_Record_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NullRecordDef to " & Node.Kind_Name;

      end if;
   end As_Null_Record_Def;
   function As_Null_Stmt (Node : Ada_Node'Class) return Null_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Null_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Null_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " NullStmt to " &
           Node.Kind_Name;

      end if;
   end As_Null_Stmt;
   function As_Null_Subp_Decl (Node : Ada_Node'Class) return Null_Subp_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Null_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Null_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NullSubpDecl to " & Node.Kind_Name;

      end if;
   end As_Null_Subp_Decl;
   function As_Number_Decl (Node : Ada_Node'Class) return Number_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Number_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Number_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " NumberDecl to " & Node.Kind_Name;

      end if;
   end As_Number_Decl;
   function As_Op (Node : Ada_Node'Class) return Op is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op to " &
           Node.Kind_Name;

      end if;
   end As_Op;
   function As_Op_Abs (Node : Ada_Node'Class) return Op_Abs is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Abs;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Abs_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Abs to " &
           Node.Kind_Name;

      end if;
   end As_Op_Abs;
   function As_Op_And (Node : Ada_Node'Class) return Op_And is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_And;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_And_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.And to " &
           Node.Kind_Name;

      end if;
   end As_Op_And;
   function As_Op_And_Then (Node : Ada_Node'Class) return Op_And_Then is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_And_Then;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_And_Then_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Op.AndThen to " & Node.Kind_Name;

      end if;
   end As_Op_And_Then;
   function As_Op_Concat (Node : Ada_Node'Class) return Op_Concat is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Concat;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Concat_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Concat to " &
           Node.Kind_Name;

      end if;
   end As_Op_Concat;
   function As_Op_Div (Node : Ada_Node'Class) return Op_Div is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Div;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Div_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Div to " &
           Node.Kind_Name;

      end if;
   end As_Op_Div;
   function As_Op_Double_Dot (Node : Ada_Node'Class) return Op_Double_Dot is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Double_Dot;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Double_Dot_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Op.DoubleDot to " & Node.Kind_Name;

      end if;
   end As_Op_Double_Dot;
   function As_Op_Eq (Node : Ada_Node'Class) return Op_Eq is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Eq;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Eq_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Eq to " &
           Node.Kind_Name;

      end if;
   end As_Op_Eq;
   function As_Op_Gt (Node : Ada_Node'Class) return Op_Gt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Gt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Gt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Gt to " &
           Node.Kind_Name;

      end if;
   end As_Op_Gt;
   function As_Op_Gte (Node : Ada_Node'Class) return Op_Gte is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Gte;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Gte_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Gte to " &
           Node.Kind_Name;

      end if;
   end As_Op_Gte;
   function As_Op_In (Node : Ada_Node'Class) return Op_In is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_In;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_In_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.In to " &
           Node.Kind_Name;

      end if;
   end As_Op_In;
   function As_Op_Lt (Node : Ada_Node'Class) return Op_Lt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Lt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Lt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Lt to " &
           Node.Kind_Name;

      end if;
   end As_Op_Lt;
   function As_Op_Lte (Node : Ada_Node'Class) return Op_Lte is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Lte;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Lte_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Lte to " &
           Node.Kind_Name;

      end if;
   end As_Op_Lte;
   function As_Op_Minus (Node : Ada_Node'Class) return Op_Minus is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Minus;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Minus_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Minus to " &
           Node.Kind_Name;

      end if;
   end As_Op_Minus;
   function As_Op_Mod (Node : Ada_Node'Class) return Op_Mod is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Mod;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Mod_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Mod to " &
           Node.Kind_Name;

      end if;
   end As_Op_Mod;
   function As_Op_Mult (Node : Ada_Node'Class) return Op_Mult is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Mult;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Mult_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Mult to " &
           Node.Kind_Name;

      end if;
   end As_Op_Mult;
   function As_Op_Neq (Node : Ada_Node'Class) return Op_Neq is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Neq;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Neq_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Neq to " &
           Node.Kind_Name;

      end if;
   end As_Op_Neq;
   function As_Op_Not (Node : Ada_Node'Class) return Op_Not is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Not;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Not_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Not to " &
           Node.Kind_Name;

      end if;
   end As_Op_Not;
   function As_Op_Not_In (Node : Ada_Node'Class) return Op_Not_In is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Not_In;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Not_In_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.NotIn to " &
           Node.Kind_Name;

      end if;
   end As_Op_Not_In;
   function As_Op_Or (Node : Ada_Node'Class) return Op_Or is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Or;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Or_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Or to " &
           Node.Kind_Name;

      end if;
   end As_Op_Or;
   function As_Op_Or_Else (Node : Ada_Node'Class) return Op_Or_Else is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Or_Else;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Or_Else_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.OrElse to " &
           Node.Kind_Name;

      end if;
   end As_Op_Or_Else;
   function As_Op_Plus (Node : Ada_Node'Class) return Op_Plus is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Plus;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Plus_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Plus to " &
           Node.Kind_Name;

      end if;
   end As_Op_Plus;
   function As_Op_Pow (Node : Ada_Node'Class) return Op_Pow is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Pow;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Pow_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Pow to " &
           Node.Kind_Name;

      end if;
   end As_Op_Pow;
   function As_Op_Rem (Node : Ada_Node'Class) return Op_Rem is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Rem;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Rem_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Rem to " &
           Node.Kind_Name;

      end if;
   end As_Op_Rem;
   function As_Op_Xor (Node : Ada_Node'Class) return Op_Xor is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Op_Xor;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Op_Xor_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Op.Xor to " &
           Node.Kind_Name;

      end if;
   end As_Op_Xor;
   function As_Ordinary_Fixed_Point_Def
     (Node : Ada_Node'Class) return Ordinary_Fixed_Point_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Ordinary_Fixed_Point_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Ordinary_Fixed_Point_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " OrdinaryFixedPointDef to " & Node.Kind_Name;

      end if;
   end As_Ordinary_Fixed_Point_Def;
   function As_Others_Designator
     (Node : Ada_Node'Class) return Others_Designator
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Others_Designator;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Others_Designator_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " OthersDesignator to " & Node.Kind_Name;

      end if;
   end As_Others_Designator;
   function As_Overriding_Node (Node : Ada_Node'Class) return Overriding_Node
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Overriding_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Overriding_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Overriding to " & Node.Kind_Name;

      end if;
   end As_Overriding_Node;
   function As_Overriding_Not_Overriding
     (Node : Ada_Node'Class) return Overriding_Not_Overriding
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Overriding_Not_Overriding;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Overriding_Not_Overriding_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Overriding.NotOverriding to " & Node.Kind_Name;

      end if;
   end As_Overriding_Not_Overriding;
   function As_Overriding_Overriding
     (Node : Ada_Node'Class) return Overriding_Overriding
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Overriding_Overriding;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Overriding_Overriding_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Overriding.Overriding to " & Node.Kind_Name;

      end if;
   end As_Overriding_Overriding;
   function As_Overriding_Unspecified
     (Node : Ada_Node'Class) return Overriding_Unspecified
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Overriding_Unspecified;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Overriding_Unspecified_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Overriding.Unspecified to " & Node.Kind_Name;

      end if;
   end As_Overriding_Unspecified;
   function As_Package_Body (Node : Ada_Node'Class) return Package_Body is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Package_Body;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Package_Body_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PackageBody to " & Node.Kind_Name;

      end if;
   end As_Package_Body;
   function As_Package_Body_Stub
     (Node : Ada_Node'Class) return Package_Body_Stub
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Package_Body_Stub;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Package_Body_Stub_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PackageBodyStub to " & Node.Kind_Name;

      end if;
   end As_Package_Body_Stub;
   function As_Package_Decl (Node : Ada_Node'Class) return Package_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Package_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Package_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PackageDecl to " & Node.Kind_Name;

      end if;
   end As_Package_Decl;
   function As_Package_Renaming_Decl
     (Node : Ada_Node'Class) return Package_Renaming_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Package_Renaming_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Package_Renaming_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PackageRenamingDecl to " & Node.Kind_Name;

      end if;
   end As_Package_Renaming_Decl;
   function As_Param_Assoc (Node : Ada_Node'Class) return Param_Assoc is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Param_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Param_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ParamAssoc to " & Node.Kind_Name;

      end if;
   end As_Param_Assoc;
   function As_Param_Spec (Node : Ada_Node'Class) return Param_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Param_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Param_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " ParamSpec to " &
           Node.Kind_Name;

      end if;
   end As_Param_Spec;
   function As_Param_Spec_List (Node : Ada_Node'Class) return Param_Spec_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Param_Spec_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Param_Spec_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ParamSpec.list to " & Node.Kind_Name;

      end if;
   end As_Param_Spec_List;
   function As_Params (Node : Ada_Node'Class) return Params is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Params;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Params_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Params to " &
           Node.Kind_Name;

      end if;
   end As_Params;
   function As_Paren_Expr (Node : Ada_Node'Class) return Paren_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Paren_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Paren_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " ParenExpr to " &
           Node.Kind_Name;

      end if;
   end As_Paren_Expr;
   function As_Parent_List (Node : Ada_Node'Class) return Parent_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Parent_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Parent_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ParentList to " & Node.Kind_Name;

      end if;
   end As_Parent_List;
   function As_Pragma_Argument_Assoc
     (Node : Ada_Node'Class) return Pragma_Argument_Assoc
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Pragma_Argument_Assoc;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Pragma_Argument_Assoc_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PragmaArgumentAssoc to " & Node.Kind_Name;

      end if;
   end As_Pragma_Argument_Assoc;
   function As_Pragma_Node (Node : Ada_Node'Class) return Pragma_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Pragma_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Pragma_Node_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Pragma to " &
           Node.Kind_Name;

      end if;
   end As_Pragma_Node;
   function As_Pragma_Node_List (Node : Ada_Node'Class) return Pragma_Node_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Pragma_Node_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Pragma_Node_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Pragma.list to " & Node.Kind_Name;

      end if;
   end As_Pragma_Node_List;
   function As_Prim_Type_Accessor
     (Node : Ada_Node'Class) return Prim_Type_Accessor
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Prim_Type_Accessor;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Prim_Type_Accessor_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PrimTypeAccessor to " & Node.Kind_Name;

      end if;
   end As_Prim_Type_Accessor;
   function As_Private_Node (Node : Ada_Node'Class) return Private_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Private_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Private_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Private to " &
           Node.Kind_Name;

      end if;
   end As_Private_Node;
   function As_Private_Absent (Node : Ada_Node'Class) return Private_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Private_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Private_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Private.Absent to " & Node.Kind_Name;

      end if;
   end As_Private_Absent;
   function As_Private_Part (Node : Ada_Node'Class) return Private_Part is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Private_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Private_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PrivatePart to " & Node.Kind_Name;

      end if;
   end As_Private_Part;
   function As_Private_Present (Node : Ada_Node'Class) return Private_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Private_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Private_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Private.Present to " & Node.Kind_Name;

      end if;
   end As_Private_Present;
   function As_Private_Type_Def (Node : Ada_Node'Class) return Private_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Private_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Private_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PrivateTypeDef to " & Node.Kind_Name;

      end if;
   end As_Private_Type_Def;
   function As_Protected_Node (Node : Ada_Node'Class) return Protected_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Protected to " &
           Node.Kind_Name;

      end if;
   end As_Protected_Node;
   function As_Protected_Absent (Node : Ada_Node'Class) return Protected_Absent
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Protected.Absent to " & Node.Kind_Name;

      end if;
   end As_Protected_Absent;
   function As_Protected_Body (Node : Ada_Node'Class) return Protected_Body is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Body;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Body_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ProtectedBody to " & Node.Kind_Name;

      end if;
   end As_Protected_Body;
   function As_Protected_Body_Stub
     (Node : Ada_Node'Class) return Protected_Body_Stub
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Body_Stub;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Body_Stub_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ProtectedBodyStub to " & Node.Kind_Name;

      end if;
   end As_Protected_Body_Stub;
   function As_Protected_Def (Node : Ada_Node'Class) return Protected_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ProtectedDef to " & Node.Kind_Name;

      end if;
   end As_Protected_Def;
   function As_Protected_Present
     (Node : Ada_Node'Class) return Protected_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Protected.Present to " & Node.Kind_Name;

      end if;
   end As_Protected_Present;
   function As_Protected_Type_Decl
     (Node : Ada_Node'Class) return Protected_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Protected_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Protected_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ProtectedTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Protected_Type_Decl;
   function As_Public_Part (Node : Ada_Node'Class) return Public_Part is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Public_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Public_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " PublicPart to " & Node.Kind_Name;

      end if;
   end As_Public_Part;
   function As_Qual_Expr (Node : Ada_Node'Class) return Qual_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Qual_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Qual_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " QualExpr to " &
           Node.Kind_Name;

      end if;
   end As_Qual_Expr;
   function As_Quantified_Expr (Node : Ada_Node'Class) return Quantified_Expr
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Quantified_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Quantified_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " QuantifiedExpr to " & Node.Kind_Name;

      end if;
   end As_Quantified_Expr;
   function As_Quantifier (Node : Ada_Node'Class) return Quantifier is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Quantifier;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Quantifier then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Quantifier to " & Node.Kind_Name;

      end if;
   end As_Quantifier;
   function As_Quantifier_All (Node : Ada_Node'Class) return Quantifier_All is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Quantifier_All;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Quantifier_All_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Quantifier.All to " & Node.Kind_Name;

      end if;
   end As_Quantifier_All;
   function As_Quantifier_Some (Node : Ada_Node'Class) return Quantifier_Some
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Quantifier_Some;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Quantifier_Some_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Quantifier.Some to " & Node.Kind_Name;

      end if;
   end As_Quantifier_Some;
   function As_Raise_Expr (Node : Ada_Node'Class) return Raise_Expr is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Raise_Expr;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Raise_Expr_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " RaiseExpr to " &
           Node.Kind_Name;

      end if;
   end As_Raise_Expr;
   function As_Raise_Stmt (Node : Ada_Node'Class) return Raise_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Raise_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Raise_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " RaiseStmt to " &
           Node.Kind_Name;

      end if;
   end As_Raise_Stmt;
   function As_Range_Constraint (Node : Ada_Node'Class) return Range_Constraint
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Range_Constraint;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Range_Constraint_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RangeConstraint to " & Node.Kind_Name;

      end if;
   end As_Range_Constraint;
   function As_Range_Spec (Node : Ada_Node'Class) return Range_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Range_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Range_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " RangeSpec to " &
           Node.Kind_Name;

      end if;
   end As_Range_Spec;
   function As_Real_Literal (Node : Ada_Node'Class) return Real_Literal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Real_Literal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Real_Literal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RealLiteral to " & Node.Kind_Name;

      end if;
   end As_Real_Literal;
   function As_Record_Def (Node : Ada_Node'Class) return Record_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Record_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Record_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " RecordDef to " &
           Node.Kind_Name;

      end if;
   end As_Record_Def;
   function As_Record_Rep_Clause
     (Node : Ada_Node'Class) return Record_Rep_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Record_Rep_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Record_Rep_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RecordRepClause to " & Node.Kind_Name;

      end if;
   end As_Record_Rep_Clause;
   function As_Record_Type_Def (Node : Ada_Node'Class) return Record_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Record_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Record_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RecordTypeDef to " & Node.Kind_Name;

      end if;
   end As_Record_Type_Def;
   function As_Relation_Op (Node : Ada_Node'Class) return Relation_Op is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Relation_Op;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Relation_Op_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RelationOp to " & Node.Kind_Name;

      end if;
   end As_Relation_Op;
   function As_Renaming_Clause (Node : Ada_Node'Class) return Renaming_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Renaming_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Renaming_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RenamingClause to " & Node.Kind_Name;

      end if;
   end As_Renaming_Clause;
   function As_Requeue_Stmt (Node : Ada_Node'Class) return Requeue_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Requeue_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Requeue_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " RequeueStmt to " & Node.Kind_Name;

      end if;
   end As_Requeue_Stmt;
   function As_Return_Stmt (Node : Ada_Node'Class) return Return_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Return_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Return_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " ReturnStmt to " & Node.Kind_Name;

      end if;
   end As_Return_Stmt;
   function As_Reverse_Node (Node : Ada_Node'Class) return Reverse_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Reverse_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Reverse_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Reverse to " &
           Node.Kind_Name;

      end if;
   end As_Reverse_Node;
   function As_Reverse_Absent (Node : Ada_Node'Class) return Reverse_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Reverse_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Reverse_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Reverse.Absent to " & Node.Kind_Name;

      end if;
   end As_Reverse_Absent;
   function As_Reverse_Present (Node : Ada_Node'Class) return Reverse_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Reverse_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Reverse_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Reverse.Present to " & Node.Kind_Name;

      end if;
   end As_Reverse_Present;
   function As_Select_Stmt (Node : Ada_Node'Class) return Select_Stmt is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Select_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Select_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SelectStmt to " & Node.Kind_Name;

      end if;
   end As_Select_Stmt;
   function As_Select_When_Part (Node : Ada_Node'Class) return Select_When_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Select_When_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Select_When_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SelectWhenPart to " & Node.Kind_Name;

      end if;
   end As_Select_When_Part;
   function As_Select_When_Part_List
     (Node : Ada_Node'Class) return Select_When_Part_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Select_When_Part_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Select_When_Part_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SelectWhenPart.list to " & Node.Kind_Name;

      end if;
   end As_Select_When_Part_List;
   function As_Signed_Int_Type_Def
     (Node : Ada_Node'Class) return Signed_Int_Type_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Signed_Int_Type_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Signed_Int_Type_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SignedIntTypeDef to " & Node.Kind_Name;

      end if;
   end As_Signed_Int_Type_Def;
   function As_Single_Protected_Decl
     (Node : Ada_Node'Class) return Single_Protected_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Single_Protected_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Single_Protected_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SingleProtectedDecl to " & Node.Kind_Name;

      end if;
   end As_Single_Protected_Decl;
   function As_Single_Task_Decl (Node : Ada_Node'Class) return Single_Task_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Single_Task_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Single_Task_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SingleTaskDecl to " & Node.Kind_Name;

      end if;
   end As_Single_Task_Decl;
   function As_Task_Type_Decl (Node : Ada_Node'Class) return Task_Type_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Task_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Task_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " TaskTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Task_Type_Decl;
   function As_Single_Task_Type_Decl
     (Node : Ada_Node'Class) return Single_Task_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Single_Task_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Single_Task_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SingleTaskTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Single_Task_Type_Decl;
   function As_Stmt_List (Node : Ada_Node'Class) return Stmt_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Stmt_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Stmt_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " StmtList to " &
           Node.Kind_Name;

      end if;
   end As_Stmt_List;
   function As_String_Literal (Node : Ada_Node'Class) return String_Literal is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_String_Literal;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_String_Literal_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " StringLiteral to " & Node.Kind_Name;

      end if;
   end As_String_Literal;
   function As_Subp_Body (Node : Ada_Node'Class) return Subp_Body is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Body;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Body_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " SubpBody to " &
           Node.Kind_Name;

      end if;
   end As_Subp_Body;
   function As_Subp_Body_Stub (Node : Ada_Node'Class) return Subp_Body_Stub is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Body_Stub;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Body_Stub_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SubpBodyStub to " & Node.Kind_Name;

      end if;
   end As_Subp_Body_Stub;
   function As_Subp_Decl (Node : Ada_Node'Class) return Subp_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " SubpDecl to " &
           Node.Kind_Name;

      end if;
   end As_Subp_Decl;
   function As_Subp_Kind (Node : Ada_Node'Class) return Subp_Kind is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Kind;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Kind then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " SubpKind to " &
           Node.Kind_Name;

      end if;
   end As_Subp_Kind;
   function As_Subp_Kind_Function
     (Node : Ada_Node'Class) return Subp_Kind_Function
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Kind_Function;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Kind_Function_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SubpKind.Function to " & Node.Kind_Name;

      end if;
   end As_Subp_Kind_Function;
   function As_Subp_Kind_Procedure
     (Node : Ada_Node'Class) return Subp_Kind_Procedure
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Kind_Procedure;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Kind_Procedure_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SubpKind.Procedure to " & Node.Kind_Name;

      end if;
   end As_Subp_Kind_Procedure;
   function As_Subp_Renaming_Decl
     (Node : Ada_Node'Class) return Subp_Renaming_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Renaming_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Renaming_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SubpRenamingDecl to " & Node.Kind_Name;

      end if;
   end As_Subp_Renaming_Decl;
   function As_Subp_Spec (Node : Ada_Node'Class) return Subp_Spec is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subp_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subp_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " SubpSpec to " &
           Node.Kind_Name;

      end if;
   end As_Subp_Spec;
   function As_Subtype_Decl (Node : Ada_Node'Class) return Subtype_Decl is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subtype_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subtype_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SubtypeDecl to " & Node.Kind_Name;

      end if;
   end As_Subtype_Decl;
   function As_Subunit (Node : Ada_Node'Class) return Subunit is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Subunit;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Subunit_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Subunit to " &
           Node.Kind_Name;

      end if;
   end As_Subunit;
   function As_Synchronized_Node
     (Node : Ada_Node'Class) return Synchronized_Node
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Synchronized_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Synchronized_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Synchronized to " & Node.Kind_Name;

      end if;
   end As_Synchronized_Node;
   function As_Synchronized_Absent
     (Node : Ada_Node'Class) return Synchronized_Absent
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Synchronized_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Synchronized_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Synchronized.Absent to " & Node.Kind_Name;

      end if;
   end As_Synchronized_Absent;
   function As_Synchronized_Present
     (Node : Ada_Node'Class) return Synchronized_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Synchronized_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Synchronized_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Synchronized.Present to " & Node.Kind_Name;

      end if;
   end As_Synchronized_Present;
   function As_Synth_Anonymous_Type_Decl
     (Node : Ada_Node'Class) return Synth_Anonymous_Type_Decl
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Synth_Anonymous_Type_Decl;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Synth_Anonymous_Type_Decl_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SynthAnonymousTypeDecl to " & Node.Kind_Name;

      end if;
   end As_Synth_Anonymous_Type_Decl;
   function As_Synthetic_Renaming_Clause
     (Node : Ada_Node'Class) return Synthetic_Renaming_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Synthetic_Renaming_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Synthetic_Renaming_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " SyntheticRenamingClause to " & Node.Kind_Name;

      end if;
   end As_Synthetic_Renaming_Clause;
   function As_Tagged_Node (Node : Ada_Node'Class) return Tagged_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Tagged_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Tagged_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Tagged to " &
           Node.Kind_Name;

      end if;
   end As_Tagged_Node;
   function As_Tagged_Absent (Node : Ada_Node'Class) return Tagged_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Tagged_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Tagged_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Tagged.Absent to " & Node.Kind_Name;

      end if;
   end As_Tagged_Absent;
   function As_Tagged_Present (Node : Ada_Node'Class) return Tagged_Present is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Tagged_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Tagged_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Tagged.Present to " & Node.Kind_Name;

      end if;
   end As_Tagged_Present;
   function As_Target_Name (Node : Ada_Node'Class) return Target_Name is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Target_Name;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Target_Name_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " TargetName to " & Node.Kind_Name;

      end if;
   end As_Target_Name;
   function As_Task_Body (Node : Ada_Node'Class) return Task_Body is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Task_Body;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Task_Body_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " TaskBody to " &
           Node.Kind_Name;

      end if;
   end As_Task_Body;
   function As_Task_Body_Stub (Node : Ada_Node'Class) return Task_Body_Stub is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Task_Body_Stub;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Task_Body_Stub_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " TaskBodyStub to " & Node.Kind_Name;

      end if;
   end As_Task_Body_Stub;
   function As_Task_Def (Node : Ada_Node'Class) return Task_Def is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Task_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Task_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " TaskDef to " &
           Node.Kind_Name;

      end if;
   end As_Task_Def;
   function As_Terminate_Alternative
     (Node : Ada_Node'Class) return Terminate_Alternative
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Terminate_Alternative;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Terminate_Alternative_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " TerminateAlternative to " & Node.Kind_Name;

      end if;
   end As_Terminate_Alternative;
   function As_Type_Access_Def (Node : Ada_Node'Class) return Type_Access_Def
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Type_Access_Def;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Type_Access_Def_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " TypeAccessDef to " & Node.Kind_Name;

      end if;
   end As_Type_Access_Def;
   function As_Un_Op (Node : Ada_Node'Class) return Un_Op is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Un_Op;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Un_Op_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " UnOp to " &
           Node.Kind_Name;

      end if;
   end As_Un_Op;
   function As_Unconstrained_Array_Index
     (Node : Ada_Node'Class) return Unconstrained_Array_Index
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Unconstrained_Array_Index;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Unconstrained_Array_Index_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UnconstrainedArrayIndex to " & Node.Kind_Name;

      end if;
   end As_Unconstrained_Array_Index;
   function As_Unconstrained_Array_Index_List
     (Node : Ada_Node'Class) return Unconstrained_Array_Index_List
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Unconstrained_Array_Index_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Unconstrained_Array_Index_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UnconstrainedArrayIndex.list to " & Node.Kind_Name;

      end if;
   end As_Unconstrained_Array_Index_List;
   function As_Unconstrained_Array_Indices
     (Node : Ada_Node'Class) return Unconstrained_Array_Indices
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Unconstrained_Array_Indices;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Unconstrained_Array_Indices_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UnconstrainedArrayIndices to " & Node.Kind_Name;

      end if;
   end As_Unconstrained_Array_Indices;
   function As_Unknown_Discriminant_Part
     (Node : Ada_Node'Class) return Unknown_Discriminant_Part
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Unknown_Discriminant_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Unknown_Discriminant_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UnknownDiscriminantPart to " & Node.Kind_Name;

      end if;
   end As_Unknown_Discriminant_Part;
   function As_Until_Node (Node : Ada_Node'Class) return Until_Node is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Until_Node;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Until_Node then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Until to " &
           Node.Kind_Name;

      end if;
   end As_Until_Node;
   function As_Until_Absent (Node : Ada_Node'Class) return Until_Absent is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Until_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Until_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Until.Absent to " & Node.Kind_Name;

      end if;
   end As_Until_Absent;
   function As_Until_Present (Node : Ada_Node'Class) return Until_Present is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Until_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Until_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Until.Present to " & Node.Kind_Name;

      end if;
   end As_Until_Present;
   function As_Update_Attribute_Ref
     (Node : Ada_Node'Class) return Update_Attribute_Ref
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Update_Attribute_Ref;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Update_Attribute_Ref_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UpdateAttributeRef to " & Node.Kind_Name;

      end if;
   end As_Update_Attribute_Ref;
   function As_Use_Clause (Node : Ada_Node'Class) return Use_Clause is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Use_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Use_Clause then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " UseClause to " &
           Node.Kind_Name;

      end if;
   end As_Use_Clause;
   function As_Use_Package_Clause
     (Node : Ada_Node'Class) return Use_Package_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Use_Package_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Use_Package_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UsePackageClause to " & Node.Kind_Name;

      end if;
   end As_Use_Package_Clause;
   function As_Use_Type_Clause (Node : Ada_Node'Class) return Use_Type_Clause
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Use_Type_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Use_Type_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " UseTypeClause to " & Node.Kind_Name;

      end if;
   end As_Use_Type_Clause;
   function As_Variant (Node : Ada_Node'Class) return Variant is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Variant;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Variant_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" & " Variant to " &
           Node.Kind_Name;

      end if;
   end As_Variant;
   function As_Variant_List (Node : Ada_Node'Class) return Variant_List is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Variant_List;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Variant_List_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " Variant.list to " & Node.Kind_Name;

      end if;
   end As_Variant_List;
   function As_Variant_Part (Node : Ada_Node'Class) return Variant_Part is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_Variant_Part;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_Variant_Part_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " VariantPart to " & Node.Kind_Name;

      end if;
   end As_Variant_Part;
   function As_While_Loop_Spec (Node : Ada_Node'Class) return While_Loop_Spec
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_While_Loop_Spec;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_While_Loop_Spec_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " WhileLoopSpec to " & Node.Kind_Name;

      end if;
   end As_While_Loop_Spec;
   function As_While_Loop_Stmt (Node : Ada_Node'Class) return While_Loop_Stmt
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_While_Loop_Stmt;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_While_Loop_Stmt_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " WhileLoopStmt to " & Node.Kind_Name;

      end if;
   end As_While_Loop_Stmt;
   function As_With_Clause (Node : Ada_Node'Class) return With_Clause is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_With_Clause;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_With_Clause_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " WithClause to " & Node.Kind_Name;

      end if;
   end As_With_Clause;
   function As_With_Private (Node : Ada_Node'Class) return With_Private is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_With_Private;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_With_Private then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " WithPrivate to " & Node.Kind_Name;

      end if;
   end As_With_Private;
   function As_With_Private_Absent
     (Node : Ada_Node'Class) return With_Private_Absent
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_With_Private_Absent;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_With_Private_Absent_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " WithPrivate.Absent to " & Node.Kind_Name;

      end if;
   end As_With_Private_Absent;
   function As_With_Private_Present
     (Node : Ada_Node'Class) return With_Private_Present
   is
      N : constant Bare_Ada_Node := Node.Internal.Node;
   begin
      if N = null then
         return No_With_Private_Present;
      end if;

      Check_Safety_Net (Node.Safety_Net);

      if N.Kind in Ada_With_Private_Present_Range then

         return (Internal => (Node => N, Info => Node.Internal.Info),
            Safety_Net    => Node.Safety_Net);

      else

         raise Constraint_Error
           with "Libadalang: invalid type conversion from" &
           " WithPrivate.Present to " & Node.Kind_Name;

      end if;
   end As_With_Private_Present;

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Hash --
   ----------

   function Hash (Node : Ada_Node) return Ada.Containers.Hash_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Hash_Entity (Node.Internal);
   end Hash;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Ada_Node'Class) return Ada_Node_Kind_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Ada_Node'Class) return String is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Kind_Name (Node.Internal.Node);
   end Kind_Name;

   function To_Public_Text_Type
     (Value : Character_Type_Array_Access) return Text_Type
   is
   begin
      return Result : Text_Type (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) := Value.Items (I);
         end loop;
      end return;
   end To_Public_Text_Type;

   function To_Public_Completion_Item_Array
     (Value : Internal_Completion_Item_Array_Access)
      return Completion_Item_Array
   is
   begin
      return Result : Completion_Item_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              To_Public_Completion_Item (Value.Items (I));
         end loop;
      end return;
   end To_Public_Completion_Item_Array;

   function To_Public_Doc_Annotation_Array
     (Value : Internal_Doc_Annotation_Array_Access) return Doc_Annotation_Array
   is
   begin
      return Result : Doc_Annotation_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              To_Public_Doc_Annotation (Value.Items (I));
         end loop;
      end return;
   end To_Public_Doc_Annotation_Array;

   function To_Public_Ada_Node_Array
     (Value : Internal_Entity_Array_Access) return Ada_Node_Array
   is
   begin
      return Result : Ada_Node_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info);
         end loop;
      end return;
   end To_Public_Ada_Node_Array;

   function To_Public_Base_Formal_Param_Decl_Array
     (Value : Internal_Entity_Base_Formal_Param_Decl_Array_Access)
      return Base_Formal_Param_Decl_Array
   is
   begin
      return Result : Base_Formal_Param_Decl_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Base_Formal_Param_Decl;
         end loop;
      end return;
   end To_Public_Base_Formal_Param_Decl_Array;

   function To_Public_Base_Type_Decl_Array
     (Value : Internal_Entity_Base_Type_Decl_Array_Access)
      return Base_Type_Decl_Array
   is
   begin
      return Result : Base_Type_Decl_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Base_Type_Decl;
         end loop;
      end return;
   end To_Public_Base_Type_Decl_Array;

   function To_Public_Basic_Decl_Array
     (Value : Internal_Entity_Basic_Decl_Array_Access) return Basic_Decl_Array
   is
   begin
      return Result : Basic_Decl_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Basic_Decl;
         end loop;
      end return;
   end To_Public_Basic_Decl_Array;

   function To_Public_Compilation_Unit_Array
     (Value : Internal_Entity_Compilation_Unit_Array_Access)
      return Compilation_Unit_Array
   is
   begin
      return Result : Compilation_Unit_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Compilation_Unit;
         end loop;
      end return;
   end To_Public_Compilation_Unit_Array;

   function To_Public_Defining_Name_Array
     (Value : Internal_Entity_Defining_Name_Array_Access)
      return Defining_Name_Array
   is
   begin
      return Result : Defining_Name_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Defining_Name;
         end loop;
      end return;
   end To_Public_Defining_Name_Array;

   function To_Public_Generic_Instantiation_Array
     (Value : Internal_Entity_Generic_Instantiation_Array_Access)
      return Generic_Instantiation_Array
   is
   begin
      return Result : Generic_Instantiation_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Generic_Instantiation;
         end loop;
      end return;
   end To_Public_Generic_Instantiation_Array;

   function To_Public_Param_Spec_Array
     (Value : Internal_Entity_Param_Spec_Array_Access) return Param_Spec_Array
   is
   begin
      return Result : Param_Spec_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Param_Spec;
         end loop;
      end return;
   end To_Public_Param_Spec_Array;

   function To_Public_Type_Decl_Array
     (Value : Internal_Entity_Type_Decl_Array_Access) return Type_Decl_Array
   is
   begin
      return Result : Type_Decl_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
                .As_Type_Decl;
         end loop;
      end return;
   end To_Public_Type_Decl_Array;

   function To_Public_Param_Actual_Array
     (Value : Internal_Param_Actual_Array_Access) return Param_Actual_Array
   is
   begin
      return Result : Param_Actual_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              To_Public_Param_Actual (Value.Items (I));
         end loop;
      end return;
   end To_Public_Param_Actual_Array;

   function To_Public_Ref_Result_Array
     (Value : Internal_Ref_Result_Array_Access) return Ref_Result_Array
   is
   begin
      return Result : Ref_Result_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              To_Public_Ref_Result (Value.Items (I));
         end loop;
      end return;
   end To_Public_Ref_Result_Array;

   function To_Internal_Substitution_Array
     (Value : Substitution_Array) return Internal_Substitution_Array_Access
   is
      Result : constant Internal_Substitution_Array_Access :=
        Create_Internal_Substitution_Array (Value'Length);
   begin
      for I in Value'Range loop
         Result.Items (I - Value'First + Result.Items'First) :=
           To_Internal_Substitution (Value (I));
      end loop;
      return Result;
   end To_Internal_Substitution_Array;

   function To_Public_Analysis_Unit_Array
     (Value : Internal_Unit_Array_Access) return Analysis_Unit_Array
   is
   begin
      return Result : Analysis_Unit_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              Wrap_Unit (Value.Items (I));
         end loop;
      end return;
   end To_Public_Analysis_Unit_Array;

   function To_Internal_Analysis_Unit_Array
     (Value : Analysis_Unit_Array) return Internal_Unit_Array_Access
   is
      Result : constant Internal_Unit_Array_Access :=
        Create_Internal_Unit_Array (Value'Length);
   begin
      for I in Value'Range loop
         Result.Items (I - Value'First + Result.Items'First) :=
           Unwrap_Unit (Value (I));
      end loop;
      return Result;
   end To_Internal_Analysis_Unit_Array;

   function To_Public_Unbounded_Text_Type_Array
     (Value : Symbol_Type_Array_Access) return Unbounded_Text_Type_Array
   is
   begin
      return Result : Unbounded_Text_Type_Array (1 .. Value.N) do
         for I in Result'Range loop

            Result (I - Value.Items'First + Result'First) :=
              To_Unbounded_Text (Image (Value.Items (I)));
         end loop;
      end return;
   end To_Public_Unbounded_Text_Type_Array;

   function Exists (Self : Aspect) return Boolean is
      Record_Ref : constant Boxed_Aspect.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Exists;
   end Exists;

   function Node (Self : Aspect) return Ada_Node'Class is
      Record_Ref : constant Boxed_Aspect.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Node;
   end Node;

   function Value (Self : Aspect) return Expr'Class is
      Record_Ref : constant Boxed_Aspect.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Value;
   end Value;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Aspect_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Aspect_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Aspect (Value : Internal_Aspect) return Aspect is
      Result     : constant Aspect := Aspect (Boxed_Aspect.Create_Element);
      Record_Ref : constant Boxed_Aspect.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Exists := Value.Exists;

      Record_Ref.Internal_Node := Wrap_Node (Value.Node.Node, Value.Node.Info);

      Record_Ref.Internal_Value :=
        Wrap_Node (Value.Value.Node, Value.Value.Info).As_Expr;
      return Result;
   end To_Public_Aspect;

   function Create_Aspect
     (Exists : Boolean; Node : Ada_Node'Class; Value : Expr'Class)
      return Aspect
   is
      Result     : constant Aspect := Aspect (Boxed_Aspect.Create_Element);
      Record_Def : constant Boxed_Aspect.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Exists := Exists;

      Record_Def.Internal_Node := Node.As_Ada_Node;

      Record_Def.Internal_Value := Value.As_Expr;
      return Result;
   end Create_Aspect;

   function Decl (Self : Completion_Item) return Basic_Decl'Class is
      Record_Ref : constant Boxed_Completion_Item.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Decl;
   end Decl;

   function Is_Dot_Call (Self : Completion_Item) return Boolean is
      Record_Ref : constant Boxed_Completion_Item.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Is_Dot_Call;
   end Is_Dot_Call;

   --------------
   -- Refcount --
   --------------

   function Refcount
     (Self : Internal_Completion_Item_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Completion_Item_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Completion_Item
     (Value : Internal_Completion_Item) return Completion_Item
   is
      Result : constant Completion_Item :=
        Completion_Item (Boxed_Completion_Item.Create_Element);
      Record_Ref : constant Boxed_Completion_Item.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Decl :=
        Wrap_Node (Value.Decl.Node, Value.Decl.Info).As_Basic_Decl;

      Record_Ref.Internal_Is_Dot_Call := Value.Is_Dot_Call;
      return Result;
   end To_Public_Completion_Item;

   function Create_Completion_Item
     (Decl : Basic_Decl'Class; Is_Dot_Call : Boolean) return Completion_Item
   is
      Result : constant Completion_Item :=
        Completion_Item (Boxed_Completion_Item.Create_Element);
      Record_Def : constant Boxed_Completion_Item.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Decl := Decl.As_Basic_Decl;

      Record_Def.Internal_Is_Dot_Call := Is_Dot_Call;
      return Result;
   end Create_Completion_Item;

   function Low_Bound (Self : Discrete_Range) return Expr'Class is
      Record_Ref : constant Boxed_Discrete_Range.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Low_Bound;
   end Low_Bound;

   function High_Bound (Self : Discrete_Range) return Expr'Class is
      Record_Ref : constant Boxed_Discrete_Range.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_High_Bound;
   end High_Bound;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Discrete_Range_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Discrete_Range_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Discrete_Range
     (Value : Internal_Discrete_Range) return Discrete_Range
   is
      Result : constant Discrete_Range :=
        Discrete_Range (Boxed_Discrete_Range.Create_Element);
      Record_Ref : constant Boxed_Discrete_Range.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Low_Bound :=
        Wrap_Node (Value.Low_Bound.Node, Value.Low_Bound.Info).As_Expr;

      Record_Ref.Internal_High_Bound :=
        Wrap_Node (Value.High_Bound.Node, Value.High_Bound.Info).As_Expr;
      return Result;
   end To_Public_Discrete_Range;

   function Create_Discrete_Range
     (Low_Bound : Expr'Class; High_Bound : Expr'Class) return Discrete_Range
   is
      Result : constant Discrete_Range :=
        Discrete_Range (Boxed_Discrete_Range.Create_Element);
      Record_Def : constant Boxed_Discrete_Range.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Low_Bound := Low_Bound.As_Expr;

      Record_Def.Internal_High_Bound := High_Bound.As_Expr;
      return Result;
   end Create_Discrete_Range;

   function Key (Self : Doc_Annotation) return Text_Type is
      Record_Ref : constant Boxed_Doc_Annotation.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Key.all;
   end Key;

   function Value (Self : Doc_Annotation) return Text_Type is
      Record_Ref : constant Boxed_Doc_Annotation.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Value.all;
   end Value;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Doc_Annotation_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Doc_Annotation_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Internal_Doc_Annotation_Record) is

   begin
      Free (Self.Internal_Key);
      Free (Self.Internal_Value);
   end Release;

   function To_Public_Doc_Annotation
     (Value : Internal_Doc_Annotation) return Doc_Annotation
   is
      Result : constant Doc_Annotation :=
        Doc_Annotation (Boxed_Doc_Annotation.Create_Element);
      Record_Ref : constant Boxed_Doc_Annotation.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Key :=
        new Text_Type'(To_Public_Text_Type (Value.Key));

      Record_Ref.Internal_Value :=
        new Text_Type'(To_Public_Text_Type (Value.Value));
      return Result;
   end To_Public_Doc_Annotation;

   function Create_Doc_Annotation
     (Key : Text_Type; Value : Text_Type) return Doc_Annotation
   is
      Result : constant Doc_Annotation :=
        Doc_Annotation (Boxed_Doc_Annotation.Create_Element);
      Record_Def : constant Boxed_Doc_Annotation.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Key := new Text_Type'(Key);

      Record_Def.Internal_Value := new Text_Type'(Value);
      return Result;
   end Create_Doc_Annotation;

   function Param (Self : Param_Actual) return Defining_Name'Class is
      Record_Ref : constant Boxed_Param_Actual.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Param;
   end Param;

   function Actual (Self : Param_Actual) return Expr'Class is
      Record_Ref : constant Boxed_Param_Actual.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Actual;
   end Actual;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Param_Actual_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Param_Actual_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Param_Actual
     (Value : Internal_Param_Actual) return Param_Actual
   is
      Result : constant Param_Actual :=
        Param_Actual (Boxed_Param_Actual.Create_Element);
      Record_Ref : constant Boxed_Param_Actual.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Param :=
        Wrap_Node (Value.Param.Node, Value.Param.Info).As_Defining_Name;

      Record_Ref.Internal_Actual :=
        Wrap_Node (Value.Actual.Node, Value.Actual.Info).As_Expr;
      return Result;
   end To_Public_Param_Actual;

   function Create_Param_Actual
     (Param : Defining_Name'Class; Actual : Expr'Class) return Param_Actual
   is
      Result : constant Param_Actual :=
        Param_Actual (Boxed_Param_Actual.Create_Element);
      Record_Def : constant Boxed_Param_Actual.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Param := Param.As_Defining_Name;

      Record_Def.Internal_Actual := Actual.As_Expr;
      return Result;
   end Create_Param_Actual;

   function Ref (Self : Ref_Result) return Base_Id'Class is
      Record_Ref : constant Boxed_Ref_Result.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Ref;
   end Ref;

   function Kind (Self : Ref_Result) return Ref_Result_Kind is
      Record_Ref : constant Boxed_Ref_Result.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Kind;
   end Kind;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Ref_Result_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Ref_Result_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Ref_Result
     (Value : Internal_Ref_Result) return Ref_Result
   is
      Result : constant Ref_Result :=
        Ref_Result (Boxed_Ref_Result.Create_Element);
      Record_Ref : constant Boxed_Ref_Result.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Ref :=
        Wrap_Node (Value.Ref.Node, Value.Ref.Info).As_Base_Id;

      Record_Ref.Internal_Kind := Value.Kind;
      return Result;
   end To_Public_Ref_Result;

   function Create_Ref_Result
     (Ref : Base_Id'Class; Kind : Ref_Result_Kind) return Ref_Result
   is
      Result : constant Ref_Result :=
        Ref_Result (Boxed_Ref_Result.Create_Element);
      Record_Def : constant Boxed_Ref_Result.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Ref := Ref.As_Base_Id;

      Record_Def.Internal_Kind := Kind;
      return Result;
   end Create_Ref_Result;

   function Decl (Self : Refd_Decl) return Basic_Decl'Class is
      Record_Ref : constant Boxed_Refd_Decl.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Decl;
   end Decl;

   function Kind (Self : Refd_Decl) return Ref_Result_Kind is
      Record_Ref : constant Boxed_Refd_Decl.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Kind;
   end Kind;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Refd_Decl_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Refd_Decl_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Refd_Decl (Value : Internal_Refd_Decl) return Refd_Decl
   is
      Result : constant Refd_Decl :=
        Refd_Decl (Boxed_Refd_Decl.Create_Element);
      Record_Ref : constant Boxed_Refd_Decl.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Decl :=
        Wrap_Node (Value.Decl.Node, Value.Decl.Info).As_Basic_Decl;

      Record_Ref.Internal_Kind := Value.Kind;
      return Result;
   end To_Public_Refd_Decl;

   function Create_Refd_Decl
     (Decl : Basic_Decl'Class; Kind : Ref_Result_Kind) return Refd_Decl
   is
      Result : constant Refd_Decl :=
        Refd_Decl (Boxed_Refd_Decl.Create_Element);
      Record_Def : constant Boxed_Refd_Decl.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Decl := Decl.As_Basic_Decl;

      Record_Def.Internal_Kind := Kind;
      return Result;
   end Create_Refd_Decl;

   function Def_Name (Self : Refd_Def) return Defining_Name'Class is
      Record_Ref : constant Boxed_Refd_Def.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Def_Name;
   end Def_Name;

   function Kind (Self : Refd_Def) return Ref_Result_Kind is
      Record_Ref : constant Boxed_Refd_Def.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Kind;
   end Kind;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Refd_Def_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Refd_Def_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Public_Refd_Def (Value : Internal_Refd_Def) return Refd_Def is
      Result : constant Refd_Def := Refd_Def (Boxed_Refd_Def.Create_Element);
      Record_Ref : constant Boxed_Refd_Def.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Ref.Internal_Def_Name :=
        Wrap_Node (Value.Def_Name.Node, Value.Def_Name.Info).As_Defining_Name;

      Record_Ref.Internal_Kind := Value.Kind;
      return Result;
   end To_Public_Refd_Def;

   function Create_Refd_Def
     (Def_Name : Defining_Name'Class; Kind : Ref_Result_Kind) return Refd_Def
   is
      Result : constant Refd_Def := Refd_Def (Boxed_Refd_Def.Create_Element);
      Record_Def : constant Boxed_Refd_Def.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_Def_Name := Def_Name.As_Defining_Name;

      Record_Def.Internal_Kind := Kind;
      return Result;
   end Create_Refd_Def;

   function From_Decl (Self : Substitution) return Basic_Decl'Class is
      Record_Ref : constant Boxed_Substitution.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_From_Decl;
   end From_Decl;

   function To_Value (Self : Substitution) return Big_Integer is
      Record_Ref : constant Boxed_Substitution.Element_Access :=
        Internal_Access (Self);
   begin
      return Result : Big_Integer do
         Result.Set (Record_Ref.Internal_To_Value);
      end return;
   end To_Value;

   function Value_Type (Self : Substitution) return Base_Type_Decl'Class is
      Record_Ref : constant Boxed_Substitution.Element_Access :=
        Internal_Access (Self);
   begin
      return Record_Ref.Internal_Value_Type;
   end Value_Type;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Substitution_Record) return Positive is
     (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Substitution_Record; Count : Positive)
   is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   function To_Internal_Substitution
     (Value : Substitution) return Internal_Substitution
   is
      Record_Ref : constant Boxed_Substitution.Element_Access :=
        Internal_Access (Value);
      Result : Internal_Substitution;
   begin

      Result.From_Decl :=
        (Record_Ref.Internal_From_Decl.Internal.Node,
         Record_Ref.Internal_From_Decl.Internal.Info);

      Result.To_Value := Create_Big_Integer (Record_Ref.Internal_To_Value);

      Result.Value_Type :=
        (Record_Ref.Internal_Value_Type.Internal.Node,
         Record_Ref.Internal_Value_Type.Internal.Info);
      return Result;
   end To_Internal_Substitution;

   function Create_Substitution
     (From_Decl  : Basic_Decl'Class; To_Value : Big_Integer;
      Value_Type : Base_Type_Decl'Class) return Substitution
   is
      Result : constant Substitution :=
        Substitution (Boxed_Substitution.Create_Element);
      Record_Def : constant Boxed_Substitution.Element_Access :=
        Internal_Access (Result);
   begin

      Record_Def.Internal_From_Decl := From_Decl.As_Basic_Decl;

      Record_Def.Internal_To_Value.Set (To_Value);

      Record_Def.Internal_Value_Type := Value_Type.As_Base_Type_Decl;
      return Result;
   end Create_Substitution;

   function P_Declarative_Scope (Node : Ada_Node'Class) return Declarative_Part
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Bare_Declarative_Part :=
           Libadalang.Implementation.Ada_Node_P_Declarative_Scope
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result, No_Entity_Info)
             .As_Declarative_Part;
      end;
   end P_Declarative_Scope;

   function P_Complete (Node : Ada_Node'Class) return Completion_Item_Array is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Completion_Item_Array_Access :=
           Libadalang.Implementation.Dispatcher_Ada_Node_P_Complete
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Completion_Item_Array :=
             To_Public_Completion_Item_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Complete;

   function P_Valid_Keywords
     (Node : Ada_Node'Class) return Unbounded_Text_Type_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Symbol_Type_Array_Access :=
           Libadalang.Implementation.Ada_Node_P_Valid_Keywords
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Unbounded_Text_Type_Array :=
             To_Public_Unbounded_Text_Type_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Valid_Keywords;

   function P_Generic_Instantiations
     (Node : Ada_Node'Class) return Generic_Instantiation_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Generic_Instantiation_Array_Access :=
           Libadalang.Implementation.Ada_Node_P_Generic_Instantiations
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Generic_Instantiation_Array :=
             To_Public_Generic_Instantiation_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Generic_Instantiations;

   function P_Semantic_Parent (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Ada_Node_P_Semantic_Parent
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Semantic_Parent;

   function P_Parent_Basic_Decl (Node : Ada_Node'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Ada_Node_P_Parent_Basic_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Parent_Basic_Decl;

   function P_Filter_Is_Imported_By
     (Node : Ada_Node'Class; Units : Analysis_Unit_Array; Transitive : Boolean)
      return Analysis_Unit_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Units : Internal_Unit_Array_Access :=
           To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Transitive : constant Boolean := Transitive;

         Property_Result : Internal_Unit_Array_Access :=
           Libadalang.Implementation.Extensions
             .Ada_Node_P_Filter_Is_Imported_By
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units,
              Internal_Arg_Transitive);
      begin

         return
           Result : constant Analysis_Unit_Array :=
             To_Public_Analysis_Unit_Array (Property_Result) do
            Dec_Ref (Internal_Arg_Units);
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Filter_Is_Imported_By;

   function P_Xref_Entry_Point (Node : Ada_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Ada_Node_P_Xref_Entry_Point
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Xref_Entry_Point;

   function P_Resolve_Names (Node : Ada_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Ada_Node_P_Resolve_Names
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Resolve_Names;

   function P_Standard_Unit (Node : Ada_Node'Class) return Analysis_Unit is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Unit :=
           Libadalang.Implementation.Extensions.Ada_Node_P_Standard_Unit
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Unit (Property_Result);
      end;
   end P_Standard_Unit;

   function P_Std_Entity
     (Node : Ada_Node'Class; Sym : Unbounded_Text_Type) return Ada_Node
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Sym : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Sym));

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Ada_Node_P_Std_Entity
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Sym);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Std_Entity;

   function P_Bool_Type (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Ada_Node_P_Bool_Type
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Bool_Type;

   function P_Int_Type (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Ada_Node_P_Int_Type
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Int_Type;

   function P_Universal_Int_Type (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Ada_Node_P_Universal_Int_Type
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Universal_Int_Type;

   function P_Universal_Real_Type (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Ada_Node_P_Universal_Real_Type
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Universal_Real_Type;

   function P_Top_Level_Decl
     (Node : Ada_Node'Class; Unit : Analysis_Unit'Class) return Basic_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Unit : constant Internal_Unit := Unwrap_Unit (Unit);

         Property_Result : constant Bare_Basic_Decl :=
           Libadalang.Implementation.Ada_Node_P_Top_Level_Decl
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Unit);
      begin

         return Wrap_Node (Property_Result, No_Entity_Info).As_Basic_Decl;
      end;
   end P_Top_Level_Decl;

   function P_Choice_Match
     (Node : Ada_Node'Class; Value : Big_Integer) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Value : Big_Integer_Type := Create_Big_Integer (Value);

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Ada_Node_P_Choice_Match
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Value,
              E_Info => Node.Internal.Info);
      begin

         return Result : constant Boolean := Property_Result do
            Dec_Ref (Internal_Arg_Value);
         end return;
      end;
   end P_Choice_Match;

   function P_Gnat_Xref
     (Node : Ada_Node'Class; Imprecise_Fallback : Boolean := False)
      return Defining_Name
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Ada_Node_P_Gnat_Xref
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Gnat_Xref;

   function Parent (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Parent
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end Parent;

   function Parents (Node : Ada_Node'Class) return Ada_Node_Array is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Array_Access :=
           Libadalang.Implementation.Parents
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ada_Node_Array :=
             To_Public_Ada_Node_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end Parents;

   function Children (Node : Ada_Node'Class) return Ada_Node_Array is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Array_Access :=
           Libadalang.Implementation.Children
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ada_Node_Array :=
             To_Public_Ada_Node_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end Children;

   function Token_Start (Node : Ada_Node'Class) return Token_Reference is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Token_Reference :=
           Libadalang.Implementation.Token_Start
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end Token_Start;

   function Token_End (Node : Ada_Node'Class) return Token_Reference is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Token_Reference :=
           Libadalang.Implementation.Token_End
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end Token_End;

   function Child_Index (Node : Ada_Node'Class) return Integer is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Integer :=
           Libadalang.Implementation.Child_Index
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end Child_Index;

   function Previous_Sibling (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Previous_Sibling
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end Previous_Sibling;

   function Next_Sibling (Node : Ada_Node'Class) return Ada_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation.Next_Sibling
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end Next_Sibling;

   function Unit (Node : Ada_Node'Class) return Analysis_Unit is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Unit :=
           Libadalang.Implementation.Unit (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Unit (Property_Result);
      end;
   end Unit;

   function Is_Ghost (Node : Ada_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Is_Ghost
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end Is_Ghost;

   function Full_Sloc_Image (Node : Ada_Node'Class) return Text_Type is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Character_Type_Array_Access :=
           Libadalang.Implementation.Full_Sloc_Image
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Text_Type := To_Public_Text_Type (Property_Result)
         do
            Dec_Ref (Property_Result);
         end return;
      end;
   end Full_Sloc_Image;

   function P_Expression_Type (Node : Expr'Class) return Base_Type_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Expr_P_Expression_Type
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Expression_Type;

   function P_Is_Static_Expr
     (Node : Expr'Class; Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Expr_P_Is_Static_Expr
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Static_Expr;

   function P_First_Corresponding_Decl (Node : Expr'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Dispatcher_Expr_P_First_Corresponding_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_First_Corresponding_Decl;

   function P_Eval_As_Int (Node : Expr'Class) return Big_Integer is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Big_Integer_Type :=
           Libadalang.Implementation.Expr_P_Eval_As_Int
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Big_Integer :=
             Create_Public_Big_Integer (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Eval_As_Int;

   function P_Eval_As_Int_In_Env
     (Node : Expr'Class; Env : Substitution_Array) return Big_Integer
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Env : Internal_Substitution_Array_Access :=
           To_Internal_Substitution_Array (Env);

         Property_Result : Big_Integer_Type :=
           Libadalang.Implementation.Extensions.Expr_P_Eval_As_Int_In_Env
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Env,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Big_Integer :=
             Create_Public_Big_Integer (Property_Result) do
            Dec_Ref (Internal_Arg_Env);
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Eval_As_Int_In_Env;

   function P_Matching_Nodes (Node : Expr'Class) return Ada_Node_Array is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Array_Access :=
           Libadalang.Implementation.Expr_P_Matching_Nodes
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ada_Node_Array :=
             To_Public_Ada_Node_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Matching_Nodes;

   function F_Aspects (Node : Basic_Decl'Class) return Aspect_Spec is
      Result : Bare_Aspect_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Basic_Decl_F_Aspects (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Aspects;

   function P_Is_Formal (Node : Basic_Decl'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Basic_Decl_P_Is_Formal
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Is_Formal;

   function P_Doc_Annotations
     (Node : Basic_Decl'Class) return Doc_Annotation_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Doc_Annotation_Array_Access :=
           Libadalang.Implementation.Extensions.Basic_Decl_P_Doc_Annotations
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Doc_Annotation_Array :=
             To_Public_Doc_Annotation_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Doc_Annotations;

   function P_Doc (Node : Basic_Decl'Class) return Text_Type is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Character_Type_Array_Access :=
           Libadalang.Implementation.Extensions.Basic_Decl_P_Doc
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Text_Type := To_Public_Text_Type (Property_Result)
         do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Doc;

   function P_Previous_Part_For_Decl
     (Node : Basic_Decl'Class) return Basic_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation
             .Dispatcher_Basic_Decl_P_Previous_Part_For_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Previous_Part_For_Decl;

   function P_Canonical_Part (Node : Basic_Decl'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Basic_Decl_P_Canonical_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Canonical_Part;

   function P_Is_Static_Decl
     (Node : Basic_Decl'Class; Imprecise_Fallback : Boolean := False)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Static_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Static_Decl;

   function P_Is_Imported (Node : Basic_Decl'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Imported
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Imported;

   function P_Get_Aspect_Assoc
     (Node : Basic_Decl'Class; Name : Unbounded_Text_Type) return Aspect_Assoc
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Name : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

         Property_Result : constant Internal_Entity_Aspect_Assoc :=
           Libadalang.Implementation.Dispatcher_Basic_Decl_P_Get_Aspect_Assoc
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Aspect_Assoc;
      end;
   end P_Get_Aspect_Assoc;

   function P_Get_Aspect_Spec_Expr
     (Node : Basic_Decl'Class; Name : Unbounded_Text_Type) return Expr
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Name : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

         Property_Result : constant Internal_Entity_Expr :=
           Libadalang.Implementation.Basic_Decl_P_Get_Aspect_Spec_Expr
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Expr;
      end;
   end P_Get_Aspect_Spec_Expr;

   function P_Get_Aspect
     (Node               : Basic_Decl'Class; Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Aspect
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Name : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Aspect :=
           Libadalang.Implementation.Basic_Decl_P_Get_Aspect
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return To_Public_Aspect (Property_Result);
      end;
   end P_Get_Aspect;

   function P_Has_Aspect
     (Node               : Basic_Decl'Class; Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Name : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Basic_Decl_P_Has_Aspect
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Has_Aspect;

   function P_Get_Pragma
     (Node : Basic_Decl'Class; Name : Unbounded_Text_Type) return Pragma_Node
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Name : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

         Property_Result : constant Internal_Entity_Pragma_Node :=
           Libadalang.Implementation.Basic_Decl_P_Get_Pragma
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Pragma_Node;
      end;
   end P_Get_Pragma;

   function P_Get_Representation_Clause
     (Node               : Basic_Decl'Class; Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Attribute_Def_Clause
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Name : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Entity_Attribute_Def_Clause :=
           Libadalang.Implementation.Basic_Decl_P_Get_Representation_Clause
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Attribute_Def_Clause;
      end;
   end P_Get_Representation_Clause;

   function P_Is_Compilation_Unit_Root (Node : Basic_Decl'Class) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Basic_Decl_P_Is_Compilation_Unit_Root
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Is_Compilation_Unit_Root;

   function P_Is_Visible
     (Node : Basic_Decl'Class; From_Node : Ada_Node'Class) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_From_Node : constant Internal_Entity :=
           (From_Node.Internal.Node, From_Node.Internal.Info);

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Basic_Decl_P_Is_Visible
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_From_Node,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Visible;

   function P_Base_Subp_Declarations
     (Node : Basic_Decl'Class) return Basic_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Basic_Decl_Array_Access :=
           Libadalang.Implementation.Basic_Decl_P_Base_Subp_Declarations
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Basic_Decl_Array :=
             To_Public_Basic_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Base_Subp_Declarations;

   function P_Root_Subp_Declarations
     (Node : Basic_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Basic_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : Internal_Entity_Basic_Decl_Array_Access :=
           Libadalang.Implementation.Basic_Decl_P_Root_Subp_Declarations
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Basic_Decl_Array :=
             To_Public_Basic_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Root_Subp_Declarations;

   function P_Find_All_Overrides
     (Node               : Basic_Decl'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Basic_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Units : Internal_Unit_Array_Access :=
           To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : Internal_Entity_Basic_Decl_Array_Access :=
           Libadalang.Implementation.Basic_Decl_P_Find_All_Overrides
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Basic_Decl_Array :=
             To_Public_Basic_Decl_Array (Property_Result) do
            Dec_Ref (Internal_Arg_Units);
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Find_All_Overrides;

   function P_Defining_Names
     (Node : Basic_Decl'Class) return Defining_Name_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Defining_Name_Array_Access :=
           Libadalang.Implementation.Dispatcher_Basic_Decl_P_Defining_Names
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Defining_Name_Array :=
             To_Public_Defining_Name_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Defining_Names;

   function P_Defining_Name (Node : Basic_Decl'Class) return Defining_Name is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Basic_Decl_P_Defining_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Defining_Name;

   function P_Type_Expression (Node : Basic_Decl'Class) return Type_Expr is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Type_Expr :=
           Libadalang.Implementation.Dispatcher_Basic_Decl_P_Type_Expression
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Type_Expr;
      end;
   end P_Type_Expression;

   function P_Subp_Spec_Or_Null
     (Node : Basic_Decl'Class; Follow_Generic : Boolean := False)
      return Base_Subp_Spec
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Follow_Generic : constant Boolean := Follow_Generic;

         Property_Result : constant Internal_Entity_Base_Subp_Spec :=
           Libadalang.Implementation.Basic_Decl_P_Subp_Spec_Or_Null
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Follow_Generic,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Subp_Spec;
      end;
   end P_Subp_Spec_Or_Null;

   function P_Is_Subprogram (Node : Basic_Decl'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Basic_Decl_P_Is_Subprogram
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Is_Subprogram;

   function P_Relative_Name (Node : Basic_Decl'Class) return Single_Tok_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Single_Tok_Node :=
           Libadalang.Implementation.Basic_Decl_P_Relative_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Single_Tok_Node;
      end;
   end P_Relative_Name;

   function P_Relative_Name_Text
     (Node : Basic_Decl'Class) return Unbounded_Text_Type
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Symbol_Type :=
           Libadalang.Implementation.Basic_Decl_P_Relative_Name_Text
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return To_Unbounded_Text (Image (Property_Result));
      end;
   end P_Relative_Name_Text;

   function P_Next_Part_For_Decl (Node : Basic_Decl'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Dispatcher_Basic_Decl_P_Next_Part_For_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Next_Part_For_Decl;

   function P_Body_Part_For_Decl (Node : Basic_Decl'Class) return Body_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Body_Node :=
           Libadalang.Implementation.Basic_Decl_P_Body_Part_For_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Body_Node;
      end;
   end P_Body_Part_For_Decl;

   function P_Fully_Qualified_Name_Array
     (Node : Basic_Decl'Class) return Unbounded_Text_Type_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Symbol_Type_Array_Access :=
           Libadalang.Implementation.Basic_Decl_P_Fully_Qualified_Name_Array
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Unbounded_Text_Type_Array :=
             To_Public_Unbounded_Text_Type_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Fully_Qualified_Name_Array;

   function P_Fully_Qualified_Name (Node : Basic_Decl'Class) return Text_Type
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Character_Type_Array_Access :=
           Libadalang.Implementation.Basic_Decl_P_Fully_Qualified_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Text_Type := To_Public_Text_Type (Property_Result)
         do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Fully_Qualified_Name;

   function P_Canonical_Fully_Qualified_Name
     (Node : Basic_Decl'Class) return Text_Type
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Character_Type_Array_Access :=
           Libadalang.Implementation
             .Basic_Decl_P_Canonical_Fully_Qualified_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Text_Type := To_Public_Text_Type (Property_Result)
         do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Canonical_Fully_Qualified_Name;

   function P_Unique_Identifying_Name
     (Node : Basic_Decl'Class) return Text_Type
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Character_Type_Array_Access :=
           Libadalang.Implementation.Basic_Decl_P_Unique_Identifying_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Text_Type := To_Public_Text_Type (Property_Result)
         do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Unique_Identifying_Name;

   function P_As_Bool (Node : Abort_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Abort_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Names (Node : Abort_Stmt'Class) return Name_List is
      Result : Bare_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Abort_Stmt_F_Names (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Names;

   function P_As_Bool (Node : Abstract_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Abstract_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function P_Subp_Decl_Spec
     (Node : Basic_Subp_Decl'Class) return Base_Subp_Spec
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Subp_Spec :=
           Libadalang.Implementation
             .Dispatcher_Basic_Subp_Decl_P_Subp_Decl_Spec
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Subp_Spec;
      end;
   end P_Subp_Decl_Spec;

   function P_Body_Part (Node : Basic_Subp_Decl'Class) return Base_Subp_Body is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Subp_Body :=
           Libadalang.Implementation.Basic_Subp_Decl_P_Body_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Subp_Body;
      end;
   end P_Body_Part;

   function F_Overriding
     (Node : Classic_Subp_Decl'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Classic_Subp_Decl_F_Overriding (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Overriding;

   function F_Overriding
     (Node : Classic_Subp_Decl'Class) return Ada_Overriding_Node is
     (Overriding_Node'(Node.F_Overriding).Kind);

   function F_Subp_Spec (Node : Classic_Subp_Decl'Class) return Subp_Spec is
      Result : Bare_Subp_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Classic_Subp_Decl_F_Subp_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Spec;

   function F_Default_Expr (Node : Formal_Subp_Decl'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Formal_Subp_Decl_F_Default_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Default_Expr;

   function F_Name (Node : Accept_Stmt'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Accept_Stmt_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Entry_Index_Expr (Node : Accept_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Accept_Stmt_F_Entry_Index_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Entry_Index_Expr;

   function F_Params
     (Node : Accept_Stmt'Class) return Entry_Completion_Formal_Params
   is
      Result : Bare_Entry_Completion_Formal_Params;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Accept_Stmt_F_Params (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Params;

   function F_Stmts (Node : Accept_Stmt_With_Stmts'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Accept_Stmt_With_Stmts_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Accept_Stmt_With_Stmts'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Accept_Stmt_With_Stmts_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Has_Not_Null (Node : Access_Def'Class) return Not_Null is
      Result : Bare_Not_Null;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Access_Def_F_Has_Not_Null (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Not_Null;

   function F_Has_Not_Null (Node : Access_Def'Class) return Boolean is
     (Not_Null'(Node.F_Has_Not_Null).Kind = Ada_Not_Null_Present);

   function F_Has_Protected
     (Node : Access_To_Subp_Def'Class) return Protected_Node
   is
      Result : Bare_Protected_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Access_To_Subp_Def_F_Has_Protected (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Protected;

   function F_Has_Protected (Node : Access_To_Subp_Def'Class) return Boolean is
     (Protected_Node'(Node.F_Has_Protected).Kind = Ada_Protected_Present);

   function F_Subp_Spec (Node : Access_To_Subp_Def'Class) return Subp_Spec is
      Result : Bare_Subp_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Access_To_Subp_Def_F_Subp_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Spec;

   function Ada_Node_List_First (Node : Ada_Node_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Ada_Node_List_First;

   function Ada_Node_List_Next
     (Node : Ada_Node_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Ada_Node_List_Next;

   function Ada_Node_List_Has_Element
     (Node : Ada_Node_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Ada_Node_List_Has_Element;

   function Ada_Node_List_Element
     (Node : Ada_Node_List; Cursor : Positive) return Ada_Node'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Ada_Node'(Child.As_Ada_Node);
   end Ada_Node_List_Element;

   function F_Ancestor_Expr (Node : Base_Aggregate'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Aggregate_F_Ancestor_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ancestor_Expr;

   function F_Assocs (Node : Base_Aggregate'Class) return Assoc_List is
      Result : Bare_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Base_Aggregate_F_Assocs (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Assocs;

   function P_Get_Params
     (Node : Basic_Assoc'Class; Imprecise_Fallback : Boolean := False)
      return Defining_Name_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : Internal_Entity_Defining_Name_Array_Access :=
           Libadalang.Implementation.Basic_Assoc_P_Get_Params
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Defining_Name_Array :=
             To_Public_Defining_Name_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Get_Params;

   function F_Designators
     (Node : Aggregate_Assoc'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Aggregate_Assoc_F_Designators (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Designators;

   function F_R_Expr (Node : Aggregate_Assoc'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Aggregate_Assoc_F_R_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_R_Expr;

   function P_As_Bool (Node : Aliased_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Aliased_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function P_As_Bool (Node : All_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_All_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Subpool (Node : Allocator'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Allocator_F_Subpool (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subpool;

   function F_Type_Or_Expr (Node : Allocator'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Allocator_F_Type_Or_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Or_Expr;

   function P_Get_Allocated_Type (Node : Allocator'Class) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Allocator_P_Get_Allocated_Type
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Get_Allocated_Type;

   function F_Ids (Node : Object_Decl'Class) return Defining_Name_List is
      Result : Bare_Defining_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Object_Decl_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Has_Aliased (Node : Object_Decl'Class) return Aliased_Node is
      Result : Bare_Aliased_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Object_Decl_F_Has_Aliased (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Aliased;

   function F_Has_Aliased (Node : Object_Decl'Class) return Boolean is
     (Aliased_Node'(Node.F_Has_Aliased).Kind = Ada_Aliased_Present);

   function F_Has_Constant (Node : Object_Decl'Class) return Constant_Node is
      Result : Bare_Constant_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Object_Decl_F_Has_Constant (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Constant;

   function F_Has_Constant (Node : Object_Decl'Class) return Boolean is
     (Constant_Node'(Node.F_Has_Constant).Kind = Ada_Constant_Present);

   function F_Mode (Node : Object_Decl'Class) return Mode is
      Result : Bare_Mode;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Object_Decl_F_Mode (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Mode;

   function F_Mode (Node : Object_Decl'Class) return Ada_Mode is
     (Mode'(Node.F_Mode).Kind);

   function F_Type_Expr (Node : Object_Decl'Class) return Type_Expr is
      Result : Bare_Type_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Object_Decl_F_Type_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Expr;

   function F_Default_Expr (Node : Object_Decl'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Object_Decl_F_Default_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Default_Expr;

   function F_Renaming_Clause (Node : Object_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Object_Decl_F_Renaming_Clause (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renaming_Clause;

   function P_Public_Part_Decl (Node : Object_Decl'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Object_Decl_P_Public_Part_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Public_Part_Decl;

   function P_Type_Name (Node : Type_Expr'Class) return Name is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Name :=
           Libadalang.Implementation.Type_Expr_P_Type_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Name;
      end;
   end P_Type_Name;

   function P_Designated_Type_Decl
     (Node : Type_Expr'Class) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Type_Expr_P_Designated_Type_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Designated_Type_Decl;

   function P_Designated_Type_Decl_From
     (Node : Type_Expr'Class; Origin_Node : Ada_Node'Class)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin_Node : constant Internal_Entity :=
           (Origin_Node.Internal.Node, Origin_Node.Internal.Info);

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Type_Expr_P_Designated_Type_Decl_From
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin_Node,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Designated_Type_Decl_From;

   function F_Type_Decl
     (Node : Anonymous_Type'Class) return Anonymous_Type_Decl
   is
      Result : Bare_Anonymous_Type_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Anonymous_Type_F_Type_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Decl;

   function F_Type_Decl
     (Node : Anonymous_Type_Access_Def'Class) return Base_Type_Decl
   is
      Result : Bare_Base_Type_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Anonymous_Type_Access_Def_F_Type_Decl
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Decl;

   function F_Name (Node : Base_Type_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Base_Type_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function P_Base_Subtype
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Type_Decl_P_Base_Subtype
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Base_Subtype;

   function P_Private_Completion
     (Node : Base_Type_Decl'Class) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Type_Decl_P_Private_Completion
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Private_Completion;

   function P_Get_Record_Representation_Clause
     (Node : Base_Type_Decl'Class; Imprecise_Fallback : Boolean := False)
      return Record_Rep_Clause
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Entity_Record_Rep_Clause :=
           Libadalang.Implementation
             .Base_Type_Decl_P_Get_Record_Representation_Clause
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Record_Rep_Clause;
      end;
   end P_Get_Record_Representation_Clause;

   function P_Get_Enum_Representation_Clause
     (Node : Base_Type_Decl'Class; Imprecise_Fallback : Boolean := False)
      return Enum_Rep_Clause
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Entity_Enum_Rep_Clause :=
           Libadalang.Implementation
             .Base_Type_Decl_P_Get_Enum_Representation_Clause
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Enum_Rep_Clause;
      end;
   end P_Get_Enum_Representation_Clause;

   function P_Is_Record_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Record_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Record_Type;

   function P_Is_Array_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Base_Type_Decl_P_Is_Array_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Array_Type;

   function P_Find_Derived_Types
     (Node   : Base_Type_Decl'Class; Root : Ada_Node'Class;
      Origin : Ada_Node'Class; Imprecise_Fallback : Boolean := False)
      return Type_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Root : constant Internal_Entity :=
           (Root.Internal.Node, Root.Internal.Info);
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;
         Internal_Arg_Imprecise_Fallback : constant Boolean       :=
           Imprecise_Fallback;

         Property_Result : Internal_Entity_Type_Decl_Array_Access :=
           Libadalang.Implementation.Base_Type_Decl_P_Find_Derived_Types
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Root,
              Internal_Arg_Origin, Internal_Arg_Imprecise_Fallback,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Type_Decl_Array :=
             To_Public_Type_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Find_Derived_Types;

   function P_Is_Real_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Real_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Real_Type;

   function P_Is_Float_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Float_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Float_Type;

   function P_Is_Fixed_Point
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Fixed_Point
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Fixed_Point;

   function P_Is_Enum_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Enum_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Enum_Type;

   function P_Is_Access_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Access_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Access_Type;

   function P_Is_Char_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Char_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Char_Type;

   function P_Discrete_Range
     (Node : Base_Type_Decl'Class) return Discrete_Range
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Discrete_Range :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Discrete_Range
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return To_Public_Discrete_Range (Property_Result);
      end;
   end P_Discrete_Range;

   function P_Is_Discrete_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation
             .Dispatcher_Base_Type_Decl_P_Is_Discrete_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Discrete_Type;

   function P_Is_Int_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Int_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Int_Type;

   function P_Accessed_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Accessed_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Accessed_Type;

   function P_Is_Tagged_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Tagged_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Tagged_Type;

   function P_Base_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Base_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Base_Type;

   function P_Base_Types
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : Internal_Entity_Base_Type_Decl_Array_Access :=
           Libadalang.Implementation.Base_Type_Decl_P_Base_Types
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Base_Type_Decl_Array :=
             To_Public_Base_Type_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Base_Types;

   function P_Find_All_Derived_Types
     (Node               : Base_Type_Decl'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Type_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Units : Internal_Unit_Array_Access :=
           To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : Internal_Entity_Type_Decl_Array_Access :=
           Libadalang.Implementation.Base_Type_Decl_P_Find_All_Derived_Types
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Type_Decl_Array :=
             To_Public_Type_Decl_Array (Property_Result) do
            Dec_Ref (Internal_Arg_Units);
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Find_All_Derived_Types;

   function P_Comp_Type
     (Node   : Base_Type_Decl'Class; Is_Subscript : Boolean := False;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Is_Subscript : constant Boolean       := Is_Subscript;
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Type_Decl_P_Comp_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Is_Subscript,
              Internal_Arg_Origin, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Comp_Type;

   function P_Index_Type
     (Node   : Base_Type_Decl'Class; Dim : Integer;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Dim    : constant Integer       := Dim;
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Type_Decl_P_Index_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Dim,
              Internal_Arg_Origin, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Index_Type;

   function P_Is_Derived_Type
     (Node   : Base_Type_Decl'Class; Other_Type : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Other_Type : constant Internal_Entity_Base_Type_Decl :=
           (Other_Type.Internal.Node, Other_Type.Internal.Info);
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Base_Type_Decl_P_Is_Derived_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Other_Type,
              Internal_Arg_Origin, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Derived_Type;

   function P_Is_Interface_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Base_Type_Decl_P_Is_Interface_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Interface_Type;

   function P_Matching_Type
     (Node   : Base_Type_Decl'Class; Expected_Type : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Expected_Type : constant Internal_Entity_Base_Type_Decl :=
           (Expected_Type.Internal.Node, Expected_Type.Internal.Info);
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Base_Type_Decl_P_Matching_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Expected_Type,
              Internal_Arg_Origin, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Matching_Type;

   function P_Canonical_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Canonical_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Canonical_Type;

   function P_Previous_Part
     (Node : Base_Type_Decl'Class; Go_To_Incomplete : Boolean := True)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Go_To_Incomplete : constant Boolean := Go_To_Incomplete;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Previous_Part
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Go_To_Incomplete, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Previous_Part;

   function P_Next_Part (Node : Base_Type_Decl'Class) return Base_Type_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Type_Decl_P_Next_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Next_Part;

   function P_Full_View (Node : Base_Type_Decl'Class) return Base_Type_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Type_Decl_P_Full_View
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Full_View;

   function P_Is_Definite_Subtype
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Base_Type_Decl_P_Is_Definite_Subtype
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Definite_Subtype;

   function P_Is_Private (Node : Base_Type_Decl'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Private
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Private;

   function P_Root_Type
     (Node : Base_Type_Decl'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Root_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Root_Type;

   function F_Discriminants (Node : Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Type_Decl_F_Discriminants (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discriminants;

   function F_Type_Def (Node : Type_Decl'Class) return Type_Def is
      Result : Bare_Type_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Type_Decl_F_Type_Def (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Def;

   function P_Get_Primitives
     (Node : Type_Decl'Class; Only_Inherited : Boolean := False)
      return Basic_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Only_Inherited : constant Boolean := Only_Inherited;

         Property_Result : Internal_Entity_Basic_Decl_Array_Access :=
           Libadalang.Implementation.Type_Decl_P_Get_Primitives
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Only_Inherited,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Basic_Decl_Array :=
             To_Public_Basic_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Get_Primitives;

   function F_Indices (Node : Array_Type_Def'Class) return Array_Indices is
      Result : Bare_Array_Indices;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Array_Type_Def_F_Indices (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Indices;

   function F_Component_Type (Node : Array_Type_Def'Class) return Component_Def
   is
      Result : Bare_Component_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Array_Type_Def_F_Component_Type (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Component_Type;

   function F_Id (Node : Aspect_Assoc'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Aspect_Assoc_F_Id (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id;

   function F_Expr (Node : Aspect_Assoc'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Aspect_Assoc_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Aspect_Assoc_List'Class; Index : Positive) return Aspect_Assoc
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Aspect_Assoc;
   end List_Child;

   function Aspect_Assoc_List_First (Node : Aspect_Assoc_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Aspect_Assoc_List_First;

   function Aspect_Assoc_List_Next
     (Node : Aspect_Assoc_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Aspect_Assoc_List_Next;

   function Aspect_Assoc_List_Has_Element
     (Node : Aspect_Assoc_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Aspect_Assoc_List_Has_Element;

   function Aspect_Assoc_List_Element
     (Node : Aspect_Assoc_List; Cursor : Positive) return Aspect_Assoc'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Aspect_Assoc'(Child.As_Aspect_Assoc);
   end Aspect_Assoc_List_Element;

   function F_Aspect_Assocs (Node : Aspect_Spec'Class) return Aspect_Assoc_List
   is
      Result : Bare_Aspect_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Aspect_Spec_F_Aspect_Assocs (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Aspect_Assocs;

   function F_Dest (Node : Assign_Stmt'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Assign_Stmt_F_Dest (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Dest;

   function F_Expr (Node : Assign_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Assign_Stmt_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Basic_Assoc_List'Class; Index : Positive) return Basic_Assoc
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Basic_Assoc;
   end List_Child;

   function Basic_Assoc_List_First (Node : Basic_Assoc_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Basic_Assoc_List_First;

   function Basic_Assoc_List_Next
     (Node : Basic_Assoc_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Basic_Assoc_List_Next;

   function Basic_Assoc_List_Has_Element
     (Node : Basic_Assoc_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Basic_Assoc_List_Has_Element;

   function Basic_Assoc_List_Element
     (Node : Basic_Assoc_List; Cursor : Positive) return Basic_Assoc'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Basic_Assoc'(Child.As_Basic_Assoc);
   end Basic_Assoc_List_Element;

   function P_Zip_With_Params
     (Node : Assoc_List'Class; Imprecise_Fallback : Boolean := False)
      return Param_Actual_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : Internal_Param_Actual_Array_Access :=
           Libadalang.Implementation.Assoc_List_P_Zip_With_Params
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Param_Actual_Array :=
             To_Public_Param_Actual_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Zip_With_Params;

   function F_Name (Node : At_Clause'Class) return Base_Id is
      Result : Bare_Base_Id;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.At_Clause_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Expr (Node : At_Clause'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.At_Clause_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Attribute_Expr (Node : Attribute_Def_Clause'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Attribute_Def_Clause_F_Attribute_Expr
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Attribute_Expr;

   function F_Expr (Node : Attribute_Def_Clause'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Attribute_Def_Clause_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function P_Enclosing_Defining_Name (Node : Name'Class) return Defining_Name
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Name_P_Enclosing_Defining_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Enclosing_Defining_Name;

   function P_Is_Defining (Node : Name'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Defining
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Defining;

   function P_Name_Is
     (Node : Name'Class; Sym : Unbounded_Text_Type) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Sym : constant Symbol_Type :=
           Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Sym));

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Name_Is
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Sym);
      begin

         return Property_Result;
      end;
   end P_Name_Is;

   function P_Is_Direct_Call (Node : Name'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Direct_Call
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Direct_Call;

   function P_Is_Access_Call (Node : Name'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Access_Call
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Access_Call;

   function P_Is_Call (Node : Name'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Call
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Call;

   function P_Is_Dot_Call
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Dot_Call
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Dot_Call;

   function P_Failsafe_Referenced_Def_Name
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Refd_Def
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Refd_Def :=
           Libadalang.Implementation.Name_P_Failsafe_Referenced_Def_Name
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return To_Public_Refd_Def (Property_Result);
      end;
   end P_Failsafe_Referenced_Def_Name;

   function P_Referenced_Defining_Name
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Defining_Name
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Name_P_Referenced_Defining_Name
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Referenced_Defining_Name;

   function P_All_Env_Elements
     (Node     : Name'Class; Seq : Boolean := True;
      Seq_From : Ada_Node'Class := No_Ada_Node) return Ada_Node_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Seq      : constant Boolean       := Seq;
         Internal_Arg_Seq_From : constant Bare_Ada_Node :=
           Seq_From.Internal.Node;

         Property_Result : Internal_Entity_Array_Access :=
           Libadalang.Implementation.Name_P_All_Env_Elements
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Seq,
              Internal_Arg_Seq_From, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ada_Node_Array :=
             To_Public_Ada_Node_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_All_Env_Elements;

   function P_Called_Subp_Spec
     (Node : Name'Class) return Base_Formal_Param_Holder
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Formal_Param_Holder :=
           Libadalang.Implementation.Name_P_Called_Subp_Spec
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Formal_Param_Holder;
      end;
   end P_Called_Subp_Spec;

   function P_Referenced_Decl
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Basic_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Name_P_Referenced_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Referenced_Decl;

   function P_Failsafe_Referenced_Decl
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Refd_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Refd_Decl :=
           Libadalang.Implementation.Name_P_Failsafe_Referenced_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return To_Public_Refd_Decl (Property_Result);
      end;
   end P_Failsafe_Referenced_Decl;

   function P_Referenced_Decl_Internal
     (Node : Name'Class; Imprecise_Fallback : Boolean := False)
      return Refd_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Internal_Refd_Decl :=
           Libadalang.Implementation.Name_P_Referenced_Decl_Internal
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return To_Public_Refd_Decl (Property_Result);
      end;
   end P_Referenced_Decl_Internal;

   function P_Name_Designated_Type (Node : Name'Class) return Base_Type_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Name_P_Name_Designated_Type
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Name_Designated_Type;

   function P_Is_Static_Subtype
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Static_Subtype
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Static_Subtype;

   function P_Name_Matches (Node : Name'Class; N : Name'Class) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_N : constant Internal_Entity_Name :=
           (N.Internal.Node, N.Internal.Info);

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Name_Matches
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_N);
      begin

         return Property_Result;
      end;
   end P_Name_Matches;

   function P_Relative_Name (Node : Name'Class) return Single_Tok_Node is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Single_Tok_Node :=
           Libadalang.Implementation.Dispatcher_Name_P_Relative_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Single_Tok_Node;
      end;
   end P_Relative_Name;

   function P_Is_Operator_Name (Node : Name'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Operator_Name
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Operator_Name;

   function P_Is_Write_Reference
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Write_Reference
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Write_Reference;

   function P_Is_Dispatching_Call
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Dispatching_Call
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Dispatching_Call;

   function P_Is_Static_Call
     (Node : Name'Class; Imprecise_Fallback : Boolean := False) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Name_P_Is_Static_Call
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Static_Call;

   function P_As_Symbol_Array
     (Node : Name'Class) return Unbounded_Text_Type_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Symbol_Type_Array_Access :=
           Libadalang.Implementation.Name_P_As_Symbol_Array
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Unbounded_Text_Type_Array :=
             To_Public_Unbounded_Text_Type_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_As_Symbol_Array;

   function F_Prefix (Node : Attribute_Ref'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Attribute_Ref_F_Prefix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Prefix;

   function F_Attribute (Node : Attribute_Ref'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Attribute_Ref_F_Attribute (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Attribute;

   function F_Args (Node : Attribute_Ref'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Attribute_Ref_F_Args (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Args;

   function P_Assoc_Expr (Node : Base_Assoc'Class) return Expr is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Expr :=
           Libadalang.Implementation.Dispatcher_Base_Assoc_P_Assoc_Expr
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Expr;
      end;
   end P_Assoc_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Base_Assoc_List'Class; Index : Positive) return Base_Assoc
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Base_Assoc;
   end List_Child;

   function Base_Assoc_List_First (Node : Base_Assoc_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Base_Assoc_List_First;

   function Base_Assoc_List_Next
     (Node : Base_Assoc_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Base_Assoc_List_Next;

   function Base_Assoc_List_Has_Element
     (Node : Base_Assoc_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Base_Assoc_List_Has_Element;

   function Base_Assoc_List_Element
     (Node : Base_Assoc_List; Cursor : Positive) return Base_Assoc'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Base_Assoc'(Child.As_Base_Assoc);
   end Base_Assoc_List_Element;

   function P_Formal_Type
     (Node   : Base_Formal_Param_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Formal_Param_Decl_P_Formal_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Formal_Type;

   function P_Abstract_Formal_Params
     (Node : Base_Formal_Param_Holder'Class)
      return Base_Formal_Param_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Base_Formal_Param_Decl_Array_Access :=
           Libadalang.Implementation
             .Dispatcher_Base_Formal_Param_Holder_P_Abstract_Formal_Params
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Base_Formal_Param_Decl_Array :=
             To_Public_Base_Formal_Param_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Abstract_Formal_Params;

   function P_Nb_Min_Params
     (Node : Base_Formal_Param_Holder'Class) return Integer
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Integer :=
           Libadalang.Implementation.Base_Formal_Param_Holder_P_Nb_Min_Params
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Nb_Min_Params;

   function P_Nb_Max_Params
     (Node : Base_Formal_Param_Holder'Class) return Integer
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Integer :=
           Libadalang.Implementation.Base_Formal_Param_Holder_P_Nb_Max_Params
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Nb_Max_Params;

   function P_Param_Types
     (Node   : Base_Formal_Param_Holder'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : Internal_Entity_Base_Type_Decl_Array_Access :=
           Libadalang.Implementation.Base_Formal_Param_Holder_P_Param_Types
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Base_Type_Decl_Array :=
             To_Public_Base_Type_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Param_Types;

   function P_Canonical_Text
     (Node : Single_Tok_Node'Class) return Unbounded_Text_Type
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Symbol_Type :=
           Libadalang.Implementation.Single_Tok_Node_P_Canonical_Text
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return To_Unbounded_Text (Image (Property_Result));
      end;
   end P_Canonical_Text;

   function F_Spec (Node : Base_Loop_Stmt'Class) return Loop_Spec is
      Result : Bare_Loop_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Base_Loop_Stmt_F_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Spec;

   function F_Stmts (Node : Base_Loop_Stmt'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Base_Loop_Stmt_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Base_Loop_Stmt'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Base_Loop_Stmt_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Package_Name
     (Node : Base_Package_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Package_Decl_F_Package_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Package_Name;

   function F_Public_Part (Node : Base_Package_Decl'Class) return Public_Part
   is
      Result : Bare_Public_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Package_Decl_F_Public_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Public_Part;

   function F_Private_Part (Node : Base_Package_Decl'Class) return Private_Part
   is
      Result : Bare_Private_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Package_Decl_F_Private_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Private_Part;

   function F_End_Name (Node : Base_Package_Decl'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Package_Decl_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function P_Body_Part (Node : Base_Package_Decl'Class) return Package_Body is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Package_Body :=
           Libadalang.Implementation.Base_Package_Decl_P_Body_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Package_Body;
      end;
   end P_Body_Part;

   function F_Components (Node : Base_Record_Def'Class) return Component_List
   is
      Result : Bare_Component_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Record_Def_F_Components (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Components;

   function P_Previous_Part (Node : Body_Node'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Body_Node_P_Previous_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Previous_Part;

   function P_Decl_Part (Node : Body_Node'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Body_Node_P_Decl_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Decl_Part;

   function P_Subunit_Root (Node : Body_Node'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Body_Node_P_Subunit_Root
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Subunit_Root;

   function F_Overriding (Node : Base_Subp_Body'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Base_Subp_Body_F_Overriding (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Overriding;

   function F_Overriding
     (Node : Base_Subp_Body'Class) return Ada_Overriding_Node is
     (Overriding_Node'(Node.F_Overriding).Kind);

   function F_Subp_Spec (Node : Base_Subp_Body'Class) return Subp_Spec is
      Result : Bare_Subp_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Base_Subp_Body_F_Subp_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Spec;

   function P_Returns (Node : Base_Subp_Spec'Class) return Type_Expr is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Type_Expr :=
           Libadalang.Implementation.Dispatcher_Base_Subp_Spec_P_Returns
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Type_Expr;
      end;
   end P_Returns;

   function P_Params (Node : Base_Subp_Spec'Class) return Param_Spec_Array is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Param_Spec_Array_Access :=
           Libadalang.Implementation.Dispatcher_Base_Subp_Spec_P_Params
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Param_Spec_Array :=
             To_Public_Param_Spec_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Params;

   function P_Primitive_Subp_Types
     (Node : Base_Subp_Spec'Class) return Base_Type_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Base_Type_Decl_Array_Access :=
           Libadalang.Implementation.Base_Subp_Spec_P_Primitive_Subp_Types
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Base_Type_Decl_Array :=
             To_Public_Base_Type_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Primitive_Subp_Types;

   function P_Primitive_Subp_First_Type
     (Node : Base_Subp_Spec'Class) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Subp_Spec_P_Primitive_Subp_First_Type
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Primitive_Subp_First_Type;

   function P_Primitive_Subp_Tagged_Type
     (Node : Base_Subp_Spec'Class) return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation
             .Base_Subp_Spec_P_Primitive_Subp_Tagged_Type
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Primitive_Subp_Tagged_Type;

   function P_Return_Type
     (Node : Base_Subp_Spec'Class; Origin : Ada_Node'Class := No_Ada_Node)
      return Base_Type_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;

         Property_Result : constant Internal_Entity_Base_Type_Decl :=
           Libadalang.Implementation.Base_Subp_Spec_P_Return_Type
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin,
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Type_Decl;
      end;
   end P_Return_Type;

   function F_Stmts (Node : Begin_Block'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Begin_Block_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Begin_Block'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Begin_Block_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Left (Node : Bin_Op'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Bin_Op_F_Left (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Left;

   function F_Op (Node : Bin_Op'Class) return Op is
      Result : Bare_Op;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Bin_Op_F_Op (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Op;

   function F_Op (Node : Bin_Op'Class) return Ada_Op is (Op'(Node.F_Op).Kind);

   function F_Right (Node : Bin_Op'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Bin_Op_F_Right (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Right;

   function F_Name (Node : Call_Expr'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Call_Expr_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Suffix (Node : Call_Expr'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Call_Expr_F_Suffix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Suffix;

   function P_Is_Array_Slice (Node : Call_Expr'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Call_Expr_P_Is_Array_Slice
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Array_Slice;

   function F_Call (Node : Call_Stmt'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Call_Stmt_F_Call (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Call;

   function F_Expr (Node : Case_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Case_Expr_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Cases (Node : Case_Expr'Class) return Case_Expr_Alternative_List
   is
      Result : Bare_Case_Expr_Alternative_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Case_Expr_F_Cases (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cases;

   function F_Choices
     (Node : Case_Expr_Alternative'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Case_Expr_Alternative_F_Choices (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Choices;

   function F_Expr (Node : Case_Expr_Alternative'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Case_Expr_Alternative_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Case_Expr_Alternative_List'Class; Index : Positive)
      return Case_Expr_Alternative
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Case_Expr_Alternative;
   end List_Child;

   function Case_Expr_Alternative_List_First
     (Node : Case_Expr_Alternative_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Case_Expr_Alternative_List_First;

   function Case_Expr_Alternative_List_Next
     (Node : Case_Expr_Alternative_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Case_Expr_Alternative_List_Next;

   function Case_Expr_Alternative_List_Has_Element
     (Node : Case_Expr_Alternative_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Case_Expr_Alternative_List_Has_Element;

   function Case_Expr_Alternative_List_Element
     (Node : Case_Expr_Alternative_List; Cursor : Positive)
      return Case_Expr_Alternative'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Case_Expr_Alternative'(Child.As_Case_Expr_Alternative);
   end Case_Expr_Alternative_List_Element;

   function F_Expr (Node : Case_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Case_Stmt_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Alternatives
     (Node : Case_Stmt'Class) return Case_Stmt_Alternative_List
   is
      Result : Bare_Case_Stmt_Alternative_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Case_Stmt_F_Alternatives (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Alternatives;

   function F_Choices
     (Node : Case_Stmt_Alternative'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Case_Stmt_Alternative_F_Choices (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Choices;

   function F_Stmts (Node : Case_Stmt_Alternative'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Case_Stmt_Alternative_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Case_Stmt_Alternative_List'Class; Index : Positive)
      return Case_Stmt_Alternative
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Case_Stmt_Alternative;
   end List_Child;

   function Case_Stmt_Alternative_List_First
     (Node : Case_Stmt_Alternative_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Case_Stmt_Alternative_List_First;

   function Case_Stmt_Alternative_List_Next
     (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Case_Stmt_Alternative_List_Next;

   function Case_Stmt_Alternative_List_Has_Element
     (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Case_Stmt_Alternative_List_Has_Element;

   function Case_Stmt_Alternative_List_Element
     (Node : Case_Stmt_Alternative_List; Cursor : Positive)
      return Case_Stmt_Alternative'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Case_Stmt_Alternative'(Child.As_Case_Stmt_Alternative);
   end Case_Stmt_Alternative_List_Element;

   function P_Denoted_Value (Node : Char_Literal'Class) return Character_Type
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Character_Type :=
           Libadalang.Implementation.Extensions.Char_Literal_P_Denoted_Value
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Denoted_Value;

   function F_Prelude (Node : Compilation_Unit'Class) return Ada_Node_List is
      Result : Bare_Ada_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Compilation_Unit_F_Prelude (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Prelude;

   function F_Body (Node : Compilation_Unit'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Compilation_Unit_F_Body (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Body;

   function F_Pragmas (Node : Compilation_Unit'Class) return Pragma_Node_List
   is
      Result : Bare_Pragma_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Compilation_Unit_F_Pragmas (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Pragmas;

   function P_Syntactic_Fully_Qualified_Name
     (Node : Compilation_Unit'Class) return Unbounded_Text_Type_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Symbol_Type_Array_Access :=
           Libadalang.Implementation
             .Compilation_Unit_P_Syntactic_Fully_Qualified_Name
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Unbounded_Text_Type_Array :=
             To_Public_Unbounded_Text_Type_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Syntactic_Fully_Qualified_Name;

   function P_Unit_Kind
     (Node : Compilation_Unit'Class) return Analysis_Unit_Kind
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Analysis_Unit_Kind :=
           Libadalang.Implementation.Compilation_Unit_P_Unit_Kind
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_Unit_Kind;

   function P_Withed_Units
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Compilation_Unit_Array_Access :=
           Libadalang.Implementation.Compilation_Unit_P_Withed_Units
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Compilation_Unit_Array :=
             To_Public_Compilation_Unit_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Withed_Units;

   function P_Imported_Units
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Compilation_Unit_Array_Access :=
           Libadalang.Implementation.Compilation_Unit_P_Imported_Units
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Compilation_Unit_Array :=
             To_Public_Compilation_Unit_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Imported_Units;

   function P_Unit_Dependencies
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Compilation_Unit_Array_Access :=
           Libadalang.Implementation.Compilation_Unit_P_Unit_Dependencies
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Compilation_Unit_Array :=
             To_Public_Compilation_Unit_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Unit_Dependencies;

   function P_Decl (Node : Compilation_Unit'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Bare_Basic_Decl :=
           Libadalang.Implementation.Compilation_Unit_P_Decl
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result, No_Entity_Info).As_Basic_Decl;
      end;
   end P_Decl;

   function P_Is_Preelaborable (Node : Compilation_Unit'Class) return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Compilation_Unit_P_Is_Preelaborable
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Preelaborable;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Compilation_Unit_List'Class; Index : Positive)
      return Compilation_Unit
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Compilation_Unit;
   end List_Child;

   function Compilation_Unit_List_First
     (Node : Compilation_Unit_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Compilation_Unit_List_First;

   function Compilation_Unit_List_Next
     (Node : Compilation_Unit_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Compilation_Unit_List_Next;

   function Compilation_Unit_List_Has_Element
     (Node : Compilation_Unit_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Compilation_Unit_List_Has_Element;

   function Compilation_Unit_List_Element
     (Node : Compilation_Unit_List; Cursor : Positive)
      return Compilation_Unit'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Compilation_Unit'(Child.As_Compilation_Unit);
   end Compilation_Unit_List_Element;

   function F_Id (Node : Component_Clause'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Component_Clause_F_Id (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id;

   function F_Position (Node : Component_Clause'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_Clause_F_Position (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Position;

   function F_Range (Node : Component_Clause'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Component_Clause_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Ids (Node : Component_Decl'Class) return Defining_Name_List is
      Result : Bare_Defining_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Component_Decl_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Component_Def (Node : Component_Decl'Class) return Component_Def
   is
      Result : Bare_Component_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_Decl_F_Component_Def (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Component_Def;

   function F_Default_Expr (Node : Component_Decl'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_Decl_F_Default_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Default_Expr;

   function F_Has_Aliased (Node : Component_Def'Class) return Aliased_Node is
      Result : Bare_Aliased_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_Def_F_Has_Aliased (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Aliased;

   function F_Has_Aliased (Node : Component_Def'Class) return Boolean is
     (Aliased_Node'(Node.F_Has_Aliased).Kind = Ada_Aliased_Present);

   function F_Has_Constant (Node : Component_Def'Class) return Constant_Node is
      Result : Bare_Constant_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_Def_F_Has_Constant (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Constant;

   function F_Has_Constant (Node : Component_Def'Class) return Boolean is
     (Constant_Node'(Node.F_Has_Constant).Kind = Ada_Constant_Present);

   function F_Type_Expr (Node : Component_Def'Class) return Type_Expr is
      Result : Bare_Type_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Component_Def_F_Type_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Expr;

   function F_Components (Node : Component_List'Class) return Ada_Node_List is
      Result : Bare_Ada_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_List_F_Components (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Components;

   function F_Variant_Part (Node : Component_List'Class) return Variant_Part is
      Result : Bare_Variant_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Component_List_F_Variant_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Variant_Part;

   function P_As_Bool (Node : Constant_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Constant_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_List
     (Node : Constrained_Array_Indices'Class) return Constraint_List
   is
      Result : Bare_Constraint_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Constrained_Array_Indices_F_List (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_List;

   function F_Has_Not_Null (Node : Subtype_Indication'Class) return Not_Null is
      Result : Bare_Not_Null;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Subtype_Indication_F_Has_Not_Null (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Not_Null;

   function F_Has_Not_Null (Node : Subtype_Indication'Class) return Boolean is
     (Not_Null'(Node.F_Has_Not_Null).Kind = Ada_Not_Null_Present);

   function F_Name (Node : Subtype_Indication'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subtype_Indication_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Constraint (Node : Subtype_Indication'Class) return Constraint is
      Result : Bare_Constraint;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Subtype_Indication_F_Constraint (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Constraint;

   function P_Is_Static_Subtype
     (Node : Subtype_Indication'Class; Imprecise_Fallback : Boolean := False)
      return Boolean
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Subtype_Indication_P_Is_Static_Subtype
             (Bare_Ada_Node (Node.Internal.Node),
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return Property_Result;
      end;
   end P_Is_Static_Subtype;

   function F_Guard (Node : Contract_Case_Assoc'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Contract_Case_Assoc_F_Guard (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Guard;

   function F_Consequence (Node : Contract_Case_Assoc'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Contract_Case_Assoc_F_Consequence (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Consequence;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Contract_Case_Assoc_List'Class; Index : Positive)
      return Contract_Case_Assoc
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Contract_Case_Assoc;
   end List_Child;

   function Contract_Case_Assoc_List_First
     (Node : Contract_Case_Assoc_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Contract_Case_Assoc_List_First;

   function Contract_Case_Assoc_List_Next
     (Node : Contract_Case_Assoc_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Contract_Case_Assoc_List_Next;

   function Contract_Case_Assoc_List_Has_Element
     (Node : Contract_Case_Assoc_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Contract_Case_Assoc_List_Has_Element;

   function Contract_Case_Assoc_List_Element
     (Node : Contract_Case_Assoc_List; Cursor : Positive)
      return Contract_Case_Assoc'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Contract_Case_Assoc'(Child.As_Contract_Case_Assoc);
   end Contract_Case_Assoc_List_Element;

   function F_Contract_Cases
     (Node : Contract_Cases'Class) return Contract_Case_Assoc_List
   is
      Result : Bare_Contract_Case_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Contract_Cases_F_Contract_Cases (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Contract_Cases;

   function F_Delta (Node : Decimal_Fixed_Point_Def'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Decimal_Fixed_Point_Def_F_Delta (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Delta;

   function F_Digits (Node : Decimal_Fixed_Point_Def'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Decimal_Fixed_Point_Def_F_Digits (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Digits;

   function F_Range (Node : Decimal_Fixed_Point_Def'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Decimal_Fixed_Point_Def_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Decls (Node : Decl_Block'Class) return Declarative_Part is
      Result : Bare_Declarative_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Decl_Block_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_Stmts (Node : Decl_Block'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Decl_Block_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Decl_Block'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Decl_Block_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Decls (Node : Declarative_Part'Class) return Ada_Node_List is
      Result : Bare_Ada_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Declarative_Part_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_Name (Node : Defining_Name'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Defining_Name_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function P_Basic_Decl (Node : Defining_Name'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Defining_Name_P_Basic_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Basic_Decl;

   function P_Find_Refs
     (Node   : Defining_Name'Class; Root : Ada_Node'Class;
      Origin : Ada_Node'Class; Imprecise_Fallback : Boolean := False)
      return Ref_Result_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Root : constant Internal_Entity :=
           (Root.Internal.Node, Root.Internal.Info);
         Internal_Arg_Origin : constant Bare_Ada_Node := Origin.Internal.Node;
         Internal_Arg_Imprecise_Fallback : constant Boolean       :=
           Imprecise_Fallback;

         Property_Result : Internal_Ref_Result_Array_Access :=
           Libadalang.Implementation.Defining_Name_P_Find_Refs
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Root,
              Internal_Arg_Origin, Internal_Arg_Imprecise_Fallback,
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ref_Result_Array :=
             To_Public_Ref_Result_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Find_Refs;

   function P_Find_All_References
     (Node               : Defining_Name'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Units : Internal_Unit_Array_Access :=
           To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : Internal_Ref_Result_Array_Access :=
           Libadalang.Implementation.Defining_Name_P_Find_All_References
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ref_Result_Array :=
             To_Public_Ref_Result_Array (Property_Result) do
            Dec_Ref (Internal_Arg_Units);
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Find_All_References;

   function P_Find_All_Calls
     (Node               : Defining_Name'Class; Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare
         Internal_Arg_Units : Internal_Unit_Array_Access :=
           To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Imprecise_Fallback : constant Boolean :=
           Imprecise_Fallback;

         Property_Result : Internal_Ref_Result_Array_Access :=
           Libadalang.Implementation.Defining_Name_P_Find_All_Calls
             (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units,
              Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Ref_Result_Array :=
             To_Public_Ref_Result_Array (Property_Result) do
            Dec_Ref (Internal_Arg_Units);
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Find_All_Calls;

   function P_Next_Part (Node : Defining_Name'Class) return Defining_Name is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Defining_Name_P_Next_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Next_Part;

   function P_Previous_Part (Node : Defining_Name'Class) return Defining_Name
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Defining_Name_P_Previous_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Previous_Part;

   function P_Canonical_Part (Node : Defining_Name'Class) return Defining_Name
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Defining_Name :=
           Libadalang.Implementation.Defining_Name_P_Canonical_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Defining_Name;
      end;
   end P_Canonical_Part;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Defining_Name_List'Class; Index : Positive) return Defining_Name
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Defining_Name;
   end List_Child;

   function Defining_Name_List_First
     (Node : Defining_Name_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Defining_Name_List_First;

   function Defining_Name_List_Next
     (Node : Defining_Name_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Defining_Name_List_Next;

   function Defining_Name_List_Has_Element
     (Node : Defining_Name_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Defining_Name_List_Has_Element;

   function Defining_Name_List_Element
     (Node : Defining_Name_List; Cursor : Positive) return Defining_Name'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Defining_Name'(Child.As_Defining_Name);
   end Defining_Name_List_Element;

   function F_Has_Until (Node : Delay_Stmt'Class) return Until_Node is
      Result : Bare_Until_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Delay_Stmt_F_Has_Until (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Until;

   function F_Has_Until (Node : Delay_Stmt'Class) return Boolean is
     (Until_Node'(Node.F_Has_Until).Kind = Ada_Until_Present);

   function F_Expr (Node : Delay_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Delay_Stmt_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Digits (Node : Delta_Constraint'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Delta_Constraint_F_Digits (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Digits;

   function F_Range (Node : Delta_Constraint'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Delta_Constraint_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Has_Abstract (Node : Derived_Type_Def'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Has_Abstract (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Abstract;

   function F_Has_Abstract (Node : Derived_Type_Def'Class) return Boolean is
     (Abstract_Node'(Node.F_Has_Abstract).Kind = Ada_Abstract_Present);

   function F_Has_Limited (Node : Derived_Type_Def'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Has_Limited (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Limited;

   function F_Has_Limited (Node : Derived_Type_Def'Class) return Boolean is
     (Limited_Node'(Node.F_Has_Limited).Kind = Ada_Limited_Present);

   function F_Has_Synchronized
     (Node : Derived_Type_Def'Class) return Synchronized_Node
   is
      Result : Bare_Synchronized_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Has_Synchronized
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Synchronized;

   function F_Has_Synchronized
     (Node : Derived_Type_Def'Class) return Boolean is
     (Synchronized_Node'(Node.F_Has_Synchronized).Kind =
      Ada_Synchronized_Present);

   function F_Subtype_Indication
     (Node : Derived_Type_Def'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Subtype_Indication
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subtype_Indication;

   function F_Interfaces (Node : Derived_Type_Def'Class) return Parent_List is
      Result : Bare_Parent_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Interfaces (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Interfaces;

   function F_Record_Extension
     (Node : Derived_Type_Def'Class) return Base_Record_Def
   is
      Result : Bare_Base_Record_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Record_Extension
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Record_Extension;

   function F_Has_With_Private
     (Node : Derived_Type_Def'Class) return With_Private
   is
      Result : Bare_With_Private;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Derived_Type_Def_F_Has_With_Private
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_With_Private;

   function F_Has_With_Private
     (Node : Derived_Type_Def'Class) return Boolean is
     (With_Private'(Node.F_Has_With_Private).Kind = Ada_With_Private_Present);

   function F_Digits (Node : Digits_Constraint'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Digits_Constraint_F_Digits (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Digits;

   function F_Range (Node : Digits_Constraint'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Digits_Constraint_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Subtype
     (Node : Discrete_Subtype_Name'Class) return Discrete_Subtype_Indication
   is
      Result : Bare_Discrete_Subtype_Indication;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Discrete_Subtype_Name_F_Subtype (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subtype;

   function F_Ids
     (Node : Discriminant_Assoc'Class) return Discriminant_Choice_List
   is
      Result : Bare_Discriminant_Choice_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Discriminant_Assoc_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Discr_Expr (Node : Discriminant_Assoc'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Discriminant_Assoc_F_Discr_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discr_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Identifier_List'Class; Index : Positive) return Identifier
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Identifier;
   end List_Child;

   function Identifier_List_First (Node : Identifier_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Identifier_List_First;

   function Identifier_List_Next
     (Node : Identifier_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Identifier_List_Next;

   function Identifier_List_Has_Element
     (Node : Identifier_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Identifier_List_Has_Element;

   function Identifier_List_Element
     (Node : Identifier_List; Cursor : Positive) return Identifier'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Identifier'(Child.As_Identifier);
   end Identifier_List_Element;

   function F_Constraints
     (Node : Discriminant_Constraint'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Discriminant_Constraint_F_Constraints
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Constraints;

   function F_Ids (Node : Discriminant_Spec'Class) return Defining_Name_List is
      Result : Bare_Defining_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Discriminant_Spec_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Type_Expr (Node : Discriminant_Spec'Class) return Type_Expr is
      Result : Bare_Type_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Discriminant_Spec_F_Type_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Expr;

   function F_Default_Expr (Node : Discriminant_Spec'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Discriminant_Spec_F_Default_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Default_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Discriminant_Spec_List'Class; Index : Positive)
      return Discriminant_Spec
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Discriminant_Spec;
   end List_Child;

   function Discriminant_Spec_List_First
     (Node : Discriminant_Spec_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Discriminant_Spec_List_First;

   function Discriminant_Spec_List_Next
     (Node : Discriminant_Spec_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Discriminant_Spec_List_Next;

   function Discriminant_Spec_List_Has_Element
     (Node : Discriminant_Spec_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Discriminant_Spec_List_Has_Element;

   function Discriminant_Spec_List_Element
     (Node : Discriminant_Spec_List; Cursor : Positive)
      return Discriminant_Spec'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Discriminant_Spec'(Child.As_Discriminant_Spec);
   end Discriminant_Spec_List_Element;

   function F_Prefix (Node : Dotted_Name'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Dotted_Name_F_Prefix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Prefix;

   function F_Suffix (Node : Dotted_Name'Class) return Base_Id is
      Result : Bare_Base_Id;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Dotted_Name_F_Suffix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Suffix;

   function F_Cond_Expr (Node : Elsif_Expr_Part'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Elsif_Expr_Part_F_Cond_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cond_Expr;

   function F_Then_Expr (Node : Elsif_Expr_Part'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Elsif_Expr_Part_F_Then_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Then_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Elsif_Expr_Part_List'Class; Index : Positive)
      return Elsif_Expr_Part
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Elsif_Expr_Part;
   end List_Child;

   function Elsif_Expr_Part_List_First
     (Node : Elsif_Expr_Part_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Elsif_Expr_Part_List_First;

   function Elsif_Expr_Part_List_Next
     (Node : Elsif_Expr_Part_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Elsif_Expr_Part_List_Next;

   function Elsif_Expr_Part_List_Has_Element
     (Node : Elsif_Expr_Part_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Elsif_Expr_Part_List_Has_Element;

   function Elsif_Expr_Part_List_Element
     (Node : Elsif_Expr_Part_List; Cursor : Positive)
      return Elsif_Expr_Part'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Elsif_Expr_Part'(Child.As_Elsif_Expr_Part);
   end Elsif_Expr_Part_List_Element;

   function F_Cond_Expr (Node : Elsif_Stmt_Part'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Elsif_Stmt_Part_F_Cond_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cond_Expr;

   function F_Stmts (Node : Elsif_Stmt_Part'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Elsif_Stmt_Part_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Elsif_Stmt_Part_List'Class; Index : Positive)
      return Elsif_Stmt_Part
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Elsif_Stmt_Part;
   end List_Child;

   function Elsif_Stmt_Part_List_First
     (Node : Elsif_Stmt_Part_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Elsif_Stmt_Part_List_First;

   function Elsif_Stmt_Part_List_Next
     (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Elsif_Stmt_Part_List_Next;

   function Elsif_Stmt_Part_List_Has_Element
     (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Elsif_Stmt_Part_List_Has_Element;

   function Elsif_Stmt_Part_List_Element
     (Node : Elsif_Stmt_Part_List; Cursor : Positive)
      return Elsif_Stmt_Part'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Elsif_Stmt_Part'(Child.As_Elsif_Stmt_Part);
   end Elsif_Stmt_Part_List_Element;

   function F_Name (Node : End_Name'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.End_Name_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function P_Basic_Decl (Node : End_Name'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.End_Name_P_Basic_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Basic_Decl;

   function F_Entry_Name (Node : Entry_Body'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_Entry_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Entry_Name;

   function F_Index_Spec (Node : Entry_Body'Class) return Entry_Index_Spec is
      Result : Bare_Entry_Index_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_Index_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Index_Spec;

   function F_Params
     (Node : Entry_Body'Class) return Entry_Completion_Formal_Params
   is
      Result : Bare_Entry_Completion_Formal_Params;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_Params (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Params;

   function F_Barrier (Node : Entry_Body'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_Barrier (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Barrier;

   function F_Decls (Node : Entry_Body'Class) return Declarative_Part is
      Result : Bare_Declarative_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_Stmts (Node : Entry_Body'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Entry_Body'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Body_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Params
     (Node : Entry_Completion_Formal_Params'Class) return Params
   is
      Result : Bare_Params;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Entry_Completion_Formal_Params_F_Params
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Params;

   function F_Overriding (Node : Entry_Decl'Class) return Overriding_Node is
      Result : Bare_Overriding_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Decl_F_Overriding (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Overriding;

   function F_Overriding
     (Node : Entry_Decl'Class) return Ada_Overriding_Node is
     (Overriding_Node'(Node.F_Overriding).Kind);

   function F_Spec (Node : Entry_Decl'Class) return Entry_Spec is
      Result : Bare_Entry_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Decl_F_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Spec;

   function F_Id (Node : Entry_Index_Spec'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Index_Spec_F_Id (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id;

   function F_Subtype (Node : Entry_Index_Spec'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Index_Spec_F_Subtype (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subtype;

   function F_Entry_Name (Node : Entry_Spec'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Spec_F_Entry_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Entry_Name;

   function F_Family_Type (Node : Entry_Spec'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Spec_F_Family_Type (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Family_Type;

   function F_Entry_Params (Node : Entry_Spec'Class) return Params is
      Result : Bare_Params;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Entry_Spec_F_Entry_Params (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Entry_Params;

   function F_Name (Node : Enum_Literal_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Enum_Literal_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function P_Enum_Type (Node : Enum_Literal_Decl'Class) return Type_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Type_Decl :=
           Libadalang.Implementation.Enum_Literal_Decl_P_Enum_Type
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Type_Decl;
      end;
   end P_Enum_Type;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Enum_Literal_Decl_List'Class; Index : Positive)
      return Enum_Literal_Decl
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Enum_Literal_Decl;
   end List_Child;

   function Enum_Literal_Decl_List_First
     (Node : Enum_Literal_Decl_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Enum_Literal_Decl_List_First;

   function Enum_Literal_Decl_List_Next
     (Node : Enum_Literal_Decl_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Enum_Literal_Decl_List_Next;

   function Enum_Literal_Decl_List_Has_Element
     (Node : Enum_Literal_Decl_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Enum_Literal_Decl_List_Has_Element;

   function Enum_Literal_Decl_List_Element
     (Node : Enum_Literal_Decl_List; Cursor : Positive)
      return Enum_Literal_Decl'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Enum_Literal_Decl'(Child.As_Enum_Literal_Decl);
   end Enum_Literal_Decl_List_Element;

   function F_Type_Name (Node : Enum_Rep_Clause'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Enum_Rep_Clause_F_Type_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Name;

   function F_Aggregate (Node : Enum_Rep_Clause'Class) return Base_Aggregate is
      Result : Bare_Base_Aggregate;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Enum_Rep_Clause_F_Aggregate (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Aggregate;

   function F_Enum_Literals
     (Node : Enum_Type_Def'Class) return Enum_Literal_Decl_List
   is
      Result : Bare_Enum_Literal_Decl_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Enum_Type_Def_F_Enum_Literals (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Enum_Literals;

   function F_Ids (Node : Exception_Decl'Class) return Defining_Name_List is
      Result : Bare_Defining_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Exception_Decl_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Renames (Node : Exception_Decl'Class) return Renaming_Clause is
      Result : Bare_Renaming_Clause;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Exception_Decl_F_Renames (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renames;

   function F_Exception_Name
     (Node : Exception_Handler'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Exception_Handler_F_Exception_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Exception_Name;

   function F_Handled_Exceptions
     (Node : Exception_Handler'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Exception_Handler_F_Handled_Exceptions
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Handled_Exceptions;

   function F_Stmts (Node : Exception_Handler'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Exception_Handler_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_Loop_Name (Node : Exit_Stmt'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Exit_Stmt_F_Loop_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Loop_Name;

   function F_Cond_Expr (Node : Exit_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Exit_Stmt_F_Cond_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cond_Expr;

   function F_Prefix (Node : Explicit_Deref'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Explicit_Deref_F_Prefix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Prefix;

   ----------------
   -- List_Child --
   ----------------

   function List_Child (Node : Expr_List'Class; Index : Positive) return Expr
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Expr;
   end List_Child;

   function Expr_List_First (Node : Expr_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Expr_List_First;

   function Expr_List_Next
     (Node : Expr_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Expr_List_Next;

   function Expr_List_Has_Element
     (Node : Expr_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Expr_List_Has_Element;

   function Expr_List_Element
     (Node : Expr_List; Cursor : Positive) return Expr'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Expr'(Child.As_Expr);
   end Expr_List_Element;

   function F_Expr (Node : Expr_Function'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Expr_Function_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Decl
     (Node : Extended_Return_Stmt'Class)
      return Extended_Return_Stmt_Object_Decl
   is
      Result : Bare_Extended_Return_Stmt_Object_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Extended_Return_Stmt_F_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decl;

   function F_Stmts (Node : Extended_Return_Stmt'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Extended_Return_Stmt_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_Num_Digits (Node : Floating_Point_Def'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Floating_Point_Def_F_Num_Digits (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Num_Digits;

   function F_Range (Node : Floating_Point_Def'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Floating_Point_Def_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Var_Decl (Node : For_Loop_Spec'Class) return For_Loop_Var_Decl is
      Result : Bare_For_Loop_Var_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.For_Loop_Spec_F_Var_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Var_Decl;

   function F_Loop_Type (Node : For_Loop_Spec'Class) return Iter_Type is
      Result : Bare_Iter_Type;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.For_Loop_Spec_F_Loop_Type (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Loop_Type;

   function F_Loop_Type (Node : For_Loop_Spec'Class) return Ada_Iter_Type is
     (Iter_Type'(Node.F_Loop_Type).Kind);

   function F_Has_Reverse (Node : For_Loop_Spec'Class) return Reverse_Node is
      Result : Bare_Reverse_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.For_Loop_Spec_F_Has_Reverse (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Reverse;

   function F_Has_Reverse (Node : For_Loop_Spec'Class) return Boolean is
     (Reverse_Node'(Node.F_Has_Reverse).Kind = Ada_Reverse_Present);

   function F_Iter_Expr (Node : For_Loop_Spec'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.For_Loop_Spec_F_Iter_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Iter_Expr;

   function F_Id (Node : For_Loop_Var_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.For_Loop_Var_Decl_F_Id (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id;

   function F_Id_Type
     (Node : For_Loop_Var_Decl'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.For_Loop_Var_Decl_F_Id_Type (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id_Type;

   function F_Formal_Part
     (Node : Generic_Decl'Class) return Generic_Formal_Part
   is
      Result : Bare_Generic_Formal_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Generic_Decl_F_Formal_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Formal_Part;

   function F_Decl (Node : Generic_Formal'Class) return Basic_Decl is
      Result : Bare_Basic_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Generic_Formal_F_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decl;

   function F_Decls (Node : Generic_Formal_Part'Class) return Ada_Node_List is
      Result : Bare_Ada_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Formal_Part_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function P_Designated_Generic_Decl
     (Node : Generic_Instantiation'Class) return Basic_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation
             .Dispatcher_Generic_Instantiation_P_Designated_Generic_Decl
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Designated_Generic_Decl;

   function F_Package_Decl
     (Node : Generic_Package_Decl'Class) return Generic_Package_Internal
   is
      Result : Bare_Generic_Package_Internal;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Package_Decl_F_Package_Decl
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Package_Decl;

   function P_Body_Part (Node : Generic_Package_Decl'Class) return Package_Body
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Package_Body :=
           Libadalang.Implementation.Generic_Package_Decl_P_Body_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Package_Body;
      end;
   end P_Body_Part;

   function F_Name
     (Node : Generic_Package_Instantiation'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Package_Instantiation_F_Name
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Generic_Pkg_Name
     (Node : Generic_Package_Instantiation'Class) return Name
   is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Package_Instantiation_F_Generic_Pkg_Name
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Generic_Pkg_Name;

   function F_Params
     (Node : Generic_Package_Instantiation'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Package_Instantiation_F_Params
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Params;

   function F_Name
     (Node : Generic_Package_Renaming_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Package_Renaming_Decl_F_Name
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Renames (Node : Generic_Package_Renaming_Decl'Class) return Name
   is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Package_Renaming_Decl_F_Renames
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renames;

   function F_Subp_Decl
     (Node : Generic_Subp_Decl'Class) return Generic_Subp_Internal
   is
      Result : Bare_Generic_Subp_Internal;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Decl_F_Subp_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Decl;

   function P_Body_Part (Node : Generic_Subp_Decl'Class) return Base_Subp_Body
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Base_Subp_Body :=
           Libadalang.Implementation.Generic_Subp_Decl_P_Body_Part
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Base_Subp_Body;
      end;
   end P_Body_Part;

   function F_Overriding
     (Node : Generic_Subp_Instantiation'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Instantiation_F_Overriding
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Overriding;

   function F_Overriding
     (Node : Generic_Subp_Instantiation'Class) return Ada_Overriding_Node is
     (Overriding_Node'(Node.F_Overriding).Kind);

   function F_Kind (Node : Generic_Subp_Instantiation'Class) return Subp_Kind
   is
      Result : Bare_Subp_Kind;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Instantiation_F_Kind (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Kind;

   function F_Kind
     (Node : Generic_Subp_Instantiation'Class) return Ada_Subp_Kind is
     (Subp_Kind'(Node.F_Kind).Kind);

   function F_Subp_Name
     (Node : Generic_Subp_Instantiation'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Instantiation_F_Subp_Name
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Name;

   function F_Generic_Subp_Name
     (Node : Generic_Subp_Instantiation'Class) return Name
   is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Instantiation_F_Generic_Subp_Name
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Generic_Subp_Name;

   function F_Params
     (Node : Generic_Subp_Instantiation'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Instantiation_F_Params
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Params;

   function P_Designated_Subp
     (Node : Generic_Subp_Instantiation'Class) return Ada_Node
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity :=
           Libadalang.Implementation
             .Generic_Subp_Instantiation_P_Designated_Subp
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info);
      end;
   end P_Designated_Subp;

   function F_Subp_Spec (Node : Generic_Subp_Internal'Class) return Subp_Spec
   is
      Result : Bare_Subp_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Internal_F_Subp_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Spec;

   function F_Kind (Node : Generic_Subp_Renaming_Decl'Class) return Subp_Kind
   is
      Result : Bare_Subp_Kind;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Renaming_Decl_F_Kind (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Kind;

   function F_Kind
     (Node : Generic_Subp_Renaming_Decl'Class) return Ada_Subp_Kind is
     (Subp_Kind'(Node.F_Kind).Kind);

   function F_Name
     (Node : Generic_Subp_Renaming_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Renaming_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Renames (Node : Generic_Subp_Renaming_Decl'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Generic_Subp_Renaming_Decl_F_Renames
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renames;

   function F_Label_Name (Node : Goto_Stmt'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Goto_Stmt_F_Label_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Label_Name;

   function F_Stmts (Node : Handled_Stmts'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Handled_Stmts_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_Exceptions (Node : Handled_Stmts'Class) return Ada_Node_List is
      Result : Bare_Ada_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Handled_Stmts_F_Exceptions (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Exceptions;

   function F_Cond_Expr (Node : If_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Expr_F_Cond_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cond_Expr;

   function F_Then_Expr (Node : If_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Expr_F_Then_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Then_Expr;

   function F_Alternatives (Node : If_Expr'Class) return Elsif_Expr_Part_List
   is
      Result : Bare_Elsif_Expr_Part_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Expr_F_Alternatives (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Alternatives;

   function F_Else_Expr (Node : If_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Expr_F_Else_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Else_Expr;

   function F_Cond_Expr (Node : If_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Stmt_F_Cond_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cond_Expr;

   function F_Then_Stmts (Node : If_Stmt'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Stmt_F_Then_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Then_Stmts;

   function F_Alternatives (Node : If_Stmt'Class) return Elsif_Stmt_Part_List
   is
      Result : Bare_Elsif_Stmt_Part_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Stmt_F_Alternatives (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Alternatives;

   function F_Else_Stmts (Node : If_Stmt'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.If_Stmt_F_Else_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Else_Stmts;

   function F_Discriminants
     (Node : Incomplete_Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Incomplete_Type_Decl_F_Discriminants
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discriminants;

   function F_Has_Abstract
     (Node : Incomplete_Tagged_Type_Decl'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Incomplete_Tagged_Type_Decl_F_Has_Abstract
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Abstract;

   function F_Has_Abstract
     (Node : Incomplete_Tagged_Type_Decl'Class) return Boolean is
     (Abstract_Node'(Node.F_Has_Abstract).Kind = Ada_Abstract_Present);

   function F_Constraints
     (Node : Index_Constraint'Class) return Constraint_List
   is
      Result : Bare_Constraint_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Index_Constraint_F_Constraints (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Constraints;

   function P_Denoted_Value (Node : Int_Literal'Class) return Big_Integer is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Big_Integer_Type :=
           Libadalang.Implementation.Extensions.Int_Literal_P_Denoted_Value
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Big_Integer :=
             Create_Public_Big_Integer (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Denoted_Value;

   function F_Interface_Kind
     (Node : Interface_Type_Def'Class) return Interface_Kind
   is
      Result : Bare_Interface_Kind;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Interface_Type_Def_F_Interface_Kind
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Interface_Kind;

   function F_Interface_Kind
     (Node : Interface_Type_Def'Class) return Ada_Interface_Kind is
     (Interface_Kind'(Node.F_Interface_Kind).Kind);

   function F_Interfaces (Node : Interface_Type_Def'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Interface_Type_Def_F_Interfaces (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Interfaces;

   function F_Discr_Specs
     (Node : Known_Discriminant_Part'Class) return Discriminant_Spec_List
   is
      Result : Bare_Discriminant_Spec_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Known_Discriminant_Part_F_Discr_Specs
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discr_Specs;

   function F_Decl (Node : Label'Class) return Label_Decl is
      Result : Bare_Label_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Label_F_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decl;

   function F_Name (Node : Label_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Label_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Has_Private (Node : Library_Item'Class) return Private_Node is
      Result : Bare_Private_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Library_Item_F_Has_Private (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Private;

   function F_Has_Private (Node : Library_Item'Class) return Boolean is
     (Private_Node'(Node.F_Has_Private).Kind = Ada_Private_Present);

   function F_Item (Node : Library_Item'Class) return Basic_Decl is
      Result : Bare_Basic_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Library_Item_F_Item (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Item;

   function P_As_Bool (Node : Limited_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Limited_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Expr (Node : Membership_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Membership_Expr_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Op (Node : Membership_Expr'Class) return Op is
      Result : Bare_Op;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Membership_Expr_F_Op (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Op;

   function F_Op (Node : Membership_Expr'Class) return Ada_Op is
     (Op'(Node.F_Op).Kind);

   function F_Membership_Exprs
     (Node : Membership_Expr'Class) return Expr_Alternatives_List
   is
      Result : Bare_Expr_Alternatives_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Membership_Expr_F_Membership_Exprs (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Membership_Exprs;

   function F_Expr (Node : Mod_Int_Type_Def'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Mod_Int_Type_Def_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child (Node : Name_List'Class; Index : Positive) return Name
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Name;
   end List_Child;

   function Name_List_First (Node : Name_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Name_List_First;

   function Name_List_Next
     (Node : Name_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Name_List_Next;

   function Name_List_Has_Element
     (Node : Name_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Name_List_Has_Element;

   function Name_List_Element
     (Node : Name_List; Cursor : Positive) return Name'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Name'(Child.As_Name);
   end Name_List_Element;

   function F_Decl (Node : Named_Stmt'Class) return Named_Stmt_Decl is
      Result : Bare_Named_Stmt_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Named_Stmt_F_Decl (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decl;

   function F_Stmt (Node : Named_Stmt'Class) return Composite_Stmt is
      Result : Bare_Composite_Stmt;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Named_Stmt_F_Stmt (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmt;

   function F_Name (Node : Named_Stmt_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Named_Stmt_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function P_As_Bool (Node : Not_Null'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Not_Null_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Ids (Node : Number_Decl'Class) return Defining_Name_List is
      Result : Bare_Defining_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Number_Decl_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Expr (Node : Number_Decl'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Number_Decl_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Delta (Node : Ordinary_Fixed_Point_Def'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Ordinary_Fixed_Point_Def_F_Delta (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Delta;

   function F_Range (Node : Ordinary_Fixed_Point_Def'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Ordinary_Fixed_Point_Def_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Package_Name (Node : Package_Body'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Package_Body_F_Package_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Package_Name;

   function F_Decls (Node : Package_Body'Class) return Declarative_Part is
      Result : Bare_Declarative_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Package_Body_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_Stmts (Node : Package_Body'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Package_Body_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Package_Body'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Package_Body_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Name (Node : Package_Body_Stub'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Package_Body_Stub_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Name (Node : Package_Renaming_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Package_Renaming_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Renames
     (Node : Package_Renaming_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Package_Renaming_Decl_F_Renames (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renames;

   function P_Renamed_Package
     (Node : Package_Renaming_Decl'Class) return Basic_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Package_Renaming_Decl_P_Renamed_Package
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Renamed_Package;

   function P_Final_Renamed_Package
     (Node : Package_Renaming_Decl'Class) return Basic_Decl
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation
             .Package_Renaming_Decl_P_Final_Renamed_Package
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Final_Renamed_Package;

   function F_Designator (Node : Param_Assoc'Class) return Ada_Node is
      Result : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Assoc_F_Designator (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Designator;

   function F_R_Expr (Node : Param_Assoc'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Assoc_F_R_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_R_Expr;

   function F_Ids (Node : Param_Spec'Class) return Defining_Name_List is
      Result : Bare_Defining_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Spec_F_Ids (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Ids;

   function F_Has_Aliased (Node : Param_Spec'Class) return Aliased_Node is
      Result : Bare_Aliased_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Spec_F_Has_Aliased (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Aliased;

   function F_Has_Aliased (Node : Param_Spec'Class) return Boolean is
     (Aliased_Node'(Node.F_Has_Aliased).Kind = Ada_Aliased_Present);

   function F_Mode (Node : Param_Spec'Class) return Mode is
      Result : Bare_Mode;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Spec_F_Mode (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Mode;

   function F_Mode (Node : Param_Spec'Class) return Ada_Mode is
     (Mode'(Node.F_Mode).Kind);

   function F_Type_Expr (Node : Param_Spec'Class) return Type_Expr is
      Result : Bare_Type_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Spec_F_Type_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Type_Expr;

   function F_Default_Expr (Node : Param_Spec'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Param_Spec_F_Default_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Default_Expr;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Param_Spec_List'Class; Index : Positive) return Param_Spec
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Param_Spec;
   end List_Child;

   function Param_Spec_List_First (Node : Param_Spec_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Param_Spec_List_First;

   function Param_Spec_List_Next
     (Node : Param_Spec_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Param_Spec_List_Next;

   function Param_Spec_List_Has_Element
     (Node : Param_Spec_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Param_Spec_List_Has_Element;

   function Param_Spec_List_Element
     (Node : Param_Spec_List; Cursor : Positive) return Param_Spec'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Param_Spec'(Child.As_Param_Spec);
   end Param_Spec_List_Element;

   function F_Params (Node : Params'Class) return Param_Spec_List is
      Result : Bare_Param_Spec_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Params_F_Params (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Params;

   function F_Expr (Node : Paren_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Paren_Expr_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Id (Node : Pragma_Argument_Assoc'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Pragma_Argument_Assoc_F_Id (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id;

   function F_Expr (Node : Pragma_Argument_Assoc'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Pragma_Argument_Assoc_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Id (Node : Pragma_Node'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Pragma_Node_F_Id (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Id;

   function F_Args (Node : Pragma_Node'Class) return Base_Assoc_List is
      Result : Bare_Base_Assoc_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Pragma_Node_F_Args (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Args;

   function P_Associated_Decls
     (Node : Pragma_Node'Class) return Basic_Decl_Array
   is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Internal_Entity_Basic_Decl_Array_Access :=
           Libadalang.Implementation.Pragma_Node_P_Associated_Decls
             (Bare_Ada_Node (Node.Internal.Node),
              E_Info => Node.Internal.Info);
      begin

         return
           Result : constant Basic_Decl_Array :=
             To_Public_Basic_Decl_Array (Property_Result) do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Associated_Decls;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Pragma_Node_List'Class; Index : Positive) return Pragma_Node
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Pragma_Node;
   end List_Child;

   function Pragma_Node_List_First (Node : Pragma_Node_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Pragma_Node_List_First;

   function Pragma_Node_List_Next
     (Node : Pragma_Node_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Pragma_Node_List_Next;

   function Pragma_Node_List_Has_Element
     (Node : Pragma_Node_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Pragma_Node_List_Has_Element;

   function Pragma_Node_List_Element
     (Node : Pragma_Node_List; Cursor : Positive) return Pragma_Node'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Pragma_Node'(Child.As_Pragma_Node);
   end Pragma_Node_List_Element;

   function P_As_Bool (Node : Private_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Private_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Has_Abstract (Node : Private_Type_Def'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Private_Type_Def_F_Has_Abstract (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Abstract;

   function F_Has_Abstract (Node : Private_Type_Def'Class) return Boolean is
     (Abstract_Node'(Node.F_Has_Abstract).Kind = Ada_Abstract_Present);

   function F_Has_Tagged (Node : Private_Type_Def'Class) return Tagged_Node is
      Result : Bare_Tagged_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Private_Type_Def_F_Has_Tagged (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Tagged;

   function F_Has_Tagged (Node : Private_Type_Def'Class) return Boolean is
     (Tagged_Node'(Node.F_Has_Tagged).Kind = Ada_Tagged_Present);

   function F_Has_Limited (Node : Private_Type_Def'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Private_Type_Def_F_Has_Limited (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Limited;

   function F_Has_Limited (Node : Private_Type_Def'Class) return Boolean is
     (Limited_Node'(Node.F_Has_Limited).Kind = Ada_Limited_Present);

   function P_As_Bool (Node : Protected_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Protected_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Name (Node : Protected_Body'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Protected_Body_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Decls (Node : Protected_Body'Class) return Declarative_Part is
      Result : Bare_Declarative_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Protected_Body_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_End_Name (Node : Protected_Body'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Protected_Body_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Name (Node : Protected_Body_Stub'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Protected_Body_Stub_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Public_Part (Node : Protected_Def'Class) return Public_Part is
      Result : Bare_Public_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Protected_Def_F_Public_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Public_Part;

   function F_Private_Part (Node : Protected_Def'Class) return Private_Part is
      Result : Bare_Private_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Protected_Def_F_Private_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Private_Part;

   function F_End_Name (Node : Protected_Def'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Protected_Def_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Discriminants
     (Node : Protected_Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Protected_Type_Decl_F_Discriminants
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discriminants;

   function F_Interfaces (Node : Protected_Type_Decl'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Protected_Type_Decl_F_Interfaces (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Interfaces;

   function F_Definition
     (Node : Protected_Type_Decl'Class) return Protected_Def
   is
      Result : Bare_Protected_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Protected_Type_Decl_F_Definition (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Definition;

   function F_Prefix (Node : Qual_Expr'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Qual_Expr_F_Prefix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Prefix;

   function F_Suffix (Node : Qual_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Qual_Expr_F_Suffix (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Suffix;

   function F_Quantifier (Node : Quantified_Expr'Class) return Quantifier is
      Result : Bare_Quantifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Quantified_Expr_F_Quantifier (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Quantifier;

   function F_Quantifier
     (Node : Quantified_Expr'Class) return Ada_Quantifier is
     (Quantifier'(Node.F_Quantifier).Kind);

   function F_Loop_Spec (Node : Quantified_Expr'Class) return For_Loop_Spec is
      Result : Bare_For_Loop_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Quantified_Expr_F_Loop_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Loop_Spec;

   function F_Expr (Node : Quantified_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Quantified_Expr_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Exception_Name (Node : Raise_Expr'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Raise_Expr_F_Exception_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Exception_Name;

   function F_Error_Message (Node : Raise_Expr'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Raise_Expr_F_Error_Message (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Error_Message;

   function F_Exception_Name (Node : Raise_Stmt'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Raise_Stmt_F_Exception_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Exception_Name;

   function F_Error_Message (Node : Raise_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Raise_Stmt_F_Error_Message (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Error_Message;

   function F_Range (Node : Range_Constraint'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Range_Constraint_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Range (Node : Range_Spec'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Range_Spec_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Name (Node : Record_Rep_Clause'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Record_Rep_Clause_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_At_Expr (Node : Record_Rep_Clause'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Record_Rep_Clause_F_At_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_At_Expr;

   function F_Components (Node : Record_Rep_Clause'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Record_Rep_Clause_F_Components (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Components;

   function F_Has_Abstract (Node : Record_Type_Def'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Record_Type_Def_F_Has_Abstract (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Abstract;

   function F_Has_Abstract (Node : Record_Type_Def'Class) return Boolean is
     (Abstract_Node'(Node.F_Has_Abstract).Kind = Ada_Abstract_Present);

   function F_Has_Tagged (Node : Record_Type_Def'Class) return Tagged_Node is
      Result : Bare_Tagged_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Record_Type_Def_F_Has_Tagged (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Tagged;

   function F_Has_Tagged (Node : Record_Type_Def'Class) return Boolean is
     (Tagged_Node'(Node.F_Has_Tagged).Kind = Ada_Tagged_Present);

   function F_Has_Limited (Node : Record_Type_Def'Class) return Limited_Node is
      Result : Bare_Limited_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Record_Type_Def_F_Has_Limited (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Limited;

   function F_Has_Limited (Node : Record_Type_Def'Class) return Boolean is
     (Limited_Node'(Node.F_Has_Limited).Kind = Ada_Limited_Present);

   function F_Record_Def (Node : Record_Type_Def'Class) return Base_Record_Def
   is
      Result : Bare_Base_Record_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Record_Type_Def_F_Record_Def (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Record_Def;

   function F_Renamed_Object (Node : Renaming_Clause'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Renaming_Clause_F_Renamed_Object (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renamed_Object;

   function F_Call_Name (Node : Requeue_Stmt'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Requeue_Stmt_F_Call_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Call_Name;

   function F_Has_Abort (Node : Requeue_Stmt'Class) return Abort_Node is
      Result : Bare_Abort_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Requeue_Stmt_F_Has_Abort (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Abort;

   function F_Has_Abort (Node : Requeue_Stmt'Class) return Boolean is
     (Abort_Node'(Node.F_Has_Abort).Kind = Ada_Abort_Present);

   function F_Return_Expr (Node : Return_Stmt'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Return_Stmt_F_Return_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Return_Expr;

   function P_As_Bool (Node : Reverse_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Reverse_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Guards (Node : Select_Stmt'Class) return Select_When_Part_List is
      Result : Bare_Select_When_Part_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Select_Stmt_F_Guards (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Guards;

   function F_Else_Stmts (Node : Select_Stmt'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Select_Stmt_F_Else_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Else_Stmts;

   function F_Abort_Stmts (Node : Select_Stmt'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Select_Stmt_F_Abort_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Abort_Stmts;

   function F_Cond_Expr (Node : Select_When_Part'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Select_When_Part_F_Cond_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Cond_Expr;

   function F_Stmts (Node : Select_When_Part'Class) return Stmt_List is
      Result : Bare_Stmt_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Select_When_Part_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Select_When_Part_List'Class; Index : Positive)
      return Select_When_Part
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Select_When_Part;
   end List_Child;

   function Select_When_Part_List_First
     (Node : Select_When_Part_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Select_When_Part_List_First;

   function Select_When_Part_List_Next
     (Node : Select_When_Part_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Select_When_Part_List_Next;

   function Select_When_Part_List_Has_Element
     (Node : Select_When_Part_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Select_When_Part_List_Has_Element;

   function Select_When_Part_List_Element
     (Node : Select_When_Part_List; Cursor : Positive)
      return Select_When_Part'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Select_When_Part'(Child.As_Select_When_Part);
   end Select_When_Part_List_Element;

   function F_Range (Node : Signed_Int_Type_Def'Class) return Range_Spec is
      Result : Bare_Range_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Signed_Int_Type_Def_F_Range (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Range;

   function F_Name (Node : Single_Protected_Decl'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Single_Protected_Decl_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Interfaces
     (Node : Single_Protected_Decl'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Single_Protected_Decl_F_Interfaces (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Interfaces;

   function F_Definition
     (Node : Single_Protected_Decl'Class) return Protected_Def
   is
      Result : Bare_Protected_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Single_Protected_Decl_F_Definition (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Definition;

   function F_Task_Type
     (Node : Single_Task_Decl'Class) return Single_Task_Type_Decl
   is
      Result : Bare_Single_Task_Type_Decl;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Single_Task_Decl_F_Task_Type (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Task_Type;

   function F_Discriminants
     (Node : Task_Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Task_Type_Decl_F_Discriminants (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discriminants;

   function F_Definition (Node : Task_Type_Decl'Class) return Task_Def is
      Result : Bare_Task_Def;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Task_Type_Decl_F_Definition (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Definition;

   function P_Denoted_Value (Node : String_Literal'Class) return Text_Type is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : Character_Type_Array_Access :=
           Libadalang.Implementation.Extensions.String_Literal_P_Denoted_Value
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return
           Result : constant Text_Type := To_Public_Text_Type (Property_Result)
         do
            Dec_Ref (Property_Result);
         end return;
      end;
   end P_Denoted_Value;

   function F_Decls (Node : Subp_Body'Class) return Declarative_Part is
      Result : Bare_Declarative_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Body_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_Stmts (Node : Subp_Body'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Body_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Subp_Body'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Body_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Overriding (Node : Subp_Body_Stub'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Subp_Body_Stub_F_Overriding (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Overriding;

   function F_Overriding
     (Node : Subp_Body_Stub'Class) return Ada_Overriding_Node is
     (Overriding_Node'(Node.F_Overriding).Kind);

   function F_Subp_Spec (Node : Subp_Body_Stub'Class) return Subp_Spec is
      Result : Bare_Subp_Spec;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Body_Stub_F_Subp_Spec (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Spec;

   function F_Renames (Node : Subp_Renaming_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Subp_Renaming_Decl_F_Renames (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Renames;

   function F_Subp_Kind (Node : Subp_Spec'Class) return Subp_Kind is
      Result : Bare_Subp_Kind;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Spec_F_Subp_Kind (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Kind;

   function F_Subp_Kind (Node : Subp_Spec'Class) return Ada_Subp_Kind is
     (Subp_Kind'(Node.F_Subp_Kind).Kind);

   function F_Subp_Name (Node : Subp_Spec'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Spec_F_Subp_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Name;

   function F_Subp_Params (Node : Subp_Spec'Class) return Params is
      Result : Bare_Params;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Spec_F_Subp_Params (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Params;

   function F_Subp_Returns (Node : Subp_Spec'Class) return Type_Expr is
      Result : Bare_Type_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subp_Spec_F_Subp_Returns (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subp_Returns;

   function F_Subtype (Node : Subtype_Decl'Class) return Subtype_Indication is
      Result : Bare_Subtype_Indication;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subtype_Decl_F_Subtype (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subtype;

   function F_Name (Node : Subunit'Class) return Name is
      Result : Bare_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subunit_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Body (Node : Subunit'Class) return Body_Node is
      Result : Bare_Body_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Subunit_F_Body (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Body;

   function P_Body_Root (Node : Subunit'Class) return Basic_Decl is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Internal_Entity_Basic_Decl :=
           Libadalang.Implementation.Subunit_P_Body_Root
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Wrap_Node (Property_Result.Node, Property_Result.Info)
             .As_Basic_Decl;
      end;
   end P_Body_Root;

   function P_As_Bool (Node : Synchronized_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Synchronized_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function P_As_Bool (Node : Tagged_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Tagged_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Name (Node : Task_Body'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Body_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Decls (Node : Task_Body'Class) return Declarative_Part is
      Result : Bare_Declarative_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Body_F_Decls (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Decls;

   function F_Stmts (Node : Task_Body'Class) return Handled_Stmts is
      Result : Bare_Handled_Stmts;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Body_F_Stmts (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Stmts;

   function F_End_Name (Node : Task_Body'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Body_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Name (Node : Task_Body_Stub'Class) return Defining_Name is
      Result : Bare_Defining_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Body_Stub_F_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Name;

   function F_Interfaces (Node : Task_Def'Class) return Parent_List is
      Result : Bare_Parent_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Def_F_Interfaces (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Interfaces;

   function F_Public_Part (Node : Task_Def'Class) return Public_Part is
      Result : Bare_Public_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Def_F_Public_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Public_Part;

   function F_Private_Part (Node : Task_Def'Class) return Private_Part is
      Result : Bare_Private_Part;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Def_F_Private_Part (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Private_Part;

   function F_End_Name (Node : Task_Def'Class) return End_Name is
      Result : Bare_End_Name;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Task_Def_F_End_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_End_Name;

   function F_Has_All (Node : Type_Access_Def'Class) return All_Node is
      Result : Bare_All_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Type_Access_Def_F_Has_All (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_All;

   function F_Has_All (Node : Type_Access_Def'Class) return Boolean is
     (All_Node'(Node.F_Has_All).Kind = Ada_All_Present);

   function F_Has_Constant (Node : Type_Access_Def'Class) return Constant_Node
   is
      Result : Bare_Constant_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Type_Access_Def_F_Has_Constant (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Constant;

   function F_Has_Constant (Node : Type_Access_Def'Class) return Boolean is
     (Constant_Node'(Node.F_Has_Constant).Kind = Ada_Constant_Present);

   function F_Subtype_Indication
     (Node : Type_Access_Def'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Type_Access_Def_F_Subtype_Indication
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subtype_Indication;

   function F_Op (Node : Un_Op'Class) return Op is
      Result : Bare_Op;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Un_Op_F_Op (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Op;

   function F_Op (Node : Un_Op'Class) return Ada_Op is (Op'(Node.F_Op).Kind);

   function F_Expr (Node : Un_Op'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Un_Op_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Subtype_Indication
     (Node : Unconstrained_Array_Index'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Unconstrained_Array_Index_F_Subtype_Indication
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Subtype_Indication;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Unconstrained_Array_Index_List'Class; Index : Positive)
      return Unconstrained_Array_Index
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Unconstrained_Array_Index;
   end List_Child;

   function Unconstrained_Array_Index_List_First
     (Node : Unconstrained_Array_Index_List) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Unconstrained_Array_Index_List_First;

   function Unconstrained_Array_Index_List_Next
     (Node : Unconstrained_Array_Index_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Unconstrained_Array_Index_List_Next;

   function Unconstrained_Array_Index_List_Has_Element
     (Node : Unconstrained_Array_Index_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Unconstrained_Array_Index_List_Has_Element;

   function Unconstrained_Array_Index_List_Element
     (Node : Unconstrained_Array_Index_List; Cursor : Positive)
      return Unconstrained_Array_Index'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Unconstrained_Array_Index'(Child.As_Unconstrained_Array_Index);
   end Unconstrained_Array_Index_List_Element;

   function F_Types
     (Node : Unconstrained_Array_Indices'Class)
      return Unconstrained_Array_Index_List
   is
      Result : Bare_Unconstrained_Array_Index_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Unconstrained_Array_Indices_F_Types
          (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Types;

   function P_As_Bool (Node : Until_Node'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_Until_Node_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   function F_Packages (Node : Use_Package_Clause'Class) return Name_List is
      Result : Bare_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result :=
        Implementation.Use_Package_Clause_F_Packages (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Packages;

   function F_Has_All (Node : Use_Type_Clause'Class) return All_Node is
      Result : Bare_All_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Use_Type_Clause_F_Has_All (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_All;

   function F_Has_All (Node : Use_Type_Clause'Class) return Boolean is
     (All_Node'(Node.F_Has_All).Kind = Ada_All_Present);

   function F_Types (Node : Use_Type_Clause'Class) return Name_List is
      Result : Bare_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Use_Type_Clause_F_Types (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Types;

   function F_Choices (Node : Variant'Class) return Alternatives_List is
      Result : Bare_Alternatives_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Variant_F_Choices (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Choices;

   function F_Components (Node : Variant'Class) return Component_List is
      Result : Bare_Component_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Variant_F_Components (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Components;

   ----------------
   -- List_Child --
   ----------------

   function List_Child
     (Node : Variant_List'Class; Index : Positive) return Variant
   is
      Result : constant Ada_Node := Node.Child (Index);
   begin
      return Result.As_Variant;
   end List_Child;

   function Variant_List_First (Node : Variant_List) return Positive is
      pragma Unreferenced (Node);
   begin
      return 1;
   end Variant_List_First;

   function Variant_List_Next
     (Node : Variant_List; Cursor : Positive) return Positive
   is
      pragma Unreferenced (Node);
   begin
      return Cursor + 1;
   end Variant_List_Next;

   function Variant_List_Has_Element
     (Node : Variant_List; Cursor : Positive) return Boolean
   is
   begin
      return Cursor in 1 .. Node.Children_Count;
   end Variant_List_Has_Element;

   function Variant_List_Element
     (Node : Variant_List; Cursor : Positive) return Variant'Class
   is
      Child : constant Ada_Node := Node.Child (Cursor);
   begin
      return Variant'(Child.As_Variant);
   end Variant_List_Element;

   function F_Discr_Name (Node : Variant_Part'Class) return Identifier is
      Result : Bare_Identifier;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Variant_Part_F_Discr_Name (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Discr_Name;

   function F_Variant (Node : Variant_Part'Class) return Variant_List is
      Result : Bare_Variant_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.Variant_Part_F_Variant (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Variant;

   function F_Expr (Node : While_Loop_Spec'Class) return Expr is
      Result : Bare_Expr;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.While_Loop_Spec_F_Expr (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Expr;

   function F_Has_Limited (Node : With_Clause'Class) return Limited_Node is
      Result : Bare_Limited_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.With_Clause_F_Has_Limited (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Limited;

   function F_Has_Limited (Node : With_Clause'Class) return Boolean is
     (Limited_Node'(Node.F_Has_Limited).Kind = Ada_Limited_Present);

   function F_Has_Private (Node : With_Clause'Class) return Private_Node is
      Result : Bare_Private_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.With_Clause_F_Has_Private (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Has_Private;

   function F_Has_Private (Node : With_Clause'Class) return Boolean is
     (Private_Node'(Node.F_Has_Private).Kind = Ada_Private_Present);

   function F_Packages (Node : With_Clause'Class) return Name_List is
      Result : Bare_Name_List;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Result := Implementation.With_Clause_F_Packages (Node.Internal.Node);
      return (Internal => (Result, Node.Internal.Info),
         Safety_Net    => Node.Safety_Net);
   end F_Packages;

   function P_As_Bool (Node : With_Private'Class) return Boolean is

   begin
      Check_Safety_Net (Node.Safety_Net);

      declare

         Property_Result : constant Boolean :=
           Libadalang.Implementation.Dispatcher_With_Private_P_As_Bool
             (Bare_Ada_Node (Node.Internal.Node));
      begin

         return Property_Result;
      end;
   end P_As_Bool;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Ada_Node'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Children_Count (Node.Internal.Node);
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index (Node : Ada_Node'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return First_Child_Index (Node.Internal.Node);
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index (Node : Ada_Node'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Last_Child_Index (Node.Internal.Node);
   end Last_Child_Index;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node   : Ada_Node'Class; Index : Positive; Index_In_Bounds : out Boolean;
      Result : out Ada_Node)
   is
      N : Bare_Ada_Node;
   begin
      Check_Safety_Net (Node.Safety_Net);
      Get_Child (Node.Internal.Node, Index, Index_In_Bounds, N);
      Result := Wrap_Node (N, Node.Internal.Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child (Node : Ada_Node'Class; Index : Positive) return Ada_Node is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Wrap_Node (Child (Node.Internal.Node, Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range (Node : Ada_Node'Class) return Source_Location_Range is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Sloc_Range (Node.Internal.Node);
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Ada_Node'Class; Sloc : Source_Location) return Relative_Position
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Compare (Node.Internal.Node, Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Ada_Node'Class; Sloc : Source_Location) return Ada_Node
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Wrap_Node (Lookup (Node.Internal.Node, Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : Ada_Node'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Implementation.Text (Node.Internal.Node);
   end Text;

   ----------------
   -- Debug_Text --
   ----------------

   function Debug_Text (Node : Ada_Node'Class) return String is
   begin
      return Image (Node.Text);
   end Debug_Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range (Node : Ada_Node'Class) return Token_Iterator is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Token_Iterator'
          (Node.As_Ada_Node, Node.Internal.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Ada_Node'Class; Show_Slocs : Boolean := True;
      Line_Prefix : String := "")
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      Print (Node.Internal.Node, Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Node : Ada_Node'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node.Safety_Net);
      PP_Trivia (Node.Internal.Node, Line_Prefix);
   end PP_Trivia;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Ada_Node'Class;
      Visit : access function (Node : Ada_Node'Class) return Visit_Status)
      return Visit_Status
   is
      Info : constant Internal_Entity_Info := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper (Node : Bare_Ada_Node) return Visit_Status is
         Public_Node : constant Ada_Node :=
           Wrap_Node (Bare_Ada_Node (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      Check_Safety_Net (Node.Safety_Net);
      return Traverse (Node.Internal.Node, Wrapper'Access);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Ada_Node'Class;
      Visit : access function (Node : Ada_Node'Class) return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Ada_Node'Class) is
   begin
      Check_Safety_Net (Node.Safety_Net);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia (Node : Ada_Node'Class) return Children_Array
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      declare
         Bare_Result : constant Bare_Children_Array :=
           Children_With_Trivia (Unwrap_Node (Node));
         Result : Children_Array (Bare_Result'Range);
      begin
         for I in Bare_Result'Range loop
            declare
               BR : Bare_Child_Record renames Bare_Result (I);
               R  : Child_Record renames Result (I);
            begin
               case BR.Kind is
                  when Child =>
                     R := (Child, Wrap_Node (BR.Node));
                  when Trivia =>
                     R := (Trivia, BR.Trivia);
               end case;
            end;
         end loop;
         return Result;
      end;
   end Children_With_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Token_Start (Self.Node);
   end First_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference
   is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Next (Tok);
   end Next_Token;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean
   is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Get_Token_Index (Tok).Token <= Self.Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference
   is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Tok;
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Context : in out Analysis_Context) is
   begin
      Context.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Context : in out Analysis_Context) is
   begin
      Inc_Ref (Unwrap_Context (Context));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Context : in out Analysis_Context) is
      Ctx : Internal_Context := Unwrap_Context (Context);
   begin
      Dec_Ref (Ctx);
      Context.Internal := null;
   end Finalize;

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context;
   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context;

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit;
   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit;

   function Wrap_Node
     (Node : Bare_Ada_Node; Info : Internal_Entity_Info := No_Entity_Info)
      return Ada_Node;
   function Unwrap_Node (Node : Ada_Node'Class) return Bare_Ada_Node;
   function Unwrap_Entity (Entity : Ada_Node'Class) return Internal_Entity;

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is
   begin
      Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
         Internal => Internal_Context_Access (Context));
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context is
     (Internal_Context (Context.Internal));

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit is
     ((Internal => Internal_Unit_Access (Unit),
       Context  => Wrap_Context (Context (Unit))));

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit is
     (Internal_Unit (Unit.Internal));

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : Bare_Ada_Node; Info : Internal_Entity_Info := No_Entity_Info)
      return Ada_Node
   is
   begin
      if Node = null then
         return No_Ada_Node;
      end if;

      declare
         Unit    : constant Internal_Unit    := Node.Unit;
         Context : constant Internal_Context := Unit.Context;
      begin
         return
           ((Internal   => (Node, Info),
             Safety_Net =>
               (Context => Context, Context_Serial => Context.Serial_Number,
                Unit    => Unit, Unit_Version => Unit.Unit_Version)));
      end;
   end Wrap_Node;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node (Node : Ada_Node'Class) return Bare_Ada_Node is
     (Node.Internal.Node);

   -------------------
   -- Unwrap_Entity --
   -------------------

   function Unwrap_Entity (Entity : Ada_Node'Class) return Internal_Entity is
     ((Entity.Internal));

----------------
-- Is_Keyword --
----------------

   function Is_Keyword
     (Token : Token_Reference; Version : Language_Version) return Boolean
   is
      TDH   : constant Token_Data_Handler_Access := Get_Token_TDH (Token);
      Index : constant Token_Or_Trivia_Index     := Get_Token_Index (Token);
   begin
      return Libadalang.Lexer.Is_Keyword (TDH.all, Index, Version);
   end Is_Keyword;

begin
   Public_Converters.Wrap_Context   := Wrap_Context'Access;
   Public_Converters.Unwrap_Context := Unwrap_Context'Access;
   Public_Converters.Wrap_Unit      := Wrap_Unit'Access;
   Public_Converters.Unwrap_Unit    := Unwrap_Unit'Access;
   Public_Converters.Wrap_Node      := Wrap_Node'Access;
   Public_Converters.Unwrap_Node    := Unwrap_Node'Access;
   Public_Converters.Unwrap_Entity  := Unwrap_Entity'Access;
end Libadalang.Analysis;
