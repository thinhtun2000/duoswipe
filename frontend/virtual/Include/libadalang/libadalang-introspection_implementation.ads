with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Libadalang.Implementation; use Libadalang.Implementation;
with Libadalang.Common;         use Libadalang.Common;

private package Libadalang.Introspection_Implementation is

   use Support.Text;

   ------------------------
   -- Polymorphic values --
   ------------------------

   --  TODO: for now, support only value types that are required to represent
   --  default values for property arguments.

   subtype Internal_Value_Kind is Any_Value_Kind with
        Static_Predicate => Internal_Value_Kind in None | Boolean_Value |
            Integer_Value | Character_Value | Analysis_Unit_Kind_Value |
            Lookup_Kind_Value | Find_All_Mode_Value | Ref_Result_Kind_Value |
            Grammar_Rule_Value | Node_Value;

   type Internal_Value (Kind : Internal_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Character_Value =>
            Character_Value : Character_Type;

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

         when Node_Value =>
            Node_Value : Internal_Entity;
      end case;
   end record;

   No_Internal_Value : constant Internal_Value := (Kind => None);

   type Internal_Value_Array is array (Positive range <>) of Internal_Value;

   function As_Boolean (Self : Internal_Value) return Boolean;
   function Create_Boolean (Value : Boolean) return Internal_Value is
     ((Kind => Boolean_Value, Boolean_Value => Value));

   function As_Integer (Self : Internal_Value) return Integer;
   function Create_Integer (Value : Integer) return Internal_Value is
     ((Kind => Integer_Value, Integer_Value => Value));

   function As_Character (Self : Internal_Value) return Character_Type;
   function Create_Character (Value : Character_Type) return Internal_Value is
     ((Kind => Character_Value, Character_Value => Value));

   function As_Node (Self : Internal_Value) return Internal_Entity;
   function Create_Node (Value : Internal_Entity) return Internal_Value is
     ((Kind => Node_Value, Node_Value => Value));

   function As_Analysis_Unit_Kind
     (Self : Internal_Value) return Analysis_Unit_Kind;
   function Create_Analysis_Unit_Kind
     (Value : Analysis_Unit_Kind) return Internal_Value is
     ((Kind => Analysis_Unit_Kind_Value, Analysis_Unit_Kind_Value => Value));
   function As_Lookup_Kind (Self : Internal_Value) return Lookup_Kind;
   function Create_Lookup_Kind (Value : Lookup_Kind) return Internal_Value is
     ((Kind => Lookup_Kind_Value, Lookup_Kind_Value => Value));
   function As_Find_All_Mode (Self : Internal_Value) return Find_All_Mode;
   function Create_Find_All_Mode
     (Value : Find_All_Mode) return Internal_Value is
     ((Kind => Find_All_Mode_Value, Find_All_Mode_Value => Value));
   function As_Ref_Result_Kind (Self : Internal_Value) return Ref_Result_Kind;
   function Create_Ref_Result_Kind
     (Value : Ref_Result_Kind) return Internal_Value is
     ((Kind => Ref_Result_Kind_Value, Ref_Result_Kind_Value => Value));
   function As_Grammar_Rule (Self : Internal_Value) return Grammar_Rule;
   function Create_Grammar_Rule (Value : Grammar_Rule) return Internal_Value is
     ((Kind => Grammar_Rule_Value, Grammar_Rule_Value => Value));

   -----------------------
   -- Descriptor tables --
   -----------------------

   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;

   ------------------------------
   -- Syntax field descriptors --
   ------------------------------

   type Syntax_Field_Descriptor (Name_Length : Natural) is record
      Field_Type : Node_Type_Id;
      Name       : String (1 .. Name_Length);
   end record;
   --  General description of a field (independent of field implementations)

   type Syntax_Field_Descriptor_Access is
     access constant Syntax_Field_Descriptor;

   --  Descriptors for syntax fields

   Desc_For_Constrained_Array_Indices_F_List : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Constraint_List_Type_Id,
      Name        => "list");

   Desc_For_Unconstrained_Array_Indices_F_Types : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5,
      Field_Type  => Common.Unconstrained_Array_Index_List_Type_Id,
      Name        => "types");

   Desc_For_Aspect_Assoc_F_Id : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Name_Type_Id, Name => "id");

   Desc_For_Aspect_Assoc_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_At_Clause_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Base_Id_Type_Id, Name => "name");

   Desc_For_At_Clause_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Attribute_Def_Clause_F_Attribute_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Name_Type_Id,
      Name        => "attribute_expr");

   Desc_For_Attribute_Def_Clause_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Enum_Rep_Clause_F_Type_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Name_Type_Id,
      Name        => "type_name");

   Desc_For_Enum_Rep_Clause_F_Aggregate : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Base_Aggregate_Type_Id,
      Name        => "aggregate");

   Desc_For_Record_Rep_Clause_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "name");

   Desc_For_Record_Rep_Clause_F_At_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Expr_Type_Id, Name => "at_expr");

   Desc_For_Record_Rep_Clause_F_Components : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Ada_Node_List_Type_Id,
      Name        => "components");

   Desc_For_Aspect_Spec_F_Aspect_Assocs : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Aspect_Assoc_List_Type_Id,
      Name        => "aspect_assocs");

   Desc_For_Contract_Case_Assoc_F_Guard : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "guard");

   Desc_For_Contract_Case_Assoc_F_Consequence : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Expr_Type_Id,
      Name        => "consequence");

   Desc_For_Pragma_Argument_Assoc_F_Id : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Identifier_Type_Id, Name => "id");

   Desc_For_Pragma_Argument_Assoc_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Entry_Spec_F_Entry_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "entry_name");

   Desc_For_Entry_Spec_F_Family_Type : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "family_type");

   Desc_For_Entry_Spec_F_Entry_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Params_Type_Id,
      Name        => "entry_params");

   Desc_For_Subp_Spec_F_Subp_Kind : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Subp_Kind_Type_Id,
      Name        => "subp_kind");

   Desc_For_Subp_Spec_F_Subp_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "subp_name");

   Desc_For_Subp_Spec_F_Subp_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Params_Type_Id,
      Name        => "subp_params");

   Desc_For_Subp_Spec_F_Subp_Returns : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Type_Expr_Type_Id,
      Name        => "subp_returns");

   Desc_For_Component_List_F_Components : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Ada_Node_List_Type_Id,
      Name        => "components");

   Desc_For_Component_List_F_Variant_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Variant_Part_Type_Id,
      Name        => "variant_part");

   Desc_For_Known_Discriminant_Part_F_Discr_Specs : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Discriminant_Spec_List_Type_Id,
      Name        => "discr_specs");

   Desc_For_Entry_Completion_Formal_Params_F_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Params_Type_Id, Name => "params");

   Desc_For_Generic_Formal_Part_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Ada_Node_List_Type_Id,
      Name        => "decls");

   Desc_For_Base_Record_Def_F_Components : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Component_List_Type_Id,
      Name        => "components");

   Desc_For_Aggregate_Assoc_F_Designators : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Alternatives_List_Type_Id,
      Name        => "designators");

   Desc_For_Aggregate_Assoc_F_R_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Expr_Type_Id, Name => "r_expr");

   Desc_For_Discriminant_Assoc_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Discriminant_Choice_List_Type_Id,
      Name        => "ids");

   Desc_For_Discriminant_Assoc_F_Discr_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Expr_Type_Id,
      Name        => "discr_expr");

   Desc_For_Param_Assoc_F_Designator : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "designator");

   Desc_For_Param_Assoc_F_R_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Expr_Type_Id, Name => "r_expr");

   Desc_For_Basic_Decl_F_Aspects : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Aspect_Spec_Type_Id,
      Name        => "aspects");

   Desc_For_Component_Decl_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Defining_Name_List_Type_Id,
      Name        => "ids");

   Desc_For_Component_Decl_F_Component_Def : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Component_Def_Type_Id,
      Name        => "component_def");

   Desc_For_Component_Decl_F_Default_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Expr_Type_Id,
      Name        => "default_expr");

   Desc_For_Discriminant_Spec_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Defining_Name_List_Type_Id,
      Name        => "ids");

   Desc_For_Discriminant_Spec_F_Type_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Type_Expr_Type_Id,
      Name        => "type_expr");

   Desc_For_Discriminant_Spec_F_Default_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Expr_Type_Id,
      Name        => "default_expr");

   Desc_For_Generic_Formal_F_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Basic_Decl_Type_Id,
      Name        => "decl");

   Desc_For_Param_Spec_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Defining_Name_List_Type_Id,
      Name        => "ids");

   Desc_For_Param_Spec_F_Has_Aliased : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Aliased_Node_Type_Id,
      Name        => "has_aliased");

   Desc_For_Param_Spec_F_Mode : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Mode_Type_Id, Name => "mode");

   Desc_For_Param_Spec_F_Type_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Type_Expr_Type_Id,
      Name        => "type_expr");

   Desc_For_Param_Spec_F_Default_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Expr_Type_Id,
      Name        => "default_expr");

   Desc_For_Base_Package_Decl_F_Package_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "package_name");

   Desc_For_Base_Package_Decl_F_Public_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Public_Part_Type_Id,
      Name        => "public_part");

   Desc_For_Base_Package_Decl_F_Private_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Private_Part_Type_Id,
      Name        => "private_part");

   Desc_For_Base_Package_Decl_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Base_Type_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Subtype_Decl_F_Subtype : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Subtype_Indication_Type_Id,
      Name        => "subtype");

   Desc_For_Incomplete_Type_Decl_F_Discriminants : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Discriminant_Part_Type_Id,
      Name        => "discriminants");

   Desc_For_Incomplete_Tagged_Type_Decl_F_Has_Abstract : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Abstract_Node_Type_Id,
      Name        => "has_abstract");

   Desc_For_Protected_Type_Decl_F_Discriminants : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Discriminant_Part_Type_Id,
      Name        => "discriminants");

   Desc_For_Protected_Type_Decl_F_Interfaces : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Parent_List_Type_Id,
      Name        => "interfaces");

   Desc_For_Protected_Type_Decl_F_Definition : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Protected_Def_Type_Id,
      Name        => "definition");

   Desc_For_Task_Type_Decl_F_Discriminants : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Discriminant_Part_Type_Id,
      Name        => "discriminants");

   Desc_For_Task_Type_Decl_F_Definition : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Task_Def_Type_Id,
      Name        => "definition");

   Desc_For_Type_Decl_F_Discriminants : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Discriminant_Part_Type_Id,
      Name        => "discriminants");

   Desc_For_Type_Decl_F_Type_Def : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.Type_Def_Type_Id,
      Name        => "type_def");

   Desc_For_Classic_Subp_Decl_F_Overriding : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Overriding_Node_Type_Id,
      Name        => "overriding");

   Desc_For_Classic_Subp_Decl_F_Subp_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Subp_Spec_Type_Id,
      Name        => "subp_spec");

   Desc_For_Formal_Subp_Decl_F_Default_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Expr_Type_Id,
      Name        => "default_expr");

   Desc_For_Entry_Decl_F_Overriding : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Overriding_Node_Type_Id,
      Name        => "overriding");

   Desc_For_Entry_Decl_F_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Entry_Spec_Type_Id,
      Name        => "spec");

   Desc_For_Enum_Literal_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Generic_Subp_Internal_F_Subp_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Subp_Spec_Type_Id,
      Name        => "subp_spec");

   Desc_For_Base_Subp_Body_F_Overriding : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Overriding_Node_Type_Id,
      Name        => "overriding");

   Desc_For_Base_Subp_Body_F_Subp_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Subp_Spec_Type_Id,
      Name        => "subp_spec");

   Desc_For_Expr_Function_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Subp_Body_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Declarative_Part_Type_Id,
      Name        => "decls");

   Desc_For_Subp_Body_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Subp_Body_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Subp_Renaming_Decl_F_Renames : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Renaming_Clause_Type_Id,
      Name        => "renames");

   Desc_For_Package_Body_Stub_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Protected_Body_Stub_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Subp_Body_Stub_F_Overriding : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Overriding_Node_Type_Id,
      Name        => "overriding");

   Desc_For_Subp_Body_Stub_F_Subp_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Subp_Spec_Type_Id,
      Name        => "subp_spec");

   Desc_For_Task_Body_Stub_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Entry_Body_F_Entry_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "entry_name");

   Desc_For_Entry_Body_F_Index_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Entry_Index_Spec_Type_Id,
      Name        => "index_spec");

   Desc_For_Entry_Body_F_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6,
      Field_Type  => Common.Entry_Completion_Formal_Params_Type_Id,
      Name        => "params");

   Desc_For_Entry_Body_F_Barrier : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Expr_Type_Id, Name => "barrier");

   Desc_For_Entry_Body_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Declarative_Part_Type_Id,
      Name        => "decls");

   Desc_For_Entry_Body_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Entry_Body_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Package_Body_F_Package_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "package_name");

   Desc_For_Package_Body_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Declarative_Part_Type_Id,
      Name        => "decls");

   Desc_For_Package_Body_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Package_Body_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Protected_Body_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Protected_Body_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Declarative_Part_Type_Id,
      Name        => "decls");

   Desc_For_Protected_Body_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Task_Body_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Task_Body_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Declarative_Part_Type_Id,
      Name        => "decls");

   Desc_For_Task_Body_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Task_Body_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Entry_Index_Spec_F_Id : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "id");

   Desc_For_Entry_Index_Spec_F_Subtype : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "subtype");

   Desc_For_Exception_Decl_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Defining_Name_List_Type_Id,
      Name        => "ids");

   Desc_For_Exception_Decl_F_Renames : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Renaming_Clause_Type_Id,
      Name        => "renames");

   Desc_For_Exception_Handler_F_Exception_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "exception_name");

   Desc_For_Exception_Handler_F_Handled_Exceptions : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 18, Field_Type => Common.Alternatives_List_Type_Id,
      Name        => "handled_exceptions");

   Desc_For_Exception_Handler_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "stmts");

   Desc_For_For_Loop_Var_Decl_F_Id : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "id");

   Desc_For_For_Loop_Var_Decl_F_Id_Type : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Subtype_Indication_Type_Id,
      Name        => "id_type");

   Desc_For_Generic_Decl_F_Formal_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Generic_Formal_Part_Type_Id,
      Name        => "formal_part");

   Desc_For_Generic_Package_Decl_F_Package_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Generic_Package_Internal_Type_Id,
      Name        => "package_decl");

   Desc_For_Generic_Subp_Decl_F_Subp_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Generic_Subp_Internal_Type_Id,
      Name        => "subp_decl");

   Desc_For_Generic_Package_Instantiation_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Generic_Package_Instantiation_F_Generic_Pkg_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 16, Field_Type => Common.Name_Type_Id,
      Name        => "generic_pkg_name");

   Desc_For_Generic_Package_Instantiation_F_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Assoc_List_Type_Id,
      Name        => "params");

   Desc_For_Generic_Subp_Instantiation_F_Overriding : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Overriding_Node_Type_Id,
      Name        => "overriding");

   Desc_For_Generic_Subp_Instantiation_F_Kind : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Subp_Kind_Type_Id,
      Name        => "kind");

   Desc_For_Generic_Subp_Instantiation_F_Subp_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "subp_name");

   Desc_For_Generic_Subp_Instantiation_F_Generic_Subp_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 17, Field_Type => Common.Name_Type_Id,
      Name        => "generic_subp_name");

   Desc_For_Generic_Subp_Instantiation_F_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Assoc_List_Type_Id,
      Name        => "params");

   Desc_For_Generic_Package_Renaming_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Generic_Package_Renaming_Decl_F_Renames : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Name_Type_Id, Name => "renames");

   Desc_For_Generic_Subp_Renaming_Decl_F_Kind : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Subp_Kind_Type_Id,
      Name        => "kind");

   Desc_For_Generic_Subp_Renaming_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Generic_Subp_Renaming_Decl_F_Renames : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Name_Type_Id, Name => "renames");

   Desc_For_Label_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Named_Stmt_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Number_Decl_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Defining_Name_List_Type_Id,
      Name        => "ids");

   Desc_For_Number_Decl_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Object_Decl_F_Ids : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 3, Field_Type => Common.Defining_Name_List_Type_Id,
      Name        => "ids");

   Desc_For_Object_Decl_F_Has_Aliased : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Aliased_Node_Type_Id,
      Name        => "has_aliased");

   Desc_For_Object_Decl_F_Has_Constant : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Constant_Node_Type_Id,
      Name        => "has_constant");

   Desc_For_Object_Decl_F_Mode : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Mode_Type_Id, Name => "mode");

   Desc_For_Object_Decl_F_Type_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Type_Expr_Type_Id,
      Name        => "type_expr");

   Desc_For_Object_Decl_F_Default_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Expr_Type_Id,
      Name        => "default_expr");

   Desc_For_Object_Decl_F_Renaming_Clause : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 15, Field_Type => Common.Renaming_Clause_Type_Id,
      Name        => "renaming_clause");

   Desc_For_Package_Renaming_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Package_Renaming_Decl_F_Renames : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Renaming_Clause_Type_Id,
      Name        => "renames");

   Desc_For_Single_Protected_Decl_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Defining_Name_Type_Id,
      Name        => "name");

   Desc_For_Single_Protected_Decl_F_Interfaces : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Parent_List_Type_Id,
      Name        => "interfaces");

   Desc_For_Single_Protected_Decl_F_Definition : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Protected_Def_Type_Id,
      Name        => "definition");

   Desc_For_Single_Task_Decl_F_Task_Type : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Single_Task_Type_Decl_Type_Id,
      Name        => "task_type");

   Desc_For_Case_Stmt_Alternative_F_Choices : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Alternatives_List_Type_Id,
      Name        => "choices");

   Desc_For_Case_Stmt_Alternative_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "stmts");

   Desc_For_Compilation_Unit_F_Prelude : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Ada_Node_List_Type_Id,
      Name        => "prelude");

   Desc_For_Compilation_Unit_F_Body : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Ada_Node_Type_Id, Name => "body");

   Desc_For_Compilation_Unit_F_Pragmas : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Pragma_Node_List_Type_Id,
      Name        => "pragmas");

   Desc_For_Component_Clause_F_Id : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Identifier_Type_Id, Name => "id");

   Desc_For_Component_Clause_F_Position : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.Expr_Type_Id, Name => "position");

   Desc_For_Component_Clause_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Component_Def_F_Has_Aliased : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Aliased_Node_Type_Id,
      Name        => "has_aliased");

   Desc_For_Component_Def_F_Has_Constant : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Constant_Node_Type_Id,
      Name        => "has_constant");

   Desc_For_Component_Def_F_Type_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Type_Expr_Type_Id,
      Name        => "type_expr");

   Desc_For_Delta_Constraint_F_Digits : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Expr_Type_Id, Name => "digits");

   Desc_For_Delta_Constraint_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Digits_Constraint_F_Digits : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Expr_Type_Id, Name => "digits");

   Desc_For_Digits_Constraint_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Discriminant_Constraint_F_Constraints : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Assoc_List_Type_Id,
      Name        => "constraints");

   Desc_For_Index_Constraint_F_Constraints : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Constraint_List_Type_Id,
      Name        => "constraints");

   Desc_For_Range_Constraint_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Declarative_Part_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Ada_Node_List_Type_Id,
      Name        => "decls");

   Desc_For_Elsif_Expr_Part_F_Cond_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "cond_expr");

   Desc_For_Elsif_Expr_Part_F_Then_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "then_expr");

   Desc_For_Elsif_Stmt_Part_F_Cond_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "cond_expr");

   Desc_For_Elsif_Stmt_Part_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "stmts");

   Desc_For_Allocator_F_Subpool : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Name_Type_Id, Name => "subpool");

   Desc_For_Allocator_F_Type_Or_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "type_or_expr");

   Desc_For_Base_Aggregate_F_Ancestor_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Expr_Type_Id,
      Name        => "ancestor_expr");

   Desc_For_Base_Aggregate_F_Assocs : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Assoc_List_Type_Id,
      Name        => "assocs");

   Desc_For_Bin_Op_F_Left : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "left");

   Desc_For_Bin_Op_F_Op : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Op_Type_Id, Name => "op");

   Desc_For_Bin_Op_F_Right : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Expr_Type_Id, Name => "right");

   Desc_For_Case_Expr_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Case_Expr_F_Cases : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5,
      Field_Type  => Common.Case_Expr_Alternative_List_Type_Id,
      Name        => "cases");

   Desc_For_Case_Expr_Alternative_F_Choices : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Alternatives_List_Type_Id,
      Name        => "choices");

   Desc_For_Case_Expr_Alternative_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Contract_Cases_F_Contract_Cases : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Contract_Case_Assoc_List_Type_Id,
      Name        => "contract_cases");

   Desc_For_If_Expr_F_Cond_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "cond_expr");

   Desc_For_If_Expr_F_Then_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "then_expr");

   Desc_For_If_Expr_F_Alternatives : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Elsif_Expr_Part_List_Type_Id,
      Name        => "alternatives");

   Desc_For_If_Expr_F_Else_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "else_expr");

   Desc_For_Membership_Expr_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Membership_Expr_F_Op : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Op_Type_Id, Name => "op");

   Desc_For_Membership_Expr_F_Membership_Exprs : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 16, Field_Type => Common.Expr_Alternatives_List_Type_Id,
      Name        => "membership_exprs");

   Desc_For_Attribute_Ref_F_Prefix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Name_Type_Id, Name => "prefix");

   Desc_For_Attribute_Ref_F_Attribute : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Identifier_Type_Id,
      Name        => "attribute");

   Desc_For_Attribute_Ref_F_Args : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Ada_Node_Type_Id, Name => "args");

   Desc_For_Call_Expr_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "name");

   Desc_For_Call_Expr_F_Suffix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "suffix");

   Desc_For_Defining_Name_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "name");

   Desc_For_Discrete_Subtype_Name_F_Subtype : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7,
      Field_Type  => Common.Discrete_Subtype_Indication_Type_Id,
      Name        => "subtype");

   Desc_For_Dotted_Name_F_Prefix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Name_Type_Id, Name => "prefix");

   Desc_For_Dotted_Name_F_Suffix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Base_Id_Type_Id,
      Name        => "suffix");

   Desc_For_End_Name_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "name");

   Desc_For_Explicit_Deref_F_Prefix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Name_Type_Id, Name => "prefix");

   Desc_For_Qual_Expr_F_Prefix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Name_Type_Id, Name => "prefix");

   Desc_For_Qual_Expr_F_Suffix : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Expr_Type_Id, Name => "suffix");

   Desc_For_Paren_Expr_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Quantified_Expr_F_Quantifier : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Quantifier_Type_Id,
      Name        => "quantifier");

   Desc_For_Quantified_Expr_F_Loop_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.For_Loop_Spec_Type_Id,
      Name        => "loop_spec");

   Desc_For_Quantified_Expr_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Raise_Expr_F_Exception_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Name_Type_Id,
      Name        => "exception_name");

   Desc_For_Raise_Expr_F_Error_Message : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Expr_Type_Id,
      Name        => "error_message");

   Desc_For_Un_Op_F_Op : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Op_Type_Id, Name => "op");

   Desc_For_Un_Op_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Handled_Stmts_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "stmts");

   Desc_For_Handled_Stmts_F_Exceptions : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Ada_Node_List_Type_Id,
      Name        => "exceptions");

   Desc_For_Library_Item_F_Has_Private : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Private_Node_Type_Id,
      Name        => "has_private");

   Desc_For_Library_Item_F_Item : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Basic_Decl_Type_Id,
      Name        => "item");

   Desc_For_For_Loop_Spec_F_Var_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.For_Loop_Var_Decl_Type_Id,
      Name        => "var_decl");

   Desc_For_For_Loop_Spec_F_Loop_Type : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Iter_Type_Type_Id,
      Name        => "loop_type");

   Desc_For_For_Loop_Spec_F_Has_Reverse : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Reverse_Node_Type_Id,
      Name        => "has_reverse");

   Desc_For_For_Loop_Spec_F_Iter_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Ada_Node_Type_Id,
      Name        => "iter_expr");

   Desc_For_While_Loop_Spec_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Params_F_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Param_Spec_List_Type_Id,
      Name        => "params");

   Desc_For_Pragma_Node_F_Id : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 2, Field_Type => Common.Identifier_Type_Id, Name => "id");

   Desc_For_Pragma_Node_F_Args : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Base_Assoc_List_Type_Id,
      Name        => "args");

   Desc_For_Protected_Def_F_Public_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Public_Part_Type_Id,
      Name        => "public_part");

   Desc_For_Protected_Def_F_Private_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Private_Part_Type_Id,
      Name        => "private_part");

   Desc_For_Protected_Def_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Range_Spec_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Expr_Type_Id, Name => "range");

   Desc_For_Renaming_Clause_F_Renamed_Object : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Name_Type_Id,
      Name        => "renamed_object");

   Desc_For_Select_When_Part_F_Cond_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "cond_expr");

   Desc_For_Select_When_Part_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "stmts");

   Desc_For_Accept_Stmt_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Identifier_Type_Id,
      Name        => "name");

   Desc_For_Accept_Stmt_F_Entry_Index_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 16, Field_Type => Common.Expr_Type_Id,
      Name        => "entry_index_expr");

   Desc_For_Accept_Stmt_F_Params : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6,
      Field_Type  => Common.Entry_Completion_Formal_Params_Type_Id,
      Name        => "params");

   Desc_For_Accept_Stmt_With_Stmts_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Accept_Stmt_With_Stmts_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Base_Loop_Stmt_F_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Loop_Spec_Type_Id,
      Name        => "spec");

   Desc_For_Base_Loop_Stmt_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "stmts");

   Desc_For_Base_Loop_Stmt_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Begin_Block_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Begin_Block_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Decl_Block_F_Decls : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Declarative_Part_Type_Id,
      Name        => "decls");

   Desc_For_Decl_Block_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_Decl_Block_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Case_Stmt_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Case_Stmt_F_Alternatives : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12,
      Field_Type  => Common.Case_Stmt_Alternative_List_Type_Id,
      Name        => "alternatives");

   Desc_For_Extended_Return_Stmt_F_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4,
      Field_Type  => Common.Extended_Return_Stmt_Object_Decl_Type_Id,
      Name        => "decl");

   Desc_For_Extended_Return_Stmt_F_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Handled_Stmts_Type_Id,
      Name        => "stmts");

   Desc_For_If_Stmt_F_Cond_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "cond_expr");

   Desc_For_If_Stmt_F_Then_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "then_stmts");

   Desc_For_If_Stmt_F_Alternatives : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Elsif_Stmt_Part_List_Type_Id,
      Name        => "alternatives");

   Desc_For_If_Stmt_F_Else_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "else_stmts");

   Desc_For_Named_Stmt_F_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Named_Stmt_Decl_Type_Id,
      Name        => "decl");

   Desc_For_Named_Stmt_F_Stmt : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Composite_Stmt_Type_Id,
      Name        => "stmt");

   Desc_For_Select_Stmt_F_Guards : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Select_When_Part_List_Type_Id,
      Name        => "guards");

   Desc_For_Select_Stmt_F_Else_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "else_stmts");

   Desc_For_Select_Stmt_F_Abort_Stmts : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Stmt_List_Type_Id,
      Name        => "abort_stmts");

   Desc_For_Abort_Stmt_F_Names : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Name_List_Type_Id,
      Name        => "names");

   Desc_For_Assign_Stmt_F_Dest : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "dest");

   Desc_For_Assign_Stmt_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Call_Stmt_F_Call : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "call");

   Desc_For_Delay_Stmt_F_Has_Until : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Until_Node_Type_Id,
      Name        => "has_until");

   Desc_For_Delay_Stmt_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Exit_Stmt_F_Loop_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Identifier_Type_Id,
      Name        => "loop_name");

   Desc_For_Exit_Stmt_F_Cond_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Expr_Type_Id,
      Name        => "cond_expr");

   Desc_For_Goto_Stmt_F_Label_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Name_Type_Id,
      Name        => "label_name");

   Desc_For_Label_F_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Label_Decl_Type_Id,
      Name        => "decl");

   Desc_For_Raise_Stmt_F_Exception_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Name_Type_Id,
      Name        => "exception_name");

   Desc_For_Raise_Stmt_F_Error_Message : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Expr_Type_Id,
      Name        => "error_message");

   Desc_For_Requeue_Stmt_F_Call_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Name_Type_Id,
      Name        => "call_name");

   Desc_For_Requeue_Stmt_F_Has_Abort : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Abort_Node_Type_Id,
      Name        => "has_abort");

   Desc_For_Return_Stmt_F_Return_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Expr_Type_Id,
      Name        => "return_expr");

   Desc_For_Subunit_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "name");

   Desc_For_Subunit_F_Body : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Body_Node_Type_Id,
      Name        => "body");

   Desc_For_Task_Def_F_Interfaces : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Parent_List_Type_Id,
      Name        => "interfaces");

   Desc_For_Task_Def_F_Public_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Public_Part_Type_Id,
      Name        => "public_part");

   Desc_For_Task_Def_F_Private_Part : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Private_Part_Type_Id,
      Name        => "private_part");

   Desc_For_Task_Def_F_End_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.End_Name_Type_Id,
      Name        => "end_name");

   Desc_For_Access_Def_F_Has_Not_Null : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Not_Null_Type_Id,
      Name        => "has_not_null");

   Desc_For_Access_To_Subp_Def_F_Has_Protected : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Protected_Node_Type_Id,
      Name        => "has_protected");

   Desc_For_Access_To_Subp_Def_F_Subp_Spec : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Subp_Spec_Type_Id,
      Name        => "subp_spec");

   Desc_For_Anonymous_Type_Access_Def_F_Type_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Base_Type_Decl_Type_Id,
      Name        => "type_decl");

   Desc_For_Type_Access_Def_F_Has_All : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.All_Node_Type_Id,
      Name        => "has_all");

   Desc_For_Type_Access_Def_F_Has_Constant : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Constant_Node_Type_Id,
      Name        => "has_constant");

   Desc_For_Type_Access_Def_F_Subtype_Indication : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 18, Field_Type => Common.Subtype_Indication_Type_Id,
      Name        => "subtype_indication");

   Desc_For_Array_Type_Def_F_Indices : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Array_Indices_Type_Id,
      Name        => "indices");

   Desc_For_Array_Type_Def_F_Component_Type : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Component_Def_Type_Id,
      Name        => "component_type");

   Desc_For_Derived_Type_Def_F_Has_Abstract : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Abstract_Node_Type_Id,
      Name        => "has_abstract");

   Desc_For_Derived_Type_Def_F_Has_Limited : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Limited_Node_Type_Id,
      Name        => "has_limited");

   Desc_For_Derived_Type_Def_F_Has_Synchronized : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 16, Field_Type => Common.Synchronized_Node_Type_Id,
      Name        => "has_synchronized");

   Desc_For_Derived_Type_Def_F_Subtype_Indication : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 18, Field_Type => Common.Subtype_Indication_Type_Id,
      Name        => "subtype_indication");

   Desc_For_Derived_Type_Def_F_Interfaces : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Parent_List_Type_Id,
      Name        => "interfaces");

   Desc_For_Derived_Type_Def_F_Record_Extension : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 16, Field_Type => Common.Base_Record_Def_Type_Id,
      Name        => "record_extension");

   Desc_For_Derived_Type_Def_F_Has_With_Private : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 16, Field_Type => Common.With_Private_Type_Id,
      Name        => "has_with_private");

   Desc_For_Enum_Type_Def_F_Enum_Literals : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 13, Field_Type => Common.Enum_Literal_Decl_List_Type_Id,
      Name        => "enum_literals");

   Desc_For_Interface_Type_Def_F_Interface_Kind : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 14, Field_Type => Common.Interface_Kind_Type_Id,
      Name        => "interface_kind");

   Desc_For_Interface_Type_Def_F_Interfaces : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Parent_List_Type_Id,
      Name        => "interfaces");

   Desc_For_Mod_Int_Type_Def_F_Expr : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Expr_Type_Id, Name => "expr");

   Desc_For_Private_Type_Def_F_Has_Abstract : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Abstract_Node_Type_Id,
      Name        => "has_abstract");

   Desc_For_Private_Type_Def_F_Has_Tagged : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Tagged_Node_Type_Id,
      Name        => "has_tagged");

   Desc_For_Private_Type_Def_F_Has_Limited : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Limited_Node_Type_Id,
      Name        => "has_limited");

   Desc_For_Decimal_Fixed_Point_Def_F_Delta : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Expr_Type_Id, Name => "delta");

   Desc_For_Decimal_Fixed_Point_Def_F_Digits : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 6, Field_Type => Common.Expr_Type_Id, Name => "digits");

   Desc_For_Decimal_Fixed_Point_Def_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Floating_Point_Def_F_Num_Digits : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Expr_Type_Id,
      Name        => "num_digits");

   Desc_For_Floating_Point_Def_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Ordinary_Fixed_Point_Def_F_Delta : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Expr_Type_Id, Name => "delta");

   Desc_For_Ordinary_Fixed_Point_Def_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Record_Type_Def_F_Has_Abstract : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Abstract_Node_Type_Id,
      Name        => "has_abstract");

   Desc_For_Record_Type_Def_F_Has_Tagged : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Tagged_Node_Type_Id,
      Name        => "has_tagged");

   Desc_For_Record_Type_Def_F_Has_Limited : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Limited_Node_Type_Id,
      Name        => "has_limited");

   Desc_For_Record_Type_Def_F_Record_Def : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Base_Record_Def_Type_Id,
      Name        => "record_def");

   Desc_For_Signed_Int_Type_Def_F_Range : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Range_Spec_Type_Id,
      Name        => "range");

   Desc_For_Anonymous_Type_F_Type_Decl : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 9, Field_Type => Common.Anonymous_Type_Decl_Type_Id,
      Name        => "type_decl");

   Desc_For_Subtype_Indication_F_Has_Not_Null : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 12, Field_Type => Common.Not_Null_Type_Id,
      Name        => "has_not_null");

   Desc_For_Subtype_Indication_F_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 4, Field_Type => Common.Name_Type_Id, Name => "name");

   Desc_For_Subtype_Indication_F_Constraint : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Constraint_Type_Id,
      Name        => "constraint");

   Desc_For_Unconstrained_Array_Index_F_Subtype_Indication : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 18, Field_Type => Common.Subtype_Indication_Type_Id,
      Name        => "subtype_indication");

   Desc_For_Use_Package_Clause_F_Packages : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.Name_List_Type_Id,
      Name        => "packages");

   Desc_For_Use_Type_Clause_F_Has_All : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.All_Node_Type_Id,
      Name        => "has_all");

   Desc_For_Use_Type_Clause_F_Types : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 5, Field_Type => Common.Name_List_Type_Id,
      Name        => "types");

   Desc_For_Variant_F_Choices : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Alternatives_List_Type_Id,
      Name        => "choices");

   Desc_For_Variant_F_Components : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Component_List_Type_Id,
      Name        => "components");

   Desc_For_Variant_Part_F_Discr_Name : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 10, Field_Type => Common.Identifier_Type_Id,
      Name        => "discr_name");

   Desc_For_Variant_Part_F_Variant : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 7, Field_Type => Common.Variant_List_Type_Id,
      Name        => "variant");

   Desc_For_With_Clause_F_Has_Limited : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Limited_Node_Type_Id,
      Name        => "has_limited");

   Desc_For_With_Clause_F_Has_Private : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 11, Field_Type => Common.Private_Node_Type_Id,
      Name        => "has_private");

   Desc_For_With_Clause_F_Packages : aliased constant Syntax_Field_Descriptor :=
     (Name_Length => 8, Field_Type => Common.Name_List_Type_Id,
      Name        => "packages");

   Syntax_Field_Descriptors : constant array
     (Field_Reference) of Syntax_Field_Descriptor_Access :=
     (Constrained_Array_Indices_F_List =>
        Desc_For_Constrained_Array_Indices_F_List'Access,
      Unconstrained_Array_Indices_F_Types =>
        Desc_For_Unconstrained_Array_Indices_F_Types'Access,
      Aspect_Assoc_F_Id => Desc_For_Aspect_Assoc_F_Id'Access,
      Aspect_Assoc_F_Expr => Desc_For_Aspect_Assoc_F_Expr'Access,
      At_Clause_F_Name => Desc_For_At_Clause_F_Name'Access,
      At_Clause_F_Expr => Desc_For_At_Clause_F_Expr'Access,
      Attribute_Def_Clause_F_Attribute_Expr =>
        Desc_For_Attribute_Def_Clause_F_Attribute_Expr'Access,
      Attribute_Def_Clause_F_Expr =>
        Desc_For_Attribute_Def_Clause_F_Expr'Access,
      Enum_Rep_Clause_F_Type_Name =>
        Desc_For_Enum_Rep_Clause_F_Type_Name'Access,
      Enum_Rep_Clause_F_Aggregate =>
        Desc_For_Enum_Rep_Clause_F_Aggregate'Access,
      Record_Rep_Clause_F_Name    => Desc_For_Record_Rep_Clause_F_Name'Access,
      Record_Rep_Clause_F_At_Expr =>
        Desc_For_Record_Rep_Clause_F_At_Expr'Access,
      Record_Rep_Clause_F_Components =>
        Desc_For_Record_Rep_Clause_F_Components'Access,
      Aspect_Spec_F_Aspect_Assocs =>
        Desc_For_Aspect_Spec_F_Aspect_Assocs'Access,
      Contract_Case_Assoc_F_Guard =>
        Desc_For_Contract_Case_Assoc_F_Guard'Access,
      Contract_Case_Assoc_F_Consequence =>
        Desc_For_Contract_Case_Assoc_F_Consequence'Access,
      Pragma_Argument_Assoc_F_Id => Desc_For_Pragma_Argument_Assoc_F_Id'Access,
      Pragma_Argument_Assoc_F_Expr =>
        Desc_For_Pragma_Argument_Assoc_F_Expr'Access,
      Entry_Spec_F_Entry_Name     => Desc_For_Entry_Spec_F_Entry_Name'Access,
      Entry_Spec_F_Family_Type    => Desc_For_Entry_Spec_F_Family_Type'Access,
      Entry_Spec_F_Entry_Params   => Desc_For_Entry_Spec_F_Entry_Params'Access,
      Subp_Spec_F_Subp_Kind       => Desc_For_Subp_Spec_F_Subp_Kind'Access,
      Subp_Spec_F_Subp_Name       => Desc_For_Subp_Spec_F_Subp_Name'Access,
      Subp_Spec_F_Subp_Params     => Desc_For_Subp_Spec_F_Subp_Params'Access,
      Subp_Spec_F_Subp_Returns    => Desc_For_Subp_Spec_F_Subp_Returns'Access,
      Component_List_F_Components =>
        Desc_For_Component_List_F_Components'Access,
      Component_List_F_Variant_Part =>
        Desc_For_Component_List_F_Variant_Part'Access,
      Known_Discriminant_Part_F_Discr_Specs =>
        Desc_For_Known_Discriminant_Part_F_Discr_Specs'Access,
      Entry_Completion_Formal_Params_F_Params =>
        Desc_For_Entry_Completion_Formal_Params_F_Params'Access,
      Generic_Formal_Part_F_Decls =>
        Desc_For_Generic_Formal_Part_F_Decls'Access,
      Base_Record_Def_F_Components =>
        Desc_For_Base_Record_Def_F_Components'Access,
      Aggregate_Assoc_F_Designators =>
        Desc_For_Aggregate_Assoc_F_Designators'Access,
      Aggregate_Assoc_F_R_Expr => Desc_For_Aggregate_Assoc_F_R_Expr'Access,
      Discriminant_Assoc_F_Ids => Desc_For_Discriminant_Assoc_F_Ids'Access,
      Discriminant_Assoc_F_Discr_Expr =>
        Desc_For_Discriminant_Assoc_F_Discr_Expr'Access,
      Param_Assoc_F_Designator => Desc_For_Param_Assoc_F_Designator'Access,
      Param_Assoc_F_R_Expr           => Desc_For_Param_Assoc_F_R_Expr'Access,
      Basic_Decl_F_Aspects           => Desc_For_Basic_Decl_F_Aspects'Access,
      Component_Decl_F_Ids           => Desc_For_Component_Decl_F_Ids'Access,
      Component_Decl_F_Component_Def =>
        Desc_For_Component_Decl_F_Component_Def'Access,
      Component_Decl_F_Default_Expr =>
        Desc_For_Component_Decl_F_Default_Expr'Access,
      Discriminant_Spec_F_Ids       => Desc_For_Discriminant_Spec_F_Ids'Access,
      Discriminant_Spec_F_Type_Expr =>
        Desc_For_Discriminant_Spec_F_Type_Expr'Access,
      Discriminant_Spec_F_Default_Expr =>
        Desc_For_Discriminant_Spec_F_Default_Expr'Access,
      Generic_Formal_F_Decl => Desc_For_Generic_Formal_F_Decl'Access,
      Param_Spec_F_Ids                 => Desc_For_Param_Spec_F_Ids'Access,
      Param_Spec_F_Has_Aliased => Desc_For_Param_Spec_F_Has_Aliased'Access,
      Param_Spec_F_Mode                => Desc_For_Param_Spec_F_Mode'Access,
      Param_Spec_F_Type_Expr => Desc_For_Param_Spec_F_Type_Expr'Access,
      Param_Spec_F_Default_Expr => Desc_For_Param_Spec_F_Default_Expr'Access,
      Base_Package_Decl_F_Package_Name =>
        Desc_For_Base_Package_Decl_F_Package_Name'Access,
      Base_Package_Decl_F_Public_Part =>
        Desc_For_Base_Package_Decl_F_Public_Part'Access,
      Base_Package_Decl_F_Private_Part =>
        Desc_For_Base_Package_Decl_F_Private_Part'Access,
      Base_Package_Decl_F_End_Name =>
        Desc_For_Base_Package_Decl_F_End_Name'Access,
      Base_Type_Decl_F_Name => Desc_For_Base_Type_Decl_F_Name'Access,
      Subtype_Decl_F_Subtype => Desc_For_Subtype_Decl_F_Subtype'Access,
      Incomplete_Type_Decl_F_Discriminants =>
        Desc_For_Incomplete_Type_Decl_F_Discriminants'Access,
      Incomplete_Tagged_Type_Decl_F_Has_Abstract =>
        Desc_For_Incomplete_Tagged_Type_Decl_F_Has_Abstract'Access,
      Protected_Type_Decl_F_Discriminants =>
        Desc_For_Protected_Type_Decl_F_Discriminants'Access,
      Protected_Type_Decl_F_Interfaces =>
        Desc_For_Protected_Type_Decl_F_Interfaces'Access,
      Protected_Type_Decl_F_Definition =>
        Desc_For_Protected_Type_Decl_F_Definition'Access,
      Task_Type_Decl_F_Discriminants =>
        Desc_For_Task_Type_Decl_F_Discriminants'Access,
      Task_Type_Decl_F_Definition =>
        Desc_For_Task_Type_Decl_F_Definition'Access,
      Type_Decl_F_Discriminants => Desc_For_Type_Decl_F_Discriminants'Access,
      Type_Decl_F_Type_Def           => Desc_For_Type_Decl_F_Type_Def'Access,
      Classic_Subp_Decl_F_Overriding =>
        Desc_For_Classic_Subp_Decl_F_Overriding'Access,
      Classic_Subp_Decl_F_Subp_Spec =>
        Desc_For_Classic_Subp_Decl_F_Subp_Spec'Access,
      Formal_Subp_Decl_F_Default_Expr =>
        Desc_For_Formal_Subp_Decl_F_Default_Expr'Access,
      Entry_Decl_F_Overriding => Desc_For_Entry_Decl_F_Overriding'Access,
      Entry_Decl_F_Spec                 => Desc_For_Entry_Decl_F_Spec'Access,
      Enum_Literal_Decl_F_Name => Desc_For_Enum_Literal_Decl_F_Name'Access,
      Generic_Subp_Internal_F_Subp_Spec =>
        Desc_For_Generic_Subp_Internal_F_Subp_Spec'Access,
      Base_Subp_Body_F_Overriding =>
        Desc_For_Base_Subp_Body_F_Overriding'Access,
      Base_Subp_Body_F_Subp_Spec => Desc_For_Base_Subp_Body_F_Subp_Spec'Access,
      Expr_Function_F_Expr         => Desc_For_Expr_Function_F_Expr'Access,
      Subp_Body_F_Decls            => Desc_For_Subp_Body_F_Decls'Access,
      Subp_Body_F_Stmts            => Desc_For_Subp_Body_F_Stmts'Access,
      Subp_Body_F_End_Name         => Desc_For_Subp_Body_F_End_Name'Access,
      Subp_Renaming_Decl_F_Renames =>
        Desc_For_Subp_Renaming_Decl_F_Renames'Access,
      Package_Body_Stub_F_Name    => Desc_For_Package_Body_Stub_F_Name'Access,
      Protected_Body_Stub_F_Name => Desc_For_Protected_Body_Stub_F_Name'Access,
      Subp_Body_Stub_F_Overriding =>
        Desc_For_Subp_Body_Stub_F_Overriding'Access,
      Subp_Body_Stub_F_Subp_Spec => Desc_For_Subp_Body_Stub_F_Subp_Spec'Access,
      Task_Body_Stub_F_Name       => Desc_For_Task_Body_Stub_F_Name'Access,
      Entry_Body_F_Entry_Name     => Desc_For_Entry_Body_F_Entry_Name'Access,
      Entry_Body_F_Index_Spec     => Desc_For_Entry_Body_F_Index_Spec'Access,
      Entry_Body_F_Params         => Desc_For_Entry_Body_F_Params'Access,
      Entry_Body_F_Barrier        => Desc_For_Entry_Body_F_Barrier'Access,
      Entry_Body_F_Decls          => Desc_For_Entry_Body_F_Decls'Access,
      Entry_Body_F_Stmts          => Desc_For_Entry_Body_F_Stmts'Access,
      Entry_Body_F_End_Name       => Desc_For_Entry_Body_F_End_Name'Access,
      Package_Body_F_Package_Name =>
        Desc_For_Package_Body_F_Package_Name'Access,
      Package_Body_F_Decls => Desc_For_Package_Body_F_Decls'Access,
      Package_Body_F_Stmts => Desc_For_Package_Body_F_Stmts'Access,
      Package_Body_F_End_Name => Desc_For_Package_Body_F_End_Name'Access,
      Protected_Body_F_Name => Desc_For_Protected_Body_F_Name'Access,
      Protected_Body_F_Decls => Desc_For_Protected_Body_F_Decls'Access,
      Protected_Body_F_End_Name => Desc_For_Protected_Body_F_End_Name'Access,
      Task_Body_F_Name                   => Desc_For_Task_Body_F_Name'Access,
      Task_Body_F_Decls                  => Desc_For_Task_Body_F_Decls'Access,
      Task_Body_F_Stmts                  => Desc_For_Task_Body_F_Stmts'Access,
      Task_Body_F_End_Name => Desc_For_Task_Body_F_End_Name'Access,
      Entry_Index_Spec_F_Id => Desc_For_Entry_Index_Spec_F_Id'Access,
      Entry_Index_Spec_F_Subtype => Desc_For_Entry_Index_Spec_F_Subtype'Access,
      Exception_Decl_F_Ids => Desc_For_Exception_Decl_F_Ids'Access,
      Exception_Decl_F_Renames => Desc_For_Exception_Decl_F_Renames'Access,
      Exception_Handler_F_Exception_Name =>
        Desc_For_Exception_Handler_F_Exception_Name'Access,
      Exception_Handler_F_Handled_Exceptions =>
        Desc_For_Exception_Handler_F_Handled_Exceptions'Access,
      Exception_Handler_F_Stmts   => Desc_For_Exception_Handler_F_Stmts'Access,
      For_Loop_Var_Decl_F_Id      => Desc_For_For_Loop_Var_Decl_F_Id'Access,
      For_Loop_Var_Decl_F_Id_Type =>
        Desc_For_For_Loop_Var_Decl_F_Id_Type'Access,
      Generic_Decl_F_Formal_Part => Desc_For_Generic_Decl_F_Formal_Part'Access,
      Generic_Package_Decl_F_Package_Decl =>
        Desc_For_Generic_Package_Decl_F_Package_Decl'Access,
      Generic_Subp_Decl_F_Subp_Decl =>
        Desc_For_Generic_Subp_Decl_F_Subp_Decl'Access,
      Generic_Package_Instantiation_F_Name =>
        Desc_For_Generic_Package_Instantiation_F_Name'Access,
      Generic_Package_Instantiation_F_Generic_Pkg_Name =>
        Desc_For_Generic_Package_Instantiation_F_Generic_Pkg_Name'Access,
      Generic_Package_Instantiation_F_Params =>
        Desc_For_Generic_Package_Instantiation_F_Params'Access,
      Generic_Subp_Instantiation_F_Overriding =>
        Desc_For_Generic_Subp_Instantiation_F_Overriding'Access,
      Generic_Subp_Instantiation_F_Kind =>
        Desc_For_Generic_Subp_Instantiation_F_Kind'Access,
      Generic_Subp_Instantiation_F_Subp_Name =>
        Desc_For_Generic_Subp_Instantiation_F_Subp_Name'Access,
      Generic_Subp_Instantiation_F_Generic_Subp_Name =>
        Desc_For_Generic_Subp_Instantiation_F_Generic_Subp_Name'Access,
      Generic_Subp_Instantiation_F_Params =>
        Desc_For_Generic_Subp_Instantiation_F_Params'Access,
      Generic_Package_Renaming_Decl_F_Name =>
        Desc_For_Generic_Package_Renaming_Decl_F_Name'Access,
      Generic_Package_Renaming_Decl_F_Renames =>
        Desc_For_Generic_Package_Renaming_Decl_F_Renames'Access,
      Generic_Subp_Renaming_Decl_F_Kind =>
        Desc_For_Generic_Subp_Renaming_Decl_F_Kind'Access,
      Generic_Subp_Renaming_Decl_F_Name =>
        Desc_For_Generic_Subp_Renaming_Decl_F_Name'Access,
      Generic_Subp_Renaming_Decl_F_Renames =>
        Desc_For_Generic_Subp_Renaming_Decl_F_Renames'Access,
      Label_Decl_F_Name             => Desc_For_Label_Decl_F_Name'Access,
      Named_Stmt_Decl_F_Name        => Desc_For_Named_Stmt_Decl_F_Name'Access,
      Number_Decl_F_Ids             => Desc_For_Number_Decl_F_Ids'Access,
      Number_Decl_F_Expr            => Desc_For_Number_Decl_F_Expr'Access,
      Object_Decl_F_Ids             => Desc_For_Object_Decl_F_Ids'Access,
      Object_Decl_F_Has_Aliased => Desc_For_Object_Decl_F_Has_Aliased'Access,
      Object_Decl_F_Has_Constant => Desc_For_Object_Decl_F_Has_Constant'Access,
      Object_Decl_F_Mode            => Desc_For_Object_Decl_F_Mode'Access,
      Object_Decl_F_Type_Expr       => Desc_For_Object_Decl_F_Type_Expr'Access,
      Object_Decl_F_Default_Expr => Desc_For_Object_Decl_F_Default_Expr'Access,
      Object_Decl_F_Renaming_Clause =>
        Desc_For_Object_Decl_F_Renaming_Clause'Access,
      Package_Renaming_Decl_F_Name =>
        Desc_For_Package_Renaming_Decl_F_Name'Access,
      Package_Renaming_Decl_F_Renames =>
        Desc_For_Package_Renaming_Decl_F_Renames'Access,
      Single_Protected_Decl_F_Name =>
        Desc_For_Single_Protected_Decl_F_Name'Access,
      Single_Protected_Decl_F_Interfaces =>
        Desc_For_Single_Protected_Decl_F_Interfaces'Access,
      Single_Protected_Decl_F_Definition =>
        Desc_For_Single_Protected_Decl_F_Definition'Access,
      Single_Task_Decl_F_Task_Type =>
        Desc_For_Single_Task_Decl_F_Task_Type'Access,
      Case_Stmt_Alternative_F_Choices =>
        Desc_For_Case_Stmt_Alternative_F_Choices'Access,
      Case_Stmt_Alternative_F_Stmts =>
        Desc_For_Case_Stmt_Alternative_F_Stmts'Access,
      Compilation_Unit_F_Prelude => Desc_For_Compilation_Unit_F_Prelude'Access,
      Compilation_Unit_F_Body     => Desc_For_Compilation_Unit_F_Body'Access,
      Compilation_Unit_F_Pragmas => Desc_For_Compilation_Unit_F_Pragmas'Access,
      Component_Clause_F_Id       => Desc_For_Component_Clause_F_Id'Access,
      Component_Clause_F_Position =>
        Desc_For_Component_Clause_F_Position'Access,
      Component_Clause_F_Range    => Desc_For_Component_Clause_F_Range'Access,
      Component_Def_F_Has_Aliased =>
        Desc_For_Component_Def_F_Has_Aliased'Access,
      Component_Def_F_Has_Constant =>
        Desc_For_Component_Def_F_Has_Constant'Access,
      Component_Def_F_Type_Expr => Desc_For_Component_Def_F_Type_Expr'Access,
      Delta_Constraint_F_Digits => Desc_For_Delta_Constraint_F_Digits'Access,
      Delta_Constraint_F_Range => Desc_For_Delta_Constraint_F_Range'Access,
      Digits_Constraint_F_Digits => Desc_For_Digits_Constraint_F_Digits'Access,
      Digits_Constraint_F_Range => Desc_For_Digits_Constraint_F_Range'Access,
      Discriminant_Constraint_F_Constraints =>
        Desc_For_Discriminant_Constraint_F_Constraints'Access,
      Index_Constraint_F_Constraints =>
        Desc_For_Index_Constraint_F_Constraints'Access,
      Range_Constraint_F_Range    => Desc_For_Range_Constraint_F_Range'Access,
      Declarative_Part_F_Decls    => Desc_For_Declarative_Part_F_Decls'Access,
      Elsif_Expr_Part_F_Cond_Expr =>
        Desc_For_Elsif_Expr_Part_F_Cond_Expr'Access,
      Elsif_Expr_Part_F_Then_Expr =>
        Desc_For_Elsif_Expr_Part_F_Then_Expr'Access,
      Elsif_Stmt_Part_F_Cond_Expr =>
        Desc_For_Elsif_Stmt_Part_F_Cond_Expr'Access,
      Elsif_Stmt_Part_F_Stmts => Desc_For_Elsif_Stmt_Part_F_Stmts'Access,
      Allocator_F_Subpool            => Desc_For_Allocator_F_Subpool'Access,
      Allocator_F_Type_Or_Expr => Desc_For_Allocator_F_Type_Or_Expr'Access,
      Base_Aggregate_F_Ancestor_Expr =>
        Desc_For_Base_Aggregate_F_Ancestor_Expr'Access,
      Base_Aggregate_F_Assocs => Desc_For_Base_Aggregate_F_Assocs'Access,
      Bin_Op_F_Left                   => Desc_For_Bin_Op_F_Left'Access,
      Bin_Op_F_Op                     => Desc_For_Bin_Op_F_Op'Access,
      Bin_Op_F_Right                  => Desc_For_Bin_Op_F_Right'Access,
      Case_Expr_F_Expr                => Desc_For_Case_Expr_F_Expr'Access,
      Case_Expr_F_Cases               => Desc_For_Case_Expr_F_Cases'Access,
      Case_Expr_Alternative_F_Choices =>
        Desc_For_Case_Expr_Alternative_F_Choices'Access,
      Case_Expr_Alternative_F_Expr =>
        Desc_For_Case_Expr_Alternative_F_Expr'Access,
      Contract_Cases_F_Contract_Cases =>
        Desc_For_Contract_Cases_F_Contract_Cases'Access,
      If_Expr_F_Cond_Expr => Desc_For_If_Expr_F_Cond_Expr'Access,
      If_Expr_F_Then_Expr => Desc_For_If_Expr_F_Then_Expr'Access,
      If_Expr_F_Alternatives => Desc_For_If_Expr_F_Alternatives'Access,
      If_Expr_F_Else_Expr => Desc_For_If_Expr_F_Else_Expr'Access,
      Membership_Expr_F_Expr => Desc_For_Membership_Expr_F_Expr'Access,
      Membership_Expr_F_Op => Desc_For_Membership_Expr_F_Op'Access,
      Membership_Expr_F_Membership_Exprs =>
        Desc_For_Membership_Expr_F_Membership_Exprs'Access,
      Attribute_Ref_F_Prefix => Desc_For_Attribute_Ref_F_Prefix'Access,
      Attribute_Ref_F_Attribute => Desc_For_Attribute_Ref_F_Attribute'Access,
      Attribute_Ref_F_Args            => Desc_For_Attribute_Ref_F_Args'Access,
      Call_Expr_F_Name                => Desc_For_Call_Expr_F_Name'Access,
      Call_Expr_F_Suffix              => Desc_For_Call_Expr_F_Suffix'Access,
      Defining_Name_F_Name            => Desc_For_Defining_Name_F_Name'Access,
      Discrete_Subtype_Name_F_Subtype =>
        Desc_For_Discrete_Subtype_Name_F_Subtype'Access,
      Dotted_Name_F_Prefix         => Desc_For_Dotted_Name_F_Prefix'Access,
      Dotted_Name_F_Suffix         => Desc_For_Dotted_Name_F_Suffix'Access,
      End_Name_F_Name              => Desc_For_End_Name_F_Name'Access,
      Explicit_Deref_F_Prefix      => Desc_For_Explicit_Deref_F_Prefix'Access,
      Qual_Expr_F_Prefix           => Desc_For_Qual_Expr_F_Prefix'Access,
      Qual_Expr_F_Suffix           => Desc_For_Qual_Expr_F_Suffix'Access,
      Paren_Expr_F_Expr            => Desc_For_Paren_Expr_F_Expr'Access,
      Quantified_Expr_F_Quantifier =>
        Desc_For_Quantified_Expr_F_Quantifier'Access,
      Quantified_Expr_F_Loop_Spec =>
        Desc_For_Quantified_Expr_F_Loop_Spec'Access,
      Quantified_Expr_F_Expr      => Desc_For_Quantified_Expr_F_Expr'Access,
      Raise_Expr_F_Exception_Name =>
        Desc_For_Raise_Expr_F_Exception_Name'Access,
      Raise_Expr_F_Error_Message => Desc_For_Raise_Expr_F_Error_Message'Access,
      Un_Op_F_Op                  => Desc_For_Un_Op_F_Op'Access,
      Un_Op_F_Expr                => Desc_For_Un_Op_F_Expr'Access,
      Handled_Stmts_F_Stmts       => Desc_For_Handled_Stmts_F_Stmts'Access,
      Handled_Stmts_F_Exceptions => Desc_For_Handled_Stmts_F_Exceptions'Access,
      Library_Item_F_Has_Private => Desc_For_Library_Item_F_Has_Private'Access,
      Library_Item_F_Item         => Desc_For_Library_Item_F_Item'Access,
      For_Loop_Spec_F_Var_Decl    => Desc_For_For_Loop_Spec_F_Var_Decl'Access,
      For_Loop_Spec_F_Loop_Type   => Desc_For_For_Loop_Spec_F_Loop_Type'Access,
      For_Loop_Spec_F_Has_Reverse =>
        Desc_For_For_Loop_Spec_F_Has_Reverse'Access,
      For_Loop_Spec_F_Iter_Expr   => Desc_For_For_Loop_Spec_F_Iter_Expr'Access,
      While_Loop_Spec_F_Expr      => Desc_For_While_Loop_Spec_F_Expr'Access,
      Params_F_Params             => Desc_For_Params_F_Params'Access,
      Pragma_Node_F_Id            => Desc_For_Pragma_Node_F_Id'Access,
      Pragma_Node_F_Args          => Desc_For_Pragma_Node_F_Args'Access,
      Protected_Def_F_Public_Part =>
        Desc_For_Protected_Def_F_Public_Part'Access,
      Protected_Def_F_Private_Part =>
        Desc_For_Protected_Def_F_Private_Part'Access,
      Protected_Def_F_End_Name => Desc_For_Protected_Def_F_End_Name'Access,
      Range_Spec_F_Range               => Desc_For_Range_Spec_F_Range'Access,
      Renaming_Clause_F_Renamed_Object =>
        Desc_For_Renaming_Clause_F_Renamed_Object'Access,
      Select_When_Part_F_Cond_Expr =>
        Desc_For_Select_When_Part_F_Cond_Expr'Access,
      Select_When_Part_F_Stmts => Desc_For_Select_When_Part_F_Stmts'Access,
      Accept_Stmt_F_Name             => Desc_For_Accept_Stmt_F_Name'Access,
      Accept_Stmt_F_Entry_Index_Expr =>
        Desc_For_Accept_Stmt_F_Entry_Index_Expr'Access,
      Accept_Stmt_F_Params           => Desc_For_Accept_Stmt_F_Params'Access,
      Accept_Stmt_With_Stmts_F_Stmts =>
        Desc_For_Accept_Stmt_With_Stmts_F_Stmts'Access,
      Accept_Stmt_With_Stmts_F_End_Name =>
        Desc_For_Accept_Stmt_With_Stmts_F_End_Name'Access,
      Base_Loop_Stmt_F_Spec       => Desc_For_Base_Loop_Stmt_F_Spec'Access,
      Base_Loop_Stmt_F_Stmts      => Desc_For_Base_Loop_Stmt_F_Stmts'Access,
      Base_Loop_Stmt_F_End_Name   => Desc_For_Base_Loop_Stmt_F_End_Name'Access,
      Begin_Block_F_Stmts         => Desc_For_Begin_Block_F_Stmts'Access,
      Begin_Block_F_End_Name      => Desc_For_Begin_Block_F_End_Name'Access,
      Decl_Block_F_Decls          => Desc_For_Decl_Block_F_Decls'Access,
      Decl_Block_F_Stmts          => Desc_For_Decl_Block_F_Stmts'Access,
      Decl_Block_F_End_Name       => Desc_For_Decl_Block_F_End_Name'Access,
      Case_Stmt_F_Expr            => Desc_For_Case_Stmt_F_Expr'Access,
      Case_Stmt_F_Alternatives    => Desc_For_Case_Stmt_F_Alternatives'Access,
      Extended_Return_Stmt_F_Decl =>
        Desc_For_Extended_Return_Stmt_F_Decl'Access,
      Extended_Return_Stmt_F_Stmts =>
        Desc_For_Extended_Return_Stmt_F_Stmts'Access,
      If_Stmt_F_Cond_Expr         => Desc_For_If_Stmt_F_Cond_Expr'Access,
      If_Stmt_F_Then_Stmts        => Desc_For_If_Stmt_F_Then_Stmts'Access,
      If_Stmt_F_Alternatives      => Desc_For_If_Stmt_F_Alternatives'Access,
      If_Stmt_F_Else_Stmts        => Desc_For_If_Stmt_F_Else_Stmts'Access,
      Named_Stmt_F_Decl           => Desc_For_Named_Stmt_F_Decl'Access,
      Named_Stmt_F_Stmt           => Desc_For_Named_Stmt_F_Stmt'Access,
      Select_Stmt_F_Guards        => Desc_For_Select_Stmt_F_Guards'Access,
      Select_Stmt_F_Else_Stmts    => Desc_For_Select_Stmt_F_Else_Stmts'Access,
      Select_Stmt_F_Abort_Stmts   => Desc_For_Select_Stmt_F_Abort_Stmts'Access,
      Abort_Stmt_F_Names          => Desc_For_Abort_Stmt_F_Names'Access,
      Assign_Stmt_F_Dest          => Desc_For_Assign_Stmt_F_Dest'Access,
      Assign_Stmt_F_Expr          => Desc_For_Assign_Stmt_F_Expr'Access,
      Call_Stmt_F_Call            => Desc_For_Call_Stmt_F_Call'Access,
      Delay_Stmt_F_Has_Until      => Desc_For_Delay_Stmt_F_Has_Until'Access,
      Delay_Stmt_F_Expr           => Desc_For_Delay_Stmt_F_Expr'Access,
      Exit_Stmt_F_Loop_Name       => Desc_For_Exit_Stmt_F_Loop_Name'Access,
      Exit_Stmt_F_Cond_Expr       => Desc_For_Exit_Stmt_F_Cond_Expr'Access,
      Goto_Stmt_F_Label_Name      => Desc_For_Goto_Stmt_F_Label_Name'Access,
      Label_F_Decl                => Desc_For_Label_F_Decl'Access,
      Raise_Stmt_F_Exception_Name =>
        Desc_For_Raise_Stmt_F_Exception_Name'Access,
      Raise_Stmt_F_Error_Message => Desc_For_Raise_Stmt_F_Error_Message'Access,
      Requeue_Stmt_F_Call_Name => Desc_For_Requeue_Stmt_F_Call_Name'Access,
      Requeue_Stmt_F_Has_Abort => Desc_For_Requeue_Stmt_F_Has_Abort'Access,
      Return_Stmt_F_Return_Expr => Desc_For_Return_Stmt_F_Return_Expr'Access,
      Subunit_F_Name                     => Desc_For_Subunit_F_Name'Access,
      Subunit_F_Body                     => Desc_For_Subunit_F_Body'Access,
      Task_Def_F_Interfaces => Desc_For_Task_Def_F_Interfaces'Access,
      Task_Def_F_Public_Part => Desc_For_Task_Def_F_Public_Part'Access,
      Task_Def_F_Private_Part => Desc_For_Task_Def_F_Private_Part'Access,
      Task_Def_F_End_Name => Desc_For_Task_Def_F_End_Name'Access,
      Access_Def_F_Has_Not_Null => Desc_For_Access_Def_F_Has_Not_Null'Access,
      Access_To_Subp_Def_F_Has_Protected =>
        Desc_For_Access_To_Subp_Def_F_Has_Protected'Access,
      Access_To_Subp_Def_F_Subp_Spec =>
        Desc_For_Access_To_Subp_Def_F_Subp_Spec'Access,
      Anonymous_Type_Access_Def_F_Type_Decl =>
        Desc_For_Anonymous_Type_Access_Def_F_Type_Decl'Access,
      Type_Access_Def_F_Has_All => Desc_For_Type_Access_Def_F_Has_All'Access,
      Type_Access_Def_F_Has_Constant =>
        Desc_For_Type_Access_Def_F_Has_Constant'Access,
      Type_Access_Def_F_Subtype_Indication =>
        Desc_For_Type_Access_Def_F_Subtype_Indication'Access,
      Array_Type_Def_F_Indices => Desc_For_Array_Type_Def_F_Indices'Access,
      Array_Type_Def_F_Component_Type =>
        Desc_For_Array_Type_Def_F_Component_Type'Access,
      Derived_Type_Def_F_Has_Abstract =>
        Desc_For_Derived_Type_Def_F_Has_Abstract'Access,
      Derived_Type_Def_F_Has_Limited =>
        Desc_For_Derived_Type_Def_F_Has_Limited'Access,
      Derived_Type_Def_F_Has_Synchronized =>
        Desc_For_Derived_Type_Def_F_Has_Synchronized'Access,
      Derived_Type_Def_F_Subtype_Indication =>
        Desc_For_Derived_Type_Def_F_Subtype_Indication'Access,
      Derived_Type_Def_F_Interfaces =>
        Desc_For_Derived_Type_Def_F_Interfaces'Access,
      Derived_Type_Def_F_Record_Extension =>
        Desc_For_Derived_Type_Def_F_Record_Extension'Access,
      Derived_Type_Def_F_Has_With_Private =>
        Desc_For_Derived_Type_Def_F_Has_With_Private'Access,
      Enum_Type_Def_F_Enum_Literals =>
        Desc_For_Enum_Type_Def_F_Enum_Literals'Access,
      Interface_Type_Def_F_Interface_Kind =>
        Desc_For_Interface_Type_Def_F_Interface_Kind'Access,
      Interface_Type_Def_F_Interfaces =>
        Desc_For_Interface_Type_Def_F_Interfaces'Access,
      Mod_Int_Type_Def_F_Expr => Desc_For_Mod_Int_Type_Def_F_Expr'Access,
      Private_Type_Def_F_Has_Abstract =>
        Desc_For_Private_Type_Def_F_Has_Abstract'Access,
      Private_Type_Def_F_Has_Tagged =>
        Desc_For_Private_Type_Def_F_Has_Tagged'Access,
      Private_Type_Def_F_Has_Limited =>
        Desc_For_Private_Type_Def_F_Has_Limited'Access,
      Decimal_Fixed_Point_Def_F_Delta =>
        Desc_For_Decimal_Fixed_Point_Def_F_Delta'Access,
      Decimal_Fixed_Point_Def_F_Digits =>
        Desc_For_Decimal_Fixed_Point_Def_F_Digits'Access,
      Decimal_Fixed_Point_Def_F_Range =>
        Desc_For_Decimal_Fixed_Point_Def_F_Range'Access,
      Floating_Point_Def_F_Num_Digits =>
        Desc_For_Floating_Point_Def_F_Num_Digits'Access,
      Floating_Point_Def_F_Range => Desc_For_Floating_Point_Def_F_Range'Access,
      Ordinary_Fixed_Point_Def_F_Delta =>
        Desc_For_Ordinary_Fixed_Point_Def_F_Delta'Access,
      Ordinary_Fixed_Point_Def_F_Range =>
        Desc_For_Ordinary_Fixed_Point_Def_F_Range'Access,
      Record_Type_Def_F_Has_Abstract =>
        Desc_For_Record_Type_Def_F_Has_Abstract'Access,
      Record_Type_Def_F_Has_Tagged =>
        Desc_For_Record_Type_Def_F_Has_Tagged'Access,
      Record_Type_Def_F_Has_Limited =>
        Desc_For_Record_Type_Def_F_Has_Limited'Access,
      Record_Type_Def_F_Record_Def =>
        Desc_For_Record_Type_Def_F_Record_Def'Access,
      Signed_Int_Type_Def_F_Range =>
        Desc_For_Signed_Int_Type_Def_F_Range'Access,
      Anonymous_Type_F_Type_Decl => Desc_For_Anonymous_Type_F_Type_Decl'Access,
      Subtype_Indication_F_Has_Not_Null =>
        Desc_For_Subtype_Indication_F_Has_Not_Null'Access,
      Subtype_Indication_F_Name => Desc_For_Subtype_Indication_F_Name'Access,
      Subtype_Indication_F_Constraint =>
        Desc_For_Subtype_Indication_F_Constraint'Access,
      Unconstrained_Array_Index_F_Subtype_Indication =>
        Desc_For_Unconstrained_Array_Index_F_Subtype_Indication'Access,
      Use_Package_Clause_F_Packages =>
        Desc_For_Use_Package_Clause_F_Packages'Access,
      Use_Type_Clause_F_Has_All => Desc_For_Use_Type_Clause_F_Has_All'Access,
      Use_Type_Clause_F_Types   => Desc_For_Use_Type_Clause_F_Types'Access,
      Variant_F_Choices         => Desc_For_Variant_F_Choices'Access,
      Variant_F_Components      => Desc_For_Variant_F_Components'Access,
      Variant_Part_F_Discr_Name => Desc_For_Variant_Part_F_Discr_Name'Access,
      Variant_Part_F_Variant    => Desc_For_Variant_Part_F_Variant'Access,
      With_Clause_F_Has_Limited => Desc_For_With_Clause_F_Has_Limited'Access,
      With_Clause_F_Has_Private => Desc_For_With_Clause_F_Has_Private'Access,
      With_Clause_F_Packages    => Desc_For_With_Clause_F_Packages'Access);

   --------------------------
   -- Property descriptors --
   --------------------------

   type Property_Descriptor (Name_Length : Natural;
      --  Length of the proprety name

Arity : Natural
         --  Number of arguments this property takes (exclude the Self
         --  argument)
         ) is record
      Name : String (1 .. Name_Length);
      --  Lower-case name for this property

      Return_Type : Value_Constraint;
      --  Return type for this property

      Argument_Types : Value_Constraint_Array (1 .. Arity);
      --  Types of the arguments that this property takes

      Argument_Names : String_Array (1 .. Arity);
      --  Lower-case names for arguments that this property takes

      Argument_Default_Values : Internal_Value_Array (1 .. Arity);
      --  Default values (if any, otherwise No_Internal_Value) for arguments
      --  that this property takes.
   end record;

   type Property_Descriptor_Access is access constant Property_Descriptor;

   --  Descriptors for properties

   Name_For_dim                : aliased constant String := "dim";
   Name_For_env                : aliased constant String := "env";
   Name_For_expected_type      : aliased constant String := "expected_type";
   Name_For_follow_generic     : aliased constant String := "follow_generic";
   Name_For_from_node          : aliased constant String := "from_node";
   Name_For_go_to_incomplete   : aliased constant String := "go_to_incomplete";
   Name_For_imprecise_fallback : aliased constant String :=
     "imprecise_fallback";
   Name_For_is_subscript   : aliased constant String := "is_subscript";
   Name_For_n              : aliased constant String := "n";
   Name_For_name           : aliased constant String := "name";
   Name_For_only_inherited : aliased constant String := "only_inherited";
   Name_For_origin         : aliased constant String := "origin";
   Name_For_origin_node    : aliased constant String := "origin_node";
   Name_For_other_type     : aliased constant String := "other_type";
   Name_For_root           : aliased constant String := "root";
   Name_For_seq            : aliased constant String := "seq";
   Name_For_seq_from       : aliased constant String := "seq_from";
   Name_For_sym            : aliased constant String := "sym";
   Name_For_transitive     : aliased constant String := "transitive";
   Name_For_unit           : aliased constant String := "unit";
   Name_For_units          : aliased constant String := "units";
   Name_For_value          : aliased constant String := "value";

   Desc_For_Ada_Node_P_Declarative_Scope : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 0,
Name              => "declarative_scope",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Declarative_Part_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Complete : aliased constant Property_Descriptor :=
     (Name_Length => 8, Arity => 0,
Name              => "complete",

      Return_Type             => (Kind => Completion_Item_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Valid_Keywords : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "valid_keywords",

      Return_Type             => (Kind => Unbounded_Text_Type_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Generic_Instantiations : aliased constant Property_Descriptor :=
     (Name_Length => 22, Arity => 0,
Name              => "generic_instantiations",

      Return_Type             => (Kind => Generic_Instantiation_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Semantic_Parent : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "semantic_parent",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Parent_Basic_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 0,
Name              => "parent_basic_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Filter_Is_Imported_By : aliased constant Property_Descriptor :=
     (Name_Length => 21, Arity => 2,
Name              => "filter_is_imported_by",

      Return_Type    => (Kind => Analysis_Unit_Array_Value),
      Argument_Types =>
        (1 => (Kind => Analysis_Unit_Array_Value),
         2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_units'Access, 2 => Name_For_transitive'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => No_Internal_Value));

   Desc_For_Ada_Node_P_Xref_Entry_Point : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "xref_entry_point",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Resolve_Names : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "resolve_names",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Standard_Unit : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "standard_unit",

      Return_Type             => (Kind => Analysis_Unit_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Std_Entity : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 1,
Name              => "std_entity",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types          => (1 => (Kind => Unbounded_Text_Value)),
      Argument_Names          => (1 => Name_For_sym'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Ada_Node_P_Bool_Type : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "bool_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Int_Type : aliased constant Property_Descriptor :=
     (Name_Length => 8, Arity => 0,
Name              => "int_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Universal_Int_Type : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 0,
Name              => "universal_int_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Universal_Real_Type : aliased constant Property_Descriptor :=
     (Name_Length => 19, Arity => 0,
Name              => "universal_real_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_P_Top_Level_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "top_level_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types          => (1 => (Kind => Analysis_Unit_Value)),
      Argument_Names          => (1 => Name_For_unit'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Ada_Node_P_Choice_Match : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 1,
Name              => "choice_match",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Big_Integer_Value)),
      Argument_Names          => (1 => Name_For_value'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Ada_Node_P_Gnat_Xref : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 1,
Name              => "gnat_xref",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Ada_Node_Parent : aliased constant Property_Descriptor :=
     (Name_Length => 6, Arity => 0,
Name              => "parent",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Parents : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "parents",

      Return_Type             => (Kind => Ada_Node_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Children : aliased constant Property_Descriptor :=
     (Name_Length => 8, Arity => 0,
Name              => "children",

      Return_Type             => (Kind => Ada_Node_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Token_Start : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 0,
Name              => "token_start",

      Return_Type => (Kind => Token_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Token_End : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "token_end",

      Return_Type => (Kind => Token_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Child_Index : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 0,
Name              => "child_index",

      Return_Type => (Kind => Integer_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Previous_Sibling : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "previous_sibling",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Next_Sibling : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 0,
Name              => "next_sibling",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Unit : aliased constant Property_Descriptor :=
     (Name_Length => 4, Arity => 0,
Name              => "unit",

      Return_Type             => (Kind => Analysis_Unit_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Is_Ghost : aliased constant Property_Descriptor :=
     (Name_Length => 8, Arity => 0,
Name              => "is_ghost",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Ada_Node_Full_Sloc_Image : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "full_sloc_image",

      Return_Type             => (Kind => Text_Type_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Abort_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Abstract_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Assoc_List_P_Zip_With_Params : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 1,
Name              => "zip_with_params",

      Return_Type             => (Kind => Param_Actual_Array_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Aliased_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_All_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Assoc_P_Assoc_Expr : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 0,
Name              => "assoc_expr",

      Return_Type => (Kind => Node_Value, Node_Type => Common.Expr_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Formal_Param_Holder_P_Abstract_Formal_Params : aliased constant Property_Descriptor :=
     (Name_Length => 22, Arity => 0,
Name              => "abstract_formal_params",

      Return_Type             => (Kind => Base_Formal_Param_Decl_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Formal_Param_Holder_P_Nb_Min_Params : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "nb_min_params",

      Return_Type => (Kind => Integer_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Formal_Param_Holder_P_Nb_Max_Params : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "nb_max_params",

      Return_Type => (Kind => Integer_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Formal_Param_Holder_P_Param_Types : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 1,
Name              => "param_types",

      Return_Type    => (Kind => Base_Type_Decl_Array_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Subp_Spec_P_Returns : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "returns",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Type_Expr_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Subp_Spec_P_Params : aliased constant Property_Descriptor :=
     (Name_Length => 6, Arity => 0,
Name              => "params",

      Return_Type             => (Kind => Param_Spec_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Subp_Spec_P_Primitive_Subp_Types : aliased constant Property_Descriptor :=
     (Name_Length => 20, Arity => 0,
Name              => "primitive_subp_types",

      Return_Type             => (Kind => Base_Type_Decl_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Subp_Spec_P_Primitive_Subp_First_Type : aliased constant Property_Descriptor :=
     (Name_Length => 25, Arity => 0,
Name              => "primitive_subp_first_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Subp_Spec_P_Primitive_Subp_Tagged_Type : aliased constant Property_Descriptor :=
     (Name_Length => 26, Arity => 0,
Name              => "primitive_subp_tagged_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Subp_Spec_P_Return_Type : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 1,
Name              => "return_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Basic_Assoc_P_Get_Params : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 1,
Name              => "get_params",

      Return_Type             => (Kind => Defining_Name_Array_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Is_Formal : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "is_formal",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Doc_Annotations : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "doc_annotations",

      Return_Type             => (Kind => Doc_Annotation_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Doc : aliased constant Property_Descriptor :=
     (Name_Length => 3, Arity => 0,
Name              => "doc",

      Return_Type             => (Kind => Text_Type_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Previous_Part_For_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 22, Arity => 0,
Name              => "previous_part_for_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Canonical_Part : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "canonical_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Is_Static_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_static_decl",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Is_Imported : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 0,
Name              => "is_imported",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Get_Aspect_Assoc : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 1,
Name              => "get_aspect_assoc",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Aspect_Assoc_Type_Id),
      Argument_Types          => (1 => (Kind => Unbounded_Text_Value)),
      Argument_Names          => (1 => Name_For_name'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Basic_Decl_P_Get_Aspect_Spec_Expr : aliased constant Property_Descriptor :=
     (Name_Length => 20, Arity => 1,
Name              => "get_aspect_spec_expr",

      Return_Type => (Kind => Node_Value, Node_Type => Common.Expr_Type_Id),
      Argument_Types          => (1 => (Kind => Unbounded_Text_Value)),
      Argument_Names          => (1 => Name_For_name'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Basic_Decl_P_Get_Aspect : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 2,
Name              => "get_aspect",

      Return_Type    => (Kind => Aspect_Value),
      Argument_Types =>
        (1 => (Kind => Unbounded_Text_Value), 2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_name'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Has_Aspect : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 2,
Name              => "has_aspect",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Unbounded_Text_Value), 2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_name'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Get_Pragma : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 1,
Name              => "get_pragma",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Pragma_Node_Type_Id),
      Argument_Types          => (1 => (Kind => Unbounded_Text_Value)),
      Argument_Names          => (1 => Name_For_name'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Basic_Decl_P_Get_Representation_Clause : aliased constant Property_Descriptor :=
     (Name_Length => 25, Arity => 2,
Name              => "get_representation_clause",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Attribute_Def_Clause_Type_Id),
      Argument_Types =>
        (1 => (Kind => Unbounded_Text_Value), 2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_name'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Is_Compilation_Unit_Root : aliased constant Property_Descriptor :=
     (Name_Length => 24, Arity => 0,
Name              => "is_compilation_unit_root",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Is_Visible : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 1,
Name              => "is_visible",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_from_node'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Basic_Decl_P_Base_Subp_Declarations : aliased constant Property_Descriptor :=
     (Name_Length => 22, Arity => 0,
Name              => "base_subp_declarations",

      Return_Type             => (Kind => Basic_Decl_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Root_Subp_Declarations : aliased constant Property_Descriptor :=
     (Name_Length => 22, Arity => 1,
Name              => "root_subp_declarations",

      Return_Type    => (Kind => Basic_Decl_Array_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Basic_Decl_P_Find_All_Overrides : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 2,
Name              => "find_all_overrides",

      Return_Type    => (Kind => Basic_Decl_Array_Value),
      Argument_Types =>
        (1 => (Kind => Analysis_Unit_Array_Value),
         2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_units'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Defining_Names : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "defining_names",

      Return_Type             => (Kind => Defining_Name_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Defining_Name : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "defining_name",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Type_Expression : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "type_expression",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Type_Expr_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Subp_Spec_Or_Null : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 1,
Name              => "subp_spec_or_null",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Subp_Spec_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_follow_generic'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Basic_Decl_P_Is_Subprogram : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "is_subprogram",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Relative_Name : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "relative_name",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Single_Tok_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Relative_Name_Text : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 0,
Name              => "relative_name_text",

      Return_Type             => (Kind => Unbounded_Text_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Next_Part_For_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 0,
Name              => "next_part_for_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Body_Part_For_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 0,
Name              => "body_part_for_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Body_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Fully_Qualified_Name_Array : aliased constant Property_Descriptor :=
     (Name_Length => 26, Arity => 0,
Name              => "fully_qualified_name_array",

      Return_Type             => (Kind => Unbounded_Text_Type_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Fully_Qualified_Name : aliased constant Property_Descriptor :=
     (Name_Length => 20, Arity => 0,
Name              => "fully_qualified_name",

      Return_Type             => (Kind => Text_Type_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Canonical_Fully_Qualified_Name : aliased constant Property_Descriptor :=
     (Name_Length => 30, Arity => 0,
Name              => "canonical_fully_qualified_name",

      Return_Type             => (Kind => Text_Type_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Decl_P_Unique_Identifying_Name : aliased constant Property_Descriptor :=
     (Name_Length => 23, Arity => 0,
Name              => "unique_identifying_name",

      Return_Type             => (Kind => Text_Type_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Formal_Param_Decl_P_Formal_Type : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 1,
Name              => "formal_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Package_Decl_P_Body_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "body_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Package_Body_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Type_Decl_P_Base_Subtype : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 1,
Name              => "base_subtype",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Private_Completion : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 0,
Name              => "private_completion",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Type_Decl_P_Get_Record_Representation_Clause : aliased constant Property_Descriptor :=
     (Name_Length => 32, Arity => 1,

      Name => "get_record_representation_clause",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Record_Rep_Clause_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Base_Type_Decl_P_Get_Enum_Representation_Clause : aliased constant Property_Descriptor :=
     (Name_Length => 30, Arity => 1,
Name              => "get_enum_representation_clause",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Enum_Rep_Clause_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Base_Type_Decl_P_Is_Record_Type : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_record_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Array_Type : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 1,
Name              => "is_array_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Find_Derived_Types : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 3,
Name              => "find_derived_types",

      Return_Type    => (Kind => Type_Decl_Array_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
         3 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_root'Access, 2 => Name_For_origin'Access,
         3 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => No_Internal_Value,
         3 => Create_Boolean (False)));

   Desc_For_Base_Type_Decl_P_Is_Real_Type : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 1,
Name              => "is_real_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Float_Type : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 1,
Name              => "is_float_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Fixed_Point : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_fixed_point",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Enum_Type : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 1,
Name              => "is_enum_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Access_Type : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_access_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Char_Type : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 1,
Name              => "is_char_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Discrete_Range : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "discrete_range",

      Return_Type             => (Kind => Discrete_Range_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Type_Decl_P_Is_Discrete_Type : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 1,
Name              => "is_discrete_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Int_Type : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 1,
Name              => "is_int_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Accessed_Type : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 1,
Name              => "accessed_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Tagged_Type : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_tagged_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Base_Type : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 1,
Name              => "base_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Base_Types : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 1,
Name              => "base_types",

      Return_Type    => (Kind => Base_Type_Decl_Array_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Find_All_Derived_Types : aliased constant Property_Descriptor :=
     (Name_Length => 22, Arity => 2,
Name              => "find_all_derived_types",

      Return_Type    => (Kind => Type_Decl_Array_Value),
      Argument_Types =>
        (1 => (Kind => Analysis_Unit_Array_Value),
         2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_units'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Base_Type_Decl_P_Comp_Type : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 2,
Name              => "comp_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Boolean_Value),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names =>
        (1 => Name_For_is_subscript'Access, 2 => Name_For_origin'Access),
      Argument_Default_Values =>
        (1 => Create_Boolean (False), 2 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Index_Type : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 2,
Name              => "index_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Integer_Value),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names =>
        (1 => Name_For_dim'Access, 2 => Name_For_origin'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Derived_Type : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 2,
Name              => "is_derived_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names =>
        (1 => Name_For_other_type'Access, 2 => Name_For_origin'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Interface_Type : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 1,
Name              => "is_interface_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Matching_Type : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 2,
Name              => "matching_type",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names =>
        (1 => Name_For_expected_type'Access, 2 => Name_For_origin'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Canonical_Type : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "canonical_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Previous_Part : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 1,
Name              => "previous_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_go_to_incomplete'Access),
      Argument_Default_Values => (1 => Create_Boolean (True)));

   Desc_For_Base_Type_Decl_P_Next_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "next_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Type_Decl_P_Full_View : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "full_view",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Type_Decl_P_Is_Definite_Subtype : aliased constant Property_Descriptor :=
     (Name_Length => 19, Arity => 1,
Name              => "is_definite_subtype",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Base_Type_Decl_P_Is_Private : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 0,
Name              => "is_private",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Base_Type_Decl_P_Root_Type : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 1,
Name              => "root_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin'Access),
      Argument_Default_Values => (1 => Create_Node (No_Entity)));

   Desc_For_Type_Decl_P_Get_Primitives : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "get_primitives",

      Return_Type             => (Kind => Basic_Decl_Array_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_only_inherited'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Basic_Subp_Decl_P_Subp_Decl_Spec : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "subp_decl_spec",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Subp_Spec_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Basic_Subp_Decl_P_Body_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "body_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Subp_Body_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Enum_Literal_Decl_P_Enum_Type : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "enum_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Body_Node_P_Previous_Part : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "previous_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Body_Node_P_Decl_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "decl_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Body_Node_P_Subunit_Root : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 0,
Name              => "subunit_root",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Generic_Package_Decl_P_Body_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "body_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Package_Body_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Generic_Subp_Decl_P_Body_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "body_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Subp_Body_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Generic_Instantiation_P_Designated_Generic_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 23, Arity => 0,
Name              => "designated_generic_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Generic_Subp_Instantiation_P_Designated_Subp : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "designated_subp",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Object_Decl_P_Public_Part_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "public_part_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Package_Renaming_Decl_P_Renamed_Package : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "renamed_package",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Package_Renaming_Decl_P_Final_Renamed_Package : aliased constant Property_Descriptor :=
     (Name_Length => 21, Arity => 0,
Name              => "final_renamed_package",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Syntactic_Fully_Qualified_Name : aliased constant Property_Descriptor :=
     (Name_Length => 30, Arity => 0,
Name              => "syntactic_fully_qualified_name",

      Return_Type             => (Kind => Unbounded_Text_Type_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Unit_Kind : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "unit_kind",

      Return_Type             => (Kind => Analysis_Unit_Kind_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Withed_Units : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 0,
Name              => "withed_units",

      Return_Type             => (Kind => Compilation_Unit_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Imported_Units : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "imported_units",

      Return_Type             => (Kind => Compilation_Unit_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Unit_Dependencies : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 0,
Name              => "unit_dependencies",

      Return_Type             => (Kind => Compilation_Unit_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 4, Arity => 0,
Name              => "decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Compilation_Unit_P_Is_Preelaborable : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "is_preelaborable",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Constant_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Expr_P_Expression_Type : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "expression_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Expr_P_Is_Static_Expr : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_static_expr",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Expr_P_First_Corresponding_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 24, Arity => 0,
Name              => "first_corresponding_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Expr_P_Eval_As_Int : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 0,
Name              => "eval_as_int",

      Return_Type             => (Kind => Big_Integer_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Expr_P_Eval_As_Int_In_Env : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 1,
Name              => "eval_as_int_in_env",

      Return_Type             => (Kind => Big_Integer_Value),
      Argument_Types          => (1 => (Kind => Substitution_Array_Value)),
      Argument_Names          => (1 => Name_For_env'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Expr_P_Matching_Nodes : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "matching_nodes",

      Return_Type             => (Kind => Ada_Node_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Allocator_P_Get_Allocated_Type : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 0,
Name              => "get_allocated_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Enclosing_Defining_Name : aliased constant Property_Descriptor :=
     (Name_Length => 23, Arity => 0,
Name              => "enclosing_defining_name",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Defining : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 0,
Name              => "is_defining",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Name_Is : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 1,
Name              => "name_is",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Unbounded_Text_Value)),
      Argument_Names          => (1 => Name_For_sym'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Name_P_Is_Direct_Call : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "is_direct_call",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Access_Call : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "is_access_call",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Call : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "is_call",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Dot_Call : aliased constant Property_Descriptor :=
     (Name_Length => 11, Arity => 1,
Name              => "is_dot_call",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Failsafe_Referenced_Def_Name : aliased constant Property_Descriptor :=
     (Name_Length => 28, Arity => 1,
Name              => "failsafe_referenced_def_name",

      Return_Type             => (Kind => Refd_Def_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Referenced_Defining_Name : aliased constant Property_Descriptor :=
     (Name_Length => 24, Arity => 1,
Name              => "referenced_defining_name",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_All_Env_Elements : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 2,
Name              => "all_env_elements",

      Return_Type    => (Kind => Ada_Node_Array_Value),
      Argument_Types =>
        (1 => (Kind => Boolean_Value),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names =>
        (1 => Name_For_seq'Access, 2 => Name_For_seq_from'Access),
      Argument_Default_Values =>
        (1 => Create_Boolean (True), 2 => Create_Node (No_Entity)));

   Desc_For_Name_P_Called_Subp_Spec : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "called_subp_spec",

      Return_Type =>
        (Kind      => Node_Value,
         Node_Type => Common.Base_Formal_Param_Holder_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Referenced_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 1,
Name              => "referenced_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Failsafe_Referenced_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 24, Arity => 1,
Name              => "failsafe_referenced_decl",

      Return_Type             => (Kind => Refd_Decl_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Referenced_Decl_Internal : aliased constant Property_Descriptor :=
     (Name_Length => 24, Arity => 1,
Name              => "referenced_decl_internal",

      Return_Type             => (Kind => Refd_Decl_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Name_Designated_Type : aliased constant Property_Descriptor :=
     (Name_Length => 20, Arity => 0,
Name              => "name_designated_type",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Static_Subtype : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 1,
Name              => "is_static_subtype",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Name_Matches : aliased constant Property_Descriptor :=
     (Name_Length => 12, Arity => 1,
Name              => "name_matches",

      Return_Type    => (Kind => Boolean_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Name_Type_Id)),
      Argument_Names          => (1 => Name_For_n'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Name_P_Relative_Name : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "relative_name",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Single_Tok_Node_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Operator_Name : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "is_operator_name",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Name_P_Is_Write_Reference : aliased constant Property_Descriptor :=
     (Name_Length => 18, Arity => 1,
Name              => "is_write_reference",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Is_Dispatching_Call : aliased constant Property_Descriptor :=
     (Name_Length => 19, Arity => 1,
Name              => "is_dispatching_call",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_Is_Static_Call : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 1,
Name              => "is_static_call",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Name_P_As_Symbol_Array : aliased constant Property_Descriptor :=
     (Name_Length => 15, Arity => 0,
Name              => "as_symbol_array",

      Return_Type             => (Kind => Unbounded_Text_Type_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Call_Expr_P_Is_Array_Slice : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "is_array_slice",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Defining_Name_P_Basic_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 0,
Name              => "basic_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Defining_Name_P_Find_Refs : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 3,
Name              => "find_refs",

      Return_Type    => (Kind => Ref_Result_Array_Value),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
         2 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id),
         3 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_root'Access, 2 => Name_For_origin'Access,
         3 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => No_Internal_Value,
         3 => Create_Boolean (False)));

   Desc_For_Defining_Name_P_Find_All_References : aliased constant Property_Descriptor :=
     (Name_Length => 19, Arity => 2,
Name              => "find_all_references",

      Return_Type    => (Kind => Ref_Result_Array_Value),
      Argument_Types =>
        (1 => (Kind => Analysis_Unit_Array_Value),
         2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_units'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Defining_Name_P_Find_All_Calls : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 2,
Name              => "find_all_calls",

      Return_Type    => (Kind => Ref_Result_Array_Value),
      Argument_Types =>
        (1 => (Kind => Analysis_Unit_Array_Value),
         2 => (Kind => Boolean_Value)),
      Argument_Names =>
        (1 => Name_For_units'Access, 2 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values =>
        (1 => No_Internal_Value, 2 => Create_Boolean (False)));

   Desc_For_Defining_Name_P_Next_Part : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "next_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Defining_Name_P_Previous_Part : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "previous_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Defining_Name_P_Canonical_Part : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "canonical_part",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Defining_Name_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_End_Name_P_Basic_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 10, Arity => 0,
Name              => "basic_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Single_Tok_Node_P_Canonical_Text : aliased constant Property_Descriptor :=
     (Name_Length => 14, Arity => 0,
Name              => "canonical_text",

      Return_Type             => (Kind => Unbounded_Text_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Char_Literal_P_Denoted_Value : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "denoted_value",

      Return_Type             => (Kind => Character_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_String_Literal_P_Denoted_Value : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "denoted_value",

      Return_Type             => (Kind => Text_Type_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Int_Literal_P_Denoted_Value : aliased constant Property_Descriptor :=
     (Name_Length => 13, Arity => 0,
Name              => "denoted_value",

      Return_Type             => (Kind => Big_Integer_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Limited_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Not_Null_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Pragma_Node_P_Associated_Decls : aliased constant Property_Descriptor :=
     (Name_Length => 16, Arity => 0,
Name              => "associated_decls",

      Return_Type             => (Kind => Basic_Decl_Array_Value),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Private_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Protected_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Reverse_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Subunit_P_Body_Root : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "body_root",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Basic_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Synchronized_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Tagged_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Type_Expr_P_Type_Name : aliased constant Property_Descriptor :=
     (Name_Length => 9, Arity => 0,
Name              => "type_name",

      Return_Type => (Kind => Node_Value, Node_Type => Common.Name_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Type_Expr_P_Designated_Type_Decl : aliased constant Property_Descriptor :=
     (Name_Length => 20, Arity => 0,
Name              => "designated_type_decl",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types => (1 .. 0 => <>), Argument_Names => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_Type_Expr_P_Designated_Type_Decl_From : aliased constant Property_Descriptor :=
     (Name_Length => 25, Arity => 1,
Name              => "designated_type_decl_from",

      Return_Type =>
        (Kind => Node_Value, Node_Type => Common.Base_Type_Decl_Type_Id),
      Argument_Types =>
        (1 => (Kind => Node_Value, Node_Type => Common.Ada_Node_Type_Id)),
      Argument_Names          => (1 => Name_For_origin_node'Access),
      Argument_Default_Values => (1 => No_Internal_Value));

   Desc_For_Subtype_Indication_P_Is_Static_Subtype : aliased constant Property_Descriptor :=
     (Name_Length => 17, Arity => 1,
Name              => "is_static_subtype",

      Return_Type             => (Kind => Boolean_Value),
      Argument_Types          => (1 => (Kind => Boolean_Value)),
      Argument_Names          => (1 => Name_For_imprecise_fallback'Access),
      Argument_Default_Values => (1 => Create_Boolean (False)));

   Desc_For_Until_Node_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Desc_For_With_Private_P_As_Bool : aliased constant Property_Descriptor :=
     (Name_Length => 7, Arity => 0,
Name              => "as_bool",

      Return_Type => (Kind => Boolean_Value), Argument_Types => (1 .. 0 => <>),
      Argument_Names          => (1 .. 0 => <>),
      Argument_Default_Values => (1 .. 0 => <>));

   Property_Descriptors : constant array
     (Property_Reference) of Property_Descriptor_Access :=
     (Desc_For_Ada_Node_P_Declarative_Scope'Access,
      Desc_For_Ada_Node_P_Complete'Access,
      Desc_For_Ada_Node_P_Valid_Keywords'Access,
      Desc_For_Ada_Node_P_Generic_Instantiations'Access,
      Desc_For_Ada_Node_P_Semantic_Parent'Access,
      Desc_For_Ada_Node_P_Parent_Basic_Decl'Access,
      Desc_For_Ada_Node_P_Filter_Is_Imported_By'Access,
      Desc_For_Ada_Node_P_Xref_Entry_Point'Access,
      Desc_For_Ada_Node_P_Resolve_Names'Access,
      Desc_For_Ada_Node_P_Standard_Unit'Access,
      Desc_For_Ada_Node_P_Std_Entity'Access,
      Desc_For_Ada_Node_P_Bool_Type'Access,
      Desc_For_Ada_Node_P_Int_Type'Access,
      Desc_For_Ada_Node_P_Universal_Int_Type'Access,
      Desc_For_Ada_Node_P_Universal_Real_Type'Access,
      Desc_For_Ada_Node_P_Top_Level_Decl'Access,
      Desc_For_Ada_Node_P_Choice_Match'Access,
      Desc_For_Ada_Node_P_Gnat_Xref'Access, Desc_For_Ada_Node_Parent'Access,
      Desc_For_Ada_Node_Parents'Access, Desc_For_Ada_Node_Children'Access,
      Desc_For_Ada_Node_Token_Start'Access, Desc_For_Ada_Node_Token_End'Access,
      Desc_For_Ada_Node_Child_Index'Access,
      Desc_For_Ada_Node_Previous_Sibling'Access,
      Desc_For_Ada_Node_Next_Sibling'Access, Desc_For_Ada_Node_Unit'Access,
      Desc_For_Ada_Node_Is_Ghost'Access,
      Desc_For_Ada_Node_Full_Sloc_Image'Access,
      Desc_For_Abort_Node_P_As_Bool'Access,
      Desc_For_Abstract_Node_P_As_Bool'Access,
      Desc_For_Assoc_List_P_Zip_With_Params'Access,
      Desc_For_Aliased_Node_P_As_Bool'Access,
      Desc_For_All_Node_P_As_Bool'Access,
      Desc_For_Base_Assoc_P_Assoc_Expr'Access,
      Desc_For_Base_Formal_Param_Holder_P_Abstract_Formal_Params'Access,
      Desc_For_Base_Formal_Param_Holder_P_Nb_Min_Params'Access,
      Desc_For_Base_Formal_Param_Holder_P_Nb_Max_Params'Access,
      Desc_For_Base_Formal_Param_Holder_P_Param_Types'Access,
      Desc_For_Base_Subp_Spec_P_Returns'Access,
      Desc_For_Base_Subp_Spec_P_Params'Access,
      Desc_For_Base_Subp_Spec_P_Primitive_Subp_Types'Access,
      Desc_For_Base_Subp_Spec_P_Primitive_Subp_First_Type'Access,
      Desc_For_Base_Subp_Spec_P_Primitive_Subp_Tagged_Type'Access,
      Desc_For_Base_Subp_Spec_P_Return_Type'Access,
      Desc_For_Basic_Assoc_P_Get_Params'Access,
      Desc_For_Basic_Decl_P_Is_Formal'Access,
      Desc_For_Basic_Decl_P_Doc_Annotations'Access,
      Desc_For_Basic_Decl_P_Doc'Access,
      Desc_For_Basic_Decl_P_Previous_Part_For_Decl'Access,
      Desc_For_Basic_Decl_P_Canonical_Part'Access,
      Desc_For_Basic_Decl_P_Is_Static_Decl'Access,
      Desc_For_Basic_Decl_P_Is_Imported'Access,
      Desc_For_Basic_Decl_P_Get_Aspect_Assoc'Access,
      Desc_For_Basic_Decl_P_Get_Aspect_Spec_Expr'Access,
      Desc_For_Basic_Decl_P_Get_Aspect'Access,
      Desc_For_Basic_Decl_P_Has_Aspect'Access,
      Desc_For_Basic_Decl_P_Get_Pragma'Access,
      Desc_For_Basic_Decl_P_Get_Representation_Clause'Access,
      Desc_For_Basic_Decl_P_Is_Compilation_Unit_Root'Access,
      Desc_For_Basic_Decl_P_Is_Visible'Access,
      Desc_For_Basic_Decl_P_Base_Subp_Declarations'Access,
      Desc_For_Basic_Decl_P_Root_Subp_Declarations'Access,
      Desc_For_Basic_Decl_P_Find_All_Overrides'Access,
      Desc_For_Basic_Decl_P_Defining_Names'Access,
      Desc_For_Basic_Decl_P_Defining_Name'Access,
      Desc_For_Basic_Decl_P_Type_Expression'Access,
      Desc_For_Basic_Decl_P_Subp_Spec_Or_Null'Access,
      Desc_For_Basic_Decl_P_Is_Subprogram'Access,
      Desc_For_Basic_Decl_P_Relative_Name'Access,
      Desc_For_Basic_Decl_P_Relative_Name_Text'Access,
      Desc_For_Basic_Decl_P_Next_Part_For_Decl'Access,
      Desc_For_Basic_Decl_P_Body_Part_For_Decl'Access,
      Desc_For_Basic_Decl_P_Fully_Qualified_Name_Array'Access,
      Desc_For_Basic_Decl_P_Fully_Qualified_Name'Access,
      Desc_For_Basic_Decl_P_Canonical_Fully_Qualified_Name'Access,
      Desc_For_Basic_Decl_P_Unique_Identifying_Name'Access,
      Desc_For_Base_Formal_Param_Decl_P_Formal_Type'Access,
      Desc_For_Base_Package_Decl_P_Body_Part'Access,
      Desc_For_Base_Type_Decl_P_Base_Subtype'Access,
      Desc_For_Base_Type_Decl_P_Private_Completion'Access,
      Desc_For_Base_Type_Decl_P_Get_Record_Representation_Clause'Access,
      Desc_For_Base_Type_Decl_P_Get_Enum_Representation_Clause'Access,
      Desc_For_Base_Type_Decl_P_Is_Record_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Array_Type'Access,
      Desc_For_Base_Type_Decl_P_Find_Derived_Types'Access,
      Desc_For_Base_Type_Decl_P_Is_Real_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Float_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Fixed_Point'Access,
      Desc_For_Base_Type_Decl_P_Is_Enum_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Access_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Char_Type'Access,
      Desc_For_Base_Type_Decl_P_Discrete_Range'Access,
      Desc_For_Base_Type_Decl_P_Is_Discrete_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Int_Type'Access,
      Desc_For_Base_Type_Decl_P_Accessed_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Tagged_Type'Access,
      Desc_For_Base_Type_Decl_P_Base_Type'Access,
      Desc_For_Base_Type_Decl_P_Base_Types'Access,
      Desc_For_Base_Type_Decl_P_Find_All_Derived_Types'Access,
      Desc_For_Base_Type_Decl_P_Comp_Type'Access,
      Desc_For_Base_Type_Decl_P_Index_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Derived_Type'Access,
      Desc_For_Base_Type_Decl_P_Is_Interface_Type'Access,
      Desc_For_Base_Type_Decl_P_Matching_Type'Access,
      Desc_For_Base_Type_Decl_P_Canonical_Type'Access,
      Desc_For_Base_Type_Decl_P_Previous_Part'Access,
      Desc_For_Base_Type_Decl_P_Next_Part'Access,
      Desc_For_Base_Type_Decl_P_Full_View'Access,
      Desc_For_Base_Type_Decl_P_Is_Definite_Subtype'Access,
      Desc_For_Base_Type_Decl_P_Is_Private'Access,
      Desc_For_Base_Type_Decl_P_Root_Type'Access,
      Desc_For_Type_Decl_P_Get_Primitives'Access,
      Desc_For_Basic_Subp_Decl_P_Subp_Decl_Spec'Access,
      Desc_For_Basic_Subp_Decl_P_Body_Part'Access,
      Desc_For_Enum_Literal_Decl_P_Enum_Type'Access,
      Desc_For_Body_Node_P_Previous_Part'Access,
      Desc_For_Body_Node_P_Decl_Part'Access,
      Desc_For_Body_Node_P_Subunit_Root'Access,
      Desc_For_Generic_Package_Decl_P_Body_Part'Access,
      Desc_For_Generic_Subp_Decl_P_Body_Part'Access,
      Desc_For_Generic_Instantiation_P_Designated_Generic_Decl'Access,
      Desc_For_Generic_Subp_Instantiation_P_Designated_Subp'Access,
      Desc_For_Object_Decl_P_Public_Part_Decl'Access,
      Desc_For_Package_Renaming_Decl_P_Renamed_Package'Access,
      Desc_For_Package_Renaming_Decl_P_Final_Renamed_Package'Access,
      Desc_For_Compilation_Unit_P_Syntactic_Fully_Qualified_Name'Access,
      Desc_For_Compilation_Unit_P_Unit_Kind'Access,
      Desc_For_Compilation_Unit_P_Withed_Units'Access,
      Desc_For_Compilation_Unit_P_Imported_Units'Access,
      Desc_For_Compilation_Unit_P_Unit_Dependencies'Access,
      Desc_For_Compilation_Unit_P_Decl'Access,
      Desc_For_Compilation_Unit_P_Is_Preelaborable'Access,
      Desc_For_Constant_Node_P_As_Bool'Access,
      Desc_For_Expr_P_Expression_Type'Access,
      Desc_For_Expr_P_Is_Static_Expr'Access,
      Desc_For_Expr_P_First_Corresponding_Decl'Access,
      Desc_For_Expr_P_Eval_As_Int'Access,
      Desc_For_Expr_P_Eval_As_Int_In_Env'Access,
      Desc_For_Expr_P_Matching_Nodes'Access,
      Desc_For_Allocator_P_Get_Allocated_Type'Access,
      Desc_For_Name_P_Enclosing_Defining_Name'Access,
      Desc_For_Name_P_Is_Defining'Access, Desc_For_Name_P_Name_Is'Access,
      Desc_For_Name_P_Is_Direct_Call'Access,
      Desc_For_Name_P_Is_Access_Call'Access, Desc_For_Name_P_Is_Call'Access,
      Desc_For_Name_P_Is_Dot_Call'Access,
      Desc_For_Name_P_Failsafe_Referenced_Def_Name'Access,
      Desc_For_Name_P_Referenced_Defining_Name'Access,
      Desc_For_Name_P_All_Env_Elements'Access,
      Desc_For_Name_P_Called_Subp_Spec'Access,
      Desc_For_Name_P_Referenced_Decl'Access,
      Desc_For_Name_P_Failsafe_Referenced_Decl'Access,
      Desc_For_Name_P_Referenced_Decl_Internal'Access,
      Desc_For_Name_P_Name_Designated_Type'Access,
      Desc_For_Name_P_Is_Static_Subtype'Access,
      Desc_For_Name_P_Name_Matches'Access,
      Desc_For_Name_P_Relative_Name'Access,
      Desc_For_Name_P_Is_Operator_Name'Access,
      Desc_For_Name_P_Is_Write_Reference'Access,
      Desc_For_Name_P_Is_Dispatching_Call'Access,
      Desc_For_Name_P_Is_Static_Call'Access,
      Desc_For_Name_P_As_Symbol_Array'Access,
      Desc_For_Call_Expr_P_Is_Array_Slice'Access,
      Desc_For_Defining_Name_P_Basic_Decl'Access,
      Desc_For_Defining_Name_P_Find_Refs'Access,
      Desc_For_Defining_Name_P_Find_All_References'Access,
      Desc_For_Defining_Name_P_Find_All_Calls'Access,
      Desc_For_Defining_Name_P_Next_Part'Access,
      Desc_For_Defining_Name_P_Previous_Part'Access,
      Desc_For_Defining_Name_P_Canonical_Part'Access,
      Desc_For_End_Name_P_Basic_Decl'Access,
      Desc_For_Single_Tok_Node_P_Canonical_Text'Access,
      Desc_For_Char_Literal_P_Denoted_Value'Access,
      Desc_For_String_Literal_P_Denoted_Value'Access,
      Desc_For_Int_Literal_P_Denoted_Value'Access,
      Desc_For_Limited_Node_P_As_Bool'Access,
      Desc_For_Not_Null_P_As_Bool'Access,
      Desc_For_Pragma_Node_P_Associated_Decls'Access,
      Desc_For_Private_Node_P_As_Bool'Access,
      Desc_For_Protected_Node_P_As_Bool'Access,
      Desc_For_Reverse_Node_P_As_Bool'Access,
      Desc_For_Subunit_P_Body_Root'Access,
      Desc_For_Synchronized_Node_P_As_Bool'Access,
      Desc_For_Tagged_Node_P_As_Bool'Access,
      Desc_For_Type_Expr_P_Type_Name'Access,
      Desc_For_Type_Expr_P_Designated_Type_Decl'Access,
      Desc_For_Type_Expr_P_Designated_Type_Decl_From'Access,
      Desc_For_Subtype_Indication_P_Is_Static_Subtype'Access,
      Desc_For_Until_Node_P_As_Bool'Access,
      Desc_For_With_Private_P_As_Bool'Access);

   ---------------------------
   -- Node type descriptors --
   ---------------------------

   type Node_Field_Descriptor (Is_Abstract_Or_Null : Boolean) is record
      Field : Field_Reference;
      --  Reference to the field this describes

      --  Only non-null concrete fields are assigned an index

      case Is_Abstract_Or_Null is
         when False =>
            Index : Positive;
            --  Index for this field

         when True =>
            null;
      end case;
   end record;
   --  Description of a field as implemented by a specific node

   type Node_Field_Descriptor_Access is access constant Node_Field_Descriptor;
   type Node_Field_Descriptor_Array is
     array (Positive range <>) of Node_Field_Descriptor_Access;

   type Node_Type_Descriptor (Is_Abstract : Boolean;
      Derivations_Count                   : Natural; Fields_Count : Natural;
      Properties_Count                    : Natural) is record
      Base_Type : Any_Node_Type_Id;
      --  Reference to the node type from which this derives

      Derivations : Node_Type_Id_Array (1 .. Derivations_Count);
      --  List of references for all node types that derives from this

      DSL_Name : Unbounded_String;
      --  Name for this type in the Langkit DSL

      Inherited_Fields : Natural;
      --  Number of syntax field inherited from the base type

      Fields : Node_Field_Descriptor_Array (1 .. Fields_Count);
      --  For regular node types, list of syntax fields that are specific to
      --  this derivation (i.e. excluding fields from the base type).

      Properties : Property_Reference_Array (1 .. Properties_Count);
      --  List of properties that this node provides that are specific to this
      --  derivation (i.e. excluding fields from the base type).

      --  Only concrete nodes are assigned a node kind

      case Is_Abstract is
         when False =>
            Kind : Ada_Node_Kind_Type;
            --  Kind corresponding this this node type

         when True =>
            null;
      end case;
   end record;

   type Node_Type_Descriptor_Access is access constant Node_Type_Descriptor;

   --  Descriptors for node types and their syntax fields

   Desc_For_Ada_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 61, Fields_Count => 0,
      Properties_Count => 29,
Base_Type              => None,
      Derivations      =>
        (1  => Common.Ada_List_Type_Id, 2 => Common.Basic_Decl_Type_Id,
         3  => Common.Base_Formal_Param_Holder_Type_Id,
         4  => Common.Type_Def_Type_Id, 5 => Common.Variant_Type_Id,
         6 => Common.Variant_Part_Type_Id, 7 => Common.Base_Record_Def_Type_Id,
         8  => Common.Tagged_Node_Type_Id, 9 => Common.Abstract_Node_Type_Id,
         10 => Common.Limited_Node_Type_Id, 11 => Common.Private_Node_Type_Id,
         12 => Common.Aliased_Node_Type_Id, 13 => Common.Not_Null_Type_Id,
         14 => Common.Constant_Node_Type_Id, 15 => Common.All_Node_Type_Id,
         16 => Common.Abort_Node_Type_Id, 17 => Common.Reverse_Node_Type_Id,
         18 => Common.With_Private_Type_Id, 19 => Common.Until_Node_Type_Id,
         20 => Common.Synchronized_Node_Type_Id,
         21 => Common.Protected_Node_Type_Id,
         22 => Common.Prim_Type_Accessor_Type_Id,
         23 => Common.Base_Assoc_Type_Id, 24 => Common.Constraint_Type_Id,
         25 => Common.Basic_Assoc_Type_Id, 26 => Common.Array_Indices_Type_Id,
         27 => Common.Component_Def_Type_Id,
         28 => Common.Interface_Kind_Type_Id, 29 => Common.Task_Def_Type_Id,
         30 => Common.Protected_Def_Type_Id,
         31 => Common.Null_Component_Decl_Type_Id,
         32 => Common.With_Clause_Type_Id, 33 => Common.Use_Clause_Type_Id,
         34 => Common.Type_Expr_Type_Id, 35 => Common.Mode_Type_Id,
         36 => Common.Aspect_Spec_Type_Id,
         37 => Common.Overriding_Node_Type_Id,
         38 => Common.Pragma_Node_Type_Id, 39 => Common.Aspect_Clause_Type_Id,
         40 => Common.Component_Clause_Type_Id,
         41 => Common.Aspect_Assoc_Type_Id,
         42 => Common.Declarative_Part_Type_Id,
         43 => Common.Renaming_Clause_Type_Id, 44 => Common.Subp_Kind_Type_Id,
         45 => Common.Expr_Type_Id, 46 => Common.Others_Designator_Type_Id,
         47 => Common.Elsif_Expr_Part_Type_Id, 48 => Common.Quantifier_Type_Id,
         49 => Common.Iter_Type_Type_Id, 50 => Common.Loop_Spec_Type_Id,
         51 => Common.Compilation_Unit_Type_Id,
         52 => Common.Handled_Stmts_Type_Id, 53 => Common.Stmt_Type_Id,
         54 => Common.Elsif_Stmt_Part_Type_Id,
         55 => Common.Case_Stmt_Alternative_Type_Id,
         56 => Common.Select_When_Part_Type_Id, 57 => Common.Subunit_Type_Id,
         58 => Common.Library_Item_Type_Id, 59 => Common.Range_Spec_Type_Id,
         60 => Common.Params_Type_Id,
         61 => Common.Unconstrained_Array_Index_Type_Id),

      DSL_Name   => To_Unbounded_String ("AdaNode"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1  => Ada_Node_P_Declarative_Scope, 2 => Ada_Node_P_Complete,
         3  => Ada_Node_P_Valid_Keywords,
         4  => Ada_Node_P_Generic_Instantiations,
         5  => Ada_Node_P_Semantic_Parent, 6 => Ada_Node_P_Parent_Basic_Decl,
         7  => Ada_Node_P_Filter_Is_Imported_By,
         8  => Ada_Node_P_Xref_Entry_Point, 9 => Ada_Node_P_Resolve_Names,
         10 => Ada_Node_P_Standard_Unit, 11 => Ada_Node_P_Std_Entity,
         12 => Ada_Node_P_Bool_Type, 13 => Ada_Node_P_Int_Type,
         14 => Ada_Node_P_Universal_Int_Type,
         15 => Ada_Node_P_Universal_Real_Type, 16 => Ada_Node_P_Top_Level_Decl,
         17 => Ada_Node_P_Choice_Match, 18 => Ada_Node_P_Gnat_Xref,
         19 => Ada_Node_Parent, 20 => Ada_Node_Parents,
         21 => Ada_Node_Children, 22 => Ada_Node_Token_Start,
         23 => Ada_Node_Token_End, 24 => Ada_Node_Child_Index,
         25 => Ada_Node_Previous_Sibling, 26 => Ada_Node_Next_Sibling,
         27 => Ada_Node_Unit, 28 => Ada_Node_Is_Ghost,
         29 => Ada_Node_Full_Sloc_Image)
);

   Desc_For_Abort_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Abort_Present_Type_Id, 2 => Common.Abort_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Abort"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Abort_Node_P_As_Bool)
);

   Desc_For_Abort_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Abort_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Abort.Absent"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Abort_Absent);

   Desc_For_Abort_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Abort_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Abort.Present"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Abort_Present);

   Desc_For_Abstract_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Abstract_Present_Type_Id,
         2 => Common.Abstract_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Abstract"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Abstract_Node_P_As_Bool)
);

   Desc_For_Abstract_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Abstract_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Abstract.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Abstract_Absent);

   Desc_For_Abstract_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Abstract_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Abstract.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Abstract_Present);

   Desc_For_Ada_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 21, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1  => Common.Contract_Case_Assoc_List_Type_Id,
         2  => Common.Basic_Assoc_List_Type_Id,
         3  => Common.Ada_Node_List_Type_Id,
         4  => Common.Param_Spec_List_Type_Id, 5 => Common.Name_List_Type_Id,
         6  => Common.Identifier_List_Type_Id, 7 => Common.Expr_List_Type_Id,
         8  => Common.Aspect_Assoc_List_Type_Id,
         9  => Common.Defining_Name_List_Type_Id,
         10 => Common.Elsif_Expr_Part_List_Type_Id,
         11 => Common.Select_When_Part_List_Type_Id,
         12 => Common.Unconstrained_Array_Index_List_Type_Id,
         13 => Common.Pragma_Node_List_Type_Id,
         14 => Common.Elsif_Stmt_Part_List_Type_Id,
         15 => Common.Discriminant_Spec_List_Type_Id,
         16 => Common.Case_Stmt_Alternative_List_Type_Id,
         17 => Common.Compilation_Unit_List_Type_Id,
         18 => Common.Variant_List_Type_Id,
         19 => Common.Base_Assoc_List_Type_Id,
         20 => Common.Case_Expr_Alternative_List_Type_Id,
         21 => Common.Enum_Literal_Decl_List_Type_Id),

      DSL_Name   => To_Unbounded_String ("AdaList"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Ada_Node_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      =>
        (1 => Common.Decl_List_Type_Id, 2 => Common.Stmt_List_Type_Id,
         3 => Common.Alternatives_List_Type_Id,
         4 => Common.Constraint_List_Type_Id),

      DSL_Name   => To_Unbounded_String ("AdaNode.list"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Ada_Node_List);

   Desc_For_Alternatives_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AlternativesList"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Alternatives_List);

   Desc_For_Constraint_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ConstraintList"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Constraint_List);

   Desc_For_Decl_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("DeclList"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Decl_List);

   Desc_For_Stmt_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("StmtList"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Stmt_List);

   Desc_For_Aspect_Assoc_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AspectAssoc.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Aspect_Assoc_List);

   Desc_For_Base_Assoc_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("BaseAssoc.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Base_Assoc_List);

   Desc_For_Basic_Assoc_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 1, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 => Common.Assoc_List_Type_Id),

      DSL_Name => To_Unbounded_String ("BasicAssoc.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
);

   Desc_For_Assoc_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Basic_Assoc_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AssocList"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties => (1 => Assoc_List_P_Zip_With_Params)
,
      Kind => Ada_Assoc_List);

   Desc_For_Case_Expr_Alternative_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseExprAlternative.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Case_Expr_Alternative_List);

   Desc_For_Case_Stmt_Alternative_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseStmtAlternative.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Case_Stmt_Alternative_List);

   Desc_For_Compilation_Unit_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompilationUnit.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Compilation_Unit_List);

   Desc_For_Contract_Case_Assoc_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ContractCaseAssoc.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Contract_Case_Assoc_List);

   Desc_For_Defining_Name_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DefiningName.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Defining_Name_List);

   Desc_For_Discriminant_Spec_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscriminantSpec.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discriminant_Spec_List);

   Desc_For_Elsif_Expr_Part_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ElsifExprPart.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Elsif_Expr_Part_List);

   Desc_For_Elsif_Stmt_Part_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ElsifStmtPart.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Elsif_Stmt_Part_List);

   Desc_For_Enum_Literal_Decl_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EnumLiteralDecl.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Enum_Literal_Decl_List);

   Desc_For_Expr_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 1, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 => Common.Expr_Alternatives_List_Type_Id),

      DSL_Name   => To_Unbounded_String ("Expr.list"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Expr_Alternatives_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Expr_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExprAlternativesList"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Expr_Alternatives_List);

   Desc_For_Identifier_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 1, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 => Common.Discriminant_Choice_List_Type_Id),

      DSL_Name => To_Unbounded_String ("Identifier.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
);

   Desc_For_Discriminant_Choice_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Identifier_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscriminantChoiceList"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discriminant_Choice_List);

   Desc_For_Name_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 => Common.Parent_List_Type_Id),

      DSL_Name   => To_Unbounded_String ("Name.list"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Name_List);

   Desc_For_Parent_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Name_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ParentList"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Parent_List);

   Desc_For_Param_Spec_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ParamSpec.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Param_Spec_List);

   Desc_For_Pragma_Node_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Pragma.list"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Pragma_Node_List);

   Desc_For_Select_When_Part_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SelectWhenPart.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Select_When_Part_List);

   Desc_For_Unconstrained_Array_Index_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("UnconstrainedArrayIndex.list"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
,
      Kind => Ada_Unconstrained_Array_Index_List);

   Desc_For_Variant_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_List_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Variant.list"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Variant_List);

   Desc_For_Aliased_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Aliased_Present_Type_Id,
         2 => Common.Aliased_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Aliased"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Aliased_Node_P_As_Bool)
);

   Desc_For_Aliased_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Aliased_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Aliased.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Aliased_Absent);

   Desc_For_Aliased_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Aliased_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Aliased.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Aliased_Present);

   Desc_For_All_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.All_Present_Type_Id, 2 => Common.All_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("All"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => All_Node_P_As_Bool)
);

   Desc_For_All_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.All_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("All.Absent"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_All_Absent);

   Desc_For_All_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.All_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("All.Present"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_All_Present);

   Desc_For_Array_Indices : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Unconstrained_Array_Indices_Type_Id,
         2 => Common.Constrained_Array_Indices_Type_Id),

      DSL_Name   => To_Unbounded_String ("ArrayIndices"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Constrained_Array_Indices_F_List_For_Constrained_Array_Indices : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Constrained_Array_Indices_F_List
,
      Index => 1);

   Desc_For_Constrained_Array_Indices : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Array_Indices_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ConstrainedArrayIndices"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Constrained_Array_Indices_F_List_For_Constrained_Array_Indices'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Constrained_Array_Indices);

   Unconstrained_Array_Indices_F_Types_For_Unconstrained_Array_Indices : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Unconstrained_Array_Indices_F_Types
, Index                   => 1);

   Desc_For_Unconstrained_Array_Indices : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Array_Indices_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("UnconstrainedArrayIndices"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Unconstrained_Array_Indices_F_Types_For_Unconstrained_Array_Indices'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Unconstrained_Array_Indices);

   Aspect_Assoc_F_Id_For_Aspect_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Aspect_Assoc_F_Id
, Index                   => 1);
   Aspect_Assoc_F_Expr_For_Aspect_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Aspect_Assoc_F_Expr
, Index                   => 2);

   Desc_For_Aspect_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AspectAssoc"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Aspect_Assoc_F_Id_For_Aspect_Assoc'Access,
         2 => Aspect_Assoc_F_Expr_For_Aspect_Assoc'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Aspect_Assoc);

   Desc_For_Aspect_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Enum_Rep_Clause_Type_Id,
         2 => Common.Attribute_Def_Clause_Type_Id,
         3 => Common.Record_Rep_Clause_Type_Id, 4 => Common.At_Clause_Type_Id),

      DSL_Name   => To_Unbounded_String ("AspectClause"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   At_Clause_F_Name_For_At_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => At_Clause_F_Name
, Index                   => 1);
   At_Clause_F_Expr_For_At_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => At_Clause_F_Expr
, Index                   => 2);

   Desc_For_At_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Aspect_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AtClause"),
Inherited_Fields => 0,
      Fields     =>
        (1 => At_Clause_F_Name_For_At_Clause'Access,
         2 => At_Clause_F_Expr_For_At_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_At_Clause);

   Attribute_Def_Clause_F_Attribute_Expr_For_Attribute_Def_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Attribute_Def_Clause_F_Attribute_Expr
, Index                   => 1);
   Attribute_Def_Clause_F_Expr_For_Attribute_Def_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Attribute_Def_Clause_F_Expr
,
      Index => 2);

   Desc_For_Attribute_Def_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Aspect_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AttributeDefClause"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Attribute_Def_Clause_F_Attribute_Expr_For_Attribute_Def_Clause'
             Access,
         2 => Attribute_Def_Clause_F_Expr_For_Attribute_Def_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Attribute_Def_Clause);

   Enum_Rep_Clause_F_Type_Name_For_Enum_Rep_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Enum_Rep_Clause_F_Type_Name
,
      Index => 1);
   Enum_Rep_Clause_F_Aggregate_For_Enum_Rep_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Enum_Rep_Clause_F_Aggregate
,
      Index => 2);

   Desc_For_Enum_Rep_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Aspect_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EnumRepClause"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Enum_Rep_Clause_F_Type_Name_For_Enum_Rep_Clause'Access,
         2 => Enum_Rep_Clause_F_Aggregate_For_Enum_Rep_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Enum_Rep_Clause);

   Record_Rep_Clause_F_Name_For_Record_Rep_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Rep_Clause_F_Name
,
      Index => 1);
   Record_Rep_Clause_F_At_Expr_For_Record_Rep_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Rep_Clause_F_At_Expr
,
      Index => 2);
   Record_Rep_Clause_F_Components_For_Record_Rep_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Rep_Clause_F_Components
,
      Index => 3);

   Desc_For_Record_Rep_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Aspect_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("RecordRepClause"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Record_Rep_Clause_F_Name_For_Record_Rep_Clause'Access,
         2 => Record_Rep_Clause_F_At_Expr_For_Record_Rep_Clause'Access,
         3 => Record_Rep_Clause_F_Components_For_Record_Rep_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Record_Rep_Clause);

   Aspect_Spec_F_Aspect_Assocs_For_Aspect_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Aspect_Spec_F_Aspect_Assocs
,
      Index => 1);

   Desc_For_Aspect_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AspectSpec"),
Inherited_Fields => 0,
      Fields     => (1 => Aspect_Spec_F_Aspect_Assocs_For_Aspect_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Aspect_Spec);

   Desc_For_Base_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Pragma_Argument_Assoc_Type_Id,
         2 => Common.Contract_Case_Assoc_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseAssoc"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Base_Assoc_P_Assoc_Expr)
);

   Contract_Case_Assoc_F_Guard_For_Contract_Case_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Contract_Case_Assoc_F_Guard
,
      Index => 1);
   Contract_Case_Assoc_F_Consequence_For_Contract_Case_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Contract_Case_Assoc_F_Consequence
,
      Index => 2);

   Desc_For_Contract_Case_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Base_Assoc_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ContractCaseAssoc"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Contract_Case_Assoc_F_Guard_For_Contract_Case_Assoc'Access,
         2 =>
           Contract_Case_Assoc_F_Consequence_For_Contract_Case_Assoc'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Contract_Case_Assoc);

   Pragma_Argument_Assoc_F_Id_For_Pragma_Argument_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Pragma_Argument_Assoc_F_Id
,
      Index => 1);
   Pragma_Argument_Assoc_F_Expr_For_Pragma_Argument_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Pragma_Argument_Assoc_F_Expr
,
      Index => 2);

   Desc_For_Pragma_Argument_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Base_Assoc_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PragmaArgumentAssoc"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Pragma_Argument_Assoc_F_Id_For_Pragma_Argument_Assoc'Access,
         2 => Pragma_Argument_Assoc_F_Expr_For_Pragma_Argument_Assoc'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Pragma_Argument_Assoc);

   Desc_For_Base_Formal_Param_Holder : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 5, Fields_Count => 0,
      Properties_Count => 4,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Discriminant_Part_Type_Id,
         2 => Common.Component_List_Type_Id,
         3 => Common.Generic_Formal_Part_Type_Id,
         4 => Common.Base_Subp_Spec_Type_Id,
         5 => Common.Entry_Completion_Formal_Params_Type_Id),

      DSL_Name => To_Unbounded_String ("BaseFormalParamHolder"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties =>
        (1 => Base_Formal_Param_Holder_P_Abstract_Formal_Params,
         2 => Base_Formal_Param_Holder_P_Nb_Min_Params,
         3 => Base_Formal_Param_Holder_P_Nb_Max_Params,
         4 => Base_Formal_Param_Holder_P_Param_Types)
);

   Desc_For_Base_Subp_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 6,

      Base_Type   => Common.Base_Formal_Param_Holder_Type_Id,
      Derivations =>
        (1 => Common.Enum_Subp_Spec_Type_Id, 2 => Common.Subp_Spec_Type_Id,
         3 => Common.Entry_Spec_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseSubpSpec"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1 => Base_Subp_Spec_P_Returns, 2 => Base_Subp_Spec_P_Params,
         3 => Base_Subp_Spec_P_Primitive_Subp_Types,
         4 => Base_Subp_Spec_P_Primitive_Subp_First_Type,
         5 => Base_Subp_Spec_P_Primitive_Subp_Tagged_Type,
         6 => Base_Subp_Spec_P_Return_Type)
);

   Entry_Spec_F_Entry_Name_For_Entry_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Spec_F_Entry_Name
,
      Index => 1);
   Entry_Spec_F_Family_Type_For_Entry_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Spec_F_Family_Type
,
      Index => 2);
   Entry_Spec_F_Entry_Params_For_Entry_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Spec_F_Entry_Params
,
      Index => 3);

   Desc_For_Entry_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Spec_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EntrySpec"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Entry_Spec_F_Entry_Name_For_Entry_Spec'Access,
         2 => Entry_Spec_F_Family_Type_For_Entry_Spec'Access,
         3 => Entry_Spec_F_Entry_Params_For_Entry_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Entry_Spec);

   Desc_For_Enum_Subp_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Spec_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EnumSubpSpec"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Enum_Subp_Spec);

   Subp_Spec_F_Subp_Kind_For_Subp_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Spec_F_Subp_Kind
,
      Index => 1);
   Subp_Spec_F_Subp_Name_For_Subp_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Spec_F_Subp_Name
,
      Index => 2);
   Subp_Spec_F_Subp_Params_For_Subp_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Spec_F_Subp_Params
,
      Index => 3);
   Subp_Spec_F_Subp_Returns_For_Subp_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Spec_F_Subp_Returns
,
      Index => 4);

   Desc_For_Subp_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Spec_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("SubpSpec"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Subp_Spec_F_Subp_Kind_For_Subp_Spec'Access,
         2 => Subp_Spec_F_Subp_Name_For_Subp_Spec'Access,
         3 => Subp_Spec_F_Subp_Params_For_Subp_Spec'Access,
         4 => Subp_Spec_F_Subp_Returns_For_Subp_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Spec);

   Component_List_F_Components_For_Component_List : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_List_F_Components
,
      Index => 1);
   Component_List_F_Variant_Part_For_Component_List : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_List_F_Variant_Part
,
      Index => 2);

   Desc_For_Component_List : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Holder_Type_Id,
      Derivations => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ComponentList"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Component_List_F_Components_For_Component_List'Access,
         2 => Component_List_F_Variant_Part_For_Component_List'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Component_List);

   Desc_For_Discriminant_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Holder_Type_Id,
      Derivations =>
        (1 => Common.Known_Discriminant_Part_Type_Id,
         2 => Common.Unknown_Discriminant_Part_Type_Id),

      DSL_Name => To_Unbounded_String ("DiscriminantPart"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
);

   Known_Discriminant_Part_F_Discr_Specs_For_Known_Discriminant_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Known_Discriminant_Part_F_Discr_Specs
, Index                   => 1);

   Desc_For_Known_Discriminant_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Discriminant_Part_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("KnownDiscriminantPart"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Known_Discriminant_Part_F_Discr_Specs_For_Known_Discriminant_Part'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Known_Discriminant_Part);

   Desc_For_Unknown_Discriminant_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Discriminant_Part_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("UnknownDiscriminantPart"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Unknown_Discriminant_Part);

   Entry_Completion_Formal_Params_F_Params_For_Entry_Completion_Formal_Params : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Entry_Completion_Formal_Params_F_Params
, Index                   => 1);

   Desc_For_Entry_Completion_Formal_Params : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Holder_Type_Id,
      Derivations => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EntryCompletionFormalParams"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Entry_Completion_Formal_Params_F_Params_For_Entry_Completion_Formal_Params'
             Access),

      Properties => (1 .. 0 => <>)
,
      Kind => Ada_Entry_Completion_Formal_Params);

   Generic_Formal_Part_F_Decls_For_Generic_Formal_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Formal_Part_F_Decls
,
      Index => 1);

   Desc_For_Generic_Formal_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Holder_Type_Id,
      Derivations => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericFormalPart"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Generic_Formal_Part_F_Decls_For_Generic_Formal_Part'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Formal_Part);

   Base_Record_Def_F_Components_For_Base_Record_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Record_Def_F_Components
,
      Index => 1);

   Desc_For_Base_Record_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Record_Def_Type_Id, 2 => Common.Null_Record_Def_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseRecordDef"),
Inherited_Fields => 0,
      Fields => (1 => Base_Record_Def_F_Components_For_Base_Record_Def'Access),

      Properties => (1 .. 0 => <>)
);

   Desc_For_Null_Record_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Record_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NullRecordDef"),
Inherited_Fields => 1,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Null_Record_Def);

   Desc_For_Record_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Record_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RecordDef"),
Inherited_Fields => 1,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Record_Def);

   Desc_For_Basic_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Discriminant_Assoc_Type_Id,
         2 => Common.Param_Assoc_Type_Id, 3 => Common.Aggregate_Assoc_Type_Id),

      DSL_Name   => To_Unbounded_String ("BasicAssoc"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Basic_Assoc_P_Get_Params)
);

   Aggregate_Assoc_F_Designators_For_Aggregate_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Aggregate_Assoc_F_Designators
,
      Index => 1);
   Aggregate_Assoc_F_R_Expr_For_Aggregate_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Aggregate_Assoc_F_R_Expr
,
      Index => 2);

   Desc_For_Aggregate_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Assoc_Type_Id,
      Derivations      => (1 => Common.Multi_Dim_Array_Assoc_Type_Id),

      DSL_Name => To_Unbounded_String ("AggregateAssoc"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Aggregate_Assoc_F_Designators_For_Aggregate_Assoc'Access,
         2 => Aggregate_Assoc_F_R_Expr_For_Aggregate_Assoc'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Aggregate_Assoc);

   Desc_For_Multi_Dim_Array_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Aggregate_Assoc_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("MultiDimArrayAssoc"),

      Inherited_Fields => 2, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Multi_Dim_Array_Assoc);

   Discriminant_Assoc_F_Ids_For_Discriminant_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Discriminant_Assoc_F_Ids
,
      Index => 1);
   Discriminant_Assoc_F_Discr_Expr_For_Discriminant_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Discriminant_Assoc_F_Discr_Expr
,
      Index => 2);

   Desc_For_Discriminant_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Assoc_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscriminantAssoc"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Discriminant_Assoc_F_Ids_For_Discriminant_Assoc'Access,
         2 => Discriminant_Assoc_F_Discr_Expr_For_Discriminant_Assoc'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discriminant_Assoc);

   Param_Assoc_F_Designator_For_Param_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Assoc_F_Designator
,
      Index => 1);
   Param_Assoc_F_R_Expr_For_Param_Assoc : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Assoc_F_R_Expr
, Index                   => 2);

   Desc_For_Param_Assoc : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Assoc_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ParamAssoc"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Param_Assoc_F_Designator_For_Param_Assoc'Access,
         2 => Param_Assoc_F_R_Expr_For_Param_Assoc'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Param_Assoc);

   Basic_Decl_F_Aspects_For_Basic_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Basic_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 20, Fields_Count => 1,
      Properties_Count => 31,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1  => Common.Error_Decl_Type_Id, 2 => Common.Body_Node_Type_Id,
         3  => Common.Base_Formal_Param_Decl_Type_Id,
         4  => Common.Base_Type_Decl_Type_Id,
         5  => Common.Basic_Subp_Decl_Type_Id,
         6  => Common.Single_Task_Decl_Type_Id,
         7  => Common.Single_Protected_Decl_Type_Id,
         8  => Common.Number_Decl_Type_Id, 9 => Common.Object_Decl_Type_Id,
         10 => Common.Base_Package_Decl_Type_Id,
         11 => Common.Exception_Decl_Type_Id,
         12 => Common.Generic_Instantiation_Type_Id,
         13 => Common.Package_Renaming_Decl_Type_Id,
         14 => Common.Generic_Renaming_Decl_Type_Id,
         15 => Common.Generic_Decl_Type_Id,
         16 => Common.For_Loop_Var_Decl_Type_Id,
         17 => Common.Exception_Handler_Type_Id,
         18 => Common.Label_Decl_Type_Id, 19 => Common.Named_Stmt_Decl_Type_Id,
         20 => Common.Entry_Index_Spec_Type_Id),

      DSL_Name   => To_Unbounded_String ("BasicDecl"),
Inherited_Fields => 0,
      Fields     => (1 => Basic_Decl_F_Aspects_For_Basic_Decl'Access),

      Properties =>
        (1  => Basic_Decl_P_Is_Formal, 2 => Basic_Decl_P_Doc_Annotations,
         3  => Basic_Decl_P_Doc, 4 => Basic_Decl_P_Previous_Part_For_Decl,
         5  => Basic_Decl_P_Canonical_Part, 6 => Basic_Decl_P_Is_Static_Decl,
         7  => Basic_Decl_P_Is_Imported, 8 => Basic_Decl_P_Get_Aspect_Assoc,
         9 => Basic_Decl_P_Get_Aspect_Spec_Expr, 10 => Basic_Decl_P_Get_Aspect,
         11 => Basic_Decl_P_Has_Aspect, 12 => Basic_Decl_P_Get_Pragma,
         13 => Basic_Decl_P_Get_Representation_Clause,
         14 => Basic_Decl_P_Is_Compilation_Unit_Root,
         15 => Basic_Decl_P_Is_Visible,
         16 => Basic_Decl_P_Base_Subp_Declarations,
         17 => Basic_Decl_P_Root_Subp_Declarations,
         18 => Basic_Decl_P_Find_All_Overrides,
         19 => Basic_Decl_P_Defining_Names, 20 => Basic_Decl_P_Defining_Name,
         21 => Basic_Decl_P_Type_Expression,
         22 => Basic_Decl_P_Subp_Spec_Or_Null,
         23 => Basic_Decl_P_Is_Subprogram, 24 => Basic_Decl_P_Relative_Name,
         25 => Basic_Decl_P_Relative_Name_Text,
         26 => Basic_Decl_P_Next_Part_For_Decl,
         27 => Basic_Decl_P_Body_Part_For_Decl,
         28 => Basic_Decl_P_Fully_Qualified_Name_Array,
         29 => Basic_Decl_P_Fully_Qualified_Name,
         30 => Basic_Decl_P_Canonical_Fully_Qualified_Name,
         31 => Basic_Decl_P_Unique_Identifying_Name)
);

   Desc_For_Base_Formal_Param_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Discriminant_Spec_Type_Id,
         2 => Common.Component_Decl_Type_Id, 3 => Common.Param_Spec_Type_Id,
         4 => Common.Generic_Formal_Type_Id),

      DSL_Name => To_Unbounded_String ("BaseFormalParamDecl"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 => Base_Formal_Param_Decl_P_Formal_Type)
);

   Component_Decl_F_Ids_For_Component_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Decl_F_Ids
, Index                   => 1);
   Component_Decl_F_Component_Def_For_Component_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Decl_F_Component_Def
,
      Index => 2);
   Component_Decl_F_Default_Expr_For_Component_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Decl_F_Default_Expr
,
      Index => 3);
   Component_Decl_F_Aspects_For_Component_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Component_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Decl_Type_Id,
      Derivations => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ComponentDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Component_Decl_F_Ids_For_Component_Decl'Access,
         2 => Component_Decl_F_Component_Def_For_Component_Decl'Access,
         3 => Component_Decl_F_Default_Expr_For_Component_Decl'Access,
         4 => Component_Decl_F_Aspects_For_Component_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Component_Decl);

   Discriminant_Spec_F_Ids_For_Discriminant_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Discriminant_Spec_F_Ids
,
      Index => 1);
   Discriminant_Spec_F_Type_Expr_For_Discriminant_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Discriminant_Spec_F_Type_Expr
,
      Index => 2);
   Discriminant_Spec_F_Default_Expr_For_Discriminant_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Discriminant_Spec_F_Default_Expr
,
      Index => 3);
   Discriminant_Spec_F_Aspects_For_Discriminant_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Discriminant_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Decl_Type_Id,
      Derivations => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscriminantSpec"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Discriminant_Spec_F_Ids_For_Discriminant_Spec'Access,
         2 => Discriminant_Spec_F_Type_Expr_For_Discriminant_Spec'Access,
         3 => Discriminant_Spec_F_Default_Expr_For_Discriminant_Spec'Access,
         4 => Discriminant_Spec_F_Aspects_For_Discriminant_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discriminant_Spec);

   Generic_Formal_F_Decl_For_Generic_Formal : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Formal_F_Decl
,
      Index => 1);
   Generic_Formal_F_Aspects_For_Generic_Formal : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Generic_Formal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 2,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Decl_Type_Id,
      Derivations =>
        (1 => Common.Generic_Formal_Obj_Decl_Type_Id,
         2 => Common.Generic_Formal_Type_Decl_Type_Id,
         3 => Common.Generic_Formal_Subp_Decl_Type_Id,
         4 => Common.Generic_Formal_Package_Type_Id),

      DSL_Name   => To_Unbounded_String ("GenericFormal"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Generic_Formal_F_Decl_For_Generic_Formal'Access,
         2 => Generic_Formal_F_Aspects_For_Generic_Formal'Access),

      Properties => (1 .. 0 => <>)
);

   Desc_For_Generic_Formal_Obj_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Generic_Formal_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericFormalObjDecl"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Formal_Obj_Decl);

   Desc_For_Generic_Formal_Package : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Generic_Formal_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericFormalPackage"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Formal_Package);

   Desc_For_Generic_Formal_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Generic_Formal_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericFormalSubpDecl"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Formal_Subp_Decl);

   Desc_For_Generic_Formal_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Generic_Formal_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericFormalTypeDecl"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Formal_Type_Decl);

   Param_Spec_F_Ids_For_Param_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Spec_F_Ids
, Index                   => 1);
   Param_Spec_F_Has_Aliased_For_Param_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Spec_F_Has_Aliased
,
      Index => 2);
   Param_Spec_F_Mode_For_Param_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Spec_F_Mode
, Index                   => 3);
   Param_Spec_F_Type_Expr_For_Param_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Spec_F_Type_Expr
,
      Index => 4);
   Param_Spec_F_Default_Expr_For_Param_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Param_Spec_F_Default_Expr
,
      Index => 5);
   Param_Spec_F_Aspects_For_Param_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Param_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 6,
      Properties_Count => 0,

      Base_Type   => Common.Base_Formal_Param_Decl_Type_Id,
      Derivations => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ParamSpec"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Param_Spec_F_Ids_For_Param_Spec'Access,
         2 => Param_Spec_F_Has_Aliased_For_Param_Spec'Access,
         3 => Param_Spec_F_Mode_For_Param_Spec'Access,
         4 => Param_Spec_F_Type_Expr_For_Param_Spec'Access,
         5 => Param_Spec_F_Default_Expr_For_Param_Spec'Access,
         6 => Param_Spec_F_Aspects_For_Param_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Param_Spec);

   Base_Package_Decl_F_Package_Name_For_Base_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Package_Decl_F_Package_Name
,
      Index => 1);
   Base_Package_Decl_F_Aspects_For_Base_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);
   Base_Package_Decl_F_Public_Part_For_Base_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Package_Decl_F_Public_Part
,
      Index => 3);
   Base_Package_Decl_F_Private_Part_For_Base_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Package_Decl_F_Private_Part
,
      Index => 4);
   Base_Package_Decl_F_End_Name_For_Base_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Package_Decl_F_End_Name
,
      Index => 5);

   Desc_For_Base_Package_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 5,
      Properties_Count => 1,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Package_Decl_Type_Id,
         2 => Common.Generic_Package_Internal_Type_Id),

      DSL_Name => To_Unbounded_String ("BasePackageDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Base_Package_Decl_F_Package_Name_For_Base_Package_Decl'Access,
         2 => Base_Package_Decl_F_Aspects_For_Base_Package_Decl'Access,
         3 => Base_Package_Decl_F_Public_Part_For_Base_Package_Decl'Access,
         4 => Base_Package_Decl_F_Private_Part_For_Base_Package_Decl'Access,
         5 => Base_Package_Decl_F_End_Name_For_Base_Package_Decl'Access),

      Properties => (1 => Base_Package_Decl_P_Body_Part)
);

   Desc_For_Generic_Package_Internal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Package_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericPackageInternal"),

      Inherited_Fields => 5, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Package_Internal);

   Desc_For_Package_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Package_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("PackageDecl"),
Inherited_Fields => 5,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Package_Decl);

   Base_Type_Decl_F_Name_For_Base_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Type_Decl_F_Name
,
      Index => 1);

   Desc_For_Base_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 6, Fields_Count => 1,
      Properties_Count => 33,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Classwide_Type_Decl_Type_Id,
         2 => Common.Type_Decl_Type_Id, 3 => Common.Base_Subtype_Decl_Type_Id,
         4 => Common.Task_Type_Decl_Type_Id,
         5 => Common.Protected_Type_Decl_Type_Id,
         6 => Common.Incomplete_Type_Decl_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseTypeDecl"),
Inherited_Fields => 0,
      Fields     => (1 => Base_Type_Decl_F_Name_For_Base_Type_Decl'Access),

      Properties =>
        (1  => Base_Type_Decl_P_Base_Subtype,
         2  => Base_Type_Decl_P_Private_Completion,
         3  => Base_Type_Decl_P_Get_Record_Representation_Clause,
         4  => Base_Type_Decl_P_Get_Enum_Representation_Clause,
         5  => Base_Type_Decl_P_Is_Record_Type,
         6  => Base_Type_Decl_P_Is_Array_Type,
         7  => Base_Type_Decl_P_Find_Derived_Types,
         8  => Base_Type_Decl_P_Is_Real_Type,
         9  => Base_Type_Decl_P_Is_Float_Type,
         10 => Base_Type_Decl_P_Is_Fixed_Point,
         11 => Base_Type_Decl_P_Is_Enum_Type,
         12 => Base_Type_Decl_P_Is_Access_Type,
         13 => Base_Type_Decl_P_Is_Char_Type,
         14 => Base_Type_Decl_P_Discrete_Range,
         15 => Base_Type_Decl_P_Is_Discrete_Type,
         16 => Base_Type_Decl_P_Is_Int_Type,
         17 => Base_Type_Decl_P_Accessed_Type,
         18 => Base_Type_Decl_P_Is_Tagged_Type,
         19 => Base_Type_Decl_P_Base_Type, 20 => Base_Type_Decl_P_Base_Types,
         21 => Base_Type_Decl_P_Find_All_Derived_Types,
         22 => Base_Type_Decl_P_Comp_Type, 23 => Base_Type_Decl_P_Index_Type,
         24 => Base_Type_Decl_P_Is_Derived_Type,
         25 => Base_Type_Decl_P_Is_Interface_Type,
         26 => Base_Type_Decl_P_Matching_Type,
         27 => Base_Type_Decl_P_Canonical_Type,
         28 => Base_Type_Decl_P_Previous_Part,
         29 => Base_Type_Decl_P_Next_Part, 30 => Base_Type_Decl_P_Full_View,
         31 => Base_Type_Decl_P_Is_Definite_Subtype,
         32 => Base_Type_Decl_P_Is_Private, 33 => Base_Type_Decl_P_Root_Type)
);

   Desc_For_Base_Subtype_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Subtype_Decl_Type_Id,
         2 => Common.Discrete_Base_Subtype_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("BaseSubtypeDecl"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
);

   Discrete_Base_Subtype_Decl_F_Aspects_For_Discrete_Base_Subtype_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Discrete_Base_Subtype_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Base_Subtype_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscreteBaseSubtypeDecl"),

      Inherited_Fields => 1,
      Fields           =>
        (1 =>
           Discrete_Base_Subtype_Decl_F_Aspects_For_Discrete_Base_Subtype_Decl'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discrete_Base_Subtype_Decl);

   Subtype_Decl_F_Subtype_For_Subtype_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subtype_Decl_F_Subtype
,
      Index => 2);
   Subtype_Decl_F_Aspects_For_Subtype_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Subtype_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Base_Subtype_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("SubtypeDecl"),
Inherited_Fields => 1,
      Fields     =>
        (1 => Subtype_Decl_F_Subtype_For_Subtype_Decl'Access,
         2 => Subtype_Decl_F_Aspects_For_Subtype_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subtype_Decl);

   Classwide_Type_Decl_F_Aspects_For_Classwide_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Classwide_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ClasswideTypeDecl"),

      Inherited_Fields => 1,
      Fields           =>
        (1 => Classwide_Type_Decl_F_Aspects_For_Classwide_Type_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Classwide_Type_Decl);

   Incomplete_Type_Decl_F_Discriminants_For_Incomplete_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Incomplete_Type_Decl_F_Discriminants
, Index                   => 2);
   Incomplete_Type_Decl_F_Aspects_For_Incomplete_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Incomplete_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Decl_Type_Id,
      Derivations      => (1 => Common.Incomplete_Tagged_Type_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("IncompleteTypeDecl"),

      Inherited_Fields => 1,
      Fields           =>
        (1 =>
           Incomplete_Type_Decl_F_Discriminants_For_Incomplete_Type_Decl'
             Access,
         2 => Incomplete_Type_Decl_F_Aspects_For_Incomplete_Type_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Incomplete_Type_Decl);

   Incomplete_Tagged_Type_Decl_F_Has_Abstract_For_Incomplete_Tagged_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Incomplete_Tagged_Type_Decl_F_Has_Abstract
, Index                   => 3);

   Desc_For_Incomplete_Tagged_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Incomplete_Type_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("IncompleteTaggedTypeDecl"),

      Inherited_Fields => 2,
      Fields           =>
        (1 =>
           Incomplete_Tagged_Type_Decl_F_Has_Abstract_For_Incomplete_Tagged_Type_Decl'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Incomplete_Tagged_Type_Decl);

   Protected_Type_Decl_F_Discriminants_For_Protected_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Protected_Type_Decl_F_Discriminants
, Index                   => 2);
   Protected_Type_Decl_F_Aspects_For_Protected_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);
   Protected_Type_Decl_F_Interfaces_For_Protected_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Type_Decl_F_Interfaces
,
      Index => 4);
   Protected_Type_Decl_F_Definition_For_Protected_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Type_Decl_F_Definition
,
      Index => 5);

   Desc_For_Protected_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProtectedTypeDecl"),

      Inherited_Fields => 1,
      Fields           =>
        (1 =>
           Protected_Type_Decl_F_Discriminants_For_Protected_Type_Decl'Access,
         2 => Protected_Type_Decl_F_Aspects_For_Protected_Type_Decl'Access,
         3 => Protected_Type_Decl_F_Interfaces_For_Protected_Type_Decl'Access,
         4 => Protected_Type_Decl_F_Definition_For_Protected_Type_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Protected_Type_Decl);

   Task_Type_Decl_F_Discriminants_For_Task_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Type_Decl_F_Discriminants
,
      Index => 2);
   Task_Type_Decl_F_Aspects_For_Task_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);
   Task_Type_Decl_F_Definition_For_Task_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Type_Decl_F_Definition
,
      Index => 4);

   Desc_For_Task_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Decl_Type_Id,
      Derivations      => (1 => Common.Single_Task_Type_Decl_Type_Id),

      DSL_Name   => To_Unbounded_String ("TaskTypeDecl"),
Inherited_Fields => 1,
      Fields     =>
        (1 => Task_Type_Decl_F_Discriminants_For_Task_Type_Decl'Access,
         2 => Task_Type_Decl_F_Aspects_For_Task_Type_Decl'Access,
         3 => Task_Type_Decl_F_Definition_For_Task_Type_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Task_Type_Decl);

   Desc_For_Single_Task_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Task_Type_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SingleTaskTypeDecl"),

      Inherited_Fields => 4, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Single_Task_Type_Decl);

   Type_Decl_F_Discriminants_For_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Type_Decl_F_Discriminants
,
      Index => 2);
   Type_Decl_F_Type_Def_For_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Type_Decl_F_Type_Def
, Index                   => 3);
   Type_Decl_F_Aspects_For_Type_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 3,
      Properties_Count => 1,
Base_Type              => Common.Base_Type_Decl_Type_Id,
      Derivations      => (1 => Common.Anonymous_Type_Decl_Type_Id),

      DSL_Name   => To_Unbounded_String ("TypeDecl"),
Inherited_Fields => 1,
      Fields     =>
        (1 => Type_Decl_F_Discriminants_For_Type_Decl'Access,
         2 => Type_Decl_F_Type_Def_For_Type_Decl'Access,
         3 => Type_Decl_F_Aspects_For_Type_Decl'Access),

      Properties => (1 => Type_Decl_P_Get_Primitives)
, Kind           => Ada_Type_Decl);

   Desc_For_Anonymous_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Type_Decl_Type_Id,
      Derivations      => (1 => Common.Synth_Anonymous_Type_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("AnonymousTypeDecl"),

      Inherited_Fields => 4, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Anonymous_Type_Decl);

   Desc_For_Synth_Anonymous_Type_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Anonymous_Type_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SynthAnonymousTypeDecl"),

      Inherited_Fields => 4, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Synth_Anonymous_Type_Decl);

   Desc_For_Basic_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 2,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Classic_Subp_Decl_Type_Id,
         2 => Common.Generic_Subp_Internal_Type_Id,
         3 => Common.Enum_Literal_Decl_Type_Id,
         4 => Common.Entry_Decl_Type_Id),

      DSL_Name   => To_Unbounded_String ("BasicSubpDecl"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1 => Basic_Subp_Decl_P_Subp_Decl_Spec,
         2 => Basic_Subp_Decl_P_Body_Part)
);

   Classic_Subp_Decl_F_Overriding_For_Classic_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Classic_Subp_Decl_F_Overriding
,
      Index => 1);
   Classic_Subp_Decl_F_Subp_Spec_For_Classic_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Classic_Subp_Decl_F_Subp_Spec
,
      Index => 2);

   Desc_For_Classic_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Subp_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Subp_Decl_Type_Id, 2 => Common.Abstract_Subp_Decl_Type_Id,
         3 => Common.Formal_Subp_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("ClassicSubpDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Classic_Subp_Decl_F_Overriding_For_Classic_Subp_Decl'Access,
         2 => Classic_Subp_Decl_F_Subp_Spec_For_Classic_Subp_Decl'Access),

      Properties => (1 .. 0 => <>)
);

   Abstract_Subp_Decl_F_Aspects_For_Abstract_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Abstract_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Classic_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AbstractSubpDecl"),

      Inherited_Fields => 2,
      Fields           =>
        (1 => Abstract_Subp_Decl_F_Aspects_For_Abstract_Subp_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Abstract_Subp_Decl);

   Formal_Subp_Decl_F_Default_Expr_For_Formal_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Formal_Subp_Decl_F_Default_Expr
,
      Index => 3);
   Formal_Subp_Decl_F_Aspects_For_Formal_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Formal_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Classic_Subp_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Concrete_Formal_Subp_Decl_Type_Id,
         2 => Common.Abstract_Formal_Subp_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("FormalSubpDecl"),

      Inherited_Fields => 2,
      Fields           =>
        (1 => Formal_Subp_Decl_F_Default_Expr_For_Formal_Subp_Decl'Access,
         2 => Formal_Subp_Decl_F_Aspects_For_Formal_Subp_Decl'Access),

      Properties => (1 .. 0 => <>)
);

   Desc_For_Abstract_Formal_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Formal_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AbstractFormalSubpDecl"),

      Inherited_Fields => 4, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Abstract_Formal_Subp_Decl);

   Desc_For_Concrete_Formal_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Formal_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ConcreteFormalSubpDecl"),

      Inherited_Fields => 4, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Concrete_Formal_Subp_Decl);

   Subp_Decl_F_Aspects_For_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Classic_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("SubpDecl"),
Inherited_Fields => 2,
      Fields     => (1 => Subp_Decl_F_Aspects_For_Subp_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Decl);

   Entry_Decl_F_Overriding_For_Entry_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Decl_F_Overriding
,
      Index => 1);
   Entry_Decl_F_Spec_For_Entry_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Decl_F_Spec
, Index                   => 2);
   Entry_Decl_F_Aspects_For_Entry_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Entry_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Basic_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EntryDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Entry_Decl_F_Overriding_For_Entry_Decl'Access,
         2 => Entry_Decl_F_Spec_For_Entry_Decl'Access,
         3 => Entry_Decl_F_Aspects_For_Entry_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Entry_Decl);

   Enum_Literal_Decl_F_Name_For_Enum_Literal_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Enum_Literal_Decl_F_Name
,
      Index => 1);
   Enum_Literal_Decl_F_Aspects_For_Enum_Literal_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Enum_Literal_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Basic_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EnumLiteralDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Enum_Literal_Decl_F_Name_For_Enum_Literal_Decl'Access,
         2 => Enum_Literal_Decl_F_Aspects_For_Enum_Literal_Decl'Access),

      Properties => (1 => Enum_Literal_Decl_P_Enum_Type)
,
      Kind => Ada_Enum_Literal_Decl);

   Generic_Subp_Internal_F_Subp_Spec_For_Generic_Subp_Internal : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Subp_Internal_F_Subp_Spec
,
      Index => 1);
   Generic_Subp_Internal_F_Aspects_For_Generic_Subp_Internal : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);

   Desc_For_Generic_Subp_Internal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Subp_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericSubpInternal"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Generic_Subp_Internal_F_Subp_Spec_For_Generic_Subp_Internal'Access,
         2 =>
           Generic_Subp_Internal_F_Aspects_For_Generic_Subp_Internal'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Subp_Internal);

   Desc_For_Body_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 6, Fields_Count => 0,
      Properties_Count => 3,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Body_Stub_Type_Id, 2 => Common.Base_Subp_Body_Type_Id,
         3 => Common.Package_Body_Type_Id, 4 => Common.Task_Body_Type_Id,
         5 => Common.Protected_Body_Type_Id, 6 => Common.Entry_Body_Type_Id),

      DSL_Name   => To_Unbounded_String ("Body"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1 => Body_Node_P_Previous_Part, 2 => Body_Node_P_Decl_Part,
         3 => Body_Node_P_Subunit_Root)
);

   Base_Subp_Body_F_Overriding_For_Base_Subp_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Subp_Body_F_Overriding
,
      Index => 1);
   Base_Subp_Body_F_Subp_Spec_For_Base_Subp_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Subp_Body_F_Subp_Spec
,
      Index => 2);

   Desc_For_Base_Subp_Body : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Body_Node_Type_Id,
      Derivations      =>
        (1 => Common.Expr_Function_Type_Id, 2 => Common.Null_Subp_Decl_Type_Id,
         3 => Common.Subp_Renaming_Decl_Type_Id,
         4 => Common.Subp_Body_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseSubpBody"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Base_Subp_Body_F_Overriding_For_Base_Subp_Body'Access,
         2 => Base_Subp_Body_F_Subp_Spec_For_Base_Subp_Body'Access),

      Properties => (1 .. 0 => <>)
);

   Expr_Function_F_Expr_For_Expr_Function : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Expr_Function_F_Expr
, Index                   => 3);
   Expr_Function_F_Aspects_For_Expr_Function : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Expr_Function : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Body_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ExprFunction"),
Inherited_Fields => 2,
      Fields     =>
        (1 => Expr_Function_F_Expr_For_Expr_Function'Access,
         2 => Expr_Function_F_Aspects_For_Expr_Function'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Expr_Function);

   Null_Subp_Decl_F_Aspects_For_Null_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Null_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Body_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NullSubpDecl"),
Inherited_Fields => 2,
      Fields     => (1 => Null_Subp_Decl_F_Aspects_For_Null_Subp_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Null_Subp_Decl);

   Subp_Body_F_Aspects_For_Subp_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);
   Subp_Body_F_Decls_For_Subp_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Body_F_Decls
, Index                   => 4);
   Subp_Body_F_Stmts_For_Subp_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Body_F_Stmts
, Index                   => 5);
   Subp_Body_F_End_Name_For_Subp_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Body_F_End_Name
, Index                   => 6);

   Desc_For_Subp_Body : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Body_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("SubpBody"),
Inherited_Fields => 2,
      Fields     =>
        (1 => Subp_Body_F_Aspects_For_Subp_Body'Access,
         2 => Subp_Body_F_Decls_For_Subp_Body'Access,
         3 => Subp_Body_F_Stmts_For_Subp_Body'Access,
         4 => Subp_Body_F_End_Name_For_Subp_Body'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Body);

   Subp_Renaming_Decl_F_Renames_For_Subp_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Renaming_Decl_F_Renames
,
      Index => 3);
   Subp_Renaming_Decl_F_Aspects_For_Subp_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Subp_Renaming_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Base_Subp_Body_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SubpRenamingDecl"),

      Inherited_Fields => 2,
      Fields           =>
        (1 => Subp_Renaming_Decl_F_Renames_For_Subp_Renaming_Decl'Access,
         2 => Subp_Renaming_Decl_F_Aspects_For_Subp_Renaming_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Renaming_Decl);

   Desc_For_Body_Stub : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Body_Node_Type_Id,
      Derivations      =>
        (1 => Common.Protected_Body_Stub_Type_Id,
         2 => Common.Subp_Body_Stub_Type_Id,
         3 => Common.Package_Body_Stub_Type_Id,
         4 => Common.Task_Body_Stub_Type_Id),

      DSL_Name   => To_Unbounded_String ("BodyStub"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Package_Body_Stub_F_Name_For_Package_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Body_Stub_F_Name
,
      Index => 1);
   Package_Body_Stub_F_Aspects_For_Package_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);

   Desc_For_Package_Body_Stub : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Body_Stub_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PackageBodyStub"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Package_Body_Stub_F_Name_For_Package_Body_Stub'Access,
         2 => Package_Body_Stub_F_Aspects_For_Package_Body_Stub'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Package_Body_Stub);

   Protected_Body_Stub_F_Name_For_Protected_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Body_Stub_F_Name
,
      Index => 1);
   Protected_Body_Stub_F_Aspects_For_Protected_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);

   Desc_For_Protected_Body_Stub : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Body_Stub_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ProtectedBodyStub"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Protected_Body_Stub_F_Name_For_Protected_Body_Stub'Access,
         2 => Protected_Body_Stub_F_Aspects_For_Protected_Body_Stub'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Protected_Body_Stub);

   Subp_Body_Stub_F_Overriding_For_Subp_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Body_Stub_F_Overriding
,
      Index => 1);
   Subp_Body_Stub_F_Subp_Spec_For_Subp_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subp_Body_Stub_F_Subp_Spec
,
      Index => 2);
   Subp_Body_Stub_F_Aspects_For_Subp_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Subp_Body_Stub : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Body_Stub_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("SubpBodyStub"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Subp_Body_Stub_F_Overriding_For_Subp_Body_Stub'Access,
         2 => Subp_Body_Stub_F_Subp_Spec_For_Subp_Body_Stub'Access,
         3 => Subp_Body_Stub_F_Aspects_For_Subp_Body_Stub'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Body_Stub);

   Task_Body_Stub_F_Name_For_Task_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Body_Stub_F_Name
,
      Index => 1);
   Task_Body_Stub_F_Aspects_For_Task_Body_Stub : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);

   Desc_For_Task_Body_Stub : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Body_Stub_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("TaskBodyStub"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Task_Body_Stub_F_Name_For_Task_Body_Stub'Access,
         2 => Task_Body_Stub_F_Aspects_For_Task_Body_Stub'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Task_Body_Stub);

   Entry_Body_F_Entry_Name_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_Entry_Name
,
      Index => 1);
   Entry_Body_F_Index_Spec_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_Index_Spec
,
      Index => 2);
   Entry_Body_F_Params_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_Params
, Index                   => 3);
   Entry_Body_F_Barrier_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_Barrier
, Index                   => 4);
   Entry_Body_F_Decls_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_Decls
, Index                   => 5);
   Entry_Body_F_Stmts_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_Stmts
, Index                   => 6);
   Entry_Body_F_End_Name_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Body_F_End_Name
,
      Index => 7);
   Entry_Body_F_Aspects_For_Entry_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Entry_Body : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 8,
      Properties_Count => 0,
Base_Type              => Common.Body_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EntryBody"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Entry_Body_F_Entry_Name_For_Entry_Body'Access,
         2 => Entry_Body_F_Index_Spec_For_Entry_Body'Access,
         3 => Entry_Body_F_Params_For_Entry_Body'Access,
         4 => Entry_Body_F_Barrier_For_Entry_Body'Access,
         5 => Entry_Body_F_Decls_For_Entry_Body'Access,
         6 => Entry_Body_F_Stmts_For_Entry_Body'Access,
         7 => Entry_Body_F_End_Name_For_Entry_Body'Access,
         8 => Entry_Body_F_Aspects_For_Entry_Body'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Entry_Body);

   Package_Body_F_Package_Name_For_Package_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Body_F_Package_Name
,
      Index => 1);
   Package_Body_F_Aspects_For_Package_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);
   Package_Body_F_Decls_For_Package_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Body_F_Decls
, Index                   => 3);
   Package_Body_F_Stmts_For_Package_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Body_F_Stmts
, Index                   => 4);
   Package_Body_F_End_Name_For_Package_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Body_F_End_Name
,
      Index => 5);

   Desc_For_Package_Body : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 5,
      Properties_Count => 0,
Base_Type              => Common.Body_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("PackageBody"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Package_Body_F_Package_Name_For_Package_Body'Access,
         2 => Package_Body_F_Aspects_For_Package_Body'Access,
         3 => Package_Body_F_Decls_For_Package_Body'Access,
         4 => Package_Body_F_Stmts_For_Package_Body'Access,
         5 => Package_Body_F_End_Name_For_Package_Body'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Package_Body);

   Protected_Body_F_Name_For_Protected_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Body_F_Name
,
      Index => 1);
   Protected_Body_F_Aspects_For_Protected_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);
   Protected_Body_F_Decls_For_Protected_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Body_F_Decls
,
      Index => 3);
   Protected_Body_F_End_Name_For_Protected_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Body_F_End_Name
,
      Index => 4);

   Desc_For_Protected_Body : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Body_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ProtectedBody"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Protected_Body_F_Name_For_Protected_Body'Access,
         2 => Protected_Body_F_Aspects_For_Protected_Body'Access,
         3 => Protected_Body_F_Decls_For_Protected_Body'Access,
         4 => Protected_Body_F_End_Name_For_Protected_Body'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Protected_Body);

   Task_Body_F_Name_For_Task_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Body_F_Name
, Index                   => 1);
   Task_Body_F_Aspects_For_Task_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);
   Task_Body_F_Decls_For_Task_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Body_F_Decls
, Index                   => 3);
   Task_Body_F_Stmts_For_Task_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Body_F_Stmts
, Index                   => 4);
   Task_Body_F_End_Name_For_Task_Body : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Body_F_End_Name
, Index                   => 5);

   Desc_For_Task_Body : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 5,
      Properties_Count => 0,
Base_Type              => Common.Body_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("TaskBody"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Task_Body_F_Name_For_Task_Body'Access,
         2 => Task_Body_F_Aspects_For_Task_Body'Access,
         3 => Task_Body_F_Decls_For_Task_Body'Access,
         4 => Task_Body_F_Stmts_For_Task_Body'Access,
         5 => Task_Body_F_End_Name_For_Task_Body'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Task_Body);

   Entry_Index_Spec_F_Id_For_Entry_Index_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Index_Spec_F_Id
,
      Index => 1);
   Entry_Index_Spec_F_Subtype_For_Entry_Index_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Entry_Index_Spec_F_Subtype
,
      Index => 2);
   Entry_Index_Spec_F_Aspects_For_Entry_Index_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Entry_Index_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EntryIndexSpec"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Entry_Index_Spec_F_Id_For_Entry_Index_Spec'Access,
         2 => Entry_Index_Spec_F_Subtype_For_Entry_Index_Spec'Access,
         3 => Entry_Index_Spec_F_Aspects_For_Entry_Index_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Entry_Index_Spec);

   Error_Decl_F_Aspects_For_Error_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Error_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ErrorDecl"),
Inherited_Fields => 0,
      Fields     => (1 => Error_Decl_F_Aspects_For_Error_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Error_Decl);

   Exception_Decl_F_Ids_For_Exception_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Exception_Decl_F_Ids
, Index                   => 1);
   Exception_Decl_F_Renames_For_Exception_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Exception_Decl_F_Renames
,
      Index => 2);
   Exception_Decl_F_Aspects_For_Exception_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Exception_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ExceptionDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Exception_Decl_F_Ids_For_Exception_Decl'Access,
         2 => Exception_Decl_F_Renames_For_Exception_Decl'Access,
         3 => Exception_Decl_F_Aspects_For_Exception_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Exception_Decl);

   Exception_Handler_F_Exception_Name_For_Exception_Handler : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Exception_Handler_F_Exception_Name
, Index                   => 1);
   Exception_Handler_F_Handled_Exceptions_For_Exception_Handler : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Exception_Handler_F_Handled_Exceptions
, Index                   => 2);
   Exception_Handler_F_Stmts_For_Exception_Handler : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Exception_Handler_F_Stmts
,
      Index => 3);
   Exception_Handler_F_Aspects_For_Exception_Handler : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Exception_Handler : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExceptionHandler"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Exception_Handler_F_Exception_Name_For_Exception_Handler'Access,
         2 =>
           Exception_Handler_F_Handled_Exceptions_For_Exception_Handler'Access,
         3 => Exception_Handler_F_Stmts_For_Exception_Handler'Access,
         4 => Exception_Handler_F_Aspects_For_Exception_Handler'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Exception_Handler);

   For_Loop_Var_Decl_F_Id_For_For_Loop_Var_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => For_Loop_Var_Decl_F_Id
,
      Index => 1);
   For_Loop_Var_Decl_F_Id_Type_For_For_Loop_Var_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => For_Loop_Var_Decl_F_Id_Type
,
      Index => 2);
   For_Loop_Var_Decl_F_Aspects_For_For_Loop_Var_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_For_Loop_Var_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ForLoopVarDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => For_Loop_Var_Decl_F_Id_For_For_Loop_Var_Decl'Access,
         2 => For_Loop_Var_Decl_F_Id_Type_For_For_Loop_Var_Decl'Access,
         3 => For_Loop_Var_Decl_F_Aspects_For_For_Loop_Var_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_For_Loop_Var_Decl);

   Generic_Decl_F_Formal_Part_For_Generic_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Decl_F_Formal_Part
,
      Index => 1);

   Desc_For_Generic_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Generic_Subp_Decl_Type_Id,
         2 => Common.Generic_Package_Decl_Type_Id),

      DSL_Name   => To_Unbounded_String ("GenericDecl"),
Inherited_Fields => 0,
      Fields     => (1 => Generic_Decl_F_Formal_Part_For_Generic_Decl'Access),

      Properties => (1 .. 0 => <>)
);

   Generic_Package_Decl_F_Package_Decl_For_Generic_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Package_Decl_F_Package_Decl
, Index                   => 2);
   Generic_Package_Decl_F_Aspects_For_Generic_Package_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Generic_Package_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Generic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericPackageDecl"),

      Inherited_Fields => 1,
      Fields           =>
        (1 =>
           Generic_Package_Decl_F_Package_Decl_For_Generic_Package_Decl'Access,
         2 => Generic_Package_Decl_F_Aspects_For_Generic_Package_Decl'Access),

      Properties => (1 => Generic_Package_Decl_P_Body_Part)
,
      Kind => Ada_Generic_Package_Decl);

   Generic_Subp_Decl_F_Subp_Decl_For_Generic_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Subp_Decl_F_Subp_Decl
,
      Index => 2);
   Generic_Subp_Decl_F_Aspects_For_Generic_Subp_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Generic_Subp_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Generic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericSubpDecl"),

      Inherited_Fields => 1,
      Fields           =>
        (1 => Generic_Subp_Decl_F_Subp_Decl_For_Generic_Subp_Decl'Access,
         2 => Generic_Subp_Decl_F_Aspects_For_Generic_Subp_Decl'Access),

      Properties => (1 => Generic_Subp_Decl_P_Body_Part)
,
      Kind => Ada_Generic_Subp_Decl);

   Desc_For_Generic_Instantiation : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Generic_Subp_Instantiation_Type_Id,
         2 => Common.Generic_Package_Instantiation_Type_Id),

      DSL_Name => To_Unbounded_String ("GenericInstantiation"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 => Generic_Instantiation_P_Designated_Generic_Decl)
);

   Generic_Package_Instantiation_F_Name_For_Generic_Package_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Package_Instantiation_F_Name
, Index                   => 1);
   Generic_Package_Instantiation_F_Generic_Pkg_Name_For_Generic_Package_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Package_Instantiation_F_Generic_Pkg_Name
, Index                   => 2);
   Generic_Package_Instantiation_F_Params_For_Generic_Package_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Package_Instantiation_F_Params
, Index                   => 3);
   Generic_Package_Instantiation_F_Aspects_For_Generic_Package_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Generic_Package_Instantiation : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Generic_Instantiation_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericPackageInstantiation"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Generic_Package_Instantiation_F_Name_For_Generic_Package_Instantiation'
             Access,
         2 =>
           Generic_Package_Instantiation_F_Generic_Pkg_Name_For_Generic_Package_Instantiation'
             Access,
         3 =>
           Generic_Package_Instantiation_F_Params_For_Generic_Package_Instantiation'
             Access,
         4 =>
           Generic_Package_Instantiation_F_Aspects_For_Generic_Package_Instantiation'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Package_Instantiation);

   Generic_Subp_Instantiation_F_Overriding_For_Generic_Subp_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Subp_Instantiation_F_Overriding
, Index                   => 1);
   Generic_Subp_Instantiation_F_Kind_For_Generic_Subp_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Subp_Instantiation_F_Kind
,
      Index => 2);
   Generic_Subp_Instantiation_F_Subp_Name_For_Generic_Subp_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Subp_Instantiation_F_Subp_Name
, Index                   => 3);
   Generic_Subp_Instantiation_F_Generic_Subp_Name_For_Generic_Subp_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Subp_Instantiation_F_Generic_Subp_Name
, Index                   => 4);
   Generic_Subp_Instantiation_F_Params_For_Generic_Subp_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Subp_Instantiation_F_Params
, Index                   => 5);
   Generic_Subp_Instantiation_F_Aspects_For_Generic_Subp_Instantiation : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 6);

   Desc_For_Generic_Subp_Instantiation : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 6,
      Properties_Count => 1,
Base_Type              => Common.Generic_Instantiation_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericSubpInstantiation"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Generic_Subp_Instantiation_F_Overriding_For_Generic_Subp_Instantiation'
             Access,
         2 =>
           Generic_Subp_Instantiation_F_Kind_For_Generic_Subp_Instantiation'
             Access,
         3 =>
           Generic_Subp_Instantiation_F_Subp_Name_For_Generic_Subp_Instantiation'
             Access,
         4 =>
           Generic_Subp_Instantiation_F_Generic_Subp_Name_For_Generic_Subp_Instantiation'
             Access,
         5 =>
           Generic_Subp_Instantiation_F_Params_For_Generic_Subp_Instantiation'
             Access,
         6 =>
           Generic_Subp_Instantiation_F_Aspects_For_Generic_Subp_Instantiation'
             Access),

      Properties => (1 => Generic_Subp_Instantiation_P_Designated_Subp)
,
      Kind => Ada_Generic_Subp_Instantiation);

   Desc_For_Generic_Renaming_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Generic_Package_Renaming_Decl_Type_Id,
         2 => Common.Generic_Subp_Renaming_Decl_Type_Id),

      DSL_Name => To_Unbounded_String ("GenericRenamingDecl"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
);

   Generic_Package_Renaming_Decl_F_Name_For_Generic_Package_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Package_Renaming_Decl_F_Name
, Index                   => 1);
   Generic_Package_Renaming_Decl_F_Renames_For_Generic_Package_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Package_Renaming_Decl_F_Renames
, Index                   => 2);
   Generic_Package_Renaming_Decl_F_Aspects_For_Generic_Package_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Generic_Package_Renaming_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Generic_Renaming_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericPackageRenamingDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Generic_Package_Renaming_Decl_F_Name_For_Generic_Package_Renaming_Decl'
             Access,
         2 =>
           Generic_Package_Renaming_Decl_F_Renames_For_Generic_Package_Renaming_Decl'
             Access,
         3 =>
           Generic_Package_Renaming_Decl_F_Aspects_For_Generic_Package_Renaming_Decl'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Package_Renaming_Decl);

   Generic_Subp_Renaming_Decl_F_Kind_For_Generic_Subp_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Subp_Renaming_Decl_F_Kind
,
      Index => 1);
   Generic_Subp_Renaming_Decl_F_Name_For_Generic_Subp_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Generic_Subp_Renaming_Decl_F_Name
,
      Index => 2);
   Generic_Subp_Renaming_Decl_F_Renames_For_Generic_Subp_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Generic_Subp_Renaming_Decl_F_Renames
, Index                   => 3);
   Generic_Subp_Renaming_Decl_F_Aspects_For_Generic_Subp_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 4);

   Desc_For_Generic_Subp_Renaming_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Generic_Renaming_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GenericSubpRenamingDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Generic_Subp_Renaming_Decl_F_Kind_For_Generic_Subp_Renaming_Decl'
             Access,
         2 =>
           Generic_Subp_Renaming_Decl_F_Name_For_Generic_Subp_Renaming_Decl'
             Access,
         3 =>
           Generic_Subp_Renaming_Decl_F_Renames_For_Generic_Subp_Renaming_Decl'
             Access,
         4 =>
           Generic_Subp_Renaming_Decl_F_Aspects_For_Generic_Subp_Renaming_Decl'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Generic_Subp_Renaming_Decl);

   Label_Decl_F_Name_For_Label_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Label_Decl_F_Name
, Index                   => 1);
   Label_Decl_F_Aspects_For_Label_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Label_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("LabelDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Label_Decl_F_Name_For_Label_Decl'Access,
         2 => Label_Decl_F_Aspects_For_Label_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Label_Decl);

   Named_Stmt_Decl_F_Name_For_Named_Stmt_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Named_Stmt_Decl_F_Name
,
      Index => 1);
   Named_Stmt_Decl_F_Aspects_For_Named_Stmt_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Named_Stmt_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NamedStmtDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Named_Stmt_Decl_F_Name_For_Named_Stmt_Decl'Access,
         2 => Named_Stmt_Decl_F_Aspects_For_Named_Stmt_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Named_Stmt_Decl);

   Number_Decl_F_Ids_For_Number_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Number_Decl_F_Ids
, Index                   => 1);
   Number_Decl_F_Expr_For_Number_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Number_Decl_F_Expr
, Index                   => 2);
   Number_Decl_F_Aspects_For_Number_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Number_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NumberDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Number_Decl_F_Ids_For_Number_Decl'Access,
         2 => Number_Decl_F_Expr_For_Number_Decl'Access,
         3 => Number_Decl_F_Aspects_For_Number_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Number_Decl);

   Object_Decl_F_Ids_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Ids
, Index                   => 1);
   Object_Decl_F_Has_Aliased_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Has_Aliased
,
      Index => 2);
   Object_Decl_F_Has_Constant_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Has_Constant
,
      Index => 3);
   Object_Decl_F_Mode_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Mode
, Index                   => 4);
   Object_Decl_F_Type_Expr_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Type_Expr
,
      Index => 5);
   Object_Decl_F_Default_Expr_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Default_Expr
,
      Index => 6);
   Object_Decl_F_Renaming_Clause_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Object_Decl_F_Renaming_Clause
,
      Index => 7);
   Object_Decl_F_Aspects_For_Object_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 8);

   Desc_For_Object_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 2, Fields_Count => 8,
      Properties_Count => 1,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      =>
        (1 => Common.Anonymous_Object_Decl_Type_Id,
         2 => Common.Extended_Return_Stmt_Object_Decl_Type_Id),

      DSL_Name   => To_Unbounded_String ("ObjectDecl"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Object_Decl_F_Ids_For_Object_Decl'Access,
         2 => Object_Decl_F_Has_Aliased_For_Object_Decl'Access,
         3 => Object_Decl_F_Has_Constant_For_Object_Decl'Access,
         4 => Object_Decl_F_Mode_For_Object_Decl'Access,
         5 => Object_Decl_F_Type_Expr_For_Object_Decl'Access,
         6 => Object_Decl_F_Default_Expr_For_Object_Decl'Access,
         7 => Object_Decl_F_Renaming_Clause_For_Object_Decl'Access,
         8 => Object_Decl_F_Aspects_For_Object_Decl'Access),

      Properties => (1 => Object_Decl_P_Public_Part_Decl)
,
      Kind => Ada_Object_Decl);

   Desc_For_Anonymous_Object_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Object_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AnonymousObjectDecl"),

      Inherited_Fields => 8, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Anonymous_Object_Decl);

   Desc_For_Extended_Return_Stmt_Object_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Object_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExtendedReturnStmtObjectDecl"),

      Inherited_Fields => 8, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
,
      Kind => Ada_Extended_Return_Stmt_Object_Decl);

   Package_Renaming_Decl_F_Name_For_Package_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Renaming_Decl_F_Name
,
      Index => 1);
   Package_Renaming_Decl_F_Renames_For_Package_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Package_Renaming_Decl_F_Renames
,
      Index => 2);
   Package_Renaming_Decl_F_Aspects_For_Package_Renaming_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 3);

   Desc_For_Package_Renaming_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 2,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PackageRenamingDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Package_Renaming_Decl_F_Name_For_Package_Renaming_Decl'Access,
         2 => Package_Renaming_Decl_F_Renames_For_Package_Renaming_Decl'Access,
         3 =>
           Package_Renaming_Decl_F_Aspects_For_Package_Renaming_Decl'Access),

      Properties =>
        (1 => Package_Renaming_Decl_P_Renamed_Package,
         2 => Package_Renaming_Decl_P_Final_Renamed_Package)
,
      Kind => Ada_Package_Renaming_Decl);

   Single_Protected_Decl_F_Name_For_Single_Protected_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Single_Protected_Decl_F_Name
,
      Index => 1);
   Single_Protected_Decl_F_Aspects_For_Single_Protected_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Basic_Decl_F_Aspects
, Index                   => 2);
   Single_Protected_Decl_F_Interfaces_For_Single_Protected_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Single_Protected_Decl_F_Interfaces
, Index                   => 3);
   Single_Protected_Decl_F_Definition_For_Single_Protected_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Single_Protected_Decl_F_Definition
, Index                   => 4);

   Desc_For_Single_Protected_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SingleProtectedDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Single_Protected_Decl_F_Name_For_Single_Protected_Decl'Access,
         2 => Single_Protected_Decl_F_Aspects_For_Single_Protected_Decl'Access,
         3 =>
           Single_Protected_Decl_F_Interfaces_For_Single_Protected_Decl'Access,
         4 =>
           Single_Protected_Decl_F_Definition_For_Single_Protected_Decl'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Single_Protected_Decl);

   Single_Task_Decl_F_Task_Type_For_Single_Task_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Single_Task_Decl_F_Task_Type
,
      Index => 1);
   Single_Task_Decl_F_Aspects_For_Single_Task_Decl : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => True, Field => Basic_Decl_F_Aspects
);

   Desc_For_Single_Task_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Basic_Decl_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SingleTaskDecl"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Single_Task_Decl_F_Task_Type_For_Single_Task_Decl'Access,
         2 => Single_Task_Decl_F_Aspects_For_Single_Task_Decl'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Single_Task_Decl);

   Case_Stmt_Alternative_F_Choices_For_Case_Stmt_Alternative : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Stmt_Alternative_F_Choices
,
      Index => 1);
   Case_Stmt_Alternative_F_Stmts_For_Case_Stmt_Alternative : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Stmt_Alternative_F_Stmts
,
      Index => 2);

   Desc_For_Case_Stmt_Alternative : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseStmtAlternative"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Case_Stmt_Alternative_F_Choices_For_Case_Stmt_Alternative'Access,
         2 => Case_Stmt_Alternative_F_Stmts_For_Case_Stmt_Alternative'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Case_Stmt_Alternative);

   Compilation_Unit_F_Prelude_For_Compilation_Unit : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Compilation_Unit_F_Prelude
,
      Index => 1);
   Compilation_Unit_F_Body_For_Compilation_Unit : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Compilation_Unit_F_Body
,
      Index => 2);
   Compilation_Unit_F_Pragmas_For_Compilation_Unit : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Compilation_Unit_F_Pragmas
,
      Index => 3);

   Desc_For_Compilation_Unit : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 7,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompilationUnit"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Compilation_Unit_F_Prelude_For_Compilation_Unit'Access,
         2 => Compilation_Unit_F_Body_For_Compilation_Unit'Access,
         3 => Compilation_Unit_F_Pragmas_For_Compilation_Unit'Access),

      Properties =>
        (1 => Compilation_Unit_P_Syntactic_Fully_Qualified_Name,
         2 => Compilation_Unit_P_Unit_Kind,
         3 => Compilation_Unit_P_Withed_Units,
         4 => Compilation_Unit_P_Imported_Units,
         5 => Compilation_Unit_P_Unit_Dependencies,
         6 => Compilation_Unit_P_Decl,
         7 => Compilation_Unit_P_Is_Preelaborable)
,
      Kind => Ada_Compilation_Unit);

   Component_Clause_F_Id_For_Component_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Clause_F_Id
,
      Index => 1);
   Component_Clause_F_Position_For_Component_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Clause_F_Position
,
      Index => 2);
   Component_Clause_F_Range_For_Component_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Clause_F_Range
,
      Index => 3);

   Desc_For_Component_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ComponentClause"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Component_Clause_F_Id_For_Component_Clause'Access,
         2 => Component_Clause_F_Position_For_Component_Clause'Access,
         3 => Component_Clause_F_Range_For_Component_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Component_Clause);

   Component_Def_F_Has_Aliased_For_Component_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Def_F_Has_Aliased
,
      Index => 1);
   Component_Def_F_Has_Constant_For_Component_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Def_F_Has_Constant
,
      Index => 2);
   Component_Def_F_Type_Expr_For_Component_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Component_Def_F_Type_Expr
,
      Index => 3);

   Desc_For_Component_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ComponentDef"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Component_Def_F_Has_Aliased_For_Component_Def'Access,
         2 => Component_Def_F_Has_Constant_For_Component_Def'Access,
         3 => Component_Def_F_Type_Expr_For_Component_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Component_Def);

   Desc_For_Constant_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Constant_Present_Type_Id,
         2 => Common.Constant_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Constant"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Constant_Node_P_As_Bool)
);

   Desc_For_Constant_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Constant_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Constant.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Constant_Absent);

   Desc_For_Constant_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Constant_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Constant.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Constant_Present);

   Desc_For_Constraint : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 5, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Range_Constraint_Type_Id,
         2 => Common.Digits_Constraint_Type_Id,
         3 => Common.Delta_Constraint_Type_Id,
         4 => Common.Index_Constraint_Type_Id,
         5 => Common.Discriminant_Constraint_Type_Id),

      DSL_Name   => To_Unbounded_String ("Constraint"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Delta_Constraint_F_Digits_For_Delta_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Delta_Constraint_F_Digits
,
      Index => 1);
   Delta_Constraint_F_Range_For_Delta_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Delta_Constraint_F_Range
,
      Index => 2);

   Desc_For_Delta_Constraint : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Constraint_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DeltaConstraint"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Delta_Constraint_F_Digits_For_Delta_Constraint'Access,
         2 => Delta_Constraint_F_Range_For_Delta_Constraint'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Delta_Constraint);

   Digits_Constraint_F_Digits_For_Digits_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Digits_Constraint_F_Digits
,
      Index => 1);
   Digits_Constraint_F_Range_For_Digits_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Digits_Constraint_F_Range
,
      Index => 2);

   Desc_For_Digits_Constraint : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Constraint_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DigitsConstraint"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Digits_Constraint_F_Digits_For_Digits_Constraint'Access,
         2 => Digits_Constraint_F_Range_For_Digits_Constraint'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Digits_Constraint);

   Discriminant_Constraint_F_Constraints_For_Discriminant_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Discriminant_Constraint_F_Constraints
, Index                   => 1);

   Desc_For_Discriminant_Constraint : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Constraint_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscriminantConstraint"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Discriminant_Constraint_F_Constraints_For_Discriminant_Constraint'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discriminant_Constraint);

   Index_Constraint_F_Constraints_For_Index_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Index_Constraint_F_Constraints
,
      Index => 1);

   Desc_For_Index_Constraint : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Constraint_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("IndexConstraint"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Index_Constraint_F_Constraints_For_Index_Constraint'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Index_Constraint);

   Range_Constraint_F_Range_For_Range_Constraint : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Range_Constraint_F_Range
,
      Index => 1);

   Desc_For_Range_Constraint : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Constraint_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("RangeConstraint"),

      Inherited_Fields => 0,
      Fields => (1 => Range_Constraint_F_Range_For_Range_Constraint'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Range_Constraint);

   Declarative_Part_F_Decls_For_Declarative_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Declarative_Part_F_Decls
,
      Index => 1);

   Desc_For_Declarative_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 2, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Private_Part_Type_Id, 2 => Common.Public_Part_Type_Id),

      DSL_Name => To_Unbounded_String ("DeclarativePart"),

      Inherited_Fields => 0,
      Fields => (1 => Declarative_Part_F_Decls_For_Declarative_Part'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Declarative_Part);

   Desc_For_Private_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Declarative_Part_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("PrivatePart"),
Inherited_Fields => 1,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Private_Part);

   Desc_For_Public_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Declarative_Part_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("PublicPart"),
Inherited_Fields => 1,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Public_Part);

   Elsif_Expr_Part_F_Cond_Expr_For_Elsif_Expr_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Elsif_Expr_Part_F_Cond_Expr
,
      Index => 1);
   Elsif_Expr_Part_F_Then_Expr_For_Elsif_Expr_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Elsif_Expr_Part_F_Then_Expr
,
      Index => 2);

   Desc_For_Elsif_Expr_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ElsifExprPart"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Elsif_Expr_Part_F_Cond_Expr_For_Elsif_Expr_Part'Access,
         2 => Elsif_Expr_Part_F_Then_Expr_For_Elsif_Expr_Part'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Elsif_Expr_Part);

   Elsif_Stmt_Part_F_Cond_Expr_For_Elsif_Stmt_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Elsif_Stmt_Part_F_Cond_Expr
,
      Index => 1);
   Elsif_Stmt_Part_F_Stmts_For_Elsif_Stmt_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Elsif_Stmt_Part_F_Stmts
,
      Index => 2);

   Desc_For_Elsif_Stmt_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ElsifStmtPart"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Elsif_Stmt_Part_F_Cond_Expr_For_Elsif_Stmt_Part'Access,
         2 => Elsif_Stmt_Part_F_Stmts_For_Elsif_Stmt_Part'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Elsif_Stmt_Part);

   Desc_For_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 14, Fields_Count => 0,
      Properties_Count => 6,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1  => Common.Contract_Cases_Type_Id, 2 => Common.Paren_Expr_Type_Id,
         3  => Common.Un_Op_Type_Id, 4 => Common.Bin_Op_Type_Id,
         5  => Common.Membership_Expr_Type_Id,
         6  => Common.Base_Aggregate_Type_Id, 7 => Common.Name_Type_Id,
         8  => Common.Box_Expr_Type_Id, 9 => Common.If_Expr_Type_Id,
         10 => Common.Case_Expr_Type_Id,
         11 => Common.Case_Expr_Alternative_Type_Id,
         12 => Common.Quantified_Expr_Type_Id, 13 => Common.Allocator_Type_Id,
         14 => Common.Raise_Expr_Type_Id),

      DSL_Name   => To_Unbounded_String ("Expr"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1 => Expr_P_Expression_Type, 2 => Expr_P_Is_Static_Expr,
         3 => Expr_P_First_Corresponding_Decl, 4 => Expr_P_Eval_As_Int,
         5 => Expr_P_Eval_As_Int_In_Env, 6 => Expr_P_Matching_Nodes)
);

   Allocator_F_Subpool_For_Allocator : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Allocator_F_Subpool
, Index                   => 1);
   Allocator_F_Type_Or_Expr_For_Allocator : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Allocator_F_Type_Or_Expr
,
      Index => 2);

   Desc_For_Allocator : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Allocator"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Allocator_F_Subpool_For_Allocator'Access,
         2 => Allocator_F_Type_Or_Expr_For_Allocator'Access),

      Properties => (1 => Allocator_P_Get_Allocated_Type)
,
      Kind => Ada_Allocator);

   Base_Aggregate_F_Ancestor_Expr_For_Base_Aggregate : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Aggregate_F_Ancestor_Expr
,
      Index => 1);
   Base_Aggregate_F_Assocs_For_Base_Aggregate : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Aggregate_F_Assocs
,
      Index => 2);

   Desc_For_Base_Aggregate : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      =>
        (1 => Common.Aggregate_Type_Id,
         2 => Common.Null_Record_Aggregate_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseAggregate"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Base_Aggregate_F_Ancestor_Expr_For_Base_Aggregate'Access,
         2 => Base_Aggregate_F_Assocs_For_Base_Aggregate'Access),

      Properties => (1 .. 0 => <>)
);

   Desc_For_Aggregate : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Aggregate_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Aggregate"),
Inherited_Fields => 2,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Aggregate);

   Desc_For_Null_Record_Aggregate : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Aggregate_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NullRecordAggregate"),

      Inherited_Fields => 2, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Null_Record_Aggregate);

   Bin_Op_F_Left_For_Bin_Op : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Bin_Op_F_Left
, Index                   => 1);
   Bin_Op_F_Op_For_Bin_Op : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Bin_Op_F_Op
, Index                   => 2);
   Bin_Op_F_Right_For_Bin_Op : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Bin_Op_F_Right
, Index                   => 3);

   Desc_For_Bin_Op : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 => Common.Relation_Op_Type_Id),

      DSL_Name   => To_Unbounded_String ("BinOp"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Bin_Op_F_Left_For_Bin_Op'Access,
         2 => Bin_Op_F_Op_For_Bin_Op'Access,
         3 => Bin_Op_F_Right_For_Bin_Op'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Bin_Op);

   Desc_For_Relation_Op : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Bin_Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RelationOp"),
Inherited_Fields => 3,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Relation_Op);

   Desc_For_Box_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("BoxExpr"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Box_Expr);

   Case_Expr_F_Expr_For_Case_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Expr_F_Expr
, Index                   => 1);
   Case_Expr_F_Cases_For_Case_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Expr_F_Cases
, Index                   => 2);

   Desc_For_Case_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("CaseExpr"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Case_Expr_F_Expr_For_Case_Expr'Access,
         2 => Case_Expr_F_Cases_For_Case_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Case_Expr);

   Case_Expr_Alternative_F_Choices_For_Case_Expr_Alternative : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Expr_Alternative_F_Choices
,
      Index => 1);
   Case_Expr_Alternative_F_Expr_For_Case_Expr_Alternative : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Expr_Alternative_F_Expr
,
      Index => 2);

   Desc_For_Case_Expr_Alternative : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CaseExprAlternative"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Case_Expr_Alternative_F_Choices_For_Case_Expr_Alternative'Access,
         2 => Case_Expr_Alternative_F_Expr_For_Case_Expr_Alternative'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Case_Expr_Alternative);

   Contract_Cases_F_Contract_Cases_For_Contract_Cases : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Contract_Cases_F_Contract_Cases
,
      Index => 1);

   Desc_For_Contract_Cases : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ContractCases"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Contract_Cases_F_Contract_Cases_For_Contract_Cases'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Contract_Cases);

   If_Expr_F_Cond_Expr_For_If_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Expr_F_Cond_Expr
, Index                   => 1);
   If_Expr_F_Then_Expr_For_If_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Expr_F_Then_Expr
, Index                   => 2);
   If_Expr_F_Alternatives_For_If_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Expr_F_Alternatives
,
      Index => 3);
   If_Expr_F_Else_Expr_For_If_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Expr_F_Else_Expr
, Index                   => 4);

   Desc_For_If_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("IfExpr"),
Inherited_Fields => 0,
      Fields     =>
        (1 => If_Expr_F_Cond_Expr_For_If_Expr'Access,
         2 => If_Expr_F_Then_Expr_For_If_Expr'Access,
         3 => If_Expr_F_Alternatives_For_If_Expr'Access,
         4 => If_Expr_F_Else_Expr_For_If_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_If_Expr);

   Membership_Expr_F_Expr_For_Membership_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Membership_Expr_F_Expr
,
      Index => 1);
   Membership_Expr_F_Op_For_Membership_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Membership_Expr_F_Op
, Index                   => 2);
   Membership_Expr_F_Membership_Exprs_For_Membership_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Membership_Expr_F_Membership_Exprs
, Index                   => 3);

   Desc_For_Membership_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("MembershipExpr"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Membership_Expr_F_Expr_For_Membership_Expr'Access,
         2 => Membership_Expr_F_Op_For_Membership_Expr'Access,
         3 => Membership_Expr_F_Membership_Exprs_For_Membership_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Membership_Expr);

   Desc_For_Name : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 10, Fields_Count => 0,
      Properties_Count => 23,
Base_Type              => Common.Expr_Type_Id,
      Derivations      =>
        (1  => Common.Discrete_Subtype_Name_Type_Id,
         2  => Common.Target_Name_Type_Id, 3 => Common.Call_Expr_Type_Id,
         4  => Common.Explicit_Deref_Type_Id,
         5  => Common.Single_Tok_Node_Type_Id,
         6  => Common.Defining_Name_Type_Id, 7 => Common.End_Name_Type_Id,
         8  => Common.Qual_Expr_Type_Id, 9 => Common.Attribute_Ref_Type_Id,
         10 => Common.Dotted_Name_Type_Id),

      DSL_Name   => To_Unbounded_String ("Name"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1  => Name_P_Enclosing_Defining_Name, 2 => Name_P_Is_Defining,
         3  => Name_P_Name_Is, 4 => Name_P_Is_Direct_Call,
         5  => Name_P_Is_Access_Call, 6 => Name_P_Is_Call,
         7  => Name_P_Is_Dot_Call, 8 => Name_P_Failsafe_Referenced_Def_Name,
         9  => Name_P_Referenced_Defining_Name, 10 => Name_P_All_Env_Elements,
         11 => Name_P_Called_Subp_Spec, 12 => Name_P_Referenced_Decl,
         13 => Name_P_Failsafe_Referenced_Decl,
         14 => Name_P_Referenced_Decl_Internal,
         15 => Name_P_Name_Designated_Type, 16 => Name_P_Is_Static_Subtype,
         17 => Name_P_Name_Matches, 18 => Name_P_Relative_Name,
         19 => Name_P_Is_Operator_Name, 20 => Name_P_Is_Write_Reference,
         21 => Name_P_Is_Dispatching_Call, 22 => Name_P_Is_Static_Call,
         23 => Name_P_As_Symbol_Array)
);

   Attribute_Ref_F_Prefix_For_Attribute_Ref : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Attribute_Ref_F_Prefix
,
      Index => 1);
   Attribute_Ref_F_Attribute_For_Attribute_Ref : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Attribute_Ref_F_Attribute
,
      Index => 2);
   Attribute_Ref_F_Args_For_Attribute_Ref : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Attribute_Ref_F_Args
, Index                   => 3);

   Desc_For_Attribute_Ref : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 => Common.Update_Attribute_Ref_Type_Id),

      DSL_Name   => To_Unbounded_String ("AttributeRef"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Attribute_Ref_F_Prefix_For_Attribute_Ref'Access,
         2 => Attribute_Ref_F_Attribute_For_Attribute_Ref'Access,
         3 => Attribute_Ref_F_Args_For_Attribute_Ref'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Attribute_Ref);

   Desc_For_Update_Attribute_Ref : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Attribute_Ref_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("UpdateAttributeRef"),

      Inherited_Fields => 3, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Update_Attribute_Ref);

   Call_Expr_F_Name_For_Call_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Call_Expr_F_Name
, Index                   => 1);
   Call_Expr_F_Suffix_For_Call_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Call_Expr_F_Suffix
, Index                   => 2);

   Desc_For_Call_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("CallExpr"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Call_Expr_F_Name_For_Call_Expr'Access,
         2 => Call_Expr_F_Suffix_For_Call_Expr'Access),

      Properties => (1 => Call_Expr_P_Is_Array_Slice)
, Kind           => Ada_Call_Expr);

   Defining_Name_F_Name_For_Defining_Name : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Defining_Name_F_Name
, Index                   => 1);

   Desc_For_Defining_Name : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 7,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("DefiningName"),
Inherited_Fields => 0,
      Fields     => (1 => Defining_Name_F_Name_For_Defining_Name'Access),

      Properties =>
        (1 => Defining_Name_P_Basic_Decl, 2 => Defining_Name_P_Find_Refs,
         3 => Defining_Name_P_Find_All_References,
         4 => Defining_Name_P_Find_All_Calls, 5 => Defining_Name_P_Next_Part,
         6 => Defining_Name_P_Previous_Part,
         7 => Defining_Name_P_Canonical_Part)
,
      Kind => Ada_Defining_Name);

   Discrete_Subtype_Name_F_Subtype_For_Discrete_Subtype_Name : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Discrete_Subtype_Name_F_Subtype
,
      Index => 1);

   Desc_For_Discrete_Subtype_Name : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscreteSubtypeName"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Discrete_Subtype_Name_F_Subtype_For_Discrete_Subtype_Name'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discrete_Subtype_Name);

   Dotted_Name_F_Prefix_For_Dotted_Name : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Dotted_Name_F_Prefix
, Index                   => 1);
   Dotted_Name_F_Suffix_For_Dotted_Name : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Dotted_Name_F_Suffix
, Index                   => 2);

   Desc_For_Dotted_Name : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("DottedName"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Dotted_Name_F_Prefix_For_Dotted_Name'Access,
         2 => Dotted_Name_F_Suffix_For_Dotted_Name'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Dotted_Name);

   End_Name_F_Name_For_End_Name : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => End_Name_F_Name
, Index                   => 1);

   Desc_For_End_Name : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 1,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EndName"),
Inherited_Fields => 0,
      Fields     => (1 => End_Name_F_Name_For_End_Name'Access),

      Properties => (1 => End_Name_P_Basic_Decl)
, Kind           => Ada_End_Name);

   Explicit_Deref_F_Prefix_For_Explicit_Deref : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Explicit_Deref_F_Prefix
,
      Index => 1);

   Desc_For_Explicit_Deref : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ExplicitDeref"),
Inherited_Fields => 0,
      Fields     => (1 => Explicit_Deref_F_Prefix_For_Explicit_Deref'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Explicit_Deref);

   Qual_Expr_F_Prefix_For_Qual_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Qual_Expr_F_Prefix
, Index                   => 1);
   Qual_Expr_F_Suffix_For_Qual_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Qual_Expr_F_Suffix
, Index                   => 2);

   Desc_For_Qual_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("QualExpr"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Qual_Expr_F_Prefix_For_Qual_Expr'Access,
         2 => Qual_Expr_F_Suffix_For_Qual_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Qual_Expr);

   Desc_For_Single_Tok_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Name_Type_Id,
      Derivations      =>
        (1 => Common.Base_Id_Type_Id, 2 => Common.Num_Literal_Type_Id,
         3 => Common.Null_Literal_Type_Id),

      DSL_Name   => To_Unbounded_String ("SingleTokNode"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties => (1 => Single_Tok_Node_P_Canonical_Text)
);

   Desc_For_Base_Id : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Single_Tok_Node_Type_Id,
      Derivations      =>
        (1 => Common.Op_Type_Id, 2 => Common.Identifier_Type_Id,
         3 => Common.String_Literal_Type_Id, 4 => Common.Char_Literal_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseId"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Char_Literal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Base_Id_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("CharLiteral"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties => (1 => Char_Literal_P_Denoted_Value)
,
      Kind => Ada_Char_Literal);

   Desc_For_Identifier : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Id_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Identifier"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Identifier);

   Desc_For_Op : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 24, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Id_Type_Id,
      Derivations      =>
        (1  => Common.Op_And_Type_Id, 2 => Common.Op_Or_Type_Id,
         3  => Common.Op_Or_Else_Type_Id, 4 => Common.Op_And_Then_Type_Id,
         5  => Common.Op_Xor_Type_Id, 6 => Common.Op_In_Type_Id,
         7  => Common.Op_Not_In_Type_Id, 8 => Common.Op_Abs_Type_Id,
         9  => Common.Op_Not_Type_Id, 10 => Common.Op_Pow_Type_Id,
         11 => Common.Op_Mult_Type_Id, 12 => Common.Op_Div_Type_Id,
         13 => Common.Op_Mod_Type_Id, 14 => Common.Op_Rem_Type_Id,
         15 => Common.Op_Plus_Type_Id, 16 => Common.Op_Minus_Type_Id,
         17 => Common.Op_Concat_Type_Id, 18 => Common.Op_Eq_Type_Id,
         19 => Common.Op_Neq_Type_Id, 20 => Common.Op_Lt_Type_Id,
         21 => Common.Op_Lte_Type_Id, 22 => Common.Op_Gt_Type_Id,
         23 => Common.Op_Gte_Type_Id, 24 => Common.Op_Double_Dot_Type_Id),

      DSL_Name   => To_Unbounded_String ("Op"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Op_Abs : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Abs"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Abs);

   Desc_For_Op_And : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.And"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_And);

   Desc_For_Op_And_Then : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.AndThen"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_And_Then);

   Desc_For_Op_Concat : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Concat"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Concat);

   Desc_For_Op_Div : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Div"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Div);

   Desc_For_Op_Double_Dot : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.DoubleDot"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Double_Dot);

   Desc_For_Op_Eq : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("Op.Eq"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Op_Eq);

   Desc_For_Op_Gt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("Op.Gt"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Op_Gt);

   Desc_For_Op_Gte : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Gte"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Gte);

   Desc_For_Op_In : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("Op.In"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Op_In);

   Desc_For_Op_Lt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("Op.Lt"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Op_Lt);

   Desc_For_Op_Lte : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Lte"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Lte);

   Desc_For_Op_Minus : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Minus"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Minus);

   Desc_For_Op_Mod : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Mod"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Mod);

   Desc_For_Op_Mult : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Mult"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Mult);

   Desc_For_Op_Neq : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Neq"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Neq);

   Desc_For_Op_Not : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Not"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Not);

   Desc_For_Op_Not_In : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.NotIn"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Not_In);

   Desc_For_Op_Or : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("Op.Or"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Op_Or);

   Desc_For_Op_Or_Else : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.OrElse"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Or_Else);

   Desc_For_Op_Plus : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Plus"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Plus);

   Desc_For_Op_Pow : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Pow"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Pow);

   Desc_For_Op_Rem : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Rem"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Rem);

   Desc_For_Op_Xor : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Op_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Op.Xor"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Op_Xor);

   Desc_For_String_Literal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Base_Id_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("StringLiteral"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties => (1 => String_Literal_P_Denoted_Value)
,
      Kind => Ada_String_Literal);

   Desc_For_Null_Literal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Single_Tok_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NullLiteral"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Null_Literal);

   Desc_For_Num_Literal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Single_Tok_Node_Type_Id,
      Derivations      =>
        (1 => Common.Real_Literal_Type_Id, 2 => Common.Int_Literal_Type_Id),

      DSL_Name   => To_Unbounded_String ("NumLiteral"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Int_Literal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Num_Literal_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("IntLiteral"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties => (1 => Int_Literal_P_Denoted_Value)
,
      Kind => Ada_Int_Literal);

   Desc_For_Real_Literal : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Num_Literal_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RealLiteral"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Real_Literal);

   Desc_For_Target_Name : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Name_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("TargetName"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Target_Name);

   Paren_Expr_F_Expr_For_Paren_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Paren_Expr_F_Expr
, Index                   => 1);

   Desc_For_Paren_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ParenExpr"),
Inherited_Fields => 0,
      Fields     => (1 => Paren_Expr_F_Expr_For_Paren_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Paren_Expr);

   Quantified_Expr_F_Quantifier_For_Quantified_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Quantified_Expr_F_Quantifier
,
      Index => 1);
   Quantified_Expr_F_Loop_Spec_For_Quantified_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Quantified_Expr_F_Loop_Spec
,
      Index => 2);
   Quantified_Expr_F_Expr_For_Quantified_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Quantified_Expr_F_Expr
,
      Index => 3);

   Desc_For_Quantified_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("QuantifiedExpr"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Quantified_Expr_F_Quantifier_For_Quantified_Expr'Access,
         2 => Quantified_Expr_F_Loop_Spec_For_Quantified_Expr'Access,
         3 => Quantified_Expr_F_Expr_For_Quantified_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Quantified_Expr);

   Raise_Expr_F_Exception_Name_For_Raise_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Raise_Expr_F_Exception_Name
,
      Index => 1);
   Raise_Expr_F_Error_Message_For_Raise_Expr : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Raise_Expr_F_Error_Message
,
      Index => 2);

   Desc_For_Raise_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RaiseExpr"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Raise_Expr_F_Exception_Name_For_Raise_Expr'Access,
         2 => Raise_Expr_F_Error_Message_For_Raise_Expr'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Raise_Expr);

   Un_Op_F_Op_For_Un_Op : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Un_Op_F_Op
, Index                   => 1);
   Un_Op_F_Expr_For_Un_Op : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Un_Op_F_Expr
, Index                   => 2);

   Desc_For_Un_Op : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("UnOp"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Un_Op_F_Op_For_Un_Op'Access, 2 => Un_Op_F_Expr_For_Un_Op'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Un_Op);

   Handled_Stmts_F_Stmts_For_Handled_Stmts : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Handled_Stmts_F_Stmts
,
      Index => 1);
   Handled_Stmts_F_Exceptions_For_Handled_Stmts : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Handled_Stmts_F_Exceptions
,
      Index => 2);

   Desc_For_Handled_Stmts : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("HandledStmts"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Handled_Stmts_F_Stmts_For_Handled_Stmts'Access,
         2 => Handled_Stmts_F_Exceptions_For_Handled_Stmts'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Handled_Stmts);

   Desc_For_Interface_Kind : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Interface_Kind_Limited_Type_Id,
         2 => Common.Interface_Kind_Task_Type_Id,
         3 => Common.Interface_Kind_Protected_Type_Id,
         4 => Common.Interface_Kind_Synchronized_Type_Id),

      DSL_Name   => To_Unbounded_String ("InterfaceKind"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Interface_Kind_Limited : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Interface_Kind_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("InterfaceKind.Limited"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Interface_Kind_Limited);

   Desc_For_Interface_Kind_Protected : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Interface_Kind_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("InterfaceKind.Protected"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Interface_Kind_Protected);

   Desc_For_Interface_Kind_Synchronized : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Interface_Kind_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("InterfaceKind.Synchronized"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Interface_Kind_Synchronized);

   Desc_For_Interface_Kind_Task : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Interface_Kind_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("InterfaceKind.Task"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Interface_Kind_Task);

   Desc_For_Iter_Type : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Iter_Type_In_Type_Id, 2 => Common.Iter_Type_Of_Type_Id),

      DSL_Name   => To_Unbounded_String ("IterType"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Iter_Type_In : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Iter_Type_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("IterType.In"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Iter_Type_In);

   Desc_For_Iter_Type_Of : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Iter_Type_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("IterType.Of"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Iter_Type_Of);

   Library_Item_F_Has_Private_For_Library_Item : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Library_Item_F_Has_Private
,
      Index => 1);
   Library_Item_F_Item_For_Library_Item : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Library_Item_F_Item
, Index                   => 2);

   Desc_For_Library_Item : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("LibraryItem"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Library_Item_F_Has_Private_For_Library_Item'Access,
         2 => Library_Item_F_Item_For_Library_Item'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Library_Item);

   Desc_For_Limited_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Limited_Present_Type_Id,
         2 => Common.Limited_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Limited"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Limited_Node_P_As_Bool)
);

   Desc_For_Limited_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Limited_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Limited.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Limited_Absent);

   Desc_For_Limited_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Limited_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Limited.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Limited_Present);

   Desc_For_Loop_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.For_Loop_Spec_Type_Id,
         2 => Common.While_Loop_Spec_Type_Id),

      DSL_Name   => To_Unbounded_String ("LoopSpec"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   For_Loop_Spec_F_Var_Decl_For_For_Loop_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => For_Loop_Spec_F_Var_Decl
,
      Index => 1);
   For_Loop_Spec_F_Loop_Type_For_For_Loop_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => For_Loop_Spec_F_Loop_Type
,
      Index => 2);
   For_Loop_Spec_F_Has_Reverse_For_For_Loop_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => For_Loop_Spec_F_Has_Reverse
,
      Index => 3);
   For_Loop_Spec_F_Iter_Expr_For_For_Loop_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => For_Loop_Spec_F_Iter_Expr
,
      Index => 4);

   Desc_For_For_Loop_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Loop_Spec_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ForLoopSpec"),
Inherited_Fields => 0,
      Fields     =>
        (1 => For_Loop_Spec_F_Var_Decl_For_For_Loop_Spec'Access,
         2 => For_Loop_Spec_F_Loop_Type_For_For_Loop_Spec'Access,
         3 => For_Loop_Spec_F_Has_Reverse_For_For_Loop_Spec'Access,
         4 => For_Loop_Spec_F_Iter_Expr_For_For_Loop_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_For_Loop_Spec);

   While_Loop_Spec_F_Expr_For_While_Loop_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => While_Loop_Spec_F_Expr
,
      Index => 1);

   Desc_For_While_Loop_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Loop_Spec_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("WhileLoopSpec"),
Inherited_Fields => 0,
      Fields     => (1 => While_Loop_Spec_F_Expr_For_While_Loop_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_While_Loop_Spec);

   Desc_For_Mode : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 4, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Mode_In_Type_Id, 2 => Common.Mode_Out_Type_Id,
         3 => Common.Mode_In_Out_Type_Id, 4 => Common.Mode_Default_Type_Id),

      DSL_Name   => To_Unbounded_String ("Mode"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Mode_Default : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Mode_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Mode.Default"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Mode_Default);

   Desc_For_Mode_In : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Mode_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Mode.In"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Mode_In);

   Desc_For_Mode_In_Out : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Mode_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Mode.InOut"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Mode_In_Out);

   Desc_For_Mode_Out : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Mode_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Mode.Out"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Mode_Out);

   Desc_For_Not_Null : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Not_Null_Present_Type_Id,
         2 => Common.Not_Null_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("NotNull"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Not_Null_P_As_Bool)
);

   Desc_For_Not_Null_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Not_Null_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NotNull.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Not_Null_Absent);

   Desc_For_Not_Null_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Not_Null_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NotNull.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Not_Null_Present);

   Desc_For_Null_Component_Decl : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NullComponentDecl"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Null_Component_Decl);

   Desc_For_Others_Designator : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("OthersDesignator"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Others_Designator);

   Desc_For_Overriding_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Overriding_Overriding_Type_Id,
         2 => Common.Overriding_Not_Overriding_Type_Id,
         3 => Common.Overriding_Unspecified_Type_Id),

      DSL_Name   => To_Unbounded_String ("Overriding"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Overriding_Not_Overriding : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Overriding_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Overriding.NotOverriding"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Overriding_Not_Overriding);

   Desc_For_Overriding_Overriding : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Overriding_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Overriding.Overriding"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Overriding_Overriding);

   Desc_For_Overriding_Unspecified : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Overriding_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Overriding.Unspecified"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Overriding_Unspecified);

   Params_F_Params_For_Params : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Params_F_Params
, Index                   => 1);

   Desc_For_Params : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Params"),
Inherited_Fields => 0,
      Fields     => (1 => Params_F_Params_For_Params'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Params);

   Pragma_Node_F_Id_For_Pragma_Node : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Pragma_Node_F_Id
, Index                   => 1);
   Pragma_Node_F_Args_For_Pragma_Node : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Pragma_Node_F_Args
, Index                   => 2);

   Desc_For_Pragma_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Pragma"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Pragma_Node_F_Id_For_Pragma_Node'Access,
         2 => Pragma_Node_F_Args_For_Pragma_Node'Access),

      Properties => (1 => Pragma_Node_P_Associated_Decls)
,
      Kind => Ada_Pragma_Node);

   Desc_For_Prim_Type_Accessor : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PrimTypeAccessor"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Prim_Type_Accessor);

   Desc_For_Private_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Private_Present_Type_Id,
         2 => Common.Private_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Private"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Private_Node_P_As_Bool)
);

   Desc_For_Private_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Private_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Private.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Private_Absent);

   Desc_For_Private_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Private_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Private.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Private_Present);

   Protected_Def_F_Public_Part_For_Protected_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Def_F_Public_Part
,
      Index => 1);
   Protected_Def_F_Private_Part_For_Protected_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Def_F_Private_Part
,
      Index => 2);
   Protected_Def_F_End_Name_For_Protected_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Protected_Def_F_End_Name
,
      Index => 3);

   Desc_For_Protected_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ProtectedDef"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Protected_Def_F_Public_Part_For_Protected_Def'Access,
         2 => Protected_Def_F_Private_Part_For_Protected_Def'Access,
         3 => Protected_Def_F_End_Name_For_Protected_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Protected_Def);

   Desc_For_Protected_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Protected_Present_Type_Id,
         2 => Common.Protected_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Protected"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Protected_Node_P_As_Bool)
);

   Desc_For_Protected_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Protected_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Protected.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Protected_Absent);

   Desc_For_Protected_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Protected_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Protected.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Protected_Present);

   Desc_For_Quantifier : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Quantifier_All_Type_Id,
         2 => Common.Quantifier_Some_Type_Id),

      DSL_Name   => To_Unbounded_String ("Quantifier"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Quantifier_All : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Quantifier_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Quantifier.All"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Quantifier_All);

   Desc_For_Quantifier_Some : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Quantifier_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Quantifier.Some"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Quantifier_Some);

   Range_Spec_F_Range_For_Range_Spec : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Range_Spec_F_Range
, Index                   => 1);

   Desc_For_Range_Spec : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RangeSpec"),
Inherited_Fields => 0,
      Fields     => (1 => Range_Spec_F_Range_For_Range_Spec'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Range_Spec);

   Renaming_Clause_F_Renamed_Object_For_Renaming_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Renaming_Clause_F_Renamed_Object
,
      Index => 1);

   Desc_For_Renaming_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 => Common.Synthetic_Renaming_Clause_Type_Id),

      DSL_Name => To_Unbounded_String ("RenamingClause"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Renaming_Clause_F_Renamed_Object_For_Renaming_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Renaming_Clause);

   Desc_For_Synthetic_Renaming_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Renaming_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SyntheticRenamingClause"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Synthetic_Renaming_Clause);

   Desc_For_Reverse_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Reverse_Present_Type_Id,
         2 => Common.Reverse_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Reverse"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Reverse_Node_P_As_Bool)
);

   Desc_For_Reverse_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Reverse_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Reverse.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Reverse_Absent);

   Desc_For_Reverse_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Reverse_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Reverse.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Reverse_Present);

   Select_When_Part_F_Cond_Expr_For_Select_When_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Select_When_Part_F_Cond_Expr
,
      Index => 1);
   Select_When_Part_F_Stmts_For_Select_When_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Select_When_Part_F_Stmts
,
      Index => 2);

   Desc_For_Select_When_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SelectWhenPart"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Select_When_Part_F_Cond_Expr_For_Select_When_Part'Access,
         2 => Select_When_Part_F_Stmts_For_Select_When_Part'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Select_When_Part);

   Desc_For_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Error_Stmt_Type_Id, 2 => Common.Simple_Stmt_Type_Id,
         3 => Common.Composite_Stmt_Type_Id),

      DSL_Name   => To_Unbounded_String ("Stmt"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Composite_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 8, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Stmt_Type_Id,
      Derivations      =>
        (1 => Common.If_Stmt_Type_Id, 2 => Common.Named_Stmt_Type_Id,
         3 => Common.Base_Loop_Stmt_Type_Id, 4 => Common.Block_Stmt_Type_Id,
         5 => Common.Extended_Return_Stmt_Type_Id,
         6 => Common.Case_Stmt_Type_Id, 7 => Common.Accept_Stmt_Type_Id,
         8 => Common.Select_Stmt_Type_Id),

      DSL_Name   => To_Unbounded_String ("CompositeStmt"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Accept_Stmt_F_Name_For_Accept_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Accept_Stmt_F_Name
, Index                   => 1);
   Accept_Stmt_F_Entry_Index_Expr_For_Accept_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Accept_Stmt_F_Entry_Index_Expr
,
      Index => 2);
   Accept_Stmt_F_Params_For_Accept_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Accept_Stmt_F_Params
, Index                   => 3);

   Desc_For_Accept_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 1, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      => (1 => Common.Accept_Stmt_With_Stmts_Type_Id),

      DSL_Name   => To_Unbounded_String ("AcceptStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Accept_Stmt_F_Name_For_Accept_Stmt'Access,
         2 => Accept_Stmt_F_Entry_Index_Expr_For_Accept_Stmt'Access,
         3 => Accept_Stmt_F_Params_For_Accept_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Accept_Stmt);

   Accept_Stmt_With_Stmts_F_Stmts_For_Accept_Stmt_With_Stmts : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Accept_Stmt_With_Stmts_F_Stmts
,
      Index => 4);
   Accept_Stmt_With_Stmts_F_End_Name_For_Accept_Stmt_With_Stmts : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Accept_Stmt_With_Stmts_F_End_Name
,
      Index => 5);

   Desc_For_Accept_Stmt_With_Stmts : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Accept_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AcceptStmtWithStmts"),

      Inherited_Fields => 3,
      Fields           =>
        (1 => Accept_Stmt_With_Stmts_F_Stmts_For_Accept_Stmt_With_Stmts'Access,
         2 =>
           Accept_Stmt_With_Stmts_F_End_Name_For_Accept_Stmt_With_Stmts'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Accept_Stmt_With_Stmts);

   Base_Loop_Stmt_F_Spec_For_Base_Loop_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Loop_Stmt_F_Spec
,
      Index => 1);
   Base_Loop_Stmt_F_Stmts_For_Base_Loop_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Loop_Stmt_F_Stmts
,
      Index => 2);
   Base_Loop_Stmt_F_End_Name_For_Base_Loop_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Base_Loop_Stmt_F_End_Name
,
      Index => 3);

   Desc_For_Base_Loop_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      =>
        (1 => Common.Loop_Stmt_Type_Id, 2 => Common.For_Loop_Stmt_Type_Id,
         3 => Common.While_Loop_Stmt_Type_Id),

      DSL_Name   => To_Unbounded_String ("BaseLoopStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Base_Loop_Stmt_F_Spec_For_Base_Loop_Stmt'Access,
         2 => Base_Loop_Stmt_F_Stmts_For_Base_Loop_Stmt'Access,
         3 => Base_Loop_Stmt_F_End_Name_For_Base_Loop_Stmt'Access),

      Properties => (1 .. 0 => <>)
);

   Desc_For_For_Loop_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Loop_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ForLoopStmt"),
Inherited_Fields => 3,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_For_Loop_Stmt);

   Desc_For_Loop_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Loop_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("LoopStmt"),
Inherited_Fields => 3,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Loop_Stmt);

   Desc_For_While_Loop_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Base_Loop_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("WhileLoopStmt"),
Inherited_Fields => 3,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_While_Loop_Stmt);

   Desc_For_Block_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      =>
        (1 => Common.Decl_Block_Type_Id, 2 => Common.Begin_Block_Type_Id),

      DSL_Name   => To_Unbounded_String ("BlockStmt"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Begin_Block_F_Stmts_For_Begin_Block : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Begin_Block_F_Stmts
, Index                   => 1);
   Begin_Block_F_End_Name_For_Begin_Block : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Begin_Block_F_End_Name
,
      Index => 2);

   Desc_For_Begin_Block : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Block_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("BeginBlock"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Begin_Block_F_Stmts_For_Begin_Block'Access,
         2 => Begin_Block_F_End_Name_For_Begin_Block'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Begin_Block);

   Decl_Block_F_Decls_For_Decl_Block : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Decl_Block_F_Decls
, Index                   => 1);
   Decl_Block_F_Stmts_For_Decl_Block : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Decl_Block_F_Stmts
, Index                   => 2);
   Decl_Block_F_End_Name_For_Decl_Block : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Decl_Block_F_End_Name
,
      Index => 3);

   Desc_For_Decl_Block : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Block_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("DeclBlock"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Decl_Block_F_Decls_For_Decl_Block'Access,
         2 => Decl_Block_F_Stmts_For_Decl_Block'Access,
         3 => Decl_Block_F_End_Name_For_Decl_Block'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Decl_Block);

   Case_Stmt_F_Expr_For_Case_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Stmt_F_Expr
, Index                   => 1);
   Case_Stmt_F_Alternatives_For_Case_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Case_Stmt_F_Alternatives
,
      Index => 2);

   Desc_For_Case_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("CaseStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Case_Stmt_F_Expr_For_Case_Stmt'Access,
         2 => Case_Stmt_F_Alternatives_For_Case_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Case_Stmt);

   Extended_Return_Stmt_F_Decl_For_Extended_Return_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Extended_Return_Stmt_F_Decl
,
      Index => 1);
   Extended_Return_Stmt_F_Stmts_For_Extended_Return_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Extended_Return_Stmt_F_Stmts
,
      Index => 2);

   Desc_For_Extended_Return_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExtendedReturnStmt"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Extended_Return_Stmt_F_Decl_For_Extended_Return_Stmt'Access,
         2 => Extended_Return_Stmt_F_Stmts_For_Extended_Return_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Extended_Return_Stmt);

   If_Stmt_F_Cond_Expr_For_If_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Stmt_F_Cond_Expr
, Index                   => 1);
   If_Stmt_F_Then_Stmts_For_If_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Stmt_F_Then_Stmts
, Index                   => 2);
   If_Stmt_F_Alternatives_For_If_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Stmt_F_Alternatives
,
      Index => 3);
   If_Stmt_F_Else_Stmts_For_If_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => If_Stmt_F_Else_Stmts
, Index                   => 4);

   Desc_For_If_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("IfStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => If_Stmt_F_Cond_Expr_For_If_Stmt'Access,
         2 => If_Stmt_F_Then_Stmts_For_If_Stmt'Access,
         3 => If_Stmt_F_Alternatives_For_If_Stmt'Access,
         4 => If_Stmt_F_Else_Stmts_For_If_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_If_Stmt);

   Named_Stmt_F_Decl_For_Named_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Named_Stmt_F_Decl
, Index                   => 1);
   Named_Stmt_F_Stmt_For_Named_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Named_Stmt_F_Stmt
, Index                   => 2);

   Desc_For_Named_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NamedStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Named_Stmt_F_Decl_For_Named_Stmt'Access,
         2 => Named_Stmt_F_Stmt_For_Named_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Named_Stmt);

   Select_Stmt_F_Guards_For_Select_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Select_Stmt_F_Guards
, Index                   => 1);
   Select_Stmt_F_Else_Stmts_For_Select_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Select_Stmt_F_Else_Stmts
,
      Index => 2);
   Select_Stmt_F_Abort_Stmts_For_Select_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Select_Stmt_F_Abort_Stmts
,
      Index => 3);

   Desc_For_Select_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Composite_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("SelectStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Select_Stmt_F_Guards_For_Select_Stmt'Access,
         2 => Select_Stmt_F_Else_Stmts_For_Select_Stmt'Access,
         3 => Select_Stmt_F_Abort_Stmts_For_Select_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Select_Stmt);

   Desc_For_Error_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ErrorStmt"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Error_Stmt);

   Desc_For_Simple_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 12, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Stmt_Type_Id,
      Derivations      =>
        (1  => Common.Call_Stmt_Type_Id, 2 => Common.Null_Stmt_Type_Id,
         3  => Common.Assign_Stmt_Type_Id, 4 => Common.Goto_Stmt_Type_Id,
         5  => Common.Exit_Stmt_Type_Id, 6 => Common.Return_Stmt_Type_Id,
         7  => Common.Requeue_Stmt_Type_Id, 8 => Common.Abort_Stmt_Type_Id,
         9  => Common.Delay_Stmt_Type_Id, 10 => Common.Raise_Stmt_Type_Id,
         11 => Common.Label_Type_Id,
         12 => Common.Terminate_Alternative_Type_Id),

      DSL_Name   => To_Unbounded_String ("SimpleStmt"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Abort_Stmt_F_Names_For_Abort_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Abort_Stmt_F_Names
, Index                   => 1);

   Desc_For_Abort_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AbortStmt"),
Inherited_Fields => 0,
      Fields     => (1 => Abort_Stmt_F_Names_For_Abort_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Abort_Stmt);

   Assign_Stmt_F_Dest_For_Assign_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Assign_Stmt_F_Dest
, Index                   => 1);
   Assign_Stmt_F_Expr_For_Assign_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Assign_Stmt_F_Expr
, Index                   => 2);

   Desc_For_Assign_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AssignStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Assign_Stmt_F_Dest_For_Assign_Stmt'Access,
         2 => Assign_Stmt_F_Expr_For_Assign_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Assign_Stmt);

   Call_Stmt_F_Call_For_Call_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Call_Stmt_F_Call
, Index                   => 1);

   Desc_For_Call_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("CallStmt"),
Inherited_Fields => 0,
      Fields     => (1 => Call_Stmt_F_Call_For_Call_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Call_Stmt);

   Delay_Stmt_F_Has_Until_For_Delay_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Delay_Stmt_F_Has_Until
,
      Index => 1);
   Delay_Stmt_F_Expr_For_Delay_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Delay_Stmt_F_Expr
, Index                   => 2);

   Desc_For_Delay_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("DelayStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Delay_Stmt_F_Has_Until_For_Delay_Stmt'Access,
         2 => Delay_Stmt_F_Expr_For_Delay_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Delay_Stmt);

   Exit_Stmt_F_Loop_Name_For_Exit_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Exit_Stmt_F_Loop_Name
,
      Index => 1);
   Exit_Stmt_F_Cond_Expr_For_Exit_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Exit_Stmt_F_Cond_Expr
,
      Index => 2);

   Desc_For_Exit_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ExitStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Exit_Stmt_F_Loop_Name_For_Exit_Stmt'Access,
         2 => Exit_Stmt_F_Cond_Expr_For_Exit_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Exit_Stmt);

   Goto_Stmt_F_Label_Name_For_Goto_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Goto_Stmt_F_Label_Name
,
      Index => 1);

   Desc_For_Goto_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("GotoStmt"),
Inherited_Fields => 0,
      Fields     => (1 => Goto_Stmt_F_Label_Name_For_Goto_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Goto_Stmt);

   Label_F_Decl_For_Label : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Label_F_Decl
, Index                   => 1);

   Desc_For_Label : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),
DSL_Name               => To_Unbounded_String ("Label"),

      Inherited_Fields => 0, Fields => (1 => Label_F_Decl_For_Label'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Label);

   Desc_For_Null_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("NullStmt"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Null_Stmt);

   Raise_Stmt_F_Exception_Name_For_Raise_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Raise_Stmt_F_Exception_Name
,
      Index => 1);
   Raise_Stmt_F_Error_Message_For_Raise_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Raise_Stmt_F_Error_Message
,
      Index => 2);

   Desc_For_Raise_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RaiseStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Raise_Stmt_F_Exception_Name_For_Raise_Stmt'Access,
         2 => Raise_Stmt_F_Error_Message_For_Raise_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Raise_Stmt);

   Requeue_Stmt_F_Call_Name_For_Requeue_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Requeue_Stmt_F_Call_Name
,
      Index => 1);
   Requeue_Stmt_F_Has_Abort_For_Requeue_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Requeue_Stmt_F_Has_Abort
,
      Index => 2);

   Desc_For_Requeue_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RequeueStmt"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Requeue_Stmt_F_Call_Name_For_Requeue_Stmt'Access,
         2 => Requeue_Stmt_F_Has_Abort_For_Requeue_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Requeue_Stmt);

   Return_Stmt_F_Return_Expr_For_Return_Stmt : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Return_Stmt_F_Return_Expr
,
      Index => 1);

   Desc_For_Return_Stmt : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ReturnStmt"),
Inherited_Fields => 0,
      Fields     => (1 => Return_Stmt_F_Return_Expr_For_Return_Stmt'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Return_Stmt);

   Desc_For_Terminate_Alternative : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Simple_Stmt_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TerminateAlternative"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Terminate_Alternative);

   Desc_For_Subp_Kind : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Subp_Kind_Procedure_Type_Id,
         2 => Common.Subp_Kind_Function_Type_Id),

      DSL_Name   => To_Unbounded_String ("SubpKind"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Desc_For_Subp_Kind_Function : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Subp_Kind_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SubpKind.Function"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Kind_Function);

   Desc_For_Subp_Kind_Procedure : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Subp_Kind_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SubpKind.Procedure"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Subp_Kind_Procedure);

   Subunit_F_Name_For_Subunit : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subunit_F_Name
, Index                   => 1);
   Subunit_F_Body_For_Subunit : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subunit_F_Body
, Index                   => 2);

   Desc_For_Subunit : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Subunit"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Subunit_F_Name_For_Subunit'Access,
         2 => Subunit_F_Body_For_Subunit'Access),

      Properties => (1 => Subunit_P_Body_Root)
, Kind           => Ada_Subunit);

   Desc_For_Synchronized_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Synchronized_Present_Type_Id,
         2 => Common.Synchronized_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Synchronized"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties => (1 => Synchronized_Node_P_As_Bool)
);

   Desc_For_Synchronized_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Synchronized_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Synchronized.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Synchronized_Absent);

   Desc_For_Synchronized_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Synchronized_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Synchronized.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Synchronized_Present);

   Desc_For_Tagged_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Tagged_Present_Type_Id,
         2 => Common.Tagged_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Tagged"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Tagged_Node_P_As_Bool)
);

   Desc_For_Tagged_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Tagged_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Tagged.Absent"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Tagged_Absent);

   Desc_For_Tagged_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Tagged_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Tagged.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Tagged_Present);

   Task_Def_F_Interfaces_For_Task_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Def_F_Interfaces
,
      Index => 1);
   Task_Def_F_Public_Part_For_Task_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Def_F_Public_Part
,
      Index => 2);
   Task_Def_F_Private_Part_For_Task_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Def_F_Private_Part
,
      Index => 3);
   Task_Def_F_End_Name_For_Task_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Task_Def_F_End_Name
, Index                   => 4);

   Desc_For_Task_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("TaskDef"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Task_Def_F_Interfaces_For_Task_Def'Access,
         2 => Task_Def_F_Public_Part_For_Task_Def'Access,
         3 => Task_Def_F_Private_Part_For_Task_Def'Access,
         4 => Task_Def_F_End_Name_For_Task_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Task_Def);

   Desc_For_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 11, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1  => Common.Record_Type_Def_Type_Id,
         2  => Common.Real_Type_Def_Type_Id, 3 => Common.Enum_Type_Def_Type_Id,
         4  => Common.Derived_Type_Def_Type_Id,
         5  => Common.Private_Type_Def_Type_Id,
         6  => Common.Signed_Int_Type_Def_Type_Id,
         7  => Common.Mod_Int_Type_Def_Type_Id,
         8  => Common.Array_Type_Def_Type_Id,
         9  => Common.Interface_Type_Def_Type_Id,
         10 => Common.Access_Def_Type_Id,
         11 => Common.Formal_Discrete_Type_Def_Type_Id),

      DSL_Name   => To_Unbounded_String ("TypeDef"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Access_Def_F_Has_Not_Null_For_Access_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Access_Def_F_Has_Not_Null
,
      Index => 1);

   Desc_For_Access_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      =>
        (1 => Common.Access_To_Subp_Def_Type_Id,
         2 => Common.Base_Type_Access_Def_Type_Id),

      DSL_Name   => To_Unbounded_String ("AccessDef"),
Inherited_Fields => 0,
      Fields     => (1 => Access_Def_F_Has_Not_Null_For_Access_Def'Access),

      Properties => (1 .. 0 => <>)
);

   Access_To_Subp_Def_F_Has_Protected_For_Access_To_Subp_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Access_To_Subp_Def_F_Has_Protected
, Index                   => 2);
   Access_To_Subp_Def_F_Subp_Spec_For_Access_To_Subp_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Access_To_Subp_Def_F_Subp_Spec
,
      Index => 3);

   Desc_For_Access_To_Subp_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Access_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AccessToSubpDef"),

      Inherited_Fields => 1,
      Fields           =>
        (1 => Access_To_Subp_Def_F_Has_Protected_For_Access_To_Subp_Def'Access,
         2 => Access_To_Subp_Def_F_Subp_Spec_For_Access_To_Subp_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Access_To_Subp_Def);

   Desc_For_Base_Type_Access_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Access_Def_Type_Id,
      Derivations      =>
        (1 => Common.Type_Access_Def_Type_Id,
         2 => Common.Anonymous_Type_Access_Def_Type_Id),

      DSL_Name => To_Unbounded_String ("BaseTypeAccessDef"),

      Inherited_Fields => 1, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
);

   Anonymous_Type_Access_Def_F_Type_Decl_For_Anonymous_Type_Access_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Anonymous_Type_Access_Def_F_Type_Decl
, Index                   => 2);

   Desc_For_Anonymous_Type_Access_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Access_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AnonymousTypeAccessDef"),

      Inherited_Fields => 1,
      Fields           =>
        (1 =>
           Anonymous_Type_Access_Def_F_Type_Decl_For_Anonymous_Type_Access_Def'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Anonymous_Type_Access_Def);

   Type_Access_Def_F_Has_All_For_Type_Access_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Type_Access_Def_F_Has_All
,
      Index => 2);
   Type_Access_Def_F_Has_Constant_For_Type_Access_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Type_Access_Def_F_Has_Constant
,
      Index => 3);
   Type_Access_Def_F_Subtype_Indication_For_Type_Access_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Type_Access_Def_F_Subtype_Indication
, Index                   => 4);

   Desc_For_Type_Access_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Base_Type_Access_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("TypeAccessDef"),
Inherited_Fields => 1,
      Fields     =>
        (1 => Type_Access_Def_F_Has_All_For_Type_Access_Def'Access,
         2 => Type_Access_Def_F_Has_Constant_For_Type_Access_Def'Access,
         3 => Type_Access_Def_F_Subtype_Indication_For_Type_Access_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Type_Access_Def);

   Array_Type_Def_F_Indices_For_Array_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Array_Type_Def_F_Indices
,
      Index => 1);
   Array_Type_Def_F_Component_Type_For_Array_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Array_Type_Def_F_Component_Type
,
      Index => 2);

   Desc_For_Array_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ArrayTypeDef"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Array_Type_Def_F_Indices_For_Array_Type_Def'Access,
         2 => Array_Type_Def_F_Component_Type_For_Array_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Array_Type_Def);

   Derived_Type_Def_F_Has_Abstract_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Derived_Type_Def_F_Has_Abstract
,
      Index => 1);
   Derived_Type_Def_F_Has_Limited_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Derived_Type_Def_F_Has_Limited
,
      Index => 2);
   Derived_Type_Def_F_Has_Synchronized_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Derived_Type_Def_F_Has_Synchronized
, Index                   => 3);
   Derived_Type_Def_F_Subtype_Indication_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Derived_Type_Def_F_Subtype_Indication
, Index                   => 4);
   Derived_Type_Def_F_Interfaces_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Derived_Type_Def_F_Interfaces
,
      Index => 5);
   Derived_Type_Def_F_Record_Extension_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Derived_Type_Def_F_Record_Extension
, Index                   => 6);
   Derived_Type_Def_F_Has_With_Private_For_Derived_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Derived_Type_Def_F_Has_With_Private
, Index                   => 7);

   Desc_For_Derived_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 7,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DerivedTypeDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Derived_Type_Def_F_Has_Abstract_For_Derived_Type_Def'Access,
         2 => Derived_Type_Def_F_Has_Limited_For_Derived_Type_Def'Access,
         3 => Derived_Type_Def_F_Has_Synchronized_For_Derived_Type_Def'Access,
         4 =>
           Derived_Type_Def_F_Subtype_Indication_For_Derived_Type_Def'Access,
         5 => Derived_Type_Def_F_Interfaces_For_Derived_Type_Def'Access,
         6 => Derived_Type_Def_F_Record_Extension_For_Derived_Type_Def'Access,
         7 => Derived_Type_Def_F_Has_With_Private_For_Derived_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Derived_Type_Def);

   Enum_Type_Def_F_Enum_Literals_For_Enum_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Enum_Type_Def_F_Enum_Literals
,
      Index => 1);

   Desc_For_Enum_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("EnumTypeDef"),
Inherited_Fields => 0,
      Fields => (1 => Enum_Type_Def_F_Enum_Literals_For_Enum_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Enum_Type_Def);

   Desc_For_Formal_Discrete_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("FormalDiscreteTypeDef"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Formal_Discrete_Type_Def);

   Interface_Type_Def_F_Interface_Kind_For_Interface_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Interface_Type_Def_F_Interface_Kind
, Index                   => 1);
   Interface_Type_Def_F_Interfaces_For_Interface_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Interface_Type_Def_F_Interfaces
,
      Index => 2);

   Desc_For_Interface_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("InterfaceTypeDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Interface_Type_Def_F_Interface_Kind_For_Interface_Type_Def'Access,
         2 => Interface_Type_Def_F_Interfaces_For_Interface_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Interface_Type_Def);

   Mod_Int_Type_Def_F_Expr_For_Mod_Int_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Mod_Int_Type_Def_F_Expr
,
      Index => 1);

   Desc_For_Mod_Int_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("ModIntTypeDef"),
Inherited_Fields => 0,
      Fields     => (1 => Mod_Int_Type_Def_F_Expr_For_Mod_Int_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Mod_Int_Type_Def);

   Private_Type_Def_F_Has_Abstract_For_Private_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Private_Type_Def_F_Has_Abstract
,
      Index => 1);
   Private_Type_Def_F_Has_Tagged_For_Private_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Private_Type_Def_F_Has_Tagged
,
      Index => 2);
   Private_Type_Def_F_Has_Limited_For_Private_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Private_Type_Def_F_Has_Limited
,
      Index => 3);

   Desc_For_Private_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PrivateTypeDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Private_Type_Def_F_Has_Abstract_For_Private_Type_Def'Access,
         2 => Private_Type_Def_F_Has_Tagged_For_Private_Type_Def'Access,
         3 => Private_Type_Def_F_Has_Limited_For_Private_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Private_Type_Def);

   Desc_For_Real_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      =>
        (1 => Common.Floating_Point_Def_Type_Id,
         2 => Common.Ordinary_Fixed_Point_Def_Type_Id,
         3 => Common.Decimal_Fixed_Point_Def_Type_Id),

      DSL_Name   => To_Unbounded_String ("RealTypeDef"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Decimal_Fixed_Point_Def_F_Delta_For_Decimal_Fixed_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Decimal_Fixed_Point_Def_F_Delta
,
      Index => 1);
   Decimal_Fixed_Point_Def_F_Digits_For_Decimal_Fixed_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Decimal_Fixed_Point_Def_F_Digits
,
      Index => 2);
   Decimal_Fixed_Point_Def_F_Range_For_Decimal_Fixed_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Decimal_Fixed_Point_Def_F_Range
,
      Index => 3);

   Desc_For_Decimal_Fixed_Point_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Real_Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DecimalFixedPointDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Decimal_Fixed_Point_Def_F_Delta_For_Decimal_Fixed_Point_Def'Access,
         2 =>
           Decimal_Fixed_Point_Def_F_Digits_For_Decimal_Fixed_Point_Def'Access,
         3 =>
           Decimal_Fixed_Point_Def_F_Range_For_Decimal_Fixed_Point_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Decimal_Fixed_Point_Def);

   Floating_Point_Def_F_Num_Digits_For_Floating_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Floating_Point_Def_F_Num_Digits
,
      Index => 1);
   Floating_Point_Def_F_Range_For_Floating_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Floating_Point_Def_F_Range
,
      Index => 2);

   Desc_For_Floating_Point_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Real_Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("FloatingPointDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Floating_Point_Def_F_Num_Digits_For_Floating_Point_Def'Access,
         2 => Floating_Point_Def_F_Range_For_Floating_Point_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Floating_Point_Def);

   Ordinary_Fixed_Point_Def_F_Delta_For_Ordinary_Fixed_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Ordinary_Fixed_Point_Def_F_Delta
,
      Index => 1);
   Ordinary_Fixed_Point_Def_F_Range_For_Ordinary_Fixed_Point_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Ordinary_Fixed_Point_Def_F_Range
,
      Index => 2);

   Desc_For_Ordinary_Fixed_Point_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Real_Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("OrdinaryFixedPointDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Ordinary_Fixed_Point_Def_F_Delta_For_Ordinary_Fixed_Point_Def'
             Access,
         2 =>
           Ordinary_Fixed_Point_Def_F_Range_For_Ordinary_Fixed_Point_Def'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Ordinary_Fixed_Point_Def);

   Record_Type_Def_F_Has_Abstract_For_Record_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Type_Def_F_Has_Abstract
,
      Index => 1);
   Record_Type_Def_F_Has_Tagged_For_Record_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Type_Def_F_Has_Tagged
,
      Index => 2);
   Record_Type_Def_F_Has_Limited_For_Record_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Type_Def_F_Has_Limited
,
      Index => 3);
   Record_Type_Def_F_Record_Def_For_Record_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Record_Type_Def_F_Record_Def
,
      Index => 4);

   Desc_For_Record_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 4,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("RecordTypeDef"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Record_Type_Def_F_Has_Abstract_For_Record_Type_Def'Access,
         2 => Record_Type_Def_F_Has_Tagged_For_Record_Type_Def'Access,
         3 => Record_Type_Def_F_Has_Limited_For_Record_Type_Def'Access,
         4 => Record_Type_Def_F_Record_Def_For_Record_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Record_Type_Def);

   Signed_Int_Type_Def_F_Range_For_Signed_Int_Type_Def : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Signed_Int_Type_Def_F_Range
,
      Index => 1);

   Desc_For_Signed_Int_Type_Def : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Type_Def_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SignedIntTypeDef"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Signed_Int_Type_Def_F_Range_For_Signed_Int_Type_Def'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Signed_Int_Type_Def);

   Desc_For_Type_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 3, Fields_Count => 0,
      Properties_Count => 3,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Enum_Lit_Synth_Type_Expr_Type_Id,
         2 => Common.Anonymous_Type_Type_Id,
         3 => Common.Subtype_Indication_Type_Id),

      DSL_Name   => To_Unbounded_String ("TypeExpr"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),

      Properties =>
        (1 => Type_Expr_P_Type_Name, 2 => Type_Expr_P_Designated_Type_Decl,
         3 => Type_Expr_P_Designated_Type_Decl_From)
);

   Anonymous_Type_F_Type_Decl_For_Anonymous_Type : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Anonymous_Type_F_Type_Decl
,
      Index => 1);

   Desc_For_Anonymous_Type : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Type_Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("AnonymousType"),
Inherited_Fields => 0,
      Fields => (1 => Anonymous_Type_F_Type_Decl_For_Anonymous_Type'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Anonymous_Type);

   Desc_For_Enum_Lit_Synth_Type_Expr : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Type_Expr_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EnumLitSynthTypeExpr"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Enum_Lit_Synth_Type_Expr);

   Subtype_Indication_F_Has_Not_Null_For_Subtype_Indication : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subtype_Indication_F_Has_Not_Null
,
      Index => 1);
   Subtype_Indication_F_Name_For_Subtype_Indication : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subtype_Indication_F_Name
,
      Index => 2);
   Subtype_Indication_F_Constraint_For_Subtype_Indication : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Subtype_Indication_F_Constraint
,
      Index => 3);

   Desc_For_Subtype_Indication : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 2, Fields_Count => 3,
      Properties_Count => 1,
Base_Type              => Common.Type_Expr_Type_Id,
      Derivations      =>
        (1 => Common.Constrained_Subtype_Indication_Type_Id,
         2 => Common.Discrete_Subtype_Indication_Type_Id),

      DSL_Name => To_Unbounded_String ("SubtypeIndication"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Subtype_Indication_F_Has_Not_Null_For_Subtype_Indication'Access,
         2 => Subtype_Indication_F_Name_For_Subtype_Indication'Access,
         3 => Subtype_Indication_F_Constraint_For_Subtype_Indication'Access),

      Properties => (1 => Subtype_Indication_P_Is_Static_Subtype)
,
      Kind => Ada_Subtype_Indication);

   Desc_For_Constrained_Subtype_Indication : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Subtype_Indication_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ConstrainedSubtypeIndication"),

      Inherited_Fields => 3, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
,
      Kind => Ada_Constrained_Subtype_Indication);

   Desc_For_Discrete_Subtype_Indication : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Subtype_Indication_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DiscreteSubtypeIndication"),

      Inherited_Fields => 3, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Discrete_Subtype_Indication);

   Unconstrained_Array_Index_F_Subtype_Indication_For_Unconstrained_Array_Index : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False,
      Field               => Unconstrained_Array_Index_F_Subtype_Indication
, Index                   => 1);

   Desc_For_Unconstrained_Array_Index : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("UnconstrainedArrayIndex"),

      Inherited_Fields => 0,
      Fields           =>
        (1 =>
           Unconstrained_Array_Index_F_Subtype_Indication_For_Unconstrained_Array_Index'
             Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Unconstrained_Array_Index);

   Desc_For_Until_Node : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Until_Present_Type_Id, 2 => Common.Until_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("Until"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => Until_Node_P_As_Bool)
);

   Desc_For_Until_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Until_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Until.Absent"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Until_Absent);

   Desc_For_Until_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Until_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Until.Present"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
,
      Kind => Ada_Until_Present);

   Desc_For_Use_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.Use_Package_Clause_Type_Id,
         2 => Common.Use_Type_Clause_Type_Id),

      DSL_Name   => To_Unbounded_String ("UseClause"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 .. 0 => <>)
);

   Use_Package_Clause_F_Packages_For_Use_Package_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Use_Package_Clause_F_Packages
,
      Index => 1);

   Desc_For_Use_Package_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 1,
      Properties_Count => 0,
Base_Type              => Common.Use_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("UsePackageClause"),

      Inherited_Fields => 0,
      Fields           =>
        (1 => Use_Package_Clause_F_Packages_For_Use_Package_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Use_Package_Clause);

   Use_Type_Clause_F_Has_All_For_Use_Type_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Use_Type_Clause_F_Has_All
,
      Index => 1);
   Use_Type_Clause_F_Types_For_Use_Type_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Use_Type_Clause_F_Types
,
      Index => 2);

   Desc_For_Use_Type_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Use_Clause_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("UseTypeClause"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Use_Type_Clause_F_Has_All_For_Use_Type_Clause'Access,
         2 => Use_Type_Clause_F_Types_For_Use_Type_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Use_Type_Clause);

   Variant_F_Choices_For_Variant : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Variant_F_Choices
, Index                   => 1);
   Variant_F_Components_For_Variant : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Variant_F_Components
, Index                   => 2);

   Desc_For_Variant : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("Variant"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Variant_F_Choices_For_Variant'Access,
         2 => Variant_F_Components_For_Variant'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Variant);

   Variant_Part_F_Discr_Name_For_Variant_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Variant_Part_F_Discr_Name
,
      Index => 1);
   Variant_Part_F_Variant_For_Variant_Part : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => Variant_Part_F_Variant
,
      Index => 2);

   Desc_For_Variant_Part : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 2,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("VariantPart"),
Inherited_Fields => 0,
      Fields     =>
        (1 => Variant_Part_F_Discr_Name_For_Variant_Part'Access,
         2 => Variant_Part_F_Variant_For_Variant_Part'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_Variant_Part);

   With_Clause_F_Has_Limited_For_With_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => With_Clause_F_Has_Limited
,
      Index => 1);
   With_Clause_F_Has_Private_For_With_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => With_Clause_F_Has_Private
,
      Index => 2);
   With_Clause_F_Packages_For_With_Clause : aliased constant Node_Field_Descriptor :=
     (Is_Abstract_Or_Null => False, Field => With_Clause_F_Packages
,
      Index => 3);

   Desc_For_With_Clause : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 3,
      Properties_Count => 0,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name   => To_Unbounded_String ("WithClause"),
Inherited_Fields => 0,
      Fields     =>
        (1 => With_Clause_F_Has_Limited_For_With_Clause'Access,
         2 => With_Clause_F_Has_Private_For_With_Clause'Access,
         3 => With_Clause_F_Packages_For_With_Clause'Access),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_With_Clause);

   Desc_For_With_Private : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => True, Derivations_Count => 2, Fields_Count => 0,
      Properties_Count => 1,
Base_Type              => Common.Ada_Node_Type_Id,
      Derivations      =>
        (1 => Common.With_Private_Present_Type_Id,
         2 => Common.With_Private_Absent_Type_Id),

      DSL_Name   => To_Unbounded_String ("WithPrivate"),
Inherited_Fields => 0,
      Fields     => (1 .. 0 => <>),
Properties       => (1 => With_Private_P_As_Bool)
);

   Desc_For_With_Private_Absent : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.With_Private_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("WithPrivate.Absent"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_With_Private_Absent);

   Desc_For_With_Private_Present : aliased constant Node_Type_Descriptor :=
     (Is_Abstract      => False, Derivations_Count => 0, Fields_Count => 0,
      Properties_Count => 0,
Base_Type              => Common.With_Private_Type_Id,
      Derivations      => (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("WithPrivate.Present"),

      Inherited_Fields => 0, Fields => (1 .. 0 => <>),

      Properties => (1 .. 0 => <>)
, Kind           => Ada_With_Private_Present);

   Node_Type_Descriptors : constant array
     (Node_Type_Id) of Node_Type_Descriptor_Access :=
     (Desc_For_Ada_Node'Access, Desc_For_Abort_Node'Access,
      Desc_For_Abort_Absent'Access, Desc_For_Abort_Present'Access,
      Desc_For_Abstract_Node'Access, Desc_For_Abstract_Absent'Access,
      Desc_For_Abstract_Present'Access, Desc_For_Ada_List'Access,
      Desc_For_Ada_Node_List'Access, Desc_For_Alternatives_List'Access,
      Desc_For_Constraint_List'Access, Desc_For_Decl_List'Access,
      Desc_For_Stmt_List'Access, Desc_For_Aspect_Assoc_List'Access,
      Desc_For_Base_Assoc_List'Access, Desc_For_Basic_Assoc_List'Access,
      Desc_For_Assoc_List'Access, Desc_For_Case_Expr_Alternative_List'Access,
      Desc_For_Case_Stmt_Alternative_List'Access,
      Desc_For_Compilation_Unit_List'Access,
      Desc_For_Contract_Case_Assoc_List'Access,
      Desc_For_Defining_Name_List'Access,
      Desc_For_Discriminant_Spec_List'Access,
      Desc_For_Elsif_Expr_Part_List'Access,
      Desc_For_Elsif_Stmt_Part_List'Access,
      Desc_For_Enum_Literal_Decl_List'Access, Desc_For_Expr_List'Access,
      Desc_For_Expr_Alternatives_List'Access, Desc_For_Identifier_List'Access,
      Desc_For_Discriminant_Choice_List'Access, Desc_For_Name_List'Access,
      Desc_For_Parent_List'Access, Desc_For_Param_Spec_List'Access,
      Desc_For_Pragma_Node_List'Access, Desc_For_Select_When_Part_List'Access,
      Desc_For_Unconstrained_Array_Index_List'Access,
      Desc_For_Variant_List'Access, Desc_For_Aliased_Node'Access,
      Desc_For_Aliased_Absent'Access, Desc_For_Aliased_Present'Access,
      Desc_For_All_Node'Access, Desc_For_All_Absent'Access,
      Desc_For_All_Present'Access, Desc_For_Array_Indices'Access,
      Desc_For_Constrained_Array_Indices'Access,
      Desc_For_Unconstrained_Array_Indices'Access,
      Desc_For_Aspect_Assoc'Access, Desc_For_Aspect_Clause'Access,
      Desc_For_At_Clause'Access, Desc_For_Attribute_Def_Clause'Access,
      Desc_For_Enum_Rep_Clause'Access, Desc_For_Record_Rep_Clause'Access,
      Desc_For_Aspect_Spec'Access, Desc_For_Base_Assoc'Access,
      Desc_For_Contract_Case_Assoc'Access,
      Desc_For_Pragma_Argument_Assoc'Access,
      Desc_For_Base_Formal_Param_Holder'Access, Desc_For_Base_Subp_Spec'Access,
      Desc_For_Entry_Spec'Access, Desc_For_Enum_Subp_Spec'Access,
      Desc_For_Subp_Spec'Access, Desc_For_Component_List'Access,
      Desc_For_Discriminant_Part'Access,
      Desc_For_Known_Discriminant_Part'Access,
      Desc_For_Unknown_Discriminant_Part'Access,
      Desc_For_Entry_Completion_Formal_Params'Access,
      Desc_For_Generic_Formal_Part'Access, Desc_For_Base_Record_Def'Access,
      Desc_For_Null_Record_Def'Access, Desc_For_Record_Def'Access,
      Desc_For_Basic_Assoc'Access, Desc_For_Aggregate_Assoc'Access,
      Desc_For_Multi_Dim_Array_Assoc'Access,
      Desc_For_Discriminant_Assoc'Access, Desc_For_Param_Assoc'Access,
      Desc_For_Basic_Decl'Access, Desc_For_Base_Formal_Param_Decl'Access,
      Desc_For_Component_Decl'Access, Desc_For_Discriminant_Spec'Access,
      Desc_For_Generic_Formal'Access, Desc_For_Generic_Formal_Obj_Decl'Access,
      Desc_For_Generic_Formal_Package'Access,
      Desc_For_Generic_Formal_Subp_Decl'Access,
      Desc_For_Generic_Formal_Type_Decl'Access, Desc_For_Param_Spec'Access,
      Desc_For_Base_Package_Decl'Access,
      Desc_For_Generic_Package_Internal'Access, Desc_For_Package_Decl'Access,
      Desc_For_Base_Type_Decl'Access, Desc_For_Base_Subtype_Decl'Access,
      Desc_For_Discrete_Base_Subtype_Decl'Access, Desc_For_Subtype_Decl'Access,
      Desc_For_Classwide_Type_Decl'Access,
      Desc_For_Incomplete_Type_Decl'Access,
      Desc_For_Incomplete_Tagged_Type_Decl'Access,
      Desc_For_Protected_Type_Decl'Access, Desc_For_Task_Type_Decl'Access,
      Desc_For_Single_Task_Type_Decl'Access, Desc_For_Type_Decl'Access,
      Desc_For_Anonymous_Type_Decl'Access,
      Desc_For_Synth_Anonymous_Type_Decl'Access,
      Desc_For_Basic_Subp_Decl'Access, Desc_For_Classic_Subp_Decl'Access,
      Desc_For_Abstract_Subp_Decl'Access, Desc_For_Formal_Subp_Decl'Access,
      Desc_For_Abstract_Formal_Subp_Decl'Access,
      Desc_For_Concrete_Formal_Subp_Decl'Access, Desc_For_Subp_Decl'Access,
      Desc_For_Entry_Decl'Access, Desc_For_Enum_Literal_Decl'Access,
      Desc_For_Generic_Subp_Internal'Access, Desc_For_Body_Node'Access,
      Desc_For_Base_Subp_Body'Access, Desc_For_Expr_Function'Access,
      Desc_For_Null_Subp_Decl'Access, Desc_For_Subp_Body'Access,
      Desc_For_Subp_Renaming_Decl'Access, Desc_For_Body_Stub'Access,
      Desc_For_Package_Body_Stub'Access, Desc_For_Protected_Body_Stub'Access,
      Desc_For_Subp_Body_Stub'Access, Desc_For_Task_Body_Stub'Access,
      Desc_For_Entry_Body'Access, Desc_For_Package_Body'Access,
      Desc_For_Protected_Body'Access, Desc_For_Task_Body'Access,
      Desc_For_Entry_Index_Spec'Access, Desc_For_Error_Decl'Access,
      Desc_For_Exception_Decl'Access, Desc_For_Exception_Handler'Access,
      Desc_For_For_Loop_Var_Decl'Access, Desc_For_Generic_Decl'Access,
      Desc_For_Generic_Package_Decl'Access, Desc_For_Generic_Subp_Decl'Access,
      Desc_For_Generic_Instantiation'Access,
      Desc_For_Generic_Package_Instantiation'Access,
      Desc_For_Generic_Subp_Instantiation'Access,
      Desc_For_Generic_Renaming_Decl'Access,
      Desc_For_Generic_Package_Renaming_Decl'Access,
      Desc_For_Generic_Subp_Renaming_Decl'Access, Desc_For_Label_Decl'Access,
      Desc_For_Named_Stmt_Decl'Access, Desc_For_Number_Decl'Access,
      Desc_For_Object_Decl'Access, Desc_For_Anonymous_Object_Decl'Access,
      Desc_For_Extended_Return_Stmt_Object_Decl'Access,
      Desc_For_Package_Renaming_Decl'Access,
      Desc_For_Single_Protected_Decl'Access, Desc_For_Single_Task_Decl'Access,
      Desc_For_Case_Stmt_Alternative'Access, Desc_For_Compilation_Unit'Access,
      Desc_For_Component_Clause'Access, Desc_For_Component_Def'Access,
      Desc_For_Constant_Node'Access, Desc_For_Constant_Absent'Access,
      Desc_For_Constant_Present'Access, Desc_For_Constraint'Access,
      Desc_For_Delta_Constraint'Access, Desc_For_Digits_Constraint'Access,
      Desc_For_Discriminant_Constraint'Access,
      Desc_For_Index_Constraint'Access, Desc_For_Range_Constraint'Access,
      Desc_For_Declarative_Part'Access, Desc_For_Private_Part'Access,
      Desc_For_Public_Part'Access, Desc_For_Elsif_Expr_Part'Access,
      Desc_For_Elsif_Stmt_Part'Access, Desc_For_Expr'Access,
      Desc_For_Allocator'Access, Desc_For_Base_Aggregate'Access,
      Desc_For_Aggregate'Access, Desc_For_Null_Record_Aggregate'Access,
      Desc_For_Bin_Op'Access, Desc_For_Relation_Op'Access,
      Desc_For_Box_Expr'Access, Desc_For_Case_Expr'Access,
      Desc_For_Case_Expr_Alternative'Access, Desc_For_Contract_Cases'Access,
      Desc_For_If_Expr'Access, Desc_For_Membership_Expr'Access,
      Desc_For_Name'Access, Desc_For_Attribute_Ref'Access,
      Desc_For_Update_Attribute_Ref'Access, Desc_For_Call_Expr'Access,
      Desc_For_Defining_Name'Access, Desc_For_Discrete_Subtype_Name'Access,
      Desc_For_Dotted_Name'Access, Desc_For_End_Name'Access,
      Desc_For_Explicit_Deref'Access, Desc_For_Qual_Expr'Access,
      Desc_For_Single_Tok_Node'Access, Desc_For_Base_Id'Access,
      Desc_For_Char_Literal'Access, Desc_For_Identifier'Access,
      Desc_For_Op'Access, Desc_For_Op_Abs'Access, Desc_For_Op_And'Access,
      Desc_For_Op_And_Then'Access, Desc_For_Op_Concat'Access,
      Desc_For_Op_Div'Access, Desc_For_Op_Double_Dot'Access,
      Desc_For_Op_Eq'Access, Desc_For_Op_Gt'Access, Desc_For_Op_Gte'Access,
      Desc_For_Op_In'Access, Desc_For_Op_Lt'Access, Desc_For_Op_Lte'Access,
      Desc_For_Op_Minus'Access, Desc_For_Op_Mod'Access,
      Desc_For_Op_Mult'Access, Desc_For_Op_Neq'Access, Desc_For_Op_Not'Access,
      Desc_For_Op_Not_In'Access, Desc_For_Op_Or'Access,
      Desc_For_Op_Or_Else'Access, Desc_For_Op_Plus'Access,
      Desc_For_Op_Pow'Access, Desc_For_Op_Rem'Access, Desc_For_Op_Xor'Access,
      Desc_For_String_Literal'Access, Desc_For_Null_Literal'Access,
      Desc_For_Num_Literal'Access, Desc_For_Int_Literal'Access,
      Desc_For_Real_Literal'Access, Desc_For_Target_Name'Access,
      Desc_For_Paren_Expr'Access, Desc_For_Quantified_Expr'Access,
      Desc_For_Raise_Expr'Access, Desc_For_Un_Op'Access,
      Desc_For_Handled_Stmts'Access, Desc_For_Interface_Kind'Access,
      Desc_For_Interface_Kind_Limited'Access,
      Desc_For_Interface_Kind_Protected'Access,
      Desc_For_Interface_Kind_Synchronized'Access,
      Desc_For_Interface_Kind_Task'Access, Desc_For_Iter_Type'Access,
      Desc_For_Iter_Type_In'Access, Desc_For_Iter_Type_Of'Access,
      Desc_For_Library_Item'Access, Desc_For_Limited_Node'Access,
      Desc_For_Limited_Absent'Access, Desc_For_Limited_Present'Access,
      Desc_For_Loop_Spec'Access, Desc_For_For_Loop_Spec'Access,
      Desc_For_While_Loop_Spec'Access, Desc_For_Mode'Access,
      Desc_For_Mode_Default'Access, Desc_For_Mode_In'Access,
      Desc_For_Mode_In_Out'Access, Desc_For_Mode_Out'Access,
      Desc_For_Not_Null'Access, Desc_For_Not_Null_Absent'Access,
      Desc_For_Not_Null_Present'Access, Desc_For_Null_Component_Decl'Access,
      Desc_For_Others_Designator'Access, Desc_For_Overriding_Node'Access,
      Desc_For_Overriding_Not_Overriding'Access,
      Desc_For_Overriding_Overriding'Access,
      Desc_For_Overriding_Unspecified'Access, Desc_For_Params'Access,
      Desc_For_Pragma_Node'Access, Desc_For_Prim_Type_Accessor'Access,
      Desc_For_Private_Node'Access, Desc_For_Private_Absent'Access,
      Desc_For_Private_Present'Access, Desc_For_Protected_Def'Access,
      Desc_For_Protected_Node'Access, Desc_For_Protected_Absent'Access,
      Desc_For_Protected_Present'Access, Desc_For_Quantifier'Access,
      Desc_For_Quantifier_All'Access, Desc_For_Quantifier_Some'Access,
      Desc_For_Range_Spec'Access, Desc_For_Renaming_Clause'Access,
      Desc_For_Synthetic_Renaming_Clause'Access, Desc_For_Reverse_Node'Access,
      Desc_For_Reverse_Absent'Access, Desc_For_Reverse_Present'Access,
      Desc_For_Select_When_Part'Access, Desc_For_Stmt'Access,
      Desc_For_Composite_Stmt'Access, Desc_For_Accept_Stmt'Access,
      Desc_For_Accept_Stmt_With_Stmts'Access, Desc_For_Base_Loop_Stmt'Access,
      Desc_For_For_Loop_Stmt'Access, Desc_For_Loop_Stmt'Access,
      Desc_For_While_Loop_Stmt'Access, Desc_For_Block_Stmt'Access,
      Desc_For_Begin_Block'Access, Desc_For_Decl_Block'Access,
      Desc_For_Case_Stmt'Access, Desc_For_Extended_Return_Stmt'Access,
      Desc_For_If_Stmt'Access, Desc_For_Named_Stmt'Access,
      Desc_For_Select_Stmt'Access, Desc_For_Error_Stmt'Access,
      Desc_For_Simple_Stmt'Access, Desc_For_Abort_Stmt'Access,
      Desc_For_Assign_Stmt'Access, Desc_For_Call_Stmt'Access,
      Desc_For_Delay_Stmt'Access, Desc_For_Exit_Stmt'Access,
      Desc_For_Goto_Stmt'Access, Desc_For_Label'Access,
      Desc_For_Null_Stmt'Access, Desc_For_Raise_Stmt'Access,
      Desc_For_Requeue_Stmt'Access, Desc_For_Return_Stmt'Access,
      Desc_For_Terminate_Alternative'Access, Desc_For_Subp_Kind'Access,
      Desc_For_Subp_Kind_Function'Access, Desc_For_Subp_Kind_Procedure'Access,
      Desc_For_Subunit'Access, Desc_For_Synchronized_Node'Access,
      Desc_For_Synchronized_Absent'Access,
      Desc_For_Synchronized_Present'Access, Desc_For_Tagged_Node'Access,
      Desc_For_Tagged_Absent'Access, Desc_For_Tagged_Present'Access,
      Desc_For_Task_Def'Access, Desc_For_Type_Def'Access,
      Desc_For_Access_Def'Access, Desc_For_Access_To_Subp_Def'Access,
      Desc_For_Base_Type_Access_Def'Access,
      Desc_For_Anonymous_Type_Access_Def'Access,
      Desc_For_Type_Access_Def'Access, Desc_For_Array_Type_Def'Access,
      Desc_For_Derived_Type_Def'Access, Desc_For_Enum_Type_Def'Access,
      Desc_For_Formal_Discrete_Type_Def'Access,
      Desc_For_Interface_Type_Def'Access, Desc_For_Mod_Int_Type_Def'Access,
      Desc_For_Private_Type_Def'Access, Desc_For_Real_Type_Def'Access,
      Desc_For_Decimal_Fixed_Point_Def'Access,
      Desc_For_Floating_Point_Def'Access,
      Desc_For_Ordinary_Fixed_Point_Def'Access,
      Desc_For_Record_Type_Def'Access, Desc_For_Signed_Int_Type_Def'Access,
      Desc_For_Type_Expr'Access, Desc_For_Anonymous_Type'Access,
      Desc_For_Enum_Lit_Synth_Type_Expr'Access,
      Desc_For_Subtype_Indication'Access,
      Desc_For_Constrained_Subtype_Indication'Access,
      Desc_For_Discrete_Subtype_Indication'Access,
      Desc_For_Unconstrained_Array_Index'Access, Desc_For_Until_Node'Access,
      Desc_For_Until_Absent'Access, Desc_For_Until_Present'Access,
      Desc_For_Use_Clause'Access, Desc_For_Use_Package_Clause'Access,
      Desc_For_Use_Type_Clause'Access, Desc_For_Variant'Access,
      Desc_For_Variant_Part'Access, Desc_For_With_Clause'Access,
      Desc_For_With_Private'Access, Desc_For_With_Private_Absent'Access,
      Desc_For_With_Private_Present'Access);

   ----------------------
   -- Various mappings --
   ----------------------

   package Node_Type_Id_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String, Element_Type => Node_Type_Id,
      Equivalent_Keys => "=", Hash => Hash);

   DSL_Name_To_Node_Type : Node_Type_Id_Maps.Map;
   --  Lookup table for DSL names to node type references. Created at
   --  elaboration time and never updated after.

   Kind_To_Id : constant array (Ada_Node_Kind_Type) of Node_Type_Id :=
     (Ada_Abort_Absent               => Common.Abort_Absent_Type_Id,
      Ada_Abort_Present              => Common.Abort_Present_Type_Id,
      Ada_Abstract_Absent            => Common.Abstract_Absent_Type_Id,
      Ada_Abstract_Present           => Common.Abstract_Present_Type_Id,
      Ada_Ada_Node_List              => Common.Ada_Node_List_Type_Id,
      Ada_Alternatives_List          => Common.Alternatives_List_Type_Id,
      Ada_Constraint_List            => Common.Constraint_List_Type_Id,
      Ada_Decl_List                  => Common.Decl_List_Type_Id,
      Ada_Stmt_List                  => Common.Stmt_List_Type_Id,
      Ada_Aspect_Assoc_List          => Common.Aspect_Assoc_List_Type_Id,
      Ada_Base_Assoc_List            => Common.Base_Assoc_List_Type_Id,
      Ada_Assoc_List                 => Common.Assoc_List_Type_Id,
      Ada_Case_Expr_Alternative_List =>
        Common.Case_Expr_Alternative_List_Type_Id,
      Ada_Case_Stmt_Alternative_List =>
        Common.Case_Stmt_Alternative_List_Type_Id,
      Ada_Compilation_Unit_List => Common.Compilation_Unit_List_Type_Id,
      Ada_Contract_Case_Assoc_List => Common.Contract_Case_Assoc_List_Type_Id,
      Ada_Defining_Name_List             => Common.Defining_Name_List_Type_Id,
      Ada_Discriminant_Spec_List => Common.Discriminant_Spec_List_Type_Id,
      Ada_Elsif_Expr_Part_List => Common.Elsif_Expr_Part_List_Type_Id,
      Ada_Elsif_Stmt_Part_List => Common.Elsif_Stmt_Part_List_Type_Id,
      Ada_Enum_Literal_Decl_List => Common.Enum_Literal_Decl_List_Type_Id,
      Ada_Expr_Alternatives_List => Common.Expr_Alternatives_List_Type_Id,
      Ada_Discriminant_Choice_List => Common.Discriminant_Choice_List_Type_Id,
      Ada_Name_List                      => Common.Name_List_Type_Id,
      Ada_Parent_List                    => Common.Parent_List_Type_Id,
      Ada_Param_Spec_List                => Common.Param_Spec_List_Type_Id,
      Ada_Pragma_Node_List               => Common.Pragma_Node_List_Type_Id,
      Ada_Select_When_Part_List => Common.Select_When_Part_List_Type_Id,
      Ada_Unconstrained_Array_Index_List =>
        Common.Unconstrained_Array_Index_List_Type_Id,
      Ada_Variant_List              => Common.Variant_List_Type_Id,
      Ada_Aliased_Absent            => Common.Aliased_Absent_Type_Id,
      Ada_Aliased_Present           => Common.Aliased_Present_Type_Id,
      Ada_All_Absent                => Common.All_Absent_Type_Id,
      Ada_All_Present               => Common.All_Present_Type_Id,
      Ada_Constrained_Array_Indices =>
        Common.Constrained_Array_Indices_Type_Id,
      Ada_Unconstrained_Array_Indices =>
        Common.Unconstrained_Array_Indices_Type_Id,
      Ada_Aspect_Assoc              => Common.Aspect_Assoc_Type_Id,
      Ada_At_Clause                 => Common.At_Clause_Type_Id,
      Ada_Attribute_Def_Clause      => Common.Attribute_Def_Clause_Type_Id,
      Ada_Enum_Rep_Clause           => Common.Enum_Rep_Clause_Type_Id,
      Ada_Record_Rep_Clause         => Common.Record_Rep_Clause_Type_Id,
      Ada_Aspect_Spec               => Common.Aspect_Spec_Type_Id,
      Ada_Contract_Case_Assoc       => Common.Contract_Case_Assoc_Type_Id,
      Ada_Pragma_Argument_Assoc     => Common.Pragma_Argument_Assoc_Type_Id,
      Ada_Entry_Spec                => Common.Entry_Spec_Type_Id,
      Ada_Enum_Subp_Spec            => Common.Enum_Subp_Spec_Type_Id,
      Ada_Subp_Spec                 => Common.Subp_Spec_Type_Id,
      Ada_Component_List            => Common.Component_List_Type_Id,
      Ada_Known_Discriminant_Part   => Common.Known_Discriminant_Part_Type_Id,
      Ada_Unknown_Discriminant_Part =>
        Common.Unknown_Discriminant_Part_Type_Id,
      Ada_Entry_Completion_Formal_Params =>
        Common.Entry_Completion_Formal_Params_Type_Id,
      Ada_Generic_Formal_Part        => Common.Generic_Formal_Part_Type_Id,
      Ada_Null_Record_Def            => Common.Null_Record_Def_Type_Id,
      Ada_Record_Def                 => Common.Record_Def_Type_Id,
      Ada_Aggregate_Assoc            => Common.Aggregate_Assoc_Type_Id,
      Ada_Multi_Dim_Array_Assoc      => Common.Multi_Dim_Array_Assoc_Type_Id,
      Ada_Discriminant_Assoc         => Common.Discriminant_Assoc_Type_Id,
      Ada_Param_Assoc                => Common.Param_Assoc_Type_Id,
      Ada_Component_Decl             => Common.Component_Decl_Type_Id,
      Ada_Discriminant_Spec          => Common.Discriminant_Spec_Type_Id,
      Ada_Generic_Formal_Obj_Decl    => Common.Generic_Formal_Obj_Decl_Type_Id,
      Ada_Generic_Formal_Package     => Common.Generic_Formal_Package_Type_Id,
      Ada_Generic_Formal_Subp_Decl => Common.Generic_Formal_Subp_Decl_Type_Id,
      Ada_Generic_Formal_Type_Decl => Common.Generic_Formal_Type_Decl_Type_Id,
      Ada_Param_Spec                 => Common.Param_Spec_Type_Id,
      Ada_Generic_Package_Internal => Common.Generic_Package_Internal_Type_Id,
      Ada_Package_Decl               => Common.Package_Decl_Type_Id,
      Ada_Discrete_Base_Subtype_Decl =>
        Common.Discrete_Base_Subtype_Decl_Type_Id,
      Ada_Subtype_Decl                => Common.Subtype_Decl_Type_Id,
      Ada_Classwide_Type_Decl         => Common.Classwide_Type_Decl_Type_Id,
      Ada_Incomplete_Type_Decl        => Common.Incomplete_Type_Decl_Type_Id,
      Ada_Incomplete_Tagged_Type_Decl =>
        Common.Incomplete_Tagged_Type_Decl_Type_Id,
      Ada_Protected_Type_Decl       => Common.Protected_Type_Decl_Type_Id,
      Ada_Task_Type_Decl            => Common.Task_Type_Decl_Type_Id,
      Ada_Single_Task_Type_Decl     => Common.Single_Task_Type_Decl_Type_Id,
      Ada_Type_Decl                 => Common.Type_Decl_Type_Id,
      Ada_Anonymous_Type_Decl       => Common.Anonymous_Type_Decl_Type_Id,
      Ada_Synth_Anonymous_Type_Decl =>
        Common.Synth_Anonymous_Type_Decl_Type_Id,
      Ada_Abstract_Subp_Decl        => Common.Abstract_Subp_Decl_Type_Id,
      Ada_Abstract_Formal_Subp_Decl =>
        Common.Abstract_Formal_Subp_Decl_Type_Id,
      Ada_Concrete_Formal_Subp_Decl =>
        Common.Concrete_Formal_Subp_Decl_Type_Id,
      Ada_Subp_Decl                     => Common.Subp_Decl_Type_Id,
      Ada_Entry_Decl                    => Common.Entry_Decl_Type_Id,
      Ada_Enum_Literal_Decl             => Common.Enum_Literal_Decl_Type_Id,
      Ada_Generic_Subp_Internal => Common.Generic_Subp_Internal_Type_Id,
      Ada_Expr_Function                 => Common.Expr_Function_Type_Id,
      Ada_Null_Subp_Decl                => Common.Null_Subp_Decl_Type_Id,
      Ada_Subp_Body                     => Common.Subp_Body_Type_Id,
      Ada_Subp_Renaming_Decl            => Common.Subp_Renaming_Decl_Type_Id,
      Ada_Package_Body_Stub             => Common.Package_Body_Stub_Type_Id,
      Ada_Protected_Body_Stub           => Common.Protected_Body_Stub_Type_Id,
      Ada_Subp_Body_Stub                => Common.Subp_Body_Stub_Type_Id,
      Ada_Task_Body_Stub                => Common.Task_Body_Stub_Type_Id,
      Ada_Entry_Body                    => Common.Entry_Body_Type_Id,
      Ada_Package_Body                  => Common.Package_Body_Type_Id,
      Ada_Protected_Body                => Common.Protected_Body_Type_Id,
      Ada_Task_Body                     => Common.Task_Body_Type_Id,
      Ada_Entry_Index_Spec              => Common.Entry_Index_Spec_Type_Id,
      Ada_Error_Decl                    => Common.Error_Decl_Type_Id,
      Ada_Exception_Decl                => Common.Exception_Decl_Type_Id,
      Ada_Exception_Handler             => Common.Exception_Handler_Type_Id,
      Ada_For_Loop_Var_Decl             => Common.For_Loop_Var_Decl_Type_Id,
      Ada_Generic_Package_Decl          => Common.Generic_Package_Decl_Type_Id,
      Ada_Generic_Subp_Decl             => Common.Generic_Subp_Decl_Type_Id,
      Ada_Generic_Package_Instantiation =>
        Common.Generic_Package_Instantiation_Type_Id,
      Ada_Generic_Subp_Instantiation =>
        Common.Generic_Subp_Instantiation_Type_Id,
      Ada_Generic_Package_Renaming_Decl =>
        Common.Generic_Package_Renaming_Decl_Type_Id,
      Ada_Generic_Subp_Renaming_Decl =>
        Common.Generic_Subp_Renaming_Decl_Type_Id,
      Ada_Label_Decl                       => Common.Label_Decl_Type_Id,
      Ada_Named_Stmt_Decl                  => Common.Named_Stmt_Decl_Type_Id,
      Ada_Number_Decl                      => Common.Number_Decl_Type_Id,
      Ada_Object_Decl                      => Common.Object_Decl_Type_Id,
      Ada_Anonymous_Object_Decl => Common.Anonymous_Object_Decl_Type_Id,
      Ada_Extended_Return_Stmt_Object_Decl =>
        Common.Extended_Return_Stmt_Object_Decl_Type_Id,
      Ada_Package_Renaming_Decl       => Common.Package_Renaming_Decl_Type_Id,
      Ada_Single_Protected_Decl       => Common.Single_Protected_Decl_Type_Id,
      Ada_Single_Task_Decl            => Common.Single_Task_Decl_Type_Id,
      Ada_Case_Stmt_Alternative       => Common.Case_Stmt_Alternative_Type_Id,
      Ada_Compilation_Unit            => Common.Compilation_Unit_Type_Id,
      Ada_Component_Clause            => Common.Component_Clause_Type_Id,
      Ada_Component_Def               => Common.Component_Def_Type_Id,
      Ada_Constant_Absent             => Common.Constant_Absent_Type_Id,
      Ada_Constant_Present            => Common.Constant_Present_Type_Id,
      Ada_Delta_Constraint            => Common.Delta_Constraint_Type_Id,
      Ada_Digits_Constraint           => Common.Digits_Constraint_Type_Id,
      Ada_Discriminant_Constraint => Common.Discriminant_Constraint_Type_Id,
      Ada_Index_Constraint            => Common.Index_Constraint_Type_Id,
      Ada_Range_Constraint            => Common.Range_Constraint_Type_Id,
      Ada_Declarative_Part            => Common.Declarative_Part_Type_Id,
      Ada_Private_Part                => Common.Private_Part_Type_Id,
      Ada_Public_Part                 => Common.Public_Part_Type_Id,
      Ada_Elsif_Expr_Part             => Common.Elsif_Expr_Part_Type_Id,
      Ada_Elsif_Stmt_Part             => Common.Elsif_Stmt_Part_Type_Id,
      Ada_Allocator                   => Common.Allocator_Type_Id,
      Ada_Aggregate                   => Common.Aggregate_Type_Id,
      Ada_Null_Record_Aggregate       => Common.Null_Record_Aggregate_Type_Id,
      Ada_Bin_Op                      => Common.Bin_Op_Type_Id,
      Ada_Relation_Op                 => Common.Relation_Op_Type_Id,
      Ada_Box_Expr                    => Common.Box_Expr_Type_Id,
      Ada_Case_Expr                   => Common.Case_Expr_Type_Id,
      Ada_Case_Expr_Alternative       => Common.Case_Expr_Alternative_Type_Id,
      Ada_Contract_Cases              => Common.Contract_Cases_Type_Id,
      Ada_If_Expr                     => Common.If_Expr_Type_Id,
      Ada_Membership_Expr             => Common.Membership_Expr_Type_Id,
      Ada_Attribute_Ref               => Common.Attribute_Ref_Type_Id,
      Ada_Update_Attribute_Ref        => Common.Update_Attribute_Ref_Type_Id,
      Ada_Call_Expr                   => Common.Call_Expr_Type_Id,
      Ada_Defining_Name               => Common.Defining_Name_Type_Id,
      Ada_Discrete_Subtype_Name       => Common.Discrete_Subtype_Name_Type_Id,
      Ada_Dotted_Name                 => Common.Dotted_Name_Type_Id,
      Ada_End_Name                    => Common.End_Name_Type_Id,
      Ada_Explicit_Deref              => Common.Explicit_Deref_Type_Id,
      Ada_Qual_Expr                   => Common.Qual_Expr_Type_Id,
      Ada_Char_Literal                => Common.Char_Literal_Type_Id,
      Ada_Identifier                  => Common.Identifier_Type_Id,
      Ada_Op_Abs => Common.Op_Abs_Type_Id, Ada_Op_And => Common.Op_And_Type_Id,
      Ada_Op_And_Then                 => Common.Op_And_Then_Type_Id,
      Ada_Op_Concat                   => Common.Op_Concat_Type_Id,
      Ada_Op_Div                      => Common.Op_Div_Type_Id,
      Ada_Op_Double_Dot               => Common.Op_Double_Dot_Type_Id,
      Ada_Op_Eq => Common.Op_Eq_Type_Id, Ada_Op_Gt => Common.Op_Gt_Type_Id,
      Ada_Op_Gte => Common.Op_Gte_Type_Id, Ada_Op_In => Common.Op_In_Type_Id,
      Ada_Op_Lt => Common.Op_Lt_Type_Id, Ada_Op_Lte => Common.Op_Lte_Type_Id,
      Ada_Op_Minus                    => Common.Op_Minus_Type_Id,
      Ada_Op_Mod                      => Common.Op_Mod_Type_Id,
      Ada_Op_Mult                     => Common.Op_Mult_Type_Id,
      Ada_Op_Neq => Common.Op_Neq_Type_Id, Ada_Op_Not => Common.Op_Not_Type_Id,
      Ada_Op_Not_In                   => Common.Op_Not_In_Type_Id,
      Ada_Op_Or                       => Common.Op_Or_Type_Id,
      Ada_Op_Or_Else                  => Common.Op_Or_Else_Type_Id,
      Ada_Op_Plus                     => Common.Op_Plus_Type_Id,
      Ada_Op_Pow => Common.Op_Pow_Type_Id, Ada_Op_Rem => Common.Op_Rem_Type_Id,
      Ada_Op_Xor                      => Common.Op_Xor_Type_Id,
      Ada_String_Literal              => Common.String_Literal_Type_Id,
      Ada_Null_Literal                => Common.Null_Literal_Type_Id,
      Ada_Int_Literal                 => Common.Int_Literal_Type_Id,
      Ada_Real_Literal                => Common.Real_Literal_Type_Id,
      Ada_Target_Name                 => Common.Target_Name_Type_Id,
      Ada_Paren_Expr                  => Common.Paren_Expr_Type_Id,
      Ada_Quantified_Expr             => Common.Quantified_Expr_Type_Id,
      Ada_Raise_Expr                  => Common.Raise_Expr_Type_Id,
      Ada_Un_Op                       => Common.Un_Op_Type_Id,
      Ada_Handled_Stmts               => Common.Handled_Stmts_Type_Id,
      Ada_Interface_Kind_Limited      => Common.Interface_Kind_Limited_Type_Id,
      Ada_Interface_Kind_Protected => Common.Interface_Kind_Protected_Type_Id,
      Ada_Interface_Kind_Synchronized =>
        Common.Interface_Kind_Synchronized_Type_Id,
      Ada_Interface_Kind_Task       => Common.Interface_Kind_Task_Type_Id,
      Ada_Iter_Type_In              => Common.Iter_Type_In_Type_Id,
      Ada_Iter_Type_Of              => Common.Iter_Type_Of_Type_Id,
      Ada_Library_Item              => Common.Library_Item_Type_Id,
      Ada_Limited_Absent            => Common.Limited_Absent_Type_Id,
      Ada_Limited_Present           => Common.Limited_Present_Type_Id,
      Ada_For_Loop_Spec             => Common.For_Loop_Spec_Type_Id,
      Ada_While_Loop_Spec           => Common.While_Loop_Spec_Type_Id,
      Ada_Mode_Default              => Common.Mode_Default_Type_Id,
      Ada_Mode_In                   => Common.Mode_In_Type_Id,
      Ada_Mode_In_Out               => Common.Mode_In_Out_Type_Id,
      Ada_Mode_Out                  => Common.Mode_Out_Type_Id,
      Ada_Not_Null_Absent           => Common.Not_Null_Absent_Type_Id,
      Ada_Not_Null_Present          => Common.Not_Null_Present_Type_Id,
      Ada_Null_Component_Decl       => Common.Null_Component_Decl_Type_Id,
      Ada_Others_Designator         => Common.Others_Designator_Type_Id,
      Ada_Overriding_Not_Overriding =>
        Common.Overriding_Not_Overriding_Type_Id,
      Ada_Overriding_Overriding     => Common.Overriding_Overriding_Type_Id,
      Ada_Overriding_Unspecified    => Common.Overriding_Unspecified_Type_Id,
      Ada_Params                    => Common.Params_Type_Id,
      Ada_Pragma_Node               => Common.Pragma_Node_Type_Id,
      Ada_Prim_Type_Accessor        => Common.Prim_Type_Accessor_Type_Id,
      Ada_Private_Absent            => Common.Private_Absent_Type_Id,
      Ada_Private_Present           => Common.Private_Present_Type_Id,
      Ada_Protected_Def             => Common.Protected_Def_Type_Id,
      Ada_Protected_Absent          => Common.Protected_Absent_Type_Id,
      Ada_Protected_Present         => Common.Protected_Present_Type_Id,
      Ada_Quantifier_All            => Common.Quantifier_All_Type_Id,
      Ada_Quantifier_Some           => Common.Quantifier_Some_Type_Id,
      Ada_Range_Spec                => Common.Range_Spec_Type_Id,
      Ada_Renaming_Clause           => Common.Renaming_Clause_Type_Id,
      Ada_Synthetic_Renaming_Clause =>
        Common.Synthetic_Renaming_Clause_Type_Id,
      Ada_Reverse_Absent            => Common.Reverse_Absent_Type_Id,
      Ada_Reverse_Present           => Common.Reverse_Present_Type_Id,
      Ada_Select_When_Part          => Common.Select_When_Part_Type_Id,
      Ada_Accept_Stmt               => Common.Accept_Stmt_Type_Id,
      Ada_Accept_Stmt_With_Stmts    => Common.Accept_Stmt_With_Stmts_Type_Id,
      Ada_For_Loop_Stmt             => Common.For_Loop_Stmt_Type_Id,
      Ada_Loop_Stmt                 => Common.Loop_Stmt_Type_Id,
      Ada_While_Loop_Stmt           => Common.While_Loop_Stmt_Type_Id,
      Ada_Begin_Block               => Common.Begin_Block_Type_Id,
      Ada_Decl_Block                => Common.Decl_Block_Type_Id,
      Ada_Case_Stmt                 => Common.Case_Stmt_Type_Id,
      Ada_Extended_Return_Stmt      => Common.Extended_Return_Stmt_Type_Id,
      Ada_If_Stmt                   => Common.If_Stmt_Type_Id,
      Ada_Named_Stmt                => Common.Named_Stmt_Type_Id,
      Ada_Select_Stmt               => Common.Select_Stmt_Type_Id,
      Ada_Error_Stmt                => Common.Error_Stmt_Type_Id,
      Ada_Abort_Stmt                => Common.Abort_Stmt_Type_Id,
      Ada_Assign_Stmt               => Common.Assign_Stmt_Type_Id,
      Ada_Call_Stmt                 => Common.Call_Stmt_Type_Id,
      Ada_Delay_Stmt                => Common.Delay_Stmt_Type_Id,
      Ada_Exit_Stmt                 => Common.Exit_Stmt_Type_Id,
      Ada_Goto_Stmt                 => Common.Goto_Stmt_Type_Id,
      Ada_Label                     => Common.Label_Type_Id,
      Ada_Null_Stmt                 => Common.Null_Stmt_Type_Id,
      Ada_Raise_Stmt                => Common.Raise_Stmt_Type_Id,
      Ada_Requeue_Stmt              => Common.Requeue_Stmt_Type_Id,
      Ada_Return_Stmt               => Common.Return_Stmt_Type_Id,
      Ada_Terminate_Alternative     => Common.Terminate_Alternative_Type_Id,
      Ada_Subp_Kind_Function        => Common.Subp_Kind_Function_Type_Id,
      Ada_Subp_Kind_Procedure       => Common.Subp_Kind_Procedure_Type_Id,
      Ada_Subunit                   => Common.Subunit_Type_Id,
      Ada_Synchronized_Absent       => Common.Synchronized_Absent_Type_Id,
      Ada_Synchronized_Present      => Common.Synchronized_Present_Type_Id,
      Ada_Tagged_Absent             => Common.Tagged_Absent_Type_Id,
      Ada_Tagged_Present            => Common.Tagged_Present_Type_Id,
      Ada_Task_Def                  => Common.Task_Def_Type_Id,
      Ada_Access_To_Subp_Def        => Common.Access_To_Subp_Def_Type_Id,
      Ada_Anonymous_Type_Access_Def =>
        Common.Anonymous_Type_Access_Def_Type_Id,
      Ada_Type_Access_Def                => Common.Type_Access_Def_Type_Id,
      Ada_Array_Type_Def                 => Common.Array_Type_Def_Type_Id,
      Ada_Derived_Type_Def               => Common.Derived_Type_Def_Type_Id,
      Ada_Enum_Type_Def                  => Common.Enum_Type_Def_Type_Id,
      Ada_Formal_Discrete_Type_Def => Common.Formal_Discrete_Type_Def_Type_Id,
      Ada_Interface_Type_Def             => Common.Interface_Type_Def_Type_Id,
      Ada_Mod_Int_Type_Def               => Common.Mod_Int_Type_Def_Type_Id,
      Ada_Private_Type_Def               => Common.Private_Type_Def_Type_Id,
      Ada_Decimal_Fixed_Point_Def => Common.Decimal_Fixed_Point_Def_Type_Id,
      Ada_Floating_Point_Def             => Common.Floating_Point_Def_Type_Id,
      Ada_Ordinary_Fixed_Point_Def => Common.Ordinary_Fixed_Point_Def_Type_Id,
      Ada_Record_Type_Def                => Common.Record_Type_Def_Type_Id,
      Ada_Signed_Int_Type_Def            => Common.Signed_Int_Type_Def_Type_Id,
      Ada_Anonymous_Type                 => Common.Anonymous_Type_Type_Id,
      Ada_Enum_Lit_Synth_Type_Expr => Common.Enum_Lit_Synth_Type_Expr_Type_Id,
      Ada_Subtype_Indication             => Common.Subtype_Indication_Type_Id,
      Ada_Constrained_Subtype_Indication =>
        Common.Constrained_Subtype_Indication_Type_Id,
      Ada_Discrete_Subtype_Indication =>
        Common.Discrete_Subtype_Indication_Type_Id,
      Ada_Unconstrained_Array_Index =>
        Common.Unconstrained_Array_Index_Type_Id,
      Ada_Until_Absent         => Common.Until_Absent_Type_Id,
      Ada_Until_Present        => Common.Until_Present_Type_Id,
      Ada_Use_Package_Clause   => Common.Use_Package_Clause_Type_Id,
      Ada_Use_Type_Clause      => Common.Use_Type_Clause_Type_Id,
      Ada_Variant              => Common.Variant_Type_Id,
      Ada_Variant_Part         => Common.Variant_Part_Type_Id,
      Ada_With_Clause          => Common.With_Clause_Type_Id,
      Ada_With_Private_Absent  => Common.With_Private_Absent_Type_Id,
      Ada_With_Private_Present => Common.With_Private_Present_Type_Id);

   ----------------
   -- Node types --
   ----------------

   function DSL_Name (Id : Node_Type_Id) return String;
   --  Implementation for Introspection.DSL_Name

   function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id;
   --  Implementation for Introspection.Lookup_DSL_Name

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Abstract

   function Is_Concrete (Id : Node_Type_Id) return Boolean is
     (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type;
   --  Implementation for Introspection.Kind_For

   function Id_For_Kind (Kind : Ada_Node_Kind_Type) return Node_Type_Id;
   --  Implementation for Introspection.Id_For_Kind

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Root_NOde

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  Implementation for Introspection.Base_Type

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Implementation for Introspection.Derived_Types

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Derived_From

   ---------------
   -- Node data --
   ---------------

   function Node_Data_Name (Node_Data : Node_Data_Reference) return String;
   --  Implementation for Introspection.Node_Data_Name

   function Node_Data_Type
     (Node_Data : Node_Data_Reference) return Value_Constraint;
   --  Implementation for Introspection.Node_Data_Type

   function Lookup_Node_Data
     (Id : Node_Type_Id; Name : String) return Any_Node_Data_Reference;
   --  Implementation for Introspection.Lookup_Node_Data

   -------------------
   -- Syntax fields --
   -------------------

   function Field_Name (Field : Field_Reference) return String;
   --  Implementation for Introspection.Field_Name

   function Field_Type (Field : Field_Reference) return Node_Type_Id;
   --  Implementation for Introspection.Field_Type

   function Eval_Field
     (Node : Bare_Ada_Node; Field : Field_Reference) return Bare_Ada_Node;
   --  Implementation for Introspection.Eval_Field

   function Index
     (Kind : Ada_Node_Kind_Type; Field : Field_Reference) return Positive;
   --  Implementation for Introspection.Index

   function Field_Reference_From_Index
     (Kind : Ada_Node_Kind_Type; Index : Positive) return Field_Reference;
   --  Implementation for Introspection.Field_Reference_From_Index

   function Fields
     (Id : Node_Type_Id; Concrete_Only : Boolean) return Field_Reference_Array;
   --  Return the list of fields associated to Id. If Concrete_Only is true,
   --  collect only non-null and concrete fields. Otherwise, collect all
   --  fields.

   function Fields (Kind : Ada_Node_Kind_Type) return Field_Reference_Array;
   --  Implementation for Introspection.Fields

   function Fields (Id : Node_Type_Id) return Field_Reference_Array;
   --  Implementation for Introspection.Fields

   ----------------
   -- Properties --
   ----------------

   function Property_Name (Property : Property_Reference) return String;
   --  Implementation for Introspection.Property_Name

   function Property_Return_Type
     (Property : Property_Reference) return Value_Constraint;
   --  Implementation for Introspection.Property_Type

   function Property_Argument_Types
     (Property : Property_Reference) return Value_Constraint_Array;
   --  Implementation for Introspection.Property_Argument_Types

   function Property_Argument_Name
     (Property : Property_Reference; Argument_Number : Positive) return String;
   --  Implementation for Introspection.Property_Argument_Name

   function Property_Argument_Default_Value
     (Property : Property_Reference; Argument_Number : Positive)
      return Internal_Value;
   --  Implementation for Introspection.Property_Argument_Default_Value

   function Properties
     (Kind : Ada_Node_Kind_Type) return Property_Reference_Array;
   --  Implementation for Introspection.Properties

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Implementation for Introspection.Properties

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive);
   --  Raise a Property_Error if Argument_Number is not valid for the property
   --  that Desc describes. Do nothing otherwise.

   ------------
   -- Tokens --
   ------------

   function Token_Node_Kind (Kind : Ada_Node_Kind_Type) return Token_Kind;
   --  Implementation for Introspection.Token_Node_Kind

end Libadalang.Introspection_Implementation;
