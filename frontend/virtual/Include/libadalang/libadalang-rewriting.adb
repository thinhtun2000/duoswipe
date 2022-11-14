with Ada.Unchecked_Conversion;

with Libadalang.Common; use Libadalang.Common.Token_Data_Handlers;

with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Rewriting is

   pragma Warnings (Off, "possible aliasing problem for type");
   function Unwrap_RH is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Impl.Rewriting_Handle);
   function Wrap_RH is new Ada.Unchecked_Conversion
     (Impl.Rewriting_Handle, Rewriting_Handle);

   function Unwrap_Node_RH is new Ada.Unchecked_Conversion
     (Node_Rewriting_Handle, Impl.Node_Rewriting_Handle);
   function Wrap_Node_RH is new Ada.Unchecked_Conversion
     (Impl.Node_Rewriting_Handle, Node_Rewriting_Handle);

   function Unwrap_Unit_RH is new Ada.Unchecked_Conversion
     (Unit_Rewriting_Handle, Impl.Unit_Rewriting_Handle);
   function Wrap_Unit_RH is new Ada.Unchecked_Conversion
     (Impl.Unit_Rewriting_Handle, Unit_Rewriting_Handle);
   pragma Warnings (On, "possible aliasing problem for type");

   function Wrap_Apply_Result (Res : Impl.Apply_Result) return Apply_Result;

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array;

   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array) return Impl
     .Node_Rewriting_Handle_Array;

   function Wrap_Apply_Result (Res : Impl.Apply_Result) return Apply_Result is
   begin
      if Res.Success then
         return (Success => True);
      else
         return (Success => False, Unit => Wrap_Unit (Res.Unit),
            Diagnostics  => Res.Diagnostics);
      end if;
   end Wrap_Apply_Result;

   ------------------------
   -- Wrap_Unit_RH_Array --
   ------------------------

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array
   is
      Res : Unit_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Wrap_Unit_RH (Arr (I));
      end loop;
      return Res;
   end Wrap_Unit_RH_Array;

   --------------------------
   -- Unwrap_Node_RH_Array --
   --------------------------

   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array) return Impl
     .Node_Rewriting_Handle_Array
   is
      Res : Impl.Node_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Unwrap_Node_RH (Arr (I));
      end loop;
      return Res;
   end Unwrap_Node_RH_Array;

   ------------
   -- Handle --
   ------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Handle (Unwrap_Context (Context)));
   end Handle;

   -------------
   -- Context --
   -------------

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
   begin
      return Wrap_Context (Impl.Context (Unwrap_RH (Handle)));
   end Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
   is
   begin
      return Wrap_RH (Impl.Start_Rewriting (Unwrap_Context (Context)));
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
   begin
      Impl.Abort_Rewriting (Internal_Handle);
      Handle := Wrap_RH (Internal_Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
      Res             : Impl.Apply_Result     := Impl.Apply (Internal_Handle);
   begin
      Handle := Wrap_RH (Internal_Handle);
      return Wrap_Apply_Result (Res);
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array
   is
   begin
      return Wrap_Unit_RH_Array (Impl.Unit_Handles (Unwrap_RH (Handle)));
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
   begin
      return Wrap_Unit_RH (Impl.Handle (Unwrap_Unit (Unit)));
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit is
   begin
      return Wrap_Unit (Impl.Unit (Unwrap_Unit_RH (Handle)));
   end Unit;

   ------------
   -- Handle --
   ------------

   function Handle (Node : Ada_Node'Class) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Handle (Unwrap_Node (Node)));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node (Handle : Node_Rewriting_Handle) return Ada_Node is
   begin
      return Wrap_Node (Impl.Node (Unwrap_Node_RH (Handle)));
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Context (Unwrap_Node_RH (Handle)));
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Unparse (Unwrap_Node_RH (Handle));
   end Unparse;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return Ada_Node_Kind_Type is
   begin
      return Impl.Kind (Unwrap_Node_RH (Handle));
   end Kind;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Impl.Tied (Unwrap_Node_RH (Handle));
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Parent (Unwrap_Node_RH (Handle)));
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      return Impl.Children_Count (Unwrap_Node_RH (Handle));
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle; Index : Positive)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Child (Unwrap_Node_RH (Handle), Index));
   end Child;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Root (Unwrap_Unit_RH (Handle)));
   end Root;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle; Index : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      Impl.Set_Child (Unwrap_Node_RH (Handle), Index, Unwrap_Node_RH (Child));
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Text (Unwrap_Node_RH (Handle));
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      Impl.Set_Text (Unwrap_Node_RH (Handle), Text);
   end Set_Text;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle; Root : Node_Rewriting_Handle)
   is
   begin
      Impl.Set_Root (Unwrap_Unit_RH (Handle), Unwrap_Node_RH (Root));
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type
   is
   begin
      return Impl.Unparse (Unwrap_Unit_RH (Handle));
   end Unparse;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      Impl.Replace (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Node));
   end Replace;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle; Index : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      Impl.Insert_Child
        (Unwrap_Node_RH (Handle), Index, Unwrap_Node_RH (Child));
   end Insert_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Handle : Node_Rewriting_Handle; Child : Node_Rewriting_Handle)
   is
   begin
      Impl.Append_Child (Unwrap_Node_RH (Handle), Unwrap_Node_RH (Child));
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle; Index : Positive) is
   begin
      Impl.Remove_Child (Unwrap_Node_RH (Handle), Index);
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Clone (Unwrap_Node_RH (Handle)));
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle; Kind : Ada_Node_Kind_Type)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Create_Node (Unwrap_RH (Handle), Kind));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle; Kind : Ada_Node_Kind_Type; Text : Text_Type)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Token_Node (Unwrap_RH (Handle), Kind, Text));
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle; Kind : Ada_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Regular_Node
             (Unwrap_RH (Handle), Kind, Unwrap_Node_RH_Array (Children)));
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle; Template : Text_Type;
      Arguments : Node_Rewriting_Handle_Array; Rule : Grammar_Rule)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_From_Template
             (Unwrap_RH (Handle), Template, Unwrap_Node_RH_Array (Arguments),
              Rule));
   end Create_From_Template;

   function Create_Constrained_Array_Indices
     (Handle                           : Rewriting_Handle;
      Constrained_Array_Indices_F_List : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Constrained_Array_Indices
             (Unwrap_RH (Handle),
              Constrained_Array_Indices_F_List =>
                Unwrap_Node_RH (Constrained_Array_Indices_F_List)));
   end Create_Constrained_Array_Indices;

   function Create_Unconstrained_Array_Indices
     (Handle                              : Rewriting_Handle;
      Unconstrained_Array_Indices_F_Types : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Unconstrained_Array_Indices
             (Unwrap_RH (Handle),
              Unconstrained_Array_Indices_F_Types =>
                Unwrap_Node_RH (Unconstrained_Array_Indices_F_Types)));
   end Create_Unconstrained_Array_Indices;

   function Create_Aspect_Assoc
     (Handle : Rewriting_Handle; Aspect_Assoc_F_Id : Node_Rewriting_Handle;
      Aspect_Assoc_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Aspect_Assoc
             (Unwrap_RH (Handle),
              Aspect_Assoc_F_Id   => Unwrap_Node_RH (Aspect_Assoc_F_Id),
              Aspect_Assoc_F_Expr => Unwrap_Node_RH (Aspect_Assoc_F_Expr)));
   end Create_Aspect_Assoc;

   function Create_At_Clause
     (Handle : Rewriting_Handle; At_Clause_F_Name : Node_Rewriting_Handle;
      At_Clause_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_At_Clause
             (Unwrap_RH (Handle),
              At_Clause_F_Name => Unwrap_Node_RH (At_Clause_F_Name),
              At_Clause_F_Expr => Unwrap_Node_RH (At_Clause_F_Expr)));
   end Create_At_Clause;

   function Create_Attribute_Def_Clause
     (Handle                                : Rewriting_Handle;
      Attribute_Def_Clause_F_Attribute_Expr : Node_Rewriting_Handle;
      Attribute_Def_Clause_F_Expr           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Attribute_Def_Clause
             (Unwrap_RH (Handle),
              Attribute_Def_Clause_F_Attribute_Expr =>
                Unwrap_Node_RH (Attribute_Def_Clause_F_Attribute_Expr),
              Attribute_Def_Clause_F_Expr =>
                Unwrap_Node_RH (Attribute_Def_Clause_F_Expr)));
   end Create_Attribute_Def_Clause;

   function Create_Enum_Rep_Clause
     (Handle                      : Rewriting_Handle;
      Enum_Rep_Clause_F_Type_Name : Node_Rewriting_Handle;
      Enum_Rep_Clause_F_Aggregate : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Enum_Rep_Clause
             (Unwrap_RH (Handle),
              Enum_Rep_Clause_F_Type_Name =>
                Unwrap_Node_RH (Enum_Rep_Clause_F_Type_Name),
              Enum_Rep_Clause_F_Aggregate =>
                Unwrap_Node_RH (Enum_Rep_Clause_F_Aggregate)));
   end Create_Enum_Rep_Clause;

   function Create_Record_Rep_Clause
     (Handle                         : Rewriting_Handle;
      Record_Rep_Clause_F_Name       : Node_Rewriting_Handle;
      Record_Rep_Clause_F_At_Expr    : Node_Rewriting_Handle;
      Record_Rep_Clause_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Record_Rep_Clause
             (Unwrap_RH (Handle),
              Record_Rep_Clause_F_Name =>
                Unwrap_Node_RH (Record_Rep_Clause_F_Name),
              Record_Rep_Clause_F_At_Expr =>
                Unwrap_Node_RH (Record_Rep_Clause_F_At_Expr),
              Record_Rep_Clause_F_Components =>
                Unwrap_Node_RH (Record_Rep_Clause_F_Components)));
   end Create_Record_Rep_Clause;

   function Create_Aspect_Spec
     (Handle                      : Rewriting_Handle;
      Aspect_Spec_F_Aspect_Assocs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Aspect_Spec
             (Unwrap_RH (Handle),
              Aspect_Spec_F_Aspect_Assocs =>
                Unwrap_Node_RH (Aspect_Spec_F_Aspect_Assocs)));
   end Create_Aspect_Spec;

   function Create_Contract_Case_Assoc
     (Handle                            : Rewriting_Handle;
      Contract_Case_Assoc_F_Guard       : Node_Rewriting_Handle;
      Contract_Case_Assoc_F_Consequence : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Contract_Case_Assoc
             (Unwrap_RH (Handle),
              Contract_Case_Assoc_F_Guard =>
                Unwrap_Node_RH (Contract_Case_Assoc_F_Guard),
              Contract_Case_Assoc_F_Consequence =>
                Unwrap_Node_RH (Contract_Case_Assoc_F_Consequence)));
   end Create_Contract_Case_Assoc;

   function Create_Pragma_Argument_Assoc
     (Handle                       : Rewriting_Handle;
      Pragma_Argument_Assoc_F_Id   : Node_Rewriting_Handle;
      Pragma_Argument_Assoc_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Pragma_Argument_Assoc
             (Unwrap_RH (Handle),
              Pragma_Argument_Assoc_F_Id =>
                Unwrap_Node_RH (Pragma_Argument_Assoc_F_Id),
              Pragma_Argument_Assoc_F_Expr =>
                Unwrap_Node_RH (Pragma_Argument_Assoc_F_Expr)));
   end Create_Pragma_Argument_Assoc;

   function Create_Entry_Spec
     (Handle                    : Rewriting_Handle;
      Entry_Spec_F_Entry_Name   : Node_Rewriting_Handle;
      Entry_Spec_F_Family_Type  : Node_Rewriting_Handle;
      Entry_Spec_F_Entry_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Entry_Spec
             (Unwrap_RH (Handle),
              Entry_Spec_F_Entry_Name =>
                Unwrap_Node_RH (Entry_Spec_F_Entry_Name),
              Entry_Spec_F_Family_Type =>
                Unwrap_Node_RH (Entry_Spec_F_Family_Type),
              Entry_Spec_F_Entry_Params =>
                Unwrap_Node_RH (Entry_Spec_F_Entry_Params)));
   end Create_Entry_Spec;

   function Create_Subp_Spec
     (Handle : Rewriting_Handle; Subp_Spec_F_Subp_Kind : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Name    : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Params  : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Returns : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subp_Spec
             (Unwrap_RH (Handle),
              Subp_Spec_F_Subp_Kind => Unwrap_Node_RH (Subp_Spec_F_Subp_Kind),
              Subp_Spec_F_Subp_Name => Unwrap_Node_RH (Subp_Spec_F_Subp_Name),
              Subp_Spec_F_Subp_Params =>
                Unwrap_Node_RH (Subp_Spec_F_Subp_Params),
              Subp_Spec_F_Subp_Returns =>
                Unwrap_Node_RH (Subp_Spec_F_Subp_Returns)));
   end Create_Subp_Spec;

   function Create_Component_List
     (Handle                        : Rewriting_Handle;
      Component_List_F_Components   : Node_Rewriting_Handle;
      Component_List_F_Variant_Part : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Component_List
             (Unwrap_RH (Handle),
              Component_List_F_Components =>
                Unwrap_Node_RH (Component_List_F_Components),
              Component_List_F_Variant_Part =>
                Unwrap_Node_RH (Component_List_F_Variant_Part)));
   end Create_Component_List;

   function Create_Known_Discriminant_Part
     (Handle                                : Rewriting_Handle;
      Known_Discriminant_Part_F_Discr_Specs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Known_Discriminant_Part
             (Unwrap_RH (Handle),
              Known_Discriminant_Part_F_Discr_Specs =>
                Unwrap_Node_RH (Known_Discriminant_Part_F_Discr_Specs)));
   end Create_Known_Discriminant_Part;

   function Create_Entry_Completion_Formal_Params
     (Handle                                  : Rewriting_Handle;
      Entry_Completion_Formal_Params_F_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Entry_Completion_Formal_Params
             (Unwrap_RH (Handle),
              Entry_Completion_Formal_Params_F_Params =>
                Unwrap_Node_RH (Entry_Completion_Formal_Params_F_Params)));
   end Create_Entry_Completion_Formal_Params;

   function Create_Generic_Formal_Part
     (Handle                      : Rewriting_Handle;
      Generic_Formal_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Formal_Part
             (Unwrap_RH (Handle),
              Generic_Formal_Part_F_Decls =>
                Unwrap_Node_RH (Generic_Formal_Part_F_Decls)));
   end Create_Generic_Formal_Part;

   function Create_Null_Record_Def
     (Handle                       : Rewriting_Handle;
      Base_Record_Def_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Null_Record_Def
             (Unwrap_RH (Handle),
              Base_Record_Def_F_Components =>
                Unwrap_Node_RH (Base_Record_Def_F_Components)));
   end Create_Null_Record_Def;

   function Create_Record_Def
     (Handle                       : Rewriting_Handle;
      Base_Record_Def_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Record_Def
             (Unwrap_RH (Handle),
              Base_Record_Def_F_Components =>
                Unwrap_Node_RH (Base_Record_Def_F_Components)));
   end Create_Record_Def;

   function Create_Aggregate_Assoc
     (Handle                        : Rewriting_Handle;
      Aggregate_Assoc_F_Designators : Node_Rewriting_Handle;
      Aggregate_Assoc_F_R_Expr      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Aggregate_Assoc
             (Unwrap_RH (Handle),
              Aggregate_Assoc_F_Designators =>
                Unwrap_Node_RH (Aggregate_Assoc_F_Designators),
              Aggregate_Assoc_F_R_Expr =>
                Unwrap_Node_RH (Aggregate_Assoc_F_R_Expr)));
   end Create_Aggregate_Assoc;

   function Create_Multi_Dim_Array_Assoc
     (Handle                        : Rewriting_Handle;
      Aggregate_Assoc_F_Designators : Node_Rewriting_Handle;
      Aggregate_Assoc_F_R_Expr      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Multi_Dim_Array_Assoc
             (Unwrap_RH (Handle),
              Aggregate_Assoc_F_Designators =>
                Unwrap_Node_RH (Aggregate_Assoc_F_Designators),
              Aggregate_Assoc_F_R_Expr =>
                Unwrap_Node_RH (Aggregate_Assoc_F_R_Expr)));
   end Create_Multi_Dim_Array_Assoc;

   function Create_Discriminant_Assoc
     (Handle                          : Rewriting_Handle;
      Discriminant_Assoc_F_Ids        : Node_Rewriting_Handle;
      Discriminant_Assoc_F_Discr_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Discriminant_Assoc
             (Unwrap_RH (Handle),
              Discriminant_Assoc_F_Ids =>
                Unwrap_Node_RH (Discriminant_Assoc_F_Ids),
              Discriminant_Assoc_F_Discr_Expr =>
                Unwrap_Node_RH (Discriminant_Assoc_F_Discr_Expr)));
   end Create_Discriminant_Assoc;

   function Create_Param_Assoc
     (Handle                   : Rewriting_Handle;
      Param_Assoc_F_Designator : Node_Rewriting_Handle;
      Param_Assoc_F_R_Expr     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Param_Assoc
             (Unwrap_RH (Handle),
              Param_Assoc_F_Designator =>
                Unwrap_Node_RH (Param_Assoc_F_Designator),
              Param_Assoc_F_R_Expr => Unwrap_Node_RH (Param_Assoc_F_R_Expr)));
   end Create_Param_Assoc;

   function Create_Component_Decl
     (Handle : Rewriting_Handle; Component_Decl_F_Ids : Node_Rewriting_Handle;
      Component_Decl_F_Component_Def : Node_Rewriting_Handle;
      Component_Decl_F_Default_Expr  : Node_Rewriting_Handle;
      Component_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Component_Decl
             (Unwrap_RH (Handle),
              Component_Decl_F_Ids => Unwrap_Node_RH (Component_Decl_F_Ids),
              Component_Decl_F_Component_Def =>
                Unwrap_Node_RH (Component_Decl_F_Component_Def),
              Component_Decl_F_Default_Expr =>
                Unwrap_Node_RH (Component_Decl_F_Default_Expr),
              Component_Decl_F_Aspects =>
                Unwrap_Node_RH (Component_Decl_F_Aspects)));
   end Create_Component_Decl;

   function Create_Discriminant_Spec
     (Handle                           : Rewriting_Handle;
      Discriminant_Spec_F_Ids          : Node_Rewriting_Handle;
      Discriminant_Spec_F_Type_Expr    : Node_Rewriting_Handle;
      Discriminant_Spec_F_Default_Expr : Node_Rewriting_Handle;
      Discriminant_Spec_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Discriminant_Spec
             (Unwrap_RH (Handle),
              Discriminant_Spec_F_Ids =>
                Unwrap_Node_RH (Discriminant_Spec_F_Ids),
              Discriminant_Spec_F_Type_Expr =>
                Unwrap_Node_RH (Discriminant_Spec_F_Type_Expr),
              Discriminant_Spec_F_Default_Expr =>
                Unwrap_Node_RH (Discriminant_Spec_F_Default_Expr),
              Discriminant_Spec_F_Aspects =>
                Unwrap_Node_RH (Discriminant_Spec_F_Aspects)));
   end Create_Discriminant_Spec;

   function Create_Generic_Formal_Obj_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Formal_Obj_Decl
             (Unwrap_RH (Handle),
              Generic_Formal_F_Decl => Unwrap_Node_RH (Generic_Formal_F_Decl),
              Generic_Formal_F_Aspects =>
                Unwrap_Node_RH (Generic_Formal_F_Aspects)));
   end Create_Generic_Formal_Obj_Decl;

   function Create_Generic_Formal_Package
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Formal_Package
             (Unwrap_RH (Handle),
              Generic_Formal_F_Decl => Unwrap_Node_RH (Generic_Formal_F_Decl),
              Generic_Formal_F_Aspects =>
                Unwrap_Node_RH (Generic_Formal_F_Aspects)));
   end Create_Generic_Formal_Package;

   function Create_Generic_Formal_Subp_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Formal_Subp_Decl
             (Unwrap_RH (Handle),
              Generic_Formal_F_Decl => Unwrap_Node_RH (Generic_Formal_F_Decl),
              Generic_Formal_F_Aspects =>
                Unwrap_Node_RH (Generic_Formal_F_Aspects)));
   end Create_Generic_Formal_Subp_Decl;

   function Create_Generic_Formal_Type_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Formal_Type_Decl
             (Unwrap_RH (Handle),
              Generic_Formal_F_Decl => Unwrap_Node_RH (Generic_Formal_F_Decl),
              Generic_Formal_F_Aspects =>
                Unwrap_Node_RH (Generic_Formal_F_Aspects)));
   end Create_Generic_Formal_Type_Decl;

   function Create_Param_Spec
     (Handle : Rewriting_Handle; Param_Spec_F_Ids : Node_Rewriting_Handle;
      Param_Spec_F_Has_Aliased  : Node_Rewriting_Handle;
      Param_Spec_F_Mode         : Node_Rewriting_Handle;
      Param_Spec_F_Type_Expr    : Node_Rewriting_Handle;
      Param_Spec_F_Default_Expr : Node_Rewriting_Handle;
      Param_Spec_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Param_Spec
             (Unwrap_RH (Handle),
              Param_Spec_F_Ids         => Unwrap_Node_RH (Param_Spec_F_Ids),
              Param_Spec_F_Has_Aliased =>
                Unwrap_Node_RH (Param_Spec_F_Has_Aliased),
              Param_Spec_F_Mode      => Unwrap_Node_RH (Param_Spec_F_Mode),
              Param_Spec_F_Type_Expr =>
                Unwrap_Node_RH (Param_Spec_F_Type_Expr),
              Param_Spec_F_Default_Expr =>
                Unwrap_Node_RH (Param_Spec_F_Default_Expr),
              Param_Spec_F_Aspects => Unwrap_Node_RH (Param_Spec_F_Aspects)));
   end Create_Param_Spec;

   function Create_Generic_Package_Internal
     (Handle                           : Rewriting_Handle;
      Base_Package_Decl_F_Package_Name : Node_Rewriting_Handle;
      Base_Package_Decl_F_Aspects      : Node_Rewriting_Handle;
      Base_Package_Decl_F_Public_Part  : Node_Rewriting_Handle;
      Base_Package_Decl_F_Private_Part : Node_Rewriting_Handle;
      Base_Package_Decl_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Package_Internal
             (Unwrap_RH (Handle),
              Base_Package_Decl_F_Package_Name =>
                Unwrap_Node_RH (Base_Package_Decl_F_Package_Name),
              Base_Package_Decl_F_Aspects =>
                Unwrap_Node_RH (Base_Package_Decl_F_Aspects),
              Base_Package_Decl_F_Public_Part =>
                Unwrap_Node_RH (Base_Package_Decl_F_Public_Part),
              Base_Package_Decl_F_Private_Part =>
                Unwrap_Node_RH (Base_Package_Decl_F_Private_Part),
              Base_Package_Decl_F_End_Name =>
                Unwrap_Node_RH (Base_Package_Decl_F_End_Name)));
   end Create_Generic_Package_Internal;

   function Create_Package_Decl
     (Handle                           : Rewriting_Handle;
      Base_Package_Decl_F_Package_Name : Node_Rewriting_Handle;
      Base_Package_Decl_F_Aspects      : Node_Rewriting_Handle;
      Base_Package_Decl_F_Public_Part  : Node_Rewriting_Handle;
      Base_Package_Decl_F_Private_Part : Node_Rewriting_Handle;
      Base_Package_Decl_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Package_Decl
             (Unwrap_RH (Handle),
              Base_Package_Decl_F_Package_Name =>
                Unwrap_Node_RH (Base_Package_Decl_F_Package_Name),
              Base_Package_Decl_F_Aspects =>
                Unwrap_Node_RH (Base_Package_Decl_F_Aspects),
              Base_Package_Decl_F_Public_Part =>
                Unwrap_Node_RH (Base_Package_Decl_F_Public_Part),
              Base_Package_Decl_F_Private_Part =>
                Unwrap_Node_RH (Base_Package_Decl_F_Private_Part),
              Base_Package_Decl_F_End_Name =>
                Unwrap_Node_RH (Base_Package_Decl_F_End_Name)));
   end Create_Package_Decl;

   function Create_Discrete_Base_Subtype_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Discrete_Base_Subtype_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Discrete_Base_Subtype_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Discrete_Base_Subtype_Decl_F_Aspects =>
                Unwrap_Node_RH (Discrete_Base_Subtype_Decl_F_Aspects)));
   end Create_Discrete_Base_Subtype_Decl;

   function Create_Subtype_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Subtype_Decl_F_Subtype : Node_Rewriting_Handle;
      Subtype_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subtype_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name  => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Subtype_Decl_F_Subtype =>
                Unwrap_Node_RH (Subtype_Decl_F_Subtype),
              Subtype_Decl_F_Aspects =>
                Unwrap_Node_RH (Subtype_Decl_F_Aspects)));
   end Create_Subtype_Decl;

   function Create_Classwide_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Classwide_Type_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Classwide_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Classwide_Type_Decl_F_Aspects =>
                Unwrap_Node_RH (Classwide_Type_Decl_F_Aspects)));
   end Create_Classwide_Type_Decl;

   function Create_Incomplete_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Incomplete_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Incomplete_Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Incomplete_Type_Decl_F_Discriminants),
              Incomplete_Type_Decl_F_Aspects =>
                Unwrap_Node_RH (Incomplete_Type_Decl_F_Aspects)));
   end Create_Incomplete_Type_Decl;

   function Create_Incomplete_Tagged_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Discriminants       : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Aspects             : Node_Rewriting_Handle;
      Incomplete_Tagged_Type_Decl_F_Has_Abstract : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Incomplete_Tagged_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Incomplete_Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Incomplete_Type_Decl_F_Discriminants),
              Incomplete_Type_Decl_F_Aspects =>
                Unwrap_Node_RH (Incomplete_Type_Decl_F_Aspects),
              Incomplete_Tagged_Type_Decl_F_Has_Abstract =>
                Unwrap_Node_RH (Incomplete_Tagged_Type_Decl_F_Has_Abstract)));
   end Create_Incomplete_Tagged_Type_Decl;

   function Create_Protected_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Interfaces    : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Protected_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Protected_Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Protected_Type_Decl_F_Discriminants),
              Protected_Type_Decl_F_Aspects =>
                Unwrap_Node_RH (Protected_Type_Decl_F_Aspects),
              Protected_Type_Decl_F_Interfaces =>
                Unwrap_Node_RH (Protected_Type_Decl_F_Interfaces),
              Protected_Type_Decl_F_Definition =>
                Unwrap_Node_RH (Protected_Type_Decl_F_Definition)));
   end Create_Protected_Type_Decl;

   function Create_Task_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Task_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Task_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Task_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Task_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Task_Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Task_Type_Decl_F_Discriminants),
              Task_Type_Decl_F_Aspects =>
                Unwrap_Node_RH (Task_Type_Decl_F_Aspects),
              Task_Type_Decl_F_Definition =>
                Unwrap_Node_RH (Task_Type_Decl_F_Definition)));
   end Create_Task_Type_Decl;

   function Create_Single_Task_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Task_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Task_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Task_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Single_Task_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Task_Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Task_Type_Decl_F_Discriminants),
              Task_Type_Decl_F_Aspects =>
                Unwrap_Node_RH (Task_Type_Decl_F_Aspects),
              Task_Type_Decl_F_Definition =>
                Unwrap_Node_RH (Task_Type_Decl_F_Definition)));
   end Create_Single_Task_Type_Decl;

   function Create_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Type_Decl_F_Discriminants),
              Type_Decl_F_Type_Def => Unwrap_Node_RH (Type_Decl_F_Type_Def),
              Type_Decl_F_Aspects  => Unwrap_Node_RH (Type_Decl_F_Aspects)));
   end Create_Type_Decl;

   function Create_Anonymous_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Anonymous_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Type_Decl_F_Discriminants),
              Type_Decl_F_Type_Def => Unwrap_Node_RH (Type_Decl_F_Type_Def),
              Type_Decl_F_Aspects  => Unwrap_Node_RH (Type_Decl_F_Aspects)));
   end Create_Anonymous_Type_Decl;

   function Create_Synth_Anonymous_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Synth_Anonymous_Type_Decl
             (Unwrap_RH (Handle),
              Base_Type_Decl_F_Name => Unwrap_Node_RH (Base_Type_Decl_F_Name),
              Type_Decl_F_Discriminants =>
                Unwrap_Node_RH (Type_Decl_F_Discriminants),
              Type_Decl_F_Type_Def => Unwrap_Node_RH (Type_Decl_F_Type_Def),
              Type_Decl_F_Aspects  => Unwrap_Node_RH (Type_Decl_F_Aspects)));
   end Create_Synth_Anonymous_Type_Decl;

   function Create_Abstract_Subp_Decl
     (Handle                         : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec  : Node_Rewriting_Handle;
      Abstract_Subp_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Abstract_Subp_Decl
             (Unwrap_RH (Handle),
              Classic_Subp_Decl_F_Overriding =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Overriding),
              Classic_Subp_Decl_F_Subp_Spec =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Subp_Spec),
              Abstract_Subp_Decl_F_Aspects =>
                Unwrap_Node_RH (Abstract_Subp_Decl_F_Aspects)));
   end Create_Abstract_Subp_Decl;

   function Create_Abstract_Formal_Subp_Decl
     (Handle                          : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding  : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec   : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Default_Expr : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Abstract_Formal_Subp_Decl
             (Unwrap_RH (Handle),
              Classic_Subp_Decl_F_Overriding =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Overriding),
              Classic_Subp_Decl_F_Subp_Spec =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Subp_Spec),
              Formal_Subp_Decl_F_Default_Expr =>
                Unwrap_Node_RH (Formal_Subp_Decl_F_Default_Expr),
              Formal_Subp_Decl_F_Aspects =>
                Unwrap_Node_RH (Formal_Subp_Decl_F_Aspects)));
   end Create_Abstract_Formal_Subp_Decl;

   function Create_Concrete_Formal_Subp_Decl
     (Handle                          : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding  : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec   : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Default_Expr : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Concrete_Formal_Subp_Decl
             (Unwrap_RH (Handle),
              Classic_Subp_Decl_F_Overriding =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Overriding),
              Classic_Subp_Decl_F_Subp_Spec =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Subp_Spec),
              Formal_Subp_Decl_F_Default_Expr =>
                Unwrap_Node_RH (Formal_Subp_Decl_F_Default_Expr),
              Formal_Subp_Decl_F_Aspects =>
                Unwrap_Node_RH (Formal_Subp_Decl_F_Aspects)));
   end Create_Concrete_Formal_Subp_Decl;

   function Create_Subp_Decl
     (Handle                         : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subp_Decl
             (Unwrap_RH (Handle),
              Classic_Subp_Decl_F_Overriding =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Overriding),
              Classic_Subp_Decl_F_Subp_Spec =>
                Unwrap_Node_RH (Classic_Subp_Decl_F_Subp_Spec),
              Subp_Decl_F_Aspects => Unwrap_Node_RH (Subp_Decl_F_Aspects)));
   end Create_Subp_Decl;

   function Create_Entry_Decl
     (Handle                  : Rewriting_Handle;
      Entry_Decl_F_Overriding : Node_Rewriting_Handle;
      Entry_Decl_F_Spec       : Node_Rewriting_Handle;
      Entry_Decl_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Entry_Decl
             (Unwrap_RH (Handle),
              Entry_Decl_F_Overriding =>
                Unwrap_Node_RH (Entry_Decl_F_Overriding),
              Entry_Decl_F_Spec    => Unwrap_Node_RH (Entry_Decl_F_Spec),
              Entry_Decl_F_Aspects => Unwrap_Node_RH (Entry_Decl_F_Aspects)));
   end Create_Entry_Decl;

   function Create_Enum_Literal_Decl
     (Handle                      : Rewriting_Handle;
      Enum_Literal_Decl_F_Name    : Node_Rewriting_Handle;
      Enum_Literal_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Enum_Literal_Decl
             (Unwrap_RH (Handle),
              Enum_Literal_Decl_F_Name =>
                Unwrap_Node_RH (Enum_Literal_Decl_F_Name),
              Enum_Literal_Decl_F_Aspects =>
                Unwrap_Node_RH (Enum_Literal_Decl_F_Aspects)));
   end Create_Enum_Literal_Decl;

   function Create_Generic_Subp_Internal
     (Handle                            : Rewriting_Handle;
      Generic_Subp_Internal_F_Subp_Spec : Node_Rewriting_Handle;
      Generic_Subp_Internal_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Subp_Internal
             (Unwrap_RH (Handle),
              Generic_Subp_Internal_F_Subp_Spec =>
                Unwrap_Node_RH (Generic_Subp_Internal_F_Subp_Spec),
              Generic_Subp_Internal_F_Aspects =>
                Unwrap_Node_RH (Generic_Subp_Internal_F_Aspects)));
   end Create_Generic_Subp_Internal;

   function Create_Expr_Function
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Expr_Function_F_Expr        : Node_Rewriting_Handle;
      Expr_Function_F_Aspects     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Expr_Function
             (Unwrap_RH (Handle),
              Base_Subp_Body_F_Overriding =>
                Unwrap_Node_RH (Base_Subp_Body_F_Overriding),
              Base_Subp_Body_F_Subp_Spec =>
                Unwrap_Node_RH (Base_Subp_Body_F_Subp_Spec),
              Expr_Function_F_Expr    => Unwrap_Node_RH (Expr_Function_F_Expr),
              Expr_Function_F_Aspects =>
                Unwrap_Node_RH (Expr_Function_F_Aspects)));
   end Create_Expr_Function;

   function Create_Null_Subp_Decl
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Null_Subp_Decl_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Null_Subp_Decl
             (Unwrap_RH (Handle),
              Base_Subp_Body_F_Overriding =>
                Unwrap_Node_RH (Base_Subp_Body_F_Overriding),
              Base_Subp_Body_F_Subp_Spec =>
                Unwrap_Node_RH (Base_Subp_Body_F_Subp_Spec),
              Null_Subp_Decl_F_Aspects =>
                Unwrap_Node_RH (Null_Subp_Decl_F_Aspects)));
   end Create_Null_Subp_Decl;

   function Create_Subp_Body
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Body_F_Aspects         : Node_Rewriting_Handle;
      Subp_Body_F_Decls           : Node_Rewriting_Handle;
      Subp_Body_F_Stmts           : Node_Rewriting_Handle;
      Subp_Body_F_End_Name        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subp_Body
             (Unwrap_RH (Handle),
              Base_Subp_Body_F_Overriding =>
                Unwrap_Node_RH (Base_Subp_Body_F_Overriding),
              Base_Subp_Body_F_Subp_Spec =>
                Unwrap_Node_RH (Base_Subp_Body_F_Subp_Spec),
              Subp_Body_F_Aspects  => Unwrap_Node_RH (Subp_Body_F_Aspects),
              Subp_Body_F_Decls    => Unwrap_Node_RH (Subp_Body_F_Decls),
              Subp_Body_F_Stmts    => Unwrap_Node_RH (Subp_Body_F_Stmts),
              Subp_Body_F_End_Name => Unwrap_Node_RH (Subp_Body_F_End_Name)));
   end Create_Subp_Body;

   function Create_Subp_Renaming_Decl
     (Handle                       : Rewriting_Handle;
      Base_Subp_Body_F_Overriding  : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec   : Node_Rewriting_Handle;
      Subp_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Subp_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subp_Renaming_Decl
             (Unwrap_RH (Handle),
              Base_Subp_Body_F_Overriding =>
                Unwrap_Node_RH (Base_Subp_Body_F_Overriding),
              Base_Subp_Body_F_Subp_Spec =>
                Unwrap_Node_RH (Base_Subp_Body_F_Subp_Spec),
              Subp_Renaming_Decl_F_Renames =>
                Unwrap_Node_RH (Subp_Renaming_Decl_F_Renames),
              Subp_Renaming_Decl_F_Aspects =>
                Unwrap_Node_RH (Subp_Renaming_Decl_F_Aspects)));
   end Create_Subp_Renaming_Decl;

   function Create_Package_Body_Stub
     (Handle                      : Rewriting_Handle;
      Package_Body_Stub_F_Name    : Node_Rewriting_Handle;
      Package_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Package_Body_Stub
             (Unwrap_RH (Handle),
              Package_Body_Stub_F_Name =>
                Unwrap_Node_RH (Package_Body_Stub_F_Name),
              Package_Body_Stub_F_Aspects =>
                Unwrap_Node_RH (Package_Body_Stub_F_Aspects)));
   end Create_Package_Body_Stub;

   function Create_Protected_Body_Stub
     (Handle                        : Rewriting_Handle;
      Protected_Body_Stub_F_Name    : Node_Rewriting_Handle;
      Protected_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Protected_Body_Stub
             (Unwrap_RH (Handle),
              Protected_Body_Stub_F_Name =>
                Unwrap_Node_RH (Protected_Body_Stub_F_Name),
              Protected_Body_Stub_F_Aspects =>
                Unwrap_Node_RH (Protected_Body_Stub_F_Aspects)));
   end Create_Protected_Body_Stub;

   function Create_Subp_Body_Stub
     (Handle                      : Rewriting_Handle;
      Subp_Body_Stub_F_Overriding : Node_Rewriting_Handle;
      Subp_Body_Stub_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Body_Stub_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subp_Body_Stub
             (Unwrap_RH (Handle),
              Subp_Body_Stub_F_Overriding =>
                Unwrap_Node_RH (Subp_Body_Stub_F_Overriding),
              Subp_Body_Stub_F_Subp_Spec =>
                Unwrap_Node_RH (Subp_Body_Stub_F_Subp_Spec),
              Subp_Body_Stub_F_Aspects =>
                Unwrap_Node_RH (Subp_Body_Stub_F_Aspects)));
   end Create_Subp_Body_Stub;

   function Create_Task_Body_Stub
     (Handle : Rewriting_Handle; Task_Body_Stub_F_Name : Node_Rewriting_Handle;
      Task_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Task_Body_Stub
             (Unwrap_RH (Handle),
              Task_Body_Stub_F_Name => Unwrap_Node_RH (Task_Body_Stub_F_Name),
              Task_Body_Stub_F_Aspects =>
                Unwrap_Node_RH (Task_Body_Stub_F_Aspects)));
   end Create_Task_Body_Stub;

   function Create_Entry_Body
     (Handle                  : Rewriting_Handle;
      Entry_Body_F_Entry_Name : Node_Rewriting_Handle;
      Entry_Body_F_Index_Spec : Node_Rewriting_Handle;
      Entry_Body_F_Params     : Node_Rewriting_Handle;
      Entry_Body_F_Barrier    : Node_Rewriting_Handle;
      Entry_Body_F_Decls      : Node_Rewriting_Handle;
      Entry_Body_F_Stmts      : Node_Rewriting_Handle;
      Entry_Body_F_End_Name   : Node_Rewriting_Handle;
      Entry_Body_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Entry_Body
             (Unwrap_RH (Handle),
              Entry_Body_F_Entry_Name =>
                Unwrap_Node_RH (Entry_Body_F_Entry_Name),
              Entry_Body_F_Index_Spec =>
                Unwrap_Node_RH (Entry_Body_F_Index_Spec),
              Entry_Body_F_Params   => Unwrap_Node_RH (Entry_Body_F_Params),
              Entry_Body_F_Barrier  => Unwrap_Node_RH (Entry_Body_F_Barrier),
              Entry_Body_F_Decls    => Unwrap_Node_RH (Entry_Body_F_Decls),
              Entry_Body_F_Stmts    => Unwrap_Node_RH (Entry_Body_F_Stmts),
              Entry_Body_F_End_Name => Unwrap_Node_RH (Entry_Body_F_End_Name),
              Entry_Body_F_Aspects  => Unwrap_Node_RH (Entry_Body_F_Aspects)));
   end Create_Entry_Body;

   function Create_Package_Body
     (Handle                      : Rewriting_Handle;
      Package_Body_F_Package_Name : Node_Rewriting_Handle;
      Package_Body_F_Aspects      : Node_Rewriting_Handle;
      Package_Body_F_Decls        : Node_Rewriting_Handle;
      Package_Body_F_Stmts        : Node_Rewriting_Handle;
      Package_Body_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Package_Body
             (Unwrap_RH (Handle),
              Package_Body_F_Package_Name =>
                Unwrap_Node_RH (Package_Body_F_Package_Name),
              Package_Body_F_Aspects =>
                Unwrap_Node_RH (Package_Body_F_Aspects),
              Package_Body_F_Decls    => Unwrap_Node_RH (Package_Body_F_Decls),
              Package_Body_F_Stmts    => Unwrap_Node_RH (Package_Body_F_Stmts),
              Package_Body_F_End_Name =>
                Unwrap_Node_RH (Package_Body_F_End_Name)));
   end Create_Package_Body;

   function Create_Protected_Body
     (Handle : Rewriting_Handle; Protected_Body_F_Name : Node_Rewriting_Handle;
      Protected_Body_F_Aspects  : Node_Rewriting_Handle;
      Protected_Body_F_Decls    : Node_Rewriting_Handle;
      Protected_Body_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Protected_Body
             (Unwrap_RH (Handle),
              Protected_Body_F_Name => Unwrap_Node_RH (Protected_Body_F_Name),
              Protected_Body_F_Aspects =>
                Unwrap_Node_RH (Protected_Body_F_Aspects),
              Protected_Body_F_Decls =>
                Unwrap_Node_RH (Protected_Body_F_Decls),
              Protected_Body_F_End_Name =>
                Unwrap_Node_RH (Protected_Body_F_End_Name)));
   end Create_Protected_Body;

   function Create_Task_Body
     (Handle : Rewriting_Handle; Task_Body_F_Name : Node_Rewriting_Handle;
      Task_Body_F_Aspects  : Node_Rewriting_Handle;
      Task_Body_F_Decls    : Node_Rewriting_Handle;
      Task_Body_F_Stmts    : Node_Rewriting_Handle;
      Task_Body_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Task_Body
             (Unwrap_RH (Handle),
              Task_Body_F_Name     => Unwrap_Node_RH (Task_Body_F_Name),
              Task_Body_F_Aspects  => Unwrap_Node_RH (Task_Body_F_Aspects),
              Task_Body_F_Decls    => Unwrap_Node_RH (Task_Body_F_Decls),
              Task_Body_F_Stmts    => Unwrap_Node_RH (Task_Body_F_Stmts),
              Task_Body_F_End_Name => Unwrap_Node_RH (Task_Body_F_End_Name)));
   end Create_Task_Body;

   function Create_Entry_Index_Spec
     (Handle : Rewriting_Handle; Entry_Index_Spec_F_Id : Node_Rewriting_Handle;
      Entry_Index_Spec_F_Subtype : Node_Rewriting_Handle;
      Entry_Index_Spec_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Entry_Index_Spec
             (Unwrap_RH (Handle),
              Entry_Index_Spec_F_Id => Unwrap_Node_RH (Entry_Index_Spec_F_Id),
              Entry_Index_Spec_F_Subtype =>
                Unwrap_Node_RH (Entry_Index_Spec_F_Subtype),
              Entry_Index_Spec_F_Aspects =>
                Unwrap_Node_RH (Entry_Index_Spec_F_Aspects)));
   end Create_Entry_Index_Spec;

   function Create_Error_Decl
     (Handle : Rewriting_Handle; Error_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Error_Decl
             (Unwrap_RH (Handle),
              Error_Decl_F_Aspects => Unwrap_Node_RH (Error_Decl_F_Aspects)));
   end Create_Error_Decl;

   function Create_Exception_Decl
     (Handle : Rewriting_Handle; Exception_Decl_F_Ids : Node_Rewriting_Handle;
      Exception_Decl_F_Renames : Node_Rewriting_Handle;
      Exception_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Exception_Decl
             (Unwrap_RH (Handle),
              Exception_Decl_F_Ids => Unwrap_Node_RH (Exception_Decl_F_Ids),
              Exception_Decl_F_Renames =>
                Unwrap_Node_RH (Exception_Decl_F_Renames),
              Exception_Decl_F_Aspects =>
                Unwrap_Node_RH (Exception_Decl_F_Aspects)));
   end Create_Exception_Decl;

   function Create_Exception_Handler
     (Handle                                 : Rewriting_Handle;
      Exception_Handler_F_Exception_Name     : Node_Rewriting_Handle;
      Exception_Handler_F_Handled_Exceptions : Node_Rewriting_Handle;
      Exception_Handler_F_Stmts              : Node_Rewriting_Handle;
      Exception_Handler_F_Aspects            : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Exception_Handler
             (Unwrap_RH (Handle),
              Exception_Handler_F_Exception_Name =>
                Unwrap_Node_RH (Exception_Handler_F_Exception_Name),
              Exception_Handler_F_Handled_Exceptions =>
                Unwrap_Node_RH (Exception_Handler_F_Handled_Exceptions),
              Exception_Handler_F_Stmts =>
                Unwrap_Node_RH (Exception_Handler_F_Stmts),
              Exception_Handler_F_Aspects =>
                Unwrap_Node_RH (Exception_Handler_F_Aspects)));
   end Create_Exception_Handler;

   function Create_For_Loop_Var_Decl
     (Handle                      : Rewriting_Handle;
      For_Loop_Var_Decl_F_Id      : Node_Rewriting_Handle;
      For_Loop_Var_Decl_F_Id_Type : Node_Rewriting_Handle;
      For_Loop_Var_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_For_Loop_Var_Decl
             (Unwrap_RH (Handle),
              For_Loop_Var_Decl_F_Id =>
                Unwrap_Node_RH (For_Loop_Var_Decl_F_Id),
              For_Loop_Var_Decl_F_Id_Type =>
                Unwrap_Node_RH (For_Loop_Var_Decl_F_Id_Type),
              For_Loop_Var_Decl_F_Aspects =>
                Unwrap_Node_RH (For_Loop_Var_Decl_F_Aspects)));
   end Create_For_Loop_Var_Decl;

   function Create_Generic_Package_Decl
     (Handle                              : Rewriting_Handle;
      Generic_Decl_F_Formal_Part          : Node_Rewriting_Handle;
      Generic_Package_Decl_F_Package_Decl : Node_Rewriting_Handle;
      Generic_Package_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Package_Decl
             (Unwrap_RH (Handle),
              Generic_Decl_F_Formal_Part =>
                Unwrap_Node_RH (Generic_Decl_F_Formal_Part),
              Generic_Package_Decl_F_Package_Decl =>
                Unwrap_Node_RH (Generic_Package_Decl_F_Package_Decl),
              Generic_Package_Decl_F_Aspects =>
                Unwrap_Node_RH (Generic_Package_Decl_F_Aspects)));
   end Create_Generic_Package_Decl;

   function Create_Generic_Subp_Decl
     (Handle                        : Rewriting_Handle;
      Generic_Decl_F_Formal_Part    : Node_Rewriting_Handle;
      Generic_Subp_Decl_F_Subp_Decl : Node_Rewriting_Handle;
      Generic_Subp_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Subp_Decl
             (Unwrap_RH (Handle),
              Generic_Decl_F_Formal_Part =>
                Unwrap_Node_RH (Generic_Decl_F_Formal_Part),
              Generic_Subp_Decl_F_Subp_Decl =>
                Unwrap_Node_RH (Generic_Subp_Decl_F_Subp_Decl),
              Generic_Subp_Decl_F_Aspects =>
                Unwrap_Node_RH (Generic_Subp_Decl_F_Aspects)));
   end Create_Generic_Subp_Decl;

   function Create_Generic_Package_Instantiation
     (Handle                                           : Rewriting_Handle;
      Generic_Package_Instantiation_F_Name             : Node_Rewriting_Handle;
      Generic_Package_Instantiation_F_Generic_Pkg_Name : Node_Rewriting_Handle;
      Generic_Package_Instantiation_F_Params           : Node_Rewriting_Handle;
      Generic_Package_Instantiation_F_Aspects          : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Package_Instantiation
             (Unwrap_RH (Handle),
              Generic_Package_Instantiation_F_Name =>
                Unwrap_Node_RH (Generic_Package_Instantiation_F_Name),
              Generic_Package_Instantiation_F_Generic_Pkg_Name =>
                Unwrap_Node_RH
                  (Generic_Package_Instantiation_F_Generic_Pkg_Name),
              Generic_Package_Instantiation_F_Params =>
                Unwrap_Node_RH (Generic_Package_Instantiation_F_Params),
              Generic_Package_Instantiation_F_Aspects =>
                Unwrap_Node_RH (Generic_Package_Instantiation_F_Aspects)));
   end Create_Generic_Package_Instantiation;

   function Create_Generic_Subp_Instantiation
     (Handle                                         : Rewriting_Handle;
      Generic_Subp_Instantiation_F_Overriding        : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Kind              : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Subp_Name         : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Generic_Subp_Name : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Params            : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Aspects           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Subp_Instantiation
             (Unwrap_RH (Handle),
              Generic_Subp_Instantiation_F_Overriding =>
                Unwrap_Node_RH (Generic_Subp_Instantiation_F_Overriding),
              Generic_Subp_Instantiation_F_Kind =>
                Unwrap_Node_RH (Generic_Subp_Instantiation_F_Kind),
              Generic_Subp_Instantiation_F_Subp_Name =>
                Unwrap_Node_RH (Generic_Subp_Instantiation_F_Subp_Name),
              Generic_Subp_Instantiation_F_Generic_Subp_Name =>
                Unwrap_Node_RH
                  (Generic_Subp_Instantiation_F_Generic_Subp_Name),
              Generic_Subp_Instantiation_F_Params =>
                Unwrap_Node_RH (Generic_Subp_Instantiation_F_Params),
              Generic_Subp_Instantiation_F_Aspects =>
                Unwrap_Node_RH (Generic_Subp_Instantiation_F_Aspects)));
   end Create_Generic_Subp_Instantiation;

   function Create_Generic_Package_Renaming_Decl
     (Handle                                  : Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Package_Renaming_Decl
             (Unwrap_RH (Handle),
              Generic_Package_Renaming_Decl_F_Name =>
                Unwrap_Node_RH (Generic_Package_Renaming_Decl_F_Name),
              Generic_Package_Renaming_Decl_F_Renames =>
                Unwrap_Node_RH (Generic_Package_Renaming_Decl_F_Renames),
              Generic_Package_Renaming_Decl_F_Aspects =>
                Unwrap_Node_RH (Generic_Package_Renaming_Decl_F_Aspects)));
   end Create_Generic_Package_Renaming_Decl;

   function Create_Generic_Subp_Renaming_Decl
     (Handle                               : Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Kind    : Node_Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Generic_Subp_Renaming_Decl
             (Unwrap_RH (Handle),
              Generic_Subp_Renaming_Decl_F_Kind =>
                Unwrap_Node_RH (Generic_Subp_Renaming_Decl_F_Kind),
              Generic_Subp_Renaming_Decl_F_Name =>
                Unwrap_Node_RH (Generic_Subp_Renaming_Decl_F_Name),
              Generic_Subp_Renaming_Decl_F_Renames =>
                Unwrap_Node_RH (Generic_Subp_Renaming_Decl_F_Renames),
              Generic_Subp_Renaming_Decl_F_Aspects =>
                Unwrap_Node_RH (Generic_Subp_Renaming_Decl_F_Aspects)));
   end Create_Generic_Subp_Renaming_Decl;

   function Create_Label_Decl
     (Handle : Rewriting_Handle; Label_Decl_F_Name : Node_Rewriting_Handle;
      Label_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Label_Decl
             (Unwrap_RH (Handle),
              Label_Decl_F_Name    => Unwrap_Node_RH (Label_Decl_F_Name),
              Label_Decl_F_Aspects => Unwrap_Node_RH (Label_Decl_F_Aspects)));
   end Create_Label_Decl;

   function Create_Named_Stmt_Decl
     (Handle                    : Rewriting_Handle;
      Named_Stmt_Decl_F_Name    : Node_Rewriting_Handle;
      Named_Stmt_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Named_Stmt_Decl
             (Unwrap_RH (Handle),
              Named_Stmt_Decl_F_Name =>
                Unwrap_Node_RH (Named_Stmt_Decl_F_Name),
              Named_Stmt_Decl_F_Aspects =>
                Unwrap_Node_RH (Named_Stmt_Decl_F_Aspects)));
   end Create_Named_Stmt_Decl;

   function Create_Number_Decl
     (Handle : Rewriting_Handle; Number_Decl_F_Ids : Node_Rewriting_Handle;
      Number_Decl_F_Expr    : Node_Rewriting_Handle;
      Number_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Number_Decl
             (Unwrap_RH (Handle),
              Number_Decl_F_Ids     => Unwrap_Node_RH (Number_Decl_F_Ids),
              Number_Decl_F_Expr    => Unwrap_Node_RH (Number_Decl_F_Expr),
              Number_Decl_F_Aspects =>
                Unwrap_Node_RH (Number_Decl_F_Aspects)));
   end Create_Number_Decl;

   function Create_Object_Decl
     (Handle : Rewriting_Handle; Object_Decl_F_Ids : Node_Rewriting_Handle;
      Object_Decl_F_Has_Aliased     : Node_Rewriting_Handle;
      Object_Decl_F_Has_Constant    : Node_Rewriting_Handle;
      Object_Decl_F_Mode            : Node_Rewriting_Handle;
      Object_Decl_F_Type_Expr       : Node_Rewriting_Handle;
      Object_Decl_F_Default_Expr    : Node_Rewriting_Handle;
      Object_Decl_F_Renaming_Clause : Node_Rewriting_Handle;
      Object_Decl_F_Aspects         : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Object_Decl
             (Unwrap_RH (Handle),
              Object_Decl_F_Ids         => Unwrap_Node_RH (Object_Decl_F_Ids),
              Object_Decl_F_Has_Aliased =>
                Unwrap_Node_RH (Object_Decl_F_Has_Aliased),
              Object_Decl_F_Has_Constant =>
                Unwrap_Node_RH (Object_Decl_F_Has_Constant),
              Object_Decl_F_Mode      => Unwrap_Node_RH (Object_Decl_F_Mode),
              Object_Decl_F_Type_Expr =>
                Unwrap_Node_RH (Object_Decl_F_Type_Expr),
              Object_Decl_F_Default_Expr =>
                Unwrap_Node_RH (Object_Decl_F_Default_Expr),
              Object_Decl_F_Renaming_Clause =>
                Unwrap_Node_RH (Object_Decl_F_Renaming_Clause),
              Object_Decl_F_Aspects =>
                Unwrap_Node_RH (Object_Decl_F_Aspects)));
   end Create_Object_Decl;

   function Create_Anonymous_Object_Decl
     (Handle : Rewriting_Handle; Object_Decl_F_Ids : Node_Rewriting_Handle;
      Object_Decl_F_Has_Aliased     : Node_Rewriting_Handle;
      Object_Decl_F_Has_Constant    : Node_Rewriting_Handle;
      Object_Decl_F_Mode            : Node_Rewriting_Handle;
      Object_Decl_F_Type_Expr       : Node_Rewriting_Handle;
      Object_Decl_F_Default_Expr    : Node_Rewriting_Handle;
      Object_Decl_F_Renaming_Clause : Node_Rewriting_Handle;
      Object_Decl_F_Aspects         : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Anonymous_Object_Decl
             (Unwrap_RH (Handle),
              Object_Decl_F_Ids         => Unwrap_Node_RH (Object_Decl_F_Ids),
              Object_Decl_F_Has_Aliased =>
                Unwrap_Node_RH (Object_Decl_F_Has_Aliased),
              Object_Decl_F_Has_Constant =>
                Unwrap_Node_RH (Object_Decl_F_Has_Constant),
              Object_Decl_F_Mode      => Unwrap_Node_RH (Object_Decl_F_Mode),
              Object_Decl_F_Type_Expr =>
                Unwrap_Node_RH (Object_Decl_F_Type_Expr),
              Object_Decl_F_Default_Expr =>
                Unwrap_Node_RH (Object_Decl_F_Default_Expr),
              Object_Decl_F_Renaming_Clause =>
                Unwrap_Node_RH (Object_Decl_F_Renaming_Clause),
              Object_Decl_F_Aspects =>
                Unwrap_Node_RH (Object_Decl_F_Aspects)));
   end Create_Anonymous_Object_Decl;

   function Create_Extended_Return_Stmt_Object_Decl
     (Handle : Rewriting_Handle; Object_Decl_F_Ids : Node_Rewriting_Handle;
      Object_Decl_F_Has_Aliased     : Node_Rewriting_Handle;
      Object_Decl_F_Has_Constant    : Node_Rewriting_Handle;
      Object_Decl_F_Mode            : Node_Rewriting_Handle;
      Object_Decl_F_Type_Expr       : Node_Rewriting_Handle;
      Object_Decl_F_Default_Expr    : Node_Rewriting_Handle;
      Object_Decl_F_Renaming_Clause : Node_Rewriting_Handle;
      Object_Decl_F_Aspects         : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Extended_Return_Stmt_Object_Decl
             (Unwrap_RH (Handle),
              Object_Decl_F_Ids         => Unwrap_Node_RH (Object_Decl_F_Ids),
              Object_Decl_F_Has_Aliased =>
                Unwrap_Node_RH (Object_Decl_F_Has_Aliased),
              Object_Decl_F_Has_Constant =>
                Unwrap_Node_RH (Object_Decl_F_Has_Constant),
              Object_Decl_F_Mode      => Unwrap_Node_RH (Object_Decl_F_Mode),
              Object_Decl_F_Type_Expr =>
                Unwrap_Node_RH (Object_Decl_F_Type_Expr),
              Object_Decl_F_Default_Expr =>
                Unwrap_Node_RH (Object_Decl_F_Default_Expr),
              Object_Decl_F_Renaming_Clause =>
                Unwrap_Node_RH (Object_Decl_F_Renaming_Clause),
              Object_Decl_F_Aspects =>
                Unwrap_Node_RH (Object_Decl_F_Aspects)));
   end Create_Extended_Return_Stmt_Object_Decl;

   function Create_Package_Renaming_Decl
     (Handle                          : Rewriting_Handle;
      Package_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Package_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Package_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Package_Renaming_Decl
             (Unwrap_RH (Handle),
              Package_Renaming_Decl_F_Name =>
                Unwrap_Node_RH (Package_Renaming_Decl_F_Name),
              Package_Renaming_Decl_F_Renames =>
                Unwrap_Node_RH (Package_Renaming_Decl_F_Renames),
              Package_Renaming_Decl_F_Aspects =>
                Unwrap_Node_RH (Package_Renaming_Decl_F_Aspects)));
   end Create_Package_Renaming_Decl;

   function Create_Single_Protected_Decl
     (Handle                             : Rewriting_Handle;
      Single_Protected_Decl_F_Name       : Node_Rewriting_Handle;
      Single_Protected_Decl_F_Aspects    : Node_Rewriting_Handle;
      Single_Protected_Decl_F_Interfaces : Node_Rewriting_Handle;
      Single_Protected_Decl_F_Definition : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Single_Protected_Decl
             (Unwrap_RH (Handle),
              Single_Protected_Decl_F_Name =>
                Unwrap_Node_RH (Single_Protected_Decl_F_Name),
              Single_Protected_Decl_F_Aspects =>
                Unwrap_Node_RH (Single_Protected_Decl_F_Aspects),
              Single_Protected_Decl_F_Interfaces =>
                Unwrap_Node_RH (Single_Protected_Decl_F_Interfaces),
              Single_Protected_Decl_F_Definition =>
                Unwrap_Node_RH (Single_Protected_Decl_F_Definition)));
   end Create_Single_Protected_Decl;

   function Create_Single_Task_Decl
     (Handle                       : Rewriting_Handle;
      Single_Task_Decl_F_Task_Type : Node_Rewriting_Handle;
      Single_Task_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Single_Task_Decl
             (Unwrap_RH (Handle),
              Single_Task_Decl_F_Task_Type =>
                Unwrap_Node_RH (Single_Task_Decl_F_Task_Type),
              Single_Task_Decl_F_Aspects =>
                Unwrap_Node_RH (Single_Task_Decl_F_Aspects)));
   end Create_Single_Task_Decl;

   function Create_Case_Stmt_Alternative
     (Handle                          : Rewriting_Handle;
      Case_Stmt_Alternative_F_Choices : Node_Rewriting_Handle;
      Case_Stmt_Alternative_F_Stmts   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Case_Stmt_Alternative
             (Unwrap_RH (Handle),
              Case_Stmt_Alternative_F_Choices =>
                Unwrap_Node_RH (Case_Stmt_Alternative_F_Choices),
              Case_Stmt_Alternative_F_Stmts =>
                Unwrap_Node_RH (Case_Stmt_Alternative_F_Stmts)));
   end Create_Case_Stmt_Alternative;

   function Create_Compilation_Unit
     (Handle                     : Rewriting_Handle;
      Compilation_Unit_F_Prelude : Node_Rewriting_Handle;
      Compilation_Unit_F_Body    : Node_Rewriting_Handle;
      Compilation_Unit_F_Pragmas : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Compilation_Unit
             (Unwrap_RH (Handle),
              Compilation_Unit_F_Prelude =>
                Unwrap_Node_RH (Compilation_Unit_F_Prelude),
              Compilation_Unit_F_Body =>
                Unwrap_Node_RH (Compilation_Unit_F_Body),
              Compilation_Unit_F_Pragmas =>
                Unwrap_Node_RH (Compilation_Unit_F_Pragmas)));
   end Create_Compilation_Unit;

   function Create_Component_Clause
     (Handle : Rewriting_Handle; Component_Clause_F_Id : Node_Rewriting_Handle;
      Component_Clause_F_Position : Node_Rewriting_Handle;
      Component_Clause_F_Range    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Component_Clause
             (Unwrap_RH (Handle),
              Component_Clause_F_Id => Unwrap_Node_RH (Component_Clause_F_Id),
              Component_Clause_F_Position =>
                Unwrap_Node_RH (Component_Clause_F_Position),
              Component_Clause_F_Range =>
                Unwrap_Node_RH (Component_Clause_F_Range)));
   end Create_Component_Clause;

   function Create_Component_Def
     (Handle                       : Rewriting_Handle;
      Component_Def_F_Has_Aliased  : Node_Rewriting_Handle;
      Component_Def_F_Has_Constant : Node_Rewriting_Handle;
      Component_Def_F_Type_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Component_Def
             (Unwrap_RH (Handle),
              Component_Def_F_Has_Aliased =>
                Unwrap_Node_RH (Component_Def_F_Has_Aliased),
              Component_Def_F_Has_Constant =>
                Unwrap_Node_RH (Component_Def_F_Has_Constant),
              Component_Def_F_Type_Expr =>
                Unwrap_Node_RH (Component_Def_F_Type_Expr)));
   end Create_Component_Def;

   function Create_Delta_Constraint
     (Handle                    : Rewriting_Handle;
      Delta_Constraint_F_Digits : Node_Rewriting_Handle;
      Delta_Constraint_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Delta_Constraint
             (Unwrap_RH (Handle),
              Delta_Constraint_F_Digits =>
                Unwrap_Node_RH (Delta_Constraint_F_Digits),
              Delta_Constraint_F_Range =>
                Unwrap_Node_RH (Delta_Constraint_F_Range)));
   end Create_Delta_Constraint;

   function Create_Digits_Constraint
     (Handle                     : Rewriting_Handle;
      Digits_Constraint_F_Digits : Node_Rewriting_Handle;
      Digits_Constraint_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Digits_Constraint
             (Unwrap_RH (Handle),
              Digits_Constraint_F_Digits =>
                Unwrap_Node_RH (Digits_Constraint_F_Digits),
              Digits_Constraint_F_Range =>
                Unwrap_Node_RH (Digits_Constraint_F_Range)));
   end Create_Digits_Constraint;

   function Create_Discriminant_Constraint
     (Handle                                : Rewriting_Handle;
      Discriminant_Constraint_F_Constraints : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Discriminant_Constraint
             (Unwrap_RH (Handle),
              Discriminant_Constraint_F_Constraints =>
                Unwrap_Node_RH (Discriminant_Constraint_F_Constraints)));
   end Create_Discriminant_Constraint;

   function Create_Index_Constraint
     (Handle                         : Rewriting_Handle;
      Index_Constraint_F_Constraints : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Index_Constraint
             (Unwrap_RH (Handle),
              Index_Constraint_F_Constraints =>
                Unwrap_Node_RH (Index_Constraint_F_Constraints)));
   end Create_Index_Constraint;

   function Create_Range_Constraint
     (Handle                   : Rewriting_Handle;
      Range_Constraint_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Range_Constraint
             (Unwrap_RH (Handle),
              Range_Constraint_F_Range =>
                Unwrap_Node_RH (Range_Constraint_F_Range)));
   end Create_Range_Constraint;

   function Create_Declarative_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Declarative_Part
             (Unwrap_RH (Handle),
              Declarative_Part_F_Decls =>
                Unwrap_Node_RH (Declarative_Part_F_Decls)));
   end Create_Declarative_Part;

   function Create_Private_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Private_Part
             (Unwrap_RH (Handle),
              Declarative_Part_F_Decls =>
                Unwrap_Node_RH (Declarative_Part_F_Decls)));
   end Create_Private_Part;

   function Create_Public_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Public_Part
             (Unwrap_RH (Handle),
              Declarative_Part_F_Decls =>
                Unwrap_Node_RH (Declarative_Part_F_Decls)));
   end Create_Public_Part;

   function Create_Elsif_Expr_Part
     (Handle                      : Rewriting_Handle;
      Elsif_Expr_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Elsif_Expr_Part_F_Then_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Elsif_Expr_Part
             (Unwrap_RH (Handle),
              Elsif_Expr_Part_F_Cond_Expr =>
                Unwrap_Node_RH (Elsif_Expr_Part_F_Cond_Expr),
              Elsif_Expr_Part_F_Then_Expr =>
                Unwrap_Node_RH (Elsif_Expr_Part_F_Then_Expr)));
   end Create_Elsif_Expr_Part;

   function Create_Elsif_Stmt_Part
     (Handle                      : Rewriting_Handle;
      Elsif_Stmt_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Elsif_Stmt_Part_F_Stmts     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Elsif_Stmt_Part
             (Unwrap_RH (Handle),
              Elsif_Stmt_Part_F_Cond_Expr =>
                Unwrap_Node_RH (Elsif_Stmt_Part_F_Cond_Expr),
              Elsif_Stmt_Part_F_Stmts =>
                Unwrap_Node_RH (Elsif_Stmt_Part_F_Stmts)));
   end Create_Elsif_Stmt_Part;

   function Create_Allocator
     (Handle : Rewriting_Handle; Allocator_F_Subpool : Node_Rewriting_Handle;
      Allocator_F_Type_Or_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Allocator
             (Unwrap_RH (Handle),
              Allocator_F_Subpool      => Unwrap_Node_RH (Allocator_F_Subpool),
              Allocator_F_Type_Or_Expr =>
                Unwrap_Node_RH (Allocator_F_Type_Or_Expr)));
   end Create_Allocator;

   function Create_Aggregate
     (Handle                         : Rewriting_Handle;
      Base_Aggregate_F_Ancestor_Expr : Node_Rewriting_Handle;
      Base_Aggregate_F_Assocs        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Aggregate
             (Unwrap_RH (Handle),
              Base_Aggregate_F_Ancestor_Expr =>
                Unwrap_Node_RH (Base_Aggregate_F_Ancestor_Expr),
              Base_Aggregate_F_Assocs =>
                Unwrap_Node_RH (Base_Aggregate_F_Assocs)));
   end Create_Aggregate;

   function Create_Null_Record_Aggregate
     (Handle                         : Rewriting_Handle;
      Base_Aggregate_F_Ancestor_Expr : Node_Rewriting_Handle;
      Base_Aggregate_F_Assocs        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Null_Record_Aggregate
             (Unwrap_RH (Handle),
              Base_Aggregate_F_Ancestor_Expr =>
                Unwrap_Node_RH (Base_Aggregate_F_Ancestor_Expr),
              Base_Aggregate_F_Assocs =>
                Unwrap_Node_RH (Base_Aggregate_F_Assocs)));
   end Create_Null_Record_Aggregate;

   function Create_Bin_Op
     (Handle         : Rewriting_Handle; Bin_Op_F_Left : Node_Rewriting_Handle;
      Bin_Op_F_Op    : Node_Rewriting_Handle;
      Bin_Op_F_Right : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Bin_Op
             (Unwrap_RH (Handle),
              Bin_Op_F_Left  => Unwrap_Node_RH (Bin_Op_F_Left),
              Bin_Op_F_Op    => Unwrap_Node_RH (Bin_Op_F_Op),
              Bin_Op_F_Right => Unwrap_Node_RH (Bin_Op_F_Right)));
   end Create_Bin_Op;

   function Create_Relation_Op
     (Handle         : Rewriting_Handle; Bin_Op_F_Left : Node_Rewriting_Handle;
      Bin_Op_F_Op    : Node_Rewriting_Handle;
      Bin_Op_F_Right : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Relation_Op
             (Unwrap_RH (Handle),
              Bin_Op_F_Left  => Unwrap_Node_RH (Bin_Op_F_Left),
              Bin_Op_F_Op    => Unwrap_Node_RH (Bin_Op_F_Op),
              Bin_Op_F_Right => Unwrap_Node_RH (Bin_Op_F_Right)));
   end Create_Relation_Op;

   function Create_Case_Expr
     (Handle : Rewriting_Handle; Case_Expr_F_Expr : Node_Rewriting_Handle;
      Case_Expr_F_Cases : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Case_Expr
             (Unwrap_RH (Handle),
              Case_Expr_F_Expr  => Unwrap_Node_RH (Case_Expr_F_Expr),
              Case_Expr_F_Cases => Unwrap_Node_RH (Case_Expr_F_Cases)));
   end Create_Case_Expr;

   function Create_Case_Expr_Alternative
     (Handle                          : Rewriting_Handle;
      Case_Expr_Alternative_F_Choices : Node_Rewriting_Handle;
      Case_Expr_Alternative_F_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Case_Expr_Alternative
             (Unwrap_RH (Handle),
              Case_Expr_Alternative_F_Choices =>
                Unwrap_Node_RH (Case_Expr_Alternative_F_Choices),
              Case_Expr_Alternative_F_Expr =>
                Unwrap_Node_RH (Case_Expr_Alternative_F_Expr)));
   end Create_Case_Expr_Alternative;

   function Create_Contract_Cases
     (Handle                          : Rewriting_Handle;
      Contract_Cases_F_Contract_Cases : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Contract_Cases
             (Unwrap_RH (Handle),
              Contract_Cases_F_Contract_Cases =>
                Unwrap_Node_RH (Contract_Cases_F_Contract_Cases)));
   end Create_Contract_Cases;

   function Create_If_Expr
     (Handle : Rewriting_Handle; If_Expr_F_Cond_Expr : Node_Rewriting_Handle;
      If_Expr_F_Then_Expr    : Node_Rewriting_Handle;
      If_Expr_F_Alternatives : Node_Rewriting_Handle;
      If_Expr_F_Else_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_If_Expr
             (Unwrap_RH (Handle),
              If_Expr_F_Cond_Expr    => Unwrap_Node_RH (If_Expr_F_Cond_Expr),
              If_Expr_F_Then_Expr    => Unwrap_Node_RH (If_Expr_F_Then_Expr),
              If_Expr_F_Alternatives =>
                Unwrap_Node_RH (If_Expr_F_Alternatives),
              If_Expr_F_Else_Expr => Unwrap_Node_RH (If_Expr_F_Else_Expr)));
   end Create_If_Expr;

   function Create_Membership_Expr
     (Handle                             : Rewriting_Handle;
      Membership_Expr_F_Expr             : Node_Rewriting_Handle;
      Membership_Expr_F_Op               : Node_Rewriting_Handle;
      Membership_Expr_F_Membership_Exprs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Membership_Expr
             (Unwrap_RH (Handle),
              Membership_Expr_F_Expr =>
                Unwrap_Node_RH (Membership_Expr_F_Expr),
              Membership_Expr_F_Op => Unwrap_Node_RH (Membership_Expr_F_Op),
              Membership_Expr_F_Membership_Exprs =>
                Unwrap_Node_RH (Membership_Expr_F_Membership_Exprs)));
   end Create_Membership_Expr;

   function Create_Attribute_Ref
     (Handle                    : Rewriting_Handle;
      Attribute_Ref_F_Prefix    : Node_Rewriting_Handle;
      Attribute_Ref_F_Attribute : Node_Rewriting_Handle;
      Attribute_Ref_F_Args      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Attribute_Ref
             (Unwrap_RH (Handle),
              Attribute_Ref_F_Prefix =>
                Unwrap_Node_RH (Attribute_Ref_F_Prefix),
              Attribute_Ref_F_Attribute =>
                Unwrap_Node_RH (Attribute_Ref_F_Attribute),
              Attribute_Ref_F_Args => Unwrap_Node_RH (Attribute_Ref_F_Args)));
   end Create_Attribute_Ref;

   function Create_Update_Attribute_Ref
     (Handle                    : Rewriting_Handle;
      Attribute_Ref_F_Prefix    : Node_Rewriting_Handle;
      Attribute_Ref_F_Attribute : Node_Rewriting_Handle;
      Attribute_Ref_F_Args      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Update_Attribute_Ref
             (Unwrap_RH (Handle),
              Attribute_Ref_F_Prefix =>
                Unwrap_Node_RH (Attribute_Ref_F_Prefix),
              Attribute_Ref_F_Attribute =>
                Unwrap_Node_RH (Attribute_Ref_F_Attribute),
              Attribute_Ref_F_Args => Unwrap_Node_RH (Attribute_Ref_F_Args)));
   end Create_Update_Attribute_Ref;

   function Create_Call_Expr
     (Handle : Rewriting_Handle; Call_Expr_F_Name : Node_Rewriting_Handle;
      Call_Expr_F_Suffix : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Call_Expr
             (Unwrap_RH (Handle),
              Call_Expr_F_Name   => Unwrap_Node_RH (Call_Expr_F_Name),
              Call_Expr_F_Suffix => Unwrap_Node_RH (Call_Expr_F_Suffix)));
   end Create_Call_Expr;

   function Create_Defining_Name
     (Handle : Rewriting_Handle; Defining_Name_F_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Defining_Name
             (Unwrap_RH (Handle),
              Defining_Name_F_Name => Unwrap_Node_RH (Defining_Name_F_Name)));
   end Create_Defining_Name;

   function Create_Discrete_Subtype_Name
     (Handle                          : Rewriting_Handle;
      Discrete_Subtype_Name_F_Subtype : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Discrete_Subtype_Name
             (Unwrap_RH (Handle),
              Discrete_Subtype_Name_F_Subtype =>
                Unwrap_Node_RH (Discrete_Subtype_Name_F_Subtype)));
   end Create_Discrete_Subtype_Name;

   function Create_Dotted_Name
     (Handle : Rewriting_Handle; Dotted_Name_F_Prefix : Node_Rewriting_Handle;
      Dotted_Name_F_Suffix : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Dotted_Name
             (Unwrap_RH (Handle),
              Dotted_Name_F_Prefix => Unwrap_Node_RH (Dotted_Name_F_Prefix),
              Dotted_Name_F_Suffix => Unwrap_Node_RH (Dotted_Name_F_Suffix)));
   end Create_Dotted_Name;

   function Create_End_Name
     (Handle : Rewriting_Handle; End_Name_F_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_End_Name
             (Unwrap_RH (Handle),
              End_Name_F_Name => Unwrap_Node_RH (End_Name_F_Name)));
   end Create_End_Name;

   function Create_Explicit_Deref
     (Handle                  : Rewriting_Handle;
      Explicit_Deref_F_Prefix : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Explicit_Deref
             (Unwrap_RH (Handle),
              Explicit_Deref_F_Prefix =>
                Unwrap_Node_RH (Explicit_Deref_F_Prefix)));
   end Create_Explicit_Deref;

   function Create_Qual_Expr
     (Handle : Rewriting_Handle; Qual_Expr_F_Prefix : Node_Rewriting_Handle;
      Qual_Expr_F_Suffix : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Qual_Expr
             (Unwrap_RH (Handle),
              Qual_Expr_F_Prefix => Unwrap_Node_RH (Qual_Expr_F_Prefix),
              Qual_Expr_F_Suffix => Unwrap_Node_RH (Qual_Expr_F_Suffix)));
   end Create_Qual_Expr;

   function Create_Paren_Expr
     (Handle : Rewriting_Handle; Paren_Expr_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Paren_Expr
             (Unwrap_RH (Handle),
              Paren_Expr_F_Expr => Unwrap_Node_RH (Paren_Expr_F_Expr)));
   end Create_Paren_Expr;

   function Create_Quantified_Expr
     (Handle                       : Rewriting_Handle;
      Quantified_Expr_F_Quantifier : Node_Rewriting_Handle;
      Quantified_Expr_F_Loop_Spec  : Node_Rewriting_Handle;
      Quantified_Expr_F_Expr       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Quantified_Expr
             (Unwrap_RH (Handle),
              Quantified_Expr_F_Quantifier =>
                Unwrap_Node_RH (Quantified_Expr_F_Quantifier),
              Quantified_Expr_F_Loop_Spec =>
                Unwrap_Node_RH (Quantified_Expr_F_Loop_Spec),
              Quantified_Expr_F_Expr =>
                Unwrap_Node_RH (Quantified_Expr_F_Expr)));
   end Create_Quantified_Expr;

   function Create_Raise_Expr
     (Handle                      : Rewriting_Handle;
      Raise_Expr_F_Exception_Name : Node_Rewriting_Handle;
      Raise_Expr_F_Error_Message  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Raise_Expr
             (Unwrap_RH (Handle),
              Raise_Expr_F_Exception_Name =>
                Unwrap_Node_RH (Raise_Expr_F_Exception_Name),
              Raise_Expr_F_Error_Message =>
                Unwrap_Node_RH (Raise_Expr_F_Error_Message)));
   end Create_Raise_Expr;

   function Create_Un_Op
     (Handle       : Rewriting_Handle; Un_Op_F_Op : Node_Rewriting_Handle;
      Un_Op_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Un_Op
             (Unwrap_RH (Handle), Un_Op_F_Op => Unwrap_Node_RH (Un_Op_F_Op),
              Un_Op_F_Expr => Unwrap_Node_RH (Un_Op_F_Expr)));
   end Create_Un_Op;

   function Create_Handled_Stmts
     (Handle : Rewriting_Handle; Handled_Stmts_F_Stmts : Node_Rewriting_Handle;
      Handled_Stmts_F_Exceptions : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Handled_Stmts
             (Unwrap_RH (Handle),
              Handled_Stmts_F_Stmts => Unwrap_Node_RH (Handled_Stmts_F_Stmts),
              Handled_Stmts_F_Exceptions =>
                Unwrap_Node_RH (Handled_Stmts_F_Exceptions)));
   end Create_Handled_Stmts;

   function Create_Library_Item
     (Handle                     : Rewriting_Handle;
      Library_Item_F_Has_Private : Node_Rewriting_Handle;
      Library_Item_F_Item : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Library_Item
             (Unwrap_RH (Handle),
              Library_Item_F_Has_Private =>
                Unwrap_Node_RH (Library_Item_F_Has_Private),
              Library_Item_F_Item => Unwrap_Node_RH (Library_Item_F_Item)));
   end Create_Library_Item;

   function Create_For_Loop_Spec
     (Handle                      : Rewriting_Handle;
      For_Loop_Spec_F_Var_Decl    : Node_Rewriting_Handle;
      For_Loop_Spec_F_Loop_Type   : Node_Rewriting_Handle;
      For_Loop_Spec_F_Has_Reverse : Node_Rewriting_Handle;
      For_Loop_Spec_F_Iter_Expr   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_For_Loop_Spec
             (Unwrap_RH (Handle),
              For_Loop_Spec_F_Var_Decl =>
                Unwrap_Node_RH (For_Loop_Spec_F_Var_Decl),
              For_Loop_Spec_F_Loop_Type =>
                Unwrap_Node_RH (For_Loop_Spec_F_Loop_Type),
              For_Loop_Spec_F_Has_Reverse =>
                Unwrap_Node_RH (For_Loop_Spec_F_Has_Reverse),
              For_Loop_Spec_F_Iter_Expr =>
                Unwrap_Node_RH (For_Loop_Spec_F_Iter_Expr)));
   end Create_For_Loop_Spec;

   function Create_While_Loop_Spec
     (Handle                 : Rewriting_Handle;
      While_Loop_Spec_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_While_Loop_Spec
             (Unwrap_RH (Handle),
              While_Loop_Spec_F_Expr =>
                Unwrap_Node_RH (While_Loop_Spec_F_Expr)));
   end Create_While_Loop_Spec;

   function Create_Params
     (Handle : Rewriting_Handle; Params_F_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Params
             (Unwrap_RH (Handle),
              Params_F_Params => Unwrap_Node_RH (Params_F_Params)));
   end Create_Params;

   function Create_Pragma_Node
     (Handle : Rewriting_Handle; Pragma_Node_F_Id : Node_Rewriting_Handle;
      Pragma_Node_F_Args : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Pragma_Node
             (Unwrap_RH (Handle),
              Pragma_Node_F_Id   => Unwrap_Node_RH (Pragma_Node_F_Id),
              Pragma_Node_F_Args => Unwrap_Node_RH (Pragma_Node_F_Args)));
   end Create_Pragma_Node;

   function Create_Protected_Def
     (Handle                       : Rewriting_Handle;
      Protected_Def_F_Public_Part  : Node_Rewriting_Handle;
      Protected_Def_F_Private_Part : Node_Rewriting_Handle;
      Protected_Def_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Protected_Def
             (Unwrap_RH (Handle),
              Protected_Def_F_Public_Part =>
                Unwrap_Node_RH (Protected_Def_F_Public_Part),
              Protected_Def_F_Private_Part =>
                Unwrap_Node_RH (Protected_Def_F_Private_Part),
              Protected_Def_F_End_Name =>
                Unwrap_Node_RH (Protected_Def_F_End_Name)));
   end Create_Protected_Def;

   function Create_Range_Spec
     (Handle : Rewriting_Handle; Range_Spec_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Range_Spec
             (Unwrap_RH (Handle),
              Range_Spec_F_Range => Unwrap_Node_RH (Range_Spec_F_Range)));
   end Create_Range_Spec;

   function Create_Renaming_Clause
     (Handle                           : Rewriting_Handle;
      Renaming_Clause_F_Renamed_Object : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Renaming_Clause
             (Unwrap_RH (Handle),
              Renaming_Clause_F_Renamed_Object =>
                Unwrap_Node_RH (Renaming_Clause_F_Renamed_Object)));
   end Create_Renaming_Clause;

   function Create_Synthetic_Renaming_Clause
     (Handle                           : Rewriting_Handle;
      Renaming_Clause_F_Renamed_Object : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Synthetic_Renaming_Clause
             (Unwrap_RH (Handle),
              Renaming_Clause_F_Renamed_Object =>
                Unwrap_Node_RH (Renaming_Clause_F_Renamed_Object)));
   end Create_Synthetic_Renaming_Clause;

   function Create_Select_When_Part
     (Handle                       : Rewriting_Handle;
      Select_When_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Select_When_Part_F_Stmts     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Select_When_Part
             (Unwrap_RH (Handle),
              Select_When_Part_F_Cond_Expr =>
                Unwrap_Node_RH (Select_When_Part_F_Cond_Expr),
              Select_When_Part_F_Stmts =>
                Unwrap_Node_RH (Select_When_Part_F_Stmts)));
   end Create_Select_When_Part;

   function Create_Accept_Stmt
     (Handle : Rewriting_Handle; Accept_Stmt_F_Name : Node_Rewriting_Handle;
      Accept_Stmt_F_Entry_Index_Expr : Node_Rewriting_Handle;
      Accept_Stmt_F_Params           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Accept_Stmt
             (Unwrap_RH (Handle),
              Accept_Stmt_F_Name => Unwrap_Node_RH (Accept_Stmt_F_Name),
              Accept_Stmt_F_Entry_Index_Expr =>
                Unwrap_Node_RH (Accept_Stmt_F_Entry_Index_Expr),
              Accept_Stmt_F_Params => Unwrap_Node_RH (Accept_Stmt_F_Params)));
   end Create_Accept_Stmt;

   function Create_Accept_Stmt_With_Stmts
     (Handle : Rewriting_Handle; Accept_Stmt_F_Name : Node_Rewriting_Handle;
      Accept_Stmt_F_Entry_Index_Expr    : Node_Rewriting_Handle;
      Accept_Stmt_F_Params              : Node_Rewriting_Handle;
      Accept_Stmt_With_Stmts_F_Stmts    : Node_Rewriting_Handle;
      Accept_Stmt_With_Stmts_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Accept_Stmt_With_Stmts
             (Unwrap_RH (Handle),
              Accept_Stmt_F_Name => Unwrap_Node_RH (Accept_Stmt_F_Name),
              Accept_Stmt_F_Entry_Index_Expr =>
                Unwrap_Node_RH (Accept_Stmt_F_Entry_Index_Expr),
              Accept_Stmt_F_Params => Unwrap_Node_RH (Accept_Stmt_F_Params),
              Accept_Stmt_With_Stmts_F_Stmts =>
                Unwrap_Node_RH (Accept_Stmt_With_Stmts_F_Stmts),
              Accept_Stmt_With_Stmts_F_End_Name =>
                Unwrap_Node_RH (Accept_Stmt_With_Stmts_F_End_Name)));
   end Create_Accept_Stmt_With_Stmts;

   function Create_For_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_For_Loop_Stmt
             (Unwrap_RH (Handle),
              Base_Loop_Stmt_F_Spec  => Unwrap_Node_RH (Base_Loop_Stmt_F_Spec),
              Base_Loop_Stmt_F_Stmts =>
                Unwrap_Node_RH (Base_Loop_Stmt_F_Stmts),
              Base_Loop_Stmt_F_End_Name =>
                Unwrap_Node_RH (Base_Loop_Stmt_F_End_Name)));
   end Create_For_Loop_Stmt;

   function Create_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Loop_Stmt
             (Unwrap_RH (Handle),
              Base_Loop_Stmt_F_Spec  => Unwrap_Node_RH (Base_Loop_Stmt_F_Spec),
              Base_Loop_Stmt_F_Stmts =>
                Unwrap_Node_RH (Base_Loop_Stmt_F_Stmts),
              Base_Loop_Stmt_F_End_Name =>
                Unwrap_Node_RH (Base_Loop_Stmt_F_End_Name)));
   end Create_Loop_Stmt;

   function Create_While_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_While_Loop_Stmt
             (Unwrap_RH (Handle),
              Base_Loop_Stmt_F_Spec  => Unwrap_Node_RH (Base_Loop_Stmt_F_Spec),
              Base_Loop_Stmt_F_Stmts =>
                Unwrap_Node_RH (Base_Loop_Stmt_F_Stmts),
              Base_Loop_Stmt_F_End_Name =>
                Unwrap_Node_RH (Base_Loop_Stmt_F_End_Name)));
   end Create_While_Loop_Stmt;

   function Create_Begin_Block
     (Handle : Rewriting_Handle; Begin_Block_F_Stmts : Node_Rewriting_Handle;
      Begin_Block_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Begin_Block
             (Unwrap_RH (Handle),
              Begin_Block_F_Stmts    => Unwrap_Node_RH (Begin_Block_F_Stmts),
              Begin_Block_F_End_Name =>
                Unwrap_Node_RH (Begin_Block_F_End_Name)));
   end Create_Begin_Block;

   function Create_Decl_Block
     (Handle : Rewriting_Handle; Decl_Block_F_Decls : Node_Rewriting_Handle;
      Decl_Block_F_Stmts    : Node_Rewriting_Handle;
      Decl_Block_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Decl_Block
             (Unwrap_RH (Handle),
              Decl_Block_F_Decls    => Unwrap_Node_RH (Decl_Block_F_Decls),
              Decl_Block_F_Stmts    => Unwrap_Node_RH (Decl_Block_F_Stmts),
              Decl_Block_F_End_Name =>
                Unwrap_Node_RH (Decl_Block_F_End_Name)));
   end Create_Decl_Block;

   function Create_Case_Stmt
     (Handle : Rewriting_Handle; Case_Stmt_F_Expr : Node_Rewriting_Handle;
      Case_Stmt_F_Alternatives : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Case_Stmt
             (Unwrap_RH (Handle),
              Case_Stmt_F_Expr         => Unwrap_Node_RH (Case_Stmt_F_Expr),
              Case_Stmt_F_Alternatives =>
                Unwrap_Node_RH (Case_Stmt_F_Alternatives)));
   end Create_Case_Stmt;

   function Create_Extended_Return_Stmt
     (Handle                       : Rewriting_Handle;
      Extended_Return_Stmt_F_Decl  : Node_Rewriting_Handle;
      Extended_Return_Stmt_F_Stmts : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Extended_Return_Stmt
             (Unwrap_RH (Handle),
              Extended_Return_Stmt_F_Decl =>
                Unwrap_Node_RH (Extended_Return_Stmt_F_Decl),
              Extended_Return_Stmt_F_Stmts =>
                Unwrap_Node_RH (Extended_Return_Stmt_F_Stmts)));
   end Create_Extended_Return_Stmt;

   function Create_If_Stmt
     (Handle : Rewriting_Handle; If_Stmt_F_Cond_Expr : Node_Rewriting_Handle;
      If_Stmt_F_Then_Stmts   : Node_Rewriting_Handle;
      If_Stmt_F_Alternatives : Node_Rewriting_Handle;
      If_Stmt_F_Else_Stmts   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_If_Stmt
             (Unwrap_RH (Handle),
              If_Stmt_F_Cond_Expr    => Unwrap_Node_RH (If_Stmt_F_Cond_Expr),
              If_Stmt_F_Then_Stmts   => Unwrap_Node_RH (If_Stmt_F_Then_Stmts),
              If_Stmt_F_Alternatives =>
                Unwrap_Node_RH (If_Stmt_F_Alternatives),
              If_Stmt_F_Else_Stmts => Unwrap_Node_RH (If_Stmt_F_Else_Stmts)));
   end Create_If_Stmt;

   function Create_Named_Stmt
     (Handle : Rewriting_Handle; Named_Stmt_F_Decl : Node_Rewriting_Handle;
      Named_Stmt_F_Stmt : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Named_Stmt
             (Unwrap_RH (Handle),
              Named_Stmt_F_Decl => Unwrap_Node_RH (Named_Stmt_F_Decl),
              Named_Stmt_F_Stmt => Unwrap_Node_RH (Named_Stmt_F_Stmt)));
   end Create_Named_Stmt;

   function Create_Select_Stmt
     (Handle : Rewriting_Handle; Select_Stmt_F_Guards : Node_Rewriting_Handle;
      Select_Stmt_F_Else_Stmts  : Node_Rewriting_Handle;
      Select_Stmt_F_Abort_Stmts : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Select_Stmt
             (Unwrap_RH (Handle),
              Select_Stmt_F_Guards => Unwrap_Node_RH (Select_Stmt_F_Guards),
              Select_Stmt_F_Else_Stmts =>
                Unwrap_Node_RH (Select_Stmt_F_Else_Stmts),
              Select_Stmt_F_Abort_Stmts =>
                Unwrap_Node_RH (Select_Stmt_F_Abort_Stmts)));
   end Create_Select_Stmt;

   function Create_Abort_Stmt
     (Handle : Rewriting_Handle; Abort_Stmt_F_Names : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Abort_Stmt
             (Unwrap_RH (Handle),
              Abort_Stmt_F_Names => Unwrap_Node_RH (Abort_Stmt_F_Names)));
   end Create_Abort_Stmt;

   function Create_Assign_Stmt
     (Handle : Rewriting_Handle; Assign_Stmt_F_Dest : Node_Rewriting_Handle;
      Assign_Stmt_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Assign_Stmt
             (Unwrap_RH (Handle),
              Assign_Stmt_F_Dest => Unwrap_Node_RH (Assign_Stmt_F_Dest),
              Assign_Stmt_F_Expr => Unwrap_Node_RH (Assign_Stmt_F_Expr)));
   end Create_Assign_Stmt;

   function Create_Call_Stmt
     (Handle : Rewriting_Handle; Call_Stmt_F_Call : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Call_Stmt
             (Unwrap_RH (Handle),
              Call_Stmt_F_Call => Unwrap_Node_RH (Call_Stmt_F_Call)));
   end Create_Call_Stmt;

   function Create_Delay_Stmt
     (Handle                 : Rewriting_Handle;
      Delay_Stmt_F_Has_Until : Node_Rewriting_Handle;
      Delay_Stmt_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Delay_Stmt
             (Unwrap_RH (Handle),
              Delay_Stmt_F_Has_Until =>
                Unwrap_Node_RH (Delay_Stmt_F_Has_Until),
              Delay_Stmt_F_Expr => Unwrap_Node_RH (Delay_Stmt_F_Expr)));
   end Create_Delay_Stmt;

   function Create_Exit_Stmt
     (Handle : Rewriting_Handle; Exit_Stmt_F_Loop_Name : Node_Rewriting_Handle;
      Exit_Stmt_F_Cond_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Exit_Stmt
             (Unwrap_RH (Handle),
              Exit_Stmt_F_Loop_Name => Unwrap_Node_RH (Exit_Stmt_F_Loop_Name),
              Exit_Stmt_F_Cond_Expr =>
                Unwrap_Node_RH (Exit_Stmt_F_Cond_Expr)));
   end Create_Exit_Stmt;

   function Create_Goto_Stmt
     (Handle                 : Rewriting_Handle;
      Goto_Stmt_F_Label_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Goto_Stmt
             (Unwrap_RH (Handle),
              Goto_Stmt_F_Label_Name =>
                Unwrap_Node_RH (Goto_Stmt_F_Label_Name)));
   end Create_Goto_Stmt;

   function Create_Label
     (Handle : Rewriting_Handle; Label_F_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Label
             (Unwrap_RH (Handle),
              Label_F_Decl => Unwrap_Node_RH (Label_F_Decl)));
   end Create_Label;

   function Create_Raise_Stmt
     (Handle                      : Rewriting_Handle;
      Raise_Stmt_F_Exception_Name : Node_Rewriting_Handle;
      Raise_Stmt_F_Error_Message  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Raise_Stmt
             (Unwrap_RH (Handle),
              Raise_Stmt_F_Exception_Name =>
                Unwrap_Node_RH (Raise_Stmt_F_Exception_Name),
              Raise_Stmt_F_Error_Message =>
                Unwrap_Node_RH (Raise_Stmt_F_Error_Message)));
   end Create_Raise_Stmt;

   function Create_Requeue_Stmt
     (Handle                   : Rewriting_Handle;
      Requeue_Stmt_F_Call_Name : Node_Rewriting_Handle;
      Requeue_Stmt_F_Has_Abort : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Requeue_Stmt
             (Unwrap_RH (Handle),
              Requeue_Stmt_F_Call_Name =>
                Unwrap_Node_RH (Requeue_Stmt_F_Call_Name),
              Requeue_Stmt_F_Has_Abort =>
                Unwrap_Node_RH (Requeue_Stmt_F_Has_Abort)));
   end Create_Requeue_Stmt;

   function Create_Return_Stmt
     (Handle                    : Rewriting_Handle;
      Return_Stmt_F_Return_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Return_Stmt
             (Unwrap_RH (Handle),
              Return_Stmt_F_Return_Expr =>
                Unwrap_Node_RH (Return_Stmt_F_Return_Expr)));
   end Create_Return_Stmt;

   function Create_Subunit
     (Handle : Rewriting_Handle; Subunit_F_Name : Node_Rewriting_Handle;
      Subunit_F_Body : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subunit
             (Unwrap_RH (Handle),
              Subunit_F_Name => Unwrap_Node_RH (Subunit_F_Name),
              Subunit_F_Body => Unwrap_Node_RH (Subunit_F_Body)));
   end Create_Subunit;

   function Create_Task_Def
     (Handle : Rewriting_Handle; Task_Def_F_Interfaces : Node_Rewriting_Handle;
      Task_Def_F_Public_Part  : Node_Rewriting_Handle;
      Task_Def_F_Private_Part : Node_Rewriting_Handle;
      Task_Def_F_End_Name : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Task_Def
             (Unwrap_RH (Handle),
              Task_Def_F_Interfaces  => Unwrap_Node_RH (Task_Def_F_Interfaces),
              Task_Def_F_Public_Part =>
                Unwrap_Node_RH (Task_Def_F_Public_Part),
              Task_Def_F_Private_Part =>
                Unwrap_Node_RH (Task_Def_F_Private_Part),
              Task_Def_F_End_Name => Unwrap_Node_RH (Task_Def_F_End_Name)));
   end Create_Task_Def;

   function Create_Access_To_Subp_Def
     (Handle                             : Rewriting_Handle;
      Access_Def_F_Has_Not_Null          : Node_Rewriting_Handle;
      Access_To_Subp_Def_F_Has_Protected : Node_Rewriting_Handle;
      Access_To_Subp_Def_F_Subp_Spec     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Access_To_Subp_Def
             (Unwrap_RH (Handle),
              Access_Def_F_Has_Not_Null =>
                Unwrap_Node_RH (Access_Def_F_Has_Not_Null),
              Access_To_Subp_Def_F_Has_Protected =>
                Unwrap_Node_RH (Access_To_Subp_Def_F_Has_Protected),
              Access_To_Subp_Def_F_Subp_Spec =>
                Unwrap_Node_RH (Access_To_Subp_Def_F_Subp_Spec)));
   end Create_Access_To_Subp_Def;

   function Create_Anonymous_Type_Access_Def
     (Handle                                : Rewriting_Handle;
      Access_Def_F_Has_Not_Null             : Node_Rewriting_Handle;
      Anonymous_Type_Access_Def_F_Type_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Anonymous_Type_Access_Def
             (Unwrap_RH (Handle),
              Access_Def_F_Has_Not_Null =>
                Unwrap_Node_RH (Access_Def_F_Has_Not_Null),
              Anonymous_Type_Access_Def_F_Type_Decl =>
                Unwrap_Node_RH (Anonymous_Type_Access_Def_F_Type_Decl)));
   end Create_Anonymous_Type_Access_Def;

   function Create_Type_Access_Def
     (Handle                               : Rewriting_Handle;
      Access_Def_F_Has_Not_Null            : Node_Rewriting_Handle;
      Type_Access_Def_F_Has_All            : Node_Rewriting_Handle;
      Type_Access_Def_F_Has_Constant       : Node_Rewriting_Handle;
      Type_Access_Def_F_Subtype_Indication : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Type_Access_Def
             (Unwrap_RH (Handle),
              Access_Def_F_Has_Not_Null =>
                Unwrap_Node_RH (Access_Def_F_Has_Not_Null),
              Type_Access_Def_F_Has_All =>
                Unwrap_Node_RH (Type_Access_Def_F_Has_All),
              Type_Access_Def_F_Has_Constant =>
                Unwrap_Node_RH (Type_Access_Def_F_Has_Constant),
              Type_Access_Def_F_Subtype_Indication =>
                Unwrap_Node_RH (Type_Access_Def_F_Subtype_Indication)));
   end Create_Type_Access_Def;

   function Create_Array_Type_Def
     (Handle                          : Rewriting_Handle;
      Array_Type_Def_F_Indices        : Node_Rewriting_Handle;
      Array_Type_Def_F_Component_Type : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Array_Type_Def
             (Unwrap_RH (Handle),
              Array_Type_Def_F_Indices =>
                Unwrap_Node_RH (Array_Type_Def_F_Indices),
              Array_Type_Def_F_Component_Type =>
                Unwrap_Node_RH (Array_Type_Def_F_Component_Type)));
   end Create_Array_Type_Def;

   function Create_Derived_Type_Def
     (Handle                                : Rewriting_Handle;
      Derived_Type_Def_F_Has_Abstract       : Node_Rewriting_Handle;
      Derived_Type_Def_F_Has_Limited        : Node_Rewriting_Handle;
      Derived_Type_Def_F_Has_Synchronized   : Node_Rewriting_Handle;
      Derived_Type_Def_F_Subtype_Indication : Node_Rewriting_Handle;
      Derived_Type_Def_F_Interfaces         : Node_Rewriting_Handle;
      Derived_Type_Def_F_Record_Extension   : Node_Rewriting_Handle;
      Derived_Type_Def_F_Has_With_Private   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Derived_Type_Def
             (Unwrap_RH (Handle),
              Derived_Type_Def_F_Has_Abstract =>
                Unwrap_Node_RH (Derived_Type_Def_F_Has_Abstract),
              Derived_Type_Def_F_Has_Limited =>
                Unwrap_Node_RH (Derived_Type_Def_F_Has_Limited),
              Derived_Type_Def_F_Has_Synchronized =>
                Unwrap_Node_RH (Derived_Type_Def_F_Has_Synchronized),
              Derived_Type_Def_F_Subtype_Indication =>
                Unwrap_Node_RH (Derived_Type_Def_F_Subtype_Indication),
              Derived_Type_Def_F_Interfaces =>
                Unwrap_Node_RH (Derived_Type_Def_F_Interfaces),
              Derived_Type_Def_F_Record_Extension =>
                Unwrap_Node_RH (Derived_Type_Def_F_Record_Extension),
              Derived_Type_Def_F_Has_With_Private =>
                Unwrap_Node_RH (Derived_Type_Def_F_Has_With_Private)));
   end Create_Derived_Type_Def;

   function Create_Enum_Type_Def
     (Handle                        : Rewriting_Handle;
      Enum_Type_Def_F_Enum_Literals : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Enum_Type_Def
             (Unwrap_RH (Handle),
              Enum_Type_Def_F_Enum_Literals =>
                Unwrap_Node_RH (Enum_Type_Def_F_Enum_Literals)));
   end Create_Enum_Type_Def;

   function Create_Interface_Type_Def
     (Handle                              : Rewriting_Handle;
      Interface_Type_Def_F_Interface_Kind : Node_Rewriting_Handle;
      Interface_Type_Def_F_Interfaces     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Interface_Type_Def
             (Unwrap_RH (Handle),
              Interface_Type_Def_F_Interface_Kind =>
                Unwrap_Node_RH (Interface_Type_Def_F_Interface_Kind),
              Interface_Type_Def_F_Interfaces =>
                Unwrap_Node_RH (Interface_Type_Def_F_Interfaces)));
   end Create_Interface_Type_Def;

   function Create_Mod_Int_Type_Def
     (Handle                  : Rewriting_Handle;
      Mod_Int_Type_Def_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Mod_Int_Type_Def
             (Unwrap_RH (Handle),
              Mod_Int_Type_Def_F_Expr =>
                Unwrap_Node_RH (Mod_Int_Type_Def_F_Expr)));
   end Create_Mod_Int_Type_Def;

   function Create_Private_Type_Def
     (Handle                          : Rewriting_Handle;
      Private_Type_Def_F_Has_Abstract : Node_Rewriting_Handle;
      Private_Type_Def_F_Has_Tagged   : Node_Rewriting_Handle;
      Private_Type_Def_F_Has_Limited  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Private_Type_Def
             (Unwrap_RH (Handle),
              Private_Type_Def_F_Has_Abstract =>
                Unwrap_Node_RH (Private_Type_Def_F_Has_Abstract),
              Private_Type_Def_F_Has_Tagged =>
                Unwrap_Node_RH (Private_Type_Def_F_Has_Tagged),
              Private_Type_Def_F_Has_Limited =>
                Unwrap_Node_RH (Private_Type_Def_F_Has_Limited)));
   end Create_Private_Type_Def;

   function Create_Decimal_Fixed_Point_Def
     (Handle                           : Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Delta  : Node_Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Digits : Node_Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Decimal_Fixed_Point_Def
             (Unwrap_RH (Handle),
              Decimal_Fixed_Point_Def_F_Delta =>
                Unwrap_Node_RH (Decimal_Fixed_Point_Def_F_Delta),
              Decimal_Fixed_Point_Def_F_Digits =>
                Unwrap_Node_RH (Decimal_Fixed_Point_Def_F_Digits),
              Decimal_Fixed_Point_Def_F_Range =>
                Unwrap_Node_RH (Decimal_Fixed_Point_Def_F_Range)));
   end Create_Decimal_Fixed_Point_Def;

   function Create_Floating_Point_Def
     (Handle                          : Rewriting_Handle;
      Floating_Point_Def_F_Num_Digits : Node_Rewriting_Handle;
      Floating_Point_Def_F_Range      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Floating_Point_Def
             (Unwrap_RH (Handle),
              Floating_Point_Def_F_Num_Digits =>
                Unwrap_Node_RH (Floating_Point_Def_F_Num_Digits),
              Floating_Point_Def_F_Range =>
                Unwrap_Node_RH (Floating_Point_Def_F_Range)));
   end Create_Floating_Point_Def;

   function Create_Ordinary_Fixed_Point_Def
     (Handle                           : Rewriting_Handle;
      Ordinary_Fixed_Point_Def_F_Delta : Node_Rewriting_Handle;
      Ordinary_Fixed_Point_Def_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Ordinary_Fixed_Point_Def
             (Unwrap_RH (Handle),
              Ordinary_Fixed_Point_Def_F_Delta =>
                Unwrap_Node_RH (Ordinary_Fixed_Point_Def_F_Delta),
              Ordinary_Fixed_Point_Def_F_Range =>
                Unwrap_Node_RH (Ordinary_Fixed_Point_Def_F_Range)));
   end Create_Ordinary_Fixed_Point_Def;

   function Create_Record_Type_Def
     (Handle                         : Rewriting_Handle;
      Record_Type_Def_F_Has_Abstract : Node_Rewriting_Handle;
      Record_Type_Def_F_Has_Tagged   : Node_Rewriting_Handle;
      Record_Type_Def_F_Has_Limited  : Node_Rewriting_Handle;
      Record_Type_Def_F_Record_Def   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Record_Type_Def
             (Unwrap_RH (Handle),
              Record_Type_Def_F_Has_Abstract =>
                Unwrap_Node_RH (Record_Type_Def_F_Has_Abstract),
              Record_Type_Def_F_Has_Tagged =>
                Unwrap_Node_RH (Record_Type_Def_F_Has_Tagged),
              Record_Type_Def_F_Has_Limited =>
                Unwrap_Node_RH (Record_Type_Def_F_Has_Limited),
              Record_Type_Def_F_Record_Def =>
                Unwrap_Node_RH (Record_Type_Def_F_Record_Def)));
   end Create_Record_Type_Def;

   function Create_Signed_Int_Type_Def
     (Handle                      : Rewriting_Handle;
      Signed_Int_Type_Def_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Signed_Int_Type_Def
             (Unwrap_RH (Handle),
              Signed_Int_Type_Def_F_Range =>
                Unwrap_Node_RH (Signed_Int_Type_Def_F_Range)));
   end Create_Signed_Int_Type_Def;

   function Create_Anonymous_Type
     (Handle                     : Rewriting_Handle;
      Anonymous_Type_F_Type_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Anonymous_Type
             (Unwrap_RH (Handle),
              Anonymous_Type_F_Type_Decl =>
                Unwrap_Node_RH (Anonymous_Type_F_Type_Decl)));
   end Create_Anonymous_Type;

   function Create_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Subtype_Indication
             (Unwrap_RH (Handle),
              Subtype_Indication_F_Has_Not_Null =>
                Unwrap_Node_RH (Subtype_Indication_F_Has_Not_Null),
              Subtype_Indication_F_Name =>
                Unwrap_Node_RH (Subtype_Indication_F_Name),
              Subtype_Indication_F_Constraint =>
                Unwrap_Node_RH (Subtype_Indication_F_Constraint)));
   end Create_Subtype_Indication;

   function Create_Constrained_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Constrained_Subtype_Indication
             (Unwrap_RH (Handle),
              Subtype_Indication_F_Has_Not_Null =>
                Unwrap_Node_RH (Subtype_Indication_F_Has_Not_Null),
              Subtype_Indication_F_Name =>
                Unwrap_Node_RH (Subtype_Indication_F_Name),
              Subtype_Indication_F_Constraint =>
                Unwrap_Node_RH (Subtype_Indication_F_Constraint)));
   end Create_Constrained_Subtype_Indication;

   function Create_Discrete_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Discrete_Subtype_Indication
             (Unwrap_RH (Handle),
              Subtype_Indication_F_Has_Not_Null =>
                Unwrap_Node_RH (Subtype_Indication_F_Has_Not_Null),
              Subtype_Indication_F_Name =>
                Unwrap_Node_RH (Subtype_Indication_F_Name),
              Subtype_Indication_F_Constraint =>
                Unwrap_Node_RH (Subtype_Indication_F_Constraint)));
   end Create_Discrete_Subtype_Indication;

   function Create_Unconstrained_Array_Index
     (Handle                                         : Rewriting_Handle;
      Unconstrained_Array_Index_F_Subtype_Indication : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Unconstrained_Array_Index
             (Unwrap_RH (Handle),
              Unconstrained_Array_Index_F_Subtype_Indication =>
                Unwrap_Node_RH
                  (Unconstrained_Array_Index_F_Subtype_Indication)));
   end Create_Unconstrained_Array_Index;

   function Create_Use_Package_Clause
     (Handle                        : Rewriting_Handle;
      Use_Package_Clause_F_Packages : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Use_Package_Clause
             (Unwrap_RH (Handle),
              Use_Package_Clause_F_Packages =>
                Unwrap_Node_RH (Use_Package_Clause_F_Packages)));
   end Create_Use_Package_Clause;

   function Create_Use_Type_Clause
     (Handle                    : Rewriting_Handle;
      Use_Type_Clause_F_Has_All : Node_Rewriting_Handle;
      Use_Type_Clause_F_Types   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Use_Type_Clause
             (Unwrap_RH (Handle),
              Use_Type_Clause_F_Has_All =>
                Unwrap_Node_RH (Use_Type_Clause_F_Has_All),
              Use_Type_Clause_F_Types =>
                Unwrap_Node_RH (Use_Type_Clause_F_Types)));
   end Create_Use_Type_Clause;

   function Create_Variant
     (Handle : Rewriting_Handle; Variant_F_Choices : Node_Rewriting_Handle;
      Variant_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Variant
             (Unwrap_RH (Handle),
              Variant_F_Choices    => Unwrap_Node_RH (Variant_F_Choices),
              Variant_F_Components => Unwrap_Node_RH (Variant_F_Components)));
   end Create_Variant;

   function Create_Variant_Part
     (Handle                    : Rewriting_Handle;
      Variant_Part_F_Discr_Name : Node_Rewriting_Handle;
      Variant_Part_F_Variant    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_Variant_Part
             (Unwrap_RH (Handle),
              Variant_Part_F_Discr_Name =>
                Unwrap_Node_RH (Variant_Part_F_Discr_Name),
              Variant_Part_F_Variant =>
                Unwrap_Node_RH (Variant_Part_F_Variant)));
   end Create_Variant_Part;

   function Create_With_Clause
     (Handle                    : Rewriting_Handle;
      With_Clause_F_Has_Limited : Node_Rewriting_Handle;
      With_Clause_F_Has_Private : Node_Rewriting_Handle;
      With_Clause_F_Packages    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH
          (Impl.Create_With_Clause
             (Unwrap_RH (Handle),
              With_Clause_F_Has_Limited =>
                Unwrap_Node_RH (With_Clause_F_Has_Limited),
              With_Clause_F_Has_Private =>
                Unwrap_Node_RH (With_Clause_F_Has_Private),
              With_Clause_F_Packages =>
                Unwrap_Node_RH (With_Clause_F_Packages)));
   end Create_With_Clause;

end Libadalang.Rewriting;
