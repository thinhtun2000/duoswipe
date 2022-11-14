with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Bump_Ptr; use Langkit_Support.Bump_Ptr;
with Langkit_Support.Bump_Ptr.Vectors;

with Libadalang.Common;         use Libadalang.Common;
with Libadalang.Implementation; use Libadalang.Implementation;

--  Internal package: low-level primitives to implement syntax-based source
--  rewriting.

private package Libadalang.Rewriting_Implementation is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle_Type;
   type Unit_Rewriting_Handle_Type;
   type Node_Rewriting_Handle_Type;

   type Rewriting_Handle is access Rewriting_Handle_Type;
   --  Internal handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is access Unit_Rewriting_Handle_Type;
   --  Internal handle for the process of rewriting an analysis unit

   type Node_Rewriting_Handle is access Node_Rewriting_Handle_Type;
   --  Internal handle for the process of rewriting an analysis unit

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Unit_Rewriting_Handle,
      Hash     => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=");

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Bare_Ada_Node, Element_Type => Node_Rewriting_Handle,
      Hash     => Named_Hash, Equivalent_Keys => "=");

   package Nodes_Pools is new Langkit_Support.Bump_Ptr.Vectors
     (Node_Rewriting_Handle);

   type Rewriting_Handle_Type is record
      Context : Internal_Context;
      --  Analysis context this rewriting handle relates to

      Units : Unit_Maps.Map;
      --  Keep track of rewriting handles we create all units that Context owns

      Pool      : Bump_Ptr_Pool;
      New_Nodes : Nodes_Pools.Vector;
      --  Keep track of all node rewriting handles that don't map to original
      --  nodes, i.e. all nodes that were created during this rewriting
      --  session.
   end record;

   type Unit_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context this relates to

      Unit : Internal_Unit;
      --  Analysis unit this relates to

      Root : Node_Rewriting_Handle;
      --  Handle for the node that will become the root node of this analysis
      --  unit.

      Nodes : Node_Maps.Map;
      --  Keep track of rewriting handles we create for base AST nodes that
      --  Unit owns.
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, Node_Rewriting_Handle);

   type Node_Children_Kind is
     (Unexpanded,
   --  Dummy node rewriting handle: children don't have their own handle yet

Expanded_Regular,
   --  Expanded node rewriting handle: children have their own handle. Note
   --  that this is for all but token nodes.

Expanded_Token_Node
      --  Expanded node rewriting handle, specific for token nodes: there is no
      --  children, only some associated text.
      );

   type Node_Children (Kind : Node_Children_Kind := Unexpanded) is record
      case Kind is
         when Unexpanded =>
            null;
         when Expanded_Regular =>
            Vector : Node_Vectors.Vector;
         when Expanded_Token_Node =>
            Text : Unbounded_Wide_Wide_String;
      end case;
   end record;
   --  Lazily evaluated vector of children for a Node_Rewriting_Handle.
   --
   --  In order to avoid constructing the whole tree of Node_Rewriting_Handle
   --  for some analysis unit at once, we build them in a lazy fashion.

   Unexpanded_Children : constant Node_Children := (Kind => Unexpanded);

   type Node_Rewriting_Handle_Type is record
      Context_Handle : Rewriting_Handle;
      --  Rewriting handle for the analysis context that owns Node

      Node : Bare_Ada_Node;
      --  Bare AST node which this rewriting handle relates to

      Parent : Node_Rewriting_Handle;
      --  Rewriting handle for Node's parent, or No_Node_Rewriting_Handle if
      --  Node is a root node.

      Kind : Ada_Node_Kind_Type;
      --  Kind for the node this handle represents. When Node is not null (i.e.
      --  when this represents an already existing node, rather than a new
      --  one), this must be equal to Node.Kind.

      Tied : Boolean;
      --  Whether this node is tied to an analysis unit tree. It can be
      --  assigned as a child to another node iff it is not tied.

      Root_Of : Unit_Rewriting_Handle;
      --  If the node this handle represents is the root of a rewritten unit,
      --  this references this unit. No_Unit_Rewriting_Handle in all other
      --  cases.

      Children : Node_Children;
      --  Lazily evaluated vector of children for the rewritten node
   end record;

   type Unit_Rewriting_Handle_Array is
     array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
     array (Positive range <>) of Node_Rewriting_Handle;

   No_Rewriting_Handle      : constant Rewriting_Handle      := null;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle := null;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle := null;

   --------------------------------------------------
   -- Implementation of context rewriting routines --
   --------------------------------------------------

   function Handle (Context : Internal_Context) return Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Context (Handle : Rewriting_Handle) return Internal_Context;
   --  Implementation for Rewriting.Context

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle with
      Post => Handle (Context) /= No_Rewriting_Handle
      and then Has_With_Trivia (Context)
      and then Start_Rewriting'Result = Handle (Context)
      and then
        Libadalang.Rewriting_Implementation.Context (Start_Rewriting'Result) =
        Context;
      --  Implementation for Rewriting.Start_Rewriting

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) with
      Post => Handle = No_Rewriting_Handle;
      --  Implementation for Rewriting.Abort_Rewriting

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Internal_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True =>
            null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result with
      Post =>
      (if Apply'Result.Success then Handle = No_Rewriting_Handle
       else Handle = Handle'Old);
      --  Implementation for Rewriting.Apply

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Implementation for Rewriting.Unit_Handles

   ---------------------------------------
   -- Implementation for unit rewriting --
   ---------------------------------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit;
   --  Implementation for Rewriting.Unit

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Root

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle; Root : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Root

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Implementation for Rewriting.Unparse

   ---------------------------------------
   -- Implementation for node rewriting --
   ---------------------------------------

   function Handle (Node : Bare_Ada_Node) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Handle

   function Node (Handle : Node_Rewriting_Handle) return Bare_Ada_Node;
   --  Implementation for Rewriting.Node

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Implementation for Rewriting.Context

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Unparse

   function Kind (Handle : Node_Rewriting_Handle) return Ada_Node_Kind_Type;
   --  Implementation for Rewriting.Kind

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Implementation for Rewriting.Tied

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Parent

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Implementation for Rewriting.Children_Count

   function Child
     (Handle : Node_Rewriting_Handle; Index : Positive)
      return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Child

   procedure Set_Child
     (Handle : Node_Rewriting_Handle; Index : Positive;
      Child  : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Set_Child

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Implementation for Rewriting.Text

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Implementation for Rewriting.Set_Text

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  Implementation for Rewriting.Replace

   --------------------------------------------
   -- Implementation for list node rewriting --
   --------------------------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle; Index : Positive;
      Child  : Node_Rewriting_Handle) with
      Post => Rewriting_Implementation.Child (Handle, Index) = Child;
      --  Implementation for Rewriting.Insert_Child

   procedure Append_Child
     (Handle : Node_Rewriting_Handle; Child : Node_Rewriting_Handle) with
      Post => Rewriting_Implementation.Child
        (Handle, Children_Count (Handle)) =
      Child;
      --  Implementation for Rewriting.Append_Child

   procedure Remove_Child (Handle : Node_Rewriting_Handle; Index : Positive);
   --  Implementation for Rewriting.Remove_Child

   --------------------------------------
   -- Implementation for node creation --
   --------------------------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Clone

   function Create_Node
     (Handle : Rewriting_Handle; Kind : Ada_Node_Kind_Type)
      return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Node

   function Create_Token_Node
     (Handle : Rewriting_Handle; Kind : Ada_Node_Kind_Type; Text : Text_Type)
      return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Token_Node

   function Create_Regular_Node
     (Handle   : Rewriting_Handle; Kind : Ada_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_Regular_Node

   ----------------------------------
   -- Implementation for templates --
   ----------------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle; Template : Text_Type;
      Arguments : Node_Rewriting_Handle_Array; Rule : Grammar_Rule)
      return Node_Rewriting_Handle;
   --  Implementation for Rewriting.Create_From_Template

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------

   function Create_Constrained_Array_Indices
     (Handle                           : Rewriting_Handle;
      Constrained_Array_Indices_F_List : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Unconstrained_Array_Indices
     (Handle                              : Rewriting_Handle;
      Unconstrained_Array_Indices_F_Types : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Aspect_Assoc
     (Handle : Rewriting_Handle; Aspect_Assoc_F_Id : Node_Rewriting_Handle;
      Aspect_Assoc_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_At_Clause
     (Handle : Rewriting_Handle; At_Clause_F_Name : Node_Rewriting_Handle;
      At_Clause_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Attribute_Def_Clause
     (Handle                                : Rewriting_Handle;
      Attribute_Def_Clause_F_Attribute_Expr : Node_Rewriting_Handle;
      Attribute_Def_Clause_F_Expr           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Enum_Rep_Clause
     (Handle                      : Rewriting_Handle;
      Enum_Rep_Clause_F_Type_Name : Node_Rewriting_Handle;
      Enum_Rep_Clause_F_Aggregate : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Record_Rep_Clause
     (Handle                         : Rewriting_Handle;
      Record_Rep_Clause_F_Name       : Node_Rewriting_Handle;
      Record_Rep_Clause_F_At_Expr    : Node_Rewriting_Handle;
      Record_Rep_Clause_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Aspect_Spec
     (Handle                      : Rewriting_Handle;
      Aspect_Spec_F_Aspect_Assocs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Contract_Case_Assoc
     (Handle                            : Rewriting_Handle;
      Contract_Case_Assoc_F_Guard       : Node_Rewriting_Handle;
      Contract_Case_Assoc_F_Consequence : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Pragma_Argument_Assoc
     (Handle                       : Rewriting_Handle;
      Pragma_Argument_Assoc_F_Id   : Node_Rewriting_Handle;
      Pragma_Argument_Assoc_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Entry_Spec
     (Handle                    : Rewriting_Handle;
      Entry_Spec_F_Entry_Name   : Node_Rewriting_Handle;
      Entry_Spec_F_Family_Type  : Node_Rewriting_Handle;
      Entry_Spec_F_Entry_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subp_Spec
     (Handle : Rewriting_Handle; Subp_Spec_F_Subp_Kind : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Name    : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Params  : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Returns : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Component_List
     (Handle                        : Rewriting_Handle;
      Component_List_F_Components   : Node_Rewriting_Handle;
      Component_List_F_Variant_Part : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Known_Discriminant_Part
     (Handle                                : Rewriting_Handle;
      Known_Discriminant_Part_F_Discr_Specs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Entry_Completion_Formal_Params
     (Handle                                  : Rewriting_Handle;
      Entry_Completion_Formal_Params_F_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Formal_Part
     (Handle                      : Rewriting_Handle;
      Generic_Formal_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Null_Record_Def
     (Handle                       : Rewriting_Handle;
      Base_Record_Def_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Record_Def
     (Handle                       : Rewriting_Handle;
      Base_Record_Def_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Aggregate_Assoc
     (Handle                        : Rewriting_Handle;
      Aggregate_Assoc_F_Designators : Node_Rewriting_Handle;
      Aggregate_Assoc_F_R_Expr      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Multi_Dim_Array_Assoc
     (Handle                        : Rewriting_Handle;
      Aggregate_Assoc_F_Designators : Node_Rewriting_Handle;
      Aggregate_Assoc_F_R_Expr      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Discriminant_Assoc
     (Handle                          : Rewriting_Handle;
      Discriminant_Assoc_F_Ids        : Node_Rewriting_Handle;
      Discriminant_Assoc_F_Discr_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Param_Assoc
     (Handle                   : Rewriting_Handle;
      Param_Assoc_F_Designator : Node_Rewriting_Handle;
      Param_Assoc_F_R_Expr     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Component_Decl
     (Handle : Rewriting_Handle; Component_Decl_F_Ids : Node_Rewriting_Handle;
      Component_Decl_F_Component_Def : Node_Rewriting_Handle;
      Component_Decl_F_Default_Expr  : Node_Rewriting_Handle;
      Component_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Discriminant_Spec
     (Handle                           : Rewriting_Handle;
      Discriminant_Spec_F_Ids          : Node_Rewriting_Handle;
      Discriminant_Spec_F_Type_Expr    : Node_Rewriting_Handle;
      Discriminant_Spec_F_Default_Expr : Node_Rewriting_Handle;
      Discriminant_Spec_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Formal_Obj_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Formal_Package
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Formal_Subp_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Formal_Type_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Param_Spec
     (Handle : Rewriting_Handle; Param_Spec_F_Ids : Node_Rewriting_Handle;
      Param_Spec_F_Has_Aliased  : Node_Rewriting_Handle;
      Param_Spec_F_Mode         : Node_Rewriting_Handle;
      Param_Spec_F_Type_Expr    : Node_Rewriting_Handle;
      Param_Spec_F_Default_Expr : Node_Rewriting_Handle;
      Param_Spec_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Package_Internal
     (Handle                           : Rewriting_Handle;
      Base_Package_Decl_F_Package_Name : Node_Rewriting_Handle;
      Base_Package_Decl_F_Aspects      : Node_Rewriting_Handle;
      Base_Package_Decl_F_Public_Part  : Node_Rewriting_Handle;
      Base_Package_Decl_F_Private_Part : Node_Rewriting_Handle;
      Base_Package_Decl_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Package_Decl
     (Handle                           : Rewriting_Handle;
      Base_Package_Decl_F_Package_Name : Node_Rewriting_Handle;
      Base_Package_Decl_F_Aspects      : Node_Rewriting_Handle;
      Base_Package_Decl_F_Public_Part  : Node_Rewriting_Handle;
      Base_Package_Decl_F_Private_Part : Node_Rewriting_Handle;
      Base_Package_Decl_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Discrete_Base_Subtype_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Discrete_Base_Subtype_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subtype_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Subtype_Decl_F_Subtype : Node_Rewriting_Handle;
      Subtype_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Classwide_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Classwide_Type_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Incomplete_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Incomplete_Tagged_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Discriminants       : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Aspects             : Node_Rewriting_Handle;
      Incomplete_Tagged_Type_Decl_F_Has_Abstract : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Protected_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Interfaces    : Node_Rewriting_Handle;
      Protected_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Task_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Task_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Task_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Task_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Single_Task_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Task_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Task_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Task_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Anonymous_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Synth_Anonymous_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Abstract_Subp_Decl
     (Handle                         : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec  : Node_Rewriting_Handle;
      Abstract_Subp_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Abstract_Formal_Subp_Decl
     (Handle                          : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding  : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec   : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Default_Expr : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Concrete_Formal_Subp_Decl
     (Handle                          : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding  : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec   : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Default_Expr : Node_Rewriting_Handle;
      Formal_Subp_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subp_Decl
     (Handle                         : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Decl_F_Aspects            : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Entry_Decl
     (Handle                  : Rewriting_Handle;
      Entry_Decl_F_Overriding : Node_Rewriting_Handle;
      Entry_Decl_F_Spec       : Node_Rewriting_Handle;
      Entry_Decl_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Enum_Literal_Decl
     (Handle                      : Rewriting_Handle;
      Enum_Literal_Decl_F_Name    : Node_Rewriting_Handle;
      Enum_Literal_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Subp_Internal
     (Handle                            : Rewriting_Handle;
      Generic_Subp_Internal_F_Subp_Spec : Node_Rewriting_Handle;
      Generic_Subp_Internal_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Expr_Function
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Expr_Function_F_Expr        : Node_Rewriting_Handle;
      Expr_Function_F_Aspects     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Null_Subp_Decl
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Null_Subp_Decl_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subp_Body
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Body_F_Aspects         : Node_Rewriting_Handle;
      Subp_Body_F_Decls           : Node_Rewriting_Handle;
      Subp_Body_F_Stmts           : Node_Rewriting_Handle;
      Subp_Body_F_End_Name        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subp_Renaming_Decl
     (Handle                       : Rewriting_Handle;
      Base_Subp_Body_F_Overriding  : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec   : Node_Rewriting_Handle;
      Subp_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Subp_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Package_Body_Stub
     (Handle                      : Rewriting_Handle;
      Package_Body_Stub_F_Name    : Node_Rewriting_Handle;
      Package_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Protected_Body_Stub
     (Handle                        : Rewriting_Handle;
      Protected_Body_Stub_F_Name    : Node_Rewriting_Handle;
      Protected_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subp_Body_Stub
     (Handle                      : Rewriting_Handle;
      Subp_Body_Stub_F_Overriding : Node_Rewriting_Handle;
      Subp_Body_Stub_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Body_Stub_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Task_Body_Stub
     (Handle : Rewriting_Handle; Task_Body_Stub_F_Name : Node_Rewriting_Handle;
      Task_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

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
      return Node_Rewriting_Handle;

   function Create_Package_Body
     (Handle                      : Rewriting_Handle;
      Package_Body_F_Package_Name : Node_Rewriting_Handle;
      Package_Body_F_Aspects      : Node_Rewriting_Handle;
      Package_Body_F_Decls        : Node_Rewriting_Handle;
      Package_Body_F_Stmts        : Node_Rewriting_Handle;
      Package_Body_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Protected_Body
     (Handle : Rewriting_Handle; Protected_Body_F_Name : Node_Rewriting_Handle;
      Protected_Body_F_Aspects  : Node_Rewriting_Handle;
      Protected_Body_F_Decls    : Node_Rewriting_Handle;
      Protected_Body_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Task_Body
     (Handle : Rewriting_Handle; Task_Body_F_Name : Node_Rewriting_Handle;
      Task_Body_F_Aspects  : Node_Rewriting_Handle;
      Task_Body_F_Decls    : Node_Rewriting_Handle;
      Task_Body_F_Stmts    : Node_Rewriting_Handle;
      Task_Body_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Entry_Index_Spec
     (Handle : Rewriting_Handle; Entry_Index_Spec_F_Id : Node_Rewriting_Handle;
      Entry_Index_Spec_F_Subtype : Node_Rewriting_Handle;
      Entry_Index_Spec_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Error_Decl
     (Handle : Rewriting_Handle; Error_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Exception_Decl
     (Handle : Rewriting_Handle; Exception_Decl_F_Ids : Node_Rewriting_Handle;
      Exception_Decl_F_Renames : Node_Rewriting_Handle;
      Exception_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Exception_Handler
     (Handle                                 : Rewriting_Handle;
      Exception_Handler_F_Exception_Name     : Node_Rewriting_Handle;
      Exception_Handler_F_Handled_Exceptions : Node_Rewriting_Handle;
      Exception_Handler_F_Stmts              : Node_Rewriting_Handle;
      Exception_Handler_F_Aspects            : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_For_Loop_Var_Decl
     (Handle                      : Rewriting_Handle;
      For_Loop_Var_Decl_F_Id      : Node_Rewriting_Handle;
      For_Loop_Var_Decl_F_Id_Type : Node_Rewriting_Handle;
      For_Loop_Var_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Package_Decl
     (Handle                              : Rewriting_Handle;
      Generic_Decl_F_Formal_Part          : Node_Rewriting_Handle;
      Generic_Package_Decl_F_Package_Decl : Node_Rewriting_Handle;
      Generic_Package_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Subp_Decl
     (Handle                        : Rewriting_Handle;
      Generic_Decl_F_Formal_Part    : Node_Rewriting_Handle;
      Generic_Subp_Decl_F_Subp_Decl : Node_Rewriting_Handle;
      Generic_Subp_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Package_Instantiation
     (Handle                                           : Rewriting_Handle;
      Generic_Package_Instantiation_F_Name             : Node_Rewriting_Handle;
      Generic_Package_Instantiation_F_Generic_Pkg_Name : Node_Rewriting_Handle;
      Generic_Package_Instantiation_F_Params           : Node_Rewriting_Handle;
      Generic_Package_Instantiation_F_Aspects          : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Subp_Instantiation
     (Handle                                         : Rewriting_Handle;
      Generic_Subp_Instantiation_F_Overriding        : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Kind              : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Subp_Name         : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Generic_Subp_Name : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Params            : Node_Rewriting_Handle;
      Generic_Subp_Instantiation_F_Aspects           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Package_Renaming_Decl
     (Handle                                  : Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Generic_Subp_Renaming_Decl
     (Handle                               : Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Kind    : Node_Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Generic_Subp_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Label_Decl
     (Handle : Rewriting_Handle; Label_Decl_F_Name : Node_Rewriting_Handle;
      Label_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Named_Stmt_Decl
     (Handle                    : Rewriting_Handle;
      Named_Stmt_Decl_F_Name    : Node_Rewriting_Handle;
      Named_Stmt_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Number_Decl
     (Handle : Rewriting_Handle; Number_Decl_F_Ids : Node_Rewriting_Handle;
      Number_Decl_F_Expr    : Node_Rewriting_Handle;
      Number_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Object_Decl
     (Handle : Rewriting_Handle; Object_Decl_F_Ids : Node_Rewriting_Handle;
      Object_Decl_F_Has_Aliased     : Node_Rewriting_Handle;
      Object_Decl_F_Has_Constant    : Node_Rewriting_Handle;
      Object_Decl_F_Mode            : Node_Rewriting_Handle;
      Object_Decl_F_Type_Expr       : Node_Rewriting_Handle;
      Object_Decl_F_Default_Expr    : Node_Rewriting_Handle;
      Object_Decl_F_Renaming_Clause : Node_Rewriting_Handle;
      Object_Decl_F_Aspects         : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Anonymous_Object_Decl
     (Handle : Rewriting_Handle; Object_Decl_F_Ids : Node_Rewriting_Handle;
      Object_Decl_F_Has_Aliased     : Node_Rewriting_Handle;
      Object_Decl_F_Has_Constant    : Node_Rewriting_Handle;
      Object_Decl_F_Mode            : Node_Rewriting_Handle;
      Object_Decl_F_Type_Expr       : Node_Rewriting_Handle;
      Object_Decl_F_Default_Expr    : Node_Rewriting_Handle;
      Object_Decl_F_Renaming_Clause : Node_Rewriting_Handle;
      Object_Decl_F_Aspects         : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Extended_Return_Stmt_Object_Decl
     (Handle : Rewriting_Handle; Object_Decl_F_Ids : Node_Rewriting_Handle;
      Object_Decl_F_Has_Aliased     : Node_Rewriting_Handle;
      Object_Decl_F_Has_Constant    : Node_Rewriting_Handle;
      Object_Decl_F_Mode            : Node_Rewriting_Handle;
      Object_Decl_F_Type_Expr       : Node_Rewriting_Handle;
      Object_Decl_F_Default_Expr    : Node_Rewriting_Handle;
      Object_Decl_F_Renaming_Clause : Node_Rewriting_Handle;
      Object_Decl_F_Aspects         : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Package_Renaming_Decl
     (Handle                          : Rewriting_Handle;
      Package_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Package_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Package_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Single_Protected_Decl
     (Handle                             : Rewriting_Handle;
      Single_Protected_Decl_F_Name       : Node_Rewriting_Handle;
      Single_Protected_Decl_F_Aspects    : Node_Rewriting_Handle;
      Single_Protected_Decl_F_Interfaces : Node_Rewriting_Handle;
      Single_Protected_Decl_F_Definition : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Single_Task_Decl
     (Handle                       : Rewriting_Handle;
      Single_Task_Decl_F_Task_Type : Node_Rewriting_Handle;
      Single_Task_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Case_Stmt_Alternative
     (Handle                          : Rewriting_Handle;
      Case_Stmt_Alternative_F_Choices : Node_Rewriting_Handle;
      Case_Stmt_Alternative_F_Stmts   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Compilation_Unit
     (Handle                     : Rewriting_Handle;
      Compilation_Unit_F_Prelude : Node_Rewriting_Handle;
      Compilation_Unit_F_Body    : Node_Rewriting_Handle;
      Compilation_Unit_F_Pragmas : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Component_Clause
     (Handle : Rewriting_Handle; Component_Clause_F_Id : Node_Rewriting_Handle;
      Component_Clause_F_Position : Node_Rewriting_Handle;
      Component_Clause_F_Range    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Component_Def
     (Handle                       : Rewriting_Handle;
      Component_Def_F_Has_Aliased  : Node_Rewriting_Handle;
      Component_Def_F_Has_Constant : Node_Rewriting_Handle;
      Component_Def_F_Type_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Delta_Constraint
     (Handle                    : Rewriting_Handle;
      Delta_Constraint_F_Digits : Node_Rewriting_Handle;
      Delta_Constraint_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Digits_Constraint
     (Handle                     : Rewriting_Handle;
      Digits_Constraint_F_Digits : Node_Rewriting_Handle;
      Digits_Constraint_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Discriminant_Constraint
     (Handle                                : Rewriting_Handle;
      Discriminant_Constraint_F_Constraints : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Index_Constraint
     (Handle                         : Rewriting_Handle;
      Index_Constraint_F_Constraints : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Range_Constraint
     (Handle                   : Rewriting_Handle;
      Range_Constraint_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Declarative_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Private_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Public_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Elsif_Expr_Part
     (Handle                      : Rewriting_Handle;
      Elsif_Expr_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Elsif_Expr_Part_F_Then_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Elsif_Stmt_Part
     (Handle                      : Rewriting_Handle;
      Elsif_Stmt_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Elsif_Stmt_Part_F_Stmts     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Allocator
     (Handle : Rewriting_Handle; Allocator_F_Subpool : Node_Rewriting_Handle;
      Allocator_F_Type_Or_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Aggregate
     (Handle                         : Rewriting_Handle;
      Base_Aggregate_F_Ancestor_Expr : Node_Rewriting_Handle;
      Base_Aggregate_F_Assocs        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Null_Record_Aggregate
     (Handle                         : Rewriting_Handle;
      Base_Aggregate_F_Ancestor_Expr : Node_Rewriting_Handle;
      Base_Aggregate_F_Assocs        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Bin_Op
     (Handle         : Rewriting_Handle; Bin_Op_F_Left : Node_Rewriting_Handle;
      Bin_Op_F_Op    : Node_Rewriting_Handle;
      Bin_Op_F_Right : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Relation_Op
     (Handle         : Rewriting_Handle; Bin_Op_F_Left : Node_Rewriting_Handle;
      Bin_Op_F_Op    : Node_Rewriting_Handle;
      Bin_Op_F_Right : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Case_Expr
     (Handle : Rewriting_Handle; Case_Expr_F_Expr : Node_Rewriting_Handle;
      Case_Expr_F_Cases : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Case_Expr_Alternative
     (Handle                          : Rewriting_Handle;
      Case_Expr_Alternative_F_Choices : Node_Rewriting_Handle;
      Case_Expr_Alternative_F_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Contract_Cases
     (Handle                          : Rewriting_Handle;
      Contract_Cases_F_Contract_Cases : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_If_Expr
     (Handle : Rewriting_Handle; If_Expr_F_Cond_Expr : Node_Rewriting_Handle;
      If_Expr_F_Then_Expr    : Node_Rewriting_Handle;
      If_Expr_F_Alternatives : Node_Rewriting_Handle;
      If_Expr_F_Else_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Membership_Expr
     (Handle                             : Rewriting_Handle;
      Membership_Expr_F_Expr             : Node_Rewriting_Handle;
      Membership_Expr_F_Op               : Node_Rewriting_Handle;
      Membership_Expr_F_Membership_Exprs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Attribute_Ref
     (Handle                    : Rewriting_Handle;
      Attribute_Ref_F_Prefix    : Node_Rewriting_Handle;
      Attribute_Ref_F_Attribute : Node_Rewriting_Handle;
      Attribute_Ref_F_Args      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Update_Attribute_Ref
     (Handle                    : Rewriting_Handle;
      Attribute_Ref_F_Prefix    : Node_Rewriting_Handle;
      Attribute_Ref_F_Attribute : Node_Rewriting_Handle;
      Attribute_Ref_F_Args      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Call_Expr
     (Handle : Rewriting_Handle; Call_Expr_F_Name : Node_Rewriting_Handle;
      Call_Expr_F_Suffix : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Defining_Name
     (Handle : Rewriting_Handle; Defining_Name_F_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Discrete_Subtype_Name
     (Handle                          : Rewriting_Handle;
      Discrete_Subtype_Name_F_Subtype : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Dotted_Name
     (Handle : Rewriting_Handle; Dotted_Name_F_Prefix : Node_Rewriting_Handle;
      Dotted_Name_F_Suffix : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_End_Name
     (Handle : Rewriting_Handle; End_Name_F_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Explicit_Deref
     (Handle                  : Rewriting_Handle;
      Explicit_Deref_F_Prefix : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Qual_Expr
     (Handle : Rewriting_Handle; Qual_Expr_F_Prefix : Node_Rewriting_Handle;
      Qual_Expr_F_Suffix : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Paren_Expr
     (Handle : Rewriting_Handle; Paren_Expr_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Quantified_Expr
     (Handle                       : Rewriting_Handle;
      Quantified_Expr_F_Quantifier : Node_Rewriting_Handle;
      Quantified_Expr_F_Loop_Spec  : Node_Rewriting_Handle;
      Quantified_Expr_F_Expr       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Raise_Expr
     (Handle                      : Rewriting_Handle;
      Raise_Expr_F_Exception_Name : Node_Rewriting_Handle;
      Raise_Expr_F_Error_Message  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Un_Op
     (Handle       : Rewriting_Handle; Un_Op_F_Op : Node_Rewriting_Handle;
      Un_Op_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Handled_Stmts
     (Handle : Rewriting_Handle; Handled_Stmts_F_Stmts : Node_Rewriting_Handle;
      Handled_Stmts_F_Exceptions : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Library_Item
     (Handle                     : Rewriting_Handle;
      Library_Item_F_Has_Private : Node_Rewriting_Handle;
      Library_Item_F_Item        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_For_Loop_Spec
     (Handle                      : Rewriting_Handle;
      For_Loop_Spec_F_Var_Decl    : Node_Rewriting_Handle;
      For_Loop_Spec_F_Loop_Type   : Node_Rewriting_Handle;
      For_Loop_Spec_F_Has_Reverse : Node_Rewriting_Handle;
      For_Loop_Spec_F_Iter_Expr   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_While_Loop_Spec
     (Handle                 : Rewriting_Handle;
      While_Loop_Spec_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Params
     (Handle : Rewriting_Handle; Params_F_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Pragma_Node
     (Handle : Rewriting_Handle; Pragma_Node_F_Id : Node_Rewriting_Handle;
      Pragma_Node_F_Args : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Protected_Def
     (Handle                       : Rewriting_Handle;
      Protected_Def_F_Public_Part  : Node_Rewriting_Handle;
      Protected_Def_F_Private_Part : Node_Rewriting_Handle;
      Protected_Def_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Range_Spec
     (Handle : Rewriting_Handle; Range_Spec_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Renaming_Clause
     (Handle                           : Rewriting_Handle;
      Renaming_Clause_F_Renamed_Object : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Synthetic_Renaming_Clause
     (Handle                           : Rewriting_Handle;
      Renaming_Clause_F_Renamed_Object : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Select_When_Part
     (Handle                       : Rewriting_Handle;
      Select_When_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Select_When_Part_F_Stmts     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Accept_Stmt
     (Handle : Rewriting_Handle; Accept_Stmt_F_Name : Node_Rewriting_Handle;
      Accept_Stmt_F_Entry_Index_Expr : Node_Rewriting_Handle;
      Accept_Stmt_F_Params           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Accept_Stmt_With_Stmts
     (Handle : Rewriting_Handle; Accept_Stmt_F_Name : Node_Rewriting_Handle;
      Accept_Stmt_F_Entry_Index_Expr    : Node_Rewriting_Handle;
      Accept_Stmt_F_Params              : Node_Rewriting_Handle;
      Accept_Stmt_With_Stmts_F_Stmts    : Node_Rewriting_Handle;
      Accept_Stmt_With_Stmts_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_For_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_While_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Begin_Block
     (Handle : Rewriting_Handle; Begin_Block_F_Stmts : Node_Rewriting_Handle;
      Begin_Block_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Decl_Block
     (Handle : Rewriting_Handle; Decl_Block_F_Decls : Node_Rewriting_Handle;
      Decl_Block_F_Stmts    : Node_Rewriting_Handle;
      Decl_Block_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Case_Stmt
     (Handle : Rewriting_Handle; Case_Stmt_F_Expr : Node_Rewriting_Handle;
      Case_Stmt_F_Alternatives : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Extended_Return_Stmt
     (Handle                       : Rewriting_Handle;
      Extended_Return_Stmt_F_Decl  : Node_Rewriting_Handle;
      Extended_Return_Stmt_F_Stmts : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_If_Stmt
     (Handle : Rewriting_Handle; If_Stmt_F_Cond_Expr : Node_Rewriting_Handle;
      If_Stmt_F_Then_Stmts   : Node_Rewriting_Handle;
      If_Stmt_F_Alternatives : Node_Rewriting_Handle;
      If_Stmt_F_Else_Stmts   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Named_Stmt
     (Handle : Rewriting_Handle; Named_Stmt_F_Decl : Node_Rewriting_Handle;
      Named_Stmt_F_Stmt : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Select_Stmt
     (Handle : Rewriting_Handle; Select_Stmt_F_Guards : Node_Rewriting_Handle;
      Select_Stmt_F_Else_Stmts  : Node_Rewriting_Handle;
      Select_Stmt_F_Abort_Stmts : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Abort_Stmt
     (Handle : Rewriting_Handle; Abort_Stmt_F_Names : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Assign_Stmt
     (Handle : Rewriting_Handle; Assign_Stmt_F_Dest : Node_Rewriting_Handle;
      Assign_Stmt_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Call_Stmt
     (Handle : Rewriting_Handle; Call_Stmt_F_Call : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Delay_Stmt
     (Handle                 : Rewriting_Handle;
      Delay_Stmt_F_Has_Until : Node_Rewriting_Handle;
      Delay_Stmt_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Exit_Stmt
     (Handle : Rewriting_Handle; Exit_Stmt_F_Loop_Name : Node_Rewriting_Handle;
      Exit_Stmt_F_Cond_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Goto_Stmt
     (Handle                 : Rewriting_Handle;
      Goto_Stmt_F_Label_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Label
     (Handle : Rewriting_Handle; Label_F_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Raise_Stmt
     (Handle                      : Rewriting_Handle;
      Raise_Stmt_F_Exception_Name : Node_Rewriting_Handle;
      Raise_Stmt_F_Error_Message  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Requeue_Stmt
     (Handle                   : Rewriting_Handle;
      Requeue_Stmt_F_Call_Name : Node_Rewriting_Handle;
      Requeue_Stmt_F_Has_Abort : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Return_Stmt
     (Handle                    : Rewriting_Handle;
      Return_Stmt_F_Return_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subunit
     (Handle : Rewriting_Handle; Subunit_F_Name : Node_Rewriting_Handle;
      Subunit_F_Body : Node_Rewriting_Handle) return Node_Rewriting_Handle;

   function Create_Task_Def
     (Handle : Rewriting_Handle; Task_Def_F_Interfaces : Node_Rewriting_Handle;
      Task_Def_F_Public_Part  : Node_Rewriting_Handle;
      Task_Def_F_Private_Part : Node_Rewriting_Handle;
      Task_Def_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Access_To_Subp_Def
     (Handle                             : Rewriting_Handle;
      Access_Def_F_Has_Not_Null          : Node_Rewriting_Handle;
      Access_To_Subp_Def_F_Has_Protected : Node_Rewriting_Handle;
      Access_To_Subp_Def_F_Subp_Spec     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Anonymous_Type_Access_Def
     (Handle                                : Rewriting_Handle;
      Access_Def_F_Has_Not_Null             : Node_Rewriting_Handle;
      Anonymous_Type_Access_Def_F_Type_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Type_Access_Def
     (Handle                               : Rewriting_Handle;
      Access_Def_F_Has_Not_Null            : Node_Rewriting_Handle;
      Type_Access_Def_F_Has_All            : Node_Rewriting_Handle;
      Type_Access_Def_F_Has_Constant       : Node_Rewriting_Handle;
      Type_Access_Def_F_Subtype_Indication : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Array_Type_Def
     (Handle                          : Rewriting_Handle;
      Array_Type_Def_F_Indices        : Node_Rewriting_Handle;
      Array_Type_Def_F_Component_Type : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Derived_Type_Def
     (Handle                                : Rewriting_Handle;
      Derived_Type_Def_F_Has_Abstract       : Node_Rewriting_Handle;
      Derived_Type_Def_F_Has_Limited        : Node_Rewriting_Handle;
      Derived_Type_Def_F_Has_Synchronized   : Node_Rewriting_Handle;
      Derived_Type_Def_F_Subtype_Indication : Node_Rewriting_Handle;
      Derived_Type_Def_F_Interfaces         : Node_Rewriting_Handle;
      Derived_Type_Def_F_Record_Extension   : Node_Rewriting_Handle;
      Derived_Type_Def_F_Has_With_Private   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Enum_Type_Def
     (Handle                        : Rewriting_Handle;
      Enum_Type_Def_F_Enum_Literals : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Interface_Type_Def
     (Handle                              : Rewriting_Handle;
      Interface_Type_Def_F_Interface_Kind : Node_Rewriting_Handle;
      Interface_Type_Def_F_Interfaces     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Mod_Int_Type_Def
     (Handle                  : Rewriting_Handle;
      Mod_Int_Type_Def_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Private_Type_Def
     (Handle                          : Rewriting_Handle;
      Private_Type_Def_F_Has_Abstract : Node_Rewriting_Handle;
      Private_Type_Def_F_Has_Tagged   : Node_Rewriting_Handle;
      Private_Type_Def_F_Has_Limited  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Decimal_Fixed_Point_Def
     (Handle                           : Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Delta  : Node_Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Digits : Node_Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Floating_Point_Def
     (Handle                          : Rewriting_Handle;
      Floating_Point_Def_F_Num_Digits : Node_Rewriting_Handle;
      Floating_Point_Def_F_Range      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Ordinary_Fixed_Point_Def
     (Handle                           : Rewriting_Handle;
      Ordinary_Fixed_Point_Def_F_Delta : Node_Rewriting_Handle;
      Ordinary_Fixed_Point_Def_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Record_Type_Def
     (Handle                         : Rewriting_Handle;
      Record_Type_Def_F_Has_Abstract : Node_Rewriting_Handle;
      Record_Type_Def_F_Has_Tagged   : Node_Rewriting_Handle;
      Record_Type_Def_F_Has_Limited  : Node_Rewriting_Handle;
      Record_Type_Def_F_Record_Def   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Signed_Int_Type_Def
     (Handle                      : Rewriting_Handle;
      Signed_Int_Type_Def_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Anonymous_Type
     (Handle                     : Rewriting_Handle;
      Anonymous_Type_F_Type_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Constrained_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Discrete_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Unconstrained_Array_Index
     (Handle                                         : Rewriting_Handle;
      Unconstrained_Array_Index_F_Subtype_Indication : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Use_Package_Clause
     (Handle                        : Rewriting_Handle;
      Use_Package_Clause_F_Packages : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Use_Type_Clause
     (Handle                    : Rewriting_Handle;
      Use_Type_Clause_F_Has_All : Node_Rewriting_Handle;
      Use_Type_Clause_F_Types   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Variant
     (Handle : Rewriting_Handle; Variant_F_Choices : Node_Rewriting_Handle;
      Variant_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_Variant_Part
     (Handle                    : Rewriting_Handle;
      Variant_Part_F_Discr_Name : Node_Rewriting_Handle;
      Variant_Part_F_Variant    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   function Create_With_Clause
     (Handle                    : Rewriting_Handle;
      With_Clause_F_Has_Limited : Node_Rewriting_Handle;
      With_Clause_F_Has_Private : Node_Rewriting_Handle;
      With_Clause_F_Packages    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

end Libadalang.Rewriting_Implementation;
