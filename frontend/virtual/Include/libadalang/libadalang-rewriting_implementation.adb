with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Libadalang.Common; use Libadalang.Common;
use Libadalang.Common.Token_Data_Handlers;
with Libadalang.Implementation;
with Libadalang.Lexer_Implementation; use Libadalang.Lexer_Implementation;

with Libadalang.Unparsing_Implementation;
use Libadalang.Unparsing_Implementation;

package body Libadalang.Rewriting_Implementation is

   --  Access to rewriting handle data is always done through Rewriting_Handle,
   --  values (never through Rewriting_Handle_Pointer, except after calling
   --  Convert), so there is no aliasing problem possible.

   pragma Warnings (Off, "possible aliasing problem for type");
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Rewriting_Handle_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle_Pointer, Rewriting_Handle);
   pragma Warnings (On, "possible aliasing problem for type");

   procedure Pre_Check (Value : Boolean; Msg : String);
   --  Raise a Precondition_Failure exception with the given message if the
   --  Value is False.

   ---------------
   -- Pre_Check --
   ---------------

   procedure Pre_Check (Value : Boolean; Msg : String) is
   begin
      if not Value then
         raise Precondition_Failure with Msg;
      end if;
   end Pre_Check;

   function Handle (Context : Internal_Context) return Rewriting_Handle is
     (Convert (Get_Rewriting_Handle (Context)));

   function Context (Handle : Rewriting_Handle) return Internal_Context is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Handle.Context;
   end Context;

   function Allocate
     (Kind          : Ada_Node_Kind_Type; Context : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle with
      Pre => Context /= No_Rewriting_Handle
      and then
      (Unit_Handle = No_Unit_Rewriting_Handle
       or else Unit_Handle.Context_Handle = Context)
      and then
      (Parent_Handle = No_Node_Rewriting_Handle
       or else Parent_Handle.Context_Handle = Context);

   function Allocate
     (Node          : Bare_Ada_Node; Context : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle with
      Pre => Context /= No_Rewriting_Handle
      and then
      (Unit_Handle = No_Unit_Rewriting_Handle
       or else Unit_Handle.Context_Handle = Context)
      and then
      (Parent_Handle = No_Node_Rewriting_Handle
       or else Parent_Handle.Context_Handle = Context);
      --  Allocate a handle for Node and register it in Unit_Handle's map

   procedure Expand_Children (Node : Node_Rewriting_Handle) with
      Pre => Node /= No_Node_Rewriting_Handle;
      --  If Node.Children.Kind is Unexpanded, populate Node's list of Children
      --  to mimic the related bare AST node. Otherwise, do nothing.

   procedure Free_Handles (Handle : in out Rewriting_Handle);
   --  Free all resources tied to Handle. This also releases the rewriting
   --  handle singleton in Handle's Context.

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle; Unit : Unit_Rewriting_Handle);
   --  Tie the node represented by handle so that either:
   --
   --    * it is the root of Unit (Parent is null);
   --    * it is a child of Parent (Unit is null).
   --
   --  Do nothing if Handle is null.

   procedure Untie (Handle : Node_Rewriting_Handle);
   --  Untie the node represented by Handle. Do nothing if Handle is null.

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle
   is
   begin

      Pre_Check
        (Handle (Context) = No_Rewriting_Handle,
         "Handle (Context) must be null");

      declare
         Result : constant Rewriting_Handle :=
           new Rewriting_Handle_Type'
             (Context   => Context, Units => <>, Pool => Create,
              New_Nodes => <>);
      begin
         Result.New_Nodes := Nodes_Pools.Create (Result.Pool);
         Set_Rewriting_Handle (Context, Convert (Result));
         return Result;
      end;
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      Free_Handles (Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is

      type Processed_Unit_Record is record
         Unit     : Internal_Unit;
         New_Data : Reparsed_Unit;
      end record;
      type Processed_Unit is access Processed_Unit_Record;
      procedure Free is new Ada.Unchecked_Deallocation
        (Processed_Unit_Record, Processed_Unit);

      package Processed_Unit_Vectors is new Ada.Containers.Vectors
        (Positive, Processed_Unit);

      Units  : Processed_Unit_Vectors.Vector;
      Result : Apply_Result := (Success => True);

   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      --  Try to reparse all units that were potentially modified
      for Unit_Handle of Handle.Units loop
         declare
            PU : constant Processed_Unit :=
              new Processed_Unit_Record'
                (Unit => Unit_Handle.Unit, New_Data => <>);
            Input : Internal_Lexer_Input :=
              (Kind  => Bytes_Buffer, Charset => <>, Read_BOM => False,
               Bytes => System.Null_Address, Bytes_Count => 0);
            Bytes : String_Access;
         begin
            Units.Append (PU);

            --  Reparse (i.e. unparse and then parse) this rewritten unit
            Bytes :=
              Unparse
                (Create_Abstract_Node (Unit_Handle.Root), PU.Unit,
                 Preserve_Formatting => True, As_Unit => True);
            Input.Charset     := Unit_Handle.Unit.Charset;
            Input.Bytes       := Bytes.all'Address;
            Input.Bytes_Count := Bytes.all'Length;
            Do_Parsing (PU.Unit, Input, PU.New_Data);
            Free (Bytes);

            --  If there is a parsing error, abort the rewriting process
            if not PU.New_Data.Diagnostics.Is_Empty then
               Result :=
                 (Success => False, Unit => PU.Unit, Diagnostics => <>);
               Result.Diagnostics.Move (PU.New_Data.Diagnostics);
               Destroy (PU.New_Data);
               exit;
            end if;
         end;
      end loop;

      --  If all reparsing went fine, actually replace the AST nodes all over
      --  the context and free all resources associated to Handle.
      if Result.Success then
         for PU of Units loop
            Update_After_Reparse (PU.Unit, PU.New_Data);
         end loop;
         Free_Handles (Handle);
      end if;

      --  Clean-up our local resources and return
      for PU of Units loop
         Free (PU);
      end loop;
      return Result;
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      declare
         Count  : constant Natural := Natural (Handle.Units.Length);
         Result : Unit_Rewriting_Handle_Array (1 .. Count);
         I      : Positive         := 1;
      begin
         for Unit of Handle.Units loop
            Result (I) := Unit;
            I          := I + 1;
         end loop;
         return Result;
      end;
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle is
   begin

      Pre_Check
        (Handle (Context (Unit)) /= No_Rewriting_Handle,
         "Handle (Context (Unit)) should not be null");

      Pre_Check (not Has_Diagnostics (Unit), "Unit must not have diagnostics");

      declare
         use Unit_Maps;

         Context        : constant Internal_Context := Unit.Context;
         Context_Handle : constant Rewriting_Handle := Handle (Context);
         Filename       : constant Unbounded_String :=
           To_Unbounded_String (Get_Filename (Unit));

         Cur : constant Cursor := Context_Handle.Units.Find (Filename);
      begin
         if Cur /= No_Element then
            return Element (Cur);
         end if;

         declare
            Result : constant Unit_Rewriting_Handle :=
              new Unit_Rewriting_Handle_Type'
                (Context_Handle => Context_Handle, Unit => Unit, Root => <>,
                 Nodes          => <>);
         begin
            Context_Handle.Units.Insert (Filename, Result);
            Result.Root := Handle (Root (Unit));
            return Result;
         end;
      end;
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit is
   begin

      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle, "Handle should not be null");

      return Handle.Unit;
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle, "Handle should not be null");

      return Handle.Root;
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle; Root : Node_Rewriting_Handle)
   is
   begin

      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Root = No_Node_Rewriting_Handle or else not Tied (Root),
         "Root must not be tied to another rewriting context.");

      Untie (Handle.Root);
      Handle.Root := Root;
      Tie (Root, No_Node_Rewriting_Handle, Handle);
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type
   is
   begin

      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle, "Handle should not be null");

      return Unparsing_Implementation.Unparse
          (Node => Create_Abstract_Node (Handle.Root), Unit => Handle.Unit,
           Preserve_Formatting => True, As_Unit => True);
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle (Node : Bare_Ada_Node) return Node_Rewriting_Handle is
   begin

      Pre_Check
        (Handle (Context (Node.Unit)) /= No_Rewriting_Handle,
         "Handle (Context (Node.Unit)) should not be null");

      Pre_Check
        (not Has_Diagnostics (Node.Unit),
         "Node.Unit must not have diagnostics");

      if Node = null then
         return No_Node_Rewriting_Handle;
      end if;

      declare
         use Node_Maps;

         Unit_Handle : constant Unit_Rewriting_Handle := Handle (Node.Unit);
         Cur         : constant Cursor := Unit_Handle.Nodes.Find (Node);
      begin
         --  If we have already built a handle for this node, just return it
         if Cur /= No_Element then
            return Element (Cur);

            --  Otherwise, if this node has a parent, make sure this parent has
            --  its own handle, then expand its children. This last must create
            --  the handle we are supposed to return.
         elsif Node.Parent /= null then
            Expand_Children (Handle (Node.Parent));
            return Element (Unit_Handle.Nodes.Find (Node));
         end if;

         --  Otherwise, we are dealing with the root node: just create its
         --  rewriting handle.
         return Allocate
             (Node, Unit_Handle.Context_Handle, Unit_Handle,
              No_Node_Rewriting_Handle);
      end;
   end Handle;

   ----------
   -- Node --
   ----------

   function Node (Handle : Node_Rewriting_Handle) return Bare_Ada_Node is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return Handle.Node;
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return Handle.Context_Handle;
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return To_Wide_Wide_String
          (Unparsing_Implementation.Unparse
             (Create_Abstract_Node (Handle), Unit => null,
              Preserve_Formatting                 => True, As_Unit => False));
   end Unparse;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Kind          : Ada_Node_Kind_Type; Context : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      Tied : constant Boolean := Unit_Handle /= No_Unit_Rewriting_Handle;
   begin
      return new Node_Rewriting_Handle_Type'
          (Context_Handle => Context, Node => null, Parent => Parent_Handle,
           Kind           => Kind, Tied => Tied,
           Root_Of        =>
             (if Tied and then Parent_Handle = No_Node_Rewriting_Handle then
                Unit_Handle
              else No_Unit_Rewriting_Handle),
           Children => Unexpanded_Children);
   end Allocate;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Node          : Bare_Ada_Node; Context : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      Result : constant Node_Rewriting_Handle :=
        Allocate (Node.Kind, Context, Unit_Handle, Parent_Handle);
   begin
      Result.Node := Node;
      if Result.Tied then
         Unit_Handle.Nodes.Insert (Node, Result);
      end if;
      return Result;
   end Allocate;

   ---------------------
   -- Expand_Children --
   ---------------------

   procedure Expand_Children (Node : Node_Rewriting_Handle) is
      Children : Node_Children renames Node.Children;
   begin
      --  If this handle has already be expanded, there is nothing to do
      if Children.Kind /= Unexpanded then
         return;
      end if;

      --  Otherwise, expand to the appropriate children form: token node or
      --  regular one.
      declare
         N           : constant Bare_Ada_Node         := Node.Node;
         Unit_Handle : constant Unit_Rewriting_Handle := Handle (N.Unit);
      begin
         if Is_Token_Node (N) then
            Children :=
              (Kind => Expanded_Token_Node,
               Text => To_Unbounded_Wide_Wide_String (Text (N)));

         else
            Children := (Kind => Expanded_Regular, Vector => <>);
            declare
               Count : constant Natural := Children_Count (N);
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  declare
                     Child : constant Bare_Ada_Node :=
                       Implementation.Child (N, I);
                  begin
                     Children.Vector.Append
                       ((if Child = null then null
                         else Allocate
                             (Child, Unit_Handle.Context_Handle, Unit_Handle,
                              Node)));
                  end;
               end loop;
            end;
         end if;
      end;
   end Expand_Children;

   ------------------
   -- Free_Handles --
   ------------------

   procedure Free_Handles (Handle : in out Rewriting_Handle) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Rewriting_Handle_Type, Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Unit_Rewriting_Handle_Type, Unit_Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Rewriting_Handle_Type, Node_Rewriting_Handle);

      Ctx : constant Internal_Context := Context (Handle);
   begin
      --  Free all resources tied to Handle
      for Unit of Handle.Units loop
         for Node of Unit.Nodes loop
            Free (Node);
         end loop;
         Free (Unit);
      end loop;
      for Node of Handle.New_Nodes loop
         declare
            N : Node_Rewriting_Handle := Node;
         begin
            Free (N);
         end;
      end loop;
      Free (Handle.Pool);
      Free (Handle);

      --  Release the rewriting handle singleton for its context
      Set_Rewriting_Handle (Ctx, Convert (Handle));
   end Free_Handles;

   ---------
   -- Tie --
   ---------

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle; Unit : Unit_Rewriting_Handle)
   is
   begin
      if Handle /= No_Node_Rewriting_Handle then
         Handle.Parent := Parent;
         Handle.Tied   := True;
         if Parent = No_Node_Rewriting_Handle then
            Handle.Root_Of := Unit;
         end if;
      end if;
   end Tie;

   -----------
   -- Untie --
   -----------

   procedure Untie (Handle : Node_Rewriting_Handle) is
   begin
      if Handle /= No_Node_Rewriting_Handle then
         Handle.Parent  := No_Node_Rewriting_Handle;
         Handle.Tied    := False;
         Handle.Root_Of := No_Unit_Rewriting_Handle;
      end if;
   end Untie;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return Ada_Node_Kind_Type is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return Handle.Kind;
   end Kind;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return Handle.Tied;
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return Handle.Parent;
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      return
        (case Handle.Children.Kind is
           when Unexpanded          => Children_Count (Handle.Node),
           when Expanded_Regular    => Natural (Handle.Children.Vector.Length),
           when Expanded_Token_Node => 0);
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle; Index : Positive)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      if Index > Children_Count (Handle) then
         raise Precondition_Failure
           with "Invalid index " & Index'Image & ": Handle has " &
           Children_Count (Handle)'Image & " children";
      end if;

      --  If this handle represents an already existing node, make sure it is
      --  expanded so we have a handle to return.
      Expand_Children (Handle);
      return Handle.Children.Vector.Element (Index);
   end Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle; Index : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      if Index > Children_Count (Handle) then
         raise Precondition_Failure
           with "Invalid index " & Index'Image & ": Handle has " &
           Children_Count (Handle)'Image & " children";
      end if;

      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");

      --  If this handle represents an already existing node, make sure it is
      --  expanded so that its children vector can be modified.
      Expand_Children (Handle);

      declare
         Child_Slot : Node_Rewriting_Handle renames
           Handle.Children.Vector.Reference (Index);
      begin
         --  Untie the child to be replaced if it exists
         Untie (Child_Slot);

         --  Tie the new child if it exists
         Tie (Child, Handle, No_Unit_Rewriting_Handle);

         Child_Slot := Child;
      end;
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Is_Token_Node (Kind (Handle)),
         "Expected a token node. Got " & Kind (Handle)'Image);

      case Handle.Children.Kind is
         when Unexpanded =>
            if Is_Token_Node (Handle.Kind) then
               return Text (Handle.Node);
            else
               raise Program_Error;
            end if;
         when Expanded_Regular =>
            return (raise Program_Error);
         when Expanded_Token_Node =>
            return To_Wide_Wide_String (Handle.Children.Text);
      end case;
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Is_Token_Node (Kind (Handle)),
         "Expected a token node. Got " & Kind (Handle)'Image);

      --  Make sure Handle is expanded so we have a Text field to override
      Expand_Children (Handle);

      Handle.Children.Text := To_Unbounded_Wide_Wide_String (Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      Pre_Check (Tied (Handle), "Handle must be tied to an analysis unit.");

      Pre_Check
        (New_Node = No_Node_Rewriting_Handle or else not Tied (New_Node),
         "New_Node must not be tied to another rewriting context.");

      if Handle = New_Node then
         return;
      end if;

      if Handle.Root_Of = No_Unit_Rewriting_Handle then
         --  If Handle is not the root node of its owning unit, go replace it
         --  in its parent's children list.
         declare
            Parent : Node_Rewriting_Handle renames Handle.Parent;
            Index  : Natural := 0;
         begin
            for I in 1 .. Children_Count (Parent) loop
               if Child (Parent, I) = Handle then
                  Index := I;
                  exit;
               end if;
            end loop;
            pragma Assert (Index > 0);
            Set_Child (Parent, Index, New_Node);
         end;

      else
         --  Otherwise, replace it as a root node
         Set_Root (Handle.Root_Of, New_Node);
      end if;
   end Replace;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle; Index : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);

      if Index > Children_Count (Handle) + 1 then
         raise Precondition_Failure
           with "Invalid index " & Index'Image & ": Handle has " &
           Children_Count (Handle)'Image & " children";
      end if;

      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");

      --  First, just create room for the new node and let Set_Child take care
      --  of tiding Child to Handle's tree.
      Expand_Children (Handle);
      Handle.Children.Vector.Insert (Index, No_Node_Rewriting_Handle);
      Set_Child (Handle, Index, Child);
   end Insert_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Handle : Node_Rewriting_Handle; Child : Node_Rewriting_Handle)
   is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);

      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");

      Insert_Child (Handle, Children_Count (Handle) + 1, Child);
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle; Index : Positive) is
   begin

      Pre_Check
        (Handle /= No_Node_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);

      if Index > Children_Count (Handle) + 1 then
         raise Precondition_Failure
           with "Invalid index " & Index'Image & ": Handle has " &
           Children_Count (Handle)'Image & " children";
      end if;

      --  First, let Set_Child take care of untiding the child to remove, and
      --  then actually remove the corresponding children list slot.
      Set_Child (Handle, Index, No_Node_Rewriting_Handle);
      Handle.Children.Vector.Delete (Index);
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle;
   begin
      if Handle = No_Node_Rewriting_Handle then
         return Handle;
      end if;

      --  Make sure the original handle is expanded so we can iterate on it
      Expand_Children (Handle);

      --  If the input handle is associated to a node, so should be the cloned
      --  handle, so that its formatting is copied as well.
      Result :=
        (if Handle.Node = null then
           Allocate
             (Handle.Kind, Handle.Context_Handle, No_Unit_Rewriting_Handle,
              No_Node_Rewriting_Handle)
         else Allocate
             (Handle.Node, Handle.Context_Handle, No_Unit_Rewriting_Handle,
              No_Node_Rewriting_Handle));
      Nodes_Pools.Append (Handle.Context_Handle.New_Nodes, Result);

      --  Recursively clone children
      case Handle.Children.Kind is
         when Unexpanded =>
            raise Program_Error;

         when Expanded_Token_Node =>
            Result.Children :=
              (Kind => Expanded_Token_Node, Text => Handle.Children.Text);

         when Expanded_Regular =>
            Result.Children := (Kind => Expanded_Regular, Vector => <>);
            Result.Children.Vector.Reserve_Capacity
              (Handle.Children.Vector.Length);
            for I in 1 .. Handle.Children.Vector.Last_Index loop
               Result.Children.Vector.Append
                 (Clone (Handle.Children.Vector.Element (I)));
            end loop;
      end case;

      return Result;
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle; Kind : Ada_Node_Kind_Type)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      if Is_Token_Node (Kind) then
         return Create_Token_Node (Handle, Kind, "");
      else
         declare
            Count    : constant Integer := Kind_To_Node_Children_Count (Kind);
            Children : constant Node_Rewriting_Handle_Array (1 .. Count) :=
              (others => No_Node_Rewriting_Handle);
         begin
            return Create_Regular_Node (Handle, Kind, Children);
         end;
      end if;
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle; Kind : Ada_Node_Kind_Type; Text : Text_Type)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (Is_Token_Node (Kind), "Expected a token node. Got " & Kind'Image);

      declare
         Result : constant Node_Rewriting_Handle :=
           Allocate
             (Kind, Handle, No_Unit_Rewriting_Handle,
              No_Node_Rewriting_Handle);
      begin
         Result.Children :=
           (Kind => Expanded_Token_Node,
            Text => To_Unbounded_Wide_Wide_String (Text));
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle; Kind : Ada_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      Pre_Check
        (not Is_Token_Node (Kind), "Expected a token node. Got " & Kind'Image);

      for One_Child of Children loop

         Pre_Check
           (One_Child = No_Node_Rewriting_Handle or else not Tied (One_Child),
            "One_Child must not be tied to another rewriting context.");

      end loop;

      declare
         Result : Node_Rewriting_Handle :=
           Allocate
             (Kind, Handle, No_Unit_Rewriting_Handle,
              No_Node_Rewriting_Handle);
      begin
         Result.Children := (Kind => Expanded_Regular, Vector => <>);
         Result.Children.Vector.Reserve_Capacity (Children'Length);
         for C of Children loop
            Result.Children.Vector.Append (C);
            if C /= No_Node_Rewriting_Handle then
               Tie (C, Result, No_Unit_Rewriting_Handle);
            end if;
         end loop;
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle; Template : Text_Type;
      Arguments : Node_Rewriting_Handle_Array; Rule : Grammar_Rule)
      return Node_Rewriting_Handle
   is
      type State_Type is (Default,
      --  Default state: no meta character being processed

Open_Brace,
      --  The previous character is a open brace: the current one determines
      --  what it means.

Close_Brace
         --  The previous character is a closing brace: the current one must be
         --  another closing brace.
         );

      Buffer   : Unbounded_Wide_Wide_String;
      State    : State_Type := Default;
      Next_Arg : Positive   := Arguments'First;
   begin
      for One_Argument of Arguments loop

         Pre_Check
           (One_Argument = No_Node_Rewriting_Handle
            or else Context (One_Argument) = Handle,
            "One_Argument should be associated to rewriting context Handle.");

      end loop;

      --  Interpret the template looping over its characters with a state
      --  machine.
      for C of Template loop
         case State is
            when Default =>
               case C is
                  when '{' =>
                     State := Open_Brace;
                  when '}' =>
                     State := Close_Brace;
                  when others =>
                     Append (Buffer, C);
               end case;

            when Open_Brace =>
               case C is
                  when '{' =>
                     State := Default;
                     Append (Buffer, C);
                  when '}' =>
                     State := Default;
                     if Next_Arg in Arguments'Range then
                        declare
                           Unparsed_Arg : constant Wide_Wide_String :=
                             Unparse (Arguments (Next_Arg));
                        begin
                           Next_Arg := Next_Arg + 1;
                           Append (Buffer, Unparsed_Arg);
                        end;
                     else
                        raise Template_Args_Error
                          with "not enough arguments provided";
                     end if;
                  when others =>
                     raise Template_Format_Error
                       with "standalone ""{"" character";
               end case;

            when Close_Brace =>
               case C is
                  when '}' =>
                     State := Default;
                     Append (Buffer, C);
                  when others =>
                     raise Template_Format_Error
                       with "standalone ""}"" character";
               end case;
         end case;
      end loop;

      --  Make sure that there is no standalone metacharacter at the end of the
      --  template.
      case State is
         when Default =>
            null;
         when Open_Brace =>
            raise Template_Format_Error with "standalone ""{"" character";
         when Close_Brace =>
            raise Template_Format_Error with "standalone ""}"" character";
      end case;

      --  Make sure all given arguments were consumed
      if Next_Arg in Arguments'Range then
         raise Template_Args_Error with "too many arguments provided";
      end if;

      --  Now parse the resulting buffer and create the corresponding tree of
      --  nodes.
      declare
         Context : constant Internal_Context :=
           Rewriting_Implementation.Context (Handle);
         Unit     : constant Internal_Unit        := Templates_Unit (Context);
         Reparsed : Reparsed_Unit;
         Text     : constant Text_Type := To_Wide_Wide_String (Buffer);
         Input    : constant Internal_Lexer_Input :=
           (Kind       => Text_Buffer, Text => Text'Address,
            Text_Count => Text'Length);

         function Transform
           (Node : Bare_Ada_Node; Parent : Node_Rewriting_Handle)
            return Node_Rewriting_Handle;
         --  Turn a node from the Reparsed unit into a recursively expanded
         --  node rewriting handle.

         ---------------
         -- Transform --
         ---------------

         function Transform
           (Node : Bare_Ada_Node; Parent : Node_Rewriting_Handle)
            return Node_Rewriting_Handle
         is
            Result : Node_Rewriting_Handle;
         begin
            if Node = null then
               return No_Node_Rewriting_Handle;
            end if;

            --  Allocate the handle for Node, and don't forget to remove the
            --  backlink to Node itself as it exists only temporarily for
            --  template instantiation. Also, track the newly allocated node
            --  so that it is freed correctly upon destruction of the rewriting
            --  context.
            Result :=
              Allocate (Node, Handle, No_Unit_Rewriting_Handle, Parent);
            Result.Node := null;
            Nodes_Pools.Append (Handle.New_Nodes, Result);

            if Is_Token_Node (Node) then
               declare
                  Index : constant Natural := Natural (Node.Token_Start_Index);
                  Data  : constant Stored_Token_Data :=
                    Reparsed.TDH.Tokens.Get (Index);
                  Text : constant Text_Type :=
                    Reparsed.TDH.Source_Buffer
                      (Data.Source_First .. Data.Source_Last);
               begin
                  Result.Children :=
                    (Kind => Expanded_Token_Node,
                     Text => To_Unbounded_Wide_Wide_String (Text));
               end;

            else
               declare
                  Count : constant Natural := Children_Count (Node);
               begin
                  Result.Children := (Kind => Expanded_Regular, Vector => <>);
                  Result.Children.Vector.Reserve_Capacity
                    (Ada.Containers.Count_Type (Count));
                  for I in 1 .. Count loop
                     Result.Children.Vector.Append
                       (Transform (Child (Node, I), Result));
                  end loop;
               end;
            end if;
            return Result;
         end Transform;

      begin
         Set_Rule (Unit, Rule);
         Do_Parsing (Unit, Input, Reparsed);
         if not Reparsed.Diagnostics.Is_Empty then
            Destroy (Reparsed);
            raise Template_Instantiation_Error;
         end if;

         declare
            Result : constant Node_Rewriting_Handle :=
              Transform (Reparsed.AST_Root, No_Node_Rewriting_Handle);
         begin
            Destroy (Reparsed);
            return Result;
         end;
      end;
   end Create_From_Template;

   function Create_Constrained_Array_Indices
     (Handle                           : Rewriting_Handle;
      Constrained_Array_Indices_F_List : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Constrained_Array_Indices,
           (1 => Constrained_Array_Indices_F_List));
   end Create_Constrained_Array_Indices;

   function Create_Unconstrained_Array_Indices
     (Handle                              : Rewriting_Handle;
      Unconstrained_Array_Indices_F_Types : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Unconstrained_Array_Indices,
           (1 => Unconstrained_Array_Indices_F_Types));
   end Create_Unconstrained_Array_Indices;

   function Create_Aspect_Assoc
     (Handle : Rewriting_Handle; Aspect_Assoc_F_Id : Node_Rewriting_Handle;
      Aspect_Assoc_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Aspect_Assoc,
           (1 => Aspect_Assoc_F_Id, 2 => Aspect_Assoc_F_Expr));
   end Create_Aspect_Assoc;

   function Create_At_Clause
     (Handle : Rewriting_Handle; At_Clause_F_Name : Node_Rewriting_Handle;
      At_Clause_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_At_Clause,
           (1 => At_Clause_F_Name, 2 => At_Clause_F_Expr));
   end Create_At_Clause;

   function Create_Attribute_Def_Clause
     (Handle                                : Rewriting_Handle;
      Attribute_Def_Clause_F_Attribute_Expr : Node_Rewriting_Handle;
      Attribute_Def_Clause_F_Expr           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Attribute_Def_Clause,
           (1 => Attribute_Def_Clause_F_Attribute_Expr,
            2 => Attribute_Def_Clause_F_Expr));
   end Create_Attribute_Def_Clause;

   function Create_Enum_Rep_Clause
     (Handle                      : Rewriting_Handle;
      Enum_Rep_Clause_F_Type_Name : Node_Rewriting_Handle;
      Enum_Rep_Clause_F_Aggregate : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Enum_Rep_Clause,
           (1 => Enum_Rep_Clause_F_Type_Name,
            2 => Enum_Rep_Clause_F_Aggregate));
   end Create_Enum_Rep_Clause;

   function Create_Record_Rep_Clause
     (Handle                         : Rewriting_Handle;
      Record_Rep_Clause_F_Name       : Node_Rewriting_Handle;
      Record_Rep_Clause_F_At_Expr    : Node_Rewriting_Handle;
      Record_Rep_Clause_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Record_Rep_Clause,
           (1 => Record_Rep_Clause_F_Name, 2 => Record_Rep_Clause_F_At_Expr,
            3 => Record_Rep_Clause_F_Components));
   end Create_Record_Rep_Clause;

   function Create_Aspect_Spec
     (Handle                      : Rewriting_Handle;
      Aspect_Spec_F_Aspect_Assocs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Aspect_Spec, (1 => Aspect_Spec_F_Aspect_Assocs));
   end Create_Aspect_Spec;

   function Create_Contract_Case_Assoc
     (Handle                            : Rewriting_Handle;
      Contract_Case_Assoc_F_Guard       : Node_Rewriting_Handle;
      Contract_Case_Assoc_F_Consequence : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Contract_Case_Assoc,
           (1 => Contract_Case_Assoc_F_Guard,
            2 => Contract_Case_Assoc_F_Consequence));
   end Create_Contract_Case_Assoc;

   function Create_Pragma_Argument_Assoc
     (Handle                       : Rewriting_Handle;
      Pragma_Argument_Assoc_F_Id   : Node_Rewriting_Handle;
      Pragma_Argument_Assoc_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Pragma_Argument_Assoc,
           (1 => Pragma_Argument_Assoc_F_Id,
            2 => Pragma_Argument_Assoc_F_Expr));
   end Create_Pragma_Argument_Assoc;

   function Create_Entry_Spec
     (Handle                    : Rewriting_Handle;
      Entry_Spec_F_Entry_Name   : Node_Rewriting_Handle;
      Entry_Spec_F_Family_Type  : Node_Rewriting_Handle;
      Entry_Spec_F_Entry_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Entry_Spec,
           (1 => Entry_Spec_F_Entry_Name, 2 => Entry_Spec_F_Family_Type,
            3 => Entry_Spec_F_Entry_Params));
   end Create_Entry_Spec;

   function Create_Subp_Spec
     (Handle : Rewriting_Handle; Subp_Spec_F_Subp_Kind : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Name    : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Params  : Node_Rewriting_Handle;
      Subp_Spec_F_Subp_Returns : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subp_Spec,
           (1 => Subp_Spec_F_Subp_Kind, 2 => Subp_Spec_F_Subp_Name,
            3 => Subp_Spec_F_Subp_Params, 4 => Subp_Spec_F_Subp_Returns));
   end Create_Subp_Spec;

   function Create_Component_List
     (Handle                        : Rewriting_Handle;
      Component_List_F_Components   : Node_Rewriting_Handle;
      Component_List_F_Variant_Part : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Component_List,
           (1 => Component_List_F_Components,
            2 => Component_List_F_Variant_Part));
   end Create_Component_List;

   function Create_Known_Discriminant_Part
     (Handle                                : Rewriting_Handle;
      Known_Discriminant_Part_F_Discr_Specs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Known_Discriminant_Part,
           (1 => Known_Discriminant_Part_F_Discr_Specs));
   end Create_Known_Discriminant_Part;

   function Create_Entry_Completion_Formal_Params
     (Handle                                  : Rewriting_Handle;
      Entry_Completion_Formal_Params_F_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Entry_Completion_Formal_Params,
           (1 => Entry_Completion_Formal_Params_F_Params));
   end Create_Entry_Completion_Formal_Params;

   function Create_Generic_Formal_Part
     (Handle                      : Rewriting_Handle;
      Generic_Formal_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Formal_Part,
           (1 => Generic_Formal_Part_F_Decls));
   end Create_Generic_Formal_Part;

   function Create_Null_Record_Def
     (Handle                       : Rewriting_Handle;
      Base_Record_Def_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Null_Record_Def, (1 => Base_Record_Def_F_Components));
   end Create_Null_Record_Def;

   function Create_Record_Def
     (Handle                       : Rewriting_Handle;
      Base_Record_Def_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Record_Def, (1 => Base_Record_Def_F_Components));
   end Create_Record_Def;

   function Create_Aggregate_Assoc
     (Handle                        : Rewriting_Handle;
      Aggregate_Assoc_F_Designators : Node_Rewriting_Handle;
      Aggregate_Assoc_F_R_Expr      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Aggregate_Assoc,
           (1 => Aggregate_Assoc_F_Designators,
            2 => Aggregate_Assoc_F_R_Expr));
   end Create_Aggregate_Assoc;

   function Create_Multi_Dim_Array_Assoc
     (Handle                        : Rewriting_Handle;
      Aggregate_Assoc_F_Designators : Node_Rewriting_Handle;
      Aggregate_Assoc_F_R_Expr      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Multi_Dim_Array_Assoc,
           (1 => Aggregate_Assoc_F_Designators,
            2 => Aggregate_Assoc_F_R_Expr));
   end Create_Multi_Dim_Array_Assoc;

   function Create_Discriminant_Assoc
     (Handle                          : Rewriting_Handle;
      Discriminant_Assoc_F_Ids        : Node_Rewriting_Handle;
      Discriminant_Assoc_F_Discr_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Discriminant_Assoc,
           (1 => Discriminant_Assoc_F_Ids,
            2 => Discriminant_Assoc_F_Discr_Expr));
   end Create_Discriminant_Assoc;

   function Create_Param_Assoc
     (Handle                   : Rewriting_Handle;
      Param_Assoc_F_Designator : Node_Rewriting_Handle;
      Param_Assoc_F_R_Expr     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Param_Assoc,
           (1 => Param_Assoc_F_Designator, 2 => Param_Assoc_F_R_Expr));
   end Create_Param_Assoc;

   function Create_Component_Decl
     (Handle : Rewriting_Handle; Component_Decl_F_Ids : Node_Rewriting_Handle;
      Component_Decl_F_Component_Def : Node_Rewriting_Handle;
      Component_Decl_F_Default_Expr  : Node_Rewriting_Handle;
      Component_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Component_Decl,
           (1 => Component_Decl_F_Ids, 2 => Component_Decl_F_Component_Def,
            3 => Component_Decl_F_Default_Expr,
            4 => Component_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Discriminant_Spec,
           (1 => Discriminant_Spec_F_Ids, 2 => Discriminant_Spec_F_Type_Expr,
            3 => Discriminant_Spec_F_Default_Expr,
            4 => Discriminant_Spec_F_Aspects));
   end Create_Discriminant_Spec;

   function Create_Generic_Formal_Obj_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Formal_Obj_Decl,
           (1 => Generic_Formal_F_Decl, 2 => Generic_Formal_F_Aspects));
   end Create_Generic_Formal_Obj_Decl;

   function Create_Generic_Formal_Package
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Formal_Package,
           (1 => Generic_Formal_F_Decl, 2 => Generic_Formal_F_Aspects));
   end Create_Generic_Formal_Package;

   function Create_Generic_Formal_Subp_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Formal_Subp_Decl,
           (1 => Generic_Formal_F_Decl, 2 => Generic_Formal_F_Aspects));
   end Create_Generic_Formal_Subp_Decl;

   function Create_Generic_Formal_Type_Decl
     (Handle : Rewriting_Handle; Generic_Formal_F_Decl : Node_Rewriting_Handle;
      Generic_Formal_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Formal_Type_Decl,
           (1 => Generic_Formal_F_Decl, 2 => Generic_Formal_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Param_Spec,
           (1 => Param_Spec_F_Ids, 2 => Param_Spec_F_Has_Aliased,
            3 => Param_Spec_F_Mode, 4 => Param_Spec_F_Type_Expr,
            5 => Param_Spec_F_Default_Expr, 6 => Param_Spec_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Package_Internal,
           (1 => Base_Package_Decl_F_Package_Name,
            2 => Base_Package_Decl_F_Aspects,
            3 => Base_Package_Decl_F_Public_Part,
            4 => Base_Package_Decl_F_Private_Part,
            5 => Base_Package_Decl_F_End_Name));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Package_Decl,
           (1 => Base_Package_Decl_F_Package_Name,
            2 => Base_Package_Decl_F_Aspects,
            3 => Base_Package_Decl_F_Public_Part,
            4 => Base_Package_Decl_F_Private_Part,
            5 => Base_Package_Decl_F_End_Name));
   end Create_Package_Decl;

   function Create_Discrete_Base_Subtype_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Discrete_Base_Subtype_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Discrete_Base_Subtype_Decl,
           (1 => Base_Type_Decl_F_Name,
            2 => Discrete_Base_Subtype_Decl_F_Aspects));
   end Create_Discrete_Base_Subtype_Decl;

   function Create_Subtype_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Subtype_Decl_F_Subtype : Node_Rewriting_Handle;
      Subtype_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subtype_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Subtype_Decl_F_Subtype,
            3 => Subtype_Decl_F_Aspects));
   end Create_Subtype_Decl;

   function Create_Classwide_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Classwide_Type_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Classwide_Type_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Classwide_Type_Decl_F_Aspects));
   end Create_Classwide_Type_Decl;

   function Create_Incomplete_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Aspects       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Incomplete_Type_Decl,
           (1 => Base_Type_Decl_F_Name,
            2 => Incomplete_Type_Decl_F_Discriminants,
            3 => Incomplete_Type_Decl_F_Aspects));
   end Create_Incomplete_Type_Decl;

   function Create_Incomplete_Tagged_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Discriminants       : Node_Rewriting_Handle;
      Incomplete_Type_Decl_F_Aspects             : Node_Rewriting_Handle;
      Incomplete_Tagged_Type_Decl_F_Has_Abstract : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Incomplete_Tagged_Type_Decl,
           (1 => Base_Type_Decl_F_Name,
            2 => Incomplete_Type_Decl_F_Discriminants,
            3 => Incomplete_Type_Decl_F_Aspects,
            4 => Incomplete_Tagged_Type_Decl_F_Has_Abstract));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Protected_Type_Decl,
           (1 => Base_Type_Decl_F_Name,
            2 => Protected_Type_Decl_F_Discriminants,
            3 => Protected_Type_Decl_F_Aspects,
            4 => Protected_Type_Decl_F_Interfaces,
            5 => Protected_Type_Decl_F_Definition));
   end Create_Protected_Type_Decl;

   function Create_Task_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Task_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Task_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Task_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Task_Type_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Task_Type_Decl_F_Discriminants,
            3 => Task_Type_Decl_F_Aspects, 4 => Task_Type_Decl_F_Definition));
   end Create_Task_Type_Decl;

   function Create_Single_Task_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Task_Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Task_Type_Decl_F_Aspects       : Node_Rewriting_Handle;
      Task_Type_Decl_F_Definition    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Single_Task_Type_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Task_Type_Decl_F_Discriminants,
            3 => Task_Type_Decl_F_Aspects, 4 => Task_Type_Decl_F_Definition));
   end Create_Single_Task_Type_Decl;

   function Create_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Type_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Type_Decl_F_Discriminants,
            3 => Type_Decl_F_Type_Def, 4 => Type_Decl_F_Aspects));
   end Create_Type_Decl;

   function Create_Anonymous_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Anonymous_Type_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Type_Decl_F_Discriminants,
            3 => Type_Decl_F_Type_Def, 4 => Type_Decl_F_Aspects));
   end Create_Anonymous_Type_Decl;

   function Create_Synth_Anonymous_Type_Decl
     (Handle : Rewriting_Handle; Base_Type_Decl_F_Name : Node_Rewriting_Handle;
      Type_Decl_F_Discriminants : Node_Rewriting_Handle;
      Type_Decl_F_Type_Def      : Node_Rewriting_Handle;
      Type_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Synth_Anonymous_Type_Decl,
           (1 => Base_Type_Decl_F_Name, 2 => Type_Decl_F_Discriminants,
            3 => Type_Decl_F_Type_Def, 4 => Type_Decl_F_Aspects));
   end Create_Synth_Anonymous_Type_Decl;

   function Create_Abstract_Subp_Decl
     (Handle                         : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec  : Node_Rewriting_Handle;
      Abstract_Subp_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Abstract_Subp_Decl,
           (1 => Classic_Subp_Decl_F_Overriding,
            2 => Classic_Subp_Decl_F_Subp_Spec,
            3 => Abstract_Subp_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Abstract_Formal_Subp_Decl,
           (1 => Classic_Subp_Decl_F_Overriding,
            2 => Classic_Subp_Decl_F_Subp_Spec,
            3 => Formal_Subp_Decl_F_Default_Expr,
            4 => Formal_Subp_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Concrete_Formal_Subp_Decl,
           (1 => Classic_Subp_Decl_F_Overriding,
            2 => Classic_Subp_Decl_F_Subp_Spec,
            3 => Formal_Subp_Decl_F_Default_Expr,
            4 => Formal_Subp_Decl_F_Aspects));
   end Create_Concrete_Formal_Subp_Decl;

   function Create_Subp_Decl
     (Handle                         : Rewriting_Handle;
      Classic_Subp_Decl_F_Overriding : Node_Rewriting_Handle;
      Classic_Subp_Decl_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Decl_F_Aspects : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subp_Decl,
           (1 => Classic_Subp_Decl_F_Overriding,
            2 => Classic_Subp_Decl_F_Subp_Spec, 3 => Subp_Decl_F_Aspects));
   end Create_Subp_Decl;

   function Create_Entry_Decl
     (Handle                  : Rewriting_Handle;
      Entry_Decl_F_Overriding : Node_Rewriting_Handle;
      Entry_Decl_F_Spec       : Node_Rewriting_Handle;
      Entry_Decl_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Entry_Decl,
           (1 => Entry_Decl_F_Overriding, 2 => Entry_Decl_F_Spec,
            3 => Entry_Decl_F_Aspects));
   end Create_Entry_Decl;

   function Create_Enum_Literal_Decl
     (Handle                      : Rewriting_Handle;
      Enum_Literal_Decl_F_Name    : Node_Rewriting_Handle;
      Enum_Literal_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Enum_Literal_Decl,
           (1 => Enum_Literal_Decl_F_Name, 2 => Enum_Literal_Decl_F_Aspects));
   end Create_Enum_Literal_Decl;

   function Create_Generic_Subp_Internal
     (Handle                            : Rewriting_Handle;
      Generic_Subp_Internal_F_Subp_Spec : Node_Rewriting_Handle;
      Generic_Subp_Internal_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Subp_Internal,
           (1 => Generic_Subp_Internal_F_Subp_Spec,
            2 => Generic_Subp_Internal_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Expr_Function,
           (1 => Base_Subp_Body_F_Overriding, 2 => Base_Subp_Body_F_Subp_Spec,
            3 => Expr_Function_F_Expr, 4 => Expr_Function_F_Aspects));
   end Create_Expr_Function;

   function Create_Null_Subp_Decl
     (Handle                      : Rewriting_Handle;
      Base_Subp_Body_F_Overriding : Node_Rewriting_Handle;
      Base_Subp_Body_F_Subp_Spec  : Node_Rewriting_Handle;
      Null_Subp_Decl_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Null_Subp_Decl,
           (1 => Base_Subp_Body_F_Overriding, 2 => Base_Subp_Body_F_Subp_Spec,
            3 => Null_Subp_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subp_Body,
           (1 => Base_Subp_Body_F_Overriding, 2 => Base_Subp_Body_F_Subp_Spec,
            3 => Subp_Body_F_Aspects, 4 => Subp_Body_F_Decls,
            5 => Subp_Body_F_Stmts, 6 => Subp_Body_F_End_Name));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subp_Renaming_Decl,
           (1 => Base_Subp_Body_F_Overriding, 2 => Base_Subp_Body_F_Subp_Spec,
            3 => Subp_Renaming_Decl_F_Renames,
            4 => Subp_Renaming_Decl_F_Aspects));
   end Create_Subp_Renaming_Decl;

   function Create_Package_Body_Stub
     (Handle                      : Rewriting_Handle;
      Package_Body_Stub_F_Name    : Node_Rewriting_Handle;
      Package_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Package_Body_Stub,
           (1 => Package_Body_Stub_F_Name, 2 => Package_Body_Stub_F_Aspects));
   end Create_Package_Body_Stub;

   function Create_Protected_Body_Stub
     (Handle                        : Rewriting_Handle;
      Protected_Body_Stub_F_Name    : Node_Rewriting_Handle;
      Protected_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Protected_Body_Stub,
           (1 => Protected_Body_Stub_F_Name,
            2 => Protected_Body_Stub_F_Aspects));
   end Create_Protected_Body_Stub;

   function Create_Subp_Body_Stub
     (Handle                      : Rewriting_Handle;
      Subp_Body_Stub_F_Overriding : Node_Rewriting_Handle;
      Subp_Body_Stub_F_Subp_Spec  : Node_Rewriting_Handle;
      Subp_Body_Stub_F_Aspects    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subp_Body_Stub,
           (1 => Subp_Body_Stub_F_Overriding, 2 => Subp_Body_Stub_F_Subp_Spec,
            3 => Subp_Body_Stub_F_Aspects));
   end Create_Subp_Body_Stub;

   function Create_Task_Body_Stub
     (Handle : Rewriting_Handle; Task_Body_Stub_F_Name : Node_Rewriting_Handle;
      Task_Body_Stub_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Task_Body_Stub,
           (1 => Task_Body_Stub_F_Name, 2 => Task_Body_Stub_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Entry_Body,
           (1 => Entry_Body_F_Entry_Name, 2 => Entry_Body_F_Index_Spec,
            3 => Entry_Body_F_Params, 4 => Entry_Body_F_Barrier,
            5 => Entry_Body_F_Decls, 6 => Entry_Body_F_Stmts,
            7 => Entry_Body_F_End_Name, 8 => Entry_Body_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Package_Body,
           (1 => Package_Body_F_Package_Name, 2 => Package_Body_F_Aspects,
            3 => Package_Body_F_Decls, 4 => Package_Body_F_Stmts,
            5 => Package_Body_F_End_Name));
   end Create_Package_Body;

   function Create_Protected_Body
     (Handle : Rewriting_Handle; Protected_Body_F_Name : Node_Rewriting_Handle;
      Protected_Body_F_Aspects  : Node_Rewriting_Handle;
      Protected_Body_F_Decls    : Node_Rewriting_Handle;
      Protected_Body_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Protected_Body,
           (1 => Protected_Body_F_Name, 2 => Protected_Body_F_Aspects,
            3 => Protected_Body_F_Decls, 4 => Protected_Body_F_End_Name));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Task_Body,
           (1 => Task_Body_F_Name, 2 => Task_Body_F_Aspects,
            3 => Task_Body_F_Decls, 4 => Task_Body_F_Stmts,
            5 => Task_Body_F_End_Name));
   end Create_Task_Body;

   function Create_Entry_Index_Spec
     (Handle : Rewriting_Handle; Entry_Index_Spec_F_Id : Node_Rewriting_Handle;
      Entry_Index_Spec_F_Subtype : Node_Rewriting_Handle;
      Entry_Index_Spec_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Entry_Index_Spec,
           (1 => Entry_Index_Spec_F_Id, 2 => Entry_Index_Spec_F_Subtype,
            3 => Entry_Index_Spec_F_Aspects));
   end Create_Entry_Index_Spec;

   function Create_Error_Decl
     (Handle : Rewriting_Handle; Error_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Error_Decl, (1 => Error_Decl_F_Aspects));
   end Create_Error_Decl;

   function Create_Exception_Decl
     (Handle : Rewriting_Handle; Exception_Decl_F_Ids : Node_Rewriting_Handle;
      Exception_Decl_F_Renames : Node_Rewriting_Handle;
      Exception_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Exception_Decl,
           (1 => Exception_Decl_F_Ids, 2 => Exception_Decl_F_Renames,
            3 => Exception_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Exception_Handler,
           (1 => Exception_Handler_F_Exception_Name,
            2 => Exception_Handler_F_Handled_Exceptions,
            3 => Exception_Handler_F_Stmts, 4 => Exception_Handler_F_Aspects));
   end Create_Exception_Handler;

   function Create_For_Loop_Var_Decl
     (Handle                      : Rewriting_Handle;
      For_Loop_Var_Decl_F_Id      : Node_Rewriting_Handle;
      For_Loop_Var_Decl_F_Id_Type : Node_Rewriting_Handle;
      For_Loop_Var_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_For_Loop_Var_Decl,
           (1 => For_Loop_Var_Decl_F_Id, 2 => For_Loop_Var_Decl_F_Id_Type,
            3 => For_Loop_Var_Decl_F_Aspects));
   end Create_For_Loop_Var_Decl;

   function Create_Generic_Package_Decl
     (Handle                              : Rewriting_Handle;
      Generic_Decl_F_Formal_Part          : Node_Rewriting_Handle;
      Generic_Package_Decl_F_Package_Decl : Node_Rewriting_Handle;
      Generic_Package_Decl_F_Aspects      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Package_Decl,
           (1 => Generic_Decl_F_Formal_Part,
            2 => Generic_Package_Decl_F_Package_Decl,
            3 => Generic_Package_Decl_F_Aspects));
   end Create_Generic_Package_Decl;

   function Create_Generic_Subp_Decl
     (Handle                        : Rewriting_Handle;
      Generic_Decl_F_Formal_Part    : Node_Rewriting_Handle;
      Generic_Subp_Decl_F_Subp_Decl : Node_Rewriting_Handle;
      Generic_Subp_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Subp_Decl,
           (1 => Generic_Decl_F_Formal_Part,
            2 => Generic_Subp_Decl_F_Subp_Decl,
            3 => Generic_Subp_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Package_Instantiation,
           (1 => Generic_Package_Instantiation_F_Name,
            2 => Generic_Package_Instantiation_F_Generic_Pkg_Name,
            3 => Generic_Package_Instantiation_F_Params,
            4 => Generic_Package_Instantiation_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Subp_Instantiation,
           (1 => Generic_Subp_Instantiation_F_Overriding,
            2 => Generic_Subp_Instantiation_F_Kind,
            3 => Generic_Subp_Instantiation_F_Subp_Name,
            4 => Generic_Subp_Instantiation_F_Generic_Subp_Name,
            5 => Generic_Subp_Instantiation_F_Params,
            6 => Generic_Subp_Instantiation_F_Aspects));
   end Create_Generic_Subp_Instantiation;

   function Create_Generic_Package_Renaming_Decl
     (Handle                                  : Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Generic_Package_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Package_Renaming_Decl,
           (1 => Generic_Package_Renaming_Decl_F_Name,
            2 => Generic_Package_Renaming_Decl_F_Renames,
            3 => Generic_Package_Renaming_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Generic_Subp_Renaming_Decl,
           (1 => Generic_Subp_Renaming_Decl_F_Kind,
            2 => Generic_Subp_Renaming_Decl_F_Name,
            3 => Generic_Subp_Renaming_Decl_F_Renames,
            4 => Generic_Subp_Renaming_Decl_F_Aspects));
   end Create_Generic_Subp_Renaming_Decl;

   function Create_Label_Decl
     (Handle : Rewriting_Handle; Label_Decl_F_Name : Node_Rewriting_Handle;
      Label_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Label_Decl,
           (1 => Label_Decl_F_Name, 2 => Label_Decl_F_Aspects));
   end Create_Label_Decl;

   function Create_Named_Stmt_Decl
     (Handle                    : Rewriting_Handle;
      Named_Stmt_Decl_F_Name    : Node_Rewriting_Handle;
      Named_Stmt_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Named_Stmt_Decl,
           (1 => Named_Stmt_Decl_F_Name, 2 => Named_Stmt_Decl_F_Aspects));
   end Create_Named_Stmt_Decl;

   function Create_Number_Decl
     (Handle : Rewriting_Handle; Number_Decl_F_Ids : Node_Rewriting_Handle;
      Number_Decl_F_Expr    : Node_Rewriting_Handle;
      Number_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Number_Decl,
           (1 => Number_Decl_F_Ids, 2 => Number_Decl_F_Expr,
            3 => Number_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Object_Decl,
           (1 => Object_Decl_F_Ids, 2 => Object_Decl_F_Has_Aliased,
            3 => Object_Decl_F_Has_Constant, 4 => Object_Decl_F_Mode,
            5 => Object_Decl_F_Type_Expr, 6 => Object_Decl_F_Default_Expr,
            7 => Object_Decl_F_Renaming_Clause, 8 => Object_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Anonymous_Object_Decl,
           (1 => Object_Decl_F_Ids, 2 => Object_Decl_F_Has_Aliased,
            3 => Object_Decl_F_Has_Constant, 4 => Object_Decl_F_Mode,
            5 => Object_Decl_F_Type_Expr, 6 => Object_Decl_F_Default_Expr,
            7 => Object_Decl_F_Renaming_Clause, 8 => Object_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Extended_Return_Stmt_Object_Decl,
           (1 => Object_Decl_F_Ids, 2 => Object_Decl_F_Has_Aliased,
            3 => Object_Decl_F_Has_Constant, 4 => Object_Decl_F_Mode,
            5 => Object_Decl_F_Type_Expr, 6 => Object_Decl_F_Default_Expr,
            7 => Object_Decl_F_Renaming_Clause, 8 => Object_Decl_F_Aspects));
   end Create_Extended_Return_Stmt_Object_Decl;

   function Create_Package_Renaming_Decl
     (Handle                          : Rewriting_Handle;
      Package_Renaming_Decl_F_Name    : Node_Rewriting_Handle;
      Package_Renaming_Decl_F_Renames : Node_Rewriting_Handle;
      Package_Renaming_Decl_F_Aspects : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Package_Renaming_Decl,
           (1 => Package_Renaming_Decl_F_Name,
            2 => Package_Renaming_Decl_F_Renames,
            3 => Package_Renaming_Decl_F_Aspects));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Single_Protected_Decl,
           (1 => Single_Protected_Decl_F_Name,
            2 => Single_Protected_Decl_F_Aspects,
            3 => Single_Protected_Decl_F_Interfaces,
            4 => Single_Protected_Decl_F_Definition));
   end Create_Single_Protected_Decl;

   function Create_Single_Task_Decl
     (Handle                       : Rewriting_Handle;
      Single_Task_Decl_F_Task_Type : Node_Rewriting_Handle;
      Single_Task_Decl_F_Aspects   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Single_Task_Decl,
           (1 => Single_Task_Decl_F_Task_Type,
            2 => Single_Task_Decl_F_Aspects));
   end Create_Single_Task_Decl;

   function Create_Case_Stmt_Alternative
     (Handle                          : Rewriting_Handle;
      Case_Stmt_Alternative_F_Choices : Node_Rewriting_Handle;
      Case_Stmt_Alternative_F_Stmts   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Case_Stmt_Alternative,
           (1 => Case_Stmt_Alternative_F_Choices,
            2 => Case_Stmt_Alternative_F_Stmts));
   end Create_Case_Stmt_Alternative;

   function Create_Compilation_Unit
     (Handle                     : Rewriting_Handle;
      Compilation_Unit_F_Prelude : Node_Rewriting_Handle;
      Compilation_Unit_F_Body    : Node_Rewriting_Handle;
      Compilation_Unit_F_Pragmas : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Compilation_Unit,
           (1 => Compilation_Unit_F_Prelude, 2 => Compilation_Unit_F_Body,
            3 => Compilation_Unit_F_Pragmas));
   end Create_Compilation_Unit;

   function Create_Component_Clause
     (Handle : Rewriting_Handle; Component_Clause_F_Id : Node_Rewriting_Handle;
      Component_Clause_F_Position : Node_Rewriting_Handle;
      Component_Clause_F_Range    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Component_Clause,
           (1 => Component_Clause_F_Id, 2 => Component_Clause_F_Position,
            3 => Component_Clause_F_Range));
   end Create_Component_Clause;

   function Create_Component_Def
     (Handle                       : Rewriting_Handle;
      Component_Def_F_Has_Aliased  : Node_Rewriting_Handle;
      Component_Def_F_Has_Constant : Node_Rewriting_Handle;
      Component_Def_F_Type_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Component_Def,
           (1 => Component_Def_F_Has_Aliased,
            2 => Component_Def_F_Has_Constant,
            3 => Component_Def_F_Type_Expr));
   end Create_Component_Def;

   function Create_Delta_Constraint
     (Handle                    : Rewriting_Handle;
      Delta_Constraint_F_Digits : Node_Rewriting_Handle;
      Delta_Constraint_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Delta_Constraint,
           (1 => Delta_Constraint_F_Digits, 2 => Delta_Constraint_F_Range));
   end Create_Delta_Constraint;

   function Create_Digits_Constraint
     (Handle                     : Rewriting_Handle;
      Digits_Constraint_F_Digits : Node_Rewriting_Handle;
      Digits_Constraint_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Digits_Constraint,
           (1 => Digits_Constraint_F_Digits, 2 => Digits_Constraint_F_Range));
   end Create_Digits_Constraint;

   function Create_Discriminant_Constraint
     (Handle                                : Rewriting_Handle;
      Discriminant_Constraint_F_Constraints : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Discriminant_Constraint,
           (1 => Discriminant_Constraint_F_Constraints));
   end Create_Discriminant_Constraint;

   function Create_Index_Constraint
     (Handle                         : Rewriting_Handle;
      Index_Constraint_F_Constraints : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Index_Constraint,
           (1 => Index_Constraint_F_Constraints));
   end Create_Index_Constraint;

   function Create_Range_Constraint
     (Handle                   : Rewriting_Handle;
      Range_Constraint_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Range_Constraint, (1 => Range_Constraint_F_Range));
   end Create_Range_Constraint;

   function Create_Declarative_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Declarative_Part, (1 => Declarative_Part_F_Decls));
   end Create_Declarative_Part;

   function Create_Private_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Private_Part, (1 => Declarative_Part_F_Decls));
   end Create_Private_Part;

   function Create_Public_Part
     (Handle                   : Rewriting_Handle;
      Declarative_Part_F_Decls : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Public_Part, (1 => Declarative_Part_F_Decls));
   end Create_Public_Part;

   function Create_Elsif_Expr_Part
     (Handle                      : Rewriting_Handle;
      Elsif_Expr_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Elsif_Expr_Part_F_Then_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Elsif_Expr_Part,
           (1 => Elsif_Expr_Part_F_Cond_Expr,
            2 => Elsif_Expr_Part_F_Then_Expr));
   end Create_Elsif_Expr_Part;

   function Create_Elsif_Stmt_Part
     (Handle                      : Rewriting_Handle;
      Elsif_Stmt_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Elsif_Stmt_Part_F_Stmts     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Elsif_Stmt_Part,
           (1 => Elsif_Stmt_Part_F_Cond_Expr, 2 => Elsif_Stmt_Part_F_Stmts));
   end Create_Elsif_Stmt_Part;

   function Create_Allocator
     (Handle : Rewriting_Handle; Allocator_F_Subpool : Node_Rewriting_Handle;
      Allocator_F_Type_Or_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Allocator,
           (1 => Allocator_F_Subpool, 2 => Allocator_F_Type_Or_Expr));
   end Create_Allocator;

   function Create_Aggregate
     (Handle                         : Rewriting_Handle;
      Base_Aggregate_F_Ancestor_Expr : Node_Rewriting_Handle;
      Base_Aggregate_F_Assocs        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Aggregate,
           (1 => Base_Aggregate_F_Ancestor_Expr,
            2 => Base_Aggregate_F_Assocs));
   end Create_Aggregate;

   function Create_Null_Record_Aggregate
     (Handle                         : Rewriting_Handle;
      Base_Aggregate_F_Ancestor_Expr : Node_Rewriting_Handle;
      Base_Aggregate_F_Assocs        : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Null_Record_Aggregate,
           (1 => Base_Aggregate_F_Ancestor_Expr,
            2 => Base_Aggregate_F_Assocs));
   end Create_Null_Record_Aggregate;

   function Create_Bin_Op
     (Handle         : Rewriting_Handle; Bin_Op_F_Left : Node_Rewriting_Handle;
      Bin_Op_F_Op    : Node_Rewriting_Handle;
      Bin_Op_F_Right : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Bin_Op,
           (1 => Bin_Op_F_Left, 2 => Bin_Op_F_Op, 3 => Bin_Op_F_Right));
   end Create_Bin_Op;

   function Create_Relation_Op
     (Handle         : Rewriting_Handle; Bin_Op_F_Left : Node_Rewriting_Handle;
      Bin_Op_F_Op    : Node_Rewriting_Handle;
      Bin_Op_F_Right : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Relation_Op,
           (1 => Bin_Op_F_Left, 2 => Bin_Op_F_Op, 3 => Bin_Op_F_Right));
   end Create_Relation_Op;

   function Create_Case_Expr
     (Handle : Rewriting_Handle; Case_Expr_F_Expr : Node_Rewriting_Handle;
      Case_Expr_F_Cases : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Case_Expr,
           (1 => Case_Expr_F_Expr, 2 => Case_Expr_F_Cases));
   end Create_Case_Expr;

   function Create_Case_Expr_Alternative
     (Handle                          : Rewriting_Handle;
      Case_Expr_Alternative_F_Choices : Node_Rewriting_Handle;
      Case_Expr_Alternative_F_Expr    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Case_Expr_Alternative,
           (1 => Case_Expr_Alternative_F_Choices,
            2 => Case_Expr_Alternative_F_Expr));
   end Create_Case_Expr_Alternative;

   function Create_Contract_Cases
     (Handle                          : Rewriting_Handle;
      Contract_Cases_F_Contract_Cases : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Contract_Cases, (1 => Contract_Cases_F_Contract_Cases));
   end Create_Contract_Cases;

   function Create_If_Expr
     (Handle : Rewriting_Handle; If_Expr_F_Cond_Expr : Node_Rewriting_Handle;
      If_Expr_F_Then_Expr    : Node_Rewriting_Handle;
      If_Expr_F_Alternatives : Node_Rewriting_Handle;
      If_Expr_F_Else_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_If_Expr,
           (1 => If_Expr_F_Cond_Expr, 2 => If_Expr_F_Then_Expr,
            3 => If_Expr_F_Alternatives, 4 => If_Expr_F_Else_Expr));
   end Create_If_Expr;

   function Create_Membership_Expr
     (Handle                             : Rewriting_Handle;
      Membership_Expr_F_Expr             : Node_Rewriting_Handle;
      Membership_Expr_F_Op               : Node_Rewriting_Handle;
      Membership_Expr_F_Membership_Exprs : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Membership_Expr,
           (1 => Membership_Expr_F_Expr, 2 => Membership_Expr_F_Op,
            3 => Membership_Expr_F_Membership_Exprs));
   end Create_Membership_Expr;

   function Create_Attribute_Ref
     (Handle                    : Rewriting_Handle;
      Attribute_Ref_F_Prefix    : Node_Rewriting_Handle;
      Attribute_Ref_F_Attribute : Node_Rewriting_Handle;
      Attribute_Ref_F_Args      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Attribute_Ref,
           (1 => Attribute_Ref_F_Prefix, 2 => Attribute_Ref_F_Attribute,
            3 => Attribute_Ref_F_Args));
   end Create_Attribute_Ref;

   function Create_Update_Attribute_Ref
     (Handle                    : Rewriting_Handle;
      Attribute_Ref_F_Prefix    : Node_Rewriting_Handle;
      Attribute_Ref_F_Attribute : Node_Rewriting_Handle;
      Attribute_Ref_F_Args      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Update_Attribute_Ref,
           (1 => Attribute_Ref_F_Prefix, 2 => Attribute_Ref_F_Attribute,
            3 => Attribute_Ref_F_Args));
   end Create_Update_Attribute_Ref;

   function Create_Call_Expr
     (Handle : Rewriting_Handle; Call_Expr_F_Name : Node_Rewriting_Handle;
      Call_Expr_F_Suffix : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Call_Expr,
           (1 => Call_Expr_F_Name, 2 => Call_Expr_F_Suffix));
   end Create_Call_Expr;

   function Create_Defining_Name
     (Handle : Rewriting_Handle; Defining_Name_F_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Defining_Name, (1 => Defining_Name_F_Name));
   end Create_Defining_Name;

   function Create_Discrete_Subtype_Name
     (Handle                          : Rewriting_Handle;
      Discrete_Subtype_Name_F_Subtype : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Discrete_Subtype_Name,
           (1 => Discrete_Subtype_Name_F_Subtype));
   end Create_Discrete_Subtype_Name;

   function Create_Dotted_Name
     (Handle : Rewriting_Handle; Dotted_Name_F_Prefix : Node_Rewriting_Handle;
      Dotted_Name_F_Suffix : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Dotted_Name,
           (1 => Dotted_Name_F_Prefix, 2 => Dotted_Name_F_Suffix));
   end Create_Dotted_Name;

   function Create_End_Name
     (Handle : Rewriting_Handle; End_Name_F_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_End_Name, (1 => End_Name_F_Name));
   end Create_End_Name;

   function Create_Explicit_Deref
     (Handle                  : Rewriting_Handle;
      Explicit_Deref_F_Prefix : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Explicit_Deref, (1 => Explicit_Deref_F_Prefix));
   end Create_Explicit_Deref;

   function Create_Qual_Expr
     (Handle : Rewriting_Handle; Qual_Expr_F_Prefix : Node_Rewriting_Handle;
      Qual_Expr_F_Suffix : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Qual_Expr,
           (1 => Qual_Expr_F_Prefix, 2 => Qual_Expr_F_Suffix));
   end Create_Qual_Expr;

   function Create_Paren_Expr
     (Handle : Rewriting_Handle; Paren_Expr_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Paren_Expr, (1 => Paren_Expr_F_Expr));
   end Create_Paren_Expr;

   function Create_Quantified_Expr
     (Handle                       : Rewriting_Handle;
      Quantified_Expr_F_Quantifier : Node_Rewriting_Handle;
      Quantified_Expr_F_Loop_Spec  : Node_Rewriting_Handle;
      Quantified_Expr_F_Expr       : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Quantified_Expr,
           (1 => Quantified_Expr_F_Quantifier,
            2 => Quantified_Expr_F_Loop_Spec, 3 => Quantified_Expr_F_Expr));
   end Create_Quantified_Expr;

   function Create_Raise_Expr
     (Handle                      : Rewriting_Handle;
      Raise_Expr_F_Exception_Name : Node_Rewriting_Handle;
      Raise_Expr_F_Error_Message  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Raise_Expr,
           (1 => Raise_Expr_F_Exception_Name,
            2 => Raise_Expr_F_Error_Message));
   end Create_Raise_Expr;

   function Create_Un_Op
     (Handle       : Rewriting_Handle; Un_Op_F_Op : Node_Rewriting_Handle;
      Un_Op_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Un_Op, (1 => Un_Op_F_Op, 2 => Un_Op_F_Expr));
   end Create_Un_Op;

   function Create_Handled_Stmts
     (Handle : Rewriting_Handle; Handled_Stmts_F_Stmts : Node_Rewriting_Handle;
      Handled_Stmts_F_Exceptions : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Handled_Stmts,
           (1 => Handled_Stmts_F_Stmts, 2 => Handled_Stmts_F_Exceptions));
   end Create_Handled_Stmts;

   function Create_Library_Item
     (Handle                     : Rewriting_Handle;
      Library_Item_F_Has_Private : Node_Rewriting_Handle;
      Library_Item_F_Item : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Library_Item,
           (1 => Library_Item_F_Has_Private, 2 => Library_Item_F_Item));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_For_Loop_Spec,
           (1 => For_Loop_Spec_F_Var_Decl, 2 => For_Loop_Spec_F_Loop_Type,
            3 => For_Loop_Spec_F_Has_Reverse, 4 => For_Loop_Spec_F_Iter_Expr));
   end Create_For_Loop_Spec;

   function Create_While_Loop_Spec
     (Handle                 : Rewriting_Handle;
      While_Loop_Spec_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_While_Loop_Spec, (1 => While_Loop_Spec_F_Expr));
   end Create_While_Loop_Spec;

   function Create_Params
     (Handle : Rewriting_Handle; Params_F_Params : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node (Handle, Ada_Params, (1 => Params_F_Params));
   end Create_Params;

   function Create_Pragma_Node
     (Handle : Rewriting_Handle; Pragma_Node_F_Id : Node_Rewriting_Handle;
      Pragma_Node_F_Args : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Pragma_Node,
           (1 => Pragma_Node_F_Id, 2 => Pragma_Node_F_Args));
   end Create_Pragma_Node;

   function Create_Protected_Def
     (Handle                       : Rewriting_Handle;
      Protected_Def_F_Public_Part  : Node_Rewriting_Handle;
      Protected_Def_F_Private_Part : Node_Rewriting_Handle;
      Protected_Def_F_End_Name     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Protected_Def,
           (1 => Protected_Def_F_Public_Part,
            2 => Protected_Def_F_Private_Part, 3 => Protected_Def_F_End_Name));
   end Create_Protected_Def;

   function Create_Range_Spec
     (Handle : Rewriting_Handle; Range_Spec_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Range_Spec, (1 => Range_Spec_F_Range));
   end Create_Range_Spec;

   function Create_Renaming_Clause
     (Handle                           : Rewriting_Handle;
      Renaming_Clause_F_Renamed_Object : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Renaming_Clause,
           (1 => Renaming_Clause_F_Renamed_Object));
   end Create_Renaming_Clause;

   function Create_Synthetic_Renaming_Clause
     (Handle                           : Rewriting_Handle;
      Renaming_Clause_F_Renamed_Object : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Synthetic_Renaming_Clause,
           (1 => Renaming_Clause_F_Renamed_Object));
   end Create_Synthetic_Renaming_Clause;

   function Create_Select_When_Part
     (Handle                       : Rewriting_Handle;
      Select_When_Part_F_Cond_Expr : Node_Rewriting_Handle;
      Select_When_Part_F_Stmts     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Select_When_Part,
           (1 => Select_When_Part_F_Cond_Expr, 2 => Select_When_Part_F_Stmts));
   end Create_Select_When_Part;

   function Create_Accept_Stmt
     (Handle : Rewriting_Handle; Accept_Stmt_F_Name : Node_Rewriting_Handle;
      Accept_Stmt_F_Entry_Index_Expr : Node_Rewriting_Handle;
      Accept_Stmt_F_Params           : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Accept_Stmt,
           (1 => Accept_Stmt_F_Name, 2 => Accept_Stmt_F_Entry_Index_Expr,
            3 => Accept_Stmt_F_Params));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Accept_Stmt_With_Stmts,
           (1 => Accept_Stmt_F_Name, 2 => Accept_Stmt_F_Entry_Index_Expr,
            3 => Accept_Stmt_F_Params, 4 => Accept_Stmt_With_Stmts_F_Stmts,
            5 => Accept_Stmt_With_Stmts_F_End_Name));
   end Create_Accept_Stmt_With_Stmts;

   function Create_For_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_For_Loop_Stmt,
           (1 => Base_Loop_Stmt_F_Spec, 2 => Base_Loop_Stmt_F_Stmts,
            3 => Base_Loop_Stmt_F_End_Name));
   end Create_For_Loop_Stmt;

   function Create_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Loop_Stmt,
           (1 => Base_Loop_Stmt_F_Spec, 2 => Base_Loop_Stmt_F_Stmts,
            3 => Base_Loop_Stmt_F_End_Name));
   end Create_Loop_Stmt;

   function Create_While_Loop_Stmt
     (Handle : Rewriting_Handle; Base_Loop_Stmt_F_Spec : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_Stmts    : Node_Rewriting_Handle;
      Base_Loop_Stmt_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_While_Loop_Stmt,
           (1 => Base_Loop_Stmt_F_Spec, 2 => Base_Loop_Stmt_F_Stmts,
            3 => Base_Loop_Stmt_F_End_Name));
   end Create_While_Loop_Stmt;

   function Create_Begin_Block
     (Handle : Rewriting_Handle; Begin_Block_F_Stmts : Node_Rewriting_Handle;
      Begin_Block_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Begin_Block,
           (1 => Begin_Block_F_Stmts, 2 => Begin_Block_F_End_Name));
   end Create_Begin_Block;

   function Create_Decl_Block
     (Handle : Rewriting_Handle; Decl_Block_F_Decls : Node_Rewriting_Handle;
      Decl_Block_F_Stmts    : Node_Rewriting_Handle;
      Decl_Block_F_End_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Decl_Block,
           (1 => Decl_Block_F_Decls, 2 => Decl_Block_F_Stmts,
            3 => Decl_Block_F_End_Name));
   end Create_Decl_Block;

   function Create_Case_Stmt
     (Handle : Rewriting_Handle; Case_Stmt_F_Expr : Node_Rewriting_Handle;
      Case_Stmt_F_Alternatives : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Case_Stmt,
           (1 => Case_Stmt_F_Expr, 2 => Case_Stmt_F_Alternatives));
   end Create_Case_Stmt;

   function Create_Extended_Return_Stmt
     (Handle                       : Rewriting_Handle;
      Extended_Return_Stmt_F_Decl  : Node_Rewriting_Handle;
      Extended_Return_Stmt_F_Stmts : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Extended_Return_Stmt,
           (1 => Extended_Return_Stmt_F_Decl,
            2 => Extended_Return_Stmt_F_Stmts));
   end Create_Extended_Return_Stmt;

   function Create_If_Stmt
     (Handle : Rewriting_Handle; If_Stmt_F_Cond_Expr : Node_Rewriting_Handle;
      If_Stmt_F_Then_Stmts   : Node_Rewriting_Handle;
      If_Stmt_F_Alternatives : Node_Rewriting_Handle;
      If_Stmt_F_Else_Stmts   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_If_Stmt,
           (1 => If_Stmt_F_Cond_Expr, 2 => If_Stmt_F_Then_Stmts,
            3 => If_Stmt_F_Alternatives, 4 => If_Stmt_F_Else_Stmts));
   end Create_If_Stmt;

   function Create_Named_Stmt
     (Handle : Rewriting_Handle; Named_Stmt_F_Decl : Node_Rewriting_Handle;
      Named_Stmt_F_Stmt : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Named_Stmt,
           (1 => Named_Stmt_F_Decl, 2 => Named_Stmt_F_Stmt));
   end Create_Named_Stmt;

   function Create_Select_Stmt
     (Handle : Rewriting_Handle; Select_Stmt_F_Guards : Node_Rewriting_Handle;
      Select_Stmt_F_Else_Stmts  : Node_Rewriting_Handle;
      Select_Stmt_F_Abort_Stmts : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Select_Stmt,
           (1 => Select_Stmt_F_Guards, 2 => Select_Stmt_F_Else_Stmts,
            3 => Select_Stmt_F_Abort_Stmts));
   end Create_Select_Stmt;

   function Create_Abort_Stmt
     (Handle : Rewriting_Handle; Abort_Stmt_F_Names : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Abort_Stmt, (1 => Abort_Stmt_F_Names));
   end Create_Abort_Stmt;

   function Create_Assign_Stmt
     (Handle : Rewriting_Handle; Assign_Stmt_F_Dest : Node_Rewriting_Handle;
      Assign_Stmt_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Assign_Stmt,
           (1 => Assign_Stmt_F_Dest, 2 => Assign_Stmt_F_Expr));
   end Create_Assign_Stmt;

   function Create_Call_Stmt
     (Handle : Rewriting_Handle; Call_Stmt_F_Call : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Call_Stmt, (1 => Call_Stmt_F_Call));
   end Create_Call_Stmt;

   function Create_Delay_Stmt
     (Handle                 : Rewriting_Handle;
      Delay_Stmt_F_Has_Until : Node_Rewriting_Handle;
      Delay_Stmt_F_Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Delay_Stmt,
           (1 => Delay_Stmt_F_Has_Until, 2 => Delay_Stmt_F_Expr));
   end Create_Delay_Stmt;

   function Create_Exit_Stmt
     (Handle : Rewriting_Handle; Exit_Stmt_F_Loop_Name : Node_Rewriting_Handle;
      Exit_Stmt_F_Cond_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Exit_Stmt,
           (1 => Exit_Stmt_F_Loop_Name, 2 => Exit_Stmt_F_Cond_Expr));
   end Create_Exit_Stmt;

   function Create_Goto_Stmt
     (Handle                 : Rewriting_Handle;
      Goto_Stmt_F_Label_Name : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Goto_Stmt, (1 => Goto_Stmt_F_Label_Name));
   end Create_Goto_Stmt;

   function Create_Label
     (Handle : Rewriting_Handle; Label_F_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node (Handle, Ada_Label, (1 => Label_F_Decl));
   end Create_Label;

   function Create_Raise_Stmt
     (Handle                      : Rewriting_Handle;
      Raise_Stmt_F_Exception_Name : Node_Rewriting_Handle;
      Raise_Stmt_F_Error_Message  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Raise_Stmt,
           (1 => Raise_Stmt_F_Exception_Name,
            2 => Raise_Stmt_F_Error_Message));
   end Create_Raise_Stmt;

   function Create_Requeue_Stmt
     (Handle                   : Rewriting_Handle;
      Requeue_Stmt_F_Call_Name : Node_Rewriting_Handle;
      Requeue_Stmt_F_Has_Abort : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Requeue_Stmt,
           (1 => Requeue_Stmt_F_Call_Name, 2 => Requeue_Stmt_F_Has_Abort));
   end Create_Requeue_Stmt;

   function Create_Return_Stmt
     (Handle                    : Rewriting_Handle;
      Return_Stmt_F_Return_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Return_Stmt, (1 => Return_Stmt_F_Return_Expr));
   end Create_Return_Stmt;

   function Create_Subunit
     (Handle : Rewriting_Handle; Subunit_F_Name : Node_Rewriting_Handle;
      Subunit_F_Body : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subunit, (1 => Subunit_F_Name, 2 => Subunit_F_Body));
   end Create_Subunit;

   function Create_Task_Def
     (Handle : Rewriting_Handle; Task_Def_F_Interfaces : Node_Rewriting_Handle;
      Task_Def_F_Public_Part  : Node_Rewriting_Handle;
      Task_Def_F_Private_Part : Node_Rewriting_Handle;
      Task_Def_F_End_Name : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Task_Def,
           (1 => Task_Def_F_Interfaces, 2 => Task_Def_F_Public_Part,
            3 => Task_Def_F_Private_Part, 4 => Task_Def_F_End_Name));
   end Create_Task_Def;

   function Create_Access_To_Subp_Def
     (Handle                             : Rewriting_Handle;
      Access_Def_F_Has_Not_Null          : Node_Rewriting_Handle;
      Access_To_Subp_Def_F_Has_Protected : Node_Rewriting_Handle;
      Access_To_Subp_Def_F_Subp_Spec     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Access_To_Subp_Def,
           (1 => Access_Def_F_Has_Not_Null,
            2 => Access_To_Subp_Def_F_Has_Protected,
            3 => Access_To_Subp_Def_F_Subp_Spec));
   end Create_Access_To_Subp_Def;

   function Create_Anonymous_Type_Access_Def
     (Handle                                : Rewriting_Handle;
      Access_Def_F_Has_Not_Null             : Node_Rewriting_Handle;
      Anonymous_Type_Access_Def_F_Type_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Anonymous_Type_Access_Def,
           (1 => Access_Def_F_Has_Not_Null,
            2 => Anonymous_Type_Access_Def_F_Type_Decl));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Type_Access_Def,
           (1 => Access_Def_F_Has_Not_Null, 2 => Type_Access_Def_F_Has_All,
            3 => Type_Access_Def_F_Has_Constant,
            4 => Type_Access_Def_F_Subtype_Indication));
   end Create_Type_Access_Def;

   function Create_Array_Type_Def
     (Handle                          : Rewriting_Handle;
      Array_Type_Def_F_Indices        : Node_Rewriting_Handle;
      Array_Type_Def_F_Component_Type : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Array_Type_Def,
           (1 => Array_Type_Def_F_Indices,
            2 => Array_Type_Def_F_Component_Type));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Derived_Type_Def,
           (1 => Derived_Type_Def_F_Has_Abstract,
            2 => Derived_Type_Def_F_Has_Limited,
            3 => Derived_Type_Def_F_Has_Synchronized,
            4 => Derived_Type_Def_F_Subtype_Indication,
            5 => Derived_Type_Def_F_Interfaces,
            6 => Derived_Type_Def_F_Record_Extension,
            7 => Derived_Type_Def_F_Has_With_Private));
   end Create_Derived_Type_Def;

   function Create_Enum_Type_Def
     (Handle                        : Rewriting_Handle;
      Enum_Type_Def_F_Enum_Literals : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Enum_Type_Def, (1 => Enum_Type_Def_F_Enum_Literals));
   end Create_Enum_Type_Def;

   function Create_Interface_Type_Def
     (Handle                              : Rewriting_Handle;
      Interface_Type_Def_F_Interface_Kind : Node_Rewriting_Handle;
      Interface_Type_Def_F_Interfaces     : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Interface_Type_Def,
           (1 => Interface_Type_Def_F_Interface_Kind,
            2 => Interface_Type_Def_F_Interfaces));
   end Create_Interface_Type_Def;

   function Create_Mod_Int_Type_Def
     (Handle                  : Rewriting_Handle;
      Mod_Int_Type_Def_F_Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Mod_Int_Type_Def, (1 => Mod_Int_Type_Def_F_Expr));
   end Create_Mod_Int_Type_Def;

   function Create_Private_Type_Def
     (Handle                          : Rewriting_Handle;
      Private_Type_Def_F_Has_Abstract : Node_Rewriting_Handle;
      Private_Type_Def_F_Has_Tagged   : Node_Rewriting_Handle;
      Private_Type_Def_F_Has_Limited  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Private_Type_Def,
           (1 => Private_Type_Def_F_Has_Abstract,
            2 => Private_Type_Def_F_Has_Tagged,
            3 => Private_Type_Def_F_Has_Limited));
   end Create_Private_Type_Def;

   function Create_Decimal_Fixed_Point_Def
     (Handle                           : Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Delta  : Node_Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Digits : Node_Rewriting_Handle;
      Decimal_Fixed_Point_Def_F_Range  : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Decimal_Fixed_Point_Def,
           (1 => Decimal_Fixed_Point_Def_F_Delta,
            2 => Decimal_Fixed_Point_Def_F_Digits,
            3 => Decimal_Fixed_Point_Def_F_Range));
   end Create_Decimal_Fixed_Point_Def;

   function Create_Floating_Point_Def
     (Handle                          : Rewriting_Handle;
      Floating_Point_Def_F_Num_Digits : Node_Rewriting_Handle;
      Floating_Point_Def_F_Range      : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Floating_Point_Def,
           (1 => Floating_Point_Def_F_Num_Digits,
            2 => Floating_Point_Def_F_Range));
   end Create_Floating_Point_Def;

   function Create_Ordinary_Fixed_Point_Def
     (Handle                           : Rewriting_Handle;
      Ordinary_Fixed_Point_Def_F_Delta : Node_Rewriting_Handle;
      Ordinary_Fixed_Point_Def_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Ordinary_Fixed_Point_Def,
           (1 => Ordinary_Fixed_Point_Def_F_Delta,
            2 => Ordinary_Fixed_Point_Def_F_Range));
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

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Record_Type_Def,
           (1 => Record_Type_Def_F_Has_Abstract,
            2 => Record_Type_Def_F_Has_Tagged,
            3 => Record_Type_Def_F_Has_Limited,
            4 => Record_Type_Def_F_Record_Def));
   end Create_Record_Type_Def;

   function Create_Signed_Int_Type_Def
     (Handle                      : Rewriting_Handle;
      Signed_Int_Type_Def_F_Range : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Signed_Int_Type_Def,
           (1 => Signed_Int_Type_Def_F_Range));
   end Create_Signed_Int_Type_Def;

   function Create_Anonymous_Type
     (Handle                     : Rewriting_Handle;
      Anonymous_Type_F_Type_Decl : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Anonymous_Type, (1 => Anonymous_Type_F_Type_Decl));
   end Create_Anonymous_Type;

   function Create_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Subtype_Indication,
           (1 => Subtype_Indication_F_Has_Not_Null,
            2 => Subtype_Indication_F_Name,
            3 => Subtype_Indication_F_Constraint));
   end Create_Subtype_Indication;

   function Create_Constrained_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Constrained_Subtype_Indication,
           (1 => Subtype_Indication_F_Has_Not_Null,
            2 => Subtype_Indication_F_Name,
            3 => Subtype_Indication_F_Constraint));
   end Create_Constrained_Subtype_Indication;

   function Create_Discrete_Subtype_Indication
     (Handle                            : Rewriting_Handle;
      Subtype_Indication_F_Has_Not_Null : Node_Rewriting_Handle;
      Subtype_Indication_F_Name         : Node_Rewriting_Handle;
      Subtype_Indication_F_Constraint   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Discrete_Subtype_Indication,
           (1 => Subtype_Indication_F_Has_Not_Null,
            2 => Subtype_Indication_F_Name,
            3 => Subtype_Indication_F_Constraint));
   end Create_Discrete_Subtype_Indication;

   function Create_Unconstrained_Array_Index
     (Handle                                         : Rewriting_Handle;
      Unconstrained_Array_Index_F_Subtype_Indication : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Unconstrained_Array_Index,
           (1 => Unconstrained_Array_Index_F_Subtype_Indication));
   end Create_Unconstrained_Array_Index;

   function Create_Use_Package_Clause
     (Handle                        : Rewriting_Handle;
      Use_Package_Clause_F_Packages : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Use_Package_Clause,
           (1 => Use_Package_Clause_F_Packages));
   end Create_Use_Package_Clause;

   function Create_Use_Type_Clause
     (Handle                    : Rewriting_Handle;
      Use_Type_Clause_F_Has_All : Node_Rewriting_Handle;
      Use_Type_Clause_F_Types   : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Use_Type_Clause,
           (1 => Use_Type_Clause_F_Has_All, 2 => Use_Type_Clause_F_Types));
   end Create_Use_Type_Clause;

   function Create_Variant
     (Handle : Rewriting_Handle; Variant_F_Choices : Node_Rewriting_Handle;
      Variant_F_Components : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Variant,
           (1 => Variant_F_Choices, 2 => Variant_F_Components));
   end Create_Variant;

   function Create_Variant_Part
     (Handle                    : Rewriting_Handle;
      Variant_Part_F_Discr_Name : Node_Rewriting_Handle;
      Variant_Part_F_Variant    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_Variant_Part,
           (1 => Variant_Part_F_Discr_Name, 2 => Variant_Part_F_Variant));
   end Create_Variant_Part;

   function Create_With_Clause
     (Handle                    : Rewriting_Handle;
      With_Clause_F_Has_Limited : Node_Rewriting_Handle;
      With_Clause_F_Has_Private : Node_Rewriting_Handle;
      With_Clause_F_Packages    : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
   begin

      Pre_Check (Handle /= No_Rewriting_Handle, "Handle should not be null");

      return Create_Regular_Node
          (Handle, Ada_With_Clause,
           (1 => With_Clause_F_Has_Limited, 2 => With_Clause_F_Has_Private,
            3 => With_Clause_F_Packages));
   end Create_With_Clause;

end Libadalang.Rewriting_Implementation;
