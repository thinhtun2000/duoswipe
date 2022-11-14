with GNATCOLL.Refcount;

with Langkit_Support.Iterators;
private with Langkit_Support.Tree_Traversal_Iterator;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

--  This package provides an interface to iterate on nodes in parse trees and
--  to look for node patterns.
--
--  First, as an alternative to ``Libadalang.Analysis.Traverse``, you can do:
--
--  .. code-block:: ada
--
--     declare
--        It   : Traverse_Iterator := Traverse (My_Unit.Root);
--        Node : Ada_Node;
--     begin
--        while It.Next (Node) loop
--           --  Process Node
--        end loop;
--     end;
--
--  Now, if you are exclusively looking for nodes whose text is either ``foo``
--  or ``bar``, you can replace the call to ``Traverse`` with the following:
--
--  .. code-block:: ada
--
--        Find (My_Unit.Root, Text_Is ("foo") or Text_Is ("bar"));
--
--  The ``Find``-like functions below take as a second argument a *predicate*,
--  which is an object that can decide if a node should be processed or
--  not. This package provides several built-in predicates (``Kind_Is``,
--  ``Text_Is``, etc.), then you can either define your own, derivating the
--  ``Ada_Node_Predicate_Interface`` type, or compose them using Ada's boolean
--  operators.

package Libadalang.Iterators is

   use Support.Text;

   --------------------
   -- Iterators core --
   --------------------

   package Ada_Node_Iterators is new Support.Iterators
     (Element_Type => Ada_Node, Element_Array => Ada_Node_Array);

   type Traverse_Iterator is new Ada_Node_Iterators.Iterator with private;
   --  Iterator that yields nodes from a tree

   function Traverse (Root : Ada_Node'Class) return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) in a
   --  prefix DFS (depth first search) fashion.

   ---------------------
   -- Predicates core --
   ---------------------

   type Ada_Node_Predicate_Interface is interface;
   --  Predicate on nodes.
   --
   --  Useful predicates often rely on values from some context, so predicates
   --  that are mere accesses to a function are not powerful enough. Having a
   --  full interface for this makes it possible to package both the predicate
   --  code and some data it needs.
   --
   --  Note that predicates are not thread-safe: make sure you don't use a
   --  predicate from multiple threads, as they can contain caches.

   function Evaluate
     (P : in out Ada_Node_Predicate_Interface; N : Ada_Node)
      return Boolean is abstract;
   --  Return the value of the predicate for the ``N`` node

   package Ada_Node_Predicate_References is new GNATCOLL.Refcount
     .Shared_Pointers
     (Ada_Node_Predicate_Interface'Class);

   subtype Ada_Node_Predicate is Ada_Node_Predicate_References.Ref;
   --  Ref-counted reference to a predicate

   type Ada_Node_Predicate_Array is
     array (Positive range <>) of Ada_Node_Predicate;

   function "not" (Predicate : Ada_Node_Predicate) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes that are *not* accepted by
   --  ``Predicate``.
   --
   --% belongs-to: Ada_Node_Predicate

   function "and" (Left, Right : Ada_Node_Predicate) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes that are accepted by both
   --  ``Left`` and ``Right``.
   --
   --% belongs-to: Ada_Node_Predicate

   function "or" (Left, Right : Ada_Node_Predicate) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes that are accepted by ``Left``
   --  or ``Right``.
   --
   --% belongs-to: Ada_Node_Predicate

   function For_All
     (Predicates : Ada_Node_Predicate_Array) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes that are accepted by all
   --  given ``Predicates``.
   --
   --% belongs-to: Ada_Node_Predicate

   function For_Some
     (Predicates : Ada_Node_Predicate_Array) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes that are accepted by at least
   --  one of the given ``Predicates``.
   --
   --% belongs-to: Ada_Node_Predicate

   function For_All_Children
     (Predicate : Ada_Node_Predicate; Skip_Null : Boolean := True)
      return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes for which ``Predicate``
   --  accepts all children. Unless ``Skip_Null`` is false, this does not
   --  evaluate the predicate on null children.
   --
   --% belongs-to: Ada_Node_Predicate

   function For_Some_Children
     (Predicate : Ada_Node_Predicate; Skip_Null : Boolean := True)
      return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes for which ``Predicate``
   --  accepts at least one child. Unless ``Skip_Null`` is false, this does
   --  not evaluate the predicate on null children.
   --
   --% belongs-to: Ada_Node_Predicate

   function Child_With
     (Field : Field_Reference; Predicate : Ada_Node_Predicate)
      return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes which have a child
   --  corresponding to the given field reference and for which this child
   --  is accepted by the given predicate.
   --
   --% belongs-to: Ada_Node_Predicate

   ---------------------------
   -- Node search functions --
   ---------------------------

   function Find
     (Root : Ada_Node'Class;
      Predicate : access function (N : Ada_Node) return Boolean := null)
      return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) that
   --  satisfy the ``Predicate`` predicate.

   function Find
     (Root : Ada_Node'Class; Predicate : Ada_Node_Predicate'Class)
      return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) that
   --  satisfy the ``Predicate`` predicate.

   function Find_First
     (Root : Ada_Node'Class;
      Predicate : access function (N : Ada_Node) return Boolean := null)
      return Ada_Node;
   --  Return the first node found under ``Root`` (included) that satisfies the
   --  given ``Predicate``. Return a null node if there is no such node.

   function Find_First
     (Root : Ada_Node'Class; Predicate : Ada_Node_Predicate'Class)
      return Ada_Node;
   --  Return the first node found under ``Root`` (included) that satisfies the
   --  given ``Predicate``. Return a null node if there is no such node.

   ----------------
   -- Predicates --
   ----------------

   function Kind_Is (Kind : Ada_Node_Kind_Type) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes of the given ``Kind``
   --
   --% belongs-to: Ada_Node_Predicate

   function Kind_In
     (First, Last : Ada_Node_Kind_Type) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes whose kind is in First ..
   --  Last.
   --
   --% belongs-to: Ada_Node_Predicate

   function Text_Is (Text : Text_Type) return Ada_Node_Predicate;
   --  Return a predicate that accepts only nodes that match the given ``Text``
   --
   --% belongs-to: Ada_Node_Predicate

   function Node_Is_Null return Ada_Node_Predicate;
   --  Return a predicate that accepts only null nodes
   --
   --% belongs-to: Ada_Node_Predicate

   function Decl_Defines (Name : Text_Type) return Ada_Node_Predicate;
--  Return a predicate that accepts only ``Basic_Decl`` nodes that define the
--  given ``Name``.
--
--% belongs-to: Ada_Node_Predicate

   function Xref_Is
     (Name : Defining_Name; Imprecise_Fallback : Boolean := False)
      return Ada_Node_Predicate;
--  Return a predicate that accepts only nodes whose ``P_Gnat_Xref`` property
--  returns ``Name``.
--
--% belongs-to: Ada_Node_Predicate

private

   ------------------------
   -- Iterator internals --
   ------------------------

   function Get_Parent (N : Ada_Node) return Ada_Node;
   function First_Child_Index_For_Traverse (N : Ada_Node) return Natural;
   function Last_Child_Index_For_Traverse (N : Ada_Node) return Natural;
   function Get_Child (N : Ada_Node; I : Natural) return Ada_Node;

   package Traversal_Iterators is new Langkit_Support.Tree_Traversal_Iterator
     (Node_Type         => Ada_Node, No_Node => No_Ada_Node,
      Node_Array        => Ada_Node_Array,
      First_Child_Index => First_Child_Index_For_Traverse,
      Last_Child_Index  => Last_Child_Index_For_Traverse,
      Iterators         => Ada_Node_Iterators);

   type Traverse_Iterator is new Traversal_Iterators.Traverse_Iterator with
   null record;

   type Find_Iterator is new Traverse_Iterator with record
      Predicate : Ada_Node_Predicate;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for the ``Find`` function

   overriding function Next
     (It : in out Find_Iterator; Element : out Ada_Node) return Boolean;

   type Local_Find_Iterator is new Traverse_Iterator with record
      Predicate : access function (N : Ada_Node) return Boolean;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for the ``Find`` function that takes an access to
   --  function. It is called ``Local_Find_Iterator`` because if you use a
   --  locally declared function, the iterator itself will only be valid in
   --  the scope of the function.

   overriding function Next
     (It : in out Local_Find_Iterator; Element : out Ada_Node) return Boolean;

   --------------------------
   -- Predicates internals --
   --------------------------

   type Not_Predicate is new Ada_Node_Predicate_Interface with record
      Predicate : Ada_Node_Predicate;
   end record;

   overriding function Evaluate
     (P : in out Not_Predicate; N : Ada_Node) return Boolean;

   type For_All_Predicate (N : Natural)
   is new Ada_Node_Predicate_Interface with record
      Predicates : Ada_Node_Predicate_Array (1 .. N);
   end record;

   overriding function Evaluate
     (P : in out For_All_Predicate; N : Ada_Node) return Boolean;

   type For_Some_Predicate (N : Natural)
   is new Ada_Node_Predicate_Interface with record
      Predicates : Ada_Node_Predicate_Array (1 .. N);
   end record;

   overriding function Evaluate
     (P : in out For_Some_Predicate; N : Ada_Node) return Boolean;

   type For_All_Children_Predicate is new Ada_Node_Predicate_Interface with
   record
      Predicate : Ada_Node_Predicate;
      Skip_Null : Boolean;
   end record;

   overriding function Evaluate
     (P : in out For_All_Children_Predicate; N : Ada_Node) return Boolean;

   type For_Some_Children_Predicate is new Ada_Node_Predicate_Interface with
   record
      Predicate : Ada_Node_Predicate;
      Skip_Null : Boolean;
   end record;

   overriding function Evaluate
     (P : in out For_Some_Children_Predicate; N : Ada_Node) return Boolean;

   type Child_With_Predicate is new Ada_Node_Predicate_Interface with record
      Field     : Field_Reference;
      Predicate : Ada_Node_Predicate;
   end record;

   overriding function Evaluate
     (P : in out Child_With_Predicate; N : Ada_Node) return Boolean;

   type Kind_Predicate is new Ada_Node_Predicate_Interface with record
      First, Last : Ada_Node_Kind_Type;
   end record;
   --  Predicate that returns true for all nodes whose kind is in a given range

   overriding function Evaluate
     (P : in out Kind_Predicate; N : Ada_Node) return Boolean;

   type Text_Predicate is new Ada_Node_Predicate_Interface with record
      Text : Unbounded_Text_Type;
   end record;
   --  Predicate that returns true for all nodes that match some text

   overriding function Evaluate
     (P : in out Text_Predicate; N : Ada_Node) return Boolean;

   type Node_Is_Null_Predicate is new Ada_Node_Predicate_Interface with
   null record;

   overriding function Evaluate
     (P : in out Node_Is_Null_Predicate; N : Ada_Node) return Boolean;

end Libadalang.Iterators;
