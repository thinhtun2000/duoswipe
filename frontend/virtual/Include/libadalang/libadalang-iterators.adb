with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Introspection; use Libadalang.Introspection;

with Libadalang.Iterators.Extensions;

package body Libadalang.Iterators is

   package Predicate_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Ada_Node_Predicate,
      "="        => Ada_Node_Predicate_References."=");

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return Ada_Node_Predicate_Array;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return Ada_Node_Predicate_Array
   is
   begin
      return
        Result : Ada_Node_Predicate_Array (1 .. Natural (Predicates.Length)) do
         for I in Result'Range loop
            Result (I) := Predicates.Element (I);
         end loop;
      end return;
   end To_Array;

   --------------
   -- Traverse --
   --------------

   function Traverse (Root : Ada_Node'Class) return Traverse_Iterator'Class is
   begin
      return Result : Traverse_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Ada_Node, Result);
      end return;
   end Traverse;

   -----------
   -- "not" --
   -----------

   function "not" (Predicate : Ada_Node_Predicate) return Ada_Node_Predicate is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (Not_Predicate'
              (Ada_Node_Predicate_Interface with Predicate => Predicate));
      end return;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Ada_Node_Predicate) return Ada_Node_Predicate
   is
   begin
      return For_All ((Left, Right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Ada_Node_Predicate) return Ada_Node_Predicate
   is
   begin
      return For_Some ((Left, Right));
   end "or";

   -------------
   -- For_All --
   -------------

   function For_All
     (Predicates : Ada_Node_Predicate_Array) return Ada_Node_Predicate
   is
      Preds : Predicate_Vectors.Vector;
   begin
      --  Flatten sub-predicates that are themselves For_All predicates in
      --  Predicates.
      for P of Predicates loop
         if P.Unchecked_Get.all in For_All_Predicate'Class then
            for Sub_P of For_All_Predicate (P.Unchecked_Get.all).Predicates
            loop
               Preds.Append (Sub_P);
            end loop;
         else
            Preds.Append (P);
         end if;
      end loop;

      return Result : Ada_Node_Predicate do
         Result.Set
           (For_All_Predicate'
              (Ada_Node_Predicate_Interface with N => Natural (Preds.Length),
               Predicates                          => To_Array (Preds)));
      end return;
   end For_All;

   --------------
   -- For_Some --
   --------------

   function For_Some
     (Predicates : Ada_Node_Predicate_Array) return Ada_Node_Predicate
   is
      Preds : Predicate_Vectors.Vector;
   begin
      --  Flatten sub-predicates that are themselves For_Some predicates in
      --  Predicates.
      for P of Predicates loop
         if P.Unchecked_Get.all in For_Some_Predicate'Class then
            for Sub_P of For_Some_Predicate (P.Unchecked_Get.all).Predicates
            loop
               Preds.Append (Sub_P);
            end loop;
         else
            Preds.Append (P);
         end if;
      end loop;

      return Result : Ada_Node_Predicate do
         Result.Set
           (For_Some_Predicate'
              (Ada_Node_Predicate_Interface with N => Natural (Preds.Length),
               Predicates                          => To_Array (Preds)));
      end return;
   end For_Some;

   ----------------------
   -- For_All_Children --
   ----------------------

   function For_All_Children
     (Predicate : Ada_Node_Predicate; Skip_Null : Boolean := True)
      return Ada_Node_Predicate
   is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (For_All_Children_Predicate'
              (Ada_Node_Predicate_Interface with Predicate => Predicate,
               Skip_Null                                   => Skip_Null));
      end return;
   end For_All_Children;

   -----------------------
   -- For_Some_Children --
   -----------------------

   function For_Some_Children
     (Predicate : Ada_Node_Predicate; Skip_Null : Boolean := True)
      return Ada_Node_Predicate
   is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (For_Some_Children_Predicate'
              (Ada_Node_Predicate_Interface with Predicate => Predicate,
               Skip_Null                                   => Skip_Null));
      end return;
   end For_Some_Children;

   ----------------
   -- Child_With --
   ----------------

   function Child_With
     (Field : Field_Reference; Predicate : Ada_Node_Predicate)
      return Ada_Node_Predicate
   is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (Child_With_Predicate'
              (Ada_Node_Predicate_Interface with Field => Field,
               Predicate                               => Predicate));
      end return;
   end Child_With;

   -------------
   -- Kind_Is --
   -------------

   function Kind_Is (Kind : Ada_Node_Kind_Type) return Ada_Node_Predicate is
   begin
      return Kind_In (Kind, Kind);
   end Kind_Is;

   -------------
   -- Kind_In --
   -------------

   function Kind_In
     (First, Last : Ada_Node_Kind_Type) return Ada_Node_Predicate
   is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (Kind_Predicate'
              (Ada_Node_Predicate_Interface with First => First,
               Last                                    => Last));
      end return;
   end Kind_In;

   -------------
   -- Text_Is --
   -------------

   function Text_Is (Text : Text_Type) return Ada_Node_Predicate is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (Text_Predicate'
              (Ada_Node_Predicate_Interface with
               Text => To_Unbounded_Text (Text)));
      end return;
   end Text_Is;

   ------------------
   -- Node_Is_Null --
   ------------------

   function Node_Is_Null return Ada_Node_Predicate is
   begin
      return Result : Ada_Node_Predicate do
         Result.Set
           (Node_Is_Null_Predicate'
              (Ada_Node_Predicate_Interface with null record));
      end return;
   end Node_Is_Null;

   ----------
   -- Next --
   ----------

   function Next
     (It : in out Find_Iterator; Element : out Ada_Node) return Boolean
   is
      Parent : Traverse_Iterator := Traverse_Iterator (It);
   begin
      while Next (Parent, Element) loop
         if It.Predicate.Unchecked_Get.Evaluate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (It : in out Local_Find_Iterator; Element : out Ada_Node) return Boolean
   is
      Parent : Traverse_Iterator := Traverse_Iterator (It);
   begin
      while Next (Parent, Element) loop
         if It.Predicate = null or else It.Predicate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Find --
   ----------

   function Find
     (Root : Ada_Node'Class;
      Predicate : access function (N : Ada_Node) return Boolean := null)
      return Traverse_Iterator'Class
   is
   begin
      return Ret : Local_Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Ada_Node, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := Predicate'Unrestricted_Access.all;
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root : Ada_Node'Class;
      Predicate : access function (N : Ada_Node) return Boolean := null)
      return Ada_Node
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : Ada_Node;
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_Ada_Node;
      end if;
      return Result;
   end Find_First;

   ----------
   -- Find --
   ----------

   function Find
     (Root : Ada_Node'Class; Predicate : Ada_Node_Predicate'Class)
      return Traverse_Iterator'Class
   is
   begin
      return Ret : Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Ada_Node, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := Ada_Node_Predicate (Predicate);
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root : Ada_Node'Class; Predicate : Ada_Node_Predicate'Class)
      return Ada_Node
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : Ada_Node;
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_Ada_Node;
      end if;
      return Result;
   end Find_First;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (N : Ada_Node) return Ada_Node is (Parent (N));

   ------------------------------------
   -- First_Child_Index_For_Traverse --
   ------------------------------------

   function First_Child_Index_For_Traverse (N : Ada_Node) return Natural is
     (First_Child_Index (N));

   -----------------------------------
   -- Last_Child_Index_For_Traverse --
   -----------------------------------

   function Last_Child_Index_For_Traverse (N : Ada_Node) return Natural is
     (Last_Child_Index (N));

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (N : Ada_Node; I : Natural) return Ada_Node is
     (Child (N, I));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Not_Predicate; N : Ada_Node) return Boolean
   is
   begin
      return not P.Predicate.Unchecked_Get.Evaluate (N);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Predicate; N : Ada_Node) return Boolean
   is
   begin
      for Predicate of P.Predicates loop
         if not Predicate.Unchecked_Get.Evaluate (N) then
            return False;
         end if;
      end loop;
      return True;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_Some_Predicate; N : Ada_Node) return Boolean
   is
   begin
      for Predicate of P.Predicates loop
         if Predicate.Unchecked_Get.Evaluate (N) then
            return True;
         end if;
      end loop;
      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Children_Predicate; N : Ada_Node) return Boolean
   is
      Child_Pred : Ada_Node_Predicate_Interface'Class renames
        P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant Ada_Node := N.Child (I);
         begin
            if (not P.Skip_Null or else not Child.Is_Null)
              and then not Child_Pred.Evaluate (Child)
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_Some_Children_Predicate; N : Ada_Node) return Boolean
   is
      Child_Pred : Ada_Node_Predicate_Interface'Class renames
        P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant Ada_Node := N.Child (I);
         begin
            if (not P.Skip_Null or else not Child.Is_Null)
              and then Child_Pred.Evaluate (Child)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Child_With_Predicate; N : Ada_Node) return Boolean
   is
   begin
      if N.Is_Null then
         return False;
      end if;

      declare
         Field_Index : Positive;
      begin
         --  First check that N has the requested field
         begin
            Field_Index := Index (N.Kind, P.Field);
         exception
            when Invalid_Field =>
               return False;
         end;

         return P.Predicate.Unchecked_Get.Evaluate (N.Child (Field_Index));
      end;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Kind_Predicate; N : Ada_Node) return Boolean
   is
   begin
      return N.Kind in P.First .. P.Last;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Text_Predicate; N : Ada_Node) return Boolean
   is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      return (if N.Is_Null then P.Text = "" else N.Text = P.Text);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Node_Is_Null_Predicate; N : Ada_Node) return Boolean
   is
      pragma Unreferenced (P);
   begin
      return N.Is_Null;
   end Evaluate;

------------------
-- Decl_Defines --
------------------

   function Decl_Defines (Name : Text_Type) return Ada_Node_Predicate is
     (Libadalang.Iterators.Extensions.Decl_Defines (Name));

-------------
-- Xref_Is --
-------------

   function Xref_Is
     (Name : Defining_Name; Imprecise_Fallback : Boolean := False)
      return Ada_Node_Predicate is
     (Libadalang.Iterators.Extensions.Xref_Is (Name, Imprecise_Fallback));

end Libadalang.Iterators;
