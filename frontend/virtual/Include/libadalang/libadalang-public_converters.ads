with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Common;         use Libadalang.Common;
with Libadalang.Implementation; use Libadalang.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package Libadalang.Public_Converters is

   use Support.Text;

   type Context_Wrapper is access function
     (Context : Internal_Context) return Analysis_Context;
   Wrap_Context : Context_Wrapper;

   type Context_Unwrapper is access function
     (Context : Analysis_Context'Class) return Internal_Context;
   Unwrap_Context : Context_Unwrapper;

   type Unit_Wrapper is access function
     (Unit : Internal_Unit) return Analysis_Unit;
   Wrap_Unit : Unit_Wrapper;

   type Unit_Unwrapper is access function
     (Unit : Analysis_Unit'Class) return Internal_Unit;
   Unwrap_Unit : Unit_Unwrapper;

   type Node_Wrapper is access function
     (Node : Bare_Ada_Node; Info : Internal_Entity_Info := No_Entity_Info)
      return Ada_Node;
   Wrap_Node : Node_Wrapper;

   type Node_Unwrapper is access function
     (Node : Ada_Node'Class) return Bare_Ada_Node;
   Unwrap_Node : Node_Unwrapper;

   type Entity_Unwrapper is access function
     (Entity : Ada_Node'Class) return Internal_Entity;
   Unwrap_Entity : Entity_Unwrapper;

   ---------------------------
   -- Unit_Provider_Wrapper --
   ---------------------------

   --  This wraps a unit provider using the public API into one that fits in
   --  our internal APIs.

   type Unit_Provider_Wrapper is new Internal_Unit_Provider with record
      Ref_Count : Natural;
      Internal  : Unit_Provider_Reference;
   end record;

   overriding procedure Inc_Ref (Provider : in out Unit_Provider_Wrapper);
   overriding function Dec_Ref
     (Provider : in out Unit_Provider_Wrapper) return Boolean;

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper; Name : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;
   overriding function Get_Unit
     (Provider : Unit_Provider_Wrapper; Context : Internal_Context;
      Name     : Text_Type; Kind : Analysis_Unit_Kind; Charset : String := "";
      Reparse  : Boolean := False) return Internal_Unit;

   function Wrap_Public_Provider
     (Provider : Unit_Provider_Reference) return Internal_Unit_Provider_Access;
   --  Wrap a public unit provider inside an internal one. If Provider is
   --  a null reference, return null. Otherwise, the result is dynamically
   --  allocated and the caller must free it when done with it (see the
   --  relevant Implementation.Destroy overload).

end Libadalang.Public_Converters;
