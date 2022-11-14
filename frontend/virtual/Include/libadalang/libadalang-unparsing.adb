with Libadalang.Implementation; use Libadalang.Implementation;

with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Unparsing_Implementation;
use Libadalang.Unparsing_Implementation;

package body Libadalang.Unparsing is

   -------------
   -- Unparse --
   -------------

   function Unparse (Node : Ada_Node'Class) return String is
      N : constant Bare_Ada_Node := Unwrap_Node (Node);
   begin
      return Unparse
          (Create_Abstract_Node (N), Unwrap_Node (Node).Unit,
           Preserve_Formatting => False, As_Unit => False);
   end Unparse;

end Libadalang.Unparsing;
