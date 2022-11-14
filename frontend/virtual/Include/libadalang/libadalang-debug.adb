with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Libadalang.Common; use Libadalang.Common;

with Libadalang.Sources;

package body Libadalang.Debug is

   use Support.Slocs, Support.Text;

   --------
   -- PN --
   --------

   procedure PN (Node : Bare_Ada_Node) is
   begin
      Put_Line (Image (Short_Text_Image (Node)));
   end PN;

   --------
   -- PT --
   --------

   procedure PT (Node : Bare_Ada_Node) is
   begin
      Print (Node, Show_Slocs => True);
   end PT;

   ----------
   -- PTok --
   ----------

   procedure PTok (TDH : Token_Data_Handler_Access; T : Token_Index) is
      Index : constant Natural := Natural (T);
   begin
      if Index not in TDH.Tokens.First_Index .. TDH.Tokens.Last_Index then
         Put_Line ("<invalid token>");

      else
         declare
            D : constant Stored_Token_Data := TDH.Tokens.Get (Index);
         begin
            Put (Token_Kind_Name (To_Token_Kind (D.Kind)));
            Put
              (" " &
               Image
                 (Common.Token_Data_Handlers.Text (TDH.all, D),
                  With_Quotes => True));
            Put_Line (" [" & Image (D.Sloc_Range) & "]");
         end;
      end if;
   end PTok;

   ----------
   -- PEnv --
   ----------

   procedure PEnv (Env : AST_Envs.Lexical_Env) is
   begin
      AST_Envs.Dump_Lexical_Env_Parent_Chain (Env);
   end PEnv;

   -----------------
   -- Sym_Matches --
   -----------------

   function Sym_Matches (S : Symbol_Type; Text : String) return Boolean is
      Symbol : constant Symbolization_Result :=
        Libadalang.Sources.Canonicalize (To_Text (Text));
   begin
      return Symbol.Success and then Image (S.all) = Image (Symbol.Symbol);
   end Sym_Matches;

   ----------
   -- PRel --
   ----------

   procedure PRel (Rel : Relation; Context_Node : Bare_Ada_Node) is
   begin
      Assign_Names_To_Logic_Vars (Context_Node);
      Print_Relation (Rel, null, False);
   end PRel;

end Libadalang.Debug;
