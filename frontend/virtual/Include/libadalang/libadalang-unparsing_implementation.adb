pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with GNATCOLL.Iconv;

with Libadalang.Common; use Libadalang.Common;
use Libadalang.Common.Token_Data_Handlers;
with Libadalang.Implementation; use Libadalang.Implementation;
with Libadalang.Introspection_Implementation;
use Libadalang.Introspection_Implementation;
with Libadalang.Private_Converters; use Libadalang.Private_Converters;

package body Libadalang.Unparsing_Implementation is

   subtype String_Access is Ada.Strings.Unbounded.String_Access;

   --  The "template" data structures below are helpers for the original source
   --  code formatting preservation algorithms. A template can be thought as
   --  the instantiation of an unparser from an original AST node. It captures
   --  actual sequences of tokens.

   type Token_Sequence_Template (Present : Boolean := False) is record
      case Present is
         when False =>
            null;
         when True =>
            First, Last : Token_Reference;
      end case;
   end record;
   --  Captured sequence of tokens

   subtype Present_Token_Sequence_Template is Token_Sequence_Template (True);

   Empty_Token_Sequence_Template : constant Present_Token_Sequence_Template :=
     (Present => True, First => No_Token, Last => No_Token);

   function Create_Token_Sequence
     (Unparser : Token_Sequence_Access; First_Token : in out Token_Reference)
      return Present_Token_Sequence_Template with
      Pre => First_Token /= No_Token;
      --  Create a present sequence of tokens starting from First_Token and
      --  containing the same number of tokens as indicated in Unparser. Before
      --  returning, this updates First_Token to point at the first token that
      --  appear after the sequence.

   type Token_Sequence_Template_Array is
     array (Positive range <>) of Present_Token_Sequence_Template;

   type Field_Template (Present : Boolean := False) is record
      case Present is
         when False =>
            null;
         when True =>
            Pre_Tokens, Post_Tokens : Token_Sequence_Template (Present);
      end case;
   end record;
   --  Captured sequences of tokens before and after a node field. This is the
   --  instantiation of Field_Unparser.

   type Field_Template_Array is array (Positive range <>) of Field_Template;

   type Regular_Node_Template (Present : Boolean; Count : Natural) is record
      case Present is
         when False =>
            null;
         when True =>
            Pre_Tokens   : Present_Token_Sequence_Template;
            Fields       : Field_Template_Array (1 .. Count);
            Inter_Tokens : Token_Sequence_Template_Array (1 .. Count);
            Post_Tokens  : Present_Token_Sequence_Template;
      end case;
   end record;
   --  Captured sequences of tokens corresponding to a regular node. This is
   --  the instantiation of Regular_Node_Unparser.

   function Extract_Regular_Node_Template
     (Unparser : Regular_Node_Unparser; Rewritten_Node : Bare_Ada_Node)
      return Regular_Node_Template;
   --  Return the regular node template corresponding to the instatiation of
   --  Rewritten_Node according to Unparser.
   --
   --  This is an absent template if Rewritten_Node is null. Likewise, returned
   --  field templates are absent if the corresponding Rewritten_Node children
   --  are absent.

   function Field_Present
     (Node : Abstract_Node; Unparser : Field_Unparser) return Boolean;
   --  Return whether the given field is to be considered present according to
   --  the given field unparser.

   procedure Update_Sloc
     (Sloc : in out Source_Location; Char : Wide_Wide_Character);
   --  Update Sloc as if it represented a cursor that move right-wards after
   --  inserting Char to a buffer.

   procedure Unparse_Node
     (Node   :        Abstract_Node; Preserve_Formatting : Boolean;
      Result : in out Unparsing_Buffer);
   --  Using the Node_Unparsers unparsing tables, unparse the given Node

   procedure Unparse_Regular_Node
     (Node           :        Abstract_Node; Unparser : Regular_Node_Unparser;
      Rewritten_Node :        Bare_Ada_Node; Preserve_Formatting : Boolean;
      Result         : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on regular nodes

   procedure Unparse_List_Node
     (Node           :        Abstract_Node; Unparser : List_Node_Unparser;
      Rewritten_Node :        Bare_Ada_Node; Preserve_Formatting : Boolean;
      Result         : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on list nodes

   procedure Unparse_Token
     (Unparser : Token_Unparser; Result : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a token

   procedure Unparse_Token_Sequence
     (Unparser : Token_Sequence_Access; Result : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a sequence of tokens

   function Relative_Token
     (Token : Token_Reference; Offset : Integer) return Token_Reference with
      Pre => Token /= No_Token and then not Is_Trivia (Token)
      and then Token_Index
        (Integer (Get_Token_Index (Token).Token) + Offset) in
        First_Token_Index .. Last_Token (Get_Token_TDH (Token).all),
      Post => Relative_Token'Result /= No_Token;
      --  Considering only tokens that are not trivia and assuming Token is at
      --  index I, return the token that is at index I + Offset.

   function Last_Trivia (Token : Token_Reference) return Token_Reference with
      Pre => Token /= No_Token;
      --  If Token (which can be a token or a trivia) is followed by a sequence
      --  of trivias, return the last of them. Otherwise, return Token itself.

   procedure Append_Tokens
     (Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token :        Token_Reference;
      With_Trailing_Trivia    :        Boolean := True);
   --  Emit to Result the sequence of tokens from First_Token to Last_Token.
   --  Trivias that appear between tokens in the sequence to emit are emitted
   --  as well. If With_Trailing_Trivia is true, also emit the sequence of
   --  trivia that follows Last_Token.

   procedure Append_Tokens
     (Result : in out Unparsing_Buffer; Template : Token_Sequence_Template);
   --  Emit to Result the sequence of tokens in Template, or do nothing if the
   --  template is absent.

   Token_Spacing_Table : array (Token_Family, Token_Family) of Boolean :=

     (Common.Alphanumericals =>
        (Common.Alphanumericals => True, Common.Default_Family => False),
      Common.Default_Family =>
        (Common.Alphanumericals => False, Common.Default_Family => False));
   --  A space must be inserted between two consecutive tokens T1 and T2 iff
   --  given their respective families TF1 and TF2, the following is true:
   --  Token_Spacing_Table (TF1, TF2).

   Token_Newline_Table : array (Token_Kind) of Boolean :=
     (Common.Ada_Termination => False, Common.Ada_Lexing_Failure => False,
      Common.Ada_Identifier  => False, Common.Ada_All => False,
      Common.Ada_Abort       => False, Common.Ada_Else => False,
      Common.Ada_New         => False, Common.Ada_Return => False,
      Common.Ada_Abs         => False, Common.Ada_Elsif => False,
      Common.Ada_Not         => False, Common.Ada_Reverse => False,
      Common.Ada_End         => False, Common.Ada_Null => False,
      Common.Ada_Accept      => False, Common.Ada_Entry => False,
      Common.Ada_Select      => False, Common.Ada_Access => False,
      Common.Ada_Exception   => False, Common.Ada_Of => False,
      Common.Ada_Separate    => False, Common.Ada_Exit => False,
      Common.Ada_Or          => False, Common.Ada_Others => False,
      Common.Ada_Subtype     => False, Common.Ada_And => False,
      Common.Ada_For         => False, Common.Ada_Out => False,
      Common.Ada_Array       => False, Common.Ada_Function => False,
      Common.Ada_At          => False, Common.Ada_Generic => False,
      Common.Ada_Package     => False, Common.Ada_Task => False,
      Common.Ada_Begin       => False, Common.Ada_Goto => False,
      Common.Ada_Pragma      => False, Common.Ada_Terminate => False,
      Common.Ada_Body        => False, Common.Ada_Private => False,
      Common.Ada_Then        => False, Common.Ada_If => False,
      Common.Ada_Procedure   => False, Common.Ada_Type => False,
      Common.Ada_Case        => False, Common.Ada_In => False,
      Common.Ada_Constant    => False, Common.Ada_Is => False,
      Common.Ada_Raise       => False, Common.Ada_Use => False,
      Common.Ada_Declare     => False, Common.Ada_Range => False,
      Common.Ada_Delay       => False, Common.Ada_Limited => False,
      Common.Ada_Record      => False, Common.Ada_When => False,
      Common.Ada_Delta       => False, Common.Ada_Loop => False,
      Common.Ada_Rem         => False, Common.Ada_While => False,
      Common.Ada_Digits      => False, Common.Ada_Renames => False,
      Common.Ada_Do => False, Common.Ada_Mod => False, Common.Ada_Xor => False,
      Common.Ada_Par_Close   => False, Common.Ada_Par_Open => False,
      Common.Ada_Semicolon   => False, Common.Ada_Colon => False,
      Common.Ada_Comma       => False, Common.Ada_Doubledot => False,
      Common.Ada_Dot         => False, Common.Ada_Diamond => False,
      Common.Ada_Lte         => False, Common.Ada_Gte => False,
      Common.Ada_Arrow       => False, Common.Ada_Equal => False,
      Common.Ada_Lt => False, Common.Ada_Gt => False, Common.Ada_Plus => False,
      Common.Ada_Minus       => False, Common.Ada_Power => False,
      Common.Ada_Mult        => False, Common.Ada_Amp => False,
      Common.Ada_Notequal    => False, Common.Ada_Divide => False,
      Common.Ada_Tick        => False, Common.Ada_Pipe => False,
      Common.Ada_Assign      => False, Common.Ada_Label_Start => False,
      Common.Ada_Label_End   => False, Common.Ada_Target => False,
      Common.Ada_String      => False, Common.Ada_Char => False,
      Common.Ada_With        => False, Common.Ada_Decimal => False,
      Common.Ada_Integer     => False, Common.Ada_Comment => True,
      Common.Ada_Prep_Line   => True, Common.Ada_Whitespace => False);
   --  A line break must be append during unparsing after a token T iff
   --  Token_Newline_Table (T) is true.

   --------------------------
   -- Create_Abstract_Node --
   --------------------------

   function Create_Abstract_Node
     (Parsing_Node : Bare_Ada_Node) return Abstract_Node
   is
   begin
      return (From_Parsing, Parsing_Node);
   end Create_Abstract_Node;

   --------------------------
   -- Create_Abstract_Node --
   --------------------------

   function Create_Abstract_Node
     (Rewriting_Node : Node_Rewriting_Handle) return Abstract_Node
   is
   begin
      return (From_Rewriting, Rewriting_Node);
   end Create_Abstract_Node;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Abstract_Node) return Boolean is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node = null;
         when From_Rewriting =>
            return Node.Rewriting_Node = null;
      end case;
   end Is_Null;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Abstract_Node) return Ada_Node_Kind_Type is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node.Kind;
         when From_Rewriting =>
            return Node.Rewriting_Node.Kind;
      end case;
   end Kind;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Abstract_Node) return Natural is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Children_Count (Node.Parsing_Node);
         when From_Rewriting =>
            return Children_Count (Node.Rewriting_Node);
      end case;
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child (Node : Abstract_Node; Index : Positive) return Abstract_Node
   is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Create_Abstract_Node (Child (Node.Parsing_Node, Index));

         when From_Rewriting =>
            --  In the context of unparsing, it is pointless to expand the
            --  rewritting tree (which is what Rewriting_Implementation.Child
            --  does). If the node is not expanded, switch to the original node
            --  instead.

            declare
               RN : constant Node_Rewriting_Handle := Node.Rewriting_Node;
            begin
               if RN.Children.Kind = Unexpanded then
                  return Create_Abstract_Node (Child (RN.Node, Index));
               else
                  return Create_Abstract_Node (Child (RN, Index));
               end if;
            end;
      end case;
   end Child;

   ----------
   -- Text --
   ----------

   function Text (Node : Abstract_Node) return Text_Type is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Text (Node.Parsing_Node);
         when From_Rewriting =>
            return Text (Node.Rewriting_Node);
      end case;
   end Text;

   --------------------
   -- Rewritten_Node --
   --------------------

   function Rewritten_Node (Node : Abstract_Node) return Bare_Ada_Node is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node;
         when From_Rewriting =>
            return Node.Rewriting_Node.Node;
      end case;
   end Rewritten_Node;

   ---------------------------
   -- Create_Token_Sequence --
   ---------------------------

   function Create_Token_Sequence
     (Unparser : Token_Sequence_Access; First_Token : in out Token_Reference)
      return Present_Token_Sequence_Template
   is
      Result : Present_Token_Sequence_Template;
   begin
      if Unparser'Length = 0 then
         return (Present => True, First => No_Token, Last => No_Token);
      else
         Result.First := First_Token;
         Result.Last  := Relative_Token (First_Token, Unparser'Length - 1);
         First_Token  := Relative_Token (Result.Last, 1);
         return Result;
      end if;
   end Create_Token_Sequence;

   -----------------------------------
   -- Extract_Regular_Node_Template --
   -----------------------------------

   function Extract_Regular_Node_Template
     (Unparser : Regular_Node_Unparser; Rewritten_Node : Bare_Ada_Node)
      return Regular_Node_Template
   is
      Result     : Regular_Node_Template (True, Unparser.Field_Unparsers.N);
      Next_Token : Token_Reference;
   begin
      if Rewritten_Node = null then
         return (Present => False, Count => 0);
      end if;

      Next_Token := Token_Start (Rewritten_Node);

      --  Recover tokens that precede the first field from the rewritten node
      Result.Pre_Tokens :=
        Create_Token_Sequence (Unparser.Pre_Tokens, Next_Token);

      --  For each field, recover the tokens that surround the field itself,
      --  but only if both the original node and the one to unparse are
      --  present.
      for I in 1 .. Children_Count (Rewritten_Node) loop
         declare
            U  : Field_Unparser_List renames Unparser.Field_Unparsers.all;
            F  : Field_Unparser renames U.Field_Unparsers (I);
            T  : Token_Sequence_Access renames U.Inter_Tokens (I);
            FT : Field_Template renames Result.Fields (I);

            Rewritten_Child : constant Bare_Ada_Node :=
              Child (Rewritten_Node, I);
            R_Child : constant Abstract_Node :=
              Create_Abstract_Node (Rewritten_Child);
         begin
            Result.Inter_Tokens (I) :=
              (if I = 1 then Empty_Token_Sequence_Template
               else Create_Token_Sequence (T, Next_Token));

            if Field_Present (R_Child, F) then
               FT := (Present => True, others => <>);

               --  Pre_Tokens is the sequence that starts at Next_Token and
               --  whose length is the one the unparser gives.
               FT.Pre_Tokens :=
                 Create_Token_Sequence (F.Pre_Tokens, Next_Token);

               --  Post_Tokens is the sequence that starts right after the
               --  last token of the node field, also sized from the unparser.
               --  Beware of ghost nodes, which own no token.
               Next_Token :=
                 (if Is_Ghost (Rewritten_Child) then
                    Token_Start (Rewritten_Child)
                  else Relative_Token (Token_End (Rewritten_Child), 1));
               FT.Post_Tokens :=
                 Create_Token_Sequence (F.Post_Tokens, Next_Token);

            else
               FT := (Present => False);
            end if;
         end;
      end loop;

      --  Recover tokens that succeed to the first field from the rewritten
      --  node.
      Result.Post_Tokens :=
        Create_Token_Sequence (Unparser.Post_Tokens, Next_Token);

      return Result;
   end Extract_Regular_Node_Template;

   -------------------
   -- Field_Present --
   -------------------

   function Field_Present
     (Node : Abstract_Node; Unparser : Field_Unparser) return Boolean
   is
   begin
      return
        (not Is_Null (Node)
         and then
         (not Unparser.Empty_List_Is_Absent
          or else Children_Count (Node) > 0));
   end Field_Present;

   -----------------
   -- Update_Sloc --
   -----------------

   procedure Update_Sloc
     (Sloc : in out Source_Location; Char : Wide_Wide_Character)
   is
   begin
      --  TODO??? Handle tabs

      if Wide_Wide_Character'Pos (Char) = Character'Pos (ASCII.LF) then
         Sloc.Line   := Sloc.Line + 1;
         Sloc.Column := 1;
      else
         Sloc.Column := Sloc.Column + 1;
      end if;
   end Update_Sloc;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Unparsing_Buffer; Char : Wide_Wide_Character)
   is
   begin
      Update_Sloc (Buffer.Last_Sloc, Char);
      Append (Buffer.Content, Char);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Unparsing_Buffer; Kind : Token_Kind; Text : Text_Type)
   is
   begin
      for C of Text loop
         Update_Sloc (Buffer.Last_Sloc, C);
      end loop;
      Append (Buffer.Content, Text);
      Buffer.Last_Token := Kind;
   end Append;

   -------------------------
   -- Apply_Spacing_Rules --
   -------------------------

   procedure Apply_Spacing_Rules
     (Buffer : in out Unparsing_Buffer; Next_Token : Token_Kind)
   is
   begin
      if Length (Buffer.Content) = 0 then
         null;

      elsif Token_Newline_Table (Buffer.Last_Token) then
         Append (Buffer, Chars.LF);

      elsif Token_Spacing_Table
          (Token_Kind_To_Family (Buffer.Last_Token),
           Token_Kind_To_Family (Next_Token))
      then
         Append (Buffer, ' ');
      end if;
   end Apply_Spacing_Rules;

   -------------
   -- Unparse --
   -------------

   procedure Unparse
     (Node                :     Abstract_Node; Unit : Internal_Unit;
      Preserve_Formatting :     Boolean; As_Unit : Boolean;
      Result              : out Unparsing_Buffer)
   is
   begin
      --  Unparse Node, and the leading trivia if we are unparsing the unit as
      --  a whole.
      if As_Unit then
         declare
            First : constant Token_Reference := First_Token (Unit);
         begin
            if Is_Trivia (First) then
               Append_Tokens
                 (Result, First, Last_Trivia (First),
                  With_Trailing_Trivia => False);
            end if;
         end;
      end if;
      Unparse_Node (Node, Preserve_Formatting, Result);
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node; Unit : Internal_Unit;
      Preserve_Formatting : Boolean; As_Unit : Boolean) return String
   is
      Result : String_Access :=
        Unparse (Node, Unit, Preserve_Formatting, As_Unit);
      R : constant String := Result.all;
   begin
      Free (Result);
      return R;
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node; Unit : Internal_Unit;
      Preserve_Formatting : Boolean; As_Unit : Boolean) return String_Access
   is
      use Ada.Strings.Wide_Wide_Unbounded.Aux;

      Buffer : Unparsing_Buffer;
      --  Buffer to store the result of unparsing as text

      Buffer_Access : Big_Wide_Wide_String_Access;
      Length        : Natural;
      --  Buffer internals, to avoid costly buffer copies
   begin
      Unparse (Node, Unit, Preserve_Formatting, As_Unit, Buffer);
      Get_Wide_Wide_String (Buffer.Content, Buffer_Access, Length);

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.
      if Length = 0 then
         return new String'("");
      end if;

      declare
         use GNATCOLL.Iconv;

         State : Iconv_T :=
           Iconv_Open
             (To_Code => Get_Charset (Unit), From_Code => Text_Charset);
         Status : Iconv_Result;

         To_Convert_String : constant String (1 .. 4 * Length) with
            Import     => True,
            Convention => Ada,
            Address    => Buffer_Access.all'Address;

         Output_Buffer : String_Access :=
           new String (1 .. 4 * To_Convert_String'Length);
         --  Encodings should not take more than 4 bytes per code point, so
         --  this should be enough to hold the conversion.

         Input_Index  : Positive := To_Convert_String'First;
         Output_Index : Positive := Output_Buffer'First;
      begin
         --  TODO??? Use GNATCOLL.Iconv to properly encode this wide wide
         --  string into a mere string using this unit's charset.
         Iconv
           (State, To_Convert_String, Input_Index, Output_Buffer.all,
            Output_Index, Status);
         Iconv_Close (State);
         case Status is
            when Success =>
               null;
            when others =>
               raise Program_Error with "cannot encode result";
         end case;

         declare
            Result_Slice : String renames
              Output_Buffer (Output_Buffer'First .. Output_Index - 1);
            Result : constant String_Access := new String (Result_Slice'Range);
         begin
            Result.all := Result_Slice;
            Free (Output_Buffer);
            return Result;
         end;
      end;
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node; Unit : Internal_Unit;
      Preserve_Formatting : Boolean; As_Unit : Boolean)
      return Unbounded_Text_Type
   is
      Buffer : Unparsing_Buffer;
   begin
      if Is_Null (Node) then
         return (raise Program_Error with "cannot unparse null node");
      elsif As_Unit and then Unit = null then
         return
           (raise Program_Error
              with "cannot unparse node as unit without a unit");
      end if;

      Unparse (Node, Unit, Preserve_Formatting, As_Unit, Buffer);
      return Buffer.Content;
   end Unparse;

   ------------------
   -- Unparse_Node --
   ------------------

   procedure Unparse_Node
     (Node   :        Abstract_Node; Preserve_Formatting : Boolean;
      Result : in out Unparsing_Buffer)
   is
      Kind : constant Ada_Node_Kind_Type :=
        Unparsing_Implementation.Kind (Node);
      Unparser : Node_Unparser renames Node_Unparsers (Kind);

      Rewritten_Node : constant Bare_Ada_Node :=
        (if Preserve_Formatting then
           Unparsing_Implementation.Rewritten_Node (Node)
         else null);
   begin
      case Unparser.Kind is
         when Regular =>
            Unparse_Regular_Node
              (Node, Unparser, Rewritten_Node, Preserve_Formatting, Result);

         when List =>
            Unparse_List_Node
              (Node, Unparser, Rewritten_Node, Preserve_Formatting, Result);

         when Token =>
            declare
               Tok_Kind : constant Token_Kind := Token_Node_Kind (Kind);
            begin
               --  Add the single token that materialize Node itself
               Apply_Spacing_Rules (Result, Tok_Kind);
               Append (Result, Tok_Kind, Text (Node));

               --  If Node comes from an original node, also append the trivia
               --  that comes after.
               if Rewritten_Node /= null then
                  declare
                     Token : constant Token_Reference :=
                       Token_End (Rewritten_Node);
                     Last_Triv : constant Token_Reference :=
                       Last_Trivia (Token);
                  begin
                     Append_Tokens
                       (Result, Next (Token), Last_Triv,
                        With_Trailing_Trivia => False);
                  end;
               end if;
            end;
      end case;
   end Unparse_Node;

   --------------------------
   -- Unparse_Regular_Node --
   --------------------------

   procedure Unparse_Regular_Node
     (Node           :        Abstract_Node; Unparser : Regular_Node_Unparser;
      Rewritten_Node :        Bare_Ada_Node; Preserve_Formatting : Boolean;
      Result         : in out Unparsing_Buffer)
   is
      Template : constant Regular_Node_Template :=
        Extract_Regular_Node_Template (Unparser, Rewritten_Node);
   begin
      --  Unparse tokens that precede the first field. Re-use original ones if
      --  available.
      if Template.Present then
         Append_Tokens (Result, Template.Pre_Tokens);
      else
         Unparse_Token_Sequence (Unparser.Pre_Tokens, Result);
      end if;

      --  Unparse Node's fields, and the tokens between them
      declare
         U : Field_Unparser_List renames Unparser.Field_Unparsers.all;
      begin
         for I in 1 .. U.N loop
            declare
               F     : Field_Unparser renames U.Field_Unparsers (I);
               Child : constant Abstract_Node :=
                 Unparsing_Implementation.Child (Node, I);
            begin
               --  First unparse tokens that appear unconditionally between
               --  fields.
               if Template.Present then
                  Append_Tokens (Result, Template.Inter_Tokens (I));
               else
                  Unparse_Token_Sequence (U.Inter_Tokens (I), Result);
               end if;

               --  Then unparse the field itself
               if Field_Present (Child, F) then
                  if Template.Present and then Template.Fields (I).Present then
                     Append_Tokens (Result, Template.Fields (I).Pre_Tokens);
                     Unparse_Node (Child, Preserve_Formatting, Result);
                     Append_Tokens (Result, Template.Fields (I).Post_Tokens);

                  else
                     Unparse_Token_Sequence (F.Pre_Tokens, Result);
                     Unparse_Node (Child, Preserve_Formatting, Result);
                     Unparse_Token_Sequence (F.Post_Tokens, Result);
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Unparse tokens that suceed to the last field. Re-use original ones if
      --  available.
      if Template.Present then
         Append_Tokens (Result, Template.Post_Tokens);
      else
         Unparse_Token_Sequence (Unparser.Post_Tokens, Result);
      end if;
   end Unparse_Regular_Node;

   -----------------------
   -- Unparse_List_Node --
   -----------------------

   procedure Unparse_List_Node
     (Node           :        Abstract_Node; Unparser : List_Node_Unparser;
      Rewritten_Node :        Bare_Ada_Node; Preserve_Formatting : Boolean;
      Result         : in out Unparsing_Buffer)
   is
   begin
      for I in 1 .. Children_Count (Node) loop
         --  For all elements but the first one, emit the separator. If
         --  possible, preserve original formatting for the corresponding
         --  separator in the original source.
         if I > 1 and then Unparser.Has_Separator then
            if Rewritten_Node /= null
              and then Children_Count (Rewritten_Node) >= I
            then
               declare
                  R_Child : constant Bare_Ada_Node :=
                    Child (Rewritten_Node, I);
                  Tok : constant Token_Reference :=
                    Relative_Token (Token_Start (R_Child), -1);
               begin
                  Append_Tokens (Result, Tok, Tok);
               end;
            else
               Unparse_Token (Unparser.Separator, Result);
            end if;
         end if;

         Unparse_Node (Child (Node, I), Preserve_Formatting, Result);
      end loop;
   end Unparse_List_Node;

   -------------------
   -- Unparse_Token --
   -------------------

   procedure Unparse_Token
     (Unparser : Token_Unparser; Result : in out Unparsing_Buffer)
   is
   begin
      Apply_Spacing_Rules (Result, Unparser.Kind);
      if Unparser.Text /= null then
         Append (Result, Unparser.Kind, Unparser.Text.all);
      else
         declare
            Literal : constant Text_Type := Token_Kind_Literal (Unparser.Kind);
         begin
            pragma Assert (Literal'Length > 0);
            Append (Result, Unparser.Kind, Literal);
         end;
      end if;
   end Unparse_Token;

   ----------------------------
   -- Unparse_Token_Sequence --
   ----------------------------

   procedure Unparse_Token_Sequence
     (Unparser : Token_Sequence_Access; Result : in out Unparsing_Buffer)
   is
   begin
      for U of Unparser.all loop
         Unparse_Token (U, Result);
      end loop;
   end Unparse_Token_Sequence;

   --------------------
   -- Relative_Token --
   --------------------

   function Relative_Token
     (Token : Token_Reference; Offset : Integer) return Token_Reference
   is
      Current_Token  : Token_Reference := Token;
      Current_Offset : Integer         := 0;
   begin
      if Offset < 0 then
         while Current_Offset > Offset loop
            Current_Token := Previous (Current_Token);
            if not Is_Trivia (Current_Token) then
               Current_Offset := Current_Offset - 1;
            end if;
         end loop;

      else
         while Current_Offset < Offset loop
            Current_Token := Next (Current_Token);
            if not Is_Trivia (Current_Token) then
               Current_Offset := Current_Offset + 1;
            end if;
         end loop;
      end if;

      return Current_Token;
   end Relative_Token;

   -----------------
   -- Last_Trivia --
   -----------------

   function Last_Trivia (Token : Token_Reference) return Token_Reference is
      Result : Token_Reference := Token;
      Cur    : Token_Reference := Next (Token);
   begin
      --  Move Last to the last trivia that comes before the next token
      while Cur /= No_Token and then Is_Trivia (Cur) loop
         Result := Cur;
         Cur    := Next (Cur);
      end loop;
      return Result;
   end Last_Trivia;

   -------------------
   -- Append_Tokens --
   -------------------

   procedure Append_Tokens
     (Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token :        Token_Reference;
      With_Trailing_Trivia    :        Boolean := True)
   is
   begin
      if (First_Token = No_Token and then Last_Token = No_Token)
        or else Last_Token < First_Token
      then
         return;
      end if;
      pragma Assert (First_Token /= No_Token and then Last_Token /= No_Token);
      Apply_Spacing_Rules (Result, Kind (Data (First_Token)));

      declare
         Last : constant Token_Reference :=
           (if With_Trailing_Trivia then Last_Trivia (Last_Token)
            else Last_Token);
      begin
         Append (Result, Kind (Data (Last)), Text (First_Token, Last));
      end;
   end Append_Tokens;

   -------------------
   -- Append_Tokens --
   -------------------

   procedure Append_Tokens
     (Result : in out Unparsing_Buffer; Template : Token_Sequence_Template)
   is
   begin
      if Template.Present then
         Append_Tokens (Result, Template.First, Template.Last);
      end if;
   end Append_Tokens;

   Token_Unparser_0  : aliased constant Token_Unparser := (Ada_Pipe, null);
   Token_Unparser_1  : aliased constant Token_Unparser := (Ada_Amp, null);
   Token_Unparser_2  : aliased constant Token_Unparser := (Ada_Tick, null);
   Token_Unparser_3  : aliased constant Token_Unparser := (Ada_Par_Open, null);
   Token_Unparser_4 : aliased constant Token_Unparser := (Ada_Par_Close, null);
   Token_Unparser_5  : aliased constant Token_Unparser := (Ada_Mult, null);
   Token_Unparser_6  : aliased constant Token_Unparser := (Ada_Power, null);
   Token_Unparser_7  : aliased constant Token_Unparser := (Ada_Plus, null);
   Token_Unparser_8  : aliased constant Token_Unparser := (Ada_Comma, null);
   Token_Unparser_9  : aliased constant Token_Unparser := (Ada_Minus, null);
   Token_Unparser_10 : aliased constant Token_Unparser := (Ada_Dot, null);
   Token_Unparser_11 : aliased constant Token_Unparser :=
     (Ada_Doubledot, null);
   Token_Unparser_12 : aliased constant Token_Unparser := (Ada_Divide, null);
   Token_Unparser_13 : aliased constant Token_Unparser := (Ada_Notequal, null);
   Token_Unparser_14 : aliased constant Token_Unparser := (Ada_Colon, null);
   Token_Unparser_15 : aliased constant Token_Unparser := (Ada_Assign, null);
   Token_Unparser_16 : aliased constant Token_Unparser :=
     (Ada_Semicolon, null);
   Token_Unparser_17 : aliased constant Token_Unparser := (Ada_Lt, null);
   Token_Unparser_18 : aliased constant Token_Unparser :=
     (Ada_Label_Start, null);
   Token_Unparser_19 : aliased constant Token_Unparser := (Ada_Lte, null);
   Token_Unparser_20 : aliased constant Token_Unparser := (Ada_Diamond, null);
   Token_Unparser_21 : aliased constant Token_Unparser := (Ada_Equal, null);
   Token_Unparser_22 : aliased constant Token_Unparser := (Ada_Arrow, null);
   Token_Unparser_23 : aliased constant Token_Unparser := (Ada_Gt, null);
   Token_Unparser_24 : aliased constant Token_Unparser := (Ada_Gte, null);
   Token_Unparser_25 : aliased constant Token_Unparser :=
     (Ada_Label_End, null);
   Token_Unparser_26 : aliased constant Token_Unparser := (Ada_Target, null);
   Token_Unparser_27 : aliased constant Token_Unparser := (Ada_Abort, null);
   Token_Unparser_28 : aliased constant Token_Unparser := (Ada_Abs, null);
   Token_Unparser_29 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("abstract"));
   Token_Unparser_30 : aliased constant Token_Unparser := (Ada_Accept, null);
   Token_Unparser_31 : aliased constant Token_Unparser := (Ada_Access, null);
   Token_Unparser_32 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("aliased"));
   Token_Unparser_33 : aliased constant Token_Unparser := (Ada_All, null);
   Token_Unparser_34 : aliased constant Token_Unparser := (Ada_And, null);
   Token_Unparser_35 : aliased constant Token_Unparser := (Ada_Array, null);
   Token_Unparser_36 : aliased constant Token_Unparser := (Ada_At, null);
   Token_Unparser_37 : aliased constant Token_Unparser := (Ada_Begin, null);
   Token_Unparser_38 : aliased constant Token_Unparser := (Ada_Body, null);
   Token_Unparser_39 : aliased constant Token_Unparser := (Ada_Case, null);
   Token_Unparser_40 : aliased constant Token_Unparser := (Ada_Constant, null);
   Token_Unparser_41 : aliased constant Token_Unparser := (Ada_Declare, null);
   Token_Unparser_42 : aliased constant Token_Unparser := (Ada_Delay, null);
   Token_Unparser_43 : aliased constant Token_Unparser := (Ada_Delta, null);
   Token_Unparser_44 : aliased constant Token_Unparser := (Ada_Digits, null);
   Token_Unparser_45 : aliased constant Token_Unparser := (Ada_Do, null);
   Token_Unparser_46 : aliased constant Token_Unparser := (Ada_Else, null);
   Token_Unparser_47 : aliased constant Token_Unparser := (Ada_Elsif, null);
   Token_Unparser_48 : aliased constant Token_Unparser := (Ada_End, null);
   Token_Unparser_49 : aliased constant Token_Unparser := (Ada_Entry, null);
   Token_Unparser_50 : aliased constant Token_Unparser :=
     (Ada_Exception, null);
   Token_Unparser_51 : aliased constant Token_Unparser := (Ada_Exit, null);
   Token_Unparser_52 : aliased constant Token_Unparser := (Ada_For, null);
   Token_Unparser_53 : aliased constant Token_Unparser := (Ada_Function, null);
   Token_Unparser_54 : aliased constant Token_Unparser := (Ada_Generic, null);
   Token_Unparser_55 : aliased constant Token_Unparser := (Ada_Goto, null);
   Token_Unparser_56 : aliased constant Token_Unparser := (Ada_If, null);
   Token_Unparser_57 : aliased constant Token_Unparser := (Ada_In, null);
   Token_Unparser_58 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("interface"));
   Token_Unparser_59 : aliased constant Token_Unparser := (Ada_Is, null);
   Token_Unparser_60 : aliased constant Token_Unparser := (Ada_Limited, null);
   Token_Unparser_61 : aliased constant Token_Unparser := (Ada_Loop, null);
   Token_Unparser_62 : aliased constant Token_Unparser := (Ada_Mod, null);
   Token_Unparser_63 : aliased constant Token_Unparser := (Ada_New, null);
   Token_Unparser_64 : aliased constant Token_Unparser := (Ada_Not, null);
   Token_Unparser_65 : aliased constant Token_Unparser := (Ada_Null, null);
   Token_Unparser_66 : aliased constant Token_Unparser := (Ada_Of, null);
   Token_Unparser_67 : aliased constant Token_Unparser := (Ada_Or, null);
   Token_Unparser_68 : aliased constant Token_Unparser := (Ada_Others, null);
   Token_Unparser_69 : aliased constant Token_Unparser := (Ada_Out, null);
   Token_Unparser_70 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("overriding"));
   Token_Unparser_71 : aliased constant Token_Unparser := (Ada_Package, null);
   Token_Unparser_72 : aliased constant Token_Unparser := (Ada_Pragma, null);
   Token_Unparser_73 : aliased constant Token_Unparser := (Ada_Private, null);
   Token_Unparser_74 : aliased constant Token_Unparser :=
     (Ada_Procedure, null);
   Token_Unparser_75 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("protected"));
   Token_Unparser_76 : aliased constant Token_Unparser := (Ada_Raise, null);
   Token_Unparser_77 : aliased constant Token_Unparser := (Ada_Range, null);
   Token_Unparser_78 : aliased constant Token_Unparser := (Ada_Record, null);
   Token_Unparser_79 : aliased constant Token_Unparser := (Ada_Rem, null);
   Token_Unparser_80 : aliased constant Token_Unparser := (Ada_Renames, null);
   Token_Unparser_81 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("requeue"));
   Token_Unparser_82 : aliased constant Token_Unparser := (Ada_Return, null);
   Token_Unparser_83 : aliased constant Token_Unparser := (Ada_Reverse, null);
   Token_Unparser_84 : aliased constant Token_Unparser := (Ada_Select, null);
   Token_Unparser_85 : aliased constant Token_Unparser := (Ada_Separate, null);
   Token_Unparser_86 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("some"));
   Token_Unparser_87 : aliased constant Token_Unparser := (Ada_Subtype, null);
   Token_Unparser_88 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("synchronized"));
   Token_Unparser_89 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("tagged"));
   Token_Unparser_90 : aliased constant Token_Unparser := (Ada_Task, null);
   Token_Unparser_91 : aliased constant Token_Unparser :=
     (Ada_Terminate, null);
   Token_Unparser_92 : aliased constant Token_Unparser := (Ada_Then, null);
   Token_Unparser_93 : aliased constant Token_Unparser := (Ada_Type, null);
   Token_Unparser_94 : aliased constant Token_Unparser :=
     (Ada_Identifier, new Text_Type'("until"));
   Token_Unparser_95 : aliased constant Token_Unparser := (Ada_Use, null);
   Token_Unparser_96 : aliased constant Token_Unparser := (Ada_When, null);
   Token_Unparser_97 : aliased constant Token_Unparser := (Ada_While, null);
   Token_Unparser_98 : aliased constant Token_Unparser := (Ada_With, null);
   Token_Unparser_99 : aliased constant Token_Unparser := (Ada_Xor, null);

   Token_Sequence_1 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_1);
   Token_Sequence_2 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_2);
   Token_Sequence_3 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_3);
   Token_Sequence_4 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_3, 2 => Token_Unparser_20, 3 => Token_Unparser_4);
   Token_Sequence_5 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_3, 2 => Token_Unparser_52);
   Token_Sequence_6 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_4);
   Token_Sequence_7 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_4, 2 => Token_Unparser_66);
   Token_Sequence_8 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_5);
   Token_Sequence_9 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_6);
   Token_Sequence_10 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_7);
   Token_Sequence_11 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_9);
   Token_Sequence_12 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_10);
   Token_Sequence_13 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_10, 2 => Token_Unparser_33);
   Token_Sequence_14 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_11);
   Token_Sequence_15 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_12);
   Token_Sequence_16 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_13);
   Token_Sequence_17 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_14);
   Token_Sequence_18 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_14, 2 => Token_Unparser_40, 3 => Token_Unparser_15);
   Token_Sequence_19 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_14, 2 => Token_Unparser_50);
   Token_Sequence_20 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_15);
   Token_Sequence_21 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_16);
   Token_Sequence_22 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_17);
   Token_Sequence_23 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_18);
   Token_Sequence_24 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_19);
   Token_Sequence_25 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_20);
   Token_Sequence_26 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_21);
   Token_Sequence_27 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_22);
   Token_Sequence_28 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_23);
   Token_Sequence_29 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_24);
   Token_Sequence_30 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_25);
   Token_Sequence_31 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_26);
   Token_Sequence_32 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_27);
   Token_Sequence_33 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_28);
   Token_Sequence_34 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_29);
   Token_Sequence_35 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_30);
   Token_Sequence_36 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_31);
   Token_Sequence_37 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_32);
   Token_Sequence_38 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_33);
   Token_Sequence_39 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_34);
   Token_Sequence_40 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_34, 2 => Token_Unparser_92);
   Token_Sequence_41 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_35, 2 => Token_Unparser_3);
   Token_Sequence_42 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_36);
   Token_Sequence_43 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_36, 2 => Token_Unparser_62);
   Token_Sequence_44 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_37);
   Token_Sequence_45 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_39);
   Token_Sequence_46 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_40);
   Token_Sequence_47 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_41);
   Token_Sequence_48 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_42);
   Token_Sequence_49 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_43);
   Token_Sequence_50 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_44);
   Token_Sequence_51 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_45);
   Token_Sequence_52 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_46);
   Token_Sequence_53 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_47);
   Token_Sequence_54 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48);
   Token_Sequence_55 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_39, 3 => Token_Unparser_16);
   Token_Sequence_56 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_56, 3 => Token_Unparser_16);
   Token_Sequence_57 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_61);
   Token_Sequence_58 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_78);
   Token_Sequence_59 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_78, 3 => Token_Unparser_16);
   Token_Sequence_60 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_82);
   Token_Sequence_61 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_48, 2 => Token_Unparser_84, 3 => Token_Unparser_16);
   Token_Sequence_62 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_49);
   Token_Sequence_63 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_50);
   Token_Sequence_64 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_51);
   Token_Sequence_65 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_52);
   Token_Sequence_66 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_53);
   Token_Sequence_67 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_54);
   Token_Sequence_68 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_54, 2 => Token_Unparser_71);
   Token_Sequence_69 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_55);
   Token_Sequence_70 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_56);
   Token_Sequence_71 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_57);
   Token_Sequence_72 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_57, 2 => Token_Unparser_69);
   Token_Sequence_73 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_58);
   Token_Sequence_74 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_59);
   Token_Sequence_75 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_59, 2 => Token_Unparser_29);
   Token_Sequence_76 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_59, 2 => Token_Unparser_63);
   Token_Sequence_77 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_59, 2 => Token_Unparser_65);
   Token_Sequence_78 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_59, 2 => Token_Unparser_85);
   Token_Sequence_79 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_60);
   Token_Sequence_80 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_61);
   Token_Sequence_81 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_62);
   Token_Sequence_82 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_63);
   Token_Sequence_83 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_64);
   Token_Sequence_84 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_64, 2 => Token_Unparser_57);
   Token_Sequence_85 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_64, 2 => Token_Unparser_65);
   Token_Sequence_86 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_64, 2 => Token_Unparser_70);
   Token_Sequence_87 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_65, 2 => Token_Unparser_16);
   Token_Sequence_88 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_65, 2 => Token_Unparser_78);
   Token_Sequence_89 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_66);
   Token_Sequence_90 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_67);
   Token_Sequence_91 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_67, 2 => Token_Unparser_46);
   Token_Sequence_92 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_68);
   Token_Sequence_93 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_69);
   Token_Sequence_94 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_70);
   Token_Sequence_95 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_71);
   Token_Sequence_96 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_71, 2 => Token_Unparser_38);
   Token_Sequence_97 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_72);
   Token_Sequence_98 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_73);
   Token_Sequence_99 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_74);
   Token_Sequence_100 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_75);
   Token_Sequence_101 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_75, 2 => Token_Unparser_38);
   Token_Sequence_102 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_75, 2 => Token_Unparser_93);
   Token_Sequence_103 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_76);
   Token_Sequence_104 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_77);
   Token_Sequence_105 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_77, 2 => Token_Unparser_20);
   Token_Sequence_106 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_78);
   Token_Sequence_107 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_79);
   Token_Sequence_108 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_80);
   Token_Sequence_109 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_81);
   Token_Sequence_110 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_82);
   Token_Sequence_111 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_83);
   Token_Sequence_112 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_84);
   Token_Sequence_113 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_85, 2 => Token_Unparser_3);
   Token_Sequence_114 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_86);
   Token_Sequence_115 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_87);
   Token_Sequence_116 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_88);
   Token_Sequence_117 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_89);
   Token_Sequence_118 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_89, 2 => Token_Unparser_16);
   Token_Sequence_119 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_90);
   Token_Sequence_120 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_90, 2 => Token_Unparser_38);
   Token_Sequence_121 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_90, 2 => Token_Unparser_93);
   Token_Sequence_122 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_91, 2 => Token_Unparser_16);
   Token_Sequence_123 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_92);
   Token_Sequence_124 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_92, 2 => Token_Unparser_27);
   Token_Sequence_125 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_93);
   Token_Sequence_126 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_94);
   Token_Sequence_127 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_95);
   Token_Sequence_128 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_95, 2 => Token_Unparser_36);
   Token_Sequence_129 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_95, 2 => Token_Unparser_78);
   Token_Sequence_130 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_96);
   Token_Sequence_131 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_97);
   Token_Sequence_132 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_98);
   Token_Sequence_133 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_98, 2 => Token_Unparser_27);
   Token_Sequence_134 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_98, 2 => Token_Unparser_73);
   Token_Sequence_135 : aliased constant Token_Sequence :=
     (1 => Token_Unparser_99);

   Bare_Constrained_Array_Indices_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Unconstrained_Array_Indices_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Aspect_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_27'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_At_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_128'Access));

   Bare_Attribute_Def_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_127'Access));

   Bare_Enum_Rep_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_127'Access));

   Bare_Record_Rep_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_43'Access, Token_Sequence_21'Access, False),
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_129'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Aspect_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Contract_Case_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_27'Access));

   Bare_Pragma_Argument_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_27'Access, False),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Entry_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_3'Access, Token_Sequence_6'Access, False),
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Subp_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_110'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Component_List_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Known_Discriminant_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Entry_Completion_Formal_Params_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Generic_Formal_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Null_Record_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Record_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Aggregate_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_27'Access, True),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Multi_Dim_Array_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_27'Access));

   Bare_Discriminant_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_27'Access, True),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Param_Assoc_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_27'Access, False),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Component_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => (Token_Sequence_20'Access, Empty_Token_Sequence'Access, False),
         4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_17'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Discriminant_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => (Token_Sequence_20'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_17'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Generic_Formal_Obj_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Generic_Formal_Package_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Generic_Formal_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Generic_Formal_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Param_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => (Token_Sequence_20'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_17'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access,
         5 => Empty_Token_Sequence'Access));

   Bare_Generic_Package_Internal_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_98'Access, Empty_Token_Sequence'Access, False),
         5 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Empty_Token_Sequence'Access,
         5 => Token_Sequence_54'Access));

   Bare_Package_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_98'Access, Empty_Token_Sequence'Access, False),
         5 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Empty_Token_Sequence'Access,
         5 => Token_Sequence_54'Access));

   Bare_Subtype_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_74'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Incomplete_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Incomplete_Tagged_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access));

   Bare_Protected_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_82'Access, Token_Sequence_132'Access, True),
         5 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_74'Access,
         5 => Empty_Token_Sequence'Access));

   Bare_Task_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Single_Task_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Anonymous_Type_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Abstract_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_75'Access));

   Bare_Abstract_Formal_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_75'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Concrete_Formal_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => (Token_Sequence_74'Access, Empty_Token_Sequence'Access, False),
         4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Entry_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Enum_Literal_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Generic_Subp_Internal_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Expr_Function_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Null_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_77'Access));

   Bare_Subp_Body_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 6,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => Empty_Field_Unparser, 6 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_74'Access,
         5 => Token_Sequence_44'Access, 6 => Token_Sequence_54'Access));

   Bare_Subp_Renaming_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Package_Body_Stub_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_78'Access));

   Bare_Protected_Body_Stub_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_78'Access));

   Bare_Subp_Body_Stub_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_78'Access));

   Bare_Task_Body_Stub_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_78'Access));

   Bare_Entry_Body_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 7,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => Empty_Field_Unparser, 6 => Empty_Field_Unparser,
         7 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_130'Access,
         5 => Token_Sequence_74'Access, 6 => Token_Sequence_44'Access,
         7 => Token_Sequence_54'Access));

   Bare_Package_Body_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_44'Access, Empty_Token_Sequence'Access, False),
         5 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Empty_Token_Sequence'Access,
         5 => Token_Sequence_54'Access));

   Bare_Protected_Body_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Token_Sequence_54'Access));

   Bare_Task_Body_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Token_Sequence_44'Access,
         5 => Token_Sequence_54'Access));

   Bare_Entry_Index_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_71'Access));

   Bare_Exception_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_19'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Exception_Handler_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_17'Access, False),
         2 => Empty_Field_Unparser, 3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_27'Access));

   Bare_For_Loop_Var_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_17'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Generic_Package_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Generic_Subp_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Generic_Package_Instantiation_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => (Token_Sequence_3'Access, Token_Sequence_6'Access, True),
         4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_76'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Generic_Subp_Instantiation_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 6,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => (Token_Sequence_3'Access, Token_Sequence_6'Access, True),
         6 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_76'Access,
         5 => Empty_Token_Sequence'Access, 6 => Empty_Token_Sequence'Access));

   Bare_Generic_Package_Renaming_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_108'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Generic_Subp_Renaming_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_108'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Label_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Named_Stmt_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Number_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_18'Access));

   Bare_Object_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 8,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => Empty_Field_Unparser,
         6 => (Token_Sequence_20'Access, Empty_Token_Sequence'Access, False),
         7 => Empty_Field_Unparser, 8 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_17'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access,
         5 => Empty_Token_Sequence'Access, 6 => Empty_Token_Sequence'Access,
         7 => Empty_Token_Sequence'Access, 8 => Empty_Token_Sequence'Access));

   Bare_Extended_Return_Stmt_Object_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 8,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => Empty_Field_Unparser,
         6 => (Token_Sequence_20'Access, Empty_Token_Sequence'Access, False),
         7 => Empty_Field_Unparser, 8 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_17'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access,
         5 => Empty_Token_Sequence'Access, 6 => Empty_Token_Sequence'Access,
         7 => Empty_Token_Sequence'Access, 8 => Empty_Token_Sequence'Access));

   Bare_Package_Renaming_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Single_Protected_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => (Token_Sequence_82'Access, Token_Sequence_132'Access, True),
         4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_74'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Single_Task_Decl_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Case_Stmt_Alternative_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_27'Access));

   Bare_Compilation_Unit_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Component_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_42'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Component_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Delta_Constraint_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Digits_Constraint_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Discriminant_Constraint_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Index_Constraint_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Range_Constraint_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Declarative_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Private_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Public_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Elsif_Expr_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_123'Access));

   Bare_Elsif_Stmt_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_123'Access));

   Bare_Allocator_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Token_Sequence_3'Access, Token_Sequence_6'Access, False),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Aggregate_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_132'Access, False),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Null_Record_Aggregate_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Empty_Token_Sequence'Access, Token_Sequence_132'Access, False),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_88'Access));

   Bare_Bin_Op_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Relation_Op_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Case_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_74'Access));

   Bare_Case_Expr_Alternative_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_27'Access));

   Bare_Contract_Cases_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_If_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_52'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_123'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Membership_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Attribute_Ref_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => (Token_Sequence_3'Access, Token_Sequence_6'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_2'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Update_Attribute_Ref_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_2'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Call_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_3'Access));

   Bare_Defining_Name_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Discrete_Subtype_Name_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Dotted_Name_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_12'Access));

   Bare_End_Name_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Explicit_Deref_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Qual_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_2'Access));

   Bare_Paren_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Quantified_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_27'Access));

   Bare_Raise_Expr_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_132'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Un_Op_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Handled_Stmts_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_63'Access, Empty_Token_Sequence'Access, True)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Library_Item_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_For_Loop_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_While_Loop_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Params_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Pragma_Node_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_3'Access, Token_Sequence_6'Access, True)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Protected_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_98'Access, Empty_Token_Sequence'Access, False),
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_54'Access));

   Bare_Range_Spec_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Renaming_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Select_When_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => (Token_Sequence_130'Access, Token_Sequence_27'Access, False),
         2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Accept_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_3'Access, Token_Sequence_6'Access, False),
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Accept_Stmt_With_Stmts_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 5,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_3'Access, Token_Sequence_6'Access, False),
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_51'Access,
         5 => Token_Sequence_54'Access));

   Bare_For_Loop_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_80'Access,
         3 => Token_Sequence_57'Access));

   Bare_Loop_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_80'Access,
         3 => Token_Sequence_57'Access));

   Bare_While_Loop_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_80'Access,
         3 => Token_Sequence_57'Access));

   Bare_Begin_Block_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_54'Access));

   Bare_Decl_Block_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_44'Access,
         3 => Token_Sequence_54'Access));

   Bare_Case_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_74'Access));

   Bare_Extended_Return_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_51'Access, Token_Sequence_60'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_If_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser,
         4 => (Token_Sequence_52'Access, Empty_Token_Sequence'Access, True)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_123'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Named_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_17'Access));

   Bare_Select_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_52'Access, Empty_Token_Sequence'Access, True),
         3 => (Token_Sequence_124'Access, Empty_Token_Sequence'Access, True)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Abort_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Assign_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_20'Access));

   Bare_Call_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Delay_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Exit_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_130'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Goto_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Label_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Raise_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_132'Access, Empty_Token_Sequence'Access, False)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Requeue_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Return_Stmt_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Subunit_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_6'Access));

   Bare_Task_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => (Token_Sequence_82'Access, Token_Sequence_132'Access, True),
         2 => Empty_Field_Unparser,
         3 => (Token_Sequence_98'Access, Empty_Token_Sequence'Access, False),
         4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_54'Access));

   Bare_Access_To_Subp_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_36'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Type_Access_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_36'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Array_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_7'Access));

   Bare_Derived_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 7,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser,
         5 => (Token_Sequence_39'Access, Empty_Token_Sequence'Access, True),
         6 => (Token_Sequence_132'Access, Empty_Token_Sequence'Access, False),
         7 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Token_Sequence_82'Access,
         5 => Empty_Token_Sequence'Access, 6 => Empty_Token_Sequence'Access,
         7 => Empty_Token_Sequence'Access));

   Bare_Enum_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Interface_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser,
         2 => (Token_Sequence_39'Access, Empty_Token_Sequence'Access, True)),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_73'Access));

   Bare_Mod_Int_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Private_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Decimal_Fixed_Point_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_50'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Floating_Point_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Ordinary_Fixed_Point_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access));

   Bare_Record_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 4,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser, 4 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access, 4 => Empty_Token_Sequence'Access));

   Bare_Signed_Int_Type_Def_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Anonymous_Type_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Subtype_Indication_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Constrained_Subtype_Indication_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Discrete_Subtype_Indication_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Empty_Token_Sequence'Access));

   Bare_Unconstrained_Array_Index_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Use_Package_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N            => 1, Field_Unparsers => (1 => Empty_Field_Unparser),
      Inter_Tokens => (1 => Empty_Token_Sequence'Access));

   Bare_Use_Type_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_125'Access));

   Bare_Variant_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_27'Access));

   Bare_Variant_Part_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 2,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Token_Sequence_74'Access));

   Bare_With_Clause_Fields_Unparser_List : aliased constant Field_Unparser_List :=
     (N               => 3,
      Field_Unparsers =>
        (1 => Empty_Field_Unparser, 2 => Empty_Field_Unparser,
         3 => Empty_Field_Unparser),
      Inter_Tokens =>
        (1 => Empty_Token_Sequence'Access, 2 => Empty_Token_Sequence'Access,
         3 => Token_Sequence_132'Access));

   Node_Unparsers_Array : aliased constant Node_Unparser_Map :=
     (
Ada_Abort_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Abort_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_133'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Abstract_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Abstract_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_34'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Ada_Node_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Alternatives_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_0),

      Ada_Constraint_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Decl_List => (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Stmt_List => (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Aspect_Assoc_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Base_Assoc_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Assoc_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Case_Expr_Alternative_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Case_Stmt_Alternative_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Compilation_Unit_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Contract_Case_Assoc_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Defining_Name_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Discriminant_Spec_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_16),

      Ada_Elsif_Expr_Part_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Elsif_Stmt_Part_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Enum_Literal_Decl_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Expr_Alternatives_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_0),

      Ada_Discriminant_Choice_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_0),

      Ada_Name_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Parent_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_34),

      Ada_Param_Spec_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_16),

      Ada_Pragma_Node_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Select_When_Part_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_67),

      Ada_Unconstrained_Array_Index_List =>
        (Kind => List, Has_Separator => True, Separator => Token_Unparser_8),

      Ada_Variant_List =>
        (Kind => List, Has_Separator => False, Separator => <>),

      Ada_Aliased_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Aliased_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_37'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_All_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_All_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_38'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Constrained_Array_Indices =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Constrained_Array_Indices_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Unconstrained_Array_Indices =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Unconstrained_Array_Indices_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Aspect_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Aspect_Assoc_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_At_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_65'Access,
         Field_Unparsers => Bare_At_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Attribute_Def_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_65'Access,
         Field_Unparsers =>
           Bare_Attribute_Def_Clause_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Enum_Rep_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_65'Access,
         Field_Unparsers => Bare_Enum_Rep_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Record_Rep_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_65'Access,
         Field_Unparsers => Bare_Record_Rep_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_59'Access),

      Ada_Aspect_Spec =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_132'Access,
         Field_Unparsers => Bare_Aspect_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Contract_Case_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Contract_Case_Assoc_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Pragma_Argument_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Pragma_Argument_Assoc_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Entry_Spec =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_62'Access,
         Field_Unparsers => Bare_Entry_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Enum_Subp_Spec => (Kind => Token),

      Ada_Subp_Spec =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Subp_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Component_List =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Component_List_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Known_Discriminant_Part =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers =>
           Bare_Known_Discriminant_Part_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_6'Access),

      Ada_Unknown_Discriminant_Part =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_4'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Entry_Completion_Formal_Params =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Entry_Completion_Formal_Params_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Generic_Formal_Part =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_67'Access,
         Field_Unparsers =>
           Bare_Generic_Formal_Part_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Null_Record_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_88'Access,
         Field_Unparsers => Bare_Null_Record_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Record_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_106'Access,
         Field_Unparsers => Bare_Record_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_58'Access),

      Ada_Aggregate_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Aggregate_Assoc_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Multi_Dim_Array_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Multi_Dim_Array_Assoc_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Discriminant_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Discriminant_Assoc_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Param_Assoc =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Param_Assoc_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Component_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Component_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Discriminant_Spec =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Discriminant_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Generic_Formal_Obj_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Generic_Formal_Obj_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Generic_Formal_Package =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_132'Access,
         Field_Unparsers =>
           Bare_Generic_Formal_Package_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Generic_Formal_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Generic_Formal_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Generic_Formal_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Generic_Formal_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Param_Spec =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Param_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Generic_Package_Internal =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_95'Access,
         Field_Unparsers =>
           Bare_Generic_Package_Internal_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Package_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_95'Access,
         Field_Unparsers => Bare_Package_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Discrete_Base_Subtype_Decl => (Kind => Token),

      Ada_Subtype_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_115'Access,
         Field_Unparsers => Bare_Subtype_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Classwide_Type_Decl => (Kind => Token),

      Ada_Incomplete_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_125'Access,
         Field_Unparsers =>
           Bare_Incomplete_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Incomplete_Tagged_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_125'Access,
         Field_Unparsers =>
           Bare_Incomplete_Tagged_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_118'Access),

      Ada_Protected_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_102'Access,
         Field_Unparsers =>
           Bare_Protected_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Task_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_121'Access,
         Field_Unparsers => Bare_Task_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Single_Task_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Single_Task_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_125'Access,
         Field_Unparsers => Bare_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Anonymous_Type_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Anonymous_Type_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Synth_Anonymous_Type_Decl => (Kind => Token),

      Ada_Abstract_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Abstract_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Abstract_Formal_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_132'Access,
         Field_Unparsers =>
           Bare_Abstract_Formal_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Concrete_Formal_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_132'Access,
         Field_Unparsers =>
           Bare_Concrete_Formal_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Entry_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Entry_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Enum_Literal_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Enum_Literal_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Generic_Subp_Internal =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Generic_Subp_Internal_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Expr_Function =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Expr_Function_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Null_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Null_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Subp_Body =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Subp_Body_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Subp_Renaming_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Subp_Renaming_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Package_Body_Stub =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_96'Access,
         Field_Unparsers => Bare_Package_Body_Stub_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Protected_Body_Stub =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_101'Access,
         Field_Unparsers =>
           Bare_Protected_Body_Stub_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Subp_Body_Stub =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Subp_Body_Stub_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Task_Body_Stub =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_120'Access,
         Field_Unparsers => Bare_Task_Body_Stub_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Entry_Body =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_62'Access,
         Field_Unparsers => Bare_Entry_Body_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Package_Body =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_96'Access,
         Field_Unparsers => Bare_Package_Body_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Protected_Body =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_101'Access,
         Field_Unparsers => Bare_Protected_Body_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Task_Body =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_120'Access,
         Field_Unparsers => Bare_Task_Body_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Entry_Index_Spec =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_5'Access,
         Field_Unparsers => Bare_Entry_Index_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Error_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Exception_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Exception_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Exception_Handler =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_130'Access,
         Field_Unparsers => Bare_Exception_Handler_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_For_Loop_Var_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_For_Loop_Var_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Generic_Package_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Generic_Package_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Generic_Subp_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Generic_Subp_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Generic_Package_Instantiation =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_95'Access,
         Field_Unparsers =>
           Bare_Generic_Package_Instantiation_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Generic_Subp_Instantiation =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Generic_Subp_Instantiation_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Generic_Package_Renaming_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_68'Access,
         Field_Unparsers =>
           Bare_Generic_Package_Renaming_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Generic_Subp_Renaming_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_67'Access,
         Field_Unparsers =>
           Bare_Generic_Subp_Renaming_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Label_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Label_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Named_Stmt_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Named_Stmt_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Number_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Number_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Object_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Object_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Anonymous_Object_Decl => (Kind => Token),

      Ada_Extended_Return_Stmt_Object_Decl =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Extended_Return_Stmt_Object_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Package_Renaming_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_95'Access,
         Field_Unparsers =>
           Bare_Package_Renaming_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Single_Protected_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_100'Access,
         Field_Unparsers =>
           Bare_Single_Protected_Decl_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Single_Task_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_119'Access,
         Field_Unparsers => Bare_Single_Task_Decl_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Case_Stmt_Alternative =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_130'Access,
         Field_Unparsers =>
           Bare_Case_Stmt_Alternative_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Compilation_Unit =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Compilation_Unit_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Component_Clause =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Component_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Component_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Component_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Constant_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Constant_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_46'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Delta_Constraint =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_49'Access,
         Field_Unparsers => Bare_Delta_Constraint_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Digits_Constraint =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_50'Access,
         Field_Unparsers => Bare_Digits_Constraint_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Discriminant_Constraint =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers =>
           Bare_Discriminant_Constraint_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_6'Access),

      Ada_Index_Constraint =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers => Bare_Index_Constraint_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Range_Constraint =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Range_Constraint_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Declarative_Part =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Declarative_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Private_Part =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Private_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Public_Part =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Public_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Elsif_Expr_Part =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_53'Access,
         Field_Unparsers => Bare_Elsif_Expr_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Elsif_Stmt_Part =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_53'Access,
         Field_Unparsers => Bare_Elsif_Stmt_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Allocator =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_82'Access,
         Field_Unparsers => Bare_Allocator_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Aggregate =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers => Bare_Aggregate_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Null_Record_Aggregate =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers =>
           Bare_Null_Record_Aggregate_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_6'Access),

      Ada_Bin_Op =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Bin_Op_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Relation_Op =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Relation_Op_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Box_Expr =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_25'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Case_Expr =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_45'Access,
         Field_Unparsers => Bare_Case_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Case_Expr_Alternative =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_130'Access,
         Field_Unparsers =>
           Bare_Case_Expr_Alternative_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Contract_Cases =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers => Bare_Contract_Cases_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_If_Expr =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_70'Access,
         Field_Unparsers => Bare_If_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Membership_Expr =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Membership_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Attribute_Ref =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Attribute_Ref_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Update_Attribute_Ref =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Update_Attribute_Ref_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Call_Expr =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Call_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Defining_Name =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Defining_Name_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Discrete_Subtype_Name =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Discrete_Subtype_Name_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Dotted_Name =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Dotted_Name_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_End_Name =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_End_Name_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Explicit_Deref =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Explicit_Deref_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_13'Access),

      Ada_Qual_Expr =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Qual_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Char_Literal => (Kind => Token),
Ada_Identifier         => (Kind => Token),

      Ada_Op_Abs =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_33'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_And =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_39'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_And_Then =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_40'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Concat =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_1'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Div =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_15'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Double_Dot =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_14'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Eq =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_26'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Gt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_28'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Gte =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_29'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_In =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_71'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Lt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_22'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Lte =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_24'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Minus =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_11'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Mod =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_81'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Mult =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_8'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Neq =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_16'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Not =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_83'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Not_In =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_84'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Or =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_90'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Or_Else =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_91'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Plus =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_10'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Pow =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_9'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Rem =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_107'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Op_Xor =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_135'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_String_Literal => (Kind => Token),

      Ada_Null_Literal => (Kind => Token),
Ada_Int_Literal        => (Kind => Token),

      Ada_Real_Literal => (Kind => Token),

      Ada_Target_Name =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_31'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Paren_Expr =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers => Bare_Paren_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Quantified_Expr =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_65'Access,
         Field_Unparsers => Bare_Quantified_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Raise_Expr =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_103'Access,
         Field_Unparsers => Bare_Raise_Expr_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Un_Op =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Un_Op_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Handled_Stmts =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Handled_Stmts_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Interface_Kind_Limited =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_79'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Interface_Kind_Protected =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_100'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Interface_Kind_Synchronized =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_116'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Interface_Kind_Task =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_119'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Iter_Type_In =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_71'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Iter_Type_Of =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_89'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Library_Item =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Library_Item_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Limited_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Limited_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_79'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_For_Loop_Spec =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_For_Loop_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_While_Loop_Spec =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_131'Access,
         Field_Unparsers => Bare_While_Loop_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Mode_Default =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Mode_In =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_71'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Mode_In_Out =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_72'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Mode_Out =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_93'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Not_Null_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Not_Null_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_85'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Null_Component_Decl =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_87'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Others_Designator =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_92'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Overriding_Not_Overriding =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_86'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Overriding_Overriding =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_94'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Overriding_Unspecified =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Params =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers => Bare_Params_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Pragma_Node =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_97'Access,
         Field_Unparsers => Bare_Pragma_Node_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Prim_Type_Accessor => (Kind => Token),

      Ada_Private_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Private_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_98'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Protected_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Protected_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Protected_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Protected_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_100'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Quantifier_All =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_38'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Quantifier_Some =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_114'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Range_Spec =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_104'Access,
         Field_Unparsers => Bare_Range_Spec_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Renaming_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_108'Access,
         Field_Unparsers => Bare_Renaming_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Synthetic_Renaming_Clause => (Kind => Token),

      Ada_Reverse_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Reverse_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_111'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Select_When_Part =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Select_When_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Accept_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_35'Access,
         Field_Unparsers => Bare_Accept_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Accept_Stmt_With_Stmts =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_35'Access,
         Field_Unparsers =>
           Bare_Accept_Stmt_With_Stmts_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_For_Loop_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_65'Access,
         Field_Unparsers => Bare_For_Loop_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Loop_Stmt =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Loop_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_While_Loop_Stmt =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_While_Loop_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Begin_Block =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_44'Access,
         Field_Unparsers => Bare_Begin_Block_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Decl_Block =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_47'Access,
         Field_Unparsers => Bare_Decl_Block_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Case_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_45'Access,
         Field_Unparsers => Bare_Case_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_55'Access),

      Ada_Extended_Return_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_110'Access,
         Field_Unparsers =>
           Bare_Extended_Return_Stmt_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_If_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_70'Access,
         Field_Unparsers => Bare_If_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_56'Access),

      Ada_Named_Stmt =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Named_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Select_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_112'Access,
         Field_Unparsers => Bare_Select_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_61'Access),

      Ada_Error_Stmt =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Abort_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_32'Access,
         Field_Unparsers => Bare_Abort_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Assign_Stmt =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Assign_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Call_Stmt =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Call_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Delay_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_48'Access,
         Field_Unparsers => Bare_Delay_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Exit_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_64'Access,
         Field_Unparsers => Bare_Exit_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Goto_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_69'Access,
         Field_Unparsers => Bare_Goto_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Label =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_23'Access,
         Field_Unparsers => Bare_Label_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_30'Access),

      Ada_Null_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_87'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Raise_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_103'Access,
         Field_Unparsers => Bare_Raise_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Requeue_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_109'Access,
         Field_Unparsers => Bare_Requeue_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Return_Stmt =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_110'Access,
         Field_Unparsers => Bare_Return_Stmt_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Terminate_Alternative =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_122'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Subp_Kind_Function =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_66'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Subp_Kind_Procedure =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_99'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Subunit =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_113'Access,
         Field_Unparsers => Bare_Subunit_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Synchronized_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Synchronized_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_116'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Tagged_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Tagged_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_117'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Task_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_74'Access,
         Field_Unparsers => Bare_Task_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Access_To_Subp_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Access_To_Subp_Def_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Anonymous_Type_Access_Def => (Kind => Token),

      Ada_Type_Access_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Type_Access_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Array_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_41'Access,
         Field_Unparsers => Bare_Array_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Derived_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Derived_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Enum_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_3'Access,
         Field_Unparsers => Bare_Enum_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_6'Access),

      Ada_Formal_Discrete_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_4'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Interface_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Interface_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Mod_Int_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_81'Access,
         Field_Unparsers => Bare_Mod_Int_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Private_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Private_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_98'Access),

      Ada_Decimal_Fixed_Point_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_49'Access,
         Field_Unparsers =>
           Bare_Decimal_Fixed_Point_Def_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Floating_Point_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_50'Access,
         Field_Unparsers =>
           Bare_Floating_Point_Def_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Ordinary_Fixed_Point_Def =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_49'Access,
         Field_Unparsers =>
           Bare_Ordinary_Fixed_Point_Def_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Record_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Record_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Signed_Int_Type_Def =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Signed_Int_Type_Def_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Anonymous_Type =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_Anonymous_Type_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Enum_Lit_Synth_Type_Expr => (Kind => Token),

      Ada_Subtype_Indication =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Subtype_Indication_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Constrained_Subtype_Indication =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Constrained_Subtype_Indication_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Discrete_Subtype_Indication =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Discrete_Subtype_Indication_Fields_Unparser_List'Access,
         Post_Tokens => Empty_Token_Sequence'Access),

      Ada_Unconstrained_Array_Index =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers =>
           Bare_Unconstrained_Array_Index_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_105'Access),

      Ada_Until_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Until_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_126'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Use_Package_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_127'Access,
         Field_Unparsers =>
           Bare_Use_Package_Clause_Fields_Unparser_List'Access,
         Post_Tokens => Token_Sequence_21'Access),

      Ada_Use_Type_Clause =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_127'Access,
         Field_Unparsers => Bare_Use_Type_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_Variant =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_130'Access,
         Field_Unparsers => Bare_Variant_Fields_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_Variant_Part =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_45'Access,
         Field_Unparsers => Bare_Variant_Part_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_55'Access),

      Ada_With_Clause =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Bare_With_Clause_Fields_Unparser_List'Access,
         Post_Tokens     => Token_Sequence_21'Access),

      Ada_With_Private_Absent =>
        (Kind            => Regular, Pre_Tokens => Empty_Token_Sequence'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access),

      Ada_With_Private_Present =>
        (Kind            => Regular, Pre_Tokens => Token_Sequence_134'Access,
         Field_Unparsers => Empty_Field_Unparser_List'Access,
         Post_Tokens     => Empty_Token_Sequence'Access));

begin
   Node_Unparsers := Node_Unparsers_Array'Access;
end Libadalang.Unparsing_Implementation;
