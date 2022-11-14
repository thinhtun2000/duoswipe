with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Libadalang.Common; use Libadalang.Common;
use Libadalang.Common.Token_Data_Handlers;

--  This package provides types and primitives to split text streams into lists
--  of tokens.

package Libadalang.Lexer is

   use Support.Diagnostics, Support.Text;

   type Lexer_Input (Kind : Lexer_Input_Kind) is record
      case Kind is
         when File | Bytes_Buffer =>
            Charset : Unbounded_String;
            --  Name of the charset to use in order to decode the input source

            Read_BOM : Boolean;
            --  Whether the lexer should look for an optional Byte Order Mark

            case Kind is
               when File =>
                  Filename : GNATCOLL.VFS.Virtual_File;
                  --  Name of the file to read

               when Bytes_Buffer =>
                  Bytes : Unbounded_String;
                  --  Source buffer to read

               when others =>
                  null;
            end case;

         when Text_Buffer =>
            Text : Unbounded_Text_Type;
            --  Source buffer to read
      end case;
   end record;
   --  Input from which the lexer will read tokens

   procedure Extract_Tokens
     (Input :    Lexer_Input; Tab_Stop : Positive := 8; With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector) with
      Pre  => Initialized (TDH) and then not Has_Source_Buffer (TDH),
      Post => Has_Source_Buffer (TDH);
      --  Extract tokens out of the given ``Input`` and store them into
      --  ``TDH``.
      --
      --  ``Tab_Stop`` is a positive number to describe the effect of
      --  tabulation characters on the column number in source files.
      --
      --  Raise a ``Name_Error`` exception if this involves reading a file
      --  that can not be open. Raise an ``Unknown_Charset`` exception if the
      --  requested charset is unknown. Raise an ``Invalid_Input`` exception
      --  if the source cannot be decoded using the given ``Charset``.

   function Is_Keyword
     (TDH     : Token_Data_Handler; Index : Token_Or_Trivia_Index;
      Version : Language_Version) return Boolean;
--  Given an Ada language version, return whether a token is an Ada keyword.
--
--  Due to the way Libadalang works, every token added after Ada 83 is lexed
--  as a regular identifier, and then treated specially by the parser in some
--  circumstances (being akin to the notion of reserved word).
--
--  This function returns True for regular lexer keywords, as well as for those
--  identifiers.

end Libadalang.Lexer;
