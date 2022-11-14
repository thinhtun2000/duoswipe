------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--          Copyright (C) 2001-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GPR.Sdefault;

package body GPR.Version is

   ----------------------
   -- Copyright_Holder --
   ----------------------

   function Copyright_Holder return String is
   begin
      return "AdaCore";
   end Copyright_Holder;

   -------------------
   -- Free_Software --
   -------------------

   function Free_Software return String is
   begin
      case Build_Type is
         when GPL | FSF =>
            return
              "This is free software; see the source for copying conditions." &
              ASCII.LF &
              "There is NO warranty; not even for MERCHANTABILITY or FITNESS" &
              " FOR A PARTICULAR PURPOSE.";

         when Gnatpro =>
            return
              "This is free software; see the source for copying conditions." &
               ASCII.LF &
               "See your AdaCore support agreement for details of warranty" &
               " and support." &
               ASCII.LF &
               "If you do not have a current support agreement, then there" &
               " is absolutely" &
               ASCII.LF &
               "no warranty; not even for MERCHANTABILITY or FITNESS FOR" &
               " A PARTICULAR" &
               ASCII.LF &
               "PURPOSE.";
      end case;
   end Free_Software;

   ------------------------
   -- Gpr_Version_String --
   ------------------------

   function Gpr_Version_String (Host : Boolean := True) return String is
      Hostname       : constant String := " (" & GPR.Sdefault.Hostname & ')';
      Version_String : constant String :=
                         Gpr_Version & " (" & Date & ")"
                         & (if Host then Hostname else "");

   begin
      case Build_Type is
         when Gnatpro =>
            return "Pro " & Version_String;
         when GPL =>
            return "Community " & Version_String;
         when FSF =>
            return Version_String;
      end case;

   end Gpr_Version_String;

end GPR.Version;
