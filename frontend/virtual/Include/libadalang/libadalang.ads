with Langkit_Support;
with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

package Libadalang is

   Version      : constant String := "2020 (20200818)";
   Current_Year : constant String := "2020";

   --  Libadalang's main entry point is the Libadalang.Analysis package.

   --  Convenience renaming for support package that Langkit provides

   package Support renames Langkit_Support;
   package Diagnostics renames Support.Diagnostics;
   package Slocs renames Support.Slocs;
   package Text renames Support.Text;

end Libadalang;
