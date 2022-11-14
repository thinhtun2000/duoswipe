package body Libadalang.Introspection_Implementation is

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Internal_Value) return Boolean is
   begin
      return Self.Boolean_Value;
   end As_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Internal_Value) return Integer is
   begin
      return Self.Integer_Value;
   end As_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Internal_Value) return Character_Type is
   begin
      return Self.Character_Value;
   end As_Character;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Internal_Value) return Internal_Entity is
   begin
      return Self.Node_Value;
   end As_Node;

   function As_Analysis_Unit_Kind
     (Self : Internal_Value) return Analysis_Unit_Kind
   is
   begin
      return Self.Analysis_Unit_Kind_Value;
   end As_Analysis_Unit_Kind;

   function As_Lookup_Kind (Self : Internal_Value) return Lookup_Kind is
   begin
      return Self.Lookup_Kind_Value;
   end As_Lookup_Kind;

   function As_Find_All_Mode (Self : Internal_Value) return Find_All_Mode is
   begin
      return Self.Find_All_Mode_Value;
   end As_Find_All_Mode;

   function As_Ref_Result_Kind (Self : Internal_Value) return Ref_Result_Kind
   is
   begin
      return Self.Ref_Result_Kind_Value;
   end As_Ref_Result_Kind;

   function As_Grammar_Rule (Self : Internal_Value) return Grammar_Rule is
   begin
      return Self.Grammar_Rule_Value;
   end As_Grammar_Rule;

   --  Now we can emit descriptor tables

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return String is
   begin
      return To_String (Node_Type_Descriptors (Id).DSL_Name);
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id is
      use Node_Type_Id_Maps;

      Position : constant Cursor :=
        DSL_Name_To_Node_Type.Find (To_Unbounded_String (Name));
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return None;
      end if;
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Node_Type_Descriptors (Id).Is_Abstract;
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type is
      Desc : Node_Type_Descriptor renames Node_Type_Descriptors (Id).all;
   begin
      if Desc.Is_Abstract then
         raise Constraint_Error with "trying to get kind for abstract node";
      end if;
      return Desc.Kind;
   end Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : Ada_Node_Kind_Type) return Node_Type_Id is
   begin
      return Kind_To_Id (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Id = Common.Ada_Node_Type_Id;
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      if Is_Root_Node (Id) then
         raise Constraint_Error with "trying to get base type of root node";
      end if;
      return Node_Type_Descriptors (Id).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Node_Type_Descriptors (Id).Derivations;
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
      Cursor : Any_Node_Type_Id := Id;
   begin
      while Cursor /= None loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Node_Type_Descriptors (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

   --------------------
   -- Node_Data_Name --
   --------------------

   function Node_Data_Name (Node_Data : Node_Data_Reference) return String is
   begin
      case Node_Data is
         when Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Field_Name (Node_Data);
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Name (Node_Data);
      end case;
   end Node_Data_Name;

   --------------------
   -- Node_Data_Type --
   --------------------

   function Node_Data_Type
     (Node_Data : Node_Data_Reference) return Value_Constraint
   is
   begin
      case Node_Data is
         when Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return (Kind => Node_Value, Node_Type => Field_Type (Node_Data));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Return_Type (Node_Data);
      end case;
   end Node_Data_Type;

   ----------------------
   -- Lookup_Node_Data --
   ----------------------

   function Lookup_Node_Data
     (Id : Node_Type_Id; Name : String) return Any_Node_Data_Reference
   is
      Cursor : Any_Node_Type_Id := Id;
   begin
      --  Go through the derivation chain for Id and look for any field or
      --  property whose name matches Name.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
              Node_Type_Descriptors (Cursor).all;
         begin
            for F of Node_Desc.Fields loop
               pragma Warnings (Off, "value not in range of type");
               if Field_Name (F.Field) = Name then
                  return F.Field;
               end if;
               pragma Warnings (On, "value not in range of type");
            end loop;

            for P of Node_Desc.Properties loop
               if Property_Name (P) = Name then
                  return P;
               end if;
            end loop;

            Cursor := Node_Desc.Base_Type;
         end;
      end loop;
      return None;
   end Lookup_Node_Data;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Field : Field_Reference) return String is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Syntax_Field_Descriptors (Field).Name;
      pragma Warnings (On, "value not in range of subtype");
   end Field_Name;

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type (Field : Field_Reference) return Node_Type_Id is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Syntax_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Field_Type;

   ----------------
   -- Eval_Field --
   ----------------

   function Eval_Field
     (Node : Bare_Ada_Node; Field : Field_Reference) return Bare_Ada_Node
   is
      Kind : constant Ada_Node_Kind_Type := Node.Kind;
   begin

      case Ada_Ada_Node (Kind) is
         when Ada_Basic_Decl =>
            declare
               N_Bare_Basic_Decl : constant Bare_Basic_Decl := Node;
            begin
               case Field is
                  when Basic_Decl_F_Aspects =>
                     return Basic_Decl_F_Aspects (N_Bare_Basic_Decl);
                  when others =>
                     null;
               end case;
               case Ada_Basic_Decl (Kind) is
                  when Ada_Protected_Body_Stub_Range =>
                     declare
                        N_Bare_Protected_Body_Stub : constant Bare_Protected_Body_Stub :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Protected_Body_Stub_F_Name =>
                              return Protected_Body_Stub_F_Name
                                  (N_Bare_Protected_Body_Stub);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Subp_Body_Stub_Range =>
                     declare
                        N_Bare_Subp_Body_Stub : constant Bare_Subp_Body_Stub :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Subp_Body_Stub_F_Overriding =>
                              return Subp_Body_Stub_F_Overriding
                                  (N_Bare_Subp_Body_Stub);
                           when Subp_Body_Stub_F_Subp_Spec =>
                              return Subp_Body_Stub_F_Subp_Spec
                                  (N_Bare_Subp_Body_Stub);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Package_Body_Stub_Range =>
                     declare
                        N_Bare_Package_Body_Stub : constant Bare_Package_Body_Stub :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Package_Body_Stub_F_Name =>
                              return Package_Body_Stub_F_Name
                                  (N_Bare_Package_Body_Stub);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Task_Body_Stub_Range =>
                     declare
                        N_Bare_Task_Body_Stub : constant Bare_Task_Body_Stub :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Task_Body_Stub_F_Name =>
                              return Task_Body_Stub_F_Name
                                  (N_Bare_Task_Body_Stub);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Base_Subp_Body =>
                     declare
                        N_Bare_Base_Subp_Body : constant Bare_Base_Subp_Body :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Base_Subp_Body_F_Overriding =>
                              return Base_Subp_Body_F_Overriding
                                  (N_Bare_Base_Subp_Body);
                           when Base_Subp_Body_F_Subp_Spec =>
                              return Base_Subp_Body_F_Subp_Spec
                                  (N_Bare_Base_Subp_Body);
                           when others =>
                              null;
                        end case;
                        case Ada_Base_Subp_Body (Kind) is
                           when Ada_Expr_Function_Range =>
                              declare
                                 N_Bare_Expr_Function : constant Bare_Expr_Function :=
                                   N_Bare_Base_Subp_Body;
                              begin
                                 case Field is
                                    when Expr_Function_F_Expr =>
                                       return Expr_Function_F_Expr
                                           (N_Bare_Expr_Function);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Subp_Renaming_Decl_Range =>
                              declare
                                 N_Bare_Subp_Renaming_Decl : constant Bare_Subp_Renaming_Decl :=
                                   N_Bare_Base_Subp_Body;
                              begin
                                 case Field is
                                    when Subp_Renaming_Decl_F_Renames =>
                                       return Subp_Renaming_Decl_F_Renames
                                           (N_Bare_Subp_Renaming_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Subp_Body_Range =>
                              declare
                                 N_Bare_Subp_Body : constant Bare_Subp_Body :=
                                   N_Bare_Base_Subp_Body;
                              begin
                                 case Field is
                                    when Subp_Body_F_Decls =>
                                       return Subp_Body_F_Decls
                                           (N_Bare_Subp_Body);
                                    when Subp_Body_F_Stmts =>
                                       return Subp_Body_F_Stmts
                                           (N_Bare_Subp_Body);
                                    when Subp_Body_F_End_Name =>
                                       return Subp_Body_F_End_Name
                                           (N_Bare_Subp_Body);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Package_Body_Range =>
                     declare
                        N_Bare_Package_Body : constant Bare_Package_Body :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Package_Body_F_Package_Name =>
                              return Package_Body_F_Package_Name
                                  (N_Bare_Package_Body);
                           when Package_Body_F_Decls =>
                              return Package_Body_F_Decls
                                  (N_Bare_Package_Body);
                           when Package_Body_F_Stmts =>
                              return Package_Body_F_Stmts
                                  (N_Bare_Package_Body);
                           when Package_Body_F_End_Name =>
                              return Package_Body_F_End_Name
                                  (N_Bare_Package_Body);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Task_Body_Range =>
                     declare
                        N_Bare_Task_Body : constant Bare_Task_Body :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Task_Body_F_Name =>
                              return Task_Body_F_Name (N_Bare_Task_Body);
                           when Task_Body_F_Decls =>
                              return Task_Body_F_Decls (N_Bare_Task_Body);
                           when Task_Body_F_Stmts =>
                              return Task_Body_F_Stmts (N_Bare_Task_Body);
                           when Task_Body_F_End_Name =>
                              return Task_Body_F_End_Name (N_Bare_Task_Body);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Protected_Body_Range =>
                     declare
                        N_Bare_Protected_Body : constant Bare_Protected_Body :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Protected_Body_F_Name =>
                              return Protected_Body_F_Name
                                  (N_Bare_Protected_Body);
                           when Protected_Body_F_Decls =>
                              return Protected_Body_F_Decls
                                  (N_Bare_Protected_Body);
                           when Protected_Body_F_End_Name =>
                              return Protected_Body_F_End_Name
                                  (N_Bare_Protected_Body);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Entry_Body_Range =>
                     declare
                        N_Bare_Entry_Body : constant Bare_Entry_Body :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Entry_Body_F_Entry_Name =>
                              return Entry_Body_F_Entry_Name
                                  (N_Bare_Entry_Body);
                           when Entry_Body_F_Index_Spec =>
                              return Entry_Body_F_Index_Spec
                                  (N_Bare_Entry_Body);
                           when Entry_Body_F_Params =>
                              return Entry_Body_F_Params (N_Bare_Entry_Body);
                           when Entry_Body_F_Barrier =>
                              return Entry_Body_F_Barrier (N_Bare_Entry_Body);
                           when Entry_Body_F_Decls =>
                              return Entry_Body_F_Decls (N_Bare_Entry_Body);
                           when Entry_Body_F_Stmts =>
                              return Entry_Body_F_Stmts (N_Bare_Entry_Body);
                           when Entry_Body_F_End_Name =>
                              return Entry_Body_F_End_Name (N_Bare_Entry_Body);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Discriminant_Spec_Range =>
                     declare
                        N_Bare_Discriminant_Spec : constant Bare_Discriminant_Spec :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Discriminant_Spec_F_Ids =>
                              return Discriminant_Spec_F_Ids
                                  (N_Bare_Discriminant_Spec);
                           when Discriminant_Spec_F_Type_Expr =>
                              return Discriminant_Spec_F_Type_Expr
                                  (N_Bare_Discriminant_Spec);
                           when Discriminant_Spec_F_Default_Expr =>
                              return Discriminant_Spec_F_Default_Expr
                                  (N_Bare_Discriminant_Spec);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Component_Decl_Range =>
                     declare
                        N_Bare_Component_Decl : constant Bare_Component_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Component_Decl_F_Ids =>
                              return Component_Decl_F_Ids
                                  (N_Bare_Component_Decl);
                           when Component_Decl_F_Component_Def =>
                              return Component_Decl_F_Component_Def
                                  (N_Bare_Component_Decl);
                           when Component_Decl_F_Default_Expr =>
                              return Component_Decl_F_Default_Expr
                                  (N_Bare_Component_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Param_Spec_Range =>
                     declare
                        N_Bare_Param_Spec : constant Bare_Param_Spec :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Param_Spec_F_Ids =>
                              return Param_Spec_F_Ids (N_Bare_Param_Spec);
                           when Param_Spec_F_Has_Aliased =>
                              return Param_Spec_F_Has_Aliased
                                  (N_Bare_Param_Spec);
                           when Param_Spec_F_Mode =>
                              return Param_Spec_F_Mode (N_Bare_Param_Spec);
                           when Param_Spec_F_Type_Expr =>
                              return Param_Spec_F_Type_Expr
                                  (N_Bare_Param_Spec);
                           when Param_Spec_F_Default_Expr =>
                              return Param_Spec_F_Default_Expr
                                  (N_Bare_Param_Spec);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Formal =>
                     declare
                        N_Bare_Generic_Formal : constant Bare_Generic_Formal :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Formal_F_Decl =>
                              return Generic_Formal_F_Decl
                                  (N_Bare_Generic_Formal);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Base_Type_Decl =>
                     declare
                        N_Bare_Base_Type_Decl : constant Bare_Base_Type_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Base_Type_Decl_F_Name =>
                              return Base_Type_Decl_F_Name
                                  (N_Bare_Base_Type_Decl);
                           when others =>
                              null;
                        end case;
                        case Ada_Base_Type_Decl (Kind) is
                           when Ada_Type_Decl_Range =>
                              declare
                                 N_Bare_Type_Decl : constant Bare_Type_Decl :=
                                   N_Bare_Base_Type_Decl;
                              begin
                                 case Field is
                                    when Type_Decl_F_Discriminants =>
                                       return Type_Decl_F_Discriminants
                                           (N_Bare_Type_Decl);
                                    when Type_Decl_F_Type_Def =>
                                       return Type_Decl_F_Type_Def
                                           (N_Bare_Type_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Subtype_Decl_Range =>
                              declare
                                 N_Bare_Subtype_Decl : constant Bare_Subtype_Decl :=
                                   N_Bare_Base_Type_Decl;
                              begin
                                 case Field is
                                    when Subtype_Decl_F_Subtype =>
                                       return Subtype_Decl_F_Subtype
                                           (N_Bare_Subtype_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Task_Type_Decl_Range =>
                              declare
                                 N_Bare_Task_Type_Decl : constant Bare_Task_Type_Decl :=
                                   N_Bare_Base_Type_Decl;
                              begin
                                 case Field is
                                    when Task_Type_Decl_F_Discriminants =>
                                       return Task_Type_Decl_F_Discriminants
                                           (N_Bare_Task_Type_Decl);
                                    when Task_Type_Decl_F_Definition =>
                                       return Task_Type_Decl_F_Definition
                                           (N_Bare_Task_Type_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Protected_Type_Decl_Range =>
                              declare
                                 N_Bare_Protected_Type_Decl : constant Bare_Protected_Type_Decl :=
                                   N_Bare_Base_Type_Decl;
                              begin
                                 case Field is
                                    when Protected_Type_Decl_F_Discriminants =>
                                       return Protected_Type_Decl_F_Discriminants
                                           (N_Bare_Protected_Type_Decl);
                                    when Protected_Type_Decl_F_Interfaces =>
                                       return Protected_Type_Decl_F_Interfaces
                                           (N_Bare_Protected_Type_Decl);
                                    when Protected_Type_Decl_F_Definition =>
                                       return Protected_Type_Decl_F_Definition
                                           (N_Bare_Protected_Type_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Incomplete_Type_Decl_Range =>
                              declare
                                 N_Bare_Incomplete_Type_Decl : constant Bare_Incomplete_Type_Decl :=
                                   N_Bare_Base_Type_Decl;
                              begin
                                 case Field is
                                    when Incomplete_Type_Decl_F_Discriminants =>
                                       return Incomplete_Type_Decl_F_Discriminants
                                           (N_Bare_Incomplete_Type_Decl);
                                    when others =>
                                       null;
                                 end case;
                                 case Ada_Incomplete_Type_Decl_Range (Kind) is
                                    when Ada_Incomplete_Tagged_Type_Decl_Range =>
                                       declare
                                          N_Bare_Incomplete_Tagged_Type_Decl : constant Bare_Incomplete_Tagged_Type_Decl :=
                                            N_Bare_Incomplete_Type_Decl;
                                       begin
                                          case Field is
                                             when Incomplete_Tagged_Type_Decl_F_Has_Abstract =>
                                                return Incomplete_Tagged_Type_Decl_F_Has_Abstract
                                                    (N_Bare_Incomplete_Tagged_Type_Decl);
                                             when others =>
                                                null;
                                          end case;
                                       end;
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Classic_Subp_Decl =>
                     declare
                        N_Bare_Classic_Subp_Decl : constant Bare_Classic_Subp_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Classic_Subp_Decl_F_Overriding =>
                              return Classic_Subp_Decl_F_Overriding
                                  (N_Bare_Classic_Subp_Decl);
                           when Classic_Subp_Decl_F_Subp_Spec =>
                              return Classic_Subp_Decl_F_Subp_Spec
                                  (N_Bare_Classic_Subp_Decl);
                           when others =>
                              null;
                        end case;
                        case Ada_Classic_Subp_Decl (Kind) is
                           when Ada_Formal_Subp_Decl =>
                              declare
                                 N_Bare_Formal_Subp_Decl : constant Bare_Formal_Subp_Decl :=
                                   N_Bare_Classic_Subp_Decl;
                              begin
                                 case Field is
                                    when Formal_Subp_Decl_F_Default_Expr =>
                                       return Formal_Subp_Decl_F_Default_Expr
                                           (N_Bare_Formal_Subp_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Subp_Internal_Range =>
                     declare
                        N_Bare_Generic_Subp_Internal : constant Bare_Generic_Subp_Internal :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Subp_Internal_F_Subp_Spec =>
                              return Generic_Subp_Internal_F_Subp_Spec
                                  (N_Bare_Generic_Subp_Internal);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Enum_Literal_Decl_Range =>
                     declare
                        N_Bare_Enum_Literal_Decl : constant Bare_Enum_Literal_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Enum_Literal_Decl_F_Name =>
                              return Enum_Literal_Decl_F_Name
                                  (N_Bare_Enum_Literal_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Entry_Decl_Range =>
                     declare
                        N_Bare_Entry_Decl : constant Bare_Entry_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Entry_Decl_F_Overriding =>
                              return Entry_Decl_F_Overriding
                                  (N_Bare_Entry_Decl);
                           when Entry_Decl_F_Spec =>
                              return Entry_Decl_F_Spec (N_Bare_Entry_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Single_Task_Decl_Range =>
                     declare
                        N_Bare_Single_Task_Decl : constant Bare_Single_Task_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Single_Task_Decl_F_Task_Type =>
                              return Single_Task_Decl_F_Task_Type
                                  (N_Bare_Single_Task_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Single_Protected_Decl_Range =>
                     declare
                        N_Bare_Single_Protected_Decl : constant Bare_Single_Protected_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Single_Protected_Decl_F_Name =>
                              return Single_Protected_Decl_F_Name
                                  (N_Bare_Single_Protected_Decl);
                           when Single_Protected_Decl_F_Interfaces =>
                              return Single_Protected_Decl_F_Interfaces
                                  (N_Bare_Single_Protected_Decl);
                           when Single_Protected_Decl_F_Definition =>
                              return Single_Protected_Decl_F_Definition
                                  (N_Bare_Single_Protected_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Number_Decl_Range =>
                     declare
                        N_Bare_Number_Decl : constant Bare_Number_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Number_Decl_F_Ids =>
                              return Number_Decl_F_Ids (N_Bare_Number_Decl);
                           when Number_Decl_F_Expr =>
                              return Number_Decl_F_Expr (N_Bare_Number_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Object_Decl_Range =>
                     declare
                        N_Bare_Object_Decl : constant Bare_Object_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Object_Decl_F_Ids =>
                              return Object_Decl_F_Ids (N_Bare_Object_Decl);
                           when Object_Decl_F_Has_Aliased =>
                              return Object_Decl_F_Has_Aliased
                                  (N_Bare_Object_Decl);
                           when Object_Decl_F_Has_Constant =>
                              return Object_Decl_F_Has_Constant
                                  (N_Bare_Object_Decl);
                           when Object_Decl_F_Mode =>
                              return Object_Decl_F_Mode (N_Bare_Object_Decl);
                           when Object_Decl_F_Type_Expr =>
                              return Object_Decl_F_Type_Expr
                                  (N_Bare_Object_Decl);
                           when Object_Decl_F_Default_Expr =>
                              return Object_Decl_F_Default_Expr
                                  (N_Bare_Object_Decl);
                           when Object_Decl_F_Renaming_Clause =>
                              return Object_Decl_F_Renaming_Clause
                                  (N_Bare_Object_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Base_Package_Decl =>
                     declare
                        N_Bare_Base_Package_Decl : constant Bare_Base_Package_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Base_Package_Decl_F_Package_Name =>
                              return Base_Package_Decl_F_Package_Name
                                  (N_Bare_Base_Package_Decl);
                           when Base_Package_Decl_F_Public_Part =>
                              return Base_Package_Decl_F_Public_Part
                                  (N_Bare_Base_Package_Decl);
                           when Base_Package_Decl_F_Private_Part =>
                              return Base_Package_Decl_F_Private_Part
                                  (N_Bare_Base_Package_Decl);
                           when Base_Package_Decl_F_End_Name =>
                              return Base_Package_Decl_F_End_Name
                                  (N_Bare_Base_Package_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Exception_Decl_Range =>
                     declare
                        N_Bare_Exception_Decl : constant Bare_Exception_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Exception_Decl_F_Ids =>
                              return Exception_Decl_F_Ids
                                  (N_Bare_Exception_Decl);
                           when Exception_Decl_F_Renames =>
                              return Exception_Decl_F_Renames
                                  (N_Bare_Exception_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Subp_Instantiation_Range =>
                     declare
                        N_Bare_Generic_Subp_Instantiation : constant Bare_Generic_Subp_Instantiation :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Subp_Instantiation_F_Overriding =>
                              return Generic_Subp_Instantiation_F_Overriding
                                  (N_Bare_Generic_Subp_Instantiation);
                           when Generic_Subp_Instantiation_F_Kind =>
                              return Generic_Subp_Instantiation_F_Kind
                                  (N_Bare_Generic_Subp_Instantiation);
                           when Generic_Subp_Instantiation_F_Subp_Name =>
                              return Generic_Subp_Instantiation_F_Subp_Name
                                  (N_Bare_Generic_Subp_Instantiation);
                           when Generic_Subp_Instantiation_F_Generic_Subp_Name =>
                              return Generic_Subp_Instantiation_F_Generic_Subp_Name
                                  (N_Bare_Generic_Subp_Instantiation);
                           when Generic_Subp_Instantiation_F_Params =>
                              return Generic_Subp_Instantiation_F_Params
                                  (N_Bare_Generic_Subp_Instantiation);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Package_Instantiation_Range =>
                     declare
                        N_Bare_Generic_Package_Instantiation : constant Bare_Generic_Package_Instantiation :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Package_Instantiation_F_Name =>
                              return Generic_Package_Instantiation_F_Name
                                  (N_Bare_Generic_Package_Instantiation);
                           when Generic_Package_Instantiation_F_Generic_Pkg_Name =>
                              return Generic_Package_Instantiation_F_Generic_Pkg_Name
                                  (N_Bare_Generic_Package_Instantiation);
                           when Generic_Package_Instantiation_F_Params =>
                              return Generic_Package_Instantiation_F_Params
                                  (N_Bare_Generic_Package_Instantiation);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Package_Renaming_Decl_Range =>
                     declare
                        N_Bare_Package_Renaming_Decl : constant Bare_Package_Renaming_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Package_Renaming_Decl_F_Name =>
                              return Package_Renaming_Decl_F_Name
                                  (N_Bare_Package_Renaming_Decl);
                           when Package_Renaming_Decl_F_Renames =>
                              return Package_Renaming_Decl_F_Renames
                                  (N_Bare_Package_Renaming_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Package_Renaming_Decl_Range =>
                     declare
                        N_Bare_Generic_Package_Renaming_Decl : constant Bare_Generic_Package_Renaming_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Package_Renaming_Decl_F_Name =>
                              return Generic_Package_Renaming_Decl_F_Name
                                  (N_Bare_Generic_Package_Renaming_Decl);
                           when Generic_Package_Renaming_Decl_F_Renames =>
                              return Generic_Package_Renaming_Decl_F_Renames
                                  (N_Bare_Generic_Package_Renaming_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Subp_Renaming_Decl_Range =>
                     declare
                        N_Bare_Generic_Subp_Renaming_Decl : constant Bare_Generic_Subp_Renaming_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Subp_Renaming_Decl_F_Kind =>
                              return Generic_Subp_Renaming_Decl_F_Kind
                                  (N_Bare_Generic_Subp_Renaming_Decl);
                           when Generic_Subp_Renaming_Decl_F_Name =>
                              return Generic_Subp_Renaming_Decl_F_Name
                                  (N_Bare_Generic_Subp_Renaming_Decl);
                           when Generic_Subp_Renaming_Decl_F_Renames =>
                              return Generic_Subp_Renaming_Decl_F_Renames
                                  (N_Bare_Generic_Subp_Renaming_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Generic_Decl =>
                     declare
                        N_Bare_Generic_Decl : constant Bare_Generic_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Generic_Decl_F_Formal_Part =>
                              return Generic_Decl_F_Formal_Part
                                  (N_Bare_Generic_Decl);
                           when others =>
                              null;
                        end case;
                        case Ada_Generic_Decl (Kind) is
                           when Ada_Generic_Subp_Decl_Range =>
                              declare
                                 N_Bare_Generic_Subp_Decl : constant Bare_Generic_Subp_Decl :=
                                   N_Bare_Generic_Decl;
                              begin
                                 case Field is
                                    when Generic_Subp_Decl_F_Subp_Decl =>
                                       return Generic_Subp_Decl_F_Subp_Decl
                                           (N_Bare_Generic_Subp_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when Ada_Generic_Package_Decl_Range =>
                              declare
                                 N_Bare_Generic_Package_Decl : constant Bare_Generic_Package_Decl :=
                                   N_Bare_Generic_Decl;
                              begin
                                 case Field is
                                    when Generic_Package_Decl_F_Package_Decl =>
                                       return Generic_Package_Decl_F_Package_Decl
                                           (N_Bare_Generic_Package_Decl);
                                    when others =>
                                       null;
                                 end case;
                              end;
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_For_Loop_Var_Decl_Range =>
                     declare
                        N_Bare_For_Loop_Var_Decl : constant Bare_For_Loop_Var_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when For_Loop_Var_Decl_F_Id =>
                              return For_Loop_Var_Decl_F_Id
                                  (N_Bare_For_Loop_Var_Decl);
                           when For_Loop_Var_Decl_F_Id_Type =>
                              return For_Loop_Var_Decl_F_Id_Type
                                  (N_Bare_For_Loop_Var_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Exception_Handler_Range =>
                     declare
                        N_Bare_Exception_Handler : constant Bare_Exception_Handler :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Exception_Handler_F_Exception_Name =>
                              return Exception_Handler_F_Exception_Name
                                  (N_Bare_Exception_Handler);
                           when Exception_Handler_F_Handled_Exceptions =>
                              return Exception_Handler_F_Handled_Exceptions
                                  (N_Bare_Exception_Handler);
                           when Exception_Handler_F_Stmts =>
                              return Exception_Handler_F_Stmts
                                  (N_Bare_Exception_Handler);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Label_Decl_Range =>
                     declare
                        N_Bare_Label_Decl : constant Bare_Label_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Label_Decl_F_Name =>
                              return Label_Decl_F_Name (N_Bare_Label_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Named_Stmt_Decl_Range =>
                     declare
                        N_Bare_Named_Stmt_Decl : constant Bare_Named_Stmt_Decl :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Named_Stmt_Decl_F_Name =>
                              return Named_Stmt_Decl_F_Name
                                  (N_Bare_Named_Stmt_Decl);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Entry_Index_Spec_Range =>
                     declare
                        N_Bare_Entry_Index_Spec : constant Bare_Entry_Index_Spec :=
                          N_Bare_Basic_Decl;
                     begin
                        case Field is
                           when Entry_Index_Spec_F_Id =>
                              return Entry_Index_Spec_F_Id
                                  (N_Bare_Entry_Index_Spec);
                           when Entry_Index_Spec_F_Subtype =>
                              return Entry_Index_Spec_F_Subtype
                                  (N_Bare_Entry_Index_Spec);
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Known_Discriminant_Part_Range =>
            declare
               N_Bare_Known_Discriminant_Part : constant Bare_Known_Discriminant_Part :=
                 Node;
            begin
               case Field is
                  when Known_Discriminant_Part_F_Discr_Specs =>
                     return Known_Discriminant_Part_F_Discr_Specs
                         (N_Bare_Known_Discriminant_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Component_List_Range =>
            declare
               N_Bare_Component_List : constant Bare_Component_List := Node;
            begin
               case Field is
                  when Component_List_F_Components =>
                     return Component_List_F_Components
                         (N_Bare_Component_List);
                  when Component_List_F_Variant_Part =>
                     return Component_List_F_Variant_Part
                         (N_Bare_Component_List);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Generic_Formal_Part_Range =>
            declare
               N_Bare_Generic_Formal_Part : constant Bare_Generic_Formal_Part :=
                 Node;
            begin
               case Field is
                  when Generic_Formal_Part_F_Decls =>
                     return Generic_Formal_Part_F_Decls
                         (N_Bare_Generic_Formal_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Subp_Spec_Range =>
            declare
               N_Bare_Subp_Spec : constant Bare_Subp_Spec := Node;
            begin
               case Field is
                  when Subp_Spec_F_Subp_Kind =>
                     return Subp_Spec_F_Subp_Kind (N_Bare_Subp_Spec);
                  when Subp_Spec_F_Subp_Name =>
                     return Subp_Spec_F_Subp_Name (N_Bare_Subp_Spec);
                  when Subp_Spec_F_Subp_Params =>
                     return Subp_Spec_F_Subp_Params (N_Bare_Subp_Spec);
                  when Subp_Spec_F_Subp_Returns =>
                     return Subp_Spec_F_Subp_Returns (N_Bare_Subp_Spec);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Entry_Spec_Range =>
            declare
               N_Bare_Entry_Spec : constant Bare_Entry_Spec := Node;
            begin
               case Field is
                  when Entry_Spec_F_Entry_Name =>
                     return Entry_Spec_F_Entry_Name (N_Bare_Entry_Spec);
                  when Entry_Spec_F_Family_Type =>
                     return Entry_Spec_F_Family_Type (N_Bare_Entry_Spec);
                  when Entry_Spec_F_Entry_Params =>
                     return Entry_Spec_F_Entry_Params (N_Bare_Entry_Spec);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Entry_Completion_Formal_Params_Range =>
            declare
               N_Bare_Entry_Completion_Formal_Params : constant Bare_Entry_Completion_Formal_Params :=
                 Node;
            begin
               case Field is
                  when Entry_Completion_Formal_Params_F_Params =>
                     return Entry_Completion_Formal_Params_F_Params
                         (N_Bare_Entry_Completion_Formal_Params);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Record_Type_Def_Range =>
            declare
               N_Bare_Record_Type_Def : constant Bare_Record_Type_Def := Node;
            begin
               case Field is
                  when Record_Type_Def_F_Has_Abstract =>
                     return Record_Type_Def_F_Has_Abstract
                         (N_Bare_Record_Type_Def);
                  when Record_Type_Def_F_Has_Tagged =>
                     return Record_Type_Def_F_Has_Tagged
                         (N_Bare_Record_Type_Def);
                  when Record_Type_Def_F_Has_Limited =>
                     return Record_Type_Def_F_Has_Limited
                         (N_Bare_Record_Type_Def);
                  when Record_Type_Def_F_Record_Def =>
                     return Record_Type_Def_F_Record_Def
                         (N_Bare_Record_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Floating_Point_Def_Range =>
            declare
               N_Bare_Floating_Point_Def : constant Bare_Floating_Point_Def :=
                 Node;
            begin
               case Field is
                  when Floating_Point_Def_F_Num_Digits =>
                     return Floating_Point_Def_F_Num_Digits
                         (N_Bare_Floating_Point_Def);
                  when Floating_Point_Def_F_Range =>
                     return Floating_Point_Def_F_Range
                         (N_Bare_Floating_Point_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Ordinary_Fixed_Point_Def_Range =>
            declare
               N_Bare_Ordinary_Fixed_Point_Def : constant Bare_Ordinary_Fixed_Point_Def :=
                 Node;
            begin
               case Field is
                  when Ordinary_Fixed_Point_Def_F_Delta =>
                     return Ordinary_Fixed_Point_Def_F_Delta
                         (N_Bare_Ordinary_Fixed_Point_Def);
                  when Ordinary_Fixed_Point_Def_F_Range =>
                     return Ordinary_Fixed_Point_Def_F_Range
                         (N_Bare_Ordinary_Fixed_Point_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Decimal_Fixed_Point_Def_Range =>
            declare
               N_Bare_Decimal_Fixed_Point_Def : constant Bare_Decimal_Fixed_Point_Def :=
                 Node;
            begin
               case Field is
                  when Decimal_Fixed_Point_Def_F_Delta =>
                     return Decimal_Fixed_Point_Def_F_Delta
                         (N_Bare_Decimal_Fixed_Point_Def);
                  when Decimal_Fixed_Point_Def_F_Digits =>
                     return Decimal_Fixed_Point_Def_F_Digits
                         (N_Bare_Decimal_Fixed_Point_Def);
                  when Decimal_Fixed_Point_Def_F_Range =>
                     return Decimal_Fixed_Point_Def_F_Range
                         (N_Bare_Decimal_Fixed_Point_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Enum_Type_Def_Range =>
            declare
               N_Bare_Enum_Type_Def : constant Bare_Enum_Type_Def := Node;
            begin
               case Field is
                  when Enum_Type_Def_F_Enum_Literals =>
                     return Enum_Type_Def_F_Enum_Literals
                         (N_Bare_Enum_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Derived_Type_Def_Range =>
            declare
               N_Bare_Derived_Type_Def : constant Bare_Derived_Type_Def :=
                 Node;
            begin
               case Field is
                  when Derived_Type_Def_F_Has_Abstract =>
                     return Derived_Type_Def_F_Has_Abstract
                         (N_Bare_Derived_Type_Def);
                  when Derived_Type_Def_F_Has_Limited =>
                     return Derived_Type_Def_F_Has_Limited
                         (N_Bare_Derived_Type_Def);
                  when Derived_Type_Def_F_Has_Synchronized =>
                     return Derived_Type_Def_F_Has_Synchronized
                         (N_Bare_Derived_Type_Def);
                  when Derived_Type_Def_F_Subtype_Indication =>
                     return Derived_Type_Def_F_Subtype_Indication
                         (N_Bare_Derived_Type_Def);
                  when Derived_Type_Def_F_Interfaces =>
                     return Derived_Type_Def_F_Interfaces
                         (N_Bare_Derived_Type_Def);
                  when Derived_Type_Def_F_Record_Extension =>
                     return Derived_Type_Def_F_Record_Extension
                         (N_Bare_Derived_Type_Def);
                  when Derived_Type_Def_F_Has_With_Private =>
                     return Derived_Type_Def_F_Has_With_Private
                         (N_Bare_Derived_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Private_Type_Def_Range =>
            declare
               N_Bare_Private_Type_Def : constant Bare_Private_Type_Def :=
                 Node;
            begin
               case Field is
                  when Private_Type_Def_F_Has_Abstract =>
                     return Private_Type_Def_F_Has_Abstract
                         (N_Bare_Private_Type_Def);
                  when Private_Type_Def_F_Has_Tagged =>
                     return Private_Type_Def_F_Has_Tagged
                         (N_Bare_Private_Type_Def);
                  when Private_Type_Def_F_Has_Limited =>
                     return Private_Type_Def_F_Has_Limited
                         (N_Bare_Private_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Signed_Int_Type_Def_Range =>
            declare
               N_Bare_Signed_Int_Type_Def : constant Bare_Signed_Int_Type_Def :=
                 Node;
            begin
               case Field is
                  when Signed_Int_Type_Def_F_Range =>
                     return Signed_Int_Type_Def_F_Range
                         (N_Bare_Signed_Int_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Mod_Int_Type_Def_Range =>
            declare
               N_Bare_Mod_Int_Type_Def : constant Bare_Mod_Int_Type_Def :=
                 Node;
            begin
               case Field is
                  when Mod_Int_Type_Def_F_Expr =>
                     return Mod_Int_Type_Def_F_Expr (N_Bare_Mod_Int_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Array_Type_Def_Range =>
            declare
               N_Bare_Array_Type_Def : constant Bare_Array_Type_Def := Node;
            begin
               case Field is
                  when Array_Type_Def_F_Indices =>
                     return Array_Type_Def_F_Indices (N_Bare_Array_Type_Def);
                  when Array_Type_Def_F_Component_Type =>
                     return Array_Type_Def_F_Component_Type
                         (N_Bare_Array_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Interface_Type_Def_Range =>
            declare
               N_Bare_Interface_Type_Def : constant Bare_Interface_Type_Def :=
                 Node;
            begin
               case Field is
                  when Interface_Type_Def_F_Interface_Kind =>
                     return Interface_Type_Def_F_Interface_Kind
                         (N_Bare_Interface_Type_Def);
                  when Interface_Type_Def_F_Interfaces =>
                     return Interface_Type_Def_F_Interfaces
                         (N_Bare_Interface_Type_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Access_Def =>
            declare
               N_Bare_Access_Def : constant Bare_Access_Def := Node;
            begin
               case Field is
                  when Access_Def_F_Has_Not_Null =>
                     return Access_Def_F_Has_Not_Null (N_Bare_Access_Def);
                  when others =>
                     null;
               end case;
               case Ada_Access_Def (Kind) is
                  when Ada_Access_To_Subp_Def_Range =>
                     declare
                        N_Bare_Access_To_Subp_Def : constant Bare_Access_To_Subp_Def :=
                          N_Bare_Access_Def;
                     begin
                        case Field is
                           when Access_To_Subp_Def_F_Has_Protected =>
                              return Access_To_Subp_Def_F_Has_Protected
                                  (N_Bare_Access_To_Subp_Def);
                           when Access_To_Subp_Def_F_Subp_Spec =>
                              return Access_To_Subp_Def_F_Subp_Spec
                                  (N_Bare_Access_To_Subp_Def);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Type_Access_Def_Range =>
                     declare
                        N_Bare_Type_Access_Def : constant Bare_Type_Access_Def :=
                          N_Bare_Access_Def;
                     begin
                        case Field is
                           when Type_Access_Def_F_Has_All =>
                              return Type_Access_Def_F_Has_All
                                  (N_Bare_Type_Access_Def);
                           when Type_Access_Def_F_Has_Constant =>
                              return Type_Access_Def_F_Has_Constant
                                  (N_Bare_Type_Access_Def);
                           when Type_Access_Def_F_Subtype_Indication =>
                              return Type_Access_Def_F_Subtype_Indication
                                  (N_Bare_Type_Access_Def);
                           when others =>
                              null;
                        end case;
                     end;
                  when Ada_Anonymous_Type_Access_Def_Range =>
                     declare
                        N_Bare_Anonymous_Type_Access_Def : constant Bare_Anonymous_Type_Access_Def :=
                          N_Bare_Access_Def;
                     begin
                        case Field is
                           when Anonymous_Type_Access_Def_F_Type_Decl =>
                              return Anonymous_Type_Access_Def_F_Type_Decl
                                  (N_Bare_Anonymous_Type_Access_Def);
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Variant_Range =>
            declare
               N_Bare_Variant : constant Bare_Variant := Node;
            begin
               case Field is
                  when Variant_F_Choices =>
                     return Variant_F_Choices (N_Bare_Variant);
                  when Variant_F_Components =>
                     return Variant_F_Components (N_Bare_Variant);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Variant_Part_Range =>
            declare
               N_Bare_Variant_Part : constant Bare_Variant_Part := Node;
            begin
               case Field is
                  when Variant_Part_F_Discr_Name =>
                     return Variant_Part_F_Discr_Name (N_Bare_Variant_Part);
                  when Variant_Part_F_Variant =>
                     return Variant_Part_F_Variant (N_Bare_Variant_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Base_Record_Def =>
            declare
               N_Bare_Base_Record_Def : constant Bare_Base_Record_Def := Node;
            begin
               case Field is
                  when Base_Record_Def_F_Components =>
                     return Base_Record_Def_F_Components
                         (N_Bare_Base_Record_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Pragma_Argument_Assoc_Range =>
            declare
               N_Bare_Pragma_Argument_Assoc : constant Bare_Pragma_Argument_Assoc :=
                 Node;
            begin
               case Field is
                  when Pragma_Argument_Assoc_F_Id =>
                     return Pragma_Argument_Assoc_F_Id
                         (N_Bare_Pragma_Argument_Assoc);
                  when Pragma_Argument_Assoc_F_Expr =>
                     return Pragma_Argument_Assoc_F_Expr
                         (N_Bare_Pragma_Argument_Assoc);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Contract_Case_Assoc_Range =>
            declare
               N_Bare_Contract_Case_Assoc : constant Bare_Contract_Case_Assoc :=
                 Node;
            begin
               case Field is
                  when Contract_Case_Assoc_F_Guard =>
                     return Contract_Case_Assoc_F_Guard
                         (N_Bare_Contract_Case_Assoc);
                  when Contract_Case_Assoc_F_Consequence =>
                     return Contract_Case_Assoc_F_Consequence
                         (N_Bare_Contract_Case_Assoc);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Range_Constraint_Range =>
            declare
               N_Bare_Range_Constraint : constant Bare_Range_Constraint :=
                 Node;
            begin
               case Field is
                  when Range_Constraint_F_Range =>
                     return Range_Constraint_F_Range (N_Bare_Range_Constraint);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Digits_Constraint_Range =>
            declare
               N_Bare_Digits_Constraint : constant Bare_Digits_Constraint :=
                 Node;
            begin
               case Field is
                  when Digits_Constraint_F_Digits =>
                     return Digits_Constraint_F_Digits
                         (N_Bare_Digits_Constraint);
                  when Digits_Constraint_F_Range =>
                     return Digits_Constraint_F_Range
                         (N_Bare_Digits_Constraint);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Delta_Constraint_Range =>
            declare
               N_Bare_Delta_Constraint : constant Bare_Delta_Constraint :=
                 Node;
            begin
               case Field is
                  when Delta_Constraint_F_Digits =>
                     return Delta_Constraint_F_Digits
                         (N_Bare_Delta_Constraint);
                  when Delta_Constraint_F_Range =>
                     return Delta_Constraint_F_Range (N_Bare_Delta_Constraint);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Index_Constraint_Range =>
            declare
               N_Bare_Index_Constraint : constant Bare_Index_Constraint :=
                 Node;
            begin
               case Field is
                  when Index_Constraint_F_Constraints =>
                     return Index_Constraint_F_Constraints
                         (N_Bare_Index_Constraint);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Discriminant_Constraint_Range =>
            declare
               N_Bare_Discriminant_Constraint : constant Bare_Discriminant_Constraint :=
                 Node;
            begin
               case Field is
                  when Discriminant_Constraint_F_Constraints =>
                     return Discriminant_Constraint_F_Constraints
                         (N_Bare_Discriminant_Constraint);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Discriminant_Assoc_Range =>
            declare
               N_Bare_Discriminant_Assoc : constant Bare_Discriminant_Assoc :=
                 Node;
            begin
               case Field is
                  when Discriminant_Assoc_F_Ids =>
                     return Discriminant_Assoc_F_Ids
                         (N_Bare_Discriminant_Assoc);
                  when Discriminant_Assoc_F_Discr_Expr =>
                     return Discriminant_Assoc_F_Discr_Expr
                         (N_Bare_Discriminant_Assoc);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Param_Assoc_Range =>
            declare
               N_Bare_Param_Assoc : constant Bare_Param_Assoc := Node;
            begin
               case Field is
                  when Param_Assoc_F_Designator =>
                     return Param_Assoc_F_Designator (N_Bare_Param_Assoc);
                  when Param_Assoc_F_R_Expr =>
                     return Param_Assoc_F_R_Expr (N_Bare_Param_Assoc);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Aggregate_Assoc_Range =>
            declare
               N_Bare_Aggregate_Assoc : constant Bare_Aggregate_Assoc := Node;
            begin
               case Field is
                  when Aggregate_Assoc_F_Designators =>
                     return Aggregate_Assoc_F_Designators
                         (N_Bare_Aggregate_Assoc);
                  when Aggregate_Assoc_F_R_Expr =>
                     return Aggregate_Assoc_F_R_Expr (N_Bare_Aggregate_Assoc);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Unconstrained_Array_Indices_Range =>
            declare
               N_Bare_Unconstrained_Array_Indices : constant Bare_Unconstrained_Array_Indices :=
                 Node;
            begin
               case Field is
                  when Unconstrained_Array_Indices_F_Types =>
                     return Unconstrained_Array_Indices_F_Types
                         (N_Bare_Unconstrained_Array_Indices);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Constrained_Array_Indices_Range =>
            declare
               N_Bare_Constrained_Array_Indices : constant Bare_Constrained_Array_Indices :=
                 Node;
            begin
               case Field is
                  when Constrained_Array_Indices_F_List =>
                     return Constrained_Array_Indices_F_List
                         (N_Bare_Constrained_Array_Indices);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Component_Def_Range =>
            declare
               N_Bare_Component_Def : constant Bare_Component_Def := Node;
            begin
               case Field is
                  when Component_Def_F_Has_Aliased =>
                     return Component_Def_F_Has_Aliased (N_Bare_Component_Def);
                  when Component_Def_F_Has_Constant =>
                     return Component_Def_F_Has_Constant
                         (N_Bare_Component_Def);
                  when Component_Def_F_Type_Expr =>
                     return Component_Def_F_Type_Expr (N_Bare_Component_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Task_Def_Range =>
            declare
               N_Bare_Task_Def : constant Bare_Task_Def := Node;
            begin
               case Field is
                  when Task_Def_F_Interfaces =>
                     return Task_Def_F_Interfaces (N_Bare_Task_Def);
                  when Task_Def_F_Public_Part =>
                     return Task_Def_F_Public_Part (N_Bare_Task_Def);
                  when Task_Def_F_Private_Part =>
                     return Task_Def_F_Private_Part (N_Bare_Task_Def);
                  when Task_Def_F_End_Name =>
                     return Task_Def_F_End_Name (N_Bare_Task_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Protected_Def_Range =>
            declare
               N_Bare_Protected_Def : constant Bare_Protected_Def := Node;
            begin
               case Field is
                  when Protected_Def_F_Public_Part =>
                     return Protected_Def_F_Public_Part (N_Bare_Protected_Def);
                  when Protected_Def_F_Private_Part =>
                     return Protected_Def_F_Private_Part
                         (N_Bare_Protected_Def);
                  when Protected_Def_F_End_Name =>
                     return Protected_Def_F_End_Name (N_Bare_Protected_Def);
                  when others =>
                     null;
               end case;
            end;
         when Ada_With_Clause_Range =>
            declare
               N_Bare_With_Clause : constant Bare_With_Clause := Node;
            begin
               case Field is
                  when With_Clause_F_Has_Limited =>
                     return With_Clause_F_Has_Limited (N_Bare_With_Clause);
                  when With_Clause_F_Has_Private =>
                     return With_Clause_F_Has_Private (N_Bare_With_Clause);
                  when With_Clause_F_Packages =>
                     return With_Clause_F_Packages (N_Bare_With_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Use_Package_Clause_Range =>
            declare
               N_Bare_Use_Package_Clause : constant Bare_Use_Package_Clause :=
                 Node;
            begin
               case Field is
                  when Use_Package_Clause_F_Packages =>
                     return Use_Package_Clause_F_Packages
                         (N_Bare_Use_Package_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Use_Type_Clause_Range =>
            declare
               N_Bare_Use_Type_Clause : constant Bare_Use_Type_Clause := Node;
            begin
               case Field is
                  when Use_Type_Clause_F_Has_All =>
                     return Use_Type_Clause_F_Has_All (N_Bare_Use_Type_Clause);
                  when Use_Type_Clause_F_Types =>
                     return Use_Type_Clause_F_Types (N_Bare_Use_Type_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Anonymous_Type_Range =>
            declare
               N_Bare_Anonymous_Type : constant Bare_Anonymous_Type := Node;
            begin
               case Field is
                  when Anonymous_Type_F_Type_Decl =>
                     return Anonymous_Type_F_Type_Decl (N_Bare_Anonymous_Type);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Subtype_Indication_Range =>
            declare
               N_Bare_Subtype_Indication : constant Bare_Subtype_Indication :=
                 Node;
            begin
               case Field is
                  when Subtype_Indication_F_Has_Not_Null =>
                     return Subtype_Indication_F_Has_Not_Null
                         (N_Bare_Subtype_Indication);
                  when Subtype_Indication_F_Name =>
                     return Subtype_Indication_F_Name
                         (N_Bare_Subtype_Indication);
                  when Subtype_Indication_F_Constraint =>
                     return Subtype_Indication_F_Constraint
                         (N_Bare_Subtype_Indication);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Aspect_Spec_Range =>
            declare
               N_Bare_Aspect_Spec : constant Bare_Aspect_Spec := Node;
            begin
               case Field is
                  when Aspect_Spec_F_Aspect_Assocs =>
                     return Aspect_Spec_F_Aspect_Assocs (N_Bare_Aspect_Spec);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Pragma_Node_Range =>
            declare
               N_Bare_Pragma_Node : constant Bare_Pragma_Node := Node;
            begin
               case Field is
                  when Pragma_Node_F_Id =>
                     return Pragma_Node_F_Id (N_Bare_Pragma_Node);
                  when Pragma_Node_F_Args =>
                     return Pragma_Node_F_Args (N_Bare_Pragma_Node);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Enum_Rep_Clause_Range =>
            declare
               N_Bare_Enum_Rep_Clause : constant Bare_Enum_Rep_Clause := Node;
            begin
               case Field is
                  when Enum_Rep_Clause_F_Type_Name =>
                     return Enum_Rep_Clause_F_Type_Name
                         (N_Bare_Enum_Rep_Clause);
                  when Enum_Rep_Clause_F_Aggregate =>
                     return Enum_Rep_Clause_F_Aggregate
                         (N_Bare_Enum_Rep_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Attribute_Def_Clause_Range =>
            declare
               N_Bare_Attribute_Def_Clause : constant Bare_Attribute_Def_Clause :=
                 Node;
            begin
               case Field is
                  when Attribute_Def_Clause_F_Attribute_Expr =>
                     return Attribute_Def_Clause_F_Attribute_Expr
                         (N_Bare_Attribute_Def_Clause);
                  when Attribute_Def_Clause_F_Expr =>
                     return Attribute_Def_Clause_F_Expr
                         (N_Bare_Attribute_Def_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Record_Rep_Clause_Range =>
            declare
               N_Bare_Record_Rep_Clause : constant Bare_Record_Rep_Clause :=
                 Node;
            begin
               case Field is
                  when Record_Rep_Clause_F_Name =>
                     return Record_Rep_Clause_F_Name
                         (N_Bare_Record_Rep_Clause);
                  when Record_Rep_Clause_F_At_Expr =>
                     return Record_Rep_Clause_F_At_Expr
                         (N_Bare_Record_Rep_Clause);
                  when Record_Rep_Clause_F_Components =>
                     return Record_Rep_Clause_F_Components
                         (N_Bare_Record_Rep_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_At_Clause_Range =>
            declare
               N_Bare_At_Clause : constant Bare_At_Clause := Node;
            begin
               case Field is
                  when At_Clause_F_Name =>
                     return At_Clause_F_Name (N_Bare_At_Clause);
                  when At_Clause_F_Expr =>
                     return At_Clause_F_Expr (N_Bare_At_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Component_Clause_Range =>
            declare
               N_Bare_Component_Clause : constant Bare_Component_Clause :=
                 Node;
            begin
               case Field is
                  when Component_Clause_F_Id =>
                     return Component_Clause_F_Id (N_Bare_Component_Clause);
                  when Component_Clause_F_Position =>
                     return Component_Clause_F_Position
                         (N_Bare_Component_Clause);
                  when Component_Clause_F_Range =>
                     return Component_Clause_F_Range (N_Bare_Component_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Aspect_Assoc_Range =>
            declare
               N_Bare_Aspect_Assoc : constant Bare_Aspect_Assoc := Node;
            begin
               case Field is
                  when Aspect_Assoc_F_Id =>
                     return Aspect_Assoc_F_Id (N_Bare_Aspect_Assoc);
                  when Aspect_Assoc_F_Expr =>
                     return Aspect_Assoc_F_Expr (N_Bare_Aspect_Assoc);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Declarative_Part_Range =>
            declare
               N_Bare_Declarative_Part : constant Bare_Declarative_Part :=
                 Node;
            begin
               case Field is
                  when Declarative_Part_F_Decls =>
                     return Declarative_Part_F_Decls (N_Bare_Declarative_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Renaming_Clause_Range =>
            declare
               N_Bare_Renaming_Clause : constant Bare_Renaming_Clause := Node;
            begin
               case Field is
                  when Renaming_Clause_F_Renamed_Object =>
                     return Renaming_Clause_F_Renamed_Object
                         (N_Bare_Renaming_Clause);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Contract_Cases_Range =>
            declare
               N_Bare_Contract_Cases : constant Bare_Contract_Cases := Node;
            begin
               case Field is
                  when Contract_Cases_F_Contract_Cases =>
                     return Contract_Cases_F_Contract_Cases
                         (N_Bare_Contract_Cases);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Paren_Expr_Range =>
            declare
               N_Bare_Paren_Expr : constant Bare_Paren_Expr := Node;
            begin
               case Field is
                  when Paren_Expr_F_Expr =>
                     return Paren_Expr_F_Expr (N_Bare_Paren_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Un_Op_Range =>
            declare
               N_Bare_Un_Op : constant Bare_Un_Op := Node;
            begin
               case Field is
                  when Un_Op_F_Op =>
                     return Un_Op_F_Op (N_Bare_Un_Op);
                  when Un_Op_F_Expr =>
                     return Un_Op_F_Expr (N_Bare_Un_Op);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Bin_Op_Range =>
            declare
               N_Bare_Bin_Op : constant Bare_Bin_Op := Node;
            begin
               case Field is
                  when Bin_Op_F_Left =>
                     return Bin_Op_F_Left (N_Bare_Bin_Op);
                  when Bin_Op_F_Op =>
                     return Bin_Op_F_Op (N_Bare_Bin_Op);
                  when Bin_Op_F_Right =>
                     return Bin_Op_F_Right (N_Bare_Bin_Op);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Membership_Expr_Range =>
            declare
               N_Bare_Membership_Expr : constant Bare_Membership_Expr := Node;
            begin
               case Field is
                  when Membership_Expr_F_Expr =>
                     return Membership_Expr_F_Expr (N_Bare_Membership_Expr);
                  when Membership_Expr_F_Op =>
                     return Membership_Expr_F_Op (N_Bare_Membership_Expr);
                  when Membership_Expr_F_Membership_Exprs =>
                     return Membership_Expr_F_Membership_Exprs
                         (N_Bare_Membership_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Base_Aggregate =>
            declare
               N_Bare_Base_Aggregate : constant Bare_Base_Aggregate := Node;
            begin
               case Field is
                  when Base_Aggregate_F_Ancestor_Expr =>
                     return Base_Aggregate_F_Ancestor_Expr
                         (N_Bare_Base_Aggregate);
                  when Base_Aggregate_F_Assocs =>
                     return Base_Aggregate_F_Assocs (N_Bare_Base_Aggregate);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Discrete_Subtype_Name_Range =>
            declare
               N_Bare_Discrete_Subtype_Name : constant Bare_Discrete_Subtype_Name :=
                 Node;
            begin
               case Field is
                  when Discrete_Subtype_Name_F_Subtype =>
                     return Discrete_Subtype_Name_F_Subtype
                         (N_Bare_Discrete_Subtype_Name);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Call_Expr_Range =>
            declare
               N_Bare_Call_Expr : constant Bare_Call_Expr := Node;
            begin
               case Field is
                  when Call_Expr_F_Name =>
                     return Call_Expr_F_Name (N_Bare_Call_Expr);
                  when Call_Expr_F_Suffix =>
                     return Call_Expr_F_Suffix (N_Bare_Call_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Explicit_Deref_Range =>
            declare
               N_Bare_Explicit_Deref : constant Bare_Explicit_Deref := Node;
            begin
               case Field is
                  when Explicit_Deref_F_Prefix =>
                     return Explicit_Deref_F_Prefix (N_Bare_Explicit_Deref);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Defining_Name_Range =>
            declare
               N_Bare_Defining_Name : constant Bare_Defining_Name := Node;
            begin
               case Field is
                  when Defining_Name_F_Name =>
                     return Defining_Name_F_Name (N_Bare_Defining_Name);
                  when others =>
                     null;
               end case;
            end;
         when Ada_End_Name_Range =>
            declare
               N_Bare_End_Name : constant Bare_End_Name := Node;
            begin
               case Field is
                  when End_Name_F_Name =>
                     return End_Name_F_Name (N_Bare_End_Name);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Qual_Expr_Range =>
            declare
               N_Bare_Qual_Expr : constant Bare_Qual_Expr := Node;
            begin
               case Field is
                  when Qual_Expr_F_Prefix =>
                     return Qual_Expr_F_Prefix (N_Bare_Qual_Expr);
                  when Qual_Expr_F_Suffix =>
                     return Qual_Expr_F_Suffix (N_Bare_Qual_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Attribute_Ref_Range =>
            declare
               N_Bare_Attribute_Ref : constant Bare_Attribute_Ref := Node;
            begin
               case Field is
                  when Attribute_Ref_F_Prefix =>
                     return Attribute_Ref_F_Prefix (N_Bare_Attribute_Ref);
                  when Attribute_Ref_F_Attribute =>
                     return Attribute_Ref_F_Attribute (N_Bare_Attribute_Ref);
                  when Attribute_Ref_F_Args =>
                     return Attribute_Ref_F_Args (N_Bare_Attribute_Ref);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Dotted_Name_Range =>
            declare
               N_Bare_Dotted_Name : constant Bare_Dotted_Name := Node;
            begin
               case Field is
                  when Dotted_Name_F_Prefix =>
                     return Dotted_Name_F_Prefix (N_Bare_Dotted_Name);
                  when Dotted_Name_F_Suffix =>
                     return Dotted_Name_F_Suffix (N_Bare_Dotted_Name);
                  when others =>
                     null;
               end case;
            end;
         when Ada_If_Expr_Range =>
            declare
               N_Bare_If_Expr : constant Bare_If_Expr := Node;
            begin
               case Field is
                  when If_Expr_F_Cond_Expr =>
                     return If_Expr_F_Cond_Expr (N_Bare_If_Expr);
                  when If_Expr_F_Then_Expr =>
                     return If_Expr_F_Then_Expr (N_Bare_If_Expr);
                  when If_Expr_F_Alternatives =>
                     return If_Expr_F_Alternatives (N_Bare_If_Expr);
                  when If_Expr_F_Else_Expr =>
                     return If_Expr_F_Else_Expr (N_Bare_If_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Case_Expr_Range =>
            declare
               N_Bare_Case_Expr : constant Bare_Case_Expr := Node;
            begin
               case Field is
                  when Case_Expr_F_Expr =>
                     return Case_Expr_F_Expr (N_Bare_Case_Expr);
                  when Case_Expr_F_Cases =>
                     return Case_Expr_F_Cases (N_Bare_Case_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Case_Expr_Alternative_Range =>
            declare
               N_Bare_Case_Expr_Alternative : constant Bare_Case_Expr_Alternative :=
                 Node;
            begin
               case Field is
                  when Case_Expr_Alternative_F_Choices =>
                     return Case_Expr_Alternative_F_Choices
                         (N_Bare_Case_Expr_Alternative);
                  when Case_Expr_Alternative_F_Expr =>
                     return Case_Expr_Alternative_F_Expr
                         (N_Bare_Case_Expr_Alternative);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Quantified_Expr_Range =>
            declare
               N_Bare_Quantified_Expr : constant Bare_Quantified_Expr := Node;
            begin
               case Field is
                  when Quantified_Expr_F_Quantifier =>
                     return Quantified_Expr_F_Quantifier
                         (N_Bare_Quantified_Expr);
                  when Quantified_Expr_F_Loop_Spec =>
                     return Quantified_Expr_F_Loop_Spec
                         (N_Bare_Quantified_Expr);
                  when Quantified_Expr_F_Expr =>
                     return Quantified_Expr_F_Expr (N_Bare_Quantified_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Allocator_Range =>
            declare
               N_Bare_Allocator : constant Bare_Allocator := Node;
            begin
               case Field is
                  when Allocator_F_Subpool =>
                     return Allocator_F_Subpool (N_Bare_Allocator);
                  when Allocator_F_Type_Or_Expr =>
                     return Allocator_F_Type_Or_Expr (N_Bare_Allocator);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Raise_Expr_Range =>
            declare
               N_Bare_Raise_Expr : constant Bare_Raise_Expr := Node;
            begin
               case Field is
                  when Raise_Expr_F_Exception_Name =>
                     return Raise_Expr_F_Exception_Name (N_Bare_Raise_Expr);
                  when Raise_Expr_F_Error_Message =>
                     return Raise_Expr_F_Error_Message (N_Bare_Raise_Expr);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Elsif_Expr_Part_Range =>
            declare
               N_Bare_Elsif_Expr_Part : constant Bare_Elsif_Expr_Part := Node;
            begin
               case Field is
                  when Elsif_Expr_Part_F_Cond_Expr =>
                     return Elsif_Expr_Part_F_Cond_Expr
                         (N_Bare_Elsif_Expr_Part);
                  when Elsif_Expr_Part_F_Then_Expr =>
                     return Elsif_Expr_Part_F_Then_Expr
                         (N_Bare_Elsif_Expr_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_For_Loop_Spec_Range =>
            declare
               N_Bare_For_Loop_Spec : constant Bare_For_Loop_Spec := Node;
            begin
               case Field is
                  when For_Loop_Spec_F_Var_Decl =>
                     return For_Loop_Spec_F_Var_Decl (N_Bare_For_Loop_Spec);
                  when For_Loop_Spec_F_Loop_Type =>
                     return For_Loop_Spec_F_Loop_Type (N_Bare_For_Loop_Spec);
                  when For_Loop_Spec_F_Has_Reverse =>
                     return For_Loop_Spec_F_Has_Reverse (N_Bare_For_Loop_Spec);
                  when For_Loop_Spec_F_Iter_Expr =>
                     return For_Loop_Spec_F_Iter_Expr (N_Bare_For_Loop_Spec);
                  when others =>
                     null;
               end case;
            end;
         when Ada_While_Loop_Spec_Range =>
            declare
               N_Bare_While_Loop_Spec : constant Bare_While_Loop_Spec := Node;
            begin
               case Field is
                  when While_Loop_Spec_F_Expr =>
                     return While_Loop_Spec_F_Expr (N_Bare_While_Loop_Spec);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Compilation_Unit_Range =>
            declare
               N_Bare_Compilation_Unit : constant Bare_Compilation_Unit :=
                 Node;
            begin
               case Field is
                  when Compilation_Unit_F_Prelude =>
                     return Compilation_Unit_F_Prelude
                         (N_Bare_Compilation_Unit);
                  when Compilation_Unit_F_Body =>
                     return Compilation_Unit_F_Body (N_Bare_Compilation_Unit);
                  when Compilation_Unit_F_Pragmas =>
                     return Compilation_Unit_F_Pragmas
                         (N_Bare_Compilation_Unit);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Handled_Stmts_Range =>
            declare
               N_Bare_Handled_Stmts : constant Bare_Handled_Stmts := Node;
            begin
               case Field is
                  when Handled_Stmts_F_Stmts =>
                     return Handled_Stmts_F_Stmts (N_Bare_Handled_Stmts);
                  when Handled_Stmts_F_Exceptions =>
                     return Handled_Stmts_F_Exceptions (N_Bare_Handled_Stmts);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Call_Stmt_Range =>
            declare
               N_Bare_Call_Stmt : constant Bare_Call_Stmt := Node;
            begin
               case Field is
                  when Call_Stmt_F_Call =>
                     return Call_Stmt_F_Call (N_Bare_Call_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Assign_Stmt_Range =>
            declare
               N_Bare_Assign_Stmt : constant Bare_Assign_Stmt := Node;
            begin
               case Field is
                  when Assign_Stmt_F_Dest =>
                     return Assign_Stmt_F_Dest (N_Bare_Assign_Stmt);
                  when Assign_Stmt_F_Expr =>
                     return Assign_Stmt_F_Expr (N_Bare_Assign_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Goto_Stmt_Range =>
            declare
               N_Bare_Goto_Stmt : constant Bare_Goto_Stmt := Node;
            begin
               case Field is
                  when Goto_Stmt_F_Label_Name =>
                     return Goto_Stmt_F_Label_Name (N_Bare_Goto_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Exit_Stmt_Range =>
            declare
               N_Bare_Exit_Stmt : constant Bare_Exit_Stmt := Node;
            begin
               case Field is
                  when Exit_Stmt_F_Loop_Name =>
                     return Exit_Stmt_F_Loop_Name (N_Bare_Exit_Stmt);
                  when Exit_Stmt_F_Cond_Expr =>
                     return Exit_Stmt_F_Cond_Expr (N_Bare_Exit_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Return_Stmt_Range =>
            declare
               N_Bare_Return_Stmt : constant Bare_Return_Stmt := Node;
            begin
               case Field is
                  when Return_Stmt_F_Return_Expr =>
                     return Return_Stmt_F_Return_Expr (N_Bare_Return_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Requeue_Stmt_Range =>
            declare
               N_Bare_Requeue_Stmt : constant Bare_Requeue_Stmt := Node;
            begin
               case Field is
                  when Requeue_Stmt_F_Call_Name =>
                     return Requeue_Stmt_F_Call_Name (N_Bare_Requeue_Stmt);
                  when Requeue_Stmt_F_Has_Abort =>
                     return Requeue_Stmt_F_Has_Abort (N_Bare_Requeue_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Abort_Stmt_Range =>
            declare
               N_Bare_Abort_Stmt : constant Bare_Abort_Stmt := Node;
            begin
               case Field is
                  when Abort_Stmt_F_Names =>
                     return Abort_Stmt_F_Names (N_Bare_Abort_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Delay_Stmt_Range =>
            declare
               N_Bare_Delay_Stmt : constant Bare_Delay_Stmt := Node;
            begin
               case Field is
                  when Delay_Stmt_F_Has_Until =>
                     return Delay_Stmt_F_Has_Until (N_Bare_Delay_Stmt);
                  when Delay_Stmt_F_Expr =>
                     return Delay_Stmt_F_Expr (N_Bare_Delay_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Raise_Stmt_Range =>
            declare
               N_Bare_Raise_Stmt : constant Bare_Raise_Stmt := Node;
            begin
               case Field is
                  when Raise_Stmt_F_Exception_Name =>
                     return Raise_Stmt_F_Exception_Name (N_Bare_Raise_Stmt);
                  when Raise_Stmt_F_Error_Message =>
                     return Raise_Stmt_F_Error_Message (N_Bare_Raise_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Label_Range =>
            declare
               N_Bare_Label : constant Bare_Label := Node;
            begin
               case Field is
                  when Label_F_Decl =>
                     return Label_F_Decl (N_Bare_Label);
                  when others =>
                     null;
               end case;
            end;
         when Ada_If_Stmt_Range =>
            declare
               N_Bare_If_Stmt : constant Bare_If_Stmt := Node;
            begin
               case Field is
                  when If_Stmt_F_Cond_Expr =>
                     return If_Stmt_F_Cond_Expr (N_Bare_If_Stmt);
                  when If_Stmt_F_Then_Stmts =>
                     return If_Stmt_F_Then_Stmts (N_Bare_If_Stmt);
                  when If_Stmt_F_Alternatives =>
                     return If_Stmt_F_Alternatives (N_Bare_If_Stmt);
                  when If_Stmt_F_Else_Stmts =>
                     return If_Stmt_F_Else_Stmts (N_Bare_If_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Named_Stmt_Range =>
            declare
               N_Bare_Named_Stmt : constant Bare_Named_Stmt := Node;
            begin
               case Field is
                  when Named_Stmt_F_Decl =>
                     return Named_Stmt_F_Decl (N_Bare_Named_Stmt);
                  when Named_Stmt_F_Stmt =>
                     return Named_Stmt_F_Stmt (N_Bare_Named_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Base_Loop_Stmt =>
            declare
               N_Bare_Base_Loop_Stmt : constant Bare_Base_Loop_Stmt := Node;
            begin
               case Field is
                  when Base_Loop_Stmt_F_Spec =>
                     return Base_Loop_Stmt_F_Spec (N_Bare_Base_Loop_Stmt);
                  when Base_Loop_Stmt_F_Stmts =>
                     return Base_Loop_Stmt_F_Stmts (N_Bare_Base_Loop_Stmt);
                  when Base_Loop_Stmt_F_End_Name =>
                     return Base_Loop_Stmt_F_End_Name (N_Bare_Base_Loop_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Decl_Block_Range =>
            declare
               N_Bare_Decl_Block : constant Bare_Decl_Block := Node;
            begin
               case Field is
                  when Decl_Block_F_Decls =>
                     return Decl_Block_F_Decls (N_Bare_Decl_Block);
                  when Decl_Block_F_Stmts =>
                     return Decl_Block_F_Stmts (N_Bare_Decl_Block);
                  when Decl_Block_F_End_Name =>
                     return Decl_Block_F_End_Name (N_Bare_Decl_Block);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Begin_Block_Range =>
            declare
               N_Bare_Begin_Block : constant Bare_Begin_Block := Node;
            begin
               case Field is
                  when Begin_Block_F_Stmts =>
                     return Begin_Block_F_Stmts (N_Bare_Begin_Block);
                  when Begin_Block_F_End_Name =>
                     return Begin_Block_F_End_Name (N_Bare_Begin_Block);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Extended_Return_Stmt_Range =>
            declare
               N_Bare_Extended_Return_Stmt : constant Bare_Extended_Return_Stmt :=
                 Node;
            begin
               case Field is
                  when Extended_Return_Stmt_F_Decl =>
                     return Extended_Return_Stmt_F_Decl
                         (N_Bare_Extended_Return_Stmt);
                  when Extended_Return_Stmt_F_Stmts =>
                     return Extended_Return_Stmt_F_Stmts
                         (N_Bare_Extended_Return_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Case_Stmt_Range =>
            declare
               N_Bare_Case_Stmt : constant Bare_Case_Stmt := Node;
            begin
               case Field is
                  when Case_Stmt_F_Expr =>
                     return Case_Stmt_F_Expr (N_Bare_Case_Stmt);
                  when Case_Stmt_F_Alternatives =>
                     return Case_Stmt_F_Alternatives (N_Bare_Case_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Accept_Stmt_Range =>
            declare
               N_Bare_Accept_Stmt : constant Bare_Accept_Stmt := Node;
            begin
               case Field is
                  when Accept_Stmt_F_Name =>
                     return Accept_Stmt_F_Name (N_Bare_Accept_Stmt);
                  when Accept_Stmt_F_Entry_Index_Expr =>
                     return Accept_Stmt_F_Entry_Index_Expr
                         (N_Bare_Accept_Stmt);
                  when Accept_Stmt_F_Params =>
                     return Accept_Stmt_F_Params (N_Bare_Accept_Stmt);
                  when others =>
                     null;
               end case;
               case Ada_Accept_Stmt_Range (Kind) is
                  when Ada_Accept_Stmt_With_Stmts_Range =>
                     declare
                        N_Bare_Accept_Stmt_With_Stmts : constant Bare_Accept_Stmt_With_Stmts :=
                          N_Bare_Accept_Stmt;
                     begin
                        case Field is
                           when Accept_Stmt_With_Stmts_F_Stmts =>
                              return Accept_Stmt_With_Stmts_F_Stmts
                                  (N_Bare_Accept_Stmt_With_Stmts);
                           when Accept_Stmt_With_Stmts_F_End_Name =>
                              return Accept_Stmt_With_Stmts_F_End_Name
                                  (N_Bare_Accept_Stmt_With_Stmts);
                           when others =>
                              null;
                        end case;
                     end;
                  when others =>
                     null;
               end case;
            end;
         when Ada_Select_Stmt_Range =>
            declare
               N_Bare_Select_Stmt : constant Bare_Select_Stmt := Node;
            begin
               case Field is
                  when Select_Stmt_F_Guards =>
                     return Select_Stmt_F_Guards (N_Bare_Select_Stmt);
                  when Select_Stmt_F_Else_Stmts =>
                     return Select_Stmt_F_Else_Stmts (N_Bare_Select_Stmt);
                  when Select_Stmt_F_Abort_Stmts =>
                     return Select_Stmt_F_Abort_Stmts (N_Bare_Select_Stmt);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Elsif_Stmt_Part_Range =>
            declare
               N_Bare_Elsif_Stmt_Part : constant Bare_Elsif_Stmt_Part := Node;
            begin
               case Field is
                  when Elsif_Stmt_Part_F_Cond_Expr =>
                     return Elsif_Stmt_Part_F_Cond_Expr
                         (N_Bare_Elsif_Stmt_Part);
                  when Elsif_Stmt_Part_F_Stmts =>
                     return Elsif_Stmt_Part_F_Stmts (N_Bare_Elsif_Stmt_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Case_Stmt_Alternative_Range =>
            declare
               N_Bare_Case_Stmt_Alternative : constant Bare_Case_Stmt_Alternative :=
                 Node;
            begin
               case Field is
                  when Case_Stmt_Alternative_F_Choices =>
                     return Case_Stmt_Alternative_F_Choices
                         (N_Bare_Case_Stmt_Alternative);
                  when Case_Stmt_Alternative_F_Stmts =>
                     return Case_Stmt_Alternative_F_Stmts
                         (N_Bare_Case_Stmt_Alternative);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Select_When_Part_Range =>
            declare
               N_Bare_Select_When_Part : constant Bare_Select_When_Part :=
                 Node;
            begin
               case Field is
                  when Select_When_Part_F_Cond_Expr =>
                     return Select_When_Part_F_Cond_Expr
                         (N_Bare_Select_When_Part);
                  when Select_When_Part_F_Stmts =>
                     return Select_When_Part_F_Stmts (N_Bare_Select_When_Part);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Subunit_Range =>
            declare
               N_Bare_Subunit : constant Bare_Subunit := Node;
            begin
               case Field is
                  when Subunit_F_Name =>
                     return Subunit_F_Name (N_Bare_Subunit);
                  when Subunit_F_Body =>
                     return Subunit_F_Body (N_Bare_Subunit);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Library_Item_Range =>
            declare
               N_Bare_Library_Item : constant Bare_Library_Item := Node;
            begin
               case Field is
                  when Library_Item_F_Has_Private =>
                     return Library_Item_F_Has_Private (N_Bare_Library_Item);
                  when Library_Item_F_Item =>
                     return Library_Item_F_Item (N_Bare_Library_Item);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Range_Spec_Range =>
            declare
               N_Bare_Range_Spec : constant Bare_Range_Spec := Node;
            begin
               case Field is
                  when Range_Spec_F_Range =>
                     return Range_Spec_F_Range (N_Bare_Range_Spec);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Params_Range =>
            declare
               N_Bare_Params : constant Bare_Params := Node;
            begin
               case Field is
                  when Params_F_Params =>
                     return Params_F_Params (N_Bare_Params);
                  when others =>
                     null;
               end case;
            end;
         when Ada_Unconstrained_Array_Index_Range =>
            declare
               N_Bare_Unconstrained_Array_Index : constant Bare_Unconstrained_Array_Index :=
                 Node;
            begin
               case Field is
                  when Unconstrained_Array_Index_F_Subtype_Indication =>
                     return Unconstrained_Array_Index_F_Subtype_Indication
                         (N_Bare_Unconstrained_Array_Index);
                  when others =>
                     null;
               end case;
            end;
         when others =>
            null;
      end case;

      return
        (raise Node_Data_Evaluation_Error with "no such field on this node");
   end Eval_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : Ada_Node_Kind_Type; Field : Field_Reference) return Positive
   is
   begin

      case Kind is
         when Ada_Abort_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Abort_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Abstract_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Abstract_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Ada_Node_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Alternatives_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Constraint_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Decl_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Stmt_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Aspect_Assoc_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Base_Assoc_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Assoc_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Case_Expr_Alternative_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Case_Stmt_Alternative_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Compilation_Unit_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Contract_Case_Assoc_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Defining_Name_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Discriminant_Spec_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Elsif_Expr_Part_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Elsif_Stmt_Part_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Enum_Literal_Decl_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Expr_Alternatives_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Discriminant_Choice_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Name_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Parent_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Param_Spec_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Pragma_Node_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Select_When_Part_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Unconstrained_Array_Index_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Variant_List =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Aliased_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Aliased_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_All_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_All_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Constrained_Array_Indices =>
            return
              (case Field is when Constrained_Array_Indices_F_List => 1,
                 when others => raise Constraint_Error);
         when Ada_Unconstrained_Array_Indices =>
            return
              (case Field is when Unconstrained_Array_Indices_F_Types => 1,
                 when others => raise Constraint_Error);
         when Ada_Aspect_Assoc =>
            return
              (case Field is when Aspect_Assoc_F_Id => 1,
                 when Aspect_Assoc_F_Expr           => 2,
                 when others                        => raise Constraint_Error);
         when Ada_At_Clause =>
            return
              (case Field is when At_Clause_F_Name => 1,
                 when At_Clause_F_Expr             => 2,
                 when others                       => raise Constraint_Error);
         when Ada_Attribute_Def_Clause =>
            return
              (case Field is when Attribute_Def_Clause_F_Attribute_Expr => 1,
                 when Attribute_Def_Clause_F_Expr                       => 2,
                 when others => raise Constraint_Error);
         when Ada_Enum_Rep_Clause =>
            return
              (case Field is when Enum_Rep_Clause_F_Type_Name => 1,
                 when Enum_Rep_Clause_F_Aggregate             => 2,
                 when others => raise Constraint_Error);
         when Ada_Record_Rep_Clause =>
            return
              (case Field is when Record_Rep_Clause_F_Name => 1,
                 when Record_Rep_Clause_F_At_Expr          => 2,
                 when Record_Rep_Clause_F_Components       => 3,
                 when others => raise Constraint_Error);
         when Ada_Aspect_Spec =>
            return
              (case Field is when Aspect_Spec_F_Aspect_Assocs => 1,
                 when others => raise Constraint_Error);
         when Ada_Contract_Case_Assoc =>
            return
              (case Field is when Contract_Case_Assoc_F_Guard => 1,
                 when Contract_Case_Assoc_F_Consequence       => 2,
                 when others => raise Constraint_Error);
         when Ada_Pragma_Argument_Assoc =>
            return
              (case Field is when Pragma_Argument_Assoc_F_Id => 1,
                 when Pragma_Argument_Assoc_F_Expr           => 2,
                 when others => raise Constraint_Error);
         when Ada_Entry_Spec =>
            return
              (case Field is when Entry_Spec_F_Entry_Name => 1,
                 when Entry_Spec_F_Family_Type            => 2,
                 when Entry_Spec_F_Entry_Params           => 3,
                 when others => raise Constraint_Error);
         when Ada_Enum_Subp_Spec =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Subp_Spec =>
            return
              (case Field is when Subp_Spec_F_Subp_Kind => 1,
                 when Subp_Spec_F_Subp_Name             => 2,
                 when Subp_Spec_F_Subp_Params           => 3,
                 when Subp_Spec_F_Subp_Returns          => 4,
                 when others => raise Constraint_Error);
         when Ada_Component_List =>
            return
              (case Field is when Component_List_F_Components => 1,
                 when Component_List_F_Variant_Part           => 2,
                 when others => raise Constraint_Error);
         when Ada_Known_Discriminant_Part =>
            return
              (case Field is when Known_Discriminant_Part_F_Discr_Specs => 1,
                 when others => raise Constraint_Error);
         when Ada_Unknown_Discriminant_Part =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Entry_Completion_Formal_Params =>
            return
              (case Field is when Entry_Completion_Formal_Params_F_Params => 1,
                 when others => raise Constraint_Error);
         when Ada_Generic_Formal_Part =>
            return
              (case Field is when Generic_Formal_Part_F_Decls => 1,
                 when others => raise Constraint_Error);
         when Ada_Null_Record_Def =>
            return
              (case Field is when Base_Record_Def_F_Components => 1,
                 when others => raise Constraint_Error);
         when Ada_Record_Def =>
            return
              (case Field is when Base_Record_Def_F_Components => 1,
                 when others => raise Constraint_Error);
         when Ada_Aggregate_Assoc =>
            return
              (case Field is when Aggregate_Assoc_F_Designators => 1,
                 when Aggregate_Assoc_F_R_Expr                  => 2,
                 when others => raise Constraint_Error);
         when Ada_Multi_Dim_Array_Assoc =>
            return
              (case Field is when Aggregate_Assoc_F_Designators => 1,
                 when Aggregate_Assoc_F_R_Expr                  => 2,
                 when others => raise Constraint_Error);
         when Ada_Discriminant_Assoc =>
            return
              (case Field is when Discriminant_Assoc_F_Ids => 1,
                 when Discriminant_Assoc_F_Discr_Expr      => 2,
                 when others => raise Constraint_Error);
         when Ada_Param_Assoc =>
            return
              (case Field is when Param_Assoc_F_Designator => 1,
                 when Param_Assoc_F_R_Expr                 => 2,
                 when others => raise Constraint_Error);
         when Ada_Component_Decl =>
            return
              (case Field is when Component_Decl_F_Ids => 1,
                 when Component_Decl_F_Component_Def   => 2,
                 when Component_Decl_F_Default_Expr    => 3,
                 when Basic_Decl_F_Aspects             => 4,
                 when others => raise Constraint_Error);
         when Ada_Discriminant_Spec =>
            return
              (case Field is when Discriminant_Spec_F_Ids => 1,
                 when Discriminant_Spec_F_Type_Expr       => 2,
                 when Discriminant_Spec_F_Default_Expr    => 3,
                 when others => raise Constraint_Error);
         when Ada_Generic_Formal_Obj_Decl =>
            return
              (case Field is when Generic_Formal_F_Decl => 1,
                 when others => raise Constraint_Error);
         when Ada_Generic_Formal_Package =>
            return
              (case Field is when Generic_Formal_F_Decl => 1,
                 when others => raise Constraint_Error);
         when Ada_Generic_Formal_Subp_Decl =>
            return
              (case Field is when Generic_Formal_F_Decl => 1,
                 when others => raise Constraint_Error);
         when Ada_Generic_Formal_Type_Decl =>
            return
              (case Field is when Generic_Formal_F_Decl => 1,
                 when others => raise Constraint_Error);
         when Ada_Param_Spec =>
            return
              (case Field is when Param_Spec_F_Ids => 1,
                 when Param_Spec_F_Has_Aliased     => 2,
                 when Param_Spec_F_Mode => 3, when Param_Spec_F_Type_Expr => 4,
                 when Param_Spec_F_Default_Expr    => 5,
                 when others                       => raise Constraint_Error);
         when Ada_Generic_Package_Internal =>
            return
              (case Field is when Base_Package_Decl_F_Package_Name => 1,
                 when Basic_Decl_F_Aspects                         => 2,
                 when Base_Package_Decl_F_Public_Part              => 3,
                 when Base_Package_Decl_F_Private_Part             => 4,
                 when Base_Package_Decl_F_End_Name                 => 5,
                 when others => raise Constraint_Error);
         when Ada_Package_Decl =>
            return
              (case Field is when Base_Package_Decl_F_Package_Name => 1,
                 when Basic_Decl_F_Aspects                         => 2,
                 when Base_Package_Decl_F_Public_Part              => 3,
                 when Base_Package_Decl_F_Private_Part             => 4,
                 when Base_Package_Decl_F_End_Name                 => 5,
                 when others => raise Constraint_Error);
         when Ada_Discrete_Base_Subtype_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when others => raise Constraint_Error);
         when Ada_Subtype_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when Subtype_Decl_F_Subtype            => 2,
                 when Basic_Decl_F_Aspects              => 3,
                 when others => raise Constraint_Error);
         when Ada_Classwide_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when others => raise Constraint_Error);
         when Ada_Incomplete_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name    => 1,
                 when Incomplete_Type_Decl_F_Discriminants => 2,
                 when others => raise Constraint_Error);
         when Ada_Incomplete_Tagged_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name          => 1,
                 when Incomplete_Type_Decl_F_Discriminants       => 2,
                 when Incomplete_Tagged_Type_Decl_F_Has_Abstract => 3,
                 when others => raise Constraint_Error);
         when Ada_Protected_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name   => 1,
                 when Protected_Type_Decl_F_Discriminants => 2,
                 when Basic_Decl_F_Aspects                => 3,
                 when Protected_Type_Decl_F_Interfaces    => 4,
                 when Protected_Type_Decl_F_Definition    => 5,
                 when others => raise Constraint_Error);
         when Ada_Task_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when Task_Type_Decl_F_Discriminants    => 2,
                 when Basic_Decl_F_Aspects              => 3,
                 when Task_Type_Decl_F_Definition       => 4,
                 when others => raise Constraint_Error);
         when Ada_Single_Task_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when Task_Type_Decl_F_Discriminants    => 2,
                 when Basic_Decl_F_Aspects              => 3,
                 when Task_Type_Decl_F_Definition       => 4,
                 when others => raise Constraint_Error);
         when Ada_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when Type_Decl_F_Discriminants         => 2,
                 when Type_Decl_F_Type_Def              => 3,
                 when Basic_Decl_F_Aspects              => 4,
                 when others => raise Constraint_Error);
         when Ada_Anonymous_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when Type_Decl_F_Discriminants         => 2,
                 when Type_Decl_F_Type_Def              => 3,
                 when Basic_Decl_F_Aspects              => 4,
                 when others => raise Constraint_Error);
         when Ada_Synth_Anonymous_Type_Decl =>
            return
              (case Field is when Base_Type_Decl_F_Name => 1,
                 when Type_Decl_F_Discriminants         => 2,
                 when Type_Decl_F_Type_Def              => 3,
                 when Basic_Decl_F_Aspects              => 4,
                 when others => raise Constraint_Error);
         when Ada_Abstract_Subp_Decl =>
            return
              (case Field is when Classic_Subp_Decl_F_Overriding => 1,
                 when Classic_Subp_Decl_F_Subp_Spec              => 2,
                 when Basic_Decl_F_Aspects                       => 3,
                 when others => raise Constraint_Error);
         when Ada_Abstract_Formal_Subp_Decl =>
            return
              (case Field is when Classic_Subp_Decl_F_Overriding => 1,
                 when Classic_Subp_Decl_F_Subp_Spec              => 2,
                 when Formal_Subp_Decl_F_Default_Expr            => 3,
                 when Basic_Decl_F_Aspects                       => 4,
                 when others => raise Constraint_Error);
         when Ada_Concrete_Formal_Subp_Decl =>
            return
              (case Field is when Classic_Subp_Decl_F_Overriding => 1,
                 when Classic_Subp_Decl_F_Subp_Spec              => 2,
                 when Formal_Subp_Decl_F_Default_Expr            => 3,
                 when Basic_Decl_F_Aspects                       => 4,
                 when others => raise Constraint_Error);
         when Ada_Subp_Decl =>
            return
              (case Field is when Classic_Subp_Decl_F_Overriding => 1,
                 when Classic_Subp_Decl_F_Subp_Spec              => 2,
                 when Basic_Decl_F_Aspects                       => 3,
                 when others => raise Constraint_Error);
         when Ada_Entry_Decl =>
            return
              (case Field is when Entry_Decl_F_Overriding => 1,
                 when Entry_Decl_F_Spec => 2, when Basic_Decl_F_Aspects => 3,
                 when others => raise Constraint_Error);
         when Ada_Enum_Literal_Decl =>
            return
              (case Field is when Enum_Literal_Decl_F_Name => 1,
                 when others => raise Constraint_Error);
         when Ada_Generic_Subp_Internal =>
            return
              (case Field is when Generic_Subp_Internal_F_Subp_Spec => 1,
                 when Basic_Decl_F_Aspects                          => 2,
                 when others => raise Constraint_Error);
         when Ada_Expr_Function =>
            return
              (case Field is when Base_Subp_Body_F_Overriding => 1,
                 when Base_Subp_Body_F_Subp_Spec              => 2,
                 when Expr_Function_F_Expr                    => 3,
                 when Basic_Decl_F_Aspects                    => 4,
                 when others => raise Constraint_Error);
         when Ada_Null_Subp_Decl =>
            return
              (case Field is when Base_Subp_Body_F_Overriding => 1,
                 when Base_Subp_Body_F_Subp_Spec              => 2,
                 when Basic_Decl_F_Aspects                    => 3,
                 when others => raise Constraint_Error);
         when Ada_Subp_Body =>
            return
              (case Field is when Base_Subp_Body_F_Overriding => 1,
                 when Base_Subp_Body_F_Subp_Spec              => 2,
                 when Basic_Decl_F_Aspects => 3, when Subp_Body_F_Decls => 4,
                 when Subp_Body_F_Stmts => 5, when Subp_Body_F_End_Name => 6,
                 when others => raise Constraint_Error);
         when Ada_Subp_Renaming_Decl =>
            return
              (case Field is when Base_Subp_Body_F_Overriding => 1,
                 when Base_Subp_Body_F_Subp_Spec              => 2,
                 when Subp_Renaming_Decl_F_Renames            => 3,
                 when Basic_Decl_F_Aspects                    => 4,
                 when others => raise Constraint_Error);
         when Ada_Package_Body_Stub =>
            return
              (case Field is when Package_Body_Stub_F_Name => 1,
                 when Basic_Decl_F_Aspects                 => 2,
                 when others => raise Constraint_Error);
         when Ada_Protected_Body_Stub =>
            return
              (case Field is when Protected_Body_Stub_F_Name => 1,
                 when Basic_Decl_F_Aspects                   => 2,
                 when others => raise Constraint_Error);
         when Ada_Subp_Body_Stub =>
            return
              (case Field is when Subp_Body_Stub_F_Overriding => 1,
                 when Subp_Body_Stub_F_Subp_Spec              => 2,
                 when Basic_Decl_F_Aspects                    => 3,
                 when others => raise Constraint_Error);
         when Ada_Task_Body_Stub =>
            return
              (case Field is when Task_Body_Stub_F_Name => 1,
                 when Basic_Decl_F_Aspects              => 2,
                 when others => raise Constraint_Error);
         when Ada_Entry_Body =>
            return
              (case Field is when Entry_Body_F_Entry_Name => 1,
                 when Entry_Body_F_Index_Spec             => 2,
                 when Entry_Body_F_Params => 3, when Entry_Body_F_Barrier => 4,
                 when Entry_Body_F_Decls => 5, when Entry_Body_F_Stmts => 6,
                 when Entry_Body_F_End_Name               => 7,
                 when others => raise Constraint_Error);
         when Ada_Package_Body =>
            return
              (case Field is when Package_Body_F_Package_Name => 1,
                 when Basic_Decl_F_Aspects                    => 2,
                 when Package_Body_F_Decls                    => 3,
                 when Package_Body_F_Stmts                    => 4,
                 when Package_Body_F_End_Name                 => 5,
                 when others => raise Constraint_Error);
         when Ada_Protected_Body =>
            return
              (case Field is when Protected_Body_F_Name => 1,
                 when Basic_Decl_F_Aspects              => 2,
                 when Protected_Body_F_Decls            => 3,
                 when Protected_Body_F_End_Name         => 4,
                 when others => raise Constraint_Error);
         when Ada_Task_Body =>
            return
              (case Field is when Task_Body_F_Name => 1,
                 when Basic_Decl_F_Aspects => 2, when Task_Body_F_Decls => 3,
                 when Task_Body_F_Stmts => 4, when Task_Body_F_End_Name => 5,
                 when others                       => raise Constraint_Error);
         when Ada_Entry_Index_Spec =>
            return
              (case Field is when Entry_Index_Spec_F_Id => 1,
                 when Entry_Index_Spec_F_Subtype        => 2,
                 when others => raise Constraint_Error);
         when Ada_Error_Decl =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Exception_Decl =>
            return
              (case Field is when Exception_Decl_F_Ids => 1,
                 when Exception_Decl_F_Renames         => 2,
                 when Basic_Decl_F_Aspects             => 3,
                 when others => raise Constraint_Error);
         when Ada_Exception_Handler =>
            return
              (case Field is when Exception_Handler_F_Exception_Name => 1,
                 when Exception_Handler_F_Handled_Exceptions         => 2,
                 when Exception_Handler_F_Stmts                      => 3,
                 when others => raise Constraint_Error);
         when Ada_For_Loop_Var_Decl =>
            return
              (case Field is when For_Loop_Var_Decl_F_Id => 1,
                 when For_Loop_Var_Decl_F_Id_Type        => 2,
                 when others => raise Constraint_Error);
         when Ada_Generic_Package_Decl =>
            return
              (case Field is when Generic_Decl_F_Formal_Part => 1,
                 when Generic_Package_Decl_F_Package_Decl    => 2,
                 when others => raise Constraint_Error);
         when Ada_Generic_Subp_Decl =>
            return
              (case Field is when Generic_Decl_F_Formal_Part => 1,
                 when Generic_Subp_Decl_F_Subp_Decl          => 2,
                 when others => raise Constraint_Error);
         when Ada_Generic_Package_Instantiation =>
            return
              (case Field is when Generic_Package_Instantiation_F_Name => 1,
                 when Generic_Package_Instantiation_F_Generic_Pkg_Name => 2,
                 when Generic_Package_Instantiation_F_Params           => 3,
                 when Basic_Decl_F_Aspects                             => 4,
                 when others => raise Constraint_Error);
         when Ada_Generic_Subp_Instantiation =>
            return
              (case Field is when Generic_Subp_Instantiation_F_Overriding => 1,
                 when Generic_Subp_Instantiation_F_Kind                   => 2,
                 when Generic_Subp_Instantiation_F_Subp_Name              => 3,
                 when Generic_Subp_Instantiation_F_Generic_Subp_Name      => 4,
                 when Generic_Subp_Instantiation_F_Params                 => 5,
                 when Basic_Decl_F_Aspects                                => 6,
                 when others => raise Constraint_Error);
         when Ada_Generic_Package_Renaming_Decl =>
            return
              (case Field is when Generic_Package_Renaming_Decl_F_Name => 1,
                 when Generic_Package_Renaming_Decl_F_Renames          => 2,
                 when Basic_Decl_F_Aspects                             => 3,
                 when others => raise Constraint_Error);
         when Ada_Generic_Subp_Renaming_Decl =>
            return
              (case Field is when Generic_Subp_Renaming_Decl_F_Kind => 1,
                 when Generic_Subp_Renaming_Decl_F_Name             => 2,
                 when Generic_Subp_Renaming_Decl_F_Renames          => 3,
                 when Basic_Decl_F_Aspects                          => 4,
                 when others => raise Constraint_Error);
         when Ada_Label_Decl =>
            return
              (case Field is when Label_Decl_F_Name => 1,
                 when others                        => raise Constraint_Error);
         when Ada_Named_Stmt_Decl =>
            return
              (case Field is when Named_Stmt_Decl_F_Name => 1,
                 when others => raise Constraint_Error);
         when Ada_Number_Decl =>
            return
              (case Field is when Number_Decl_F_Ids => 1,
                 when Number_Decl_F_Expr            => 2,
                 when others                        => raise Constraint_Error);
         when Ada_Object_Decl =>
            return
              (case Field is when Object_Decl_F_Ids => 1,
                 when Object_Decl_F_Has_Aliased     => 2,
                 when Object_Decl_F_Has_Constant    => 3,
                 when Object_Decl_F_Mode            => 4,
                 when Object_Decl_F_Type_Expr       => 5,
                 when Object_Decl_F_Default_Expr    => 6,
                 when Object_Decl_F_Renaming_Clause => 7,
                 when Basic_Decl_F_Aspects          => 8,
                 when others                        => raise Constraint_Error);
         when Ada_Anonymous_Object_Decl =>
            return
              (case Field is when Object_Decl_F_Ids => 1,
                 when Object_Decl_F_Has_Aliased     => 2,
                 when Object_Decl_F_Has_Constant    => 3,
                 when Object_Decl_F_Mode            => 4,
                 when Object_Decl_F_Type_Expr       => 5,
                 when Object_Decl_F_Default_Expr    => 6,
                 when Object_Decl_F_Renaming_Clause => 7,
                 when Basic_Decl_F_Aspects          => 8,
                 when others                        => raise Constraint_Error);
         when Ada_Extended_Return_Stmt_Object_Decl =>
            return
              (case Field is when Object_Decl_F_Ids => 1,
                 when Object_Decl_F_Has_Aliased     => 2,
                 when Object_Decl_F_Has_Constant    => 3,
                 when Object_Decl_F_Mode            => 4,
                 when Object_Decl_F_Type_Expr       => 5,
                 when Object_Decl_F_Default_Expr    => 6,
                 when Object_Decl_F_Renaming_Clause => 7,
                 when Basic_Decl_F_Aspects          => 8,
                 when others                        => raise Constraint_Error);
         when Ada_Package_Renaming_Decl =>
            return
              (case Field is when Package_Renaming_Decl_F_Name => 1,
                 when Package_Renaming_Decl_F_Renames          => 2,
                 when Basic_Decl_F_Aspects                     => 3,
                 when others => raise Constraint_Error);
         when Ada_Single_Protected_Decl =>
            return
              (case Field is when Single_Protected_Decl_F_Name => 1,
                 when Basic_Decl_F_Aspects                     => 2,
                 when Single_Protected_Decl_F_Interfaces       => 3,
                 when Single_Protected_Decl_F_Definition       => 4,
                 when others => raise Constraint_Error);
         when Ada_Single_Task_Decl =>
            return
              (case Field is when Single_Task_Decl_F_Task_Type => 1,
                 when others => raise Constraint_Error);
         when Ada_Case_Stmt_Alternative =>
            return
              (case Field is when Case_Stmt_Alternative_F_Choices => 1,
                 when Case_Stmt_Alternative_F_Stmts               => 2,
                 when others => raise Constraint_Error);
         when Ada_Compilation_Unit =>
            return
              (case Field is when Compilation_Unit_F_Prelude => 1,
                 when Compilation_Unit_F_Body                => 2,
                 when Compilation_Unit_F_Pragmas             => 3,
                 when others => raise Constraint_Error);
         when Ada_Component_Clause =>
            return
              (case Field is when Component_Clause_F_Id => 1,
                 when Component_Clause_F_Position       => 2,
                 when Component_Clause_F_Range          => 3,
                 when others => raise Constraint_Error);
         when Ada_Component_Def =>
            return
              (case Field is when Component_Def_F_Has_Aliased => 1,
                 when Component_Def_F_Has_Constant            => 2,
                 when Component_Def_F_Type_Expr               => 3,
                 when others => raise Constraint_Error);
         when Ada_Constant_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Constant_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Delta_Constraint =>
            return
              (case Field is when Delta_Constraint_F_Digits => 1,
                 when Delta_Constraint_F_Range              => 2,
                 when others => raise Constraint_Error);
         when Ada_Digits_Constraint =>
            return
              (case Field is when Digits_Constraint_F_Digits => 1,
                 when Digits_Constraint_F_Range              => 2,
                 when others => raise Constraint_Error);
         when Ada_Discriminant_Constraint =>
            return
              (case Field is when Discriminant_Constraint_F_Constraints => 1,
                 when others => raise Constraint_Error);
         when Ada_Index_Constraint =>
            return
              (case Field is when Index_Constraint_F_Constraints => 1,
                 when others => raise Constraint_Error);
         when Ada_Range_Constraint =>
            return
              (case Field is when Range_Constraint_F_Range => 1,
                 when others => raise Constraint_Error);
         when Ada_Declarative_Part =>
            return
              (case Field is when Declarative_Part_F_Decls => 1,
                 when others => raise Constraint_Error);
         when Ada_Private_Part =>
            return
              (case Field is when Declarative_Part_F_Decls => 1,
                 when others => raise Constraint_Error);
         when Ada_Public_Part =>
            return
              (case Field is when Declarative_Part_F_Decls => 1,
                 when others => raise Constraint_Error);
         when Ada_Elsif_Expr_Part =>
            return
              (case Field is when Elsif_Expr_Part_F_Cond_Expr => 1,
                 when Elsif_Expr_Part_F_Then_Expr             => 2,
                 when others => raise Constraint_Error);
         when Ada_Elsif_Stmt_Part =>
            return
              (case Field is when Elsif_Stmt_Part_F_Cond_Expr => 1,
                 when Elsif_Stmt_Part_F_Stmts                 => 2,
                 when others => raise Constraint_Error);
         when Ada_Allocator =>
            return
              (case Field is when Allocator_F_Subpool => 1,
                 when Allocator_F_Type_Or_Expr        => 2,
                 when others => raise Constraint_Error);
         when Ada_Aggregate =>
            return
              (case Field is when Base_Aggregate_F_Ancestor_Expr => 1,
                 when Base_Aggregate_F_Assocs                    => 2,
                 when others => raise Constraint_Error);
         when Ada_Null_Record_Aggregate =>
            return
              (case Field is when Base_Aggregate_F_Ancestor_Expr => 1,
                 when Base_Aggregate_F_Assocs                    => 2,
                 when others => raise Constraint_Error);
         when Ada_Bin_Op =>
            return
              (case Field is when Bin_Op_F_Left => 1, when Bin_Op_F_Op => 2,
                 when Bin_Op_F_Right            => 3,
                 when others                    => raise Constraint_Error);
         when Ada_Relation_Op =>
            return
              (case Field is when Bin_Op_F_Left => 1, when Bin_Op_F_Op => 2,
                 when Bin_Op_F_Right            => 3,
                 when others                    => raise Constraint_Error);
         when Ada_Box_Expr =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Case_Expr =>
            return
              (case Field is when Case_Expr_F_Expr => 1,
                 when Case_Expr_F_Cases            => 2,
                 when others                       => raise Constraint_Error);
         when Ada_Case_Expr_Alternative =>
            return
              (case Field is when Case_Expr_Alternative_F_Choices => 1,
                 when Case_Expr_Alternative_F_Expr                => 2,
                 when others => raise Constraint_Error);
         when Ada_Contract_Cases =>
            return
              (case Field is when Contract_Cases_F_Contract_Cases => 1,
                 when others => raise Constraint_Error);
         when Ada_If_Expr =>
            return
              (case Field is when If_Expr_F_Cond_Expr => 1,
                 when If_Expr_F_Then_Expr             => 2,
                 when If_Expr_F_Alternatives          => 3,
                 when If_Expr_F_Else_Expr             => 4,
                 when others => raise Constraint_Error);
         when Ada_Membership_Expr =>
            return
              (case Field is when Membership_Expr_F_Expr => 1,
                 when Membership_Expr_F_Op               => 2,
                 when Membership_Expr_F_Membership_Exprs => 3,
                 when others => raise Constraint_Error);
         when Ada_Attribute_Ref =>
            return
              (case Field is when Attribute_Ref_F_Prefix => 1,
                 when Attribute_Ref_F_Attribute          => 2,
                 when Attribute_Ref_F_Args               => 3,
                 when others => raise Constraint_Error);
         when Ada_Update_Attribute_Ref =>
            return
              (case Field is when Attribute_Ref_F_Prefix => 1,
                 when Attribute_Ref_F_Attribute          => 2,
                 when Attribute_Ref_F_Args               => 3,
                 when others => raise Constraint_Error);
         when Ada_Call_Expr =>
            return
              (case Field is when Call_Expr_F_Name => 1,
                 when Call_Expr_F_Suffix           => 2,
                 when others                       => raise Constraint_Error);
         when Ada_Defining_Name =>
            return
              (case Field is when Defining_Name_F_Name => 1,
                 when others => raise Constraint_Error);
         when Ada_Discrete_Subtype_Name =>
            return
              (case Field is when Discrete_Subtype_Name_F_Subtype => 1,
                 when others => raise Constraint_Error);
         when Ada_Dotted_Name =>
            return
              (case Field is when Dotted_Name_F_Prefix => 1,
                 when Dotted_Name_F_Suffix             => 2,
                 when others => raise Constraint_Error);
         when Ada_End_Name =>
            return
              (case Field is when End_Name_F_Name => 1,
                 when others                      => raise Constraint_Error);
         when Ada_Explicit_Deref =>
            return
              (case Field is when Explicit_Deref_F_Prefix => 1,
                 when others => raise Constraint_Error);
         when Ada_Qual_Expr =>
            return
              (case Field is when Qual_Expr_F_Prefix => 1,
                 when Qual_Expr_F_Suffix             => 2,
                 when others => raise Constraint_Error);
         when Ada_Char_Literal =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Identifier =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Abs =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_And =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_And_Then =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Concat =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Div =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Double_Dot =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Eq =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Gt =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Gte =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_In =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Lt =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Lte =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Minus =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Mod =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Mult =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Neq =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Not =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Not_In =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Or =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Or_Else =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Plus =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Pow =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Rem =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Op_Xor =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_String_Literal =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Null_Literal =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Int_Literal =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Real_Literal =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Target_Name =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Paren_Expr =>
            return
              (case Field is when Paren_Expr_F_Expr => 1,
                 when others                        => raise Constraint_Error);
         when Ada_Quantified_Expr =>
            return
              (case Field is when Quantified_Expr_F_Quantifier => 1,
                 when Quantified_Expr_F_Loop_Spec              => 2,
                 when Quantified_Expr_F_Expr                   => 3,
                 when others => raise Constraint_Error);
         when Ada_Raise_Expr =>
            return
              (case Field is when Raise_Expr_F_Exception_Name => 1,
                 when Raise_Expr_F_Error_Message              => 2,
                 when others => raise Constraint_Error);
         when Ada_Un_Op =>
            return
              (case Field is when Un_Op_F_Op => 1, when Un_Op_F_Expr => 2,
                 when others                 => raise Constraint_Error);
         when Ada_Handled_Stmts =>
            return
              (case Field is when Handled_Stmts_F_Stmts => 1,
                 when Handled_Stmts_F_Exceptions        => 2,
                 when others => raise Constraint_Error);
         when Ada_Interface_Kind_Limited =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Interface_Kind_Protected =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Interface_Kind_Synchronized =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Interface_Kind_Task =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Iter_Type_In =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Iter_Type_Of =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Library_Item =>
            return
              (case Field is when Library_Item_F_Has_Private => 1,
                 when Library_Item_F_Item                    => 2,
                 when others => raise Constraint_Error);
         when Ada_Limited_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Limited_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_For_Loop_Spec =>
            return
              (case Field is when For_Loop_Spec_F_Var_Decl => 1,
                 when For_Loop_Spec_F_Loop_Type            => 2,
                 when For_Loop_Spec_F_Has_Reverse          => 3,
                 when For_Loop_Spec_F_Iter_Expr            => 4,
                 when others => raise Constraint_Error);
         when Ada_While_Loop_Spec =>
            return
              (case Field is when While_Loop_Spec_F_Expr => 1,
                 when others => raise Constraint_Error);
         when Ada_Mode_Default =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Mode_In =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Mode_In_Out =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Mode_Out =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Not_Null_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Not_Null_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Null_Component_Decl =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Others_Designator =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Overriding_Not_Overriding =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Overriding_Overriding =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Overriding_Unspecified =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Params =>
            return
              (case Field is when Params_F_Params => 1,
                 when others                      => raise Constraint_Error);
         when Ada_Pragma_Node =>
            return
              (case Field is when Pragma_Node_F_Id => 1,
                 when Pragma_Node_F_Args           => 2,
                 when others                       => raise Constraint_Error);
         when Ada_Prim_Type_Accessor =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Private_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Private_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Protected_Def =>
            return
              (case Field is when Protected_Def_F_Public_Part => 1,
                 when Protected_Def_F_Private_Part            => 2,
                 when Protected_Def_F_End_Name                => 3,
                 when others => raise Constraint_Error);
         when Ada_Protected_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Protected_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Quantifier_All =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Quantifier_Some =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Range_Spec =>
            return
              (case Field is when Range_Spec_F_Range => 1,
                 when others => raise Constraint_Error);
         when Ada_Renaming_Clause =>
            return
              (case Field is when Renaming_Clause_F_Renamed_Object => 1,
                 when others => raise Constraint_Error);
         when Ada_Synthetic_Renaming_Clause =>
            return
              (case Field is when Renaming_Clause_F_Renamed_Object => 1,
                 when others => raise Constraint_Error);
         when Ada_Reverse_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Reverse_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Select_When_Part =>
            return
              (case Field is when Select_When_Part_F_Cond_Expr => 1,
                 when Select_When_Part_F_Stmts                 => 2,
                 when others => raise Constraint_Error);
         when Ada_Accept_Stmt =>
            return
              (case Field is when Accept_Stmt_F_Name => 1,
                 when Accept_Stmt_F_Entry_Index_Expr => 2,
                 when Accept_Stmt_F_Params           => 3,
                 when others => raise Constraint_Error);
         when Ada_Accept_Stmt_With_Stmts =>
            return
              (case Field is when Accept_Stmt_F_Name    => 1,
                 when Accept_Stmt_F_Entry_Index_Expr    => 2,
                 when Accept_Stmt_F_Params              => 3,
                 when Accept_Stmt_With_Stmts_F_Stmts    => 4,
                 when Accept_Stmt_With_Stmts_F_End_Name => 5,
                 when others => raise Constraint_Error);
         when Ada_For_Loop_Stmt =>
            return
              (case Field is when Base_Loop_Stmt_F_Spec => 1,
                 when Base_Loop_Stmt_F_Stmts            => 2,
                 when Base_Loop_Stmt_F_End_Name         => 3,
                 when others => raise Constraint_Error);
         when Ada_Loop_Stmt =>
            return
              (case Field is when Base_Loop_Stmt_F_Spec => 1,
                 when Base_Loop_Stmt_F_Stmts            => 2,
                 when Base_Loop_Stmt_F_End_Name         => 3,
                 when others => raise Constraint_Error);
         when Ada_While_Loop_Stmt =>
            return
              (case Field is when Base_Loop_Stmt_F_Spec => 1,
                 when Base_Loop_Stmt_F_Stmts            => 2,
                 when Base_Loop_Stmt_F_End_Name         => 3,
                 when others => raise Constraint_Error);
         when Ada_Begin_Block =>
            return
              (case Field is when Begin_Block_F_Stmts => 1,
                 when Begin_Block_F_End_Name          => 2,
                 when others => raise Constraint_Error);
         when Ada_Decl_Block =>
            return
              (case Field is when Decl_Block_F_Decls => 1,
                 when Decl_Block_F_Stmts => 2, when Decl_Block_F_End_Name => 3,
                 when others => raise Constraint_Error);
         when Ada_Case_Stmt =>
            return
              (case Field is when Case_Stmt_F_Expr => 1,
                 when Case_Stmt_F_Alternatives     => 2,
                 when others                       => raise Constraint_Error);
         when Ada_Extended_Return_Stmt =>
            return
              (case Field is when Extended_Return_Stmt_F_Decl => 1,
                 when Extended_Return_Stmt_F_Stmts            => 2,
                 when others => raise Constraint_Error);
         when Ada_If_Stmt =>
            return
              (case Field is when If_Stmt_F_Cond_Expr => 1,
                 when If_Stmt_F_Then_Stmts            => 2,
                 when If_Stmt_F_Alternatives          => 3,
                 when If_Stmt_F_Else_Stmts            => 4,
                 when others => raise Constraint_Error);
         when Ada_Named_Stmt =>
            return
              (case Field is when Named_Stmt_F_Decl => 1,
                 when Named_Stmt_F_Stmt             => 2,
                 when others                        => raise Constraint_Error);
         when Ada_Select_Stmt =>
            return
              (case Field is when Select_Stmt_F_Guards => 1,
                 when Select_Stmt_F_Else_Stmts         => 2,
                 when Select_Stmt_F_Abort_Stmts        => 3,
                 when others => raise Constraint_Error);
         when Ada_Error_Stmt =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Abort_Stmt =>
            return
              (case Field is when Abort_Stmt_F_Names => 1,
                 when others => raise Constraint_Error);
         when Ada_Assign_Stmt =>
            return
              (case Field is when Assign_Stmt_F_Dest => 1,
                 when Assign_Stmt_F_Expr             => 2,
                 when others => raise Constraint_Error);
         when Ada_Call_Stmt =>
            return
              (case Field is when Call_Stmt_F_Call => 1,
                 when others                       => raise Constraint_Error);
         when Ada_Delay_Stmt =>
            return
              (case Field is when Delay_Stmt_F_Has_Until => 1,
                 when Delay_Stmt_F_Expr                  => 2,
                 when others => raise Constraint_Error);
         when Ada_Exit_Stmt =>
            return
              (case Field is when Exit_Stmt_F_Loop_Name => 1,
                 when Exit_Stmt_F_Cond_Expr             => 2,
                 when others => raise Constraint_Error);
         when Ada_Goto_Stmt =>
            return
              (case Field is when Goto_Stmt_F_Label_Name => 1,
                 when others => raise Constraint_Error);
         when Ada_Label =>
            return
              (case Field is when Label_F_Decl => 1,
                 when others                   => raise Constraint_Error);
         when Ada_Null_Stmt =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Raise_Stmt =>
            return
              (case Field is when Raise_Stmt_F_Exception_Name => 1,
                 when Raise_Stmt_F_Error_Message              => 2,
                 when others => raise Constraint_Error);
         when Ada_Requeue_Stmt =>
            return
              (case Field is when Requeue_Stmt_F_Call_Name => 1,
                 when Requeue_Stmt_F_Has_Abort             => 2,
                 when others => raise Constraint_Error);
         when Ada_Return_Stmt =>
            return
              (case Field is when Return_Stmt_F_Return_Expr => 1,
                 when others => raise Constraint_Error);
         when Ada_Terminate_Alternative =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Subp_Kind_Function =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Subp_Kind_Procedure =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Subunit =>
            return
              (case Field is when Subunit_F_Name => 1,
                 when Subunit_F_Body             => 2,
                 when others                     => raise Constraint_Error);
         when Ada_Synchronized_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Synchronized_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Tagged_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Tagged_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Task_Def =>
            return
              (case Field is when Task_Def_F_Interfaces => 1,
                 when Task_Def_F_Public_Part            => 2,
                 when Task_Def_F_Private_Part           => 3,
                 when Task_Def_F_End_Name               => 4,
                 when others => raise Constraint_Error);
         when Ada_Access_To_Subp_Def =>
            return
              (case Field is when Access_Def_F_Has_Not_Null => 1,
                 when Access_To_Subp_Def_F_Has_Protected    => 2,
                 when Access_To_Subp_Def_F_Subp_Spec        => 3,
                 when others => raise Constraint_Error);
         when Ada_Anonymous_Type_Access_Def =>
            return
              (case Field is when Access_Def_F_Has_Not_Null => 1,
                 when Anonymous_Type_Access_Def_F_Type_Decl => 2,
                 when others => raise Constraint_Error);
         when Ada_Type_Access_Def =>
            return
              (case Field is when Access_Def_F_Has_Not_Null => 1,
                 when Type_Access_Def_F_Has_All             => 2,
                 when Type_Access_Def_F_Has_Constant        => 3,
                 when Type_Access_Def_F_Subtype_Indication  => 4,
                 when others => raise Constraint_Error);
         when Ada_Array_Type_Def =>
            return
              (case Field is when Array_Type_Def_F_Indices => 1,
                 when Array_Type_Def_F_Component_Type      => 2,
                 when others => raise Constraint_Error);
         when Ada_Derived_Type_Def =>
            return
              (case Field is when Derived_Type_Def_F_Has_Abstract => 1,
                 when Derived_Type_Def_F_Has_Limited              => 2,
                 when Derived_Type_Def_F_Has_Synchronized         => 3,
                 when Derived_Type_Def_F_Subtype_Indication       => 4,
                 when Derived_Type_Def_F_Interfaces               => 5,
                 when Derived_Type_Def_F_Record_Extension         => 6,
                 when Derived_Type_Def_F_Has_With_Private         => 7,
                 when others => raise Constraint_Error);
         when Ada_Enum_Type_Def =>
            return
              (case Field is when Enum_Type_Def_F_Enum_Literals => 1,
                 when others => raise Constraint_Error);
         when Ada_Formal_Discrete_Type_Def =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Interface_Type_Def =>
            return
              (case Field is when Interface_Type_Def_F_Interface_Kind => 1,
                 when Interface_Type_Def_F_Interfaces                 => 2,
                 when others => raise Constraint_Error);
         when Ada_Mod_Int_Type_Def =>
            return
              (case Field is when Mod_Int_Type_Def_F_Expr => 1,
                 when others => raise Constraint_Error);
         when Ada_Private_Type_Def =>
            return
              (case Field is when Private_Type_Def_F_Has_Abstract => 1,
                 when Private_Type_Def_F_Has_Tagged               => 2,
                 when Private_Type_Def_F_Has_Limited              => 3,
                 when others => raise Constraint_Error);
         when Ada_Decimal_Fixed_Point_Def =>
            return
              (case Field is when Decimal_Fixed_Point_Def_F_Delta => 1,
                 when Decimal_Fixed_Point_Def_F_Digits            => 2,
                 when Decimal_Fixed_Point_Def_F_Range             => 3,
                 when others => raise Constraint_Error);
         when Ada_Floating_Point_Def =>
            return
              (case Field is when Floating_Point_Def_F_Num_Digits => 1,
                 when Floating_Point_Def_F_Range                  => 2,
                 when others => raise Constraint_Error);
         when Ada_Ordinary_Fixed_Point_Def =>
            return
              (case Field is when Ordinary_Fixed_Point_Def_F_Delta => 1,
                 when Ordinary_Fixed_Point_Def_F_Range             => 2,
                 when others => raise Constraint_Error);
         when Ada_Record_Type_Def =>
            return
              (case Field is when Record_Type_Def_F_Has_Abstract => 1,
                 when Record_Type_Def_F_Has_Tagged               => 2,
                 when Record_Type_Def_F_Has_Limited              => 3,
                 when Record_Type_Def_F_Record_Def               => 4,
                 when others => raise Constraint_Error);
         when Ada_Signed_Int_Type_Def =>
            return
              (case Field is when Signed_Int_Type_Def_F_Range => 1,
                 when others => raise Constraint_Error);
         when Ada_Anonymous_Type =>
            return
              (case Field is when Anonymous_Type_F_Type_Decl => 1,
                 when others => raise Constraint_Error);
         when Ada_Enum_Lit_Synth_Type_Expr =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Subtype_Indication =>
            return
              (case Field is when Subtype_Indication_F_Has_Not_Null => 1,
                 when Subtype_Indication_F_Name                     => 2,
                 when Subtype_Indication_F_Constraint               => 3,
                 when others => raise Constraint_Error);
         when Ada_Constrained_Subtype_Indication =>
            return
              (case Field is when Subtype_Indication_F_Has_Not_Null => 1,
                 when Subtype_Indication_F_Name                     => 2,
                 when Subtype_Indication_F_Constraint               => 3,
                 when others => raise Constraint_Error);
         when Ada_Discrete_Subtype_Indication =>
            return
              (case Field is when Subtype_Indication_F_Has_Not_Null => 1,
                 when Subtype_Indication_F_Name                     => 2,
                 when Subtype_Indication_F_Constraint               => 3,
                 when others => raise Constraint_Error);
         when Ada_Unconstrained_Array_Index =>
            return
              (case Field is
                 when Unconstrained_Array_Index_F_Subtype_Indication => 1,
                 when others => raise Constraint_Error);
         when Ada_Until_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Until_Present =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_Use_Package_Clause =>
            return
              (case Field is when Use_Package_Clause_F_Packages => 1,
                 when others => raise Constraint_Error);
         when Ada_Use_Type_Clause =>
            return
              (case Field is when Use_Type_Clause_F_Has_All => 1,
                 when Use_Type_Clause_F_Types               => 2,
                 when others => raise Constraint_Error);
         when Ada_Variant =>
            return
              (case Field is when Variant_F_Choices => 1,
                 when Variant_F_Components          => 2,
                 when others                        => raise Constraint_Error);
         when Ada_Variant_Part =>
            return
              (case Field is when Variant_Part_F_Discr_Name => 1,
                 when Variant_Part_F_Variant                => 2,
                 when others => raise Constraint_Error);
         when Ada_With_Clause =>
            return
              (case Field is when With_Clause_F_Has_Limited => 1,
                 when With_Clause_F_Has_Private             => 2,
                 when With_Clause_F_Packages                => 3,
                 when others => raise Constraint_Error);
         when Ada_With_Private_Absent =>
            return (case Field is when others => raise Constraint_Error);
         when Ada_With_Private_Present =>
            return (case Field is when others => raise Constraint_Error);
      end case;

   end Index;

   --------------------------------
   -- Field_Reference_From_Index --
   --------------------------------

   function Field_Reference_From_Index
     (Kind : Ada_Node_Kind_Type; Index : Positive) return Field_Reference
   is
   begin

      case Ada_Ada_Node (Kind) is
         when Ada_Ada_List =>
            raise Invalid_Field with "List AST nodes have no field";
         when Ada_Protected_Body_Stub_Range =>
            case Index is
               when 1 =>
                  return Protected_Body_Stub_F_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Subp_Body_Stub_Range =>
            case Index is
               when 1 =>
                  return Subp_Body_Stub_F_Overriding;
               when 2 =>
                  return Subp_Body_Stub_F_Subp_Spec;
               when 3 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Package_Body_Stub_Range =>
            case Index is
               when 1 =>
                  return Package_Body_Stub_F_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Task_Body_Stub_Range =>
            case Index is
               when 1 =>
                  return Task_Body_Stub_F_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Base_Subp_Body =>
            case Index is
               when 1 =>
                  return Base_Subp_Body_F_Overriding;
               when 2 =>
                  return Base_Subp_Body_F_Subp_Spec;
               when others =>
                  null;
            end case;
            case Ada_Base_Subp_Body (Kind) is
               when Ada_Expr_Function_Range =>
                  case Index is
                     when 3 =>
                        return Expr_Function_F_Expr;
                     when 4 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Null_Subp_Decl_Range =>
                  case Index is
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Subp_Renaming_Decl_Range =>
                  case Index is
                     when 3 =>
                        return Subp_Renaming_Decl_F_Renames;
                     when 4 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Subp_Body_Range =>
                  case Index is
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when 4 =>
                        return Subp_Body_F_Decls;
                     when 5 =>
                        return Subp_Body_F_Stmts;
                     when 6 =>
                        return Subp_Body_F_End_Name;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Ada_Package_Body_Range =>
            case Index is
               when 1 =>
                  return Package_Body_F_Package_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when 3 =>
                  return Package_Body_F_Decls;
               when 4 =>
                  return Package_Body_F_Stmts;
               when 5 =>
                  return Package_Body_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Task_Body_Range =>
            case Index is
               when 1 =>
                  return Task_Body_F_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when 3 =>
                  return Task_Body_F_Decls;
               when 4 =>
                  return Task_Body_F_Stmts;
               when 5 =>
                  return Task_Body_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Protected_Body_Range =>
            case Index is
               when 1 =>
                  return Protected_Body_F_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when 3 =>
                  return Protected_Body_F_Decls;
               when 4 =>
                  return Protected_Body_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Entry_Body_Range =>
            case Index is
               when 1 =>
                  return Entry_Body_F_Entry_Name;
               when 2 =>
                  return Entry_Body_F_Index_Spec;
               when 3 =>
                  return Entry_Body_F_Params;
               when 4 =>
                  return Entry_Body_F_Barrier;
               when 5 =>
                  return Entry_Body_F_Decls;
               when 6 =>
                  return Entry_Body_F_Stmts;
               when 7 =>
                  return Entry_Body_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Discriminant_Spec_Range =>
            case Index is
               when 1 =>
                  return Discriminant_Spec_F_Ids;
               when 2 =>
                  return Discriminant_Spec_F_Type_Expr;
               when 3 =>
                  return Discriminant_Spec_F_Default_Expr;
               when others =>
                  null;
            end case;
         when Ada_Component_Decl_Range =>
            case Index is
               when 1 =>
                  return Component_Decl_F_Ids;
               when 2 =>
                  return Component_Decl_F_Component_Def;
               when 3 =>
                  return Component_Decl_F_Default_Expr;
               when 4 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Param_Spec_Range =>
            case Index is
               when 1 =>
                  return Param_Spec_F_Ids;
               when 2 =>
                  return Param_Spec_F_Has_Aliased;
               when 3 =>
                  return Param_Spec_F_Mode;
               when 4 =>
                  return Param_Spec_F_Type_Expr;
               when 5 =>
                  return Param_Spec_F_Default_Expr;
               when others =>
                  null;
            end case;
         when Ada_Generic_Formal =>
            case Index is
               when 1 =>
                  return Generic_Formal_F_Decl;
               when others =>
                  null;
            end case;
         when Ada_Base_Type_Decl =>
            case Index is
               when 1 =>
                  return Base_Type_Decl_F_Name;
               when others =>
                  null;
            end case;
            case Ada_Base_Type_Decl (Kind) is
               when Ada_Type_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Type_Decl_F_Discriminants;
                     when 3 =>
                        return Type_Decl_F_Type_Def;
                     when 4 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Subtype_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Subtype_Decl_F_Subtype;
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Task_Type_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Task_Type_Decl_F_Discriminants;
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when 4 =>
                        return Task_Type_Decl_F_Definition;
                     when others =>
                        null;
                  end case;
               when Ada_Protected_Type_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Protected_Type_Decl_F_Discriminants;
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when 4 =>
                        return Protected_Type_Decl_F_Interfaces;
                     when 5 =>
                        return Protected_Type_Decl_F_Definition;
                     when others =>
                        null;
                  end case;
               when Ada_Incomplete_Type_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Incomplete_Type_Decl_F_Discriminants;
                     when others =>
                        null;
                  end case;
                  case Ada_Incomplete_Type_Decl_Range (Kind) is
                     when Ada_Incomplete_Tagged_Type_Decl_Range =>
                        case Index is
                           when 3 =>
                              return Incomplete_Tagged_Type_Decl_F_Has_Abstract;
                           when others =>
                              null;
                        end case;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Ada_Classic_Subp_Decl =>
            case Index is
               when 1 =>
                  return Classic_Subp_Decl_F_Overriding;
               when 2 =>
                  return Classic_Subp_Decl_F_Subp_Spec;
               when others =>
                  null;
            end case;
            case Ada_Classic_Subp_Decl (Kind) is
               when Ada_Subp_Decl_Range =>
                  case Index is
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Abstract_Subp_Decl_Range =>
                  case Index is
                     when 3 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when Ada_Formal_Subp_Decl =>
                  case Index is
                     when 3 =>
                        return Formal_Subp_Decl_F_Default_Expr;
                     when 4 =>
                        return Basic_Decl_F_Aspects;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Ada_Generic_Subp_Internal_Range =>
            case Index is
               when 1 =>
                  return Generic_Subp_Internal_F_Subp_Spec;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Enum_Literal_Decl_Range =>
            case Index is
               when 1 =>
                  return Enum_Literal_Decl_F_Name;
               when others =>
                  null;
            end case;
         when Ada_Entry_Decl_Range =>
            case Index is
               when 1 =>
                  return Entry_Decl_F_Overriding;
               when 2 =>
                  return Entry_Decl_F_Spec;
               when 3 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Single_Task_Decl_Range =>
            case Index is
               when 1 =>
                  return Single_Task_Decl_F_Task_Type;
               when others =>
                  null;
            end case;
         when Ada_Single_Protected_Decl_Range =>
            case Index is
               when 1 =>
                  return Single_Protected_Decl_F_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when 3 =>
                  return Single_Protected_Decl_F_Interfaces;
               when 4 =>
                  return Single_Protected_Decl_F_Definition;
               when others =>
                  null;
            end case;
         when Ada_Number_Decl_Range =>
            case Index is
               when 1 =>
                  return Number_Decl_F_Ids;
               when 2 =>
                  return Number_Decl_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Object_Decl_Range =>
            case Index is
               when 1 =>
                  return Object_Decl_F_Ids;
               when 2 =>
                  return Object_Decl_F_Has_Aliased;
               when 3 =>
                  return Object_Decl_F_Has_Constant;
               when 4 =>
                  return Object_Decl_F_Mode;
               when 5 =>
                  return Object_Decl_F_Type_Expr;
               when 6 =>
                  return Object_Decl_F_Default_Expr;
               when 7 =>
                  return Object_Decl_F_Renaming_Clause;
               when 8 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Base_Package_Decl =>
            case Index is
               when 1 =>
                  return Base_Package_Decl_F_Package_Name;
               when 2 =>
                  return Basic_Decl_F_Aspects;
               when 3 =>
                  return Base_Package_Decl_F_Public_Part;
               when 4 =>
                  return Base_Package_Decl_F_Private_Part;
               when 5 =>
                  return Base_Package_Decl_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Exception_Decl_Range =>
            case Index is
               when 1 =>
                  return Exception_Decl_F_Ids;
               when 2 =>
                  return Exception_Decl_F_Renames;
               when 3 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Generic_Subp_Instantiation_Range =>
            case Index is
               when 1 =>
                  return Generic_Subp_Instantiation_F_Overriding;
               when 2 =>
                  return Generic_Subp_Instantiation_F_Kind;
               when 3 =>
                  return Generic_Subp_Instantiation_F_Subp_Name;
               when 4 =>
                  return Generic_Subp_Instantiation_F_Generic_Subp_Name;
               when 5 =>
                  return Generic_Subp_Instantiation_F_Params;
               when 6 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Generic_Package_Instantiation_Range =>
            case Index is
               when 1 =>
                  return Generic_Package_Instantiation_F_Name;
               when 2 =>
                  return Generic_Package_Instantiation_F_Generic_Pkg_Name;
               when 3 =>
                  return Generic_Package_Instantiation_F_Params;
               when 4 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Package_Renaming_Decl_Range =>
            case Index is
               when 1 =>
                  return Package_Renaming_Decl_F_Name;
               when 2 =>
                  return Package_Renaming_Decl_F_Renames;
               when 3 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Generic_Package_Renaming_Decl_Range =>
            case Index is
               when 1 =>
                  return Generic_Package_Renaming_Decl_F_Name;
               when 2 =>
                  return Generic_Package_Renaming_Decl_F_Renames;
               when 3 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Generic_Subp_Renaming_Decl_Range =>
            case Index is
               when 1 =>
                  return Generic_Subp_Renaming_Decl_F_Kind;
               when 2 =>
                  return Generic_Subp_Renaming_Decl_F_Name;
               when 3 =>
                  return Generic_Subp_Renaming_Decl_F_Renames;
               when 4 =>
                  return Basic_Decl_F_Aspects;
               when others =>
                  null;
            end case;
         when Ada_Generic_Decl =>
            case Index is
               when 1 =>
                  return Generic_Decl_F_Formal_Part;
               when others =>
                  null;
            end case;
            case Ada_Generic_Decl (Kind) is
               when Ada_Generic_Subp_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Generic_Subp_Decl_F_Subp_Decl;
                     when others =>
                        null;
                  end case;
               when Ada_Generic_Package_Decl_Range =>
                  case Index is
                     when 2 =>
                        return Generic_Package_Decl_F_Package_Decl;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Ada_For_Loop_Var_Decl_Range =>
            case Index is
               when 1 =>
                  return For_Loop_Var_Decl_F_Id;
               when 2 =>
                  return For_Loop_Var_Decl_F_Id_Type;
               when others =>
                  null;
            end case;
         when Ada_Exception_Handler_Range =>
            case Index is
               when 1 =>
                  return Exception_Handler_F_Exception_Name;
               when 2 =>
                  return Exception_Handler_F_Handled_Exceptions;
               when 3 =>
                  return Exception_Handler_F_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Label_Decl_Range =>
            case Index is
               when 1 =>
                  return Label_Decl_F_Name;
               when others =>
                  null;
            end case;
         when Ada_Named_Stmt_Decl_Range =>
            case Index is
               when 1 =>
                  return Named_Stmt_Decl_F_Name;
               when others =>
                  null;
            end case;
         when Ada_Entry_Index_Spec_Range =>
            case Index is
               when 1 =>
                  return Entry_Index_Spec_F_Id;
               when 2 =>
                  return Entry_Index_Spec_F_Subtype;
               when others =>
                  null;
            end case;
         when Ada_Known_Discriminant_Part_Range =>
            case Index is
               when 1 =>
                  return Known_Discriminant_Part_F_Discr_Specs;
               when others =>
                  null;
            end case;
         when Ada_Component_List_Range =>
            case Index is
               when 1 =>
                  return Component_List_F_Components;
               when 2 =>
                  return Component_List_F_Variant_Part;
               when others =>
                  null;
            end case;
         when Ada_Generic_Formal_Part_Range =>
            case Index is
               when 1 =>
                  return Generic_Formal_Part_F_Decls;
               when others =>
                  null;
            end case;
         when Ada_Subp_Spec_Range =>
            case Index is
               when 1 =>
                  return Subp_Spec_F_Subp_Kind;
               when 2 =>
                  return Subp_Spec_F_Subp_Name;
               when 3 =>
                  return Subp_Spec_F_Subp_Params;
               when 4 =>
                  return Subp_Spec_F_Subp_Returns;
               when others =>
                  null;
            end case;
         when Ada_Entry_Spec_Range =>
            case Index is
               when 1 =>
                  return Entry_Spec_F_Entry_Name;
               when 2 =>
                  return Entry_Spec_F_Family_Type;
               when 3 =>
                  return Entry_Spec_F_Entry_Params;
               when others =>
                  null;
            end case;
         when Ada_Entry_Completion_Formal_Params_Range =>
            case Index is
               when 1 =>
                  return Entry_Completion_Formal_Params_F_Params;
               when others =>
                  null;
            end case;
         when Ada_Record_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Record_Type_Def_F_Has_Abstract;
               when 2 =>
                  return Record_Type_Def_F_Has_Tagged;
               when 3 =>
                  return Record_Type_Def_F_Has_Limited;
               when 4 =>
                  return Record_Type_Def_F_Record_Def;
               when others =>
                  null;
            end case;
         when Ada_Floating_Point_Def_Range =>
            case Index is
               when 1 =>
                  return Floating_Point_Def_F_Num_Digits;
               when 2 =>
                  return Floating_Point_Def_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Ordinary_Fixed_Point_Def_Range =>
            case Index is
               when 1 =>
                  return Ordinary_Fixed_Point_Def_F_Delta;
               when 2 =>
                  return Ordinary_Fixed_Point_Def_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Decimal_Fixed_Point_Def_Range =>
            case Index is
               when 1 =>
                  return Decimal_Fixed_Point_Def_F_Delta;
               when 2 =>
                  return Decimal_Fixed_Point_Def_F_Digits;
               when 3 =>
                  return Decimal_Fixed_Point_Def_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Enum_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Enum_Type_Def_F_Enum_Literals;
               when others =>
                  null;
            end case;
         when Ada_Derived_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Derived_Type_Def_F_Has_Abstract;
               when 2 =>
                  return Derived_Type_Def_F_Has_Limited;
               when 3 =>
                  return Derived_Type_Def_F_Has_Synchronized;
               when 4 =>
                  return Derived_Type_Def_F_Subtype_Indication;
               when 5 =>
                  return Derived_Type_Def_F_Interfaces;
               when 6 =>
                  return Derived_Type_Def_F_Record_Extension;
               when 7 =>
                  return Derived_Type_Def_F_Has_With_Private;
               when others =>
                  null;
            end case;
         when Ada_Private_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Private_Type_Def_F_Has_Abstract;
               when 2 =>
                  return Private_Type_Def_F_Has_Tagged;
               when 3 =>
                  return Private_Type_Def_F_Has_Limited;
               when others =>
                  null;
            end case;
         when Ada_Signed_Int_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Signed_Int_Type_Def_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Mod_Int_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Mod_Int_Type_Def_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Array_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Array_Type_Def_F_Indices;
               when 2 =>
                  return Array_Type_Def_F_Component_Type;
               when others =>
                  null;
            end case;
         when Ada_Interface_Type_Def_Range =>
            case Index is
               when 1 =>
                  return Interface_Type_Def_F_Interface_Kind;
               when 2 =>
                  return Interface_Type_Def_F_Interfaces;
               when others =>
                  null;
            end case;
         when Ada_Access_Def =>
            case Index is
               when 1 =>
                  return Access_Def_F_Has_Not_Null;
               when others =>
                  null;
            end case;
            case Ada_Access_Def (Kind) is
               when Ada_Access_To_Subp_Def_Range =>
                  case Index is
                     when 2 =>
                        return Access_To_Subp_Def_F_Has_Protected;
                     when 3 =>
                        return Access_To_Subp_Def_F_Subp_Spec;
                     when others =>
                        null;
                  end case;
               when Ada_Type_Access_Def_Range =>
                  case Index is
                     when 2 =>
                        return Type_Access_Def_F_Has_All;
                     when 3 =>
                        return Type_Access_Def_F_Has_Constant;
                     when 4 =>
                        return Type_Access_Def_F_Subtype_Indication;
                     when others =>
                        null;
                  end case;
               when Ada_Anonymous_Type_Access_Def_Range =>
                  case Index is
                     when 2 =>
                        return Anonymous_Type_Access_Def_F_Type_Decl;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Ada_Variant_Range =>
            case Index is
               when 1 =>
                  return Variant_F_Choices;
               when 2 =>
                  return Variant_F_Components;
               when others =>
                  null;
            end case;
         when Ada_Variant_Part_Range =>
            case Index is
               when 1 =>
                  return Variant_Part_F_Discr_Name;
               when 2 =>
                  return Variant_Part_F_Variant;
               when others =>
                  null;
            end case;
         when Ada_Base_Record_Def =>
            case Index is
               when 1 =>
                  return Base_Record_Def_F_Components;
               when others =>
                  null;
            end case;
         when Ada_Pragma_Argument_Assoc_Range =>
            case Index is
               when 1 =>
                  return Pragma_Argument_Assoc_F_Id;
               when 2 =>
                  return Pragma_Argument_Assoc_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Contract_Case_Assoc_Range =>
            case Index is
               when 1 =>
                  return Contract_Case_Assoc_F_Guard;
               when 2 =>
                  return Contract_Case_Assoc_F_Consequence;
               when others =>
                  null;
            end case;
         when Ada_Range_Constraint_Range =>
            case Index is
               when 1 =>
                  return Range_Constraint_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Digits_Constraint_Range =>
            case Index is
               when 1 =>
                  return Digits_Constraint_F_Digits;
               when 2 =>
                  return Digits_Constraint_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Delta_Constraint_Range =>
            case Index is
               when 1 =>
                  return Delta_Constraint_F_Digits;
               when 2 =>
                  return Delta_Constraint_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Index_Constraint_Range =>
            case Index is
               when 1 =>
                  return Index_Constraint_F_Constraints;
               when others =>
                  null;
            end case;
         when Ada_Discriminant_Constraint_Range =>
            case Index is
               when 1 =>
                  return Discriminant_Constraint_F_Constraints;
               when others =>
                  null;
            end case;
         when Ada_Discriminant_Assoc_Range =>
            case Index is
               when 1 =>
                  return Discriminant_Assoc_F_Ids;
               when 2 =>
                  return Discriminant_Assoc_F_Discr_Expr;
               when others =>
                  null;
            end case;
         when Ada_Param_Assoc_Range =>
            case Index is
               when 1 =>
                  return Param_Assoc_F_Designator;
               when 2 =>
                  return Param_Assoc_F_R_Expr;
               when others =>
                  null;
            end case;
         when Ada_Aggregate_Assoc_Range =>
            case Index is
               when 1 =>
                  return Aggregate_Assoc_F_Designators;
               when 2 =>
                  return Aggregate_Assoc_F_R_Expr;
               when others =>
                  null;
            end case;
         when Ada_Unconstrained_Array_Indices_Range =>
            case Index is
               when 1 =>
                  return Unconstrained_Array_Indices_F_Types;
               when others =>
                  null;
            end case;
         when Ada_Constrained_Array_Indices_Range =>
            case Index is
               when 1 =>
                  return Constrained_Array_Indices_F_List;
               when others =>
                  null;
            end case;
         when Ada_Component_Def_Range =>
            case Index is
               when 1 =>
                  return Component_Def_F_Has_Aliased;
               when 2 =>
                  return Component_Def_F_Has_Constant;
               when 3 =>
                  return Component_Def_F_Type_Expr;
               when others =>
                  null;
            end case;
         when Ada_Task_Def_Range =>
            case Index is
               when 1 =>
                  return Task_Def_F_Interfaces;
               when 2 =>
                  return Task_Def_F_Public_Part;
               when 3 =>
                  return Task_Def_F_Private_Part;
               when 4 =>
                  return Task_Def_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Protected_Def_Range =>
            case Index is
               when 1 =>
                  return Protected_Def_F_Public_Part;
               when 2 =>
                  return Protected_Def_F_Private_Part;
               when 3 =>
                  return Protected_Def_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_With_Clause_Range =>
            case Index is
               when 1 =>
                  return With_Clause_F_Has_Limited;
               when 2 =>
                  return With_Clause_F_Has_Private;
               when 3 =>
                  return With_Clause_F_Packages;
               when others =>
                  null;
            end case;
         when Ada_Use_Package_Clause_Range =>
            case Index is
               when 1 =>
                  return Use_Package_Clause_F_Packages;
               when others =>
                  null;
            end case;
         when Ada_Use_Type_Clause_Range =>
            case Index is
               when 1 =>
                  return Use_Type_Clause_F_Has_All;
               when 2 =>
                  return Use_Type_Clause_F_Types;
               when others =>
                  null;
            end case;
         when Ada_Anonymous_Type_Range =>
            case Index is
               when 1 =>
                  return Anonymous_Type_F_Type_Decl;
               when others =>
                  null;
            end case;
         when Ada_Subtype_Indication_Range =>
            case Index is
               when 1 =>
                  return Subtype_Indication_F_Has_Not_Null;
               when 2 =>
                  return Subtype_Indication_F_Name;
               when 3 =>
                  return Subtype_Indication_F_Constraint;
               when others =>
                  null;
            end case;
         when Ada_Aspect_Spec_Range =>
            case Index is
               when 1 =>
                  return Aspect_Spec_F_Aspect_Assocs;
               when others =>
                  null;
            end case;
         when Ada_Pragma_Node_Range =>
            case Index is
               when 1 =>
                  return Pragma_Node_F_Id;
               when 2 =>
                  return Pragma_Node_F_Args;
               when others =>
                  null;
            end case;
         when Ada_Enum_Rep_Clause_Range =>
            case Index is
               when 1 =>
                  return Enum_Rep_Clause_F_Type_Name;
               when 2 =>
                  return Enum_Rep_Clause_F_Aggregate;
               when others =>
                  null;
            end case;
         when Ada_Attribute_Def_Clause_Range =>
            case Index is
               when 1 =>
                  return Attribute_Def_Clause_F_Attribute_Expr;
               when 2 =>
                  return Attribute_Def_Clause_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Record_Rep_Clause_Range =>
            case Index is
               when 1 =>
                  return Record_Rep_Clause_F_Name;
               when 2 =>
                  return Record_Rep_Clause_F_At_Expr;
               when 3 =>
                  return Record_Rep_Clause_F_Components;
               when others =>
                  null;
            end case;
         when Ada_At_Clause_Range =>
            case Index is
               when 1 =>
                  return At_Clause_F_Name;
               when 2 =>
                  return At_Clause_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Component_Clause_Range =>
            case Index is
               when 1 =>
                  return Component_Clause_F_Id;
               when 2 =>
                  return Component_Clause_F_Position;
               when 3 =>
                  return Component_Clause_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Aspect_Assoc_Range =>
            case Index is
               when 1 =>
                  return Aspect_Assoc_F_Id;
               when 2 =>
                  return Aspect_Assoc_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Declarative_Part_Range =>
            case Index is
               when 1 =>
                  return Declarative_Part_F_Decls;
               when others =>
                  null;
            end case;
         when Ada_Renaming_Clause_Range =>
            case Index is
               when 1 =>
                  return Renaming_Clause_F_Renamed_Object;
               when others =>
                  null;
            end case;
         when Ada_Contract_Cases_Range =>
            case Index is
               when 1 =>
                  return Contract_Cases_F_Contract_Cases;
               when others =>
                  null;
            end case;
         when Ada_Paren_Expr_Range =>
            case Index is
               when 1 =>
                  return Paren_Expr_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Un_Op_Range =>
            case Index is
               when 1 =>
                  return Un_Op_F_Op;
               when 2 =>
                  return Un_Op_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Bin_Op_Range =>
            case Index is
               when 1 =>
                  return Bin_Op_F_Left;
               when 2 =>
                  return Bin_Op_F_Op;
               when 3 =>
                  return Bin_Op_F_Right;
               when others =>
                  null;
            end case;
         when Ada_Membership_Expr_Range =>
            case Index is
               when 1 =>
                  return Membership_Expr_F_Expr;
               when 2 =>
                  return Membership_Expr_F_Op;
               when 3 =>
                  return Membership_Expr_F_Membership_Exprs;
               when others =>
                  null;
            end case;
         when Ada_Base_Aggregate =>
            case Index is
               when 1 =>
                  return Base_Aggregate_F_Ancestor_Expr;
               when 2 =>
                  return Base_Aggregate_F_Assocs;
               when others =>
                  null;
            end case;
         when Ada_Discrete_Subtype_Name_Range =>
            case Index is
               when 1 =>
                  return Discrete_Subtype_Name_F_Subtype;
               when others =>
                  null;
            end case;
         when Ada_Call_Expr_Range =>
            case Index is
               when 1 =>
                  return Call_Expr_F_Name;
               when 2 =>
                  return Call_Expr_F_Suffix;
               when others =>
                  null;
            end case;
         when Ada_Explicit_Deref_Range =>
            case Index is
               when 1 =>
                  return Explicit_Deref_F_Prefix;
               when others =>
                  null;
            end case;
         when Ada_Defining_Name_Range =>
            case Index is
               when 1 =>
                  return Defining_Name_F_Name;
               when others =>
                  null;
            end case;
         when Ada_End_Name_Range =>
            case Index is
               when 1 =>
                  return End_Name_F_Name;
               when others =>
                  null;
            end case;
         when Ada_Qual_Expr_Range =>
            case Index is
               when 1 =>
                  return Qual_Expr_F_Prefix;
               when 2 =>
                  return Qual_Expr_F_Suffix;
               when others =>
                  null;
            end case;
         when Ada_Attribute_Ref_Range =>
            case Index is
               when 1 =>
                  return Attribute_Ref_F_Prefix;
               when 2 =>
                  return Attribute_Ref_F_Attribute;
               when 3 =>
                  return Attribute_Ref_F_Args;
               when others =>
                  null;
            end case;
         when Ada_Dotted_Name_Range =>
            case Index is
               when 1 =>
                  return Dotted_Name_F_Prefix;
               when 2 =>
                  return Dotted_Name_F_Suffix;
               when others =>
                  null;
            end case;
         when Ada_If_Expr_Range =>
            case Index is
               when 1 =>
                  return If_Expr_F_Cond_Expr;
               when 2 =>
                  return If_Expr_F_Then_Expr;
               when 3 =>
                  return If_Expr_F_Alternatives;
               when 4 =>
                  return If_Expr_F_Else_Expr;
               when others =>
                  null;
            end case;
         when Ada_Case_Expr_Range =>
            case Index is
               when 1 =>
                  return Case_Expr_F_Expr;
               when 2 =>
                  return Case_Expr_F_Cases;
               when others =>
                  null;
            end case;
         when Ada_Case_Expr_Alternative_Range =>
            case Index is
               when 1 =>
                  return Case_Expr_Alternative_F_Choices;
               when 2 =>
                  return Case_Expr_Alternative_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Quantified_Expr_Range =>
            case Index is
               when 1 =>
                  return Quantified_Expr_F_Quantifier;
               when 2 =>
                  return Quantified_Expr_F_Loop_Spec;
               when 3 =>
                  return Quantified_Expr_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Allocator_Range =>
            case Index is
               when 1 =>
                  return Allocator_F_Subpool;
               when 2 =>
                  return Allocator_F_Type_Or_Expr;
               when others =>
                  null;
            end case;
         when Ada_Raise_Expr_Range =>
            case Index is
               when 1 =>
                  return Raise_Expr_F_Exception_Name;
               when 2 =>
                  return Raise_Expr_F_Error_Message;
               when others =>
                  null;
            end case;
         when Ada_Elsif_Expr_Part_Range =>
            case Index is
               when 1 =>
                  return Elsif_Expr_Part_F_Cond_Expr;
               when 2 =>
                  return Elsif_Expr_Part_F_Then_Expr;
               when others =>
                  null;
            end case;
         when Ada_For_Loop_Spec_Range =>
            case Index is
               when 1 =>
                  return For_Loop_Spec_F_Var_Decl;
               when 2 =>
                  return For_Loop_Spec_F_Loop_Type;
               when 3 =>
                  return For_Loop_Spec_F_Has_Reverse;
               when 4 =>
                  return For_Loop_Spec_F_Iter_Expr;
               when others =>
                  null;
            end case;
         when Ada_While_Loop_Spec_Range =>
            case Index is
               when 1 =>
                  return While_Loop_Spec_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Compilation_Unit_Range =>
            case Index is
               when 1 =>
                  return Compilation_Unit_F_Prelude;
               when 2 =>
                  return Compilation_Unit_F_Body;
               when 3 =>
                  return Compilation_Unit_F_Pragmas;
               when others =>
                  null;
            end case;
         when Ada_Handled_Stmts_Range =>
            case Index is
               when 1 =>
                  return Handled_Stmts_F_Stmts;
               when 2 =>
                  return Handled_Stmts_F_Exceptions;
               when others =>
                  null;
            end case;
         when Ada_Call_Stmt_Range =>
            case Index is
               when 1 =>
                  return Call_Stmt_F_Call;
               when others =>
                  null;
            end case;
         when Ada_Assign_Stmt_Range =>
            case Index is
               when 1 =>
                  return Assign_Stmt_F_Dest;
               when 2 =>
                  return Assign_Stmt_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Goto_Stmt_Range =>
            case Index is
               when 1 =>
                  return Goto_Stmt_F_Label_Name;
               when others =>
                  null;
            end case;
         when Ada_Exit_Stmt_Range =>
            case Index is
               when 1 =>
                  return Exit_Stmt_F_Loop_Name;
               when 2 =>
                  return Exit_Stmt_F_Cond_Expr;
               when others =>
                  null;
            end case;
         when Ada_Return_Stmt_Range =>
            case Index is
               when 1 =>
                  return Return_Stmt_F_Return_Expr;
               when others =>
                  null;
            end case;
         when Ada_Requeue_Stmt_Range =>
            case Index is
               when 1 =>
                  return Requeue_Stmt_F_Call_Name;
               when 2 =>
                  return Requeue_Stmt_F_Has_Abort;
               when others =>
                  null;
            end case;
         when Ada_Abort_Stmt_Range =>
            case Index is
               when 1 =>
                  return Abort_Stmt_F_Names;
               when others =>
                  null;
            end case;
         when Ada_Delay_Stmt_Range =>
            case Index is
               when 1 =>
                  return Delay_Stmt_F_Has_Until;
               when 2 =>
                  return Delay_Stmt_F_Expr;
               when others =>
                  null;
            end case;
         when Ada_Raise_Stmt_Range =>
            case Index is
               when 1 =>
                  return Raise_Stmt_F_Exception_Name;
               when 2 =>
                  return Raise_Stmt_F_Error_Message;
               when others =>
                  null;
            end case;
         when Ada_Label_Range =>
            case Index is
               when 1 =>
                  return Label_F_Decl;
               when others =>
                  null;
            end case;
         when Ada_If_Stmt_Range =>
            case Index is
               when 1 =>
                  return If_Stmt_F_Cond_Expr;
               when 2 =>
                  return If_Stmt_F_Then_Stmts;
               when 3 =>
                  return If_Stmt_F_Alternatives;
               when 4 =>
                  return If_Stmt_F_Else_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Named_Stmt_Range =>
            case Index is
               when 1 =>
                  return Named_Stmt_F_Decl;
               when 2 =>
                  return Named_Stmt_F_Stmt;
               when others =>
                  null;
            end case;
         when Ada_Base_Loop_Stmt =>
            case Index is
               when 1 =>
                  return Base_Loop_Stmt_F_Spec;
               when 2 =>
                  return Base_Loop_Stmt_F_Stmts;
               when 3 =>
                  return Base_Loop_Stmt_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Decl_Block_Range =>
            case Index is
               when 1 =>
                  return Decl_Block_F_Decls;
               when 2 =>
                  return Decl_Block_F_Stmts;
               when 3 =>
                  return Decl_Block_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Begin_Block_Range =>
            case Index is
               when 1 =>
                  return Begin_Block_F_Stmts;
               when 2 =>
                  return Begin_Block_F_End_Name;
               when others =>
                  null;
            end case;
         when Ada_Extended_Return_Stmt_Range =>
            case Index is
               when 1 =>
                  return Extended_Return_Stmt_F_Decl;
               when 2 =>
                  return Extended_Return_Stmt_F_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Case_Stmt_Range =>
            case Index is
               when 1 =>
                  return Case_Stmt_F_Expr;
               when 2 =>
                  return Case_Stmt_F_Alternatives;
               when others =>
                  null;
            end case;
         when Ada_Accept_Stmt_Range =>
            case Index is
               when 1 =>
                  return Accept_Stmt_F_Name;
               when 2 =>
                  return Accept_Stmt_F_Entry_Index_Expr;
               when 3 =>
                  return Accept_Stmt_F_Params;
               when others =>
                  null;
            end case;
            case Ada_Accept_Stmt_Range (Kind) is
               when Ada_Accept_Stmt_With_Stmts_Range =>
                  case Index is
                     when 4 =>
                        return Accept_Stmt_With_Stmts_F_Stmts;
                     when 5 =>
                        return Accept_Stmt_With_Stmts_F_End_Name;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when Ada_Select_Stmt_Range =>
            case Index is
               when 1 =>
                  return Select_Stmt_F_Guards;
               when 2 =>
                  return Select_Stmt_F_Else_Stmts;
               when 3 =>
                  return Select_Stmt_F_Abort_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Elsif_Stmt_Part_Range =>
            case Index is
               when 1 =>
                  return Elsif_Stmt_Part_F_Cond_Expr;
               when 2 =>
                  return Elsif_Stmt_Part_F_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Case_Stmt_Alternative_Range =>
            case Index is
               when 1 =>
                  return Case_Stmt_Alternative_F_Choices;
               when 2 =>
                  return Case_Stmt_Alternative_F_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Select_When_Part_Range =>
            case Index is
               when 1 =>
                  return Select_When_Part_F_Cond_Expr;
               when 2 =>
                  return Select_When_Part_F_Stmts;
               when others =>
                  null;
            end case;
         when Ada_Subunit_Range =>
            case Index is
               when 1 =>
                  return Subunit_F_Name;
               when 2 =>
                  return Subunit_F_Body;
               when others =>
                  null;
            end case;
         when Ada_Library_Item_Range =>
            case Index is
               when 1 =>
                  return Library_Item_F_Has_Private;
               when 2 =>
                  return Library_Item_F_Item;
               when others =>
                  null;
            end case;
         when Ada_Range_Spec_Range =>
            case Index is
               when 1 =>
                  return Range_Spec_F_Range;
               when others =>
                  null;
            end case;
         when Ada_Params_Range =>
            case Index is
               when 1 =>
                  return Params_F_Params;
               when others =>
                  null;
            end case;
         when Ada_Unconstrained_Array_Index_Range =>
            case Index is
               when 1 =>
                  return Unconstrained_Array_Index_F_Subtype_Indication;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;

      pragma Warnings (Off, "value not in range of type");
      return (raise Invalid_Field with "Index is out of bounds");
      pragma Warnings (On, "value not in range of type");
   end Field_Reference_From_Index;

   ------------
   -- Fields --
   ------------

   function Fields (Kind : Ada_Node_Kind_Type) return Field_Reference_Array is
   begin
      return Fields (Id_For_Kind (Kind), Concrete_Only => True);
   end Fields;

   ------------
   -- Fields --
   ------------

   function Fields
     (Id : Node_Type_Id; Concrete_Only : Boolean) return Field_Reference_Array
   is
      Cursor : Any_Node_Type_Id := Id;

      Added_Fields : array (Field_Reference) of Boolean := (others => False);
      --  Set of field references that were added to Result

      Result : Field_Reference_Array (1 .. Added_Fields'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin

      --  Go through the derivation chain for Id and collect fields. Do it in
      --  reverse order as we process base types last.
      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
              Node_Type_Descriptors (Cursor).all;
         begin
            for Field_Index in reverse Node_Desc.Fields'Range loop
               declare
                  Field_Desc : Node_Field_Descriptor renames
                    Node_Desc.Fields (Field_Index).all;
                  Field : Field_Reference renames Field_Desc.Field;
               begin
                  --  Abstract fields share the same Field_Reference value
                  --  with the corresponding concrete fields, so collect fields
                  --  only once. We process fields in reverse order, so we know
                  --  that concrete ones will be processed before the abstract
                  --  fields they override.
                  if not
                    (Concrete_Only and then Field_Desc.Is_Abstract_Or_Null)
                    and then not Added_Fields (Field)
                  then
                     Added_Fields (Field) := True;
                     Last                 := Last + 1;
                     Result (Last)        := Field;
                  end if;
               end;
            end loop;
            Cursor := Node_Desc.Base_Type;
         end;
      end loop;

      --  At this point, Result contains elements in the opposite order as
      --  expected, so reverse it.

      for I in 1 .. Last / 2 loop
         declare
            Other_I : constant Positive        := Last - I + 1;
            Swap    : constant Field_Reference := Result (I);
         begin
            Result (I)       := Result (Other_I);
            Result (Other_I) := Swap;
         end;
      end loop;

      return Result (1 .. Last);

   end Fields;

   ------------
   -- Fields --
   ------------

   function Fields (Id : Node_Type_Id) return Field_Reference_Array is
   begin
      return Fields (Id, Concrete_Only => False);
   end Fields;

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return String is
   begin
      return Property_Descriptors (Property).Name;
   end Property_Name;

   --------------------------
   -- Property_Return_Type --
   --------------------------

   function Property_Return_Type
     (Property : Property_Reference) return Value_Constraint
   is
   begin
      return Property_Descriptors (Property).Return_Type;
   end Property_Return_Type;

   ---------------------------
   -- Check_Argument_Number --
   ---------------------------

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive)
   is
   begin
      if Argument_Number not in Desc.Argument_Names'Range then
         raise Property_Error with "out-of-bounds argument number";
      end if;
   end Check_Argument_Number;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Value_Constraint_Array
   is
   begin
      return Property_Descriptors (Property).Argument_Types;
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property : Property_Reference; Argument_Number : Positive) return String
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return Property_Descriptors (Property).Argument_Names
          (Argument_Number).all;
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property : Property_Reference; Argument_Number : Positive)
      return Internal_Value
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return Desc.Argument_Default_Values (Argument_Number);
   end Property_Argument_Default_Value;

   ----------------
   -- Properties --
   ----------------

   function Properties
     (Kind : Ada_Node_Kind_Type) return Property_Reference_Array
   is
   begin
      return Properties (Id_For_Kind (Kind));
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
      Cursor : Any_Node_Type_Id := Id;

      Result : Property_Reference_Array (1 .. Property_Descriptors'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin
      --  Go through the derivation chain for Id and collect properties. Do it
      --  in reverse order as we process base types last.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
              Node_Type_Descriptors (Cursor).all;
         begin
            for Prop_Desc of reverse Node_Desc.Properties loop
               Last          := Last + 1;
               Result (Last) := Prop_Desc;
            end loop;
            Cursor := Node_Desc.Base_Type;
         end;
      end loop;

      --  At this point, Result contains elements in the opposite order as
      --  expected, so reverse it.

      for I in 1 .. Last / 2 loop
         declare
            Other_I : constant Positive           := Last - I + 1;
            Swap    : constant Property_Reference := Result (I);
         begin
            Result (I)       := Result (Other_I);
            Result (Other_I) := Swap;
         end;
      end loop;

      return Result (1 .. Last);
   end Properties;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : Ada_Node_Kind_Type) return Token_Kind is

   begin
      case Kind is
         when Ada_Char_Literal =>
            return Ada_Char;
         when Ada_Identifier =>
            return Ada_Identifier;
         when Ada_String_Literal =>
            return Ada_String;
         when Ada_Null_Literal =>
            return Ada_Null;
         when Ada_Int_Literal =>
            return Ada_Integer;
         when Ada_Real_Literal =>
            return Ada_Decimal;

         when others =>
            --  Kind is not a token node, and thus the precondition does not
            --  hold.
            return (raise Program_Error);
      end case;

   end Token_Node_Kind;

begin
   for D in Node_Type_Descriptors'Range loop
      DSL_Name_To_Node_Type.Insert (Node_Type_Descriptors (D).DSL_Name, D);
   end loop;
end Libadalang.Introspection_Implementation;
