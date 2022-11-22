with Ada.Text_IO;

package body Ada_Module is

   use type Interfaces.C.int;
   
   procedure Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello from Ada module!");
   end Hello;

   function Add (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int is
   begin
      Ada.Text_IO.Put_Line ("Ada adding:" & Interfaces.C.int'Image (A) & " +" & Interfaces.C.int'Image (B));
      return A + B;
   end Add;

   function Sub (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int is
   begin
      return A - B;
   end Sub;

   function Mul (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int is
   begin
      return A * B;
   end Mul;

   function Div (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int is
   begin
      return A / B;
   end Div;

end Ada_Module;

