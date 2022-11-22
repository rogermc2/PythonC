with Interfaces.C;

package Ada_Module is

   procedure Hello;
   pragma Export (C, Hello, "hello");
   
   function Add (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int;
   pragma Export (C, Add, "add");

   function Sub (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int;
   pragma Export (C, Sub, "sub");

   function Mul (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int;
   pragma Export (C, Mul, "mul");

   function Div (A : in Interfaces.C.int; B : in Interfaces.C.int) return Interfaces.C.int;
   pragma Export (C, Div, "div");

end Ada_Module;

