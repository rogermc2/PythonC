
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Python;

procedure Fibonacci is
   Number   : constant Integer := 31;
   C_Num    : constant Interfaces.C.int := Interfaces.C.int (Number);
   Module   : Python.Module;
   Result   : Integer; 
begin
   New_Line;
   Python.Initialize;
   Python.Execute_String ("print ('The Fibonacci number for'," &  Integer'Image (Number) &
                            ",'is')");
   Module := Python.Import_File ("py_fibonacci");
   Result := Python.Call (Module, "Fibonacci", Number);
   
   Python.Close_Module (Module);
   Python.Finalize;
   
   Put_Line ("Fibonacci number:" & Integer'Image (Result));
   New_Line;
   
end Fibonacci;

