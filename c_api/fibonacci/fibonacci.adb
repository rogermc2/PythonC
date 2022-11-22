
with Ada.Text_IO; use Ada.Text_IO;

with Python;

procedure Fibonacci is
   
   Module : Python.Module;
   Result : Integer; 
begin  
   Python.Initialize;
   Module := Python.Import_File ("python_module");
   Result := Python.Call (Module, "Fibonacci", 10);
   Put_Line ("Fibonacci number:" & Integer'Image (Result));
   
   Python.Close_Module (Module);

   Python.Finalize;
   
end Fibonacci;

