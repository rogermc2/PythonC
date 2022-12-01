
with Ada.Text_IO;

with Python;

procedure Ada_Main is

   Module : Python.Module;
   
   A      : constant Integer := 10;
   B      : constant Integer := 2;
   Result : Integer; 
begin  
   Python.Initialize;

   Ada.Text_IO.Put_Line ("executing Python directly from Ada:");
   Python.Execute_String ("print ('Hello from Python!')");

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("loading external Python module and calling functions from that module:");
   Module := Python.Import_File ("python_module");
   Python.Call (Module, "hello");
   
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("asking Python to add two integers:");
   Result := Python.Call (Module, "add", A, B);
   
   Ada.Text_IO.Put_Line ("Ada got result from Python:" & Integer'Image (Result));
   
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("we can try other operations, too:");
   
   Result := Python.Call (Module, "sub", A, B);
   Ada.Text_IO.Put_Line ("subtract:" & Integer'Image (Result));
   
   Result := Python.Call (Module, "mul", A, B);
   Ada.Text_IO.Put_Line ("multiply:" & Integer'Image (Result));
   
   Result := Python.Call (Module, "div", A, B);
   Ada.Text_IO.Put_Line ("divide  : " & Integer'Image (Result));
   
   Python.Close_Module (Module);
   Ada.Text_IO.New_Line;
   
   Python.Finalize;
   
exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Ada_Main exception");
      
end Ada_Main;

