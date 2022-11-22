
with Ada.Text_IO;

with Python_API;

procedure Tutorial_1 is

   Module : Python_API.Module;
   
   A      : constant Integer := 10;
   B      : constant Integer := 2;
   Result : Integer; 
begin  
   Python_API.Initialize;

   Ada.Text_IO.Put_Line ("executing Python directly from Ada:");
   Python_API.Execute_String ("print ('Hello from Python!')");

   Python_API.Execute_String ("import os");
   Python_API.Execute_String ("cwd=os.getcwd");
   Python_API.Execute_String ("os.chdir('./src')");
   
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("loading external Python module and calling functions from that module:");
   Module := Python_API.Import_File ("python_module");
   Python_API.Call (Module, "hello");
   
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("asking Python to add two integers:");
   Result := Python_API.Call (Module, "add", A, B);
   
   Ada.Text_IO.Put_Line ("Ada got result from Python:" & Integer'Image (Result));
   
   Python_API.Close_Module (Module);
   Python_API.Finalize;
   
exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Ada_Main exception");
      
end Tutorial_1;

