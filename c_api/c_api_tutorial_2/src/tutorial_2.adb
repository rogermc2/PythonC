
with Ada.Text_IO;

with Python_API;

procedure Tutorial_2 is

   Module : Python_API.Module;
   
begin  
   Python_API.Initialize;
   Python_API.Execute_String ("print ('Hello from Python!')");

   Python_API.Execute_String ("import os");
   Python_API.Execute_String ("cwd=os.getcwd");
   Python_API.Execute_String ("os.chdir('./src')");
   
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("loading external Python module");
   Module := Python_API.Import_Module ("python_module");
   
   Python_API.Close_Module (Module);
   Python_API.Finalize;
   
exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Ada_Main exception");
      
end Tutorial_2;

