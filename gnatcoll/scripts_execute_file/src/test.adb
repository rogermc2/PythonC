
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Python;         use GNATCOLL.Python;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
--  with Test_Common;

function Test return Integer
is
   Repository    : Scripts_Repository := null;
   Python_Script : Python_Scripting := null;
   Errors        : Boolean;
begin
   Put_Line ("Test");
--     Test_Common.Set_Python_Home;

   Repository := new Scripts_Repository_Record;
   Register_Python_Scripting
     (Repo        => Repository,
      Module      => "Test");
   --  Python_Name = "python"
   Python_Script := Python_Scripting (Lookup_Scripting_Language (Repository,
                                      Python_Name));

   Put_Line ("Test Execute_File");
   Python.Execute_File
     (Filename     => "simple_print.py",
      Show_Command => False,
      Errors       => Errors);
   Python.Destroy;
   Unregister_Python_Scripting (Repository);

   return 0;

exception
   when E : others =>
      Put_Line ("Test exception");
   return 0;

end Test;
