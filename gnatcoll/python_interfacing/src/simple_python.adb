
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;
with Python;

procedure Simple_Python is
   Repo    : Scripts_Repository := new Scripts_Repository_Record;
   Console : access Text_Console;
begin
--     Python.Initialize;
   Register_Python_Scripting (Repo, "GPS");
   Register_Standard_Classes (Repo, "Console");

   Console := GtkConsole.Create (...);
Set_Default_Console
  (Lookup_Scripting_Language (Repo, "python"),
   Virtual_Console (Console));

exception
    when anError : Constraint_Error =>
        Put ("Simple_Python returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Simple_Python.");
        Put_Line (Exception_Information (anError));
end Simple_Python;
