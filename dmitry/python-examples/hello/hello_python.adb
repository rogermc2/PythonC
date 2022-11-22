
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
with Py;
with Py.Load_Python_Library;

procedure Hello_Python is
begin
    -- Load library
    Put_Line ("Hello_Python");
    Py.Load (Py.Load_Python_Library.Get_Python_Path
          &  Py.Load_Python_Library.Get_Default_Name);
    Put_Line ("Initialize");
    -- Initialize environment
    Py.Initialize;
    Put_Line ("Initialized");

    declare
        GIL    : Py.Global_Interpreter_Lock;
        Hello  : Py.Handle;
        Args   : Py.Handle;
        Result : Py.Handle;
    begin
        -- Compile Python source and find entry point in there
        Hello := Py.Compile
          ("def Hello(s):" & LF & "   print (""Hello ""+s+'!')", "hello.py");
        -- Create argument list
        Args := Py.Tuple_New (1);
        Py.Tuple_SetItem (Args, 0, Py.Unicode_FromString ("Python"));
        -- Make a call to Hello
        Result := Py.Object_CallObject (Hello, Args, True);
    end;

    if Integer (Py.FinalizeEx) < 0 then -- Finalize environment
        Put_Line ("Python finalization error");
    end if;

end Hello_Python;
