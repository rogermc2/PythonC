with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Main is

   Command : constant String := "python -u src/test.py 123";
   Pd      : Process_Descriptor;
   Args    : Argument_List_Access;
   Result  : Expect_Match := 0;

begin

   Args := Argument_String_To_List (Command);
   for I in Args'First .. Args'Last loop
      Put_Line ("Arg " &  Args (I).all);
   end loop;

   Non_Blocking_Spawn
      (Pd,
       Command     => Args (Args'First).all,
       Args        => Args (Args'First + 1 .. Args'Last),
       Buffer_Size => 0);

   for I in 1 .. 5 loop
      Expect (Pd, Result, Regexp => "\d+", Timeout => 2_000);
      Put_Line ("I: " & Integer'Image (I));

      case Result is
         when Expect_Timeout =>
            Put_Line ("Expect timed out.");
         when 1  =>
            Put_Line ("Received: " & Expect_Out_Match (Pd));
         when others =>
            raise Program_Error;
      end case;

      Put_Line ("Doing other stuff...");

   end loop;

   Close (Pd);
   Free (Args);

exception
   when Process_Died =>
      Put_Line ("Process died.");
      Close (Pd);
      Free (Args);

end Main;
