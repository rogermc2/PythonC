with System;

package Python_API is

   Interpreter_Error : exception;
   
   type Module is private;

   procedure Close_Module (M : in Module);
   procedure Execute_String (Script : in String);
   procedure Finalize;   
   function Import_File (File_Name : in String) return Module;
   function Import_Module (File_Name : in String) return Module;
   procedure Initialize (Program_Name : in String := "");
   
   --  Overloads for "all" needed combinations of parameters and return types:
   
   procedure Call (M : in Module; Function_Name : in String);
   function  Call (M : in Module; Function_Name : in String; A : in Integer; B : Integer) return Integer;
   --  ...
   
private

   type Module is new System.Address;

end Python_API;

