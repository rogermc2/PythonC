with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body Python is

   subtype PyObject is System.Address;

   procedure Py_SetProgramName (Name : in Interfaces.C.char_array);
   pragma Import (C, Py_SetProgramName, "Py_SetProgramName");

   procedure Py_Initialize;
   pragma Import (C, Py_Initialize, "Py_Initialize");

   procedure Py_Finalize;
   pragma Import (C, Py_Finalize, "Py_Finalize");
    
   function PyRun_SimpleString (Command : in Interfaces.C.char_array)
                                return Interfaces.C.int;
   pragma Import (C, PyRun_SimpleString, "PyRun_SimpleString");
    
   procedure Py_IncRef (Obj : in PyObject);
   pragma Import (C, Py_IncRef, "Py_IncRef");
    
   procedure Py_DecRef (Obj : in PyObject);
   pragma Import (C, Py_DecRef, "Py_DecRef");
    
   function PyInt_AsLong (I : in PyObject) return Interfaces.C.long;
   pragma Import (C, PyInt_AsLong, "PyLong_AsLong");
      
   function PyString_FromString (Str : in Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyString_FromString, "PyUnicode_FromString");
    
   function PyImport_Import (Obj : in PyObject) return PyObject;
   pragma Import (C, PyImport_Import, "PyImport_Import");
   
   function PyImport_ImportModule (Str : Interfaces.C.char_array)
                                   return PyObject;
   pragma Import (C, PyImport_ImportModule, "PyImport_ImportModule");
   
   function PyObject_CallMethod
     (Obj    : PyObject; Name : Interfaces.C.char_array;
      Format : Interfaces.C.Strings.char_array_access) return PyObject;
   pragma Import (C, PyObject_CallMethod, "PyObject_CallMethod");
   
   function PyObject_GetAttrString
     (Obj : in PyObject; Name : in Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyObject_GetAttrString, "PyObject_GetAttrString");
   
   function PyObject_CallObject (Obj : in PyObject; Args : in PyObject) return PyObject;
   pragma Import (C, PyObject_CallObject, "PyObject_CallObject");
   
   procedure PyErr_Print;
   pragma Import (C, PyErr_Print, "PyErr_Print");
    
   function PyUnicode_AsUTF8String (Obj : PyObject) return PyObject;
   pragma Import (C, PyUnicode_AsUTF8String, "PyUnicode_AsUTF8String");
    
   function PyBytes_AsString (Obj : PyObject)
                              return Interfaces.C.Strings.char_array_access;
   pragma Import (C, PyBytes_AsString, "PyBytes_AsString");
   
   -- --------------------------------------------------------------------------
 
   procedure Initialize (Program_Name : in String := "") is
      --        use Ada.Directories;
      --        CWD : constant String := Current_Directory;  
   begin
      if Program_Name /= "" then
         declare
            C_Name           : Interfaces.C.char_array := Interfaces.C.To_C (Program_Name);
            Program_Name_Ptr : access Interfaces.C.char_array := new Interfaces.C.char_array'(C_Name);
         begin
            Py_SetProgramName (Program_Name_Ptr.all);
         end;
      end if;
       
      Py_Initialize;
      
      --  Below: workaround for the following issue:
      --  http://stackoverflow.com/questions/13422206/how-to-load-a-custom-python-module-in-c

      Execute_String ("import sys");   
      Execute_String ("sys.path.append('.')");
      --        Execute_String ("sys.path.append('/Ada_Projects/python/ada-python')");
      --        Execute_String ("sys.path.append(" & Cwd & ")");
   end Initialize;
    
   -- --------------------------------------------------------------------------
 
   procedure Finalize is
   begin
      Py_Finalize;
   end Finalize;
    
   -- --------------------------------------------------------------------------
 
   procedure Execute_String (Script : in String) is
      Dummy : Interfaces.C.int;
   begin
      Dummy := PyRun_SimpleString (Interfaces.C.To_C (Script));
   end Execute_String;
    
   -- --------------------------------------------------------------------------
 
   function Import_File (File_Name : in String) return Module is
      use type System.Address;
      PyFileName : PyObject :=
                     PyString_FromString (Interfaces.C.To_C (File_Name));
      M          : PyObject := PyImport_Import (PyFileName);
   begin
      Py_DecRef (PyFileName);
      if M = System.Null_Address then
         --PyErr_Print;
         raise Interpreter_Error with "Cannot load module from file " &
           File_Name;
      end if;
       
      return Module (M);
   end Import_File;
   
   -- --------------------------------------------------------------------------
 
   function Import_Module (File_Name : in String) return Module is
      use type System.Address;
      --        PyFileName : PyObject := PyString_FromString (Interfaces.C.To_C (File_Name));
      PyFileName : Interfaces.C.char_array := Interfaces.C.To_C (File_Name);
      M : PyObject := PyImport_ImportModule (PyFileName);
   begin
      --        Py_DecRef (PyFileName);
      if M = System.Null_Address then
         --PyErr_Print;
         raise Interpreter_Error with "Cannot load module from file " & File_Name;
      end if;
       
      return Module (M);
   end Import_Module;
   
   -- --------------------------------------------------------------------------
 
   procedure Close_Module (M : in Module) is
   begin
      Py_DecRef (PyObject (M));
   end Close_Module;

   -- --------------------------------------------------------------------------
   --  helpers for use from all overloaded Call subprograms
   
   function Get_Symbol (M : in Module; Function_Name : in String) return PyObject is
      PyModule : PyObject := PyObject (M);
      F        : PyObject := PyObject_GetAttrString (PyModule, Interfaces.C.To_C (Function_Name));
      use type System.Address;
   begin
      Py_DecRef (PyModule);
      if F = System.Null_Address then
         --PyErr_Print;
         raise Interpreter_Error with "Cannot find function " & Function_Name;
      end if;
      
      return F;
   end Get_Symbol;
   
   -- --------------------------------------------------------------------------
 
   function Call_Object (F : in PyObject; Function_Name : in String; PyParams : in PyObject) return PyObject is
      PyResult : PyObject;
      use type System.Address;
   begin
      PyResult := PyObject_CallObject (F, PyParams);
      if PyResult = System.Null_Address then
         raise Interpreter_Error with "Operation " & Function_Name & " did not return expected result";
      end if;
      
      return PyResult;
   end Call_Object;      

   -- --------------------------------------------------------------------------
   --  public operations
   
   procedure Call (M : in Module; Function_Name : in String) is
      F      : PyObject := Get_Symbol (M, Function_Name);
      Result : PyObject;
   begin
      Result := PyObject_CallObject (F, System.Null_Address);
      Py_DecRef (Result);
   end Call;

   -- --------------------------------------------------------------------------
 
   function Call (M : in Module; Function_Name : in String; A : in Integer)
                  return Integer is
      F : PyObject := Get_Symbol (M, Function_Name);
      
      function Py_BuildValue (Format : in Interfaces.C.char_array;
                              A      : in Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
      
      use type Interfaces.C.long;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(i)"),
                                 Interfaces.C.int (A));
      Ada.Text_IO.Put_Line ("Call_Object");
      PyResult := Call_Object (F, Function_Name, PyParams);
      Ada.Text_IO.Put_Line ("Call_Object");
      Result := PyInt_AsLong (PyResult);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);
   
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Fibonacci exception");
      return 0;
      
   end Call;
   
   -- --------------------------------------------------------------------------
 
   function Call (M : in Module; Function_Name : in String;
                  A : in Integer; B : Integer) return Integer is
      F : PyObject := Get_Symbol (M, Function_Name);
      
      function Py_BuildValue (Format : in Interfaces.C.char_array;
                              A      : in Interfaces.C.int;
                              B      : in Interfaces.C.int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyParams : PyObject;
      PyResult : PyObject;
      Result   : aliased Interfaces.C.long;
      
      use type Interfaces.C.int;
   begin
      PyParams := Py_BuildValue (Interfaces.C.To_C ("ii"), Interfaces.C.int (A),
                                 Interfaces.C.int (B));
      PyResult := Call_Object (F, Function_Name, PyParams);
      Result := PyInt_AsLong (PyResult);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Integer (Result);
   end Call;
   
end Python;

