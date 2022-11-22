import ctypes

print 'loading external Python module:'
ada_module = ctypes.CDLL("lib/libada_module.so")

ada_module.ada_moduleinit.restype = None
ada_module.ada_modulefinal.restype = None
ada_module.hello.restype = None
ada_module.add.restype = ctypes.c_int
ada_module.sub.restype = ctypes.c_int
ada_module.mul.restype = ctypes.c_int
ada_module.div.restype = ctypes.c_int

ada_module.ada_moduleinit()

print 'calling functions from the Ada module:'
ada_module.hello()

print ''
print 'asking Ada to add two integers:'
a = 10
b = 2
result = ada_module.add(a, b)
print 'Python got result from Ada:', result

print ''
print 'we can try other operations, too:'

result = ada_module.sub(a, b)
print 'subtract:', result

result = ada_module.mul(a, b)
print 'multiply:', result

result = ada_module.div(a, b)
print 'divide  :', result

ada_module.ada_modulefinal()

