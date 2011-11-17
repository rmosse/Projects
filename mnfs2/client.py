import xmlrpclib
proxy = xmlrpclib.ServerProxy("http://localhost:10000/")
print proxy.registerDirServer('dirserver', 'localhost' , 12000)
