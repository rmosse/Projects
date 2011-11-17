import xmlrpclib
import socket
import base64
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Fileserver:
	
	def __init__(self, authhost, authport, user, passwd):
		self.passwd = passwd
		self.user = user

		#setup server		
		server = SimpleXMLRPCServer(('', 0))
		server.register_function(self.writefile "writefile")
		host = socket.getfqdn()		
		port = server.socket.getsockname()[1]
		print host, port

		#register with Authserver
		authproxy = xmlrpclib.ServerProxy('http://'+authhost+':'+str(authport)+'/')
		token = authproxy.registerFileServer(user, host, port)
		print token 
		print self.decryptDSToken(token)
		#fire up		
		server.serve_forever()

	def decryptDSToken(self, token):
		decryptor = ARC4.new(str(self.passwd))
		return decryptor.decrypt(base64.decodestring(token))

serv = Dirserver('localhost', 10000, 'fserv', 'fspassword')
