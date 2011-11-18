import xmlrpclib
import socket
import base64
import os
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Fileserver:
	
	def __init__(self, authhost, authport, user, passwd ,rootdir):
		self.passwd = passwd
		self.user = user
		self.rootdir = rootdir
		#setup server		
		server = SimpleXMLRPCServer(('', 0))
		server.register_function(self.writefile, "writefile")
		host = socket.getfqdn()		
		port = server.socket.getsockname()[1]
		print host, port
		#register with Authserver
		authproxy = xmlrpclib.ServerProxy('http://'+authhost+':'+str(authport)+'/')
		token = authproxy.registerFileServer(user)
		token = self.decryptToken(token)
		#extract data
		ticket = token.split()[0]
		sessionkey = token.split()[1]
		self.dirserverid = token.split()[2]
		timestamp = token.split()[3]
		self.serverid = token.split()[4]
		dirhost = token.split()[5]
		dirport = token.split()[6]
		#register with dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+dirhost+':'+str(dirport)+'/')
		dirproxy.registerFileServer(ticket, self.encryptmsg(host, sessionkey), self.encryptmsg(port, sessionkey),self.encryptmsg(self.serverid, sessionkey) , self.encryptmsg(self.getfolders(), sessionkey))
		#fire up		
		server.serve_forever()

	#helper function to make a list of all files on the server
	def getfolders(self):
		self.files = []
		self.folders = []
		for root, dirs, files in os.walk(self.rootdir):
			for item in files:
				self.files.append(("/"+ str(item))[2:])
	 		if (str(root))[len(self.rootdir):] != '':
		 		self.folders.append((str(root))[len(self.rootdir):])
		print ' '.join(self.folders)
	 	return ' '.join(self.folders)	

	#decrypts tokens (from authserver)
	def decryptToken(self, token):
		token = base64.decodestring(token)
		decryptor = ARC4.new(str(self.passwd))
		token = decryptor.decrypt(token)
		return token

	#encrypts messages to be sent privately with session key
	def encryptmsg(self, message, sessionkey):
		encryptor = ARC4.new(str(sessionkey))
		message = encryptor.encrypt(str(message))
		message = base64.encodestring(message)
		return message
	
	def writefile(self):
		print 'fixme'

serv = Fileserver('localhost', 10000, 'fserv', 'fspassword', 'fsroot1')
