import xmlrpclib
import socket
import base64
import os
import time
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Fileserver:
	
	#setup
	def __init__(self, authhost, authport, user, passwd ,rootdir):
		self.passwd = passwd
		self.user = user
		self.rootdir = rootdir
		#setup server		
		server = SimpleXMLRPCServer(('', 0))
		server.register_function(self.read, "read")
		server.register_function(self.write, "write")
		server.register_function(self.append, "append")
		server.register_function(self.delete, "delete")
		host = socket.getfqdn()		
		port = server.socket.getsockname()[1]
		print host, port
		#register with Authserver
		authproxy = xmlrpclib.ServerProxy('http://'+authhost+':'+str(authport)+'/')
		token = authproxy.registerFileServer(user)
		token = self.decryptToken(token)
		#extract data
		ticket, sessionkey, self.dirserverid, timestamp, self.serverid, dirhost, dirport = token.split()
		#register with dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+dirhost+':'+str(dirport)+'/')
		confirmation = dirproxy.registerFileServer(ticket, self.encryptmsg(host, sessionkey), self.encryptmsg(port, sessionkey),self.encryptmsg(self.serverid, sessionkey) , self.encryptmsg(self.getfolders(), sessionkey))
		confirmation = self.decryptmsg(confirmation, sessionkey)
		#make sure you know who your talking to
		if timestamp == confirmation:
			#fire up		
			server.serve_forever()
		else:
			print 'rougue server'
	
#various encryption functions

	#decrypts tokens (from authserver)
	def decryptToken(self, token):
		token = base64.decodestring(token)
		decryptor = ARC4.new(str(self.passwd))
		token = decryptor.decrypt(token)
		return token

	#decrypts all tickets
	def decryptTicket(self, ticket):
		ticket = base64.decodestring(ticket)
		decryptor = ARC4.new(str(self.passwd))
		return decryptor.decrypt(ticket)

	#encrypts messages to be sent privately with session key
	def encryptmsg(self, message, sessionkey):
		encryptor = ARC4.new(str(sessionkey))
		message = encryptor.encrypt(str(message))
		message = base64.encodestring(message)
		return message

	#decrypts sessionkey encrypted messages
	def decryptmsg(self, message, sessionkey):
		message = base64.decodestring(message)
		decryptor = ARC4.new(str(sessionkey))
		return decryptor.decrypt(message)

#server actions

	#read a file from disk
	def read(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):	
			path = self.decryptmsg(path, sessionkey)
			try:
				f = open(self.rootdir+path,'r')
				return self.encryptmsg(f.read(), sessionkey), self.encryptmsg(timestamp, sessionkey)
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)
		return 1

	#write a file to disk
	def write(self, ticket, path, data):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			path = self.decryptmsg(path, sessionkey)
			data = self.decryptmsg(data, sessionkey)
			try:
				self.ensure_dir(path)
				f = open(self.rootdir+path,'w')
				f.write(data)
				return 0 #encrypt also needs to return time stamp for validation
			except:
				raise IOError('write error'+path)

		return 1 #encrypt also needs to return time stamp for validation

	#append to a file and write to disk
	def append(self, ticket, path, data):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			path = self.decryptmsg(path, sessionkey)
			data = self.decryptmsg(data, sessionkey)
			try:
				self.ensure_dir(path)
				f = open(self.rootdir+path,'a')
				f.write(data)
				return 0 #encrypt also needs to return time stamp for validation
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)
		return 1 #encrypt also needs to return time stamp for validation

	#delete a file on disk
	def delete(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			path = self.decryptmsg(path, sessionkey)
			try:
				os.remove((str(self.rootdir))+path)			
				return 0 #encrypt also needs to return time stamp for validation
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)

		return 1 #encrypt also needs to return time stamp for validation

	

#helper functions

	# validates timestamps
	def validate(self, timestamp):
		print float(timestamp), float(time.time())
		if float(timestamp) >= float(time.time()):
			return True
		return False
	#helper function to create new directories when necessary
	def ensure_dir(self, f):
		d = os.path.dirname(f)
		if not os.path.exists(d):
        		os.makedirs(d)

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

serv = Fileserver('localhost', 10000, 'fserv', 'fspassword', 'fsroot1')
