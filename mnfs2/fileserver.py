import xmlrpclib
import socket
import base64
import os
import time
import sys
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Fileserver:
	
	#setup
	def __init__(self, authhost, authport, user, passwd ,rootdir):
		self.passwd = passwd
		self.user = user
		self.rootdir = rootdir
		self.authhost = authhost 
		self.authport = str(authport)
		self.serverid = ' ' 
		#setup server		
		server = SimpleXMLRPCServer(('', 0))
		server.register_function(self.read, "read")
		server.register_function(self.readopen, "readopen")
		server.register_function(self.write, "write")
		server.register_function(self.serverwrite, "serverwrite")
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
		ticket, sessionkey, self.dirserverid, timestamp, self.serverid, self.dirhost, self.dirport = token.split()
		#register with dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
		ts, success = dirproxy.registerFileServer(ticket, self.encryptmsg(host, sessionkey), self.encryptmsg(port, sessionkey),self.encryptmsg(self.serverid, sessionkey) , self.encryptmsg(self.getfolders(), sessionkey))
		ts = self.decryptmsg(ts, sessionkey)
		success = self.decryptmsg(success, sessionkey)
		#make sure you know who your talking to
		if timestamp == ts:
			if success == 'True':
				#fire up		
				server.serve_forever()
			else:
				print 'registration failed'
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
		#validate timestamp
		if self.validate(timestamp):	
			path = self.decryptmsg(path, sessionkey)
			#try to read 
			try:
				#if not locked
				if not self.islocked(path)[0]:
					f = open(self.rootdir+path,'r')
					return self.encryptmsg('True', sessionkey),self.encryptmsg(f.read(), sessionkey), self.encryptmsg(timestamp, sessionkey)
				#if locked
				else:
					return self.encryptmsg('False', sessionkey), self.encryptmsg('None', sessionkey), self.encryptmsg(timestamp, sessionkey)

			#file not found
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)
		#invalid timestamp
		return self.encryptmsg(False, sessionkey), self.encryptmsg(timestamp, sessionkey)

	#read a file from disk
	def readopen(self, ticket, path, lockid):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		#validate timestamp
		if self.validate(timestamp):	
			path = self.decryptmsg(path, sessionkey)
			lockid = self.decryptmsg(lockid, sessionkey)
			#try to read 
			try:
				#if locked
				if self.islocked(path)[0]:
					#check valid lockid
					if self.islocked(path)[1] == lockid:
						f = open(self.rootdir+path,'r')
						return self.encryptmsg('True', sessionkey),self.encryptmsg(f.read(), sessionkey), self.encryptmsg(timestamp, sessionkey)
					#invalid lockid
					else:
						return self.encryptmsg('False', sessionkey), self.encryptmsg('None', sessionkey), self.encryptmsg(timestamp, sessionkey)

					
				#if locked
				else:
					return self.encryptmsg('False', sessionkey), self.encryptmsg('None', sessionkey), self.encryptmsg(timestamp, sessionkey)

			#file not found
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)
		#invalid timestamp
		return self.encryptmsg(False, sessionkey), self.encryptmsg(timestamp, sessionkey)

	#write a file to disk
	def write(self, ticket, path, data, lockid):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			path = self.decryptmsg(path, sessionkey)
			data = self.decryptmsg(data, sessionkey)
			lockid = self.decryptmsg(lockid, sessionkey)
		#	try:
			#if locked
			if self.islocked(path)[0]:
				#check valid lockid
				if self.islocked(path)[1] == lockid:
					#write to all replicas
					self.writepeers(path, data)	
					#write to fileserver
					self.ensure_dir(path)
					f = open(self.rootdir+path,'w')
					f.write(data)
					success, ts = self.setModified(path)
					if success:
						return self.encryptmsg(True, sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg(ts, sessionkey)
					else:	
						return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey),self.encryptmsg('None', sessionkey)			
				#invalid lockid
				else:
					return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey),self.encryptmsg('None', sessionkey)
			#if not locked
			else:
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey)
		#	except:
	#			raise IOError('write error'+path)
	#	return self.encryptmsg(False, sessionkey), self.encryptmsg(timestamp, sessionkey)

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
				return	self.encryptmsg(True, sessionkey), self.encryptmsg(timestamp, sessionkey)
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)
		return self.encryptmsg(False, sessionkey), self.encryptmsg(timestamp, sessionkey)

	#delete a file on disk
	def delete(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			path = self.decryptmsg(path, sessionkey)
			try:
				os.remove((str(self.rootdir))+path)			
				return self.encryptmsg(True, sessionkey), self.encryptmsg(timestamp, sessionkey)
			except:
				raise IOError('[Errno 2] No such file or directory: '+path)
		return self.encryptmsg(False, sessionkey), self.encryptmsg(timestamp, sessionkey)
	

#helper functions
	
	#gets peers who have a replica and calls function to write to each one
	def writepeers(self, path, contents):
		#get peers from dir server
			#do Auth
			#get ticket
			enctoken = self.authenticate('0')
			ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken).split()
		
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts, servers = dirproxy.getpeers(ticket, self.encryptmsg(path, sessionkey),self.encryptmsg(timestamp, sessionkey))
			
			servers = self.decryptmsg(servers , sessionkey).split(' ')
			servers.remove('')	
			if (self.decryptmsg(success , sessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , sessionkey) == timestamp:
					for server in servers:
						#write to each peer
						self.writes(server, path, contents)		
					return True
				else:
					return 'Error', None #'error rogue agent'
			else:
				return False, None			
			
			servers = servers.split(' ')		
	
	#writes to an actual replica		
	def writes(self, server, path, contents):
			#get server details
			fshost = server[0:server.index(':')]
			fsport = int(server[server.index(':')+1:server.rfind(':')])
			fssid  = server[server.rfind(':')+1:] 
			#quit if for this server
			if fssid == self.serverid:
				pass
			else:
				print 'writing to a replica of', path, 'sid: ', fssid
				#get token from Auth server
				enctoken = self.authenticate(fssid)
				ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken).split()
				#make request to file server
				fsproxy = xmlrpclib.ServerProxy('http://'+fshost+':'+str(fsport)+'/')
				success, ts = fsproxy.serverwrite(ticket, self.encryptmsg(path, sessionkey),self.encryptmsg(contents, sessionkey))
				if self.validate(timestamp) and success:
					return True
				else:
					return False
		
	def serverwrite(self,ticket, path, data):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp = self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			path = self.decryptmsg(path, sessionkey)
			data = self.decryptmsg(data, sessionkey)
			#do write
			self.ensure_dir(path)
			f = open(self.rootdir+path,'w')
			f.write(data)
			return self.encryptmsg(True, sessionkey), self.encryptmsg(timestamp, sessionkey)

	def islocked(self, path):
		#get ticket
		enctoken = self.authenticate('0')
		ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken).split()
		
		#make request to dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
		success, ts, lid = dirproxy.islocked(ticket, self.encryptmsg(path, sessionkey))
		if (self.decryptmsg(success , sessionkey)) == 'True':
			#check timestamp	
			if self.decryptmsg(ts , sessionkey) == timestamp:
				return True, self.decryptmsg(lid , sessionkey)
			else:
				return 'Error', None #'error rogue agent'
		else:
			return False, None
	def setModified(self, path):
		#get ticket
		enctoken = self.authenticate('0')
		ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken).split()
		
		#make request to dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
		success, ts, newts = dirproxy.setModified(ticket, self.encryptmsg(path, sessionkey))
		if (self.decryptmsg(success , sessionkey)) == 'True':
			#check timestamp	
			if self.decryptmsg(ts , sessionkey) == timestamp:
				return True, self.decryptmsg(newts, sessionkey)
			else:
				return False, None #'error rogue agent'
		else:
			return False, None


	# validates timestamps
	def validate(self, timestamp):
		if float(timestamp) >= float(time.time()):
			return True
		return False
	#helper function to create new directories when necessary
	def ensure_dir(self, f):
		d = os.path.dirname(f)
		if not os.path.exists(self.rootdir+d):
			os.makedirs(self.rootdir+d)
			

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

	#Authenticates with Authserver and returns a token or a tupple of (token, host, port)
	def authenticate(self, serverid):
		proxy = xmlrpclib.ServerProxy('http://'+self.authhost+':'+self.authport+'/')
		return proxy.authenticateServer(self.serverid, serverid)
		
serv = Fileserver('localhost', 10000, 'fserv', 'fspassword', sys.argv[1])
