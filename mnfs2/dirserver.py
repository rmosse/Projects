import xmlrpclib
import socket
import base64
import random
import math
import time
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer
from threading import Lock

mutex = Lock()
modifiedmutex = Lock()
class Dirserver:
#setup
	
	def __init__(self, authhost, authport, passwd):
		#dictionary of all known files
		self.maindict = {}
		self.passwd = passwd
		self.modified = {}
		self.locked = {}
		#setup server		
		server = SimpleXMLRPCServer(('', 0))
		server.register_multicall_functions()
		server.register_function(self.registerFileServer , "registerFileServer")
		server.register_function(self.getlocation , "getlocation")
		server.register_function(self.getpeers , "getpeers")
		server.register_function(self.lock , "lock")
		server.register_function(self.renew , "renew")
		server.register_function(self.unlock , "unlock")
		server.register_function(self.islocked , "islocked")
		server.register_function(self.setModified , "setModified")

		host = socket.getfqdn()		
		port = server.socket.getsockname()[1]
		print host, port
		
		#register with Authserver
		authproxy = xmlrpclib.ServerProxy('http://'+authhost+':'+str(authport)+'/')
		token = authproxy.registerDirServer('dserv', host , port)
		self.serverid = self.decryptDSToken(token)
		#fire up
			
		server.serve_forever()


#server actions

	#adds info about fileservers who join the network
	def registerFileServer(self, ticket, enchost, encport, encsid, encfilelist):
		sessionkey =  self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):
			host =  self.decryptmsg(enchost, sessionkey)
			port =  self.decryptmsg(encport, sessionkey)
			serverid =  self.decryptmsg(encsid, sessionkey)
			filelist =  self.decryptmsg(encfilelist, sessionkey)
			self.incorporate(filelist.split(), host, port, serverid)
			return(self.encryptmsg(timestamp, sessionkey)), self.encryptmsg('True', sessionkey)
		else: 
			return self.encryptmsg(timestamp, sessionkey), self.encryptmsg('False', sessionkey)	
	

	#handles file location requests from clients
	def getlocation(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			try:
				hosts = self.maindict[path[:path.rfind('/')]]
				rindex = int(math.floor(random.uniform(1,len(hosts))))
				self.modified[path] = str(time.time())
				return self.encryptmsg('True', sessionkey) , self.encryptmsg(timestamp, sessionkey), self.encryptmsg(hosts[rindex], sessionkey) 
			except:
				pass
		return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey)
	
	
	
	#find last time a file was modified
	def lastModified(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			try:
				return self.encryptmsg('True', sessionkey) , self.encryptmsg(timestamp, sessionkey), self.encryptmsg(self.modified[path], sessionkey) 
			except:
				pass
		return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey)
	#updates file timestamps so clients can validate local cached copies and replicas can be validated
	def setModified(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			stamp = str(time.time())
			mutex.acquire()
			self.modified[path] = str(stamp)
			mutex.release()
			return self.encryptmsg('True', sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg(stamp, sessionkey)
	

	def getpeers(self, ticket, path, timestamp):
		#see if it exists
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			try:
				key = path[0:path.rfind('/')]
				msg = self.maindict[key]
				msg = str(' '.join(msg))
				return self.encryptmsg('True', sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg(msg, sessionkey)
			except:
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg('None', sessionkey)	

#these functions handle locking

	#simply locks
	def lock(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		#validate ts
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			mutex.acquire()
			if path in self.locked.keys():
				mutex.release()			
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey) ,self.encryptmsg('None', sessionkey) 
			lockid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))	
			self.locked[path] = lockid, float(time.time()) + float(60)
			mutex.release()
			return self.encryptmsg('True', sessionkey), self.encryptmsg(timestamp, sessionkey) ,self.encryptmsg(lockid, sessionkey) 
		return self.encryptmsg('Error', sessionkey), self.encryptmsg(timestamp, sessionkey) 

	#renews a lock before it expires
	def renew(self, ticket, path, lockid):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		#validate ts
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			lockid = self.decryptmsg(lockid, sessionkey)
			mutex.acquire()
			if path not in self.locked.keys():
				mutex.release()			
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey) 
			if str((self.locked[path])[0]) == lockid: #check its locked by caller
				self.locked[path] = lockid, float(time.time()) + float(60) #give's an extra 60 seconds
				mutex.release()
				return self.encryptmsg('True', sessionkey), self.encryptmsg(timestamp, sessionkey) 
		return self.encryptmsg('Error', sessionkey), self.encryptmsg(timestamp, sessionkey) 
	
	#removes a lock
	def unlock(self, ticket, path, lockid):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		#validate ts
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			lockid = self.decryptmsg(lockid, sessionkey)
			mutex.acquire()
			if path not in self.locked.keys():
				mutex.release()			
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey) 
			if str((self.locked[path])[0]) == lockid:
				self.locked.pop(path)
				mutex.release()
				return self.encryptmsg('True', sessionkey), self.encryptmsg(timestamp, sessionkey) 
		return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey) 

	#allows you to check if a file is locked
	def islocked(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		#validate ts
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			mutex.acquire()
			#if not locked
			if path not in self.locked.keys():
				mutex.release()			
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg('None', sessionkey)  
			#if lock expired
			if (self.locked[path])[1] < time.time():
				self.locked.pop(path)
				mutex.release()
				return self.encryptmsg('False', sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg('None', sessionkey)  
			#if has valid lock 
			lockid = (self.locked[path])[0]
			mutex.release()
			return self.encryptmsg('True', sessionkey), self.encryptmsg(timestamp, sessionkey), self.encryptmsg(lockid, sessionkey) 
		#failed invalid timestamp
		return self.encryptmsg('Error', sessionkey), self.encryptmsg(timestamp, sessionkey) 

#various encryption functions

	#decrypts token recieved from Authserver 
	def decryptDSToken(self, token):
		token = base64.decodestring(token)
		decryptor = ARC4.new(str(self.passwd))
		return decryptor.decrypt(token)

	#decrypts all tickets
	def decryptTicket(self, ticket):
		ticket = base64.decodestring(ticket)
		decryptor = ARC4.new(str(self.passwd))
		return decryptor.decrypt(ticket)

	#decrypts sessionkey encrypted messages
	def decryptmsg(self, message, sessionkey):
		message = base64.decodestring(message)
		decryptor = ARC4.new(str(sessionkey))
		return decryptor.decrypt(message)
		
	#encrypts messages to be sent privately with session key
	def encryptmsg(self, message, sessionkey):
		encryptor = ARC4.new(str(sessionkey))
		message = encryptor.encrypt(str(message))
		message = base64.encodestring(message)
		return message

#various helper functions	
	
	# validates timestamps
	def validate(self, timestamp):
		if float(timestamp) >= float(time.time()):
			return True
		return False

	

	# incorporates the directory list of a new fileserver in to a global hashmap of directories to a list 
	# of servers containing them.
	# as the function needs to scale extemely well I use hashmaps and use sets to 
	# find differences between the lists instead of itteration 
	def incorporate(self, servlist, host, port, serverid):
		# append server to existing files in database (converts to sets and uses 
		# intersection to efficiantly find the the dirs that are already in the database) 
		for item in set(self.maindict).intersection(set(servlist)):
			newslist = list(self.maindict[item])
			newslist.append(host+':'+port+':'+serverid)
			self.maindict[item] = tuple(newslist)
			self.modified[item] = str(time.time())	
			print "fixme remove dead servers"
				
		#add new files (gets the diff between the two sets and the adds them to the new ones to the global dictionary)
		for item in set(self.maindict) ^ set(servlist):
			self.maindict[item] = ('',host+':'+port+':'+serverid)
			self.modified[item] = str(time.time())	
			
	#compiles a list of folders stored on the file server
	def getfolders(self):
		self.files = []
		self.folders = []
		for root, dirs, files in os.walk(self.rootdir):
			for item in files:
				self.files.append(("/"+ str(item))[2:])
	 		if (str(root))[len(self.rootdir):] != '':
		 		self.folders.append((str(root))[len(self.rootdir):])
	 	return ' '.join(self.folders)	

serv = Dirserver('localhost', 10000, 'dspassword')
