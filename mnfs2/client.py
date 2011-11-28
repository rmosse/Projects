import base64
import time
import math
import random
import os
import xmlrpclib
from Crypto.Cipher import ARC4

class mnfs:
	def __init__(self, user, passwd, authhost, authport):
		self.user = user
		self.passwd = passwd
		self.authhost = authhost 
		self.authport = authport
		self.tickets = {}
		self.cache = {}
		self.openfiles = {}

#file actions 

	#read a file from the filesystem	
	def read(self, filen):
		#see if we can use cached version
		success, data = self.cacheread(filen)
		if success:
			return True, data
		else:
			#get ticket for dirserver
			#check to see if we already have a ticket
			try:
				ticket, timestamp = self.tickets[self.dirserverid]
				assert(self.validate(timestamp))		
			except:
				enctoken, self.dirhost, self.dirport = self.authenticate('0')
				ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
				self.tickets[self.dirserverid] = ticket, timestamp
		
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts, response = dirproxy.getlocation(ticket, self.encryptmsg(filen, self.dssessionkey)) 
			if (self.decryptmsg(success , self.dssessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , self.dssessionkey) == timestamp:
					fshost, fsport, serverid = self.decryptmsg(response, self.dssessionkey).split(':')
				else:
					print 'error rogue agent'
			else:
				raise IOError('[Errno 2] No such file or directory: '+filen)
		
			#get ticket for fileserver
			#check to see if we already have a ticket
			try:
				ticket, sessionkey, serverid, timestamp = self.tickets[serverid]
				assert(self.validate(timestamp))		
			except:
				enctoken = self.authenticate(serverid)
				ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken)
				self.tickets[serverid] = ticket, sessionkey, serverid, timestamp 

			#make request to fileserver
			fsproxy = xmlrpclib.ServerProxy('http://'+fshost+':'+str(fsport)+'/')
			success, data , ts = fsproxy.read(ticket, self.encryptmsg(filen, sessionkey)) 
			if self.decryptmsg(success ,sessionkey) == 'True':
				if (self.decryptmsg(ts ,sessionkey) == timestamp):
					self.ensure_dir('cache'+filen)
					self.cachewrite(filen, self.decryptmsg(data,sessionkey))
					return True, self.decryptmsg(data,sessionkey)
				else:
					raise IOError('rogue agent')			
			else:
				return False, None
	def readopen(self, filen):
		#see if we can use cached version
		success, data = self.cacheread(filen)
		if success:
			return True, data
		else:
			#get ticket for dirserver
			#check to see if we already have a ticket
			try:
				ticket, timestamp = self.tickets[self.dirserverid]
				assert(self.validate(timestamp))		
			except:
				enctoken, self.dirhost, self.dirport = self.authenticate('0')
				ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
				self.tickets[self.dirserverid] = ticket, timestamp
		
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts, response = dirproxy.getlocation(ticket, self.encryptmsg(filen, self.dssessionkey)) 
			if (self.decryptmsg(success , self.dssessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , self.dssessionkey) == timestamp:
					fshost, fsport, serverid = self.decryptmsg(response, self.dssessionkey).split(':')
				else:
					print 'error rogue agent'
			else:
				raise IOError('[Errno 2] No such file or directory: '+filen)
		
			#get ticket for fileserver
			#check to see if we already have a ticket
			try:
				ticket, sessionkey, serverid, timestamp = self.tickets[serverid]
				assert(self.validate(timestamp))		
			except:
				enctoken = self.authenticate(serverid)
				ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken)
				self.tickets[serverid] = ticket, sessionkey, serverid, timestamp 

			#make request to fileserver
			fsproxy = xmlrpclib.ServerProxy('http://'+fshost+':'+str(fsport)+'/')
			success, data , ts = fsproxy.readopen(ticket, self.encryptmsg(filen, sessionkey), self.encryptmsg((self.openfiles[filen])[1] , sessionkey))
			if self.decryptmsg(success ,sessionkey) == 'True':
				if (self.decryptmsg(ts ,sessionkey) == timestamp):
					self.ensure_dir('cache'+filen)
					self.cachewrite(filen, self.decryptmsg(data,sessionkey))
					return True, self.decryptmsg(data,sessionkey)
				else:
					raise IOError('rogue agent')			
			else:
				return False, None

	def write(self, filen, data):
		#get ticket for dirserver
		#check to see if we already have a ticket
		try:
			ticket, timestamp = self.tickets[self.dirserverid]
			assert(self.validate(timestamp))		
		except:
			enctoken, self.dirhost, self.dirport = self.authenticate('0')
			ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
			self.tickets[self.dirserverid] = ticket, timestamp
	
		#make request to dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
		success, ts, response = dirproxy.getlocation(ticket, self.encryptmsg(filen, self.dssessionkey)) 
		if (self.decryptmsg(success , self.dssessionkey)) == 'True':
			#check timestamp	
			if self.decryptmsg(ts , self.dssessionkey) == timestamp:
				fshost, fsport, serverid = self.decryptmsg(response, self.dssessionkey).split(':')
			else:
				print 'error rogue agent'
		else:
			raise IOError('[Errno 2] No such file or directory: '+filen)
	
		#get ticket for fileserver
		#check to see if we already have a ticket
		try:
			ticket, sessionkey, serverid, timestamp = self.tickets[serverid]
			assert(self.validate(timestamp))		
		except:
			enctoken = self.authenticate(serverid)
			ticket, sessionkey, serverid, timestamp = self.decryptToken(enctoken)
			self.tickets[serverid] = ticket, sessionkey, serverid, timestamp 

		#make request to fileserver
		fsproxy = xmlrpclib.ServerProxy('http://'+fshost+':'+str(fsport)+'/')
		success, ts = fsproxy.write(ticket, self.encryptmsg(filen, sessionkey), self.encryptmsg(data, sessionkey)) 
		if (self.decryptmsg(ts ,sessionkey) == timestamp):
			self.ensure_dir('cache'+filen)
			self.cachewrite(filen, self.decryptmsg(data,sessionkey))
			return self.decryptmsg(data,sessionkey)
		else:
			raise IOError('rogue agent')		


	#TODO remove for testing only	
	def lock(self, filen):
			#get ticket for dirserver
			#check to see if we already have a ticket
			try:
				ticket, timestamp = self.tickets[self.dirserverid]
				assert(self.validate(timestamp))		
			except:
				enctoken, self.dirhost, self.dirport = self.authenticate('0')
				ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
				self.tickets[self.dirserverid] = ticket, timestamp
		
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts, response = dirproxy.lock(ticket, self.encryptmsg(filen, self.dssessionkey)) 
			if (self.decryptmsg(success , self.dssessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , self.dssessionkey) == timestamp:
					lockid = self.decryptmsg(response, self.dssessionkey)
					self.openfiles[filen] = True, lockid
					return 0
				else:
					print 'error rogue agent'
			else:
				print 'file in use'
				return 1
			
			
	#TODO remove for testing only	
	def renew(self, filen):
		try:
			lockd, lockid = self.openfiles[filen]
		except:
			lockd = False
		if lockd:
			#get ticket for dirserver
			#check to see if we already have a ticket
			try:
				ticket, timestamp = self.tickets[self.dirserverid]
				assert(self.validate(timestamp))		
			except:
				enctoken, self.dirhost, self.dirport = self.authenticate('0')
				ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
				self.tickets[self.dirserverid] = ticket, timestamp
	
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts = dirproxy.renew(ticket, self.encryptmsg(filen, self.dssessionkey),self.encryptmsg(lockid, self.dssessionkey)) 
			if (self.decryptmsg(success , self.dssessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , self.dssessionkey) == timestamp:
					print 'renewed'	
					return 0
				else:
					print 'error rogue agent'
			else:
				print 'not locked'
				return 1
			
		else:
			return 1

	#TODO remove for testing only	
	def unlock(self, filen):
		try:
			lockd, lockid = self.openfiles[filen]
		except:
			lockd = False
		if lockd:
			#get ticket for dirserver
			#check to see if we already have a ticket
			try:
				ticket, timestamp = self.tickets[self.dirserverid]
				assert(self.validate(timestamp))		
			except:
				enctoken, self.dirhost, self.dirport = self.authenticate('0')
				ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
				self.tickets[self.dirserverid] = ticket, timestamp
	
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts = dirproxy.unlock(ticket, self.encryptmsg(filen, self.dssessionkey),self.encryptmsg(lockid, self.dssessionkey)) 
			if (self.decryptmsg(success , self.dssessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , self.dssessionkey) == timestamp:
					print 'unlocked'	
					return 0
				else:
					print 'error rogue agent'
			else:
				print 'not locked'
				return 1
			
		else:
			return 1

	def islocked(self, filen):
			#get ticket for dirserver
			#check to see if we already have a ticket
			try:
				ticket, timestamp = self.tickets[self.dirserverid]
				assert(self.validate(timestamp))		
			except:
				enctoken, self.dirhost, self.dirport = self.authenticate('0')
				ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
				self.tickets[self.dirserverid] = ticket, timestamp
	
			#make request to dirserver
			dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
			success, ts = dirproxy.islocked(ticket, self.encryptmsg(filen, self.dssessionkey))
			if (self.decryptmsg(success , self.dssessionkey)) == 'True':
				#check timestamp	
				if self.decryptmsg(ts , self.dssessionkey) == timestamp:
					print 'it\'s locked'	
					return 0
				else:
					print 'error rogue agent'
			else:
				print 'not locked'
				return 1
			
		

#cache functions
	def cachewrite(self, path, data):
		ftimestamp = time.time()
		f = open('cache'+path,'w')	
		f.write(data)
		f.close()
		self.cache[path] = ftimestamp

	#try and read from the cache
	def cacheread(self, path):
		#check to see if we have a cached copy
		try:
			ftimestamp = self.cache[path]
			assert(self.upToDate(path, ftimestamp))
			f = open('cache'+fileid,'r')
			return True, f.read()
		except:
			return False, None
			
	#check to see if our copy is valid
	def upToDate(self, path, ftimestamp):
		#get ticket for dirserver
		#check to see if we already have a ticket
		try:
			ticket, timestamp = self.tickets[self.dirserverid]
			assert(self.validate(timestamp))		
		except:
			enctoken, self.dirhost, self.dirport = self.authenticate('0')
			ticket, self.dssessionkey, self.dirserverid, timestamp = self.decryptToken(enctoken)
			self.tickets[self.dirserverid] = ticket, timestamp

		#make request to dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
		success, ts, modified = dirproxy.lastModified(ticket, self.encryptmsg(path, self.dssessionkey)) 
		if (self.decryptmsg(success , self.dssessionkey)) == 'True':
			#check timestamp	
			if self.decryptmsg(ts , self.dssessionkey) == timestamp:
				lastmodified = self.decryptmsg(modified, self.dssessionkey)
				if float(lastmodified) <= float(ftimestamp):
					return True
				else:
					return False
				
			else:
				print 'error rogue agent'
		else:
			raise IOError('[Errno 2] No such file or directory: '+filen)
		
		

#various functions to handle encryption

	#Authenticates with Authserver and returns a token or a tupple of (token, host, port)
	def authenticate(self, serverid):
		proxy = xmlrpclib.ServerProxy('http://'+self.authhost+':'+self.authport+'/')
		return proxy.authenticateUser(self.user, serverid)
	
	#decrypt incoming tokens from Authserver
	def decryptToken(self, token):
		token = base64.decodestring(token)
		decryptor = ARC4.new(str(self.passwd))
		return decryptor.decrypt(token).split()
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

#helper functions

	# validates timestamps
	def validate(self, timestamp):
		if float(timestamp) >= float(time.time()):
			return True
		return False

	#helper function to create new directories when necessary
	def ensure_dir(self, f):
		d = os.path.dirname(f)
		if not os.path.exists(d):
        		os.makedirs(d)


fs = mnfs('user','password' ,'localhost', '10000')
print fs.read('/folder1/file1')
fs.lock('/folder1/file1')
print fs.read('/folder1/file1')
print fs.readopen('/folder1/file1')
