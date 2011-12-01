import base64
import time
import math
import random
import os
import xmlrpclib
from Crypto.Cipher import ARC4
from threading import Thread

class api:
	def __init__(self, user, passwd, authhost, authport):
		self.user = user
		self.passwd = passwd
		self.authhost = authhost 
		self.authport = authport
		self.tickets = {}
		self.cache = {}
		self.openfiles = {}



#API Actions
	def read(self, path):
		if path in self.openfiles:
			success, data = self.readopen(path)
			if success:
				return data
			else:
				raise IOError('File in use')
		else:
			success, data =	self.normalread(path)
			if success:
				return data
			else:
				raise IOError('File in use')
		
	def open(self, path):
		if not (path in self.openfiles):
			success = self.lock(path)
			if success:
				#fork a thread that keeps the fileopen
				t = Thread(target=self.renewer, args=(path,))
				t.start()
				return True
			else:
				raise IOError('File in use')
		else:
			return True

	def write(self, path, data):
		self.normalwrite(path, data)

	def close(self, path):
		if path in self.openfiles:
			#unlock it
			self.unlock(path)
			#unlock locally
			self.openfiles.pop(path)
			return True
		else:
			return True

#file actions 

	#read a file from the filesystem	
	def normalread(self, filen):
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
		#check if actually open
		if filen in self.openfiles:
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

	def normalwrite(self, filen, data):
		#check if actually open
		if filen in self.openfiles:
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
				success, ts, newts = fsproxy.write(ticket, self.encryptmsg(filen, sessionkey),self.encryptmsg(data, sessionkey), self.encryptmsg((self.openfiles[filen])[1] , sessionkey))
				if self.decryptmsg(success ,sessionkey) == 'True':
					if (self.decryptmsg(ts ,sessionkey) == timestamp):
						self.ensure_dir('cache'+filen)
						self.cachewrite(filen, data)
						self.cache[filen] = self.decryptmsg(newts ,sessionkey)
						return True
					else:
						raise IOError('rogue agent')			
				else:
					return False, None



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
					return True
				else:
					print 'error rogue agent'
					return False
			else:
				return False
			
			

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
					return True
				else:
					print 'error rogue agent'
			else:
				print 'not locked'
				return False
			
		else:
			return False

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
					return 0
				else:
					print 'error rogue agent'
			else:
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
			print 'cache read'
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
	#renews lock on open files with the dirserver so they don't expire
	def renewer(self, path):
		i = 0
		while True:
			try:
			  	self.openfiles[path]
				#renew lock (every 15 seconds)
				if i == 10000:
					success = self.renew(path)
					#if the file has been closed
					if not success:
						#close file and quit
						self.openfiles.pop(path)
						return 0
		
			except:
					return 0
			#sleep 1 millisecond to save cpu
			time.sleep(.001)
			i+= 1				
		
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



