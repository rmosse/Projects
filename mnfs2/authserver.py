#!/usr/bin/env python
import xmlrpclib
import math
import random
import base64
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Authserver:
	
	def __init__(self, host, port):
		#read in a db of users, fileservers, and the dirserver usernames and passwords		
		self.dserv =  {}
		self.fservs = {}
		self.users = {}
		self.readinusers()	
		#setup server	
		server = SimpleXMLRPCServer((host,  port))
		server.register_multicall_functions()
		#register functions
		server.register_function(self.registerDirServer , "registerDirServer")
		server.register_function(self.registerDirServer , "registerDirServer")

		#fire up
		server.serve_forever()	

	def readinusers(self):
		#read in userdbs
		fd = open('dservdb.dat','r')
		ff = open('fservdb.dat','r')
		fu = open('userdb.dat','r')		
		#dict for dirservers	
		dserv = fd.read()
		self.dserv[dserv.split(':')[0]] = dserv.split(':')[1]
		#dict for fileservers
		for serv in ff:
			self.fservs[serv.split(':')[0]] = serv.split(':')[1]
		#dict for users
		for user in fu:
			self.users[serv.split(':')[0]] = serv.split(':')[1]

		
			


	#registers the dirserver and gets it details for later reference
	def registerDirServer(self, username, host, port):
			self.dirhost = host
			self.dirport = port	
			self.dirserverid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		
			token = self.encryptDSToken(username, self.dirserverid)	
			print token	
			return token

	
		
	#responds to fileserver with dirserver token
	def registerFileServer(self, username):
			serverid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		
			sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		
						


			#return token for dirserver (encrypted with key derived from password)()
			return 'fstoken'				
	
	#local getter function
	def getDirServerToken(self,username, serverid):
			#return token for server (encrypted with key derived from password)
			return 'usertoken'

	
	def makefstoken(self,username):
	#token
		#ticket			
		#sessionkey
		#dirserverid
		#timestamp
		#It's own sid			
		#dirserver host, port
		
	#ticket #encrypted with key derived from dirserverpassword
		#sessionkey			
		return 'fstoken'			

	def makeuserToken(self,username, serverid):
	#token
		#ticket			
		#sessionkey
		#serverid
		#timestamp			
		registerDirServer('dserv', host , port)
	#ticket #encrypted with key derived from dirserverpassword
		#sessionkey
		return 'usertoken'

	def getDirServerId(self):
		#private not to be sent over network
		return self.dirserverid

	#encrpts a token with key derived from the client password
	def encryptDSToken(self, username, token):
		passwd = self.dserv[username]
		encryptor = ARC4.new(str(passwd))
		return base64.encodestring(encryptor.encrypt(str(token)))
			



authserver = Authserver('localhost', 10000)
