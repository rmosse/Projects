#!/usr/bin/env python
import xmlrpclib
import math
import random
import base64
import time
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Authserver:

#setup functions
	def __init__(self, host, port):
		#read in a db of users, fileservers, and the dirserver usernames and passwords		
		self.dserv =  {}
		self.fservs = {}
		self.users = {}
		self.readinusers()
		#map of serverids, to passwords for quick encrption of tickets and tokens
		self.serverpasswds = {}	

		#setup server	
		server = SimpleXMLRPCServer((host,  port))
		server.register_multicall_functions()
		#register functions
		server.register_function(self.registerDirServer , "registerDirServer")
		server.register_function(self.registerFileServer , "registerFileServer")
		server.register_function(self.authenticateUser , "authenticateUser")
		server.register_function(self.authenticateServer , "authenticateServer")
		#fire up
		server.serve_forever()	

	#reads in the database of usernames and passwords from disk
	def readinusers(self):
		#read in userdbs
		fd = open('dservdb.dat','r')
		ff = open('fservdb.dat','r')
		fu = open('userdb.dat','r')		
		#dict for dirservers	
		dserv = fd.read()
		self.dserv[dserv.split(':')[0]] = dserv.split(':')[1][:-1]
		#dict for fileservers
		for serv in ff:
			self.fservs[serv.split(':')[0]] = serv.split(':')[1][:-1]
		#dict for users
		for user in fu:
			self.users[user.split(':')[0]] = user.split(':')[1][:-1]
#server Actions
	#registers the dirserver and gets it details for later reference
	def registerDirServer(self, username, host, port):
			self.dirhost = host
			self.dirport = port	
			self.dirserverid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		
			#save a map of sid to passwd for quick futer encryption
			self.serverpasswds[str(self.dirserverid)] = self.dserv[username]
			#encrypt token			
			token = self.encryptServerToken(self.dirserverid, self.dirserverid)	
			return token
		
	#responds to fileserver with dirserver token
	def registerFileServer(self, username):
			serverid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		
			sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		
			#save a map of sid to serverpasswd
			self.serverpasswds[str(serverid)] = self.fservs[username]
			#generate timestamp
			timestamp = str(time.time()+20)
			#ticket			
			ticket = [str(sessionkey), timestamp]
			ticket = self.encryptTicket(' '.join(ticket), str(self.dirserverid))
			#token
			response = ticket, str(sessionkey), str(self.dirserverid), timestamp, str(serverid), str(self.dirhost), str(self.dirport)
			response = ' '.join(response)
			response = self.encryptServerToken(response, serverid)		
			return response				
	
	#Authenticate a user
	def authenticateUser(self, username, serverid):
		axe = True
		if serverid == '0':
			axe = False
			serverid = self.dirserverid
		sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))
		#generate timestamp
		timestamp = str(time.time()+20)	
		ticket = [str(sessionkey) , timestamp]
		ticket = self.encryptTicket(' '.join(ticket),str(serverid))
		response = str(ticket), str(sessionkey), str(serverid), timestamp
		response = self.encryptUserToken(' '.join(response), username)
		if axe == True:
			return response
		else:
			return response, self.dirhost, self.dirport

	#Authenticate a server
	def authenticateServer(self, username, serverid):
		axe = True
		if serverid == '0':
			axe = False
			serverid = self.dirserverid
		sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))
		#generate timestamp
		timestamp = str(time.time()+20)	
		ticket = [str(sessionkey) , timestamp]
		ticket = self.encryptTicket(' '.join(ticket),str(serverid))
		response = str(ticket), str(sessionkey), str(serverid), timestamp
		response = self.encryptServerToken(' '.join(response), username)
		if axe == True:
			return response
		else:
			return response


#helper encryption functions

	#encrpts a token with key derived from the server password
	def encryptServerToken(self, token, serverid):
		passwd = self.serverpasswds[str(serverid)]
		encryptor = ARC4.new(str(passwd))
		token = encryptor.encrypt(str(token))
		token = base64.encodestring(token)
		return token	
	
	#encrpts a token with key derived from the user password
	def encryptUserToken(self, token, username):
		passwd = self.users[username]
		encryptor = ARC4.new(str(passwd))
		token = encryptor.encrypt(str(token))
		token = base64.encodestring(token)
		return token

	#encrypts ticket
	def encryptTicket(self, ticket, destserverid):
		passwd = self.serverpasswds[str(destserverid)]
		encryptor = ARC4.new(str(passwd))
		ticket = encryptor.encrypt(str(ticket))
		ticket = base64.encodestring(ticket)
		return ticket[:-1]

authserver = Authserver('localhost', 10000)
authserver.registerFileServer(self, 'fserv')
