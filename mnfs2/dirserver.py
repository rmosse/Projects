import xmlrpclib
import socket
import base64
import random
import math
from Crypto.Cipher import ARC4
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Dirserver:
#setup
	
	def __init__(self, authhost, authport, passwd):
		#dictionary of all known files
		self.maindict = {}
		self.passwd = passwd
		#setup server		
		server = SimpleXMLRPCServer(('', 0))
		server.register_multicall_functions()
		server.register_function(self.registerFileServer , "registerFileServer")
		server.register_function(self.getlocation , "getlocation")
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
			return 0 #should also return a timestamp here to be validated
			print 'fixme'
		else: return 1	
	

	#handles file location requests from clients
	def getlocation(self, ticket, path):
		sessionkey = self.decryptTicket(ticket).split(' ')[0]
		timestamp =  self.decryptTicket(ticket).split(' ')[1]
		if self.validate(timestamp):		
			path = self.decryptmsg(path, sessionkey)
			try:
				hosts = self.maindict[path[:path.rfind('/')]]
				rindex = int(math.floor(random.uniform(1,len(hosts))))
				return True , self.encryptmsg(hosts[rindex], sessionkey) # TODO True should be encrypted also need to return timestamp to be validated
				print 'fixme'
			except:
				return False, 'none'

		return 1

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
		if int(timestamp) <= int(time.time):
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
			print "fixme remove dead servers"
				
		#add new files (gets the diff between the two sets and the adds them to the new ones to the global dictionary)
		for item in set(self.maindict) ^ set(servlist):
			self.maindict[item] = ('',host+':'+port+':'+serverid)
			
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
