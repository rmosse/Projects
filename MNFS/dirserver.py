import os
import socket
import ssl
import threading
import base64
import sys
from Crypto.PublicKey import RSA
from Crypto.Cipher import ARC4
import time
import math
import thread
import random
from secureconnection import secureconnection

class dirservice(threading.Thread):

	maindict = {}
	allservers = [('','','')]

	def __init__(self, newsocket, rootdir, serverid, numreplicas):
		threading.Thread.__init__(self)
		self.client_socket = ssl.wrap_socket(newsocket,server_side=True,certfile="certs/cert", 
				keyfile="certs/key",ssl_version=ssl.PROTOCOL_SSLv3)
		self.locked = []
		self.serverid = serverid
		self.lock=thread.allocate_lock()
		
	def run(self):
			data = self.secrecv()
#		try:	
			_type, path, ticket, msg, user, passwd = self.parserequest(data)
			self.sessionkey, timestamp, isServer = self.decrypt(ticket)
			if (self.valid(user, timestamp, _type, path)):
				if isServer == 'True':
					data = self.processServRequest(msg, _type, path, self.__class__.maindict)
				else:
					data = self.processClientRequest(path, _type,self.__class__.maindict)
				self.secsend(data)

		#except:
#			self.secsend("HTTP/1.0 500 Internel server Error\r\nContent-length: 0\r\n\r\n")		
			
#		self.close()
		
	"""	
	def islocked(self, path):
		if path in self.locked:
			return True
		else:
			return False
		
	def lock(self, path):
		if path in self.locked:
			return True
		else:
			self.locked.append(path)
			return True
	
	def unlock(self, path):
		if path in self.locked:
			self.locked.remove(path)
			return True
		else:
			return True
	"""

	def getid(self):
		return self.serverid
		

	#reads info from socket
	def secrecv(self):
		buffer = self.client_socket.recv(1024)
		conlen = self.getlength(buffer)
		lenrecv = (len(buffer)) - (buffer.index("\r\n\r\n")) -4
		if lenrecv >= conlen:
			return buffer
		else:
			while lenrecv <= conlen:	
				data = self.client_socket.recv(1024)
				lenrecv = lenrecv + len(data)
				buffer = buffer + data 
			return buffer
	
	#sends info over socket connection		
	def secsend(self, data):
		self.client_socket.send((data))
		
	#closes socket			
	def close(self):
		self.client_socket.close()	
			
	def parserequest(self, request):
		#parse http to a nice list
		msg = request.split('\r\n')
		for i in range(0,len(msg)):
			msg[i] = msg[i].split(' ')
	
		list = sum(msg,[])
		#extract data from list
		requesttype = list[0]
		path = list[1]
		token = list[list.index('Authorization:')+2]
		token = base64.decodestring(token).strip()
		username, password = token.split(':')		
		ticket = request[request.index("\r\n\r\n")+4:]
		message = ticket[ticket.index("\r\n\r\n")+4:]
		return requesttype, path, ticket[:ticket.index("\r\n\r\n")], message, username, password
		
	def processClientRequest(self, path, type, maindict):
		try:
			if (type == 'GET') or (type == 'PUT'):
				path = path[::-1]
				path = path[path.index('/')+1:]
				path = path[::-1]
				hosts = maindict[path]
			if (type == 'POST'):
				#see if it exists
				try:
					path = path[::-1]
					path = path[path.index('/')+1:]
					path = path[::-1]
					hosts = maindict[path]						
				#if it doesn't
				except:
					rindex = int(math.floor(random.uniform(1,len(self.__class__.allservers))))
					hosts = self.__class__.allservers[rindex]
					
			#pick a random server with the file for load balancing purposes
			rindex = int(math.floor(random.uniform(1,len(hosts))))
			hostname = hosts[rindex]
			host = hostname.split(':')[0]
			port = hostname.split(':')[1]
			sid  = hostname.split(':')[2]
			return "HTTP/1.1 302 Found\r\nLocation: "+host+':'+str(port)+path+'\r\nContent-length: '+str(len(sid))+'\r\n\r\n'+str(sid)
		except:
			return "HTTP/1.0 404 Not Found\r\nContent-length: 0\r\n\r\n"
			
	def processServRequest(self, contents, rtype, path, maindict):
		#server getpeersrequest
		if rtype == 'HEAD':
			#see if it exists
			try:
				key = path[0:path.rfind('/')]
				msg = maindict[key]
				msg = str(self.encrypt(str(' '.join(msg))))
				return "HTTP/1.0 200 OK\r\nContent-length: "+str(len(msg))+"\r\n\r\n"+msg
			except:
				rindex = int(math.floor(random.uniform(1,len(self.__class__.allservers))))
				msg = self.__class__.allservers[rindex]
				msg = str(self.encrypt(str(' '.join(msg))))	
				return "HTTP/1.0 200 OK\r\nContent-length: "+str(len(msg))+"\r\n\r\n"+msg
			
			
		else:
		#server initialization			
			#get info
			hostname = contents.split('\r\n')[0]
			serverid = contents.split('\r\n')[1]
			serverlist = contents.split('\r\n')[2]
			serverlist = serverlist.split(' ')			
			self.__class__.allservers.append(('',hostname+':'+serverid))
			#add the server to the working set
			self.incorporate(serverlist, hostname, serverid)			
			return "HTTP/1.0 200 OK\r\nContent-length: 0\r\n\r\n"
			
	# incorporates the directory list of a new fileserver in to a global hashmap of directories to a list 
	# of servers containing them.
	# as the function needs to scale extemely well I use hashmaps and use sets to 
	# find differences between the lists instead of itteration 
	def incorporate(self, servlist, hostname, serverid):
		# append server to existing files in database (converts to sets and uses 
		# intersection to efficiantly find the the dirs that are already in the database) 
		self.lock.acquire() 
		for item in set(self.__class__.maindict).intersection(set(servlist)):
			newslist = list(self.__class__.maindict[item])
			newslist.append(hostname+':'+serverid)
			self.__class__.maindict[item] = tuple(newslist)
			print "fixme remove dead servers"
				
		#add new files (gets the diff between the two sets and the adds them to the new ones to the global dictionary)
		for item in set(self.__class__.maindict) ^ set(servlist):
			self.__class__.maindict[item] = ('',hostname+':'+serverid)
			
		self.lock.release() 

	def decrypt(self, msg):
		f = open("serverkeys/"+str(self.serverid))
		key = f.read()
		encryptor = ARC4.new(key)
		ticket = encryptor.decrypt(msg)
		return ticket.split('\r\n')[0], ticket.split('\r\n')[1], ticket.split('\r\n')[2]

	def encrypt(self, msg):
		encryptor = ARC4.new(self.sessionkey)
		return encryptor.decrypt(msg)

	def getlength(self, headers):
			for item in headers.split('\r\n'):
				if item.split(' ')[0] == "Content-length:":
					return int(item.split(' ')[1])

	def valid(self, user, timestamp, _type, path):
		current = int(math.floor(time.time()))
		expires = int(math.floor(float(timestamp))) + 86400 
		# check timestamp
		Axe = True
		if (current > expires):
			Axe = False
		# get file permissions
		print "fixme user, permissions"
		return Axe

class dirserver:

	def __init__(self, authhost, authport):
			username = 'dirserver'
			password = 'dirserverpass'
			self.numreplicas = 2 # number of replicas each file should have
		#register with auth server
			#generate key
			key = RSA.generate(1024)
			self.key = key.exportKey()
			msg = str(key)+'\r\n'
			
			#setup socket and get my port
			self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			self.s.bind(('', 0))
			self.port = self.s.getsockname()[1]
			self.host = socket.getfqdn()
			
			#formulate the registration request 
			request = self.host +'\r\n'+str(self.port)+'\r\n'+self.key

			#send data
			conn = secureconnection(authhost,authport)
			conn.sendmsg(username, password,'GET', '/', '0'+"\r\n"+request)
			token = conn.recvmsg()
			conn.close()
			
			#record assigned server id
			self.serverid = self.decrypttoken(token,password)
			print "my sid:", self.serverid
			
			#start a thread to look out for dead servers
			thread.start_new_thread(self.removedeadservers, ())

	def decrypttoken(self, message, passwd):
			start = message.index("\r\n\r\n")+4
			message = message[start:]
			decryptor = ARC4.new(passwd)
			return decryptor.decrypt(message)
			
	def removedeadservers(self):
			while True:
				time.sleep(2)
				servers = dirservice.allservers[1:]
				for server in servers:
					host, port = server[1].split(':')[:2]
	#				try:
					conn2 = secureconnection(host, int(port))
					conn2.sendmsg(self.user, self.passwd,'TEST', '#','none')
					resp = conn2.recvmsg()
					conn2.close()
	#					print 'its alive',host,port
	#				except:
	#					print 'its dead',host,port
						

	def serve(self):	
			#listen for connections 
			self.s.listen(5)
			while 1:
				#spawn new connections
				(newsocket, address) = self.s.accept()
				t = dirservice(newsocket,'/home/mosser/mnfs/fsroot/', self.serverid, self.numreplicas)
				t.start()
				

server = dirserver(sys.argv[1],int(sys.argv[2]))
server.serve()
