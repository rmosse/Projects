import sys
import ssl
import socket
import threading
import time
import math
import random
import base64
import hashlib
from Crypto.Cipher import ARC4

class Authservice(threading.Thread):
	dirserverid = 0
	
	def __init__(self, newsocket, userdb , serverdb, dirserverdb):	
		threading.Thread.__init__(self)
		self.client_socket = ssl.wrap_socket(newsocket, server_side=True,
			certfile="certs/cert", keyfile="certs/key",ssl_version=ssl.PROTOCOL_SSLv3)
		#userdb
		f = open(userdb, 'r')
		self.userdb = f.read() 
		f.close()
		#serverdb		
		f = open(serverdb, 'r')
		self.serverdb = f.read() 
		f.close()
		#dirserverdb
		f = open(dirserverdb, 'r')
		self.dirserverdb = f.read() 
		f.close()
			
	def run(self):
		request = self.secrecv()
		user, passwd, self.destserverid, serverkey, _type = self.getDetails(request)
		#client
		if (self.validusr(user,passwd)):
			#generate token
			sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))
			timeout = time.time()
			ticket = str(sessionkey)+"\r\n"+str(timeout)+"\r\n"+str(False)           
			ticket = self.encrypticket(ticket, self.destserverid)
			token = str(ticket)+"\r\n"+str(sessionkey)+"\r\n"+ str(self.destserverid)+"\r\n\r\n"
			token = self.encryptoken(token, passwd)
			token = token +'\r\n\r\n'
			
			#send token
			self.secsend( "HTTP/1.1 302 Found\r\nLocation: "+self.__class__.dirserverhost+':'+str(self.__class__.dirserverport)+'/', str(token))
			
#			self.secsend( "HTTP/1.1 200 OK", str(token))
			print "Authenticated: "+str(user)
			self.close()
			quit()

			#server
		if self.validserv(user, passwd):
			#registration request
			if _type == 'GET':			
				#generate serverid
				serverid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))
				#save key
				f = open ('serverkeys/'+str(serverid),'w')
				f.write(serverkey)
				f.close()
				
				#generate token
				sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))
				timeout = time.time()
				ticket = str(sessionkey)+"\r\n"+str(timeout)+"\r\n"+str(True)
				ticket = self.encrypticket(ticket, self.__class__.dirserverid)
				token = str(ticket)+"\r\n"+str(sessionkey)+"\r\n"+ str(serverid)+"\r\n\r\n"				
				token = self.encryptoken(token, passwd)
				token = token +'\r\n\r\n'
			
				#send token
				self.secsend( "HTTP/1.1 302 Found\r\nLocation: "+self.__class__.dirserverhost+':'+str(self.__class__.dirserverport)+'/', str(token))
				self.close()
				print "Authenticated: "+str(user), 'destid', serverid 
				return 0

			#other request
			else:
				#generate token
				sessionkey = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))
				timeout = time.time()
				ticket = str(sessionkey)+"\r\n"+str(timeout)+"\r\n"+str(True)
				ticket = self.encrypticket(ticket, self.destserverid)
				token = str(ticket)+"\r\n"+str(sessionkey)+"\r\n"+ str(self.destserverid)+"\r\n\r\n"				
				token = self.encryptoken(token, passwd)
				token = token +'\r\n\r\n'
			
				#send token
				self.secsend( "HTTP/1.1 200 OK", str(token))
				self.close()						
				print "Authenticated: "+str(user) , 'destid', self.destserverid 
				return 0

		#register the Directory server
		if self.validDirserv(user,passwd):			
			#getdetails			
			self.__class__.dirserverhost = serverkey.split('\r\n')[0]
			self.__class__.dirserverport = serverkey.split('\r\n')[1]
			serverkey = serverkey.split('\r\n')[2]

			#generate serverid
			self.__class__.dirserverid = int(math.floor(random.uniform(100000000000000000, 999999999999999999)))		

			#save key
			f = open ('serverkeys/'+str(self.__class__.dirserverid),'w')
			f.write(serverkey)
			f.close()
			print 'TODO also need to get dirserer host and port from it'
			
			#use the encrpyt token on this even though its not a token 
			#because this should be encrypted in the sam way			
			sid = self.encryptoken(str(self.__class__.dirserverid), passwd)

			#send it back			
			self.secsend( 'HTTP/1.1 200 OK', str(sid))
			print "Authenticated: "+str(user), 'destid', self.__class__.dirserverid 			
	
			self.close()		
			return 0

		self.secsend( "HTTP/1.1 403 Access Denied", " ")
		self.close()
		return 0		

	#reads info from socket
	def secrecv(self):
		buffer = self.client_socket.recv(1024)
		conlen = self.getlength(buffer)
		lenrecv = (len(buffer)) - (buffer.index("\r\n\r\n")) -4
		if lenrecv >= conlen:
			return buffer
		else:
			while lenrecv < conlen:	
				data = client_socket.recv(1024)
				lenrecv = lenrecv + len(data)
				buffer = buffer + data 
			
			return buffer
	

	#sends info over socket connection		
	def secsend(self, headers, data):
		lines= [
				headers,
				'Content-length: %s' % len(data),
		       ]
		self.client_socket.send(('\r\n'.join(lines)+' \r\n\r\n')+data)

	
	#closes socket			
	def close(self):
		self.client_socket.close()		
			
		
	def getlength(self, headers):
			for item in headers.split('\r\n'):
				if item.split(' ')[0] == "Content-length:":
					return int(item.split(' ')[1])
						
	def getDetails(self, request):
		msg = request.split('\r\n')
		for i in range(0,len(msg)):
			msg[i] = msg[i].split(' ')
	
		list = sum(msg,[])
		rtype = list[0]
		token = list[list.index('Authorization:')+2]
		token = base64.decodestring(token).strip()
		username, password = token.split(':')
		serverid = request[request.index('\r\n\r\n')+ 4:]
		key = serverid[serverid.index('\r\n')+2:]
		serverid = serverid[:serverid.index('\r\n')]
		return username, password, serverid, key, rtype

	def validusr(self, user, passwd):
		if str(user+":"+passwd) in self.userdb:
			return True
		else:
			return False	
	def validserv(self, user, passwd):
		if str(user+":"+passwd) in self.serverdb:
			return True
		else:
			return False	
	def validDirserv(self, user, passwd):
		if str(user+":"+passwd) in self.dirserverdb:
			return True
		else:
			return False	

	def encrypticket(self, ticket, serverid):
		if serverid == '0':
			f = open("serverkeys/"+str(self.__class__.dirserverid))
			self.destserverid = self.__class__.dirserverid
		else:	
			f = open("serverkeys/"+str(serverid))
		key = f.read()
		encryptor = ARC4.new(key)
		return encryptor.encrypt(ticket)
	
	def encryptoken(self, token, passwd):
		encryptor = ARC4.new(passwd)
		return encryptor.encrypt(token)
		
			
class Authserver:
		
	def serve(self, port):
		#listen for connections 
		s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		s.bind(('', port))
		s.listen(5)
		while 1:
			#spawn new connections
			(newsocket, address) = s.accept()
			t = Authservice(newsocket,"userdb.dat", "serverdb.dat", "dirserverdb.dat" )
			t.start()
			
server = Authserver()
server.serve(int(sys.argv[1]))
