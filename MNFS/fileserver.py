import os
import socket
import ssl
import threading
import base64
import sys
from Crypto.Cipher import ARC4
import time
import math
from Crypto.PublicKey import RSA
from secureconnection import secureconnection
import mnfs

class fileservice(threading.Thread):

		
	def __init__(self, newsocket, rootdir, serverid, dirhost, dirport, user, passwd):
		threading.Thread.__init__(self)
		self.client_socket = ssl.wrap_socket(newsocket,server_side=True,certfile="certs/cert", 
				keyfile="certs/key",ssl_version=ssl.PROTOCOL_SSLv3)
		self.rootdir = rootdir
		self.locked = []
		self.serverid = serverid
		self.dirhost = dirhost
		self.dirport = int(dirport)
		self.username = user
		self.password = passwd
		
	def run(self):
		data = self.secrecv()
		try:
			_type, path, ticket, msg, user, passwd = self.parserequest(data)
			self.sessionkey, timestamp, self.is_server = self.decrypt(ticket)
			if (self.valid(user, timestamp, _type, path)):
				data = self.processrequest(_type, path, msg, user, passwd)
				self.secsend(data)
		except:
			self.secsend("HTTP/1.0 500 Internel server Error\r\nContent-length: 0\r\n\r\n")		
			
		self.close()
		
		
	def readf(self, path, username, password):
		if os.path.isfile((str(self.rootdir))+path):
			#authenticate user
			if self.islocked(path):
				return "HTTP/1.0 403 Access Denied (Locked)\r\nContent-length: 0\r\n\r\n"
			else:
				
				try:
					self.lock(path)
					f = open((str(self.rootdir))+path,'r')
					contents = f.read()
					f.close()
					self.unlock(path)
					contents = self.encryptresponse(contents)
					return "HTTP/1.0 200 OK\r\nContent-length: "+str(len(contents))+"\r\n\r\n" + str(contents)
				except IOError:
					return "HTTP/1.0 500 Internel server Error\r\nContent-length: 0\r\n\r\n"
		else:
			return "HTTP/1.0 404 File not found\r\nContent-length: 0\r\n\r\n"			
					
					
	def writef(self, path, contents, username, password):
		#authenticate user
		if self.islocked(path):
			return "HTTP/1.0 403 Access Denied (Locked)\r\nContent-length: 0\r\n\r\n"
		else:
			try:
				self.lock(path)
				#if client request update replicas
				if self.is_server == 'False':
					assert self.writepeers(path, contents)
				#problem must create dir if dir does not exist
				self.ensure_dir((str(self.rootdir))+path)				
				f = open((str(self.rootdir))+path,'w')
				f.write(contents)
				f.close()
				self.unlock(path)
				return "HTTP/1.0 200 OK\r\nContent-length: 0\r\n\r\n"
			except IOError:
				return "HTTP/1.0 500 Internel server Error\r\nContent-length: 0\r\n\r\n	"	
		
	def appendf(self, path, contents, username, password):
		if os.path.isfile((str(self.rootdir))+path):
			#authenticate user
			if self.islocked(path):
				return "HTTP/1.0 403 Access Denied (Locked)\r\nContent-length: 0\r\n\r\n"
			else:
				try:
					self.lock(path)
					f = open((str(self.rootdir))+path,'a')
					f.write(contents)
					f.close()
					self.unlock(path)
					return "HTTP/1.0 200 OK\r\nContent-length: 0\r\n\r\n"
				except IOError:
					return "HTTP/1.0 500 Internel server Error\r\nContent-length: 0\r\n\r\n"	
		else:
			return "HTTP/1.0 404 File not found\r\nContent-length: 0\r\n\r\n"
					
	def deletef(self, path, username, password):
		if os.path.isfile((str(self.rootdir))+path):
			#authenticate user
			if self.islocked(path):
				return "HTTP/1.0 403 Access Denied (Locked)\r\nContent-length: 0\r\n\r\n"
			else:
				try:
					self.lock(path)
					os.remove((str(self.rootdir))+path)
					self.unlock(path)
					return "HTTP/1.0 200 OK\r\nContent-length: 0\r\n\r\n" 
				except IOError:
					return "HTTP/1.0 500 Internel server Error\r\nContent-length: 0\r\n\r\n"	
		else:
			return "HTTP/1.0 404 File not found\r\nContent-length: 0\r\n\r\n"
	
	def head(self,):
		return 'not implemented'

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
			while lenrecv < conlen:					
				data = self.client_socket.recv(1024)
				lenrecv = lenrecv + len(data)
				buffer = buffer + data 
			return buffer
	
	#sends info over socket connection		
	def secsend(self, data):
		self.client_socket.send(data)
		
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
		
	def processrequest(self, requesttype, path, contents, username, password ):
		if requesttype == "GET":
			print requesttype, path
			return self.readf(path, username, password)				
		if requesttype == "PUT":
			print requesttype, path
			return self.appendf(path, contents, username, password)	
		if requesttype == "POST":
			print requesttype, path
			return self.writef(path, contents, username, password)	
		if requesttype == "DELETE":
			print requesttype, path
			return self.deletef(path, username, password)	
		if requesttype == "HEAD":
			print "fixme head"
			return self.head(path, username, password)
			
	def decrypt(self, msg):
		f = open("serverkeys/"+str(self.serverid))
		key = f.read()
		encryptor = ARC4.new(key)
		ticket = encryptor.decrypt(msg)
		return ticket.split('\r\n')[0], ticket.split('\r\n')[1], ticket.split('\r\n')[2]
	
	def encryptresponse(self, data):
		encryptor = ARC4.new(self.sessionkey)
		return encryptor.encrypt(data)
	
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
	
	#write to replicas
	def writepeers(self, path, contents):
		#get peers from dir server
			#do Auth			
			ticket, sessionkey = self.doauth(0)
			servers = self.getpeers(path, ticket, sessionkey)
			servers = servers.split(' ')			
			servers.remove('')	
			for server in servers:
				#write to each peer
				self.writes(server, path, contents)		
			return True

	#Do Auth
	def doauth(self,serverid):
		conn = secureconnection(authhost,authport)
		conn.sendmsg(self.username, self.password,'HEAD', '/', str(serverid)+"\r\nnokey")
		token = conn.recvmsg()
		conn.close()
		ticket, sessionkey, c = self.decrypttoken(token,password)
		return ticket, sessionkey

	def getpeers(self, path, ticket, sessionkey):
		#make request
		conn = secureconnection(self.dirhost, self.dirport)
		conn.sendmsg(self.username, self.password,'HEAD', path , ticket)
		resp = conn.recvmsg()
		conn.close()
		#extract data 
		headers = resp.split('\r\n\r\n')[0]
		respline =  headers.split('\r\n')[0]
		code = int(respline.split(' ')[1]) # response code
		body = resp.split('\r\n\r\n')[1]
		#decript body		
		decryptor = ARC4.new(sessionkey)
		body = (decryptor.decrypt(body))		
		return body

	#does actual file write to replicas
	def writes(self, server, path, contents):
			#get server details
			fshost = server[0:server.index(':')]
			fsport = int(server[server.index(':')+1:server.rfind(':')])
			fssid  = server[server.rfind(':')+1:] 
			#quit if for this server
			if fssid == self.getid():
				pass
			else:
				print 'writing to a replica of', path, 'sid: ', fssid
				#get token from Auth server
				ticket, sessionkey = self.doauth(fssid)
				#make request to file server
				conn2 = secureconnection(fshost, fsport)
				conn2.sendmsg(self.username, self.password,'POST', path, ticket+contents)
				resp = conn2.recvmsg()
				conn2.close()
				
	def decrypttoken(self, message, passwd):
		start = message.index("\r\n\r\n")+4
		end = message[start:].index("\r\n\r\n") +start
		message = message[start:end]
		decryptor = ARC4.new(passwd)
		message = decryptor.decrypt(message)
		ans = message.split("\r\n")
		return ans[0]+"\r\n\r\n", ans[1], ans[2]	

	def ensure_dir(self, f):
		d = os.path.dirname(f)
		if not os.path.exists(d):
        		os.makedirs(d)
			


class fileserver:

	def __init__(self, username, password, authhost, authport, rootdir):

		#setup
			#save some args			
			self.rootdir = rootdir
			self.username = username
			self.passwd = password

			#setup socket and get my port
			self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			self.s.bind(('', 0))
			port = self.s.getsockname()[1]
			host = socket.getfqdn()
			self.host = host+':'+str(port)
		
		#register with auth server
			
			#generate key
			key = RSA.generate(1024)
			self.key = key.exportKey()
			msg = str(key)+'\r\n'
			
			#send data
			conn = secureconnection(authhost,authport)
			conn.sendmsg(username, password,'GET', '/', '0'+"\r\n"+self.key)
			token = conn.recvmsg()
			conn.close()
			ticket, self.sessionkey, self.serverid = self.decrypttoken(token,password)
			self.dirhost, self.dirport = self.getdirloc(token)

			print "my sid:", self.serverid
			
		#register with dirserver
			request = self.host+'\r\n'+self.serverid+'\r\n'+str(self.getfolders())
			conn = secureconnection(self.dirhost, int(self.dirport))
			conn.sendmsg(username, password,'GET', '/', ticket+request)
			token = conn.recvmsg()
			conn.close()
			
	def decrypttoken(self, message, passwd):
		start = message.index("\r\n\r\n")+4
		end = message[start:].index("\r\n\r\n") +start
		message = message[start:end]
		decryptor = ARC4.new(passwd)
		message = decryptor.decrypt(message)
		ans = message.split("\r\n")
		return ans[0]+"\r\n\r\n", ans[1], ans[2]

	def getdirloc(self, message):
		headers = message.split('\r\n\r\n')[0]
		#extract Location
		headers = headers.split('\r\n')
		for item in headers:
			if item.split(' ')[0] == 'Location:':
				location = item.split(' ')[1]
		host = location[:location.index(':')]
		port = location[location.index(':')+1:location.index('/')]
		return host, port

	def getfolders(self):
		self.files = []
		self.folders = []
		for root, dirs, files in os.walk(self.rootdir):
			for item in files:
				self.files.append(("/"+ str(item))[2:])
	 		if (str(root))[len(self.rootdir):] != '':
		 		self.folders.append((str(root))[len(self.rootdir):])
	 	return ' '.join(self.folders)	

	def serve(self):
			#listen for connections 
			self.s.listen(5)
			while 1:
				#spawn new connections
				(newsocket, address) = self.s.accept()
				t = fileservice(newsocket,self.rootdir, self.serverid, self.dirhost, self.dirport, self.username, self.passwd)
				t.start()

username = 'fserv1'
password = 'pass1'
authhost = sys.argv[1]
authport = int(sys.argv[2])
rootdir = sys.argv[3]

server = fileserver(username, password, authhost, authport, rootdir)
server.serve()
