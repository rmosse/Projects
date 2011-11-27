import base64
import xmlrpclib
from Crypto.Cipher import ARC4

class mnfs:
	def __init__(self, user, passwd, authhost, authport):
		self.user = user
		self.passwd = passwd
		self.authhost = authhost 
		self.authport = authport


#file actions 

	#read a file from the filesystem	
	def read(self, filen):
		#get ticket for dirserver		
		enctoken, self.dirhost, self.dirport = self.authenticate('0')
		ticket, self.dssessionkey, serverid, timestamp = self.decryptToken(enctoken)
		#make request to dirserver
		dirproxy = xmlrpclib.ServerProxy('http://'+self.dirhost+':'+str(self.dirport)+'/')
		success, response = dirproxy.getlocation(ticket, self.encryptmsg(filen, self.dssessionkey)) 
		if success:
			fshost, fsport, serverid = self.decryptmsg(response, self.dssessionkey).split(':')
		else:
			raise IOError('[Errno 2] No such file or directory: '+filen)
		#get ticket for fileserver
		enctoken = self.authenticate(serverid)
		ticket, sessionkey, serverid2, timestamp = self.decryptToken(enctoken)
		#make request to fileserver
		fsproxy = xmlrpclib.ServerProxy('http://'+fshost+':'+str(fsport)+'/')
		data , ts = fsproxy.read(ticket, self.encryptmsg(filen, sessionkey)) 
		if (self.decryptmsg(ts ,sessionkey) == timestamp):
			return self.decryptmsg(data,sessionkey)
		else:
			print self.decryptmsg(ts ,sessionkey), timestamp
			raise IOError('rogue agent')			



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
	

fs = mnfs('user','password' ,'localhost', '10000')
print fs.read('/folder1/file1')
