import base64
import xmlrpclib
from Crypto.Cipher import ARC4

class mnfs:
	def __init__(self, user, passwd, authhost, authport):
		self.user = user
		self.passwd = passwd
		self.authenticate( authhost, authport)
		
	def authenticate(self, authhost, authport):
		proxy = xmlrpclib.ServerProxy('http://'+authhost+':'+authport+'/')
		token = proxy.authenticateUser(self.user,'0')
		print self.decryptToken(token)

	def decryptToken(self, token):
		token = base64.decodestring(token)
		decryptor = ARC4.new(str(self.passwd))
		return decryptor.decrypt(token)

		
fs = mnfs('user','password' ,'localhost', '10000')
