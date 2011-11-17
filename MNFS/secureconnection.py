import socket
import base64
import ssl
import sys
import re
class secureconnection:
	

	def __init__(self, host, port):
			self.host = host
			self.port = port
			self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			try:
				self.s.connect((host,port))
			except socket.error, msg:
				print "connect failed"
				sys.exit()
			try:
				self.s= ssl.wrap_socket(self.s, cert_reqs=ssl.CERT_REQUIRED,ca_certs="certs/cert")
			except ssl.SSLError, msg:
				print "cert could not be verified"
				sys.exit()
				
	def sendmsg(self, username, password, reqtype, path, msg):
			token= base64.encodestring('%s:%s' % (username, password)).strip()
			lines= [
				reqtype+' %s HTTP/1.1' % path,
				'Host: %s' % self.host,
				'Authorization: Basic %s' % token,
				'Content-length: %s' % len(msg)
			]
			req = ('\r\n'.join(lines)+' \r\n\r\n')+msg
			length = len(req)
			if  length < 1024:
				self.s.send(req)
			else:
				data = list(self.chunks(req,1024))
				for i in range(0,len(data)):
					self.s.send(data[i])
					

	def chunks(self, l, n):
	    for i in xrange(0, len(l), n):
	         yield l[i:i+n]
		

	def recvmsg(self):
			buffer = self.s.recv(1024)
			conlen = self.getlength(buffer)
			lenrecv =  (len(buffer)) - ((buffer.index("\r\n\r\n")) + 4)
			if lenrecv >= conlen:
				return buffer
			else:
				while not (lenrecv <= conlen):			
					data = self.s.recv(1024)
					lenrecv = lenrecv + len(data)
					buffer = buffer + data
				return buffer
				
	
	def getlength(self, headers):
			for item in headers.split('\r\n'):
				if item.split(' ')[0] == "Content-length:":
					return int(item.split(' ')[1])
						
						
			
	def close(self):
			self.s.close()
	

