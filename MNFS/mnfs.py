import sys
from secureconnection import secureconnection
from Crypto.Cipher import ARC4

class filesys:

	def __init__(self, username, password, AuthserverHost, Authserverport):
		self.ahost = AuthserverHost
		self.aport = Authserverport
		self.user = username 
		self.passwd = password
		
	def read(self, filePath):
		
		#get token from Auth server
		conn = secureconnection(self.ahost, self.aport)
		conn.sendmsg(self.user, self.passwd,'GET', filePath, "0\r\nnokey")
		resp = conn.recvmsg()
		conn.close()
		#check ok
		code , a, b, c = self.parsehttp(resp)
		if code == 302:
			#extract details from token
			ticket, sessionkey, serverid = self.decrypttoken(resp)
			
			#make request to dir server
			code, dhost, dport, fserverid = self.parsehttp(resp)
			conn2 = secureconnection(dhost, dport)
			conn2.sendmsg(self.user, self.passwd,'GET', filePath, ticket+"\r\nnokey")
			resp = conn2.recvmsg()
			conn2.close()
			

			#make request to file server
			code, fhost, fport, fserverid = self.parsehttp(resp)
			if code == 302:
			#Authorize request
				#get token from Auth server
				conn = secureconnection(self.ahost, self.aport)
				conn.sendmsg(self.user, self.passwd,'GET', filePath, fserverid+"\r\nnokey")
				token = conn.recvmsg()
				conn.close()

				#extract ticket
				ticket, sessionkey, serverid = self.decrypttoken(token)

			#Make request
				conn2 = secureconnection(fhost, fport)
				conn2.sendmsg(self.user, self.passwd,'GET', filePath, ticket+"\r\nnokey")
				resp = conn2.recvmsg()
				conn2.close()
			msg =  (self.parsehttp(resp)[3])
			return self.decryptresponse(msg,sessionkey)
		else:
			return "http error:" + str(code)
	
	
	def write(self, filePath, contents):
		
		#get token from Auth server
		conn = secureconnection(self.ahost, self.aport)
		conn.sendmsg(self.user, self.passwd,'POST', filePath, "0\r\nnokey")
		resp = conn.recvmsg()
		conn.close()
		
		#check ok
		code , a, b, c = self.parsehttp(resp)
		if code == 302:
			#extract details from token
			ticket, sessionkey, serverid = self.decrypttoken(resp)
			
			#make request to dir server
			code, dhost, dport, fserverid = self.parsehttp(resp)
			conn2 = secureconnection(dhost, dport)
			conn2.sendmsg(self.user, self.passwd,'POST', filePath, ticket+"\r\nnokey")
			resp = conn2.recvmsg()
			conn2.close()

			#make request to file server
			code, fhost, fport, fserverid = self.parsehttp(resp)
			print 'code: ', code
			if code == 302:
			#Authorize request
				#get token from Auth server
				conn = secureconnection(self.ahost, self.aport)
				conn.sendmsg(self.user, self.passwd,'POST', filePath, fserverid+"\r\nnokey")
				token = conn.recvmsg()
				conn.close()

				#extract ticket
				ticket, sessionkey, serverid = self.decrypttoken(token)

				#Make request
				conn2 = secureconnection(fhost, fport)
				conn2.sendmsg(self.user, self.passwd,'POST', filePath, ticket+contents)
				resp = conn2.recvmsg()
				conn2.close()
				
			return (self.parsehttp(resp)[3])[:-2]
		else:
			return "http error:" + str(code)
	
	
	
	def append(self, filePath, contents):
		
		#get token from Auth server
		conn = secureconnection(self.ahost, self.aport)
		conn.sendmsg(self.user, self.passwd,'PUT', filePath, "0\r\nnokey")
		resp = conn.recvmsg()
		conn.close()
		#check ok
		code , a, b, c = self.parsehttp(resp)
		if code == 302:
			#extract details from token
			ticket, sessionkey, serverid = self.decrypttoken(resp)
			
			#make request to dir server
			code, dhost, dport, fserverid = self.parsehttp(resp)
			conn2 = secureconnection(dhost, dport)
			conn2.sendmsg(self.user, self.passwd,'PUT', filePath, ticket+"\r\nnokey")
			resp = conn2.recvmsg()
			conn2.close()

			#make request to file server
			code, fhost, fport, fserverid = self.parsehttp(resp)
			if code == 302:
			#Authorize request
				#get token from Auth server
				conn = secureconnection(self.ahost, self.aport)
				conn.sendmsg(self.user, self.passwd,'PUT', filePath, fserverid+"\r\nnokey")
				token = conn.recvmsg()
				conn.close()

				#extract ticket
				ticket, sessionkey, serverid = self.decrypttoken(token)

			#Make request
				conn2 = secureconnection(fhost, fport)
				conn2.sendmsg(self.user, self.passwd,'PUT', filePath, ticket+contents)
				resp = conn2.recvmsg()
				conn2.close()

			return (self.parsehttp(resp)[3])[:-2]
		else:
			return "http error:" + str(code)
	
	def delete(self, filePath):
		
		#get token from Auth server
		conn = secureconnection(self.ahost, self.aport)
		conn.sendmsg(self.user, self.passwd,'DELETE', filePath, "0\r\nnokey")
		resp = conn.recvmsg()
		conn.close()
		#check ok
		code , a, b, c = self.parsehttp(resp)
		if code == 302:
			#extract details from token
			ticket, sessionkey, serverid = self.decrypttoken(resp)
			
			#make request to dir server
			code, dhost, dport, fserverid = self.parsehttp(resp)
			conn2 = secureconnection(dhost, dport)
			conn2.sendmsg(self.user, self.passwd,'DELETE', filePath, ticket+"\r\nnokey")
			resp = conn2.recvmsg()
			conn2.close()

			#make request to file server
			code, fhost, fport, fserverid = self.parsehttp(resp)
			if code == 302:
			#Authorize request
				#get token from Auth server
				conn = secureconnection(self.ahost, self.aport)
				conn.sendmsg(self.user, self.passwd,'DELETE', filePath, fserverid+"\r\nnokey")
				token = conn.recvmsg()
				conn.close()

				#extract ticket
				ticket, sessionkey, serverid = self.decrypttoken(token)

			#Make request
				conn2 = secureconnection(fhost, fport)
				conn2.sendmsg(self.user, self.passwd,'DELETE', filePath, ticket)
				resp = conn2.recvmsg()
				conn2.close()

			return (self.parsehttp(resp)[3])[:-2]
		else:
			return "http error:" + str(code)
	
	#decrypt the response using session key
	def decryptresponse(self, message, sessionkey):
		decryptor = ARC4.new(sessionkey)
		return decryptor.decrypt(message)
		
	#extract ticket, session key, and serverid from the token
	def decrypttoken(self, message):
		start = message.index("\r\n\r\n")+4
		end = message[start:].index("\r\n\r\n") +start
		message = message[start:end]
		decryptor = ARC4.new(self.passwd)
		message = decryptor.decrypt(message)
		ans = message.split("\r\n")
		return ans[0]+"\r\n\r\n", ans[1], ans[2]
	
	#splits up the http response and extracts meaningful data
	def parsehttp(self, resp):
		#extract headers
		headers = resp.split('\r\n\r\n')[0]
		headers = headers.split('\r\n')
		respline = headers[0]
	
		#response code
		code = int(respline.split(' ')[1])
		try:
			#extract Location
			for item in headers:
				if item.split(' ')[0] == 'Location:':
					location = item.split(' ')[1]
	
			host = location[:location.index(':')]
			port = location[location.index(':')+1:location.index('/')]
			#extract body
			body = resp.split('\r\n\r\n')[1]
			return code, host, int(port), body
		except:
		
			return code ,'NA', 0, resp.split('\r\n\r\n')[1] 
		

