#!/usr/bin/env python
import subprocess
def getfilepos():
		p1 = subprocess.Popen(["lsof"],stdout=subprocess.PIPE)
		p2 = subprocess.Popen(["grep","/tmp/Flash"], stdin=p1.stdout, stdout=subprocess.PIPE)
		p1.stdout.close()
		output = p2.communicate()
		line = output[0].split('\n')
		return line 

paths = getfilepos()
for path in paths:
	try:
		path = path.split()
		path = '/proc/'+path[1]+'/'+'fd'+'/'+path[3][:-1]
		print 'vlc', path
		subprocess.call(["vlc", path])
	except:
		print 'error'

