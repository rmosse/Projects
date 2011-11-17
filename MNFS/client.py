import mnfs 
import sys 
#setup api user, pass, authserverhost authserverport
mynfs = mnfs.filesys('fred','bloggs','localhost', int(sys.argv[1]))
f = open('/home/mosser/Pictures/proxyset_ss.png', 'r')
data = f.read()
print mynfs.write('/folder3/proxyset_ss.png', data)
