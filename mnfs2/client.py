#!/usr/bin/env python
from mnfs import api as distfilesys

def main():
	#read data to submit to filesys
	f = open ('testimg.png','r')
	data = f.read()

	#initialize new filesystem
	fs = distfilesys('user','password' ,'localhost', '10000')
	fs.open('/folder1/file3')
	fs.write('/folder1/file3', data)
	
	
	#write to output file
	newdata = fs.read('/folder1/file3')
	f = open('output.png','w')
	f.write(newdata)
	

main()
