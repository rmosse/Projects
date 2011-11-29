#!/usr/bin/env python
from mnfs import api as distfilesys

def main(newdata):
	fs = distfilesys('user','password' ,'localhost', '10000')
	olddata = fs.read('/folder1/file1')
	fs.open('/folder1/file1')
	fs.write('/folder1/file1', newdata)
	fs.close('/folder1/file1')
	newdata = fs.read('/folder1/file1')	
	return olddata, newdata

print main('newdata')
