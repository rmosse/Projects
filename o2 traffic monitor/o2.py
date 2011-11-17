#!/usr/bin/env python
import sys
import mechanize 
import os.system
br = mechanize.Browser()
br.set_handle_equiv(True)
br.set_handle_referer(True)
br.set_handle_robots(False)
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)
br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

r = br.open('https://www.o2online.ie/NASApp/MyAccount/PostpaidBroadbandServlet.htm')
br.select_form(nr=2)

br.form['IDToken1']='08x xxxxxxx'
br.form['IDToken2']='password'

r = br.submit()
html = r.read()

start = html.index('Data used')
start = start + 4 + html[start:].index('<td>')
end = start + html[start:].index('</td>')
print html[end -1]
if str(html[end -1]) == "G":
	print float(html[start:end -1])
	if float(html[start:end-1]) > 1.6:
		print "greater"
	os.system("php mail.php")
