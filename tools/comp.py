# -*- coding: utf-8 -*-
import urllib2, urllib, cookielib
import httplib
import re
import json
import argparse


def newOpener(userAgent,proxy=""):
	cj = cookielib.CookieJar()
	if proxy !="":
		proxy_support = urllib2.ProxyHandler({"http":"http://"+str(proxy)})
		opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj),proxy_support)
	else:
		opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))
	opener.addheaders.append(('User-agent', userAgent))
	return opener

def postStuff(opener,site,postData, refer="",printResp =False):
	postData = urllib.urlencode(postData)
	if refer != "":
		opener.addheaders.append( ('Referer', refer) )
	
	resp = opener.open(site, postData,20)
	if printResp:
		return resp.read();

def parseArgs():
	ap = argparse.ArgumentParser()
	group = ap.add_mutually_exclusive_group(required=True)
	group.add_argument('-i', nargs=1, help='input file')
	ap.add_argument('-o', nargs=1, help='output file; use filename "std" to wirite to std')
	return ap.parse_args()

def main():
	fileName = "out.c"
	args = parseArgs()
	coreCode = "".join([line for line in open(args.i[0])])
	o = newOpener(["Python compiler bot 1.0"])
	site = "http://www.rtfm-lang.org/remote_compile/request.php"
	data = [	("message_contents", "core"),
				("compiler_version","v1.0"),
				("param_D","false"),
				("param_d_ast","false"),
				("param_v","false"),
				("source_code",coreCode)
			]
	print "post"
	newData = postStuff(o,site,data,"",True)
	print newData
	retData = json.loads(newData)
	print retData['compiler_output']
	if args.o[0].lower() ==  "std":
		print retData['code_output']
	
	if args.o:
		fileName = args.o[0]

	with open(fileName,'w') as output:
		output.write(retData['code_output'])

if __name__ == "__main__":
  main()