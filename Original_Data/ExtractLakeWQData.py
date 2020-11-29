# -*- coding: utf-8 -*-
"""
A script to search the Gulf of Maine Council web site for aggregate data on
Maine Lakes, and return those raw data files.

The basic idea is to look at one particular lake, specifically, Falmouth and 
Westbrook's Highland Lake, on the Lakes of Maine website and follow the links
there to a list of related data, and scrape the links provided there.

Created on  11/26/2019 to print URLs
Modified on 11/24/ to download the files.

@author: curtis.bohlen
Casco Bay Estuary Partnership
"""
import os
import requests
from bs4 import BeautifulSoup as bs
from urllib.parse import urlparse

# The following URL is a couple of levels down in the GOMC webpage for 
# Highland lake.  It structures a sub-page (a panel under the "Documents
# and Data" tab) that you can click to directly from the web site.

url = r'https://www.gulfofmaine.org/kb/dand-lome.html?c=3734'

response = requests.get(url)

#%%
MWCSources = {}
soup = bs(response.text, 'lxml')
# References to data sources are bundled into <div> tabs. 
# The actual link is embeded in a <big> tag, while a related description is
# under a <small> tag.
for d in soup.find_all('div'):
    #print(d.big)
    if d.big:
        txt = d.big.text
        # Maine Lakes Data is labled consistently
        if 'Maine lakes water quality' in txt:
            print(txt[28:], '.....')
            print ('.....', d.big.a.get('href'))
            MWCSources[txt[28:]]= d.big.a.get('href')
        elif 'Maine lakes' in txt[0:11] or 'Maine Lakes' in txt[0:11]:
            print(txt[12:], '.....')
            print ('.....', d.big.a.get('href'))
            MWCSources[txt[28:]]= d.big.a.get('href')
print('\n\n\n')

#%%

# It turns out, as I request those URLs, they are directing me to another
# webpage, not to the data itself. In addition, the request is being redirected
# to a slightly different URL. To send an automated request to the URL,
# we need to use the "real" URL.
#  From:  http://www.gulfofmaine.org/kb/record.html?recordid=9214
#  To:    http://www.gulfofmaine.org/kb/2.0/record.html?recordid=9214
# We need to "insert" "2.0/" after "kb/"
# We do that using simple text manipulation.

fstr = 'kb/'
istr =  '2.0/'

MWCSources2=dict()
for k, url in MWCSources.items():
    idx = url.index(fstr)
    newurl = url[:idx+len(fstr)] + istr  + url[idx+len(fstr):]
    MWCSources2[k]=newurl
    
#%%
# Now that we have the real URL to a webpage describing a data set, we need
# to follow links to find the actual data files.

final_urls = list()
for item in MWCSources2.values():
    response2 = requests.get(item)
    soup2 = bs(response2.text, 'lxml')
    theurl = soup2.table.a['href']
    final_urls.append(theurl)
#%%
# Finally, we download the most recent version of the data.
for theURL in final_urls:
    a = urlparse(theURL)
    fn = (os.path.basename(a.path))
    r = requests.get(theURL, allow_redirects=True)
    print(theURL)
    open(fn, 'wb').write(r.content)
    
