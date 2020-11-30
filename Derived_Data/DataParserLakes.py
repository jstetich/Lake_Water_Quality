'''
Extracts MIDAS Lake Centroids from a KML file. (doc.kml)
Most of the data we want is located in the description tag of each placemark.
However, the latitude and longitude of the lake centroids is located
in the <Point><coordinates> tag of hte 

Filenames, etc. are hard coded for simplicity.  Refers to files in the
same folder as where the script is run.

**  Rewritten to use BeautifulSoup Nov 26, 2019, which simplifies the code.

**  Updated to use pathlib class Paths rather than os.path text manipulation 
in November of 2020.

'''
import os
import csv
from pathlib import Path
from bs4 import BeautifulSoup as bs

#TODO:  refactor to use BeautifulSoup

########################
#Set Up Header Row
########################
LakeHeader =  ['Name', 'Town', 'MIDAS Number', 'Latitude', 'Longitude',
               'Elevation', 'Notes', 'Name Change']

##########################
#Setup filenames and other parameters
##########################
myPath =  os.getcwd()
parent = os.path.dirname(myPath)

LakesFileName = 'lakes.csv'
KMLFileName = 'doc.kml'

##########################
#Setup filepaths
##########################
rootdir =  Path(os.getcwd())   # os.getcwd() points to the python file home

LakeFileName = 'lakes.csv'
KMLFileName = 'doc.kml'

myPath = rootdir               # Allows for indirection if necessary

KMLpath = myPath / KMLFileName
Lakepath  = myPath / LakeFileName


#########################
#Open the CSV file that will hold the output
#########################
with open(Lakepath, 'w') as lakefile:

    # Initialize it as a CSV DictWriter
    try:
        lakewriter = csv.DictWriter(lakefile,  LakeHeader,
                                       extrasaction='ignore', dialect='excel',
                                       lineterminator = '\n')
    except:
        print ("Could not open files as CSV")
        raise
    try:
        #Write the CSV header rows
        lakewriter.writeheader()  
    except:
        print ('Could not write the header row')
        raise
    
    ##################
    #Open the XML File
    ##################
    with open(KMLpath, 'r') as kmlfile:
        ##################
        #Parse the KML File
        ##################
        asoup = bs(kmlfile, 'lxml-xml')
        pms = asoup.find_all('Placemark')
        for pm in pms:
            dat = dict()    # create an empty dictionary  
            dat['Longitude'], dat['Latitude'],_ = pm.coordinates.text.split(',')  
            d = pm.description.text
            
            ####################
            # Parse the enclosed HTML
            ####################
            innersoup = bs(d, 'lxml')
            for row in innersoup.table.find_all('tr'):
                dat[row.contents[0].text[:-1]] = row.contents[1].text
            lakewriter.writerow(dat)
         
         




