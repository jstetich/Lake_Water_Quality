# Data Sources 

## Maine Lakes and Ponds
###  Geospatial Data
Maine Geolibrary / Maine Office of GIS no longer provides a lakes data layer
containing the MIDAS numbers of all Maine lakes. They refer users to the
National Hydrography Dataset for all hydrologic features, but the National
Hydrography Database does not contain MIDAS numbers.

We need the MIDAS numbers to connect water quality monitoring data unambiguously
with specific Maine Lakes, because many Maine lakes have similar names.

Geospatial Data on Maine lake centroids was downloaded as a KMZ file from
Maine DEP's website by Curtis C. Bohlen on November 23, 2020. Details of 
how to access these data, and combine them with the national hydrography
database are provided in the
[Maine_Lakes_MIDAS](https://github.com/ccb60/Maine_Lakes_MIDAS)
repository.

## Water Quality Data
Maine's Department of Environmental Protection aggregates data on water
quality from volunteer water quality monitors, state agencies and other
sources, and posts it on-line.  Data and information is accessible on a lake
by lake basis via the [Lakes of Maine](https://www.lakesofmaine.org/) website. 
Aggregate data is housed as excel files on the website of the 
[Gulf of Maine Council for the Marine Environment](http://www.gulfofmaine.org),
as part of their "Knowledge Base".

Because the actual data is buried several layers deep on these web sites, we
created a Python 3 script, `ExtractLakeWQData.py` to access and download the
state-wide lake water quality data. URLs and many other details are hard
coded in the script. 

Available raw water quality data includes:
1.  Chlorophyll A
2.  pH, alkalinity and related water quality parameters
3.  Phosphorus concentrations
4.  Secchi Depths
5.  Vertical temperature and dissolved oxygen profiles

Summary tables(both annual, and including all records) provide averages for
water quality parameters, including:  
*  Secchi Depth,  
*  Color,   
*  Chlorophyll A,  
*  pH,  
*  Alkalinity,  
*  Conductivity,  
*  Total Phosphorus, and  
*  Trophic State Index (TSI) calculated several different ways.  
Measures of variability are not provided.  Sample sizes are not clear (number of
"months" of data are indicated, not number of observations). Right Censored
Secchi Depths are not fully addressed.

## Lake Morphometry
Data on morphometry of many Maine lakes and ponds was accessed here:
http://www.gulfofmaine.org/kb/files/9680/MaineLakes_Geography_Morphometry.xls

# Sebago Lake Supplementary Data
In data review, we realized the Sebago Lake data from the on-line repository
was incomplete, with little data included in the DEP data from Sebago Lake
since 2010.

We requested supplementary data from Portland Water District, which manages
long-term Sebago Lake Monitoring, and received the file
'Sebago export 2_24_2020.xlsx'.  PWD staff informed us that they share their
data with DEP electronically, so it may not get incorporated
into the on-line data, which is gathered principally through volunteer lake
associations and the VLMP.

Data was e-mailed to Curtis C. Bohlen from Nate Whalen, at Portland Water
District, on December 7, 2020.  Data file name suggests data was complete as
of 2/24/2020, and it includes data through 2019.

In Separate e-mail (Dec 8, 2020) in response to some questions, Nate said:

> The UserVarEJ column in the Event tab is Secchi Data collected with a view
scope with no mask.  There was a correction factor applied to account for this
discrepancy.  I have kept the data for scientific rigor.  I would ignore the
UserVarEJ values and simply use the SecchiDepth column.  The decimal places are
an aberration from the statistical software export.  We only report one decimal
place.

## Sebago Lake Sampling Locations
We located a scanned map of sebago lake sampling locations on the
[Lakes of Maine website](https://www.lakesofmaine.org/lake-monitoring.html?m=5786)
It is saved as"Station map for Sebago Lake.jpg"

We requested geographic information on PWD (professional) sample locations from
PWD staff, on December 12, 2020, and received the file 'Historical Lake Profile
Sample Locations.jpg' from Nathan Whalen that same day.
