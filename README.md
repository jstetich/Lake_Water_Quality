# Lake_Water_Quality
Access and analysis of data on water quality in Maine lakes and ponds,
especially those in the Casco Bay watershed.

# Introduction
Maine's Department of Environmental Protection aggregates data on water
quality from volunteer water quality monitors, state agencies and other
sources, and posts it on-line.  Data and information is accessible on a lake
by lake basis via the [Lakes of Maine](https://www.lakesofmaine.org/) website. 
Aggregate data is housed as excel files on the website of the 
[Gulf of Maine Council for the Marine Environment](http://www.gulfofmaine.org),
as part of their "Knowledge base".

Because the actual data is buried several layers deep on these web sites, we
created a Python 3 script, `ExtractLakeWQData.py` to access and download the
state-wide lake water quality data.

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
Measures of variability and sample size are not provided.

# MIDAS Numbers
Water quality data are linked to "MIDAS" numbers, which provide unique
identifiers for Maine lakes and ponds. On-line metadata describes the MIDAS
numbers as follows:  
>  MIDAS numbers are unique identification numbers assigned in the 1970's
   to Maine lakes and ponds monitored and managed by Maine state agencies.
   A collaborative effort between MEDEP and MDIF&W provided an update to
   MIDAS numbers, for Maine lakes and ponds, in 2003.  

Geospatial data for Maine Lakes and Ponds in the Casco Bay watershed 
(including MIDAS numbers) are available in the
[Maine_lakes_MIDAS](https://github.com/ccb60/Maine_Lakes_MIDAS) repository.