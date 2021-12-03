# Data Preparation

## Geospatial Data on Casco Bay Lakes
Geospatial data on Casco Bay lakes and ponds with associated MIDAS numbers is 
provided in the  [Maine_lakes_MIDAS](https://github.com/ccb60/Maine_Lakes_MIDAS)
GitHub repository.  Details of preparing the geospatial data is also
provided there.

In brief, we extracted MIDAS numbers and geospatial data on Maine lake centroids
from a KML file made available by Maine DEP.  We selected centroids located in
the Casco Bay watershed, and matched centroids with polygon data from the
National Hydrography Database. We conducted careful QA/QC to identify missing or
duplicated lake polygons.

## Generating a CSV File
For our analysis here, we needed to work with a list of MIDAS numbers derived
from the geospatial data, so we would restrict our attention only to lakes 
within the Casco Bay watershed.

We exported the attribute table of "CB_Lakes" (a shapefile) to "CB_Lakes.csv".
We then edited that file (by hand) to remove the FID column, and revised the
column names as follows

*  ExtName
*  Longitude
*  Latitude
*  Name
*  Town
*  MIDAS Number
*  Elevation
*  Notes
*  Name Change

Finally, we deleted records with duplicate MIDAS numbers, selecting
The point that corresponds to the largest contiguous pond area and deleting
others.  We edited the Notes field to indicate what we did.

# Considerations for Further Data Organization
The downloaded lakes data has essentially three types of data:
1.  Lake identification, location, and Morphometry, which is lake specific and
    time independent data.
2.  Secchi depth, which is station specific (within lakes) and Date dependent.
    (Includes a TIME field, apparently for QA purposes).
3.  All other parameters, which are  lake, station, depth, and date dependent.

However, we may want to analyse depth profiles separately, since they are so
important for understanding the physics of the lakes, and so important in terms
of climate change impacts on lakes.

So this suggests we want to reorganize the data into four tables, as follows:
*  Morphometry
*  Secchi
*  Temp and DO
*  All Other Observations

## Sebago Lake Sampling Locations
We used ArcGIS to transfer positions of sampling Stations from DEP and PWD maps
to GIS, using standard methods for georeferencing images. Once approximate
Station positions were transferred to ArcGIS, we calculated a distance matrix,
in meters and kilometers, describing distances among all sampling locations.  We
reordered that matrix in Excel to identify sampling Regions, where Stations are
all within five kilometers of each other. Distance matrix and assignment
to regions are available here via an excel file:  
'Sample Point Distances and Regions.xlsx'.
