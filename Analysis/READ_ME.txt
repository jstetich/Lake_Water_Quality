
# GIS Data For Display

At the scale of the watershed, most lakes and ponds are too small to notice, so 
we need to use point features to symbolize lake condition (at least in part).  
The CB_Lakes layer still contains duplicate MIDAS numbers, as described in the 
"DATA_NOTES.md" file for the "Derived_Data" folder.  We deleted duplicated MIDAS
numbers as  follows:

MIDAS  |   Name    | Action
-------|-------------------|-------------
3402   | Otter Pond #1     | This is a single small pond, divided by a road. Delete Otter Pond #1 (North)
3445   | Rich Millpond     | A pond divided by a road.  Delete Rich Millpond North.
3714   | Little Sebago Lake|  Delete North Basin.
5786   | Sebago Lake Basin | 
5786   |Sebago Lake (three more locations) | These are all part of Sebago Lake, although distinct subbasins.  We delete all but the most central Sebago Lake Basin, which has no notes (the three subbasins are so noted in the notes attribute).

