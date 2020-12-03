Casco Bay Lakes Water Quality Data Aggregation
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/29/2020

  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Folder References](#folder-references)
      - [Lakes Geospatial Information](#lakes-geospatial-information)
      - [Load Lake Morphometry data](#load-lake-morphometry-data)
          - [Preparing the Morphometry
            Data](#preparing-the-morphometry-data)
          - [Merge Morphometry Data with Lake
            Lists](#merge-morphometry-data-with-lake-lists)
          - [Write Morphometry Data](#write-morphometry-data)
      - [List Other Excel files found in
        `Original_Data`](#list-other-excel-files-found-in-original_data)
          - [Reflections](#reflections)
      - [Read in Raw Data](#read-in-raw-data)
  - [Examine Data Organization](#examine-data-organization)
      - [Coding Method](#coding-method)
      - [Compare Column Names](#compare-column-names)
      - [Data Name Cleanup](#data-name-cleanup)
          - [Utility Functions for Renaming
            Variables](#utility-functions-for-renaming-variables)
          - [Function Fixing Names in a Data
            Frame](#function-fixing-names-in-a-data-frame)
          - [Apply the Function to Target Data
            Frames](#apply-the-function-to-target-data-frames)
  - [Checking Lake Context Data](#checking-lake-context-data)
      - [Check Uniformity of Lake Context
        Data](#check-uniformity-of-lake-context-data)
  - [Save Data for Selected Lakes](#save-data-for-selected-lakes)
  - [Cleanup Observational Data](#cleanup-observational-data)
      - [Color ‘AORT’ and ‘Color\_Method’](#color-aort-and-color_method)
      - [Redundancy in P data and P data “Repeat”
        Samples](#redundancy-in-p-data-and-p-data-repeat-samples)
      - [Pivot each data set to long
        form](#pivot-each-data-set-to-long-form)
          - [CHLA](#chla)
          - [Phosphorus](#phosphorus)
          - [pH and related data](#ph-and-related-data)
  - [Write out Sample Data](#write-out-sample-data)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Load Libraries

``` r
library(readxl)
library(readr)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v dplyr   1.0.2
    ## v tibble  3.0.3     v stringr 1.4.0
    ## v tidyr   1.1.2     v forcats 0.5.0
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

# Load Data

## Folder References

``` r
sisterfldnm <- 'Original_Data'
parent <- dirname(getwd())
sister <- file.path(parent,sisterfldnm)
```

## Lakes Geospatial Information

Note that this file is located in the `Derived_Data` folder, so we need
no folder indirection.

``` r
fn <- 'lakes.csv'
Maine_Lakes <- read_csv(fn, col_types = cols(`MIDAS Number` = col_integer())) %>%
  rename(Lake   = Name,
         MIDAS =  `MIDAS Number`,
         Name_Change = `Name Change`) %>%
  relocate(MIDAS, Lake, Town)

fn <- 'CB_Lakes.csv'
CB_Lakes <- read_csv(fn, col_types = cols(`MIDAS Number` = col_integer())) %>%
  rename(Lake   = Name,
         MIDAS =  `MIDAS Number`,
         Name_Change = `Name Change`) %>%
  relocate(MIDAS, Lake, Town)
```

We do have some duplicated MIDAS values. They appear to differ in
detail, and usually represent separated sub-basins of larger bodies of
water. We addressed this problem for Casco Bay lakes, but not for other
lakes in Maine. The solution is to review each replicate point
(manually) and decide how best to address the problem.

``` r
dups = Maine_Lakes$MIDAS[duplicated(Maine_Lakes$MIDAS)] %>% unique()
Maine_Lakes[Maine_Lakes$MIDAS %in% dups,]
```

    ## # A tibble: 186 x 8
    ##    MIDAS Lake    Town    Latitude Longitude Elevation Notes        Name_Change  
    ##    <int> <chr>   <chr>      <dbl>     <dbl>     <dbl> <chr>        <chr>        
    ##  1  2058 Abol D~ T2 R10~     45.8     -69.0       570 missing 24k~ <NA>         
    ##  2  2058 Abol D~ T2 R10~     45.8     -69.0       570 <NA>         <NA>         
    ##  3  2068 Abol P~ T2 R9 ~     45.8     -68.9       592 <NA>         <NA>         
    ##  4  2068 Abol P~ T2 R9 ~     45.8     -68.9       595 this featur~ <NA>         
    ##  5   982 Ambaje~ T1 R9 ~     45.7     -68.9       492 Pemadumcook~ <NA>         
    ##  6  5064 Bag Po~ Chain ~     45.3     -70.7      1275 Chain of Po~ <NA>         
    ##  7  3898 Balch ~ Acton       43.6     -71.0       558 Stump & Bal~ <NA>         
    ##  8  3624 Bear P~ Hartfo~     44.3     -70.3       373 <NA>         <NA>         
    ##  9  4048 Beaver~ Plymou~     45.9     -69.9      1073 Part of / c~ Yes - name c~
    ## 10  2858 Big Ea~ Eagle ~     46.3     -69.4       930 <NA>         <NA>         
    ## # ... with 176 more rows

But the list of Casco Bay Lakes is free of duplicated MIDAS numbers,
because we spent a fair amount of time hand editing it to get there.

``` r
dups = CB_Lakes$MIDAS[duplicated(CB_Lakes$MIDAS)] %>% unique()
CB_Lakes[CB_Lakes$MIDAS %in% dups,]
```

    ## # A tibble: 0 x 9
    ## # ... with 9 variables: MIDAS <int>, Lake <chr>, Town <chr>, ExtName <chr>,
    ## #   Longitude <dbl>, Latitude <dbl>, Elevation <dbl>, Notes <chr>,
    ## #   Name_Change <chr>

We make a list of Casco Bay MIDAS numbers, to facilitate selection of
data.

``` r
selected_lakes <- CB_Lakes %>% pull(MIDAS)
rm(fn)
```

## Load Lake Morphometry data

``` r
fn = 'MaineLakes_Geography_Morphometry.xls'
morpho_data <- read_excel(file.path(sister,fn), sheet = 'DATA',
                          col_types = c("skip", "numeric", "text", #midas numeric
                                        "numeric", "numeric", #area
                                        "numeric", "numeric", #depth
                                        "numeric", "numeric", #vol
                                        "numeric", "numeric", #Td 
                                        "text", "numeric",    #troph
                                        "numeric", "text",    #elev
                                        "text", "text",       #subdr
                                        "text", "text",       #huc
                                        "numeric", "text",    #page
                                        "text", "numeric",    #count
                                        "numeric", "numeric", #utm_y
                                        "numeric", "text",    #Long
                                        "text", "text"))      #invas
```

### Preparing the Morphometry Data

We need to:  
1\. Correct the non-syntactic names of the morphometry data,  
2\. Move the ‘Town’ column towards the front, 3. simplify the data by
shortenning names and dropping reference to the Delorme Atlas,  
4\. Replace numeric code for the presence of dams with text, 5. Create a
metric version of the table, to link to lake analysis software.

#### Fix Non-syntactic Names

``` r
morpho_names <- names(morpho_data)
morpho_names[1] <- 'MIDAS'
morpho_names[2] <- 'Lake'
morpho_names[20] <- 'Town'
morpho_names <- gsub('[\\(\\)]', '', morpho_names)
morpho_names <- gsub('\\/', '_p_', morpho_names)
morpho_names <- gsub(' ', '_', morpho_names)
morpho_names <- gsub('?', '', morpho_names)
morpho_names
```

    ##  [1] "MIDAS"                         "Lake"                         
    ##  [3] "Area_acres"                    "Perimeter_miles"              
    ##  [5] "Depth_Mean_feet"               "Depth_Max_feet"               
    ##  [7] "Volume_acrefeet"               "Direct_Drainage_Area_sq_miles"
    ##  [9] "Total_Drainage_Area_sq_miles"  "Flushing_Rate_times_p_yr"     
    ## [11] "Trophic_Category"              "Dam"                          
    ## [13] "Elevation_feet"                "Major_Drainage"               
    ## [15] "Sub_Drainage"                  "HUC10_Name"                   
    ## [17] "HUC10_Code"                    "USGS_Quad24"                  
    ## [19] "DeLorme_Page"                  "Town"                         
    ## [21] "County"                        "UTM_X"                        
    ## [23] "UTM_Y"                         "Latitude"                     
    ## [25] "Longitude"                     "Water_Quality_Statement"      
    ## [27] "Invasive_Plant_Infestation"    "Fishery_Management"

``` r
names(morpho_data) <- morpho_names
```

#### Move `Town` Data Column

``` r
morpho_data <- morpho_data %>%
relocate(Town, .after = Lake)
```

#### Simplify Data

some of those names are awkward and long, so we shorten them up

``` r
morpho_data <- morpho_data %>%
  rename(Area_ac = Area_acres, 
         Perim_mi =Perimeter_miles,
         D_Mean_ft = Depth_Mean_feet,
         D_Max_ft =  Depth_Max_feet,
         Volume_acft = Volume_acrefeet,
         DDrain_sqmi = Direct_Drainage_Area_sq_miles,
         TDrain_sqmi = Total_Drainage_Area_sq_miles,
         Flushes_p_yr = Flushing_Rate_times_p_yr,
         Trop_Cat = Trophic_Category,
         Elev_ft = Elevation_feet,
         WQ_Statement = Water_Quality_Statement,
         Invasives = Invasive_Plant_Infestation,
         Fishery = Fishery_Management
         ) %>%
    select(-DeLorme_Page) %>%
    mutate(Dam = factor(Dam, levels =c(1,3,2),
                        labels = c('no dam',
                                   'surface increased <50%',
                                   'surface increased >50%')))
```

#### No Lake has Duplicate Morphometry

``` r
morpho_data[duplicated(morpho_data$MIDAS),]
```

    ## # A tibble: 0 x 27
    ## # ... with 27 variables: MIDAS <dbl>, Lake <chr>, Town <chr>, Area_ac <dbl>,
    ## #   Perim_mi <dbl>, D_Mean_ft <dbl>, D_Max_ft <dbl>, Volume_acft <dbl>,
    ## #   DDrain_sqmi <dbl>, TDrain_sqmi <dbl>, Flushes_p_yr <dbl>, Trop_Cat <chr>,
    ## #   Dam <fct>, Elev_ft <dbl>, Major_Drainage <chr>, Sub_Drainage <chr>,
    ## #   HUC10_Name <chr>, HUC10_Code <chr>, USGS_Quad24 <chr>, County <chr>,
    ## #   UTM_X <dbl>, UTM_Y <dbl>, Latitude <dbl>, Longitude <dbl>,
    ## #   WQ_Statement <chr>, Invasives <chr>, Fishery <chr>

#### What Lakes Lack Morphometry Data?

Only a few, and none that matter for our analysis.

``` r
CB_Lakes[! CB_Lakes$MIDAS %in% morpho_data$MIDAS,]
```

    ## # A tibble: 1 x 9
    ##   MIDAS Lake    Town  ExtName     Longitude Latitude Elevation Notes Name_Change
    ##   <int> <chr>   <chr> <chr>           <dbl>    <dbl>     <dbl> <chr> <chr>      
    ## 1  6942 unname~ Gorh~ unnamed po~     -70.4     43.7       197 <NA>  <NA>

``` r
Maine_Lakes$Lake[! Maine_Lakes$MIDAS %in% morpho_data$MIDAS]
```

    ##  [1] NA                       NA                       "Adeles Frog Pond"      
    ##  [4] "Bethel Swimming Pond"   "Big Bog"                "Burlington Fire Pond"  
    ##  [7] "Campbell Pond"          "Cobscook BSP Hdqtrs Pd" "Conservation Camp Pond"
    ## [10] "Del Barker Pond"        "Eastward Pond"          "Fairfield Rec Pond"    
    ## [13] "Farm Pond #1"           "Farm Pond #2"           "Farm Pond #3"          
    ## [16] "Fenwick Pond"           "Flints Pond"            "Hannington Pond"       
    ## [19] "Hight Pond"             "Lincoln Kids Pond"      "Lower Pond"            
    ## [22] "Meadow Brook Pond"      "Mill Field Reservoir"   "Milo Pond"             
    ## [25] "Monmouth Fish Game Pd"  "Phippsburg Sports Pond" "Prouts Pond"           
    ## [28] "Rangeley Fish Game Pd"  "Raymond Pond"           "Sam Leach pond"        
    ## [31] "Stewart Pond"           "Stiles Pond"            "Town House Pond"       
    ## [34] "Town Office Pond"       "unnamed pond"           "unnamed pond"          
    ## [37] "unnamed pond"           "unnamed pond"           "unnamed pond"          
    ## [40] "unnamed pond"           "Upper Pond"

### Merge Morphometry Data with Lake Lists

#### Test for Inconsistencies

``` r
t1 <- Maine_Lakes %>% select(MIDAS, Lake, Town)
t2 <- morpho_data  %>% select(MIDAS, Lake, Town)

# Because of duplicated MIDAS numbers, the number of rows here is greater than
# in the list of Maine Lakes.  That's OK, since this is a quick QA/QC check.
t3 <- t1 %>% inner_join(t2, by = 'MIDAS')
```

``` r
t3[(t3$Lake.x != t3$Lake.y),] %>%
  select(contains('Lake'))
```

    ## # A tibble: 2,963 x 2
    ##    Lake.x               Lake.y                                                  
    ##    <chr>                <chr>                                                   
    ##  1 4th Pelletier Brook~ 4th Pelletier Brk Lake                                  
    ##  2 5th Pelletier Brook~ Fifth Pelletier Brk Lk                                  
    ##  3 6th Pelletier Brook~ Sixth Pelletier Brk Lk                                  
    ##  4 Abol Deadwater Pd-w~ Abol Deadwater                                          
    ##  5 Abol Pond (North)    Abol Pond                                               
    ##  6 Adams Pond           Adams (Rock Haven) Pond                                 
    ##  7 Alerton Lake         Alerton (Alberton) Lake                                 
    ##  8 Ambajejus Lake       Ambajejus (Elbow, North Twin, South Twin, Pemadumcook C~
    ##  9 Anderson Pond        Anderson (Evers) Pond                                   
    ## 10 Aziscohos Lake       Aziscohos (Aziscoos, Sawyer) Lake                       
    ## # ... with 2,953 more rows

So we have several thousand inconsistencies with regard to lake names,
although most appear to be: \* Differences in spelling \* Use of
abbreviations \* Inclusion of multiple different lake names.

Many of the alternate lake names have been moved in the (more recent?)
Maine Lakes data to the “Notes” Field. We have not checked consistency.

``` r
t3[(t3$Town.x != t3$Town.y),] %>%
  select(contains('Town'))
```

    ## # A tibble: 960 x 2
    ##    Town.x                        Town.y                                         
    ##    <chr>                         <chr>                                          
    ##  1 Bowdoin College Grant East T~ Bowdoin College Grant East Twp, Bowdoin Colleg~
    ##  2 Eastbrook                     Eastbrook, Franklin                            
    ##  3 Standish                      Baldwin, Standish                              
    ##  4 Chapman                       Chapman, T11 R4 WELS                           
    ##  5 T3 R5 BKP WKR                 Lower Enchanted Twp, T3 R5 BKP WKR             
    ##  6 T8 R14 WELS                   T7 R14 WELS, T8 R14 WELS                       
    ##  7 T35 MD BPP                    T35 MD                                         
    ##  8 T34 MD BPP                    T28 MD, T34 MD                                 
    ##  9 T1 R9 WELS                    T3 Indian Purchase Twp, T4 Indian Purchase Twp~
    ## 10 T10 SD BPP                    T10 SD                                         
    ## # ... with 950 more rows

``` r
rm(t1,t2,t3)
```

Again, inconsistent nomenclature is common. In general, the Morphometry
data includes multiple names for towns when lakes touch more than one.
The Maine Lakes list does not, generally selecting one town.

The (newer\!) observational data (see below) also lists multiple towns.
While those listings of multiple towns are internally consistent, We
have not formally checked if the listing of multiple towns there is
consistent with the Morphology Data (although in a few sopt checks, it
does appear to be).

Our conclusion is that, for now, we want to keep the Morphometry list of
Towns, but the lake names from Maine Lakes.

So, to assemble a joined database, we only need to import the simpler
Lake Name field, and rename the Town field to “Towns” to be clear that
it can include more than one, and thus avoid future conflicts.

``` r
morpho_data <- morpho_data %>%
  mutate(Lake = Maine_Lakes$Lake[match(MIDAS, Maine_Lakes$MIDAS)]) %>%
  rename(Towns = Town)
```

#### Create Metric Version

``` r
# unit conversion constants
m_p_ft = 0.3048
sqft_p_acre = 43560
#ha_p_m2 = 100*100
f_p_mile = 5280

morpho_metric <- morpho_data %>%
  mutate(Area_sq_m = round(Area_ac*sqft_p_acre*(m_p_ft)^2,2),
         Perim_km = round(Perim_mi * f_p_mile * m_p_ft / 10000,2),
         D_Mean_m  = round(D_Mean_ft * m_p_ft,1),
         D_Max_m   = round(D_Max_ft * m_p_ft,1),
         Volume_m3 = round(Volume_acft * sqft_p_acre * m_p_ft^3,0),
         DDrain_ha = round(DDrain_sqmi * f_p_mile^2 * m_p_ft^2 / 10000, 2),
         TDrain_ha = round(TDrain_sqmi * f_p_mile^2 * m_p_ft^2 / 10000, 2),
         Elev_m = round(Elev_ft * m_p_ft,1)) %>%
  select(-c(Area_ac, Perim_mi, D_Mean_ft, D_Max_ft,
            Volume_acft, DDrain_sqmi, TDrain_sqmi,
            Elev_ft))
```

### Write Morphometry Data

``` r
write_csv(morpho_data, 'Lake_Morphometry_English.csv')
write_csv(morpho_metric, 'Lake_Morphometry_Metric.csv')
```

## List Other Excel files found in `Original_Data`

This omits the MOrphology data, which is an `*.xls` file, since we
loaded it separately. We use a tiny tibble to iterate over the source
data, load it, and give each data frame a short name..

``` r
fls <- list.files(sister)
fls <- fls[substr(fls,nchar(fls)-4, nchar(fls))=='.xlsx'] # Restrict to .xlsx 

shortname <- c('CHLA', 'pH', 'Phosphorus', 'Secchi', 'Annual_Means', 'Overall_Means', 'Temp_DO')
(flsdf <- tibble(fls, shortname))
```

    ## # A tibble: 7 x 2
    ##   fls                                                 shortname    
    ##   <chr>                                               <chr>        
    ## 1 MaineLakes_Chlorophyll_ByDate.xlsx                  CHLA         
    ## 2 MaineLakes_pHColorCond_Alk_ByDate.xlsx              pH           
    ## 3 MaineLakes_Phosphorus.xlsx                          Phosphorus   
    ## 4 MaineLakes_Secchi_ByDate.xlsx                       Secchi       
    ## 5 MaineLakes_Secchi_Color_Chemistry_AnnualMeans.xlsx  Annual_Means 
    ## 6 MaineLakes_Secchi_Color_Chemistry_OverallMeans.xlsx Overall_Means
    ## 7 MaineLakes_Temp_DO.xlsx                             Temp_DO

``` r
rm(shortname)
```

### Reflections

In terms of data organization, data comes in five flavors: 1. Lake
Identification and morphometric data – available with a single entry per
lake. 2. Summary data – with a single entry per Sampling Station 3.
Annual Summary data – with a single entry per sampling station each
year  
4\. Secchi Depth Data – with a single entry per single entry per
sampling event (where for our purposes, a sampling event can be
interpreted as the combination of day, station and lake),  
5\. Other observational data, with one or more values reported per
sampling event, keyed to depth. 6. Depth and oxygen profiles. These are
structured by date, lake, station, and multiple depths.

In particular, the Temp\_DO data includes multiple observations in
vertical profiles on each sampling date at each sampling location. As a
result, the file is much larger than the other four.

However, the other observational files all have similar structure, so it
may make sense to combine them. The logical path forward here is to
combine them into a “long” data format" file, with samples given
identifiers that include the date, station, and lake. We will need to be
careful about correctly attributing comments and data qualifiers to the
correct observational values.

## Read in Raw Data

This code accesses a lot of data, so takes several seconds to run. It
also consumes a lot of memory, so this is probably not the best approach
if significant modeling will ensure, where memory resources may slow
computation.

``` r
for (row in 1:length(flsdf[[1]])) {
  afile = flsdf$fls[[row]]
  aname = flsdf$shortname[[row]]
  
  assign(aname, read_excel(file.path(sister,afile), sheet = 'DATA'))
}
```

# Examine Data Organization

## Coding Method

We have a list of data frame names, in the flsdf object. To explore
this, it is convenient to be able to iterate over related data sets. We
could create a list of data frames and iterate over that, but that risks
consuming memory (if you change any of the data sets, triggereing R’s
“copy on modify” rules). Instead, we use a little indirection, so al
lwe need to keep is a list of data frame names as strings.

We can convert a text string into a reference to an underlying R symbol,
which here points to a data frame, as follows:

``` r
nm <- 'Phosphorus'
head(eval(parse(text = nm)))
```

    ## # A tibble: 6 x 9
    ##   MIDAS Lake  `Town(s)` Station Date                Depth `Total P`
    ##   <chr> <chr> <chr>       <dbl> <dttm>              <dbl>     <dbl>
    ## 1 0001  Solo~ Solon           1 2018-08-16 00:00:00     7         7
    ## 2 0002  Indi~ Lexingto~       1 2006-08-17 00:00:00     3         9
    ## 3 0002  Indi~ Lexingto~       1 2006-08-17 00:00:00     5        32
    ## 4 0002  Indi~ Lexingto~       1 2006-08-22 00:00:00     0         7
    ## 5 0002  Indi~ Lexingto~       1 2006-09-07 00:00:00     0         7
    ## 6 0002  Indi~ Lexingto~       1 2014-08-14 00:00:00     3         8
    ## # ... with 2 more variables: `Sample Type` <chr>, Qualifier <chr>

With that method in hand, we can iterate over a list of data frame names
and look at the data frames themselves.

## Compare Column Names

Files are not consistent in use of names for the common data columns.

``` r
# Calculate length, so we can cbind() names together for comparison
count_names <- function(nm) length(names(eval(parse(text = nm))))
(needed_length = max(sapply(flsdf$shortname, count_names)))
```

    ## [1] 29

``` r
# Create a function to extract names, which we can pass to lapply()
namevec <- function(nm, l = needed_length) {
  nms <- names(eval(parse(text = nm)))
  length(nms) <- l
  return(nms)
}

# Use lapply() to create a list of vectors of names
name_vecs <- lapply(flsdf$shortname, namevec)

# bind them into an array for display
all_names <- do.call(cbind, name_vecs)
colnames(all_names) <- flsdf$shortname
cat('\n')
```

``` r
all_names
```

    ##       CHLA          pH                  Phosphorus    Secchi             
    ##  [1,] "MIDAS"       "MIDAS"             "MIDAS"       "Lake Code (MIDAS)"
    ##  [2,] "Lake"        "Lake"              "Lake"        "LAKE"             
    ##  [3,] "Town"        "Town"              "Town(s)"     "Town(s)"          
    ##  [4,] "Station"     "Station"           "Station"     "STATION"          
    ##  [5,] "Date"        "Date"              "Date"        "DATE"             
    ##  [6,] "Depth"       "Depth"             "Depth"       "TIME"             
    ##  [7,] "Sample Type" "Type"              "Total P"     "SECCHI DEPTH"     
    ##  [8,] "CHLA"        "pH"                "Sample Type" "SECCHI ON BOTTOM?"
    ##  [9,] NA            "pH_Method"         "Qualifier"   "SCOPE"            
    ## [10,] NA            "Color (SPU)"       NA            "WIND LEVEL"       
    ## [11,] NA            "AORT"              NA            "WIND DIRECTION"   
    ## [12,] NA            "Color_Method"      NA            "CLOUD COVER"      
    ## [13,] NA            "Conductivity (uS)" NA            NA                 
    ## [14,] NA            "Cond_Method"       NA            NA                 
    ## [15,] NA            "Alkalinity (mg/L)" NA            NA                 
    ## [16,] NA            "Alk_Method"        NA            NA                 
    ## [17,] NA            NA                  NA            NA                 
    ## [18,] NA            NA                  NA            NA                 
    ## [19,] NA            NA                  NA            NA                 
    ## [20,] NA            NA                  NA            NA                 
    ## [21,] NA            NA                  NA            NA                 
    ## [22,] NA            NA                  NA            NA                 
    ## [23,] NA            NA                  NA            NA                 
    ## [24,] NA            NA                  NA            NA                 
    ## [25,] NA            NA                  NA            NA                 
    ## [26,] NA            NA                  NA            NA                 
    ## [27,] NA            NA                  NA            NA                 
    ## [28,] NA            NA                  NA            NA                 
    ## [29,] NA            NA                  NA            NA                 
    ##       Annual_Means        Overall_Means       Temp_DO        
    ##  [1,] "LAKE CODE (MIDAS)" "LAKE CODE (MIDAS)" "MIDAS"        
    ##  [2,] "LAKE NAME"         "LAKE NAME"         "LAKE"         
    ##  [3,] "TOWN(S)"           "TOWN(S)"           "Town(s)"      
    ##  [4,] "STATION"           "STATION"           "STATION"      
    ##  [5,] "YEAR"              "MIN_SEC"           "Date"         
    ##  [6,] "MIN_SEC"           "MIN_SEC_BOTTOM?"   "DEPTH"        
    ##  [7,] "MIN_SEC_BOTTOM?"   "MEAN_SEC"          "TEMPERATURE"  
    ##  [8,] "MEAN_SEC"          "MEAN_SEC_BOTTOM?"  "OXYGEN"       
    ##  [9,] "MEAN_SEC_BOTTOM?"  "MAX_SEC"           "OXYGEN METHOD"
    ## [10,] "MAX_SEC"           "MAX_SEC_BOTTOM?"   NA             
    ## [11,] "MAX_SEC_BOTTOM?"   "NUM_MONTHS_SECCHI" NA             
    ## [12,] "NUM_MONTHS_SECCHI" "MIN_COL"           NA             
    ## [13,] "MIN_COL"           "MEAN_COL"          NA             
    ## [14,] "MEAN_COL"          "MAX_COL"           NA             
    ## [15,] "MAX_COL"           "MIN_CHLA"          NA             
    ## [16,] "MIN_CHLA"          "MEAN_CHLA"         NA             
    ## [17,] "MEAN_CHLA"         "MAX_CHLA"          NA             
    ## [18,] "MAX_CHLA"          "PH"                NA             
    ## [19,] "PH"                "ALKALINITY"        NA             
    ## [20,] "ALKALINITY"        "CONDUCTIVITY"      NA             
    ## [21,] "CONDUCTIVITY"      "TPHOS_EC"          NA             
    ## [22,] "TPHOS_EC"          "TPHOS_SG"          NA             
    ## [23,] "TPHOS_SG"          "TPHOS_BG"          NA             
    ## [24,] "TPHOS_BG"          "TPHOS_PG"          NA             
    ## [25,] "TPHOS_PG"          "TSI_SEC"           NA             
    ## [26,] "TSI_SEC"           "TSI_CHL"           NA             
    ## [27,] "TSI_CHL"           "TSI_TPEC"          NA             
    ## [28,] "TSI_TPEC"          "TSI_TPPG"          NA             
    ## [29,] "TSI_TPPG"          NA                  NA

## Data Name Cleanup

Corrections to be made: 1. Rename the first four variables to be
consistent. 2. If there is a Date variable, rename it for consistency
too. 3. Remove parenthetical unit values (only used in pH data frame) 4.
Replace non-syntactic names by replacing spaces and question marks. 5.
Capitalize first letters

### Utility Functions for Renaming Variables

Renaming functions here take a list of data names, and apply
transformations to them. By composition of suitable functions, we can
generate consistent data frame names.

The first renaming function, `clean_names()`, handles generic renaming
to capitalize consistently, and address punctuation and units provided
in parentheses.

This is a generic column name cleaning function.It suggests a
micro-package to encapsulate this kind of data name munging. This
function capitalizes every word, but in other **State of Casco Bay**
repositories, we adopted different data column name conventions. It
would be convenient to have  
ready-made functions to help create syntactic names following consistent
renaming conventons, as a way to reduce code complexity in future.

Although we do not go to that effort yet, we would want to allow the
user to specify: the style of capitalization ; a list of word
separators, , and how to handle units in parentheses.

``` r
clean_names <- function(df_names) {
  # Recapitalize every word, including words separated by underlines
  # Capitalizing first leaves units (in parentheses) untouched, so we can
  # identify units in all lowercase.
  df_names <- gsub("(^|[[:space:]]|\\_)([[:alpha:]])([[:alpha:]]*)", 
                                         "\\1\\U\\2\\L\\3", df_names, perl=TRUE)

  
  # Anything in parentheses is assumed to be units.  We convert to lower case
  # and the parentheses get dropped.
  df_names <- gsub('\\(([^)]*)\\)', '\\L\\1', df_names, perl = TRUE)
  
  # Remove all other punctuation, and replace with underlines.
  # In case of multiple underlines, replace with just a single underline.
  # Eliminate underlines that end strings.
  df_names <- gsub('[[:punct:]]', '_', df_names)
  df_names <- gsub('\\_+', '_', df_names)
  df_names <- gsub('_$', '', df_names)

  df_names <- gsub(' ', '_', df_names)
  return(df_names)
}
```

The following function isolates the data-source specific name munging we
need to do. It makes a number of assumptions about the ordering of data
columns. It works with existing data, but should be checked with future
data releases.

``` r
clean_lake_names <- function(df_names) {
  first_four <- c('MIDAS', 'Lake', 'Town', 'Station')
  if (length(df_names) >= 4) {      # for safety, and mostly for testing
    df_names[1:4]  <- first_four
  }
  # "DATE", "Date", and "date", if present, should all become "Date"
  df_names[grepl('^date$', df_names, ignore.case = TRUE)]  <- 'Date'
  return(df_names)
}

clean_lake_names(names(Overall_Means))
```

    ##  [1] "MIDAS"             "Lake"              "Town"             
    ##  [4] "Station"           "MIN_SEC"           "MIN_SEC_BOTTOM?"  
    ##  [7] "MEAN_SEC"          "MEAN_SEC_BOTTOM?"  "MAX_SEC"          
    ## [10] "MAX_SEC_BOTTOM?"   "NUM_MONTHS_SECCHI" "MIN_COL"          
    ## [13] "MEAN_COL"          "MAX_COL"           "MIN_CHLA"         
    ## [16] "MEAN_CHLA"         "MAX_CHLA"          "PH"               
    ## [19] "ALKALINITY"        "CONDUCTIVITY"      "TPHOS_EC"         
    ## [22] "TPHOS_SG"          "TPHOS_BG"          "TPHOS_PG"         
    ## [25] "TSI_SEC"           "TSI_CHL"           "TSI_TPEC"         
    ## [28] "TSI_TPPG"

We need to composite those functions appropriately to get the consistent
nomenclature we want.

``` r
names(Overall_Means)
```

    ##  [1] "LAKE CODE (MIDAS)" "LAKE NAME"         "TOWN(S)"          
    ##  [4] "STATION"           "MIN_SEC"           "MIN_SEC_BOTTOM?"  
    ##  [7] "MEAN_SEC"          "MEAN_SEC_BOTTOM?"  "MAX_SEC"          
    ## [10] "MAX_SEC_BOTTOM?"   "NUM_MONTHS_SECCHI" "MIN_COL"          
    ## [13] "MEAN_COL"          "MAX_COL"           "MIN_CHLA"         
    ## [16] "MEAN_CHLA"         "MAX_CHLA"          "PH"               
    ## [19] "ALKALINITY"        "CONDUCTIVITY"      "TPHOS_EC"         
    ## [22] "TPHOS_SG"          "TPHOS_BG"          "TPHOS_PG"         
    ## [25] "TSI_SEC"           "TSI_CHL"           "TSI_TPEC"         
    ## [28] "TSI_TPPG"

``` r
cat('\n\n')
```

``` r
clean_lake_names(clean_names(names(Overall_Means)))
```

    ##  [1] "MIDAS"             "Lake"              "Town"             
    ##  [4] "Station"           "Min_Sec"           "Min_Sec_Bottom"   
    ##  [7] "Mean_Sec"          "Mean_Sec_Bottom"   "Max_Sec"          
    ## [10] "Max_Sec_Bottom"    "Num_Months_Secchi" "Min_Col"          
    ## [13] "Mean_Col"          "Max_Col"           "Min_Chla"         
    ## [16] "Mean_Chla"         "Max_Chla"          "Ph"               
    ## [19] "Alkalinity"        "Conductivity"      "Tphos_Ec"         
    ## [22] "Tphos_Sg"          "Tphos_Bg"          "Tphos_Pg"         
    ## [25] "Tsi_Sec"           "Tsi_Chl"           "Tsi_Tpec"         
    ## [28] "Tsi_Tppg"

### Function Fixing Names in a Data Frame

Here we want to apply a function to the data frames. It would be neater
to change column names in place, and thus not trigger a copy-on-modify
event on all the data frames, but this works, and provides compact code.

We create a function that accepts a text name for a dataframe, and
returns a modified data frame with its names “fixed”.

``` r
clean_rename <- function(df_name) {
  df <- eval(parse(text = df_name))    # this is where we do evil by grabbing
                                       # a reference from the calling scope 
  newnames <- clean_lake_names(clean_names(names(df)))
 # print(newnames)
  setNames(df, newnames)
}

test = data.frame(DATE = 1:5, `Messy (variable)` = letters[1:5],
                  check.names = FALSE)
names(test)
```

    ## [1] "DATE"             "Messy (variable)"

``` r
test <- clean_rename('test')
names(test)
```

    ## [1] "Date"           "Messy_variable"

``` r
rm(test)
```

### Apply the Function to Target Data Frames

We use `lapply` to generate a (HUGE\!) list of revised dataframes. This
is a memory hog, but it works.

``` r
clean_df_list <- lapply(flsdf$shortname, clean_rename)
names(clean_df_list) <- flsdf$shortname
```

Finally, we extract the names back out of the list. In the long run, we
can either keep the list and drop the separate names, or drop the list
and keep the separate names. For now, we keep both.

``` r
walk(flsdf$shortname, function(nm) assign(nm, clean_df_list[[nm]],
                                          envir = .GlobalEnv))
```

# Checking Lake Context Data

Ideally, we would pull the Lake context data out into a separate data
file from the observations. The context information is unique to each
MIDAS number, so does not change. But I don’t trust that the context
data is identical by MIDAS number without checking.

## Check Uniformity of Lake Context Data

we construct a list of all MIDAS numbers reflected in any of the source
data. The primary purpose is to check if MIDAS\< LAke NAme and Town are
consistent across data sources, but it also provides a sumamry list of
all Maoine Lakes with any monitoring data.

``` r
# Pull out "MIDAS', 'Lake', and 'Town' data only
context_list = lapply(clean_df_list, function(df) select(df, 1:3))

midas_table <- bind_rows(context_list) %>%
  unique() %>%
  arrange(MIDAS)

midas_table
```

    ## # A tibble: 1,078 x 3
    ##    MIDAS Lake                   Town                       
    ##    <chr> <chr>                  <chr>                      
    ##  1 -0001 Skiff Lake, NB         <NA>                       
    ##  2 -0002 McAdam Pond, NB        <NA>                       
    ##  3 -0003 Ivanhoe, NH            <NA>                       
    ##  4 0001  Solon Flowage          Solon                      
    ##  5 0002  Indian Pond            Lexington Twp              
    ##  6 0004  Gilman Pond            New Portland, Lexington Twp
    ##  7 0007  Estes Lake             Alfred, Sanford            
    ##  8 0008  Little Indian Pond     Lexington Twp              
    ##  9 0009  Fish River (Fish) Lake T13 R8 WELS, T14 R8 WELS   
    ## 10 0012  Porter Lake            New Vineyard, Strong       
    ## # ... with 1,068 more rows

Note three lakes have negative MIDAS numbers in at least one data
source, but they are all apparently lakes outside of the legal
boundaries of Maine – two in New Brunswick and one in New Hampshire -
-so we do not alter those MIDAS numbers.

We show that we have no duplicated MIDAS numbers, so MIDAS numbers are
unique across all data sources, and align with only a single lake name
and single town name. We do have over 100 duplicate lake names, which
emphasizes why we need to focus on MIDAS numbers, not names in any
analyses.

``` r
midas_table %>%
  group_by(MIDAS) %>%
  mutate(n = n()) %>%
  filter(n > 1)
```

    ## # A tibble: 0 x 4
    ## # Groups:   MIDAS [0]
    ## # ... with 4 variables: MIDAS <chr>, Lake <chr>, Town <chr>, n <int>

Spot checks suggest the Town field here matches the Towns field in the
Morphometry data.

# Save Data for Selected Lakes

``` r
write_csv(morpho_metric[morpho_metric$MIDAS %in% selected_lakes,],
             'CB_Lakes_Morphometry_Metric.csv')
write_csv(Secchi[Secchi$MIDAS %in% selected_lakes,], 'Secchi.csv')
write_csv(Temp_DO[Temp_DO$MIDAS %in% selected_lakes,], 'Temp_DO.csv')
write_csv(Annual_Means[Annual_Means$MIDAS %in% selected_lakes,], 
          'Annual_Means.csv')
write_csv(Overall_Means[Overall_Means$MIDAS %in% selected_lakes,],
          'Overall_Means.csv')
```

# Cleanup Observational Data

The remaining observational data:  
\* pH and related data  
\* Chlorophyll-A data  
\* Phosphorus data  
are all based on samples. Samples are collected at a specific depth (or
range of depths), at a particular time and place, so we could consider
combining all of them into one large data frame.

The logical aim here is to produce a “long form” data set that we can
work with, with values labeled by units and qualifiers.

We have a potential problem, as each observation is often tied to some
sort of related qualifier:

``` r
names(CHLA)
```

    ## [1] "MIDAS"       "Lake"        "Town"        "Station"     "Date"       
    ## [6] "Depth"       "Sample_Type" "Chla"

``` r
names(pH)
```

    ##  [1] "MIDAS"           "Lake"            "Town"            "Station"        
    ##  [5] "Date"            "Depth"           "Type"            "Ph"             
    ##  [9] "Ph_Method"       "Color_spu"       "Aort"            "Color_Method"   
    ## [13] "Conductivity_us" "Cond_Method"     "Alkalinity_mg_l" "Alk_Method"

``` r
names(Phosphorus)
```

    ## [1] "MIDAS"       "Lake"        "Town"        "Station"     "Date"       
    ## [6] "Depth"       "Total_P"     "Sample_Type" "Qualifier"

We see that: \* Every SAMPLE has associated either a ‘Type’ or
‘Sample\_Type’ variable, with potential values of ‘C’ (for epilimnetic
core sample) or ‘G’ (for grab samples). \* `Ph` is associated with a
`PH_Method`, with possible values (H=Hach, E=electronic, C=colorimetric,
A=air-equilibrated.) It als has a `Type` value, which is either ‘C’ for
Eplilimnetic Core, or ‘G’ for Grab. \* `Color` is associated with
`Aort`, with possible values: A = apparent color (unfiltered); T = true
color (filtered). 9 = no data. It appears that in this setting “no data”
means no data on the method used, but that is not entirely clear from
the Metadata. \* `Color` ALSO has an associated ‘Color\_Method’ value,
which can take on the values C=colorimetric, H=Hach, N=Nessler, T=true
color with Hach. \* `Conductivity_us` has its related `Cond_Method`
(L=electronic lab meter, F=electronic field meter).  
\* `Alkalinity_mg_l` has `Alk_Method` (C=colorimetric, M=methyl orange,
B=Bromcresol green/methyl red, G=Gran plot).  
\* `Total_P` has a `Qualifier` value that also encodes the type of
sample. (BG=bottom grab; PG=profile grab; SG=surface grab;
EC=epilimnetic core; R=repeat sample.)

Our qualifiers are not unique across parameters, ‘C’ and ’A are each
duplicated, used to code different things in different source data sets.

## Color ‘AORT’ and ‘Color\_Method’

``` r
pH %>%
  select(Aort, Color_Method) %>%
  group_by(Color_Method) %>%
  summarise(count_A = sum(Aort == 'A'),
            count_T = sum(Aort == 'T'),
            count_na = sum(is.na(Aort)))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 22 x 4
    ##    Color_Method       count_A count_T count_na
    ##    <chr>                <int>   <int>    <int>
    ##  1 C                     3189      20        0
    ##  2 E                        1       0        0
    ##  3 F                       NA      NA        1
    ##  4 Field Device            48      29        0
    ##  5 H                       NA      NA      590
    ##  6 H2                     205     212        0
    ##  7 Hach Color Wheel       101      18        0
    ##  8 HACH COLOR WHEEL       640     473        0
    ##  9 Hach Color Wheel 2     448     454        0
    ## 10 HACH WHEEL               4       0        0
    ## # ... with 12 more rows

Several of the rare color methods are not included in the metadata,
including: method - ‘E’, ‘F’, ‘L’. Although ‘T’ is listed in the
metadata, it does not appear in the data.

Other codes appear to be referring to similar methods, including various
spellings of ‘SPECTROPHOTOMETRIC’ (including a typo).

L-10-308-00-1-A refers to a specific HACH spectrophotometric method for
measuring color in water. WE reclassify it as another spectrophotometric
method, and give it value ‘S’.

We note that the N (\>150) appears to be a left censored observation,
and probably should not be coded in this way.

We reclassify, but given uncertainties here, thse data are not likely to
be analyzable.

``` r
pH <- pH %>%
  # Drop rare values that we can't interpret
  mutate(Color_Method = if_else(grepl('^[ELF]\\b', Color_Method), NA_character_,
                                 Color_Method)) %>%
  
  # Pull in all spectrophotometric methods
  mutate(Color_Method = if_else(grepl('spec', Color_Method,
                                      ignore.case = TRUE), 'S',
                                Color_Method)) %>%
  mutate(Color_Method = if_else(grepl('L-10',Color_Method), 'S',
                                Color_Method)) %>%
  mutate(Color_Method = if_else(Color_Method == 's', 'S', Color_Method)) %>%
  
  #Sort Hach entries for color wheels one or two (?)
  mutate(Color_Method = if_else(grepl('Hach',Color_Method,
                                       ignore.case = TRUE),
                                if_else(grepl('2',Color_Method), 'H2',
                                        'H'),
                                Color_Method)) %>%
  
  # Convert "Field Device" to "F" -- although I don't know what
  # a "Field Device" is in this context.  
  mutate(Color_Method = if_else(Color_Method == 'Field Device', 'F',
                                Color_Method)) %>%
  
  #Finally, drop the left censored indicator.
  mutate(Color_Method = if_else(Color_Method == 'N (>150)', 'N',
                                Color_Method))
```

## Redundancy in P data and P data “Repeat” Samples

The Phosphorus “Qualifier” data appears to be redundant with the
‘Sample\_Type’ data. Let’s check.

``` r
unique(Phosphorus$Qualifier)
```

    ## [1] "EC" "BG" "SG" "R"  "PG" "NA" NA   "EG"

``` r
Phosphorus %>%
  select(Sample_Type, Qualifier) %>%
  group_by(Qualifier) %>%
  summarise(count_core = sum(Sample_Type == 'C'),
            count_grab = sum(Sample_Type == 'G'))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 8 x 3
    ##   Qualifier count_core count_grab
    ##   <chr>          <int>      <int>
    ## 1 BG                 0       6380
    ## 2 EC             20185          1
    ## 3 EG                 0          1
    ## 4 NA                 0          0
    ## 5 PG                 0      35365
    ## 6 R               1106        641
    ## 7 SG                 1       3719
    ## 8 <NA>               0          0

So, with a couple of exception – which are probably errors – the values
are consistent EXCEPT for the ‘R’ category, which refers to repeat
samples.

We will modify the “R” category to ‘RC’ for repeat Core samples, and
‘RG’ for repeat grab samples.

``` r
Phosphorus <- Phosphorus %>%
  mutate(Qualifier = if_else(Qualifier == 'R',
                              if_else(Sample_Type == 'C', 'RC',
                                      if_else(Sample_Type == 'G', 'RG',
                                              NA_character_)), Qualifier)) %>%
  mutate(Qualifier = if_else(Qualifier == 'NA', NA_character_, Qualifier))
```

``` r
unique(Phosphorus$Qualifier)
```

    ## [1] "EC" "BG" "SG" "RC" "PG" "RG" NA   "EG"

## Pivot each data set to long form

### CHLA

Does not need to be pivoted. it is already in appropriate form. All we
need to do is add parameter and units columns, and rename everything.

``` r
CHLA <- CHLA %>%
  mutate(Parameter = 'CHLA',
         Units = 'ug/L') %>%
  rename(Value = Chla)
```

### Phosphorus

Similarly, the Phosphorus data need not be reorganized, as it is already
in “long” form.

``` r
Phosphorus <- Phosphorus %>%
  mutate(Parameter = 'Total Phosphorus',
         Units =  'ug/L') %>%
  rename(Type = Sample_Type,
         Value = Total_P)
```

### pH and related data

The pH data needs to be reorganized

``` r
sample_data <- pH %>%
  rename(pH = Ph,
         Color = Color_spu,
         Conductivity = Conductivity_us,
         Alkalinity = Alkalinity_mg_l) %>%
  pivot_longer(c('pH', 'Color', 'Conductivity', 'Alkalinity'),
               names_to = 'Parameter', values_to = 'Value') %>%

  mutate(Method = if_else(Parameter == 'Ph', Ph_Method,
                    if_else(Parameter == 'Color', Aort, 
                      if_else(Parameter == 'Conductivity', Cond_Method,
                        if_else(Parameter == 'Alkalinity', Alk_Method,
                                NA_character_) ) ) ) ) %>%
  
  mutate(Units = if_else(Parameter == 'Ph', NA_character_,
                    if_else(Parameter == 'Color', 'SPU', 
                      if_else(Parameter == 'Conductivity', 'uS',
                        if_else(Parameter == 'Alkalinity', 'mg/l',
                                NA_character_) ) ) ) ) %>%
  select(-Ph_Method, -Aort, -Cond_Method, -Alk_Method) %>%
  filter(! is.na(Value)) %>%
  arrange(Date, MIDAS, Station, Type, Depth)
sample_data
```

    ## # A tibble: 115,197 x 12
    ##    MIDAS Lake  Town  Station Date                Depth Type  Color_Method
    ##    <chr> <chr> <chr>   <dbl> <dttm>              <dbl> <chr> <chr>       
    ##  1 3454  High~ Brid~       2 1938-06-29 00:00:00   0   G     <NA>        
    ##  2 3454  High~ Brid~       2 1938-06-29 00:00:00   4.6 G     <NA>        
    ##  3 3454  High~ Brid~       2 1938-06-29 00:00:00   6.1 G     <NA>        
    ##  4 3454  High~ Brid~       2 1938-06-29 00:00:00   7.6 G     <NA>        
    ##  5 3454  High~ Brid~       3 1938-06-29 00:00:00   0   G     <NA>        
    ##  6 3454  High~ Brid~       3 1938-06-29 00:00:00   4.6 G     <NA>        
    ##  7 3454  High~ Brid~       3 1938-06-29 00:00:00   6.1 G     <NA>        
    ##  8 3454  High~ Brid~       3 1938-06-29 00:00:00   9.1 G     <NA>        
    ##  9 3454  High~ Brid~       3 1938-06-29 00:00:00  14   G     <NA>        
    ## 10 3454  High~ Brid~       1 1938-08-20 00:00:00   0   G     <NA>        
    ## # ... with 115,187 more rows, and 4 more variables: Parameter <chr>,
    ## #   Value <dbl>, Method <chr>, Units <chr>

# Write out Sample Data

``` r
write_csv(sample_data[sample_data$MIDAS %in% selected_lakes,], 'Sample_Data.csv')
```
