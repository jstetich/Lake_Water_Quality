Compare Versions of Sebago Lake Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/29/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
  - [Read Parsed Lakes Data and limit to Sebago
    Lake](#read-parsed-lakes-data-and-limit-to-sebago-lake)
      - [Folder References](#folder-references)
  - [Read PWD Sebago Lakes Secchi Depth
    Data](#read-pwd-sebago-lakes-secchi-depth-data)
  - [Review of Sebago Lake Data](#review-of-sebago-lake-data)
      - [Sampling Stations Crosswalk](#sampling-stations-crosswalk)
          - [Stations by Years from PWD
            Data](#stations-by-years-from-pwd-data)
      - [Check for Duplicate Data](#check-for-duplicate-data)
          - [Rough Probablities of
            Duplicates](#rough-probablities-of-duplicates)
  - [Save Sebago Secchi Data in Compatible
    Format](#save-sebago-secchi-data-in-compatible-format)
      - [StationName - Station
        Correspondence](#stationname---station-correspondence)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Introduction

Sebago lake data from our primary data sources was incomplete, with
little data available from the last ten years or so. We requested
additional data on Sebago Lake water quality from Portland Water
District. This Notebook examines the two data sources (DEP and PWD) to
figure out how to integrate the PWD Sebago data into the larger lakes
data set.

PWD Staff informed us that their data is likely independent from the
volunteer-based data included in the DEP data. This Notebook checks that
claim, as part of good QA/QC practice.

This script compares data derived from our core data and data derived
from the PWD data. This poses a challenge in terms of how to organize
data and code in our GitHub repository.

For this script to run, it needs to run based on derived data files with
the original (incomplete) Sebago Lake data, while we ultimately want to
analyze complete data. To deal with that, we do not alter our principal
Derived\_Data files here, but add corrected Sebago Lakes files, which
will have to be separately loaded in all of our data analysis and
graphics notebooks.

# Load Libraries

``` r
library(readxl)
#library(readr)
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Load Data

# Read Parsed Lakes Data and limit to Sebago Lake

``` r
Secchi <- read_csv('Secchi.csv') %>%
  filter(MIDAS == 5786) %>%
  mutate(Year = as.numeric(format(Date, format = '%Y')))
```

    ## Parsed with column specification:
    ## cols(
    ##   MIDAS = col_double(),
    ##   Lake = col_character(),
    ##   Town = col_character(),
    ##   Station = col_double(),
    ##   Date = col_datetime(format = ""),
    ##   Time = col_character(),
    ##   Secchi_Depth = col_double(),
    ##   Secchi_On_Bottom = col_character(),
    ##   Scope = col_double(),
    ##   Wind_Level = col_double(),
    ##   Wind_Direction = col_double(),
    ##   Cloud_Cover = col_character()
    ## )

    ## Warning: 9 parsing failures.
    ##  row   col expected actual         file
    ## 1598 Scope a double      N 'Secchi.csv'
    ## 1599 Scope a double      N 'Secchi.csv'
    ## 1600 Scope a double      N 'Secchi.csv'
    ## 1601 Scope a double      N 'Secchi.csv'
    ## 1602 Scope a double      N 'Secchi.csv'
    ## .... ..... ........ ...... ............
    ## See problems(...) for more details.

``` r
Temp_DO  <- read_csv('Temp_DO.csv') %>%
  filter(MIDAS == 5786) %>%
  mutate(Year = as.numeric(format(Date, format = '%Y')))
```

    ## Parsed with column specification:
    ## cols(
    ##   MIDAS = col_double(),
    ##   Lake = col_character(),
    ##   Town = col_character(),
    ##   Station = col_double(),
    ##   Date = col_datetime(format = ""),
    ##   Depth = col_double(),
    ##   Temperature = col_double(),
    ##   Oxygen = col_double(),
    ##   Oxygen_Method = col_character()
    ## )

``` r
Annual_Means <- read_csv('Annual_Means.csv') %>%
  filter(MIDAS == 5786)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Lake = col_character(),
    ##   Town = col_character(),
    ##   Min_Sec_Bottom = col_character(),
    ##   Mean_Sec_Bottom = col_character(),
    ##   Max_Sec_Bottom = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
Overall_Means <- read_csv('Overall_Means.csv') %>%
  filter(MIDAS == 5786)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Lake = col_character(),
    ##   Town = col_character(),
    ##   Min_Sec_Bottom = col_character(),
    ##   Mean_Sec_Bottom = col_character(),
    ##   Max_Sec_Bottom = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
sample_data <- read_csv('Sample_Data.csv') %>%
  filter(MIDAS == 5786) %>%
  mutate(Year = as.numeric(format(Date, format = '%Y')))
```

    ## Parsed with column specification:
    ## cols(
    ##   MIDAS = col_double(),
    ##   Lake = col_character(),
    ##   Town = col_character(),
    ##   Station = col_double(),
    ##   Date = col_datetime(format = ""),
    ##   Depth = col_double(),
    ##   Type = col_character(),
    ##   Color_Method = col_character(),
    ##   Parameter = col_character(),
    ##   Value = col_double(),
    ##   Method = col_character(),
    ##   Units = col_character()
    ## )

## Folder References

``` r
sisterfldnm <- 'Original_Data'
parent <- dirname(getwd())
sister <- file.path(parent,sisterfldnm)
```

# Read PWD Sebago Lakes Secchi Depth Data

``` r
fn <- 'Sebago export 2_24_2020.xlsx'
Secchi_Sebago <- read_excel(file.path(sister, fn), 
    sheet = "Event", col_types = c("skip", "text",    "date", "skip", "skip",
                                   "skip", "numeric", "skip", "skip", "skip", 
                                   "skip", "skip",    "skip", "skip", "skip", 
                                   "skip", "skip",    "skip", "skip", "skip")) %>%
  mutate(Lake   = "Sebago Lake",
         MIDAS =  5786,
         Town =  "Gray, Windham",
         Year = as.numeric(format(Date, format = '%Y'))) %>%
  rename(Secchi_Depth = SecchiDepth) %>%
  relocate(MIDAS, Lake, Town, StationName, Date, Year) %>%
  filter (! is.na(Secchi_Depth))
```

# Review of Sebago Lake Data

## Sampling Stations Crosswalk

We check to see if DEP and PWD samples coincide by station and year.
\#\#\# Stations by Years from Main Data

``` r
knitr::kable(xtabs(~ Year + Station, data = Secchi))
```

|      |  1 | 2 | 3 | 4 | 5 |  6 |  7 | 8 | 9 | 10 | 11 | 12 | 13 | 50 |
| :--- | -: | -: | -: | -: | -: | -: | -: | -: | -: | -: | -: | -: | -: | -: |
| 1970 |  2 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1971 |  2 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1972 |  2 | 3 | 3 | 3 | 3 |  3 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1973 |  1 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1976 |  4 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1977 |  0 | 0 | 0 | 0 | 0 |  0 |  4 | 5 | 6 |  4 |  0 |  0 |  0 |  0 |
| 1979 |  1 | 0 | 0 | 0 | 0 |  0 | 12 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1980 |  0 | 0 | 0 | 0 | 0 |  0 |  9 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1981 |  0 | 0 | 0 | 0 | 0 | 11 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1982 | 13 | 0 | 0 | 0 | 0 |  9 |  0 | 0 | 0 |  0 |  4 |  0 |  0 |  0 |
| 1983 | 15 | 0 | 0 | 0 | 0 |  1 | 11 | 1 | 2 |  0 | 10 |  0 |  0 |  0 |
| 1984 |  3 | 0 | 0 | 0 | 0 |  0 | 13 | 0 | 0 |  0 |  2 |  0 |  0 |  0 |
| 1985 |  8 | 0 | 0 | 0 | 0 |  0 |  8 | 0 | 0 |  0 |  7 |  0 |  0 |  0 |
| 1986 |  9 | 0 | 0 | 0 | 0 |  0 |  6 | 0 | 0 |  0 |  8 |  0 |  0 |  0 |
| 1987 |  8 | 0 | 0 | 0 | 0 |  0 |  5 | 0 | 0 |  0 |  8 |  0 |  0 |  0 |
| 1988 |  8 | 0 | 0 | 0 | 0 |  0 |  9 | 0 | 0 |  0 |  9 |  0 |  0 |  0 |
| 1989 |  0 | 0 | 0 | 0 | 0 |  0 |  3 | 0 | 0 |  0 |  5 | 15 |  0 |  0 |
| 1990 |  4 | 1 | 1 | 1 | 1 | 25 |  1 | 1 | 1 | 11 | 15 | 17 |  0 |  0 |
| 1991 |  5 | 0 | 0 | 0 | 0 | 12 |  0 | 0 | 0 | 11 | 14 | 20 |  0 |  0 |
| 1992 | 19 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  1 |  3 |  0 |  0 |  0 |
| 1993 | 15 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1994 |  9 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1995 | 14 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1996 | 17 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1997 | 16 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 1998 | 20 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  1 |  0 |  0 |  0 |
| 1999 | 18 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2000 | 22 | 0 | 0 | 0 | 0 |  8 |  0 | 0 | 0 |  8 |  9 |  0 |  0 |  0 |
| 2001 | 16 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2002 | 20 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2003 | 15 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2004 | 20 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2005 | 24 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2006 | 16 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2007 | 18 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2008 | 18 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2009 |  9 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  5 |  0 |  0 |
| 2010 | 11 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  0 |
| 2013 |  0 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  1 |  0 |
| 2017 |  0 | 0 | 0 | 0 | 0 |  1 |  0 | 0 | 0 |  1 |  1 |  0 |  0 |  1 |
| 2018 |  0 | 0 | 0 | 0 | 0 |  0 |  0 | 0 | 0 |  0 |  0 |  0 |  0 |  1 |

### Stations by Years from PWD Data

``` r
knitr::kable(xtabs(~ Year + StationName, data = Secchi_Sebago))
```

|      | Big Bay | Crooked-Songo | Harmons beach | Intakes | Jordan Bay | Lower Bay | Wards Cove |
| :--- | ------: | ------------: | ------------: | ------: | ---------: | --------: | ---------: |
| 1976 |       0 |             0 |             0 |       0 |          0 |        21 |          0 |
| 1977 |       0 |             0 |             0 |       0 |          0 |         7 |          0 |
| 1978 |       0 |             0 |             0 |       0 |          0 |        11 |          0 |
| 1979 |       0 |             0 |             0 |       0 |          0 |        18 |          0 |
| 1980 |       0 |             0 |             0 |       0 |          0 |         8 |          0 |
| 1981 |       0 |             0 |             0 |       0 |          0 |        12 |          0 |
| 1982 |       0 |             0 |             0 |       0 |          0 |        11 |          0 |
| 1983 |       0 |             0 |             0 |       0 |          0 |         7 |          0 |
| 1984 |       0 |             0 |             0 |       0 |          0 |        11 |          0 |
| 1985 |       0 |             0 |             0 |       0 |          0 |         8 |          0 |
| 1986 |       0 |             0 |             0 |       0 |          0 |         6 |          0 |
| 1987 |       0 |             0 |             0 |       0 |          0 |         5 |          0 |
| 1988 |       0 |             0 |             0 |       0 |          0 |         6 |          0 |
| 1989 |       0 |             0 |             0 |       0 |          0 |         9 |          0 |
| 1990 |       9 |             0 |             0 |       0 |         10 |        15 |          0 |
| 1991 |       8 |             0 |             0 |       0 |          7 |        14 |          0 |
| 1992 |       8 |             0 |             0 |       0 |          7 |        13 |          0 |
| 1993 |       9 |             0 |             0 |       0 |          8 |        13 |          0 |
| 1994 |       8 |             0 |             0 |       0 |          8 |        16 |          0 |
| 1995 |       4 |             0 |             0 |       0 |          4 |        13 |          0 |
| 1996 |      11 |             0 |             0 |       0 |         10 |        13 |          0 |
| 1997 |       0 |             0 |             0 |       0 |          0 |         8 |          0 |
| 1998 |       7 |             0 |             0 |       0 |          7 |         7 |          0 |
| 1999 |       8 |             0 |             0 |       0 |          8 |         6 |          0 |
| 2000 |       8 |             8 |             8 |       8 |          8 |         8 |          8 |
| 2001 |       7 |             4 |             5 |       7 |          7 |         7 |          4 |
| 2002 |       7 |             6 |             6 |       5 |          7 |         7 |          6 |
| 2003 |       6 |             6 |             6 |       6 |          6 |         6 |          6 |
| 2004 |       6 |             6 |             6 |       6 |          6 |         6 |          6 |
| 2005 |       6 |             6 |             6 |       6 |          6 |         6 |          6 |
| 2006 |       6 |             6 |             0 |       5 |          6 |         6 |          0 |
| 2007 |       6 |             6 |             0 |       6 |          6 |         6 |          0 |
| 2008 |       6 |             6 |             0 |       6 |          6 |         6 |          0 |
| 2009 |       6 |             6 |             0 |       5 |          6 |         6 |          0 |
| 2010 |       6 |             6 |             0 |       6 |          6 |         6 |          0 |
| 2011 |       7 |             6 |             0 |       6 |          7 |         6 |          0 |
| 2012 |       7 |             6 |             0 |       6 |          7 |         9 |          0 |
| 2013 |       6 |             6 |             0 |       6 |          6 |         6 |          0 |
| 2014 |       6 |             0 |             0 |       0 |          6 |         6 |          0 |
| 2015 |       6 |             0 |             0 |       0 |          6 |         7 |          0 |
| 2016 |       6 |             0 |             0 |       0 |          6 |         7 |          0 |
| 2017 |       6 |             0 |             0 |       0 |          6 |         6 |          0 |
| 2018 |       5 |             0 |             0 |       0 |          5 |         7 |          0 |
| 2019 |       6 |             0 |             0 |       0 |          6 |         6 |          0 |

No historical sampling location (by Station Number) lines up with any of
the PWD sampling Stations, in terms of number of samples per year. This
suggests the two data sources are independent.

## Check for Duplicate Data

We create a combined data source, giving arbitrary Station Numbers to
PWD sampling locations, just so we can combine data sets informally.

``` r
test <- Secchi_Sebago %>%
  mutate(Station = as.numeric(factor(StationName)) + 100) %>%
  bind_rows(Secchi) %>%
  mutate(Source = c("PWD", "DEP")[(Station > 100)+1] )

ggplot(test, aes(Date, Secchi_Depth)) +
  geom_point(aes(color = Source), alpha = 0.25)
```

    ## Warning: Removed 54 rows containing missing values (geom_point).

![](Compare-Sebago-Data_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
It’s quite clear these are non-overlapping or minimally overlapping data
series.

We can search explicitly for duplicated observations, although it takes
careful manual review to identify any issues. Also, to return both
members of a duplicated pair, we need to search both forward and
backward with `duplicated()`.

``` r
test %>%
  filter (! is.na(Secchi_Depth)) %>%
  mutate(from_PWD =  ! is.na(StationName)) %>%
  select(Date, Station, Secchi_Depth, from_PWD) %>%
  mutate(dup_data = duplicated(cbind(Secchi_Depth, Date)) |
                    duplicated(cbind(Secchi_Depth, Date),
                               fromLast = TRUE),
         dup_sample = duplicated(cbind(Station, Date)) |
                      duplicated(cbind(Station, Date),
                                 fromLast = TRUE)) %>%
  filter(dup_data) %>%
  select(-dup_data) %>%
  arrange(Date, Secchi_Depth, Station )
```

    ## # A tibble: 151 x 5
    ##    Date                Station Secchi_Depth from_PWD dup_sample
    ##    <dttm>                <dbl>        <dbl> <lgl>    <lgl>     
    ##  1 1977-08-16 00:00:00       7          7.4 FALSE    FALSE     
    ##  2 1977-08-16 00:00:00       9          7.4 FALSE    FALSE     
    ##  3 1977-09-12 00:00:00       8         10.1 FALSE    FALSE     
    ##  4 1977-09-12 00:00:00       9         10.1 FALSE    FALSE     
    ##  5 1982-06-25 00:00:00       1          6   FALSE    FALSE     
    ##  6 1982-06-25 00:00:00      11          6   FALSE    FALSE     
    ##  7 1983-05-21 00:00:00       1          8   FALSE    TRUE      
    ##  8 1983-05-21 00:00:00      11          8   FALSE    FALSE     
    ##  9 1983-06-22 00:00:00       1          7.2 FALSE    TRUE      
    ## 10 1983-06-22 00:00:00      11          7.2 FALSE    FALSE     
    ## # ... with 141 more rows

1.  In 1982, 1983, 1984, and 1985, stations 1 and 11 frequently show
    duplicate values, and they are, for some reason, being flagged as
    duplicate Stations and Dates, even though duplicates are not showing
    here. That suggests there is a start of replication, and either an
    unusual number of replicates, or some data may have been relabeled.
    But stations 1 and 11 are not close to each other. The error, if
    error it is, is rate. WE have only weak evidence that we should
    remove these data, so we leave them.

2.  There is evidence that field duplicate sampling practices starting
    in 1980s or so, in both data sources. That’s O.K.

3.  Some Secchi values are reported to three (or more) decimal places.
    Three decimal places for Secchi depth implies unlikely precision, to
    millimeter accuracy. Checking with PWD staff, we learned that these
    values are the result of data correction they applied, correcting
    observations for different equipment used in the field. Looking
    closely at the PWD source data in Excel, those three decimal place
    (or better) values occur consistently up through 1996, and not
    later. They formally report only to the nearest 10 cm (1/10th of a
    meter).

4.  Pairs of results from different stations are sometimes equal to
    three decimal places in that older PWD data. Duplication to three
    decimals is rare enough that these almost certainly represent chance
    occurrences.

<!-- end list -->

  - Stations 105 and 106 in October of 1990
  - Stations 101 and 105 in July and October of 1994
  - Stations 101 and 105 in July 1996 and 1998

<!-- end list -->

5.  More frequently, matching values are reported identical to one
    decimal place. That is expected from time to time simply due to the
    sampling process. Values should be similar, especially at nearby
    locations in the lake, and identical observations to one decimal
    place should occur by chance. We argue here that these duplicate
    values are legitimate data, and should be left in place until and
    unless we get additional information on which to base a decision to
    omit observations.

### Rough Probablities of Duplicates

Roughly speaking, one could imagine that an observation is drawn
randomly over a two meter range, giving twenty possible observations. On
any given date, with five observations, each equally likely, the
probability of at least one match is:

\[1 - (20/20)(19/20)(18/20)(17/20)(16/20)\]

\[1 - ( \frac{20!}{15!} \times 20^-5) \approx 0.42% \] In reality, we
don’t expect the observational error to be quite that large, or for the
ranges at different Stations to exactly line up, so that probably
overstates the probability of matches. What it shows is that we should
expect a fair number of matches.

Out of 1800 observations, (again, very roughly; since we don’t always
have five observations on any given day) we would expect
\(0.42 \times 1800/5 \approx 150\) matches. In fact, we got about half
that many. Suggesting the frequency of matches we observe is well within
expectations.

We have some data reported as duplicated values and dates or replicates,
within each data source. A total of 151 duplicates – INCLUDING
replicates – out of almost 1800 samples is not unreasonable. We may have
a few duplicate samples buried in these data, but there is no systematic
duplication of records.

For Secchi Depth, we will fold the PWD data in with the volunteer-based
data to create a composite data record. That means we need the

# Save Sebago Secchi Data in Compatible Format

The Secchi Data From Sebago Lake lacks some of the metadata available
for the volunteer data, but it is otherwise compatible. We save it here,
to read it into our data analysis and graphics Notebooks. To simplify
later analyses, we add arbitrary Station Code numbers, since DEP data
identifies sampling stations with a numeric code.

``` r
Secchi_Sebago <- Secchi_Sebago %>%
  filter (! is.na(Secchi_Depth)) %>%
  filter(Year < 2019) %>%
  mutate(Station = as.numeric(factor(StationName)) + 100)
```

## StationName - Station Correspondence

``` r
Secchi_Sebago %>%
  select(StationName, Station) %>%
  unique() %>%
  knitr::kable()
```

| StationName   | Station |
| :------------ | ------: |
| Big Bay       |     101 |
| Crooked-Songo |     102 |
| Harmons beach |     103 |
| Intakes       |     104 |
| Jordan Bay    |     105 |
| Lower Bay     |     106 |
| Wards Cove    |     107 |

``` r
Secchi_Sebago %>%
  write_csv('Secchi_Sebago.csv')
```
