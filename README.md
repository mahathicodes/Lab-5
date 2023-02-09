Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(leaflet)
```

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- data.table::fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Loading MET data
met <- data.table::fread("met_all.gz")
```

3.  Merge the data as we did during the lecture.

``` r
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y="USAF"
)

met_lz <- lazy_dt(met, immutable = FALSE)
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
# met <- met[!is.na(temp)]
# quantile(met$temp, 0.5)
# met <- met[!is.na(wind.sp)]
# quantile(met$wind.sp, 0.5)
# met <- met[!is.na(stm.press)]
# quantile(met$stm.press, 0.5)

met_avg_lz <- met_lz %>% 
  group_by(USAFID) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE)
  )

met_med_lz <- met_avg_lz %>% 
  summarise(
    across(
      2:4,
      function(x) quantile(x, probs = 0.5, na.rm = TRUE)
    )
  )

temp_us_id <- met_avg_lz %>%
  mutate(
    temp_diff = abs(temp - met_med_lz %>% pull(temp))
  ) %>% 
  arrange(temp_diff) %>% 
  slice(1) %>% pull(USAFID)

wind_us_id <- met_avg_lz %>%
  mutate(
    diff = abs(wind.sp - met_med_lz %>% pull(wind.sp))
  ) %>% 
  arrange(diff) %>% 
  slice(1) %>% pull(USAFID)

atm_us_id <- met_avg_lz %>%
  mutate(
    diff = abs(atm.press - met_med_lz %>% pull(atm.press))
  ) %>% 
  arrange(diff) %>% 
  slice(1) %>% pull(USAFID)
```

``` r
met_lz %>% 
  select(USAFID, lon, lat) %>% 
  distinct() %>% 
  filter(USAFID %in% c(temp_us_id, wind_us_id, atm_us_id))
```

    ## Source: local data table [4 x 3]
    ## Call:   unique(`_DT1`[, .(USAFID, lon, lat)])[USAFID %in% c(temp_us_id, 
    ##     wind_us_id, atm_us_id)]
    ## 
    ##   USAFID   lon   lat
    ##    <int> <dbl> <dbl>
    ## 1 720458 -82.6  37.8
    ## 2 720929 -92.0  45.5
    ## 3 722238 -85.7  31.4
    ## 4 722238 -85.7  31.3
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
met_stations <- unique(met[, .(USAFID, STATE, temp, wind.sp, atm.press, lon, lat)])
met_stations <- na.omit(met_stations)
met_stations[, temp_mid := quantile(temp, probs = .5, na.rm = TRUE), by = STATE]
met_stations[, wind_mid := quantile(wind.sp, probs = .5, na.rm = TRUE), by = STATE]
met_stations[, atm_mid := quantile(atm.press, probs = .5, na.rm = TRUE), by = STATE]
met_stations[,  distance := sqrt((temp_mid - met_stations$temp)^2 + (wind_mid - met_stations$wind.sp)^2 + (atm_mid - met_stations$atm.press)^2)]
met_stations[, minrecord := which.min(lat), by = STATE]
met_stations[, n := 1:.N, by = STATE]
met_state <- met_stations[n == minrecord, .(USAFID, STATE, temp, wind.sp, atm.press, lon, lat)]
met_state
```

    ##     USAFID STATE temp wind.sp atm.press      lon    lat
    ##  1: 720365    OR 20.6     3.6    1017.1 -124.290 42.070
    ##  2: 720379    KY 22.8     4.1    1019.5  -84.856 36.855
    ##  3: 722010    FL 29.4     3.1    1015.3  -81.750 24.550
    ##  4: 722085    SC 32.2     1.5    1019.1  -80.723 32.477
    ##  5: 722096    NV 25.0     3.1    1014.8 -115.132 35.976
    ##  6: 722151    RI 25.0     2.1    1016.6  -71.799 41.350
    ##  7: 722166    GA 27.8     0.0    1017.7  -83.277 30.783
    ##  8: 722235    AL 28.9     1.5    1016.7  -88.068 30.626
    ##  9: 722329    LA 26.1     0.0    1018.2  -91.339 29.710
    ## 10: 722358    MS 22.8     0.0    1016.4  -90.472 31.178
    ## 11: 722508    TX 28.9     6.7    1014.1  -97.346 26.166
    ## 12: 722725    NM 30.0     7.7    1011.6 -107.720 32.262
    ## 13: 722728    AZ 33.3     1.5    1012.3 -110.848 31.418
    ## 14: 722909    CA 21.1     2.6    1014.5 -117.116 32.567
    ## 15: 723020    NC 27.8     2.1    1017.0  -77.900 34.267
    ## 16: 723240    TN 28.9     0.0    1016.6  -85.200 35.033
    ## 17: 723300    MO 26.1     2.1    1016.4  -90.325 36.773
    ## 18: 723419    AR 28.9     1.5    1016.1  -92.814 33.221
    ## 19: 723528    OK 36.1     5.7    1011.7  -98.983 34.344
    ## 20: 724075    NJ 22.8     2.6    1016.5  -75.078 39.366
    ## 21: 724080    PA 23.9     2.6    1016.9  -75.227 39.873
    ## 22: 724093    DE 22.8     0.0    1017.0  -75.359 38.689
    ## 23: 724106    VA 22.8     2.1    1018.0  -79.335 36.573
    ## 24: 724125    WV 18.9     2.1    1018.9  -81.204 37.298
    ## 25: 724297    OH 26.1     3.1    1017.4  -84.419 39.103
    ## 26: 724320    IN 28.9     2.6    1018.1  -87.532 38.037
    ## 27: 724336    IL 28.3     5.1    1008.9  -89.252 37.778
    ## 28: 724516    KS 19.4     2.1    1013.2 -100.960 37.044
    ## 29: 724625    CO 25.0     0.0    1017.6 -107.760 37.140
    ## 30: 724754    UT 23.9     3.6    1009.7 -113.593 37.091
    ## 31: 725030    NY 30.0     2.1    1018.4  -73.873 40.777
    ## 32: 725040    CT 22.8     2.1    1015.9  -73.129 41.158
    ## 33: 725060    MA 21.7     4.1    1016.9  -70.667 41.250
    ## 34: 725404    MI 22.8     3.6    1019.5  -84.079 41.868
    ## 35: 725499    IA 21.7     3.1    1020.9  -93.900 40.630
    ## 36: 725514    MD 23.3     1.5    1016.7  -76.429 38.142
    ## 37: 725533    NE 27.8     2.6    1009.2  -95.592 40.079
    ## 38: 725640    WY 26.7     2.1    1014.2 -104.800 41.150
    ## 39: 725866    ID 32.2     4.6    1007.8 -114.486 42.482
    ## 40: 726064    ME 22.8     2.6    1015.5  -70.708 43.394
    ## 41: 726163    NH 23.3     0.0    1017.5  -72.004 42.805
    ## 42: 726166    VT 26.1     4.1    1020.1  -73.246 42.891
    ## 43: 726505    WI 18.3     2.1    1021.3  -87.938 42.595
    ## 44: 726525    SD 18.3     3.1    1020.2  -97.364 42.879
    ## 45: 726586    MN 22.2     3.1    1020.5  -94.416 43.644
    ## 46: 726798    MT 23.3    12.4    1017.0 -110.440 45.698
    ##     USAFID STATE temp wind.sp atm.press      lon    lat

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.
