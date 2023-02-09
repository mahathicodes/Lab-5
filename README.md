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
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(dplyr)
library(ggplot2)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

``` r
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
met_state <- met_stations[n == minrecord, .(USAFID, STATE, lon, lat)]
```

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
met_middle <- unique(met[, .(USAFID, STATE, lon, lat)])
met_middle <- na.omit(met_middle)

met_middle[, lat_mid := quantile(lat, probs = .5, na.rm = TRUE), by = STATE]
met_middle[, lon_mid := quantile(lon, probs = .5, na.rm = TRUE), by = STATE]

met_middle[,  distance := sqrt((lat_mid - met_middle$lat)^2 + (lon_mid - met_middle$lat)^2)]

met_middle[, minrecord := which.min(distance), by = STATE]
met_middle[, n := 1:.N, by = STATE]
met_midpt <- met_middle[n == minrecord, .(USAFID, STATE, lon, lat)]
```

``` r
met_state[, type := "Q2"]
met_midpt[, type := "Q3"]
met_combined <- rbind(met_state, met_midpt)
rh <- colorFactor(c('blue', 'red'),
                       domain = as.factor(met_combined$type))
leaflet(met_combined) %>%
  addProviderTiles("OpenStreetMap") %>%
  addCircles(lng = ~lon, lat = ~lat, color=~rh(type))
```

<div class="leaflet html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f29ce28fd2b912d11ef2" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f29ce28fd2b912d11ef2">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[42.07,36.855,24.55,32.477,35.976,41.35,30.783,30.626,29.71,31.178,26.166,32.262,31.418,32.567,34.267,35.033,36.773,33.221,34.344,39.366,39.873,38.689,36.573,37.298,39.103,38.037,37.778,37.044,37.14,37.091,40.777,41.158,41.25,41.868,40.63,38.142,40.079,41.15,42.482,43.394,42.805,42.891,42.595,42.879,43.644,45.698,32.217,46.677,41.736,36.611,42.07,30.29,46.217,36.533,29.117,35.947,27.63,43.62,24.55,41.35,30.783,33.929,41.037,26.166,32.262,31.418,32.567,35.033,33.221,33.909,39.366,39.873,38.689,36.573,37.298,39.103,38.037,37,37.14,37.091,37.064,40.777,41.158,41.25,40.46,38.142,40.079,42.149,43.394,42.805,42.891,42.595,42.879,44.683],[-124.29,-84.856,-81.75,-80.723,-115.132,-71.799,-83.277,-88.068,-91.339,-90.472,-97.346,-107.72,-110.848,-117.116,-77.9,-85.2,-90.325,-92.814,-98.983,-75.078,-75.227,-75.359,-79.335,-81.204,-84.419,-87.532,-89.252,-100.96,-107.76,-113.593,-73.873,-73.129,-70.667,-84.079,-93.9,-76.429,-95.592,-104.8,-114.486,-70.708,-72.004,-73.246,-87.938,-97.364,-94.416,-110.44,-80.7,-122.983,-83.655,-83.738,-124.29,-87.672,-97.633,-93.2,-89.55,-114.861,-90.45,-96.22,-81.75,-71.799,-83.277,-78.075,-107.492,-97.346,-107.72,-110.848,-117.116,-85.2,-92.814,-94.859,-75.078,-75.227,-75.359,-79.335,-81.204,-84.419,-87.532,-101.883,-107.76,-113.593,-89.219,-73.873,-73.129,-70.667,-91.428,-76.429,-95.592,-112.287,-70.708,-72.004,-73.246,-87.938,-97.364,-111.116],10,null,null,{"interactive":true,"className":"","stroke":true,"color":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"weight":5,"opacity":0.5,"fill":true,"fillColor":["#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#0000FF","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000"],"fillOpacity":0.2},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[24.55,46.677],"lng":[-124.29,-70.667]}},"evals":[],"jsHooks":[]}</script>

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

``` r
met_avg_temp <- met %>% 
  group_by(STATE) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE)
  )

met_low_temp <- met_avg_temp %>% 
  filter(temp < 20)
met_mid_temp <- met_avg_temp %>% 
  filter(temp >= 20 & temp < 25)
met_high_temp <- met_avg_temp %>% 
  filter(temp >= 25)
```

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

``` r
met_low_stations <- met %>% 
  select(USAFID, STATE, lon, lat) %>% 
  distinct() %>% 
  filter(STATE %in% c("CO", "ME", "MN", "MT", "ND", "NH", "OR", "VT", "WA", "WI", "WY"))

met_low_temp %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE))
```

    ## Source: local data table [1 x 3]
    ## Call:   `_DT2`[, .(temp = mean(temp, na.rm = TRUE), wind.sp = mean(wind.sp, 
    ##     na.rm = TRUE), atm.press = mean(atm.press, na.rm = TRUE)), 
    ##     keyby = .(STATE)][temp < 20, .(temp = mean(temp, na.rm = TRUE), 
    ##     wind.sp = mean(wind.sp, na.rm = TRUE), atm.press = mean(atm.press, 
    ##         na.rm = TRUE))]
    ## 
    ##    temp wind.sp atm.press
    ##   <dbl>   <dbl>     <dbl>
    ## 1  18.7    2.55     1014.
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Low temperature level Number of entries (records) - 11 Number of NA
entries - There are two NaN entries in the atm.press column. Number of
stations - 466 Number of states included - 11 Mean temperature,
wind-speed, and atmospheric pressure - 18.7C, 2.55mph, 1014mmHg
respectively

``` r
met_mid_stations <- met %>% 
  select(USAFID, STATE, lon, lat) %>% 
  distinct() %>% 
  filter(STATE %in% c("CA", "CT", "DE", "IA", "ID", "IL", "IN", "KS", "KY", "MA", "MD", "MI", "MO", "NC", "NE", "NJ", "NM", "NY", "OH", "PA", "RI", "SD", "TN", "VA", "WV"))

met_mid_temp %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE))
```

    ## Source: local data table [1 x 3]
    ## Call:   `_DT2`[, .(temp = mean(temp, na.rm = TRUE), wind.sp = mean(wind.sp, 
    ##     na.rm = TRUE), atm.press = mean(atm.press, na.rm = TRUE)), 
    ##     keyby = .(STATE)][temp >= 20 & temp < 25, .(temp = mean(temp, 
    ##     na.rm = TRUE), wind.sp = mean(wind.sp, na.rm = TRUE), atm.press = mean(atm.press, 
    ##     na.rm = TRUE))]
    ## 
    ##    temp wind.sp atm.press
    ##   <dbl>   <dbl>     <dbl>
    ## 1  22.6    2.39     1015.
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Mid temperature level Number of entries (records) - 25 Number of NA
entries - 0 Number of stations - 1435 Number of states included - 25
Mean temperature, wind-speed, and atmospheric pressure - 22.6C, 2.39mph,
1015mmHg respectively

``` r
met_high_stations <- met %>% 
  select(USAFID, STATE, lon, lat) %>% 
  distinct() %>% 
  filter(STATE %in% c("AL", "AR", "AZ", "FL", "GA", "LA", "MS", "NV", "OK", "SC", "TX", "UT"))

met_high_temp %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE))
```

    ## Source: local data table [1 x 3]
    ## Call:   `_DT2`[, .(temp = mean(temp, na.rm = TRUE), wind.sp = mean(wind.sp, 
    ##     na.rm = TRUE), atm.press = mean(atm.press, na.rm = TRUE)), 
    ##     keyby = .(STATE)][temp >= 25, .(temp = mean(temp, na.rm = TRUE), 
    ##     wind.sp = mean(wind.sp, na.rm = TRUE), atm.press = mean(atm.press, 
    ##         na.rm = TRUE))]
    ## 
    ##    temp wind.sp atm.press
    ##   <dbl>   <dbl>     <dbl>
    ## 1  27.0    2.43     1014.
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

High temperature level Number of entries (records) - 12 Number of NA
entries - 0 Number of stations - 983 Number of states included - 12 Mean
temperature, wind-speed, and atmospheric pressure - 27C, 2.43mph,
1014mmHg respectively

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

``` r
met_med_lz <- met_lz %>% 
  group_by(USAFID) %>% 
  summarise(
    temp = quantile(temp, probs = .5, na.rm = TRUE),
    wind.sp = quantile(wind.sp, probs = .5, na.rm = TRUE),
    atm.press = quantile(atm.press, probs = .5, na.rm = TRUE))
met_med_lz <- as.data.frame(met_med_lz)

ggplot(met_med_lz, aes(x=wind.sp, y=temp))+
    geom_point()+
    geom_smooth(method='lm',col="red")+
    geom_smooth(col="blue")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 16 rows containing non-finite values (`stat_smooth()`).

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 16 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 16 rows containing missing values (`geom_point()`).

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
lmod <- lm(temp~wind.sp,data=met_med_lz)
summary(lmod)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ wind.sp, data = met_med_lz)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -17.7243  -2.6518  -0.2309   2.7691  14.5052 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 22.23088    0.21779  102.08  < 2e-16 ***
    ## wind.sp      0.48614    0.08212    5.92 3.94e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.849 on 1577 degrees of freedom
    ##   (16 observations deleted due to missingness)
    ## Multiple R-squared:  0.02174,    Adjusted R-squared:  0.02112 
    ## F-statistic: 35.05 on 1 and 1577 DF,  p-value: 3.941e-09

``` r
plot(lmod)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
gmod <- gam(temp~s(wind.sp, fx=TRUE, bs="cr"), data=met_med_lz)
plot(gmod)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
summary(gmod)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(wind.sp, fx = TRUE, bs = "cr")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 23.38566    0.09548   244.9   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##            edf Ref.df     F  p-value    
    ## s(wind.sp)   9      9 10.01 4.52e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0489   Deviance explained = 5.43%
    ## GCV = 14.486  Scale est. = 14.394    n = 1579

The spline model is a better fit than the linear model. We can tell this
from the adjusted $R^2$ value. The spline model (0.0489) has a higher
value than the linear model (0.0211), however, the values themselves are
very low and may not be significant.

References

1.  <https://github.com/cbegay89/PM566-labs/blob/master/05-lab.rmd>
