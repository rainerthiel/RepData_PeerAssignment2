---
title: "Reproducible Research: Peer Assessment 2"
author: "SlowLearner"
date: "2025-05-27"
output: 
  html_document:
    toc: TRUE
    toc_depth: 4
    toc_float: TRUE
    keep_md: true
---

------------------------------------------------------------------------

# Severe weather events in the USA and their consequences

------------------------------------------------------------------------

## Introduction

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.<br/> This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Synopsis

The objective of our analysis is to use the NOAA storm database to answer the following questions.

Q1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

Q2. Across the United States, which types of events have the greatest economic consequences?

Fatalities and injuries variables (`FATALITIES`, `INJURIES`) will be used to identify event types [most harmful to population health]{.underline} (Q1)<br/> Property and crop damage variables (`PROPDMG`, `CROPDMG`) will be used to identify event types with the [greatest economic consequences]{.underline} (Q2)<br/><br/> We are not expected to qualify our analysis results either by location or date.<br/><br/> The analysis variables are measured at 2 different scales:

-   Fatalities and injuries variables are expressed as a count of individuals affected.

-   Property and crop damage variables are expressed as dollar amounts.

These variables are normalized up front to ensure they contribute equally to the analysis.

------------------------------------------------------------------------

## Data Processing and Analysis

### Setup

Load required libraries, assign work variables and show session information.


``` r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(kableExtra)
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

``` r
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

``` r
local_zip <- "StormData.bz2"
local_csv <- "StormData.csv"
source_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
data_dir <- "./Data"
sessionInfo()
```

```
## R version 4.3.3 (2024-02-29 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19045)
## 
## Matrix products: default
## 
## 
## locale:
## [1] LC_COLLATE=English_South Africa.utf8  LC_CTYPE=English_South Africa.utf8   
## [3] LC_MONETARY=English_South Africa.utf8 LC_NUMERIC=C                         
## [5] LC_TIME=English_South Africa.utf8    
## 
## time zone: Africa/Johannesburg
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] gridExtra_2.3    kableExtra_1.4.0 dplyr_1.1.4      ggplot2_3.5.1   
## 
## loaded via a namespace (and not attached):
##  [1] gtable_0.3.5      jsonlite_1.8.8    compiler_4.3.3    tidyselect_1.2.1 
##  [5] xml2_1.3.6        stringr_1.5.1     jquerylib_0.1.4   systemfonts_1.2.2
##  [9] scales_1.3.0      yaml_2.3.8        fastmap_1.2.0     R6_2.5.1         
## [13] generics_0.1.3    knitr_1.47        tibble_3.2.1      munsell_0.5.1    
## [17] svglite_2.1.3     bslib_0.7.0       pillar_1.9.0      rlang_1.1.3      
## [21] utf8_1.2.4        cachem_1.1.0      stringi_1.8.3     xfun_0.44        
## [25] sass_0.4.9        viridisLite_0.4.2 cli_3.6.2         withr_3.0.0      
## [29] magrittr_2.0.3    digest_0.6.35     grid_4.3.3        rstudioapi_0.16.0
## [33] lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.24.0   glue_1.7.0       
## [37] fansi_1.0.6       colorspace_2.1-0  rmarkdown_2.27    tools_4.3.3      
## [41] pkgconfig_2.0.3   htmltools_0.5.8.1
```

------------------------------------------------------------------------

### Data preparation

#### Retrieve source data

The source data for analysis is a compressed comma separated values (.csv) located at <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2>.

#### Load data into R environment

1.  The file is downloaded to a local file folder.


``` r
    local_zip <- paste(data_dir, local_zip, sep='/')

    if (!dir.exists(data_dir)) dir.create(data_dir)
    if(!file.exists(local_zip)) {
        download.file(source_url, destfile=local_zip , mode='wb')
    }
```

2.  Create the analysis input dataset `local_csv` using the `read.table` command (no need need to unzip the compressed file separately, this command handles automatically)
3.  Provide some basic stats about the file created.


``` r
    storms_data <- read.table(local_zip, sep=',', header=TRUE)
```

##### Loaded analysis data overview


``` r
typ <- class(storms_data)
nro <- nrow(storms_data)
nva <- ncol(storms_data)
icv <- complete.cases(storms_data)
nic <- nro - length(icv[icv == FALSE])
```

-   `storms_data` is of type data.frame.
-   It contains *902297* observations.
-   Each observation has *37* variables.
-   There are *0* incomplete cases in the data
-   Compact structure display


``` r
str(storms_data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr  "" "" "" "" ...
##  $ BGN_LOCATI: chr  "" "" "" "" ...
##  $ END_DATE  : chr  "" "" "" "" ...
##  $ END_TIME  : chr  "" "" "" "" ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr  "" "" "" "" ...
##  $ END_LOCATI: chr  "" "" "" "" ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
##  $ WFO       : chr  "" "" "" "" ...
##  $ STATEOFFIC: chr  "" "" "" "" ...
##  $ ZONENAMES : chr  "" "" "" "" ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : chr  "" "" "" "" ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

#### Discard noise

Remove attributes not required for the analysis and free unused memory.


``` r
storms_abbr <- select(storms_data,c('EVTYPE',
                                    'FATALITIES',
                                    'INJURIES',
                                    'PROPDMG',
                                    'CROPDMG'))
str(storms_abbr)
```

```
## 'data.frame':	902297 obs. of  5 variables:
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
```

``` r
gc()
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  1439012  76.9    2503190  133.7   1662101   88.8
## Vcells 60519346 461.8  177304376 1352.8 221630414 1691.0
```

------------------------------------------------------------------------

### Data Analysis

The prepared file `storms_abbr` is processed by:

-   normalizing the data
-   grouping by event type `EVTYPE`
-   summarizing to provide totals of each variable under scrutiny

Results are stored in `results_data`.


``` r
normalize <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}
norm_data <- as.data.frame(lapply(storms_abbr[2:5], normalize))
norm_data$EVTYPE <- storms_abbr$EVTYPE

summary_data <- summarize(group_by(norm_data, EVTYPE),
                          totFATALITIES = sum(FATALITIES),
                          totINJURIES = sum(INJURIES),
                          totPROPDMG = sum(PROPDMG),
                          totCROPDMG = sum(CROPDMG))
summary(summary_data)
```

```
##     EVTYPE          totFATALITIES      totINJURIES         totPROPDMG     
##  Length:985         Min.   :0.00000   Min.   : 0.00000   Min.   :  0.000  
##  Class :character   1st Qu.:0.00000   1st Qu.: 0.00000   1st Qu.:  0.000  
##  Mode  :character   Median :0.00000   Median : 0.00000   Median :  0.000  
##                     Mean   :0.02637   Mean   : 0.08392   Mean   :  2.210  
##                     3rd Qu.:0.00000   3rd Qu.: 0.00000   3rd Qu.:  0.007  
##                     Max.   :9.66209   Max.   :53.73294   Max.   :642.452  
##    totCROPDMG     
##  Min.   :  0.000  
##  1st Qu.:  0.000  
##  Median :  0.000  
##  Mean   :  1.413  
##  3rd Qu.:  0.000  
##  Max.   :585.451
```

``` r
results_data <- summary_data %>%
                filter(totFATALITIES == max(totFATALITIES) |
                       totINJURIES == max(totINJURIES) | 
                           totPROPDMG == max(totPROPDMG) |
                           totCROPDMG == max(totCROPDMG))
```
#### Plots

[Terms used in the plots]{.underline}

-   *Consequence* - umbrella term for the variables under scrutiny: `FATALITIES`, `INJURIES`, `PROPDMG`, `CROPDMG`
-   *Severity* - the normalized totals of each consequence
-   *Peril* - synonym for storm / weather event type



`summary_data` is transformed into a list of the top 5 event types for each variable under scrutiny (`top5_data`).  

The plots below show storm and other weather events by *Peril* and categorized by *Consequence* for the top 5 event types in terms of severity.


``` r
top5_data <- bind_rows(
    mutate(select(arrange(summary_data, desc(totFATALITIES)),
                 count = totFATALITIES,
                 event_type = EVTYPE),
                 category = "FATALITIES")[1:5,],
    mutate(select(arrange(summary_data, desc(totINJURIES)),
                 count = totINJURIES,
                 event_type = EVTYPE),
                 category = "INJURIES")[1:5,],
    mutate(select(arrange(summary_data, desc(totPROPDMG)),
                 count = totPROPDMG,
                 event_type = EVTYPE),
                 category = "PROPDMG")[1:5,],
    mutate(select(arrange(summary_data, desc(totCROPDMG)),
                 count = totCROPDMG,
                 event_type = EVTYPE),
                 category = "CROPDMG")[1:5,])

p1 <- ggplot(data=top5_data[top5_data$category %in% c("FATALITIES", "INJURIES"),],
       aes(fill=category, y=count, x=event_type)) +
       geom_bar(stat="identity", width=.9) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
             plot.margin = margin(0,0.0,0.2,0.7, "cm")) +
       labs(x = "Peril",
            y = "Severity",
       title = "Health outcomes") + 
       scale_fill_brewer(palette=4, direction=-1)

p2 <- ggplot(data=top5_data[top5_data$category %in% c("PROPDMG", "CROPDMG"),],
       aes(fill=category, y=count, x=event_type)) +
       geom_bar(stat="identity", width=.9) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
             plot.margin = margin(0,0.0,0.2,0.7, "cm")) +
       labs(x = "Peril",
            y = "Severity",
       title = "Economic impact") + 
       scale_fill_brewer(palette="Set3", direction=-1)

grid.arrange(grobs = list(p1, p2), ncol = 2, top="Figure 1: Severities by Peril")
```

![](PA2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
<br/><br/><br/>

The data of `top5_data` (see table below) shows most clearly  that i) `EXCESSIVE HEAT` is amongst the top 5 health consequences, and ii) `FLASH FLOODS` are the 2^nd^ most severe economic consequence for both property and crop damage.

Further analysis could be done to see how these have changed since measurements began.


``` r
select(top5_data, category, event_type, count) %>%
             group_by(category) %>%
             kbl(caption = "Table 1: Top 5 severe weather event types") %>% 
             kable_material(c("basic")) %>%
             kable_styling(bootstrap_options = "striped",
                           font_size = 12) %>%
             collapse_rows(valign = "top")
```

<table class=" lightable-material table table-striped" style='font-family: "Source Sans Pro", helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 12px; margin-left: auto; margin-right: auto;'>
<caption style="font-size: initial !important;">Table 1: Top 5 severe weather event types</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> category </th>
   <th style="text-align:left;"> event_type </th>
   <th style="text-align:right;"> count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="5"> FATALITIES </td>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 9.662093 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> EXCESSIVE HEAT </td>
   <td style="text-align:right;"> 3.264151 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> FLASH FLOOD </td>
   <td style="text-align:right;"> 1.677530 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> HEAT </td>
   <td style="text-align:right;"> 1.607204 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> LIGHTNING </td>
   <td style="text-align:right;"> 1.399657 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="5"> INJURIES </td>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 53.732941 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> TSTM WIND </td>
   <td style="text-align:right;"> 4.092353 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> FLOOD </td>
   <td style="text-align:right;"> 3.993529 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> EXCESSIVE HEAT </td>
   <td style="text-align:right;"> 3.838235 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> LIGHTNING </td>
   <td style="text-align:right;"> 3.076471 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="5"> PROPDMG </td>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 642.451632 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> FLASH FLOOD </td>
   <td style="text-align:right;"> 284.024918 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> TSTM WIND </td>
   <td style="text-align:right;"> 267.193122 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> FLOOD </td>
   <td style="text-align:right;"> 179.987696 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> THUNDERSTORM WIND </td>
   <td style="text-align:right;"> 175.368834 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="5"> CROPDMG </td>
   <td style="text-align:left;"> HAIL </td>
   <td style="text-align:right;"> 585.450788 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> FLASH FLOOD </td>
   <td style="text-align:right;"> 181.010566 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> FLOOD </td>
   <td style="text-align:right;"> 169.735232 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> TSTM WIND </td>
   <td style="text-align:right;"> 110.305657 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 101.028808 </td>
  </tr>
</tbody>
</table>
## Results

The table below shows the event types that have the severest impact in terms of:

-   Health (Q1)

    -   fatalities
    -   injuries

-   Economic consequences (Q2)

    -   property damage
    -   crop damage  

    
    ``` r
    colnames(results_data) <- c("Event Type",
                              "Total Fatalities",
                              "Total Injuries",
                              "Total Property Damage",
                              "Total Crop Damage")
    results_data %>% 
                 kbl(caption = "Table 2: Weather events with severest impact") %>% 
                 add_header_above(c(" ", "Health" = 2, "Economic" = 2)) %>%
                 kable_styling(bootstrap_options = "striped",
                               font_size = 12) %>%
                 kable_material(c("basic"))
    ```
    
    <table class="table table-striped lightable-material" style='font-size: 12px; margin-left: auto; margin-right: auto; font-family: "Source Sans Pro", helvetica, sans-serif; margin-left: auto; margin-right: auto;'>
    <caption style="font-size: initial !important;">Table 2: Weather events with severest impact</caption>
     <thead>
    <tr>
    <th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
    <th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Health</div></th>
    <th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Economic</div></th>
    </tr>
      <tr>
       <th style="text-align:left;"> Event Type </th>
       <th style="text-align:right;"> Total Fatalities </th>
       <th style="text-align:right;"> Total Injuries </th>
       <th style="text-align:right;"> Total Property Damage </th>
       <th style="text-align:right;"> Total Crop Damage </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:left;"> HAIL </td>
       <td style="text-align:right;"> 0.025729 </td>
       <td style="text-align:right;"> 0.8005882 </td>
       <td style="text-align:right;"> 137.7387 </td>
       <td style="text-align:right;"> 585.4508 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> TORNADO </td>
       <td style="text-align:right;"> 9.662093 </td>
       <td style="text-align:right;"> 53.7329412 </td>
       <td style="text-align:right;"> 642.4516 </td>
       <td style="text-align:right;"> 101.0288 </td>
      </tr>
    </tbody>
    </table>
### Conclusion

    > *Tornadoes* are the most destructive weather events regarding fatalities, injuries and property damage.<br/><br/>*Hail* storms do the most damage to crops. Although tornadoes also wreak havoc to crops, the economic impact of hail storms is close to six times greater!

<p style="text-align: center;"><strong>----- End -----</strong></p>
