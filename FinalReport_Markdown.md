##### Green Buildings

For the green building problem, we wanted to explore and calculate how much money we can save on energy and utility bills. Because green building should be energy efficient and we will save more money (= profit, basically)

``` r
library(mosaic)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Loading required package: ggplot2

    ## Loading required package: ggstance

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Loading required package: Matrix

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median,
    ##     prop.test, quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

``` r
library(readr)
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------- tidyverse 1.2.1 --

    ## v tibble  2.1.3     v purrr   0.3.2
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v tibble  2.1.3     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------- tidyverse_conflicts() --
    ## x mosaic::count()            masks dplyr::count()
    ## x purrr::cross()             masks mosaic::cross()
    ## x mosaic::do()               masks dplyr::do()
    ## x tidyr::expand()            masks Matrix::expand()
    ## x dplyr::filter()            masks stats::filter()
    ## x ggstance::geom_errorbarh() masks ggplot2::geom_errorbarh()
    ## x dplyr::lag()               masks stats::lag()
    ## x mosaic::stat()             masks ggplot2::stat()
    ## x mosaic::tally()            masks dplyr::tally()

``` r
green = read_csv("greenbuildings.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
head(green)
```

    ## # A tibble: 6 x 23
    ##   CS_PropertyID cluster   size empl_gr  Rent leasing_rate stories   age
    ##           <dbl>   <dbl>  <dbl>   <dbl> <dbl>        <dbl>   <dbl> <dbl>
    ## 1        379105       1 260300    2.22  38.6         91.4      14    16
    ## 2        122151       1  67861    2.22  28.6         87.1       5    27
    ## 3        379839       1 164848    2.22  33.3         88.9      13    36
    ## 4         94614       1  93372    2.22  35           97.0      13    46
    ## 5        379285       1 174307    2.22  40.7         96.6      16     5
    ## 6         94765       1 231633    2.22  43.2         92.7      14    20
    ## # ... with 15 more variables: renovated <dbl>, class_a <dbl>,
    ## #   class_b <dbl>, LEED <dbl>, Energystar <dbl>, green_rating <dbl>,
    ## #   net <dbl>, amenities <dbl>, cd_total_07 <dbl>, hd_total07 <dbl>,
    ## #   total_dd_07 <dbl>, Precipitation <dbl>, Gas_Costs <dbl>,
    ## #   Electricity_Costs <dbl>, cluster_rent <dbl>

``` r
cluster<- green %>% group_by(cluster)
cluster
```

    ## # A tibble: 7,894 x 23
    ## # Groups:   cluster [693]
    ##    CS_PropertyID cluster   size empl_gr  Rent leasing_rate stories   age
    ##            <dbl>   <dbl>  <dbl>   <dbl> <dbl>        <dbl>   <dbl> <dbl>
    ##  1        379105       1 260300    2.22  38.6         91.4      14    16
    ##  2        122151       1  67861    2.22  28.6         87.1       5    27
    ##  3        379839       1 164848    2.22  33.3         88.9      13    36
    ##  4         94614       1  93372    2.22  35           97.0      13    46
    ##  5        379285       1 174307    2.22  40.7         96.6      16     5
    ##  6         94765       1 231633    2.22  43.2         92.7      14    20
    ##  7        236739       6 210038    4.01  12.5         94.3      11    38
    ##  8        234578       6 225895    4.01  14.8         91.0      15    24
    ##  9         42087       6 912011    4.01  17           99.3      31    34
    ## 10        233989       6 518578    4.01  17           93.5      21    36
    ## # ... with 7,884 more rows, and 15 more variables: renovated <dbl>,
    ## #   class_a <dbl>, class_b <dbl>, LEED <dbl>, Energystar <dbl>,
    ## #   green_rating <dbl>, net <dbl>, amenities <dbl>, cd_total_07 <dbl>,
    ## #   hd_total07 <dbl>, total_dd_07 <dbl>, Precipitation <dbl>,
    ## #   Gas_Costs <dbl>, Electricity_Costs <dbl>, cluster_rent <dbl>

We have ordered the data by their cluster, and we analyze the hot days & cold days total to figure out the demand for energy distribution across the clusters.

``` r
summary(green$hd_total07)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    1419    2739    3432    4796    7200

``` r
summary(green$cd_total_07)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      39     684     966    1229    1620    5240

We decide to only exam the clusters that have similar weather to Austin, so our conclusion would be more relatable and convincing. We take out the data with "net=1" because residents pay their own utility bills and as building owners won't save much.

``` r
green_zero = green %>% filter(net == 0 ) 
green_one = green %>% filter(net == 1 )

a = data.frame(group ="cd",value= green_zero$cd_total_07)
b = data.frame(group = "hd", value= green_zero$hd_total07)
plot.data = rbind(a, b) 
plot.data %>% group_by(group)
```

    ## # A tibble: 15,240 x 2
    ## # Groups:   group [2]
    ##    group value
    ##    <fct> <dbl>
    ##  1 cd     4988
    ##  2 cd     4988
    ##  3 cd     4988
    ##  4 cd     4988
    ##  5 cd     4988
    ##  6 cd     4988
    ##  7 cd     2746
    ##  8 cd     2746
    ##  9 cd     2746
    ## 10 cd     2746
    ## # ... with 15,230 more rows

``` r
ggplot(plot.data, aes(x=group, y=value, fill=group)) +
 geom_boxplot()
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
f = data.frame(group = "non_net", value = nrow(green_zero))
d = data.frame(group = 'net', value = nrow(green_one))
c = rbind(f,d)
ggplot(c, aes(x=group, y=value, fill = group))+
  geom_col()
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-4-2.png) To define the "Austin weather", we take heatdays &gt; median and colddays &lt;median. Next we only gona work with cluster that matches this condition.

``` r
one_q = quantile(green_zero$cd_total_07, .5)
one_q
```

    ## 50% 
    ## 966

``` r
three_q= quantile(green_zero$hd_total07, .5)

green_filter = green_zero  %>% 
  filter(green_zero$cd_total_07 < one_q)

green_filt = green_filter %>%
  filter(green_filter$hd_total07 > three_q)

print(green_filt)
```

    ## # A tibble: 788 x 23
    ##    CS_PropertyID cluster   size empl_gr  Rent leasing_rate stories   age
    ##            <dbl>   <dbl>  <dbl>   <dbl> <dbl>        <dbl>   <dbl> <dbl>
    ##  1       1212036      20 385469      NA  18.0         99.5      14    20
    ##  2       1211872      20 127982      NA  15           82.3      18    86
    ##  3       1212027      20  63150      NA  15           66.2       6    85
    ##  4       1216881      20  98725      NA  15.5         85.8       6    97
    ##  5       1211896      20   3000      NA  16           50         4   127
    ##  6       1216662      20  13100      NA  16           11.0       1    45
    ##  7       1211247      20 850000      NA  16.5         95.9      11    90
    ##  8       1211853      20  70377      NA  18           75.4       6    45
    ##  9       1215535      20 300000      NA  18           63.4      27    40
    ## 10       1216862      20 309686      NA  18.6         86.1       8    81
    ## # ... with 778 more rows, and 15 more variables: renovated <dbl>,
    ## #   class_a <dbl>, class_b <dbl>, LEED <dbl>, Energystar <dbl>,
    ## #   green_rating <dbl>, net <dbl>, amenities <dbl>, cd_total_07 <dbl>,
    ## #   hd_total07 <dbl>, total_dd_07 <dbl>, Precipitation <dbl>,
    ## #   Gas_Costs <dbl>, Electricity_Costs <dbl>, cluster_rent <dbl>

``` r
green_filt%>% group_by(cluster)%>%summarize(count = n())
```

    ## # A tibble: 52 x 2
    ##    cluster count
    ##      <dbl> <int>
    ##  1      20    22
    ##  2      52    19
    ##  3      58     4
    ##  4      81    14
    ##  5      87    35
    ##  6      93     5
    ##  7     142     1
    ##  8     164     4
    ##  9     167     9
    ## 10     174     4
    ## # ... with 42 more rows

Then we tried the exact same analysis done by the immature "data scientist" did in the problem, just on our selected dataset.

``` r
df = green_filt %>% filter(green_filt$leasing_rate > 10)
df_green= df %>% filter(green_rating == 1)
df_non_green= df %>% filter(green_rating != 1)

summary(df_green$Rent)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   14.00   17.97   24.00   27.51   36.36   55.94

``` r
summary(df_non_green$Rent)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    5.82   18.27   23.89   24.49   29.55   75.00

``` r
green = median(df_green$Rent)
non_green = median(df_non_green$Rent)
new_rent_dif = green- non_green
amt_saved = new_rent_dif * 250000
amt_saved
```

    ## [1] 27500

We found out that in clusters that have similar weathcer as Austin, the rent median difference isn't that great bewteen green and non-green buildings. So there is a way smaller "extra revenue" we gonna make and so far ($27,500, comparing to 650,000 computed by the guy), it seems we will recuperate the green building costs in way longer than the 7 years time period he calculated. So bad deal?

We continue to explore the revenue generated by energy savings.

``` r
df_green
```

    ## # A tibble: 45 x 23
    ##    CS_PropertyID cluster   size empl_gr  Rent leasing_rate stories   age
    ##            <dbl>   <dbl>  <dbl>   <dbl> <dbl>        <dbl>   <dbl> <dbl>
    ##  1       1212036      20 385469   NA     18.0         99.5      14    20
    ##  2        717047      52 315133    3.39  25.2         81.2      20    25
    ##  3       1258500      58  60185    0.47  16           94.5       3    27
    ##  4        469978      81 408459    2.44  37.9         92.6      24    24
    ##  5         42051      87  56303    2.44  24.2         89.7       5    97
    ##  6        804325      93 239250    2.44  37.5         97.3      14    13
    ##  7       1271164     164  98567    0.47  14           98.2       3     5
    ##  8       1376269     167 149426    4.02  14.5         93.9      14    39
    ##  9        717026     174  30000    3.39  15.9         96.1       3    98
    ## 10        102132     175 215000    0.26  16           88.4       6    95
    ## # ... with 35 more rows, and 15 more variables: renovated <dbl>,
    ## #   class_a <dbl>, class_b <dbl>, LEED <dbl>, Energystar <dbl>,
    ## #   green_rating <dbl>, net <dbl>, amenities <dbl>, cd_total_07 <dbl>,
    ## #   hd_total07 <dbl>, total_dd_07 <dbl>, Precipitation <dbl>,
    ## #   Gas_Costs <dbl>, Electricity_Costs <dbl>, cluster_rent <dbl>

``` r
df_green$gas = df_green$cd_total_07 * df_green$Gas_Costs
df_green$elec = df_green$hd_total07 * df_green$Electricity_Costs

df_green$total_saved = (df_green$gas + df_green$elec) * 0.25
df_green$total_cost = (df_green$gas + df_green$elec) * 0.75
df_green
```

    ## # A tibble: 45 x 27
    ##    CS_PropertyID cluster   size empl_gr  Rent leasing_rate stories   age
    ##            <dbl>   <dbl>  <dbl>   <dbl> <dbl>        <dbl>   <dbl> <dbl>
    ##  1       1212036      20 385469   NA     18.0         99.5      14    20
    ##  2        717047      52 315133    3.39  25.2         81.2      20    25
    ##  3       1258500      58  60185    0.47  16           94.5       3    27
    ##  4        469978      81 408459    2.44  37.9         92.6      24    24
    ##  5         42051      87  56303    2.44  24.2         89.7       5    97
    ##  6        804325      93 239250    2.44  37.5         97.3      14    13
    ##  7       1271164     164  98567    0.47  14           98.2       3     5
    ##  8       1376269     167 149426    4.02  14.5         93.9      14    39
    ##  9        717026     174  30000    3.39  15.9         96.1       3    98
    ## 10        102132     175 215000    0.26  16           88.4       6    95
    ## # ... with 35 more rows, and 19 more variables: renovated <dbl>,
    ## #   class_a <dbl>, class_b <dbl>, LEED <dbl>, Energystar <dbl>,
    ## #   green_rating <dbl>, net <dbl>, amenities <dbl>, cd_total_07 <dbl>,
    ## #   hd_total07 <dbl>, total_dd_07 <dbl>, Precipitation <dbl>,
    ## #   Gas_Costs <dbl>, Electricity_Costs <dbl>, cluster_rent <dbl>,
    ## #   gas <dbl>, elec <dbl>, total_saved <dbl>, total_cost <dbl>

Here, we made some assumptions about the dataset:

cold days means demand for gas (heating), hot days means demand for electricity (cooling A/C)

So our formulas are:

cd\_total \* Gas\_Costs = gas bill per year hd\_total \* Electricity\_Costs = electricity bill per year

These cost are per unit area per year.

We also assumed that green buildings in general can save 25% energy compare to non-green, according to numbers from LEED and National Geographics websites. So we multiple our cost by 0.25, and count it as the cost saved (extra revenue)

``` r
hist(df_green$total_saved, 10)
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
median_saved = median(df_green$total_saved)
yearly_saved = median_saved * 250000
yearly_saved
```

    ## [1] 9235600

We calculated the median of total\_saved in clusters that have similar weather in Austin, then multiplied it by our building's planned area in the problem, 250,000.

Our yearly saving on utility bills would be around 9,235,600.

``` r
green_mediancost = median(df_green$total_cost)
green_mean = mean(df_green$total_cost)

df_non_green$gas = df_non_green$cd_total_07 * df_non_green$Gas_Costs
df_non_green$elec = df_non_green$hd_total07 * df_non_green$Electricity_Costs
df_non_green$total_cost = (df_non_green$gas + df_non_green$elec)

nongreen_median_cost = median(df_non_green$total_cost)
nongreen_mean = mean(df_non_green$total_cost)

b = data.frame(group = 'Green Buildings', value = green_mean)
a = data.frame(group = 'Nongreen Buildings', value = nongreen_mean)
viz = rbind(a, b)
viz %>% group_by(group)
```

    ## # A tibble: 2 x 2
    ## # Groups:   group [2]
    ##   group              value
    ##   <fct>              <dbl>
    ## 1 Nongreen Buildings  131.
    ## 2 Green Buildings     113.

``` r
ggplot(viz, aes(x=group, y=value, fill=group)) +
  geom_col() + ylab('Cost of Gas+Electricity per year') + xlab("Building Type")
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-10-1.png) hd/cdboth counts bp

``` r
e = green_zero %>%
  filter(green_zero$hd_total07 > three_q)

r = green_zero  %>% 
  filter(green_zero$cd_total_07 < one_q)

c= green_filt

g= data.frame(group= "above hd", value = nrow(e))
d = data.frame(group= "below cd", value = nrow(r))
c = data.frame(group = "total", value = nrow(c))

viz = rbind(g,d,c)
viz %>% group_by(group)
```

    ## # A tibble: 3 x 2
    ## # Groups:   group [3]
    ##   group    value
    ##   <fct>    <int>
    ## 1 above hd  3505
    ## 2 below cd  3660
    ## 3 total      788

``` r
ggplot(viz, aes(x=group, y=value, fill=group)) +
 geom_col() + ylab("demand count")
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-11-1.png)

Because yearly extra saving(revenue) is around 9 million per year, the green building 5% premium fee 5 million extra cost will be recuperated in less than a year. On top of that, the building owner would save a lot more each year on the utility bills for the building running in general.

So based on our approach, we conclude that green building is a great idea. The immature data scientist's approach has the similar conclusion as ours, but he ignored too many factors.

Our approach explored deeper into the factor of weather, and utility cost.

Please clear your Environment before running the next problem. May have repeated variable names\# \#\#\#\#\#Flight at ABIA\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
library(mosaic)
library(tidyverse)
library(ggplot2)
library(ggthemes)
```

    ## 
    ## Attaching package: 'ggthemes'

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     theme_map

``` r
library(ggpubr)
```

    ## Loading required package: magrittr

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
flight = read.csv('ABIA.csv')
attach(flight)
names(flight)
```

    ##  [1] "Year"              "Month"             "DayofMonth"       
    ##  [4] "DayOfWeek"         "DepTime"           "CRSDepTime"       
    ##  [7] "ArrTime"           "CRSArrTime"        "UniqueCarrier"    
    ## [10] "FlightNum"         "TailNum"           "ActualElapsedTime"
    ## [13] "CRSElapsedTime"    "AirTime"           "ArrDelay"         
    ## [16] "DepDelay"          "Origin"            "Dest"             
    ## [19] "Distance"          "TaxiIn"            "TaxiOut"          
    ## [22] "Cancelled"         "CancellationCode"  "Diverted"         
    ## [25] "CarrierDelay"      "WeatherDelay"      "NASDelay"         
    ## [28] "SecurityDelay"     "LateAircraftDelay"

We are interested in looking at the cancellation rate for each airport thus we created a subset with all cancellations.

``` r
cancel = subset(flight, Cancelled == 1)
cancel
```

    ##       Year Month DayofMonth DayOfWeek DepTime CRSDepTime ArrTime
    ## 250   2008     1          1         2      NA        646      NA
    ## 251   2008     1          1         2      NA       1740      NA
    ## 252   2008     1          1         2      NA       1525      NA
    ## 253   2008     1          1         2      NA       1915      NA
    ## 254   2008     1          1         2      NA       1620      NA
    ## 255   2008     1          1         2      NA       1735      NA
    ## 552   2008     1          2         3      NA       1830      NA
    ## 553   2008     1          2         3      NA       2050      NA
    ## 554   2008     1          2         3      NA       1720      NA
    ## 555   2008     1          2         3      NA       1930      NA
    ## 849   2008     1          3         4      NA       1748      NA
    ## 850   2008     1          3         4      NA       1830      NA
    ## 851   2008     1          3         4      NA       1440      NA
    ## 852   2008     1          3         4      NA       1136      NA
    ## 853   2008     1          3         4      NA       1325      NA
    ## 854   2008     1          3         4      NA       1720      NA
    ## 1151  2008     1          4         5      NA        920      NA
    ## 1152  2008     1          4         5      NA       1550      NA
    ## 1153  2008     1          4         5      NA       1430      NA
    ## 1154  2008     1          4         5      NA        800      NA
    ## 1674  2008     1          6         7      NA       2200      NA
    ## 1675  2008     1          6         7      NA       1005      NA
    ## 1676  2008     1          6         7      NA       1740      NA
    ## 2259  2008     1          8         2      NA       1735      NA
    ## 2550  2008     1          9         3      NA       1830      NA
    ## 2551  2008     1          9         3      NA       1720      NA
    ## 2846  2008     1         10         4      NA       1738      NA
    ## 2847  2008     1         10         4      NA        705      NA
    ## 2848  2008     1         10         4      NA       1922      NA
    ## 2849  2008     1         10         4      NA        545      NA
    ## 3147  2008     1         11         5      NA       1724      NA
    ## 3370  2008     1         12         6      NA        605      NA
    ## 3643  2008     1         13         7      NA       1715      NA
    ## 3929  2008     1         14         1      NA        705      NA
    ## 3930  2008     1         14         1      NA       1905      NA
    ## 3931  2008     1         14         1      NA       1955      NA
    ## 3932  2008     1         14         1      NA       1315      NA
    ## 3933  2008     1         14         1      NA        700      NA
    ## 3934  2008     1         14         1      NA        545      NA
    ## 3935  2008     1         14         1      NA       1745      NA
    ## 3936  2008     1         14         1      NA       1805      NA
    ## 3937  2008     1         14         1      NA        850      NA
    ## 3938  2008     1         14         1      NA        755      NA
    ## 3939  2008     1         14         1      NA       1435      NA
    ## 4222  2008     1         15         2      NA        850      NA
    ## 4223  2008     1         15         2      NA        600      NA
    ## 4224  2008     1         15         2      NA        815      NA
    ## 4225  2008     1         15         2      NA       1305      NA
    ## 4514  2008     1         16         3      NA       1520      NA
    ## 4515  2008     1         16         3      NA        705      NA
    ## 4516  2008     1         16         3      NA       2120      NA
    ## 4517  2008     1         16         3      NA       1840      NA
    ## 4810  2008     1         17         4      NA       1724      NA
    ## 4811  2008     1         17         4      NA        720      NA
    ## 4812  2008     1         17         4      NA       2000      NA
    ## 4813  2008     1         17         4      NA       1007      NA
    ## 4814  2008     1         17         4      NA        830      NA
    ## 4815  2008     1         17         4      NA       1040      NA
    ## 5114  2008     1         18         5      NA        632      NA
    ## 5320  2008     1         19         6      NA        830      NA
    ## 5321  2008     1         19         6      NA        720      NA
    ## 5322  2008     1         19         6      NA       1605      NA
    ## 5323  2008     1         19         6      NA        600      NA
    ## 5324  2008     1         19         6      NA       1450      NA
    ## 5325  2008     1         19         6      NA       1007      NA
    ## 5326  2008     1         19         6      NA       1314      NA
    ## 5593  2008     1         20         7      NA       1630      NA
    ## 5594  2008     1         20         7      NA       1600      NA
    ## 5595  2008     1         20         7      NA       1400      NA
    ## 5596  2008     1         20         7      NA       1230      NA
    ## 5892  2008     1         21         1      NA       1724      NA
    ## 5893  2008     1         21         1      NA       2000      NA
    ## 5894  2008     1         21         1      NA       1914      NA
    ## 6175  2008     1         22         2      NA        600      NA
    ## 6176  2008     1         22         2      NA        705      NA
    ## 6177  2008     1         22         2      NA       1600      NA
    ## 6178  2008     1         22         2      NA       1007      NA
    ## 6179  2008     1         22         2      NA        545      NA
    ## 6180  2008     1         22         2      NA       1230      NA
    ## 6470  2008     1         23         3      NA        620      NA
    ## 6471  2008     1         23         3      NA       1735      NA
    ## 6472  2008     1         23         3      NA       1405      NA
    ## 6767  2008     1         24         4      NA        925      NA
    ## 6768  2008     1         24         4      NA       1755      NA
    ## 6769  2008     1         24         4      NA       1955      NA
    ## 6770  2008     1         24         4      NA       1425      NA
    ## 6771  2008     1         24         4      NA       1105      NA
    ## 6772  2008     1         24         4      NA       1555      NA
    ## 7066  2008     1         25         5      NA       1630      NA
    ## 7067  2008     1         25         5      NA       1050      NA
    ## 7068  2008     1         25         5      NA       1400      NA
    ## 7069  2008     1         25         5      NA        800      NA
    ## 7070  2008     1         25         5      NA       1040      NA
    ## 7280  2008     1         26         6      NA        720      NA
    ## 7281  2008     1         26         6      NA       1445      NA
    ## 7282  2008     1         26         6      NA       1007      NA
    ## 7546  2008     1         27         7      NA        920      NA
    ## 7547  2008     1         27         7      NA        705      NA
    ## 7548  2008     1         27         7      NA       1550      NA
    ## 7549  2008     1         27         7      NA       1125      NA
    ## 7550  2008     1         27         7      NA       2120      NA
    ## 7551  2008     1         27         7      NA       1010      NA
    ## 7552  2008     1         27         7      NA       1430      NA
    ## 7553  2008     1         27         7      NA        800      NA
    ## 7842  2008     1         28         1      NA       1640      NA
    ## 7843  2008     1         28         1      NA        550      NA
    ## 7844  2008     1         28         1      NA       1724      NA
    ## 7845  2008     1         28         1      NA       1735      NA
    ## 7846  2008     1         28         1      NA       1140      NA
    ## 7847  2008     1         28         1      NA        955      NA
    ## 7848  2008     1         28         1      NA       1405      NA
    ## 7849  2008     1         28         1      NA       2135      NA
    ## 8122  2008     1         29         2      NA        705      NA
    ## 8123  2008     1         29         2      NA       1405      NA
    ## 8124  2008     1         29         2      NA       1415      NA
    ## 8125  2008     1         29         2      NA       1915      NA
    ## 8126  2008     1         29         2      NA       1735      NA
    ## 8127  2008     1         29         2      NA       1140      NA
    ## 8128  2008     1         29         2      NA       1830      NA
    ## 8129  2008     1         29         2      NA       2045      NA
    ## 8130  2008     1         29         2      NA       1245      NA
    ## 8131  2008     1         29         2      NA       1735      NA
    ## 8132  2008     1         29         2      NA       1235      NA
    ## 8133  2008     1         29         2      NA       1405      NA
    ## 8134  2008     1         29         2      NA       2135      NA
    ## 8135  2008     1         29         2      NA       1435      NA
    ## 8424  2008     1         30         3      NA       1125      NA
    ## 8425  2008     1         30         3      NA       1005      NA
    ## 8426  2008     1         30         3      NA       1010      NA
    ## 8427  2008     1         30         3      NA       1230      NA
    ## 8719  2008     1         31         4      NA       1724      NA
    ## 8720  2008     1         31         4      NA       1600      NA
    ## 8721  2008     1         31         4      NA       1735      NA
    ## 8722  2008     1         31         4      NA       1140      NA
    ## 8723  2008     1         31         4      NA       2000      NA
    ## 8724  2008     1         31         4      NA        815      NA
    ## 8725  2008     1         31         4      NA       1745      NA
    ## 8726  2008     1         31         4      NA       2130      NA
    ## 9023  2008     2          1         5      NA        850      NA
    ## 9024  2008     2          1         5      NA       2045      NA
    ## 9226  2008     2          2         6      NA       1201      NA
    ## 9227  2008     2          2         6      NA       1600      NA
    ## 9228  2008     2          2         6      NA       1140      NA
    ## 9229  2008     2          2         6      NA        700      NA
    ## 9230  2008     2          2         6      NA       1745      NA
    ## 9231  2008     2          2         6      NA        810      NA
    ## 9232  2008     2          2         6      NA        925      NA
    ## 9233  2008     2          2         6      NA        815      NA
    ## 9499  2008     2          3         7      NA       1720      NA
    ## 9500  2008     2          3         7      NA       1201      NA
    ## 9501  2008     2          3         7      NA        605      NA
    ## 9502  2008     2          3         7      NA       1610      NA
    ## 9503  2008     2          3         7      NA       2130      NA
    ## 9785  2008     2          4         1      NA       1450      NA
    ## 9786  2008     2          4         1      NA       1605      NA
    ## 9787  2008     2          4         1      NA       1724      NA
    ## 9788  2008     2          4         1      NA       2050      NA
    ## 9789  2008     2          4         1      NA       1125      NA
    ## 9790  2008     2          4         1      NA       1600      NA
    ## 9791  2008     2          4         1      NA        600      NA
    ## 9792  2008     2          4         1      NA       1140      NA
    ## 9793  2008     2          4         1      NA       1945      NA
    ## 9794  2008     2          4         1      NA        830      NA
    ## 9795  2008     2          4         1      NA       2000      NA
    ## 9796  2008     2          4         1      NA       1010      NA
    ## 9797  2008     2          4         1      NA       1930      NA
    ## 9798  2008     2          4         1      NA        815      NA
    ## 9799  2008     2          4         1      NA       1745      NA
    ## 10078 2008     2          5         2      NA       1720      NA
    ## 10079 2008     2          5         2      NA        705      NA
    ## 10080 2008     2          5         2      NA       1730      NA
    ## 10081 2008     2          5         2      NA       1735      NA
    ## 10082 2008     2          5         2      NA       1610      NA
    ## 10083 2008     2          5         2      NA        545      NA
    ## 10084 2008     2          5         2      NA       1610      NA
    ## 10085 2008     2          5         2      NA       1915      NA
    ## 10360 2008     2          6         3      NA       1055      NA
    ## 10361 2008     2          6         3      NA       1130      NA
    ## 10362 2008     2          6         3      NA       1125      NA
    ## 10363 2008     2          6         3      NA        605      NA
    ## 10364 2008     2          6         3      NA       1600      NA
    ## 10365 2008     2          6         3      NA        955      NA
    ## 10366 2008     2          6         3      NA       1805      NA
    ## 10367 2008     2          6         3      NA       1735      NA
    ## 10368 2008     2          6         3      NA       1140      NA
    ## 10369 2008     2          6         3      NA       2115      NA
    ## 10370 2008     2          6         3      NA        905      NA
    ## 10371 2008     2          6         3      NA       2000      NA
    ## 10372 2008     2          6         3      NA       1010      NA
    ## 10373 2008     2          6         3      NA       1405      NA
    ## 10374 2008     2          6         3      NA        815      NA
    ## 10375 2008     2          6         3      NA       1230      NA
    ## 10376 2008     2          6         3      NA       1745      NA
    ## 10377 2008     2          6         3      NA       2130      NA
    ## 10672 2008     2          7         4      NA       2200      NA
    ## 10673 2008     2          7         4      NA       1130      NA
    ## 10674 2008     2          7         4      NA       1624      NA
    ## 10675 2008     2          7         4      NA        905      NA
    ## 10971 2008     2          8         5      NA       1724      NA
    ## 10972 2008     2          8         5      NA       1205      NA
    ## 10973 2008     2          8         5      NA        925      NA
    ## 11183 2008     2          9         6      NA        900      NA
    ## 11750 2008     2         11         1      NA       1730      NA
    ## 11751 2008     2         11         1      NA       1610      NA
    ## 12035 2008     2         12         2      NA       1724      NA
    ## 12036 2008     2         12         2      NA       1600      NA
    ## 12037 2008     2         12         2      NA       1745      NA
    ## 12038 2008     2         12         2      NA       2045      NA
    ## 12328 2008     2         13         3      NA       2050      NA
    ## 12329 2008     2         13         3      NA       1610      NA
    ## 12330 2008     2         13         3      NA       1705      NA
    ## 12331 2008     2         13         3      NA       1140      NA
    ## 12332 2008     2         13         3      NA        700      NA
    ## 12333 2008     2         13         3      NA       1930      NA
    ## 12334 2008     2         13         3      NA       1335      NA
    ## 12335 2008     2         13         3      NA        815      NA
    ## 12336 2008     2         13         3      NA       1410      NA
    ## 12632 2008     2         14         4      NA       1720      NA
    ## 12633 2008     2         14         4      NA        705      NA
    ## 12634 2008     2         14         4      NA       1155      NA
    ## 12635 2008     2         14         4      NA       2115      NA
    ## 12636 2008     2         14         4      NA       2045      NA
    ## 12637 2008     2         14         4      NA        705      NA
    ## 12638 2008     2         14         4      NA       1624      NA
    ## 12639 2008     2         14         4      NA       1950      NA
    ## 12942 2008     2         15         5      NA        605      NA
    ## 12943 2008     2         15         5      NA       1950      NA
    ## 13149 2008     2         16         6      NA        605      NA
    ## 13150 2008     2         16         6      NA       1205      NA
    ## 13151 2008     2         16         6      NA       1610      NA
    ## 13152 2008     2         16         6      NA       1700      NA
    ## 13153 2008     2         16         6      NA       1805      NA
    ## 13154 2008     2         16         6      NA       1525      NA
    ## 13155 2008     2         16         6      NA       1105      NA
    ## 13156 2008     2         16         6      NA       1335      NA
    ## 13157 2008     2         16         6      NA        925      NA
    ## 13158 2008     2         16         6      NA       1735      NA
    ## 13159 2008     2         16         6      NA       1520      NA
    ## 13160 2008     2         16         6      NA       1935      NA
    ## 13161 2008     2         16         6      NA       1625      NA
    ## 13422 2008     2         17         7      NA       1030      NA
    ## 13423 2008     2         17         7      NA       1130      NA
    ## 13424 2008     2         17         7      NA       1735      NA
    ## 13425 2008     2         17         7      NA       1140      NA
    ## 13426 2008     2         17         7      NA        700      NA
    ## 13427 2008     2         17         7      NA       1400      NA
    ## 13428 2008     2         17         7      NA       1724      NA
    ## 13429 2008     2         17         7      NA       1405      NA
    ## 13430 2008     2         17         7      NA       2130      NA
    ## 13725 2008     2         18         1      NA        600      NA
    ## 13726 2008     2         18         1      NA       1730      NA
    ## 14021 2008     2         19         2      NA        600      NA
    ## 14022 2008     2         19         2      NA       1905      NA
    ## 14023 2008     2         19         2      NA       1140      NA
    ## 14024 2008     2         19         2      NA       1745      NA
    ## 14025 2008     2         19         2      NA        815      NA
    ## 14325 2008     2         20         3      NA        920      NA
    ## 14326 2008     2         20         3      NA        755      NA
    ## 14623 2008     2         21         4      NA       1130      NA
    ## 14624 2008     2         21         4      NA       1705      NA
    ## 14625 2008     2         21         4      NA       1720      NA
    ## 14626 2008     2         21         4      NA       1900      NA
    ## 14627 2008     2         21         4      NA        905      NA
    ## 14628 2008     2         21         4      NA       1410      NA
    ## 14629 2008     2         21         4      NA       1905      NA
    ## 14630 2008     2         21         4      NA       1935      NA
    ## 14918 2008     2         22         5      NA        715      NA
    ## 14919 2008     2         22         5      NA       1120      NA
    ## 14920 2008     2         22         5      NA       1125      NA
    ## 14921 2008     2         22         5      NA        705      NA
    ## 14922 2008     2         22         5      NA        700      NA
    ## 14923 2008     2         22         5      NA       1730      NA
    ## 14924 2008     2         22         5      NA        630      NA
    ## 14925 2008     2         22         5      NA       1150      NA
    ## 14926 2008     2         22         5      NA       1900      NA
    ## 14927 2008     2         22         5      NA       1915      NA
    ## 14928 2008     2         22         5      NA        900      NA
    ## 14929 2008     2         22         5      NA       2155      NA
    ## 14930 2008     2         22         5      NA       1010      NA
    ## 14931 2008     2         22         5      NA        755      NA
    ## 14932 2008     2         22         5      NA       1335      NA
    ## 14933 2008     2         22         5      NA       1435      NA
    ## 14934 2008     2         22         5      NA        755      NA
    ## 15154 2008     2         23         6      NA        700      NA
    ## 15155 2008     2         23         6      NA        730      NA
    ## 15156 2008     2         23         6      NA        800      NA
    ## 15436 2008     2         24         7      NA       1950      NA
    ## 15728 2008     2         25         1      NA       1720      NA
    ## 15729 2008     2         25         1      NA       1055      NA
    ## 15730 2008     2         25         1      NA       1130      NA
    ## 15731 2008     2         25         1      NA       1710      NA
    ## 15732 2008     2         25         1      NA        605      NA
    ## 15733 2008     2         25         1      NA       1140      NA
    ## 15734 2008     2         25         1      NA       1610      NA
    ## 15735 2008     2         25         1      NA        950      NA
    ## 15736 2008     2         25         1      NA       1750      NA
    ## 15737 2008     2         25         1      NA       1500      NA
    ## 15738 2008     2         25         1      NA       1745      NA
    ## 16034 2008     2         26         2      NA       1155      NA
    ## 16035 2008     2         26         2      NA       1140      NA
    ## 16036 2008     2         26         2      NA       2017      NA
    ## 16037 2008     2         26         2      NA       1745      NA
    ## 16318 2008     2         27         3      NA       1652      NA
    ## 16596 2008     2         28         4      NA        835      NA
    ## 16597 2008     2         28         4      NA        920      NA
    ## 16598 2008     2         28         4      NA        850      NA
    ## 16599 2008     2         28         4      NA       1344      NA
    ## 16600 2008     2         28         4      NA        755      NA
    ## 16882 2008     2         29         5      NA       1950      NA
    ## 17105 2008     3          1         6      NA        605      NA
    ## 17680 2008     3          3         1      NA       1550      NA
    ## 17681 2008     3          3         1      NA       1415      NA
    ## 17682 2008     3          3         1      NA       1915      NA
    ## 17683 2008     3          3         1      NA       1705      NA
    ## 17684 2008     3          3         1      NA       1820      NA
    ## 17685 2008     3          3         1      NA       1510      NA
    ## 17686 2008     3          3         1      NA       1430      NA
    ## 17687 2008     3          3         1      NA       1735      NA
    ## 17688 2008     3          3         1      NA       1215      NA
    ## 17689 2008     3          3         1      NA       1845      NA
    ## 17690 2008     3          3         1      NA       1410      NA
    ## 17691 2008     3          3         1      NA       1335      NA
    ## 17977 2008     3          4         2      NA       1415      NA
    ## 17978 2008     3          4         2      NA       1110      NA
    ## 17979 2008     3          4         2      NA        945      NA
    ## 17980 2008     3          4         2      NA       1705      NA
    ## 17981 2008     3          4         2      NA       1140      NA
    ## 17982 2008     3          4         2      NA       1700      NA
    ## 17983 2008     3          4         2      NA       1300      NA
    ## 17984 2008     3          4         2      NA       2151      NA
    ## 17985 2008     3          4         2      NA        805      NA
    ## 17986 2008     3          4         2      NA        815      NA
    ## 17987 2008     3          4         2      NA       1410      NA
    ## 17988 2008     3          4         2      NA       1450      NA
    ## 18286 2008     3          5         3      NA       1200      NA
    ## 18287 2008     3          5         3      NA        835      NA
    ## 18288 2008     3          5         3      NA        620      NA
    ## 18289 2008     3          5         3      NA        910      NA
    ## 18567 2008     3          6         4      NA       1550      NA
    ## 18568 2008     3          6         4      NA       1730      NA
    ## 18569 2008     3          6         4      NA       1905      NA
    ## 18570 2008     3          6         4      NA       1325      NA
    ## 18571 2008     3          6         4      NA       1415      NA
    ## 18572 2008     3          6         4      NA       1700      NA
    ## 18573 2008     3          6         4      NA       1805      NA
    ## 18574 2008     3          6         4      NA       1525      NA
    ## 18575 2008     3          6         4      NA       1225      NA
    ## 18576 2008     3          6         4      NA       1915      NA
    ## 18577 2008     3          6         4      NA       1955      NA
    ## 18578 2008     3          6         4      NA       1610      NA
    ## 18579 2008     3          6         4      NA       1745      NA
    ## 18580 2008     3          6         4      NA       1430      NA
    ## 18581 2008     3          6         4      NA       2135      NA
    ## 18582 2008     3          6         4      NA       1735      NA
    ## 18583 2008     3          6         4      NA       1520      NA
    ## 18584 2008     3          6         4      NA       1805      NA
    ## 18585 2008     3          6         4      NA       1235      NA
    ## 18586 2008     3          6         4      NA       1845      NA
    ## 18587 2008     3          6         4      NA       2240      NA
    ## 18588 2008     3          6         4      NA       1935      NA
    ## 18589 2008     3          6         4      NA       1625      NA
    ## 18590 2008     3          6         4      NA       1040      NA
    ## 18591 2008     3          6         4      NA       1345      NA
    ## 18592 2008     3          6         4      NA       1130      NA
    ## 18593 2008     3          6         4      NA       2050      NA
    ## 18883 2008     3          7         5      NA       2050      NA
    ## 18884 2008     3          7         5      NA        835      NA
    ## 18885 2008     3          7         5      NA        945      NA
    ## 18886 2008     3          7         5      NA        705      NA
    ## 18887 2008     3          7         5      NA        600      NA
    ## 18888 2008     3          7         5      NA        740      NA
    ## 18889 2008     3          7         5      NA       1050      NA
    ## 18890 2008     3          7         5      NA        705      NA
    ## 18891 2008     3          7         5      NA       2045      NA
    ## 18892 2008     3          7         5      NA       2055      NA
    ## 18893 2008     3          7         5      NA       2151      NA
    ## 18894 2008     3          7         5      NA       1310      NA
    ## 18895 2008     3          7         5      NA       1930      NA
    ## 18896 2008     3          7         5      NA       1950      NA
    ## 18897 2008     3          7         5      NA        805      NA
    ## 19118 2008     3          8         6      NA        735      NA
    ## 19119 2008     3          8         6      NA        800      NA
    ## 19120 2008     3          8         6      NA       1200      NA
    ## 19121 2008     3          8         6      NA        605      NA
    ## 19122 2008     3          8         6      NA       1730      NA
    ## 19123 2008     3          8         6      NA       1930      NA
    ## 19406 2008     3          9         7      NA        650      NA
    ## 19407 2008     3          9         7      NA       1110      NA
    ## 19408 2008     3          9         7      NA       1930      NA
    ## 19409 2008     3          9         7      NA       2151      NA
    ## 19705 2008     3         10         1      NA       1355      NA
    ## 19706 2008     3         10         1      NA        710      NA
    ## 19707 2008     3         10         1      NA        620      NA
    ## 19708 2008     3         10         1      NA       1550      NA
    ## 19709 2008     3         10         1      NA       2050      NA
    ## 19710 2008     3         10         1      NA       1245      NA
    ## 19711 2008     3         10         1      NA       1645      NA
    ## 19712 2008     3         10         1      NA       1430      NA
    ## 19713 2008     3         10         1      NA       1930      NA
    ## 19999 2008     3         11         2      NA        630      NA
    ## 20000 2008     3         11         2      NA        745      NA
    ## 20001 2008     3         11         2      NA        920      NA
    ## 20002 2008     3         11         2      NA        705      NA
    ## 20003 2008     3         11         2      NA       1550      NA
    ## 20004 2008     3         11         2      NA       1905      NA
    ## 20005 2008     3         11         2      NA       1015      NA
    ## 20006 2008     3         11         2      NA        825      NA
    ## 20007 2008     3         11         2      NA       1010      NA
    ## 20008 2008     3         11         2      NA        545      NA
    ## 20009 2008     3         11         2      NA       1745      NA
    ## 20010 2008     3         11         2      NA       1430      NA
    ## 20311 2008     3         12         3      NA        810      NA
    ## 20611 2008     3         13         4      NA        705      NA
    ## 20612 2008     3         13         4      NA        640      NA
    ## 20613 2008     3         13         4      NA       1705      NA
    ## 20614 2008     3         13         4      NA        545      NA
    ## 20615 2008     3         13         4      NA        905      NA
    ## 20918 2008     3         14         5      NA       1125      NA
    ## 20919 2008     3         14         5      NA       1010      NA
    ## 21147 2008     3         15         6      NA       1735      NA
    ## 21432 2008     3         16         7      NA       1145      NA
    ## 21433 2008     3         16         7      NA       1645      NA
    ## 21434 2008     3         16         7      NA       1950      NA
    ## 21736 2008     3         17         1      NA       1905      NA
    ## 21737 2008     3         17         1      NA        605      NA
    ## 21738 2008     3         17         1      NA       1415      NA
    ## 21739 2008     3         17         1      NA       1745      NA
    ## 21740 2008     3         17         1      NA       1235      NA
    ## 22000 2008     3         18         2      NA       1145      NA
    ## 22001 2008     3         18         2      NA       1435      NA
    ## 22002 2008     3         18         2      NA       1700      NA
    ## 22003 2008     3         18         2      NA       1550      NA
    ## 22004 2008     3         18         2      NA       1730      NA
    ## 22005 2008     3         18         2      NA       2050      NA
    ## 22006 2008     3         18         2      NA       1125      NA
    ## 22007 2008     3         18         2      NA       1405      NA
    ## 22008 2008     3         18         2      NA       1325      NA
    ## 22009 2008     3         18         2      NA       1415      NA
    ## 22010 2008     3         18         2      NA       1700      NA
    ## 22011 2008     3         18         2      NA       1805      NA
    ## 22012 2008     3         18         2      NA       1525      NA
    ## 22013 2008     3         18         2      NA       1225      NA
    ## 22014 2008     3         18         2      NA       1915      NA
    ## 22015 2008     3         18         2      NA       1955      NA
    ## 22016 2008     3         18         2      NA        915      NA
    ## 22017 2008     3         18         2      NA       1920      NA
    ## 22018 2008     3         18         2      NA       1615      NA
    ## 22019 2008     3         18         2      NA       1405      NA
    ## 22020 2008     3         18         2      NA       1245      NA
    ## 22021 2008     3         18         2      NA       1145      NA
    ## 22022 2008     3         18         2      NA       2017      NA
    ## 22023 2008     3         18         2      NA       1610      NA
    ## 22024 2008     3         18         2      NA       1245      NA
    ## 22025 2008     3         18         2      NA       1430      NA
    ## 22026 2008     3         18         2      NA       1930      NA
    ## 22027 2008     3         18         2      NA       1950      NA
    ## 22028 2008     3         18         2      NA       2135      NA
    ## 22029 2008     3         18         2      NA       1735      NA
    ## 22030 2008     3         18         2      NA       1520      NA
    ## 22031 2008     3         18         2      NA       1805      NA
    ## 22032 2008     3         18         2      NA       1235      NA
    ## 22033 2008     3         18         2      NA       1845      NA
    ## 22034 2008     3         18         2      NA       2240      NA
    ## 22035 2008     3         18         2      NA       1935      NA
    ## 22036 2008     3         18         2      NA       1625      NA
    ## 22037 2008     3         18         2      NA       1345      NA
    ## 22038 2008     3         18         2      NA       1130      NA
    ## 22039 2008     3         18         2      NA       2050      NA
    ## 22040 2008     3         18         2      NA       1915      NA
    ## 22041 2008     3         18         2      NA       1430      NA
    ## 22042 2008     3         18         2      NA       1738      NA
    ## 22043 2008     3         18         2      NA        725      NA
    ## 22338 2008     3         19         3      NA       1110      NA
    ## 22339 2008     3         19         3      NA        705      NA
    ## 22340 2008     3         19         3      NA        605      NA
    ## 22341 2008     3         19         3      NA       1610      NA
    ## 22342 2008     3         19         3      NA        835      NA
    ## 22343 2008     3         19         3      NA        645      NA
    ## 22344 2008     3         19         3      NA       2151      NA
    ## 22345 2008     3         19         3      NA        545      NA
    ## 22346 2008     3         19         3      NA       1335      NA
    ## 22652 2008     3         20         4      NA        620      NA
    ## 22952 2008     3         21         5      NA        550      NA
    ## 22953 2008     3         21         5      NA        620      NA
    ## 22954 2008     3         21         5      NA        621      NA
    ## 22955 2008     3         21         5      NA        955      NA
    ## 22956 2008     3         21         5      NA       2045      NA
    ## 22957 2008     3         21         5      NA       1315      NA
    ## 22958 2008     3         21         5      NA       2017      NA
    ## 22959 2008     3         21         5      NA       1230      NA
    ## 23472 2008     3         23         7      NA        920      NA
    ## 23473 2008     3         23         7      NA        755      NA
    ## 24077 2008     3         25         2      NA       1145      NA
    ## 24078 2008     3         25         2      NA       2017      NA
    ## 24367 2008     3         26         3      NA        640      NA
    ## 24368 2008     3         26         3      NA       1550      NA
    ## 24369 2008     3         26         3      NA        945      NA
    ## 24370 2008     3         26         3      NA       1915      NA
    ## 24371 2008     3         26         3      NA       1705      NA
    ## 24372 2008     3         26         3      NA        740      NA
    ## 24373 2008     3         26         3      NA       1050      NA
    ## 24374 2008     3         26         3      NA       1735      NA
    ## 24375 2008     3         26         3      NA       1430      NA
    ## 24376 2008     3         26         3      NA        925      NA
    ## 24377 2008     3         26         3      NA       1520      NA
    ## 24378 2008     3         26         3      NA       1410      NA
    ## 24379 2008     3         26         3      NA        805      NA
    ## 24380 2008     3         26         3      NA       1845      NA
    ## 24381 2008     3         26         3      NA       1410      NA
    ## 24682 2008     3         27         4      NA       1540      NA
    ## 24683 2008     3         27         4      NA       1415      NA
    ## 24684 2008     3         27         4      NA       1705      NA
    ## 24685 2008     3         27         4      NA       1335      NA
    ## 24686 2008     3         27         4      NA       1235      NA
    ## 24687 2008     3         27         4      NA       1410      NA
    ## 24992 2008     3         28         5      NA       2151      NA
    ## 24993 2008     3         28         5      NA       1710      NA
    ## 25218 2008     3         29         6      NA       1610      NA
    ## 25219 2008     3         29         6      NA       1335      NA
    ## 25792 2008     3         31         1      NA       2045      NA
    ## 25793 2008     3         31         1      NA       1730      NA
    ## 25794 2008     3         31         1      NA       1405      NA
    ## 25795 2008     3         31         1      NA       1600      NA
    ## 25796 2008     3         31         1      NA        955      NA
    ## 25797 2008     3         31         1      NA       1955      NA
    ## 25798 2008     3         31         1      NA       1935      NA
    ## 25799 2008     3         31         1      NA       1610      NA
    ## 25800 2008     3         31         1      NA       1245      NA
    ## 25801 2008     3         31         1      NA       1805      NA
    ## 25802 2008     3         31         1      NA        815      NA
    ## 25803 2008     3         31         1      NA       1745      NA
    ## 26088 2008     4          1         2      NA        835      NA
    ## 26667 2008     4          3         4      NA       1110      NA
    ## 26668 2008     4          3         4      NA        925      NA
    ## 26959 2008     4          4         5      NA        835      NA
    ## 26960 2008     4          4         5      NA       1610      NA
    ## 27448 2008     4          6         7      NA       2151      NA
    ## 27732 2008     4          7         1      NA        620      NA
    ## 27733 2008     4          7         1      NA       1630      NA
    ## 27734 2008     4          7         1      NA       1415      NA
    ## 27735 2008     4          7         1      NA       2000      NA
    ## 27736 2008     4          7         1      NA       1400      NA
    ## 27737 2008     4          7         1      NA       1810      NA
    ## 27738 2008     4          7         1      NA       1240      NA
    ## 28004 2008     4          8         2      NA       1840      NA
    ## 28005 2008     4          8         2      NA       1705      NA
    ## 28006 2008     4          8         2      NA       1530      NA
    ## 28007 2008     4          8         2      NA       1530      NA
    ## 28008 2008     4          8         2      NA       1635      NA
    ## 28009 2008     4          8         2      NA       1745      NA
    ## 28010 2008     4          8         2      NA       1810      NA
    ## 28011 2008     4          8         2      NA       2000      NA
    ## 28012 2008     4          8         2      NA       2000      NA
    ## 28013 2008     4          8         2      NA       1930      NA
    ## 28014 2008     4          8         2      NA       1655      NA
    ## 28015 2008     4          8         2      NA       1810      NA
    ## 28016 2008     4          8         2      NA       1400      NA
    ## 28017 2008     4          8         2      NA       2205      NA
    ## 28018 2008     4          8         2      NA       1855      NA
    ## 28019 2008     4          8         2      NA       2300      NA
    ## 28020 2008     4          8         2      NA       2020      NA
    ## 28021 2008     4          8         2      NA       1540      NA
    ## 28022 2008     4          8         2      NA       1610      NA
    ## 28023 2008     4          8         2      NA       1850      NA
    ## 28024 2008     4          8         2      NA       1900      NA
    ## 28025 2008     4          8         2      NA       1715      NA
    ## 28026 2008     4          8         2      NA       1500      NA
    ## 28027 2008     4          8         2      NA       2135      NA
    ## 28275 2008     4          9         3      NA        850      NA
    ## 28276 2008     4          9         3      NA       1415      NA
    ## 28277 2008     4          9         3      NA       1840      NA
    ## 28278 2008     4          9         3      NA        710      NA
    ## 28279 2008     4          9         3      NA       1705      NA
    ## 28280 2008     4          9         3      NA        635      NA
    ## 28281 2008     4          9         3      NA       1530      NA
    ## 28282 2008     4          9         3      NA        945      NA
    ## 28283 2008     4          9         3      NA        600      NA
    ## 28284 2008     4          9         3      NA       1040      NA
    ## 28285 2008     4          9         3      NA       1425      NA
    ## 28286 2008     4          9         3      NA        615      NA
    ## 28287 2008     4          9         3      NA       1000      NA
    ## 28288 2008     4          9         3      NA        740      NA
    ## 28289 2008     4          9         3      NA       1225      NA
    ## 28290 2008     4          9         3      NA       1120      NA
    ## 28291 2008     4          9         3      NA        815      NA
    ## 28292 2008     4          9         3      NA        600      NA
    ## 28293 2008     4          9         3      NA       1635      NA
    ## 28294 2008     4          9         3      NA       1745      NA
    ## 28295 2008     4          9         3      NA       1210      NA
    ## 28296 2008     4          9         3      NA       1810      NA
    ## 28297 2008     4          9         3      NA        825      NA
    ## 28298 2008     4          9         3      NA       1305      NA
    ## 28299 2008     4          9         3      NA       2000      NA
    ## 28300 2008     4          9         3      NA       1345      NA
    ## 28301 2008     4          9         3      NA       1930      NA
    ## 28302 2008     4          9         3      NA       1655      NA
    ## 28303 2008     4          9         3      NA       1810      NA
    ## 28304 2008     4          9         3      NA       1240      NA
    ## 28305 2008     4          9         3      NA       1400      NA
    ## 28306 2008     4          9         3      NA       1205      NA
    ## 28307 2008     4          9         3      NA       1855      NA
    ## 28308 2008     4          9         3      NA        820      NA
    ## 28309 2008     4          9         3      NA        640      NA
    ## 28310 2008     4          9         3      NA        800      NA
    ## 28311 2008     4          9         3      NA       1300      NA
    ## 28312 2008     4          9         3      NA       1035      NA
    ## 28313 2008     4          9         3      NA       1130      NA
    ## 28314 2008     4          9         3      NA       2020      NA
    ## 28315 2008     4          9         3      NA       1540      NA
    ## 28316 2008     4          9         3      NA       1610      NA
    ## 28317 2008     4          9         3      NA        815      NA
    ## 28318 2008     4          9         3      NA       1850      NA
    ## 28319 2008     4          9         3      NA       1715      NA
    ## 28320 2008     4          9         3      NA       1500      NA
    ## 28321 2008     4          9         3      NA       2135      NA
    ## 28585 2008     4         10         4      NA        610      NA
    ## 28586 2008     4         10         4      NA        850      NA
    ## 28587 2008     4         10         4      NA       1415      NA
    ## 28588 2008     4         10         4      NA        710      NA
    ## 28589 2008     4         10         4      NA       1705      NA
    ## 28590 2008     4         10         4      NA        945      NA
    ## 28591 2008     4         10         4      NA        600      NA
    ## 28592 2008     4         10         4      NA       1040      NA
    ## 28593 2008     4         10         4      NA        615      NA
    ## 28594 2008     4         10         4      NA       1000      NA
    ## 28595 2008     4         10         4      NA       1530      NA
    ## 28596 2008     4         10         4      NA        740      NA
    ## 28597 2008     4         10         4      NA       1120      NA
    ## 28598 2008     4         10         4      NA        600      NA
    ## 28599 2008     4         10         4      NA       1635      NA
    ## 28600 2008     4         10         4      NA       1745      NA
    ## 28601 2008     4         10         4      NA       1210      NA
    ## 28602 2008     4         10         4      NA       1305      NA
    ## 28603 2008     4         10         4      NA       2000      NA
    ## 28604 2008     4         10         4      NA       2000      NA
    ## 28605 2008     4         10         4      NA       1345      NA
    ## 28606 2008     4         10         4      NA       1810      NA
    ## 28607 2008     4         10         4      NA        820      NA
    ## 28608 2008     4         10         4      NA        640      NA
    ## 28609 2008     4         10         4      NA        800      NA
    ## 28610 2008     4         10         4      NA       2300      NA
    ## 28611 2008     4         10         4      NA       1300      NA
    ## 28612 2008     4         10         4      NA       1035      NA
    ## 28613 2008     4         10         4      NA       1130      NA
    ## 28614 2008     4         10         4      NA       1610      NA
    ## 28615 2008     4         10         4      NA       1850      NA
    ## 28616 2008     4         10         4      NA       1500      NA
    ## 28617 2008     4         10         4      NA       2135      NA
    ## 28898 2008     4         11         5      NA        850      NA
    ## 28899 2008     4         11         5      NA       1705      NA
    ## 28900 2008     4         11         5      NA        600      NA
    ## 28901 2008     4         11         5      NA       1000      NA
    ## 28902 2008     4         11         5      NA       1530      NA
    ## 28903 2008     4         11         5      NA       1120      NA
    ## 28904 2008     4         11         5      NA        600      NA
    ## 28905 2008     4         11         5      NA       1745      NA
    ## 28906 2008     4         11         5      NA       1305      NA
    ## 28907 2008     4         11         5      NA       1345      NA
    ## 28908 2008     4         11         5      NA       1400      NA
    ## 28909 2008     4         11         5      NA       2205      NA
    ## 28910 2008     4         11         5      NA        820      NA
    ## 28911 2008     4         11         5      NA        800      NA
    ## 28912 2008     4         11         5      NA       1130      NA
    ## 28913 2008     4         11         5      NA       1610      NA
    ## 28914 2008     4         11         5      NA       2135      NA
    ## 29131 2008     4         12         6      NA        600      NA
    ## 29132 2008     4         12         6      NA       1040      NA
    ## 29133 2008     4         12         6      NA       1000      NA
    ## 29134 2008     4         12         6      NA        600      NA
    ## 29135 2008     4         12         6      NA       1400      NA
    ## 29136 2008     4         12         6      NA       1345      NA
    ## 29137 2008     4         12         6      NA        820      NA
    ## 29138 2008     4         12         6      NA       1300      NA
    ## 29139 2008     4         12         6      NA       1130      NA
    ## 29417 2008     4         13         7      NA       1125      NA
    ## 29418 2008     4         13         7      NA       1605      NA
    ## 29419 2008     4         13         7      NA       1915      NA
    ## 29420 2008     4         13         7      NA       2300      NA
    ## 29421 2008     4         13         7      NA       1430      NA
    ## 29715 2008     4         14         1      NA        550      NA
    ## 30002 2008     4         15         2      NA        920      NA
    ## 30003 2008     4         15         2      NA        755      NA
    ## 30297 2008     4         16         3      NA       1330      NA
    ## 30589 2008     4         17         4      NA       2050      NA
    ## 30590 2008     4         17         4      NA       2000      NA
    ## 30591 2008     4         17         4      NA       1930      NA
    ## 30592 2008     4         17         4      NA       2205      NA
    ## 30593 2008     4         17         4      NA       2020      NA
    ## 30890 2008     4         18         5      NA        600      NA
    ## 31113 2008     4         19         6      NA       1210      NA
    ## 31114 2008     4         19         6      NA       1035      NA
    ## 32270 2008     4         23         3      NA        705      NA
    ## 32271 2008     4         23         3      NA       2000      NA
    ## 32272 2008     4         23         3      NA        545      NA
    ## 32273 2008     4         23         3      NA       2205      NA
    ## 32566 2008     4         24         4      NA        725      NA
    ## 32567 2008     4         24         4      NA       1550      NA
    ## 32568 2008     4         24         4      NA       1905      NA
    ## 32569 2008     4         24         4      NA       1139      NA
    ## 32570 2008     4         24         4      NA       1745      NA
    ## 32571 2008     4         24         4      NA       1430      NA
    ## 32863 2008     4         25         5      NA       1730      NA
    ## 32864 2008     4         25         5      NA       1905      NA
    ## 32865 2008     4         25         5      NA       1705      NA
    ## 32866 2008     4         25         5      NA       1530      NA
    ## 32867 2008     4         25         5      NA       2050      NA
    ## 32868 2008     4         25         5      NA       1610      NA
    ## 32869 2008     4         25         5      NA       1745      NA
    ## 32870 2008     4         25         5      NA       2135      NA
    ## 33095 2008     4         26         6      NA        600      NA
    ## 33096 2008     4         26         6      NA       1215      NA
    ## 33674 2008     4         28         1      NA       1150      NA
    ## 33675 2008     4         28         1      NA        830      NA
    ## 34259 2008     4         30         3      NA       1649      NA
    ## 34260 2008     4         30         3      NA       1200      NA
    ## 34261 2008     4         30         3      NA        925      NA
    ## 34864 2008     5          2         5      NA       1730      NA
    ## 34865 2008     5          2         5      NA        945      NA
    ## 34866 2008     5          2         5      NA       1120      NA
    ## 34867 2008     5          2         5      NA       1610      NA
    ## 34868 2008     5          2         5      NA       1345      NA
    ## 35097 2008     5          3         6      NA        820      NA
    ## 35386 2008     5          4         7      NA        600      NA
    ## 35387 2008     5          4         7      NA       1345      NA
    ## 35686 2008     5          5         1      NA       1855      NA
    ## 35687 2008     5          5         1      NA       1125      NA
    ## 35688 2008     5          5         1      NA       1810      NA
    ## 35689 2008     5          5         1      NA       1010      NA
    ## 35690 2008     5          5         1      NA       1205      NA
    ## 35691 2008     5          5         1      NA       1745      NA
    ## 35980 2008     5          6         2      NA        945      NA
    ## 35981 2008     5          6         2      NA       1550      NA
    ## 35982 2008     5          6         2      NA       1125      NA
    ## 35983 2008     5          6         2      NA       1635      NA
    ## 35984 2008     5          6         2      NA        655      NA
    ## 35985 2008     5          6         2      NA        825      NA
    ## 35986 2008     5          6         2      NA       1010      NA
    ## 35987 2008     5          6         2      NA       1430      NA
    ## 35988 2008     5          6         2      NA       1500      NA
    ## 36287 2008     5          7         3      NA       1805      NA
    ## 36288 2008     5          7         3      NA       1415      NA
    ## 36289 2008     5          7         3      NA       1105      NA
    ## 36290 2008     5          7         3      NA       2153      NA
    ## 36291 2008     5          7         3      NA        930      NA
    ## 36292 2008     5          7         3      NA       1240      NA
    ## 36596 2008     5          8         4      NA        805      NA
    ## 36597 2008     5          8         4      NA       1550      NA
    ## 36598 2008     5          8         4      NA       1430      NA
    ## 36904 2008     5          9         5      NA        635      NA
    ## 36905 2008     5          9         5      NA       1645      NA
    ## 37148 2008     5         10         6      NA       1540      NA
    ## 37149 2008     5         10         6      NA       1130      NA
    ## 37436 2008     5         11         7      NA        700      NA
    ## 37437 2008     5         11         7      NA        945      NA
    ## 37438 2008     5         11         7      NA       1425      NA
    ## 37439 2008     5         11         7      NA        815      NA
    ## 37440 2008     5         11         7      NA       2135      NA
    ## 38033 2008     5         13         2      NA       1000      NA
    ## 38034 2008     5         13         2      NA       1145      NA
    ## 38035 2008     5         13         2      NA        805      NA
    ## 38036 2008     5         13         2      NA       2020      NA
    ## 38335 2008     5         14         3      NA       1135      NA
    ## 38336 2008     5         14         3      NA        705      NA
    ## 38337 2008     5         14         3      NA       1125      NA
    ## 38338 2008     5         14         3      NA       1105      NA
    ## 38339 2008     5         14         3      NA       1320      NA
    ## 38340 2008     5         14         3      NA       1010      NA
    ## 38341 2008     5         14         3      NA        545      NA
    ## 38342 2008     5         14         3      NA        930      NA
    ## 38650 2008     5         15         4      NA        720      NA
    ## 38958 2008     5         16         5      NA        700      NA
    ## 38959 2008     5         16         5      NA       1915      NA
    ## 39204 2008     5         17         6      NA       1530      NA
    ## 39495 2008     5         18         7      NA       1730      NA
    ## 39496 2008     5         18         7      NA       1610      NA
    ## 40407 2008     5         21         3      NA       2045      NA
    ## 40408 2008     5         21         3      NA        615      NA
    ## 40716 2008     5         22         4      NA        550      NA
    ## 41526 2008     5         25         7      NA       1630      NA
    ## 41527 2008     5         25         7      NA       1425      NA
    ## 41528 2008     5         25         7      NA       1400      NA
    ## 41529 2008     5         25         7      NA        815      NA
    ## 41806 2008     5         26         1      NA        605      NA
    ## 41807 2008     5         26         1      NA       1405      NA
    ## 41808 2008     5         26         1      NA       1245      NA
    ## 42086 2008     5         27         2      NA       1720      NA
    ## 42087 2008     5         27         2      NA       1550      NA
    ## 42088 2008     5         27         2      NA       1730      NA
    ## 42089 2008     5         27         2      NA       1905      NA
    ## 42090 2008     5         27         2      NA       1125      NA
    ## 42091 2008     5         27         2      NA       1405      NA
    ## 42092 2008     5         27         2      NA       1415      NA
    ## 42093 2008     5         27         2      NA       1530      NA
    ## 42094 2008     5         27         2      NA       1635      NA
    ## 42095 2008     5         27         2      NA       1305      NA
    ## 42096 2008     5         27         2      NA       1920      NA
    ## 42097 2008     5         27         2      NA       2000      NA
    ## 42098 2008     5         27         2      NA       1205      NA
    ## 42099 2008     5         27         2      NA       1216      NA
    ## 42100 2008     5         27         2      NA       1850      NA
    ## 42101 2008     5         27         2      NA       1010      NA
    ## 42102 2008     5         27         2      NA       1610      NA
    ## 42103 2008     5         27         2      NA       1245      NA
    ## 42104 2008     5         27         2      NA       1745      NA
    ## 42105 2008     5         27         2      NA       1430      NA
    ## 42106 2008     5         27         2      NA       1655      NA
    ## 42107 2008     5         27         2      NA       1240      NA
    ## 42108 2008     5         27         2      NA       1400      NA
    ## 42109 2008     5         27         2      NA       1745      NA
    ## 42110 2008     5         27         2      NA       2300      NA
    ## 42111 2008     5         27         2      NA       1610      NA
    ## 42112 2008     5         27         2      NA       1850      NA
    ## 42113 2008     5         27         2      NA       1500      NA
    ## 42114 2008     5         27         2      NA       1945      NA
    ## 42413 2008     5         28         3      NA        805      NA
    ## 42414 2008     5         28         3      NA       1905      NA
    ## 42415 2008     5         28         3      NA        635      NA
    ## 42416 2008     5         28         3      NA       1425      NA
    ## 42417 2008     5         28         3      NA        615      NA
    ## 42418 2008     5         28         3      NA        615      NA
    ## 42419 2008     5         28         3      NA       1745      NA
    ## 42420 2008     5         28         3      NA       1655      NA
    ## 42726 2008     5         29         4      NA       1040      NA
    ## 42727 2008     5         29         4      NA       1205      NA
    ## 42728 2008     5         29         4      NA       1300      NA
    ## 43034 2008     5         30         5      NA        607      NA
    ## 43035 2008     5         30         5      NA        635      NA
    ## 43036 2008     5         30         5      NA       1120      NA
    ## 43037 2008     5         30         5      NA       1715      NA
    ## 43281 2008     5         31         6      NA       1920      NA
    ## 43282 2008     5         31         6      NA       1745      NA
    ## 43569 2008     6          1         7      NA       1550      NA
    ## 43570 2008     6          1         7      NA       1125      NA
    ## 43571 2008     6          1         7      NA       1920      NA
    ## 43572 2008     6          1         7      NA       1010      NA
    ## 43573 2008     6          1         7      NA       1435      NA
    ## 43574 2008     6          1         7      NA       1745      NA
    ## 43879 2008     6          2         1      NA       1730      NA
    ## 43880 2008     6          2         1      NA       1610      NA
    ## 44179 2008     6          3         2      NA       2000      NA
    ## 44180 2008     6          3         2      NA       1810      NA
    ## 44485 2008     6          4         3      NA        700      NA
    ## 44797 2008     6          5         4      NA       2300      NA
    ## 44798 2008     6          5         4      NA       1001      NA
    ## 45113 2008     6          6         5      NA        700      NA
    ## 45114 2008     6          6         5      NA       1720      NA
    ## 45363 2008     6          7         6      NA        729      NA
    ## 45364 2008     6          7         6      NA        840      NA
    ## 45365 2008     6          7         6      NA       1415      NA
    ## 45366 2008     6          7         6      NA       1240      NA
    ## 45664 2008     6          8         7      NA       1530      NA
    ## 45665 2008     6          8         7      NA       1715      NA
    ## 45970 2008     6          9         1      NA       1625      NA
    ## 45971 2008     6          9         1      NA       1550      NA
    ## 45972 2008     6          9         1      NA       1125      NA
    ## 45973 2008     6          9         1      NA       1920      NA
    ## 45974 2008     6          9         1      NA       2000      NA
    ## 45975 2008     6          9         1      NA       1400      NA
    ## 45976 2008     6          9         1      NA       1010      NA
    ## 45977 2008     6          9         1      NA       1435      NA
    ## 45978 2008     6          9         1      NA       1810      NA
    ## 45979 2008     6          9         1      NA       1745      NA
    ## 46289 2008     6         10         2      NA        920      NA
    ## 46290 2008     6         10         2      NA       1425      NA
    ## 46291 2008     6         10         2      NA       1855      NA
    ## 46292 2008     6         10         2      NA        750      NA
    ## 46293 2008     6         10         2      NA       1655      NA
    ## 46603 2008     6         11         3      NA        655      NA
    ## 46604 2008     6         11         3      NA        920      NA
    ## 46605 2008     6         11         3      NA       1550      NA
    ## 46606 2008     6         11         3      NA       1435      NA
    ## 46607 2008     6         11         3      NA        750      NA
    ## 46921 2008     6         12         4      NA        600      NA
    ## 46922 2008     6         12         4      NA        920      NA
    ## 46923 2008     6         12         4      NA        750      NA
    ## 47239 2008     6         13         5      NA       1425      NA
    ## 47240 2008     6         13         5      NA        815      NA
    ## 47797 2008     6         15         7      NA       2000      NA
    ## 48108 2008     6         16         1      NA       2010      NA
    ## 48109 2008     6         16         1      NA        700      NA
    ## 48110 2008     6         16         1      NA       1645      NA
    ## 48111 2008     6         16         1      NA       2000      NA
    ## 48414 2008     6         17         2      NA       1905      NA
    ## 48415 2008     6         17         2      NA       1405      NA
    ## 48416 2008     6         17         2      NA       1415      NA
    ## 48417 2008     6         17         2      NA       1530      NA
    ## 48418 2008     6         17         2      NA       1640      NA
    ## 48419 2008     6         17         2      NA       1745      NA
    ## 48420 2008     6         17         2      NA       1245      NA
    ## 48421 2008     6         17         2      NA       1745      NA
    ## 48422 2008     6         17         2      NA       1240      NA
    ## 48423 2008     6         17         2      NA       1355      NA
    ## 48424 2008     6         17         2      NA       1610      NA
    ## 48425 2008     6         17         2      NA       1505      NA
    ## 48738 2008     6         18         3      NA       1125      NA
    ## 48739 2008     6         18         3      NA       1010      NA
    ## 49047 2008     6         19         4      NA       1550      NA
    ## 49048 2008     6         19         4      NA       2050      NA
    ## 49049 2008     6         19         4      NA       1000      NA
    ## 49050 2008     6         19         4      NA       1210      NA
    ## 49051 2008     6         19         4      NA       1435      NA
    ## 49052 2008     6         19         4      NA       1930      NA
    ## 49053 2008     6         19         4      NA       1355      NA
    ## 49054 2008     6         19         4      NA       1130      NA
    ## 49368 2008     6         20         5      NA        900      NA
    ## 49369 2008     6         20         5      NA       1135      NA
    ## 49370 2008     6         20         5      NA        705      NA
    ## 49371 2008     6         20         5      NA        847      NA
    ## 49922 2008     6         22         7      NA       1135      NA
    ## 49923 2008     6         22         7      NA        635      NA
    ## 49924 2008     6         22         7      NA       1305      NA
    ## 49925 2008     6         22         7      NA       1305      NA
    ## 49926 2008     6         22         7      NA        847      NA
    ## 49927 2008     6         22         7      NA       1130      NA
    ## 49928 2008     6         22         7      NA        815      NA
    ## 49929 2008     6         22         7      NA       2000      NA
    ## 50556 2008     6         24         2      NA       1905      NA
    ## 50557 2008     6         24         2      NA       1745      NA
    ## 51176 2008     6         26         4      NA       1625      NA
    ## 51177 2008     6         26         4      NA       1643      NA
    ## 51178 2008     6         26         4      NA       1550      NA
    ## 51179 2008     6         26         4      NA       1400      NA
    ## 51180 2008     6         26         4      NA       1620      NA
    ## 51181 2008     6         26         4      NA       1400      NA
    ## 51182 2008     6         26         4      NA       1117      NA
    ## 51183 2008     6         26         4      NA       1435      NA
    ## 51184 2008     6         26         4      NA       1140      NA
    ## 51185 2008     6         26         4      NA       1440      NA
    ## 51501 2008     6         27         5      NA       1030      NA
    ## 51502 2008     6         27         5      NA        844      NA
    ## 51756 2008     6         28         6      NA       1745      NA
    ## 51757 2008     6         28         6      NA       1935      NA
    ## 51758 2008     6         28         6      NA       2205      NA
    ## 51759 2008     6         28         6      NA       2300      NA
    ## 51760 2008     6         28         6      NA       2020      NA
    ## 51761 2008     6         28         6      NA       1850      NA
    ## 52055 2008     6         29         7      NA        850      NA
    ## 52056 2008     6         29         7      NA        600      NA
    ## 52057 2008     6         29         7      NA        615      NA
    ## 52058 2008     6         29         7      NA        740      NA
    ## 52371 2008     6         30         1      NA       1120      NA
    ## 52372 2008     6         30         1      NA        800      NA
    ## 52680 2008     7          1         2      NA       1120      NA
    ## 52681 2008     7          1         2      NA       1810      NA
    ## 52682 2008     7          1         2      NA       1855      NA
    ## 52683 2008     7          1         2      NA       1205      NA
    ## 52684 2008     7          1         2      NA        800      NA
    ## 52981 2008     7          2         3      NA        655      NA
    ## 53738 2008     7          5         6      NA       1530      NA
    ## 54024 2008     7          6         7      NA       1915      NA
    ## 54025 2008     7          6         7      NA        800      NA
    ## 54621 2008     7          8         2      NA       1122      NA
    ## 54622 2008     7          8         2      NA        945      NA
    ## 54623 2008     7          8         2      NA       1855      NA
    ## 54624 2008     7          8         2      NA       2020      NA
    ## 54906 2008     7          9         3      NA        615      NA
    ## 54907 2008     7          9         3      NA       1440      NA
    ## 54908 2008     7          9         3      NA        835      NA
    ## 55207 2008     7         10         4      NA        600      NA
    ## 55208 2008     7         10         4      NA       1705      NA
    ## 55209 2008     7         10         4      NA       1935      NA
    ## 55210 2008     7         10         4      NA       2155      NA
    ## 55511 2008     7         11         5      NA        720      NA
    ## 55512 2008     7         11         5      NA       1415      NA
    ## 55513 2008     7         11         5      NA       1240      NA
    ## 55769 2008     7         12         6      NA       1651      NA
    ## 56049 2008     7         13         7      NA        635      NA
    ## 56050 2008     7         13         7      NA       1548      NA
    ## 56051 2008     7         13         7      NA       1729      NA
    ## 56052 2008     7         13         7      NA        815      NA
    ## 56053 2008     7         13         7      NA       1916      NA
    ## 56054 2008     7         13         7      NA       1350      NA
    ## 56353 2008     7         14         1      NA        820      NA
    ## 56354 2008     7         14         1      NA        530      NA
    ## 57559 2008     7         18         5      NA       2045      NA
    ## 57560 2008     7         18         5      NA       2014      NA
    ## 57813 2008     7         19         6      NA        715      NA
    ## 57814 2008     7         19         6      NA       1040      NA
    ## 57815 2008     7         19         6      NA       1255      NA
    ## 58095 2008     7         20         7      NA       1105      NA
    ## 58096 2008     7         20         7      NA       2014      NA
    ## 58097 2008     7         20         7      NA        925      NA
    ## 58391 2008     7         21         1      NA        803      NA
    ## 58392 2008     7         21         1      NA       1030      NA
    ## 58393 2008     7         21         1      NA        844      NA
    ## 58685 2008     7         22         2      NA       1045      NA
    ## 58686 2008     7         22         2      NA        815      NA
    ## 58687 2008     7         22         2      NA        923      NA
    ## 58688 2008     7         22         2      NA        844      NA
    ## 58689 2008     7         22         2      NA        730      NA
    ## 58979 2008     7         23         3      NA       1320      NA
    ## 58980 2008     7         23         3      NA       1645      NA
    ## 58981 2008     7         23         3      NA       1440      NA
    ## 58982 2008     7         23         3      NA       1729      NA
    ## 58983 2008     7         23         3      NA       2014      NA
    ## 58984 2008     7         23         3      NA       1310      NA
    ## 58985 2008     7         23         3      NA       2000      NA
    ## 59276 2008     7         24         4      NA       1320      NA
    ## 59277 2008     7         24         4      NA        717      NA
    ## 59278 2008     7         24         4      NA        700      NA
    ## 59279 2008     7         24         4      NA       1755      NA
    ## 59280 2008     7         24         4      NA       1125      NA
    ## 59281 2008     7         24         4      NA       1440      NA
    ## 59282 2008     7         24         4      NA       1000      NA
    ## 59283 2008     7         24         4      NA        729      NA
    ## 59579 2008     7         25         5      NA       1320      NA
    ## 59580 2008     7         25         5      NA        923      NA
    ## 59581 2008     7         25         5      NA       1440      NA
    ## 59582 2008     7         25         5      NA        730      NA
    ## 59832 2008     7         26         6      NA       1540      NA
    ## 59833 2008     7         26         6      NA       1010      NA
    ## 59834 2008     7         26         6      NA       1700      NA
    ## 59835 2008     7         26         6      NA        844      NA
    ## 60112 2008     7         27         7      NA       1534      NA
    ## 60113 2008     7         27         7      NA       1105      NA
    ## 60114 2008     7         27         7      NA       1305      NA
    ## 60115 2008     7         27         7      NA       1645      NA
    ## 60116 2008     7         27         7      NA       1729      NA
    ## 60117 2008     7         27         7      NA       2000      NA
    ## 60413 2008     7         28         1      NA        920      NA
    ## 60708 2008     7         29         2      NA       1415      NA
    ## 60709 2008     7         29         2      NA       1240      NA
    ## 60999 2008     7         30         3      NA       1425      NA
    ## 61000 2008     7         30         3      NA       1305      NA
    ## 61001 2008     7         30         3      NA       2000      NA
    ## 61002 2008     7         30         3      NA       1935      NA
    ## 61003 2008     7         30         3      NA       1130      NA
    ## 61004 2008     7         30         3      NA       2020      NA
    ## 61005 2008     7         30         3      NA        815      NA
    ## 61301 2008     7         31         4      NA        600      NA
    ## 61302 2008     7         31         4      NA       1755      NA
    ## 61303 2008     7         31         4      NA       1000      NA
    ## 61599 2008     8          1         5      NA       2045      NA
    ## 61842 2008     8          2         6      NA       1052      NA
    ## 61843 2008     8          2         6      NA       1120      NA
    ## 61844 2008     8          2         6      NA       1645      NA
    ## 61845 2008     8          2         6      NA       1729      NA
    ## 61846 2008     8          2         6      NA        730      NA
    ## 61847 2008     8          2         6      NA        800      NA
    ## 61848 2008     8          2         6      NA       2000      NA
    ## 62125 2008     8          3         7      NA       1920      NA
    ## 62126 2008     8          3         7      NA       1745      NA
    ## 62413 2008     8          4         1      NA        850      NA
    ## 62414 2008     8          4         1      NA        740      NA
    ## 62415 2008     8          4         1      NA       1120      NA
    ## 62416 2008     8          4         1      NA       1215      NA
    ## 62417 2008     8          4         1      NA       2020      NA
    ## 62418 2008     8          4         1      NA       1850      NA
    ## 62707 2008     8          5         2      NA       1445      NA
    ## 62708 2008     8          5         2      NA       1245      NA
    ## 62709 2008     8          5         2      NA       1255      NA
    ## 62710 2008     8          5         2      NA       1105      NA
    ## 63000 2008     8          6         3      NA       1629      NA
    ## 63001 2008     8          6         3      NA       1530      NA
    ## 63002 2008     8          6         3      NA       1355      NA
    ## 63294 2008     8          7         4      NA       1645      NA
    ## 63295 2008     8          7         4      NA       1850      NA
    ## 63296 2008     8          7         4      NA       2000      NA
    ## 63589 2008     8          8         5      NA       1750      NA
    ## 63590 2008     8          8         5      NA        835      NA
    ## 64113 2008     8         10         7      NA       1120      NA
    ## 64114 2008     8         10         7      NA       1105      NA
    ## 64115 2008     8         10         7      NA       1859      NA
    ## 64116 2008     8         10         7      NA        925      NA
    ## 64117 2008     8         10         7      NA        800      NA
    ## 64404 2008     8         11         1      NA        641      NA
    ## 64405 2008     8         11         1      NA       1745      NA
    ## 64406 2008     8         11         1      NA       1305      NA
    ## 64407 2008     8         11         1      NA       1729      NA
    ## 64408 2008     8         11         1      NA       1610      NA
    ## 64409 2008     8         11         1      NA       2000      NA
    ## 64698 2008     8         12         2      NA       1530      NA
    ## 64699 2008     8         12         2      NA       1440      NA
    ## 64700 2008     8         12         2      NA       1215      NA
    ## 64701 2008     8         12         2      NA       1615      NA
    ## 64990 2008     8         13         3      NA        600      NA
    ## 64991 2008     8         13         3      NA       1900      NA
    ## 65282 2008     8         14         4      NA       1640      NA
    ## 65283 2008     8         14         4      NA       1730      NA
    ## 65284 2008     8         14         4      NA       1505      NA
    ## 65285 2008     8         14         4      NA       2000      NA
    ## 65571 2008     8         15         5      NA        820      NA
    ## 65572 2008     8         15         5      NA       1759      NA
    ## 65573 2008     8         15         5      NA       1210      NA
    ## 65574 2008     8         15         5      NA       1105      NA
    ## 65575 2008     8         15         5      NA        700      NA
    ## 65576 2008     8         15         5      NA       1305      NA
    ## 65577 2008     8         15         5      NA       1620      NA
    ## 65578 2008     8         15         5      NA       1605      NA
    ## 65579 2008     8         15         5      NA        925      NA
    ## 65580 2008     8         15         5      NA       1035      NA
    ## 65581 2008     8         15         5      NA       2000      NA
    ## 65582 2008     8         15         5      NA       1440      NA
    ## 65821 2008     8         16         6      NA       1000      NA
    ## 66084 2008     8         17         7      NA       1810      NA
    ## 66085 2008     8         17         7      NA       1205      NA
    ## 66664 2008     8         19         2      NA       1425      NA
    ## 66665 2008     8         19         2      NA        815      NA
    ## 66951 2008     8         20         3      NA       1135      NA
    ## 66952 2008     8         20         3      NA       1425      NA
    ## 66953 2008     8         20         3      NA        815      NA
    ## 67246 2008     8         21         4      NA       1030      NA
    ## 67247 2008     8         21         4      NA        844      NA
    ## 67541 2008     8         22         5      NA       1325      NA
    ## 67542 2008     8         22         5      NA       1300      NA
    ## 67767 2008     8         23         6      NA       1415      NA
    ## 67768 2008     8         23         6      NA       1240      NA
    ## 68034 2008     8         24         7      NA       2045      NA
    ## 68316 2008     8         25         1      NA        641      NA
    ## 68317 2008     8         25         1      NA       1835      NA
    ## 68590 2008     8         26         2      NA        700      NA
    ## 68591 2008     8         26         2      NA       1159      NA
    ## 68592 2008     8         26         2      NA       1425      NA
    ## 68593 2008     8         26         2      NA        956      NA
    ## 68594 2008     8         26         2      NA       1655      NA
    ## 68595 2008     8         26         2      NA        815      NA
    ## 68874 2008     8         27         3      NA       1511      NA
    ## 68875 2008     8         27         3      NA        635      NA
    ## 69159 2008     8         28         4      NA        750      NA
    ## 69160 2008     8         28         4      NA       1000      NA
    ## 69161 2008     8         28         4      NA        820      NA
    ## 69446 2008     8         29         5      NA        850      NA
    ## 69447 2008     8         29         5      NA       1745      NA
    ## 69448 2008     8         29         5      NA       1610      NA
    ## 69854 2008     8         31         7      NA       1705      NA
    ## 69855 2008     8         31         7      NA       1345      NA
    ## 69856 2008     8         31         7      NA       1655      NA
    ## 70120 2008     9          1         1      NA       1900      NA
    ## 70121 2008     9          1         1      NA        635      NA
    ## 70122 2008     9          1         1      NA       1745      NA
    ## 70123 2008     9          1         1      NA       1035      NA
    ## 71169 2008     9          5         5      NA       1150      NA
    ## 71170 2008     9          5         5      NA       1840      NA
    ## 71171 2008     9          5         5      NA       1230      NA
    ## 71361 2008     9          6         6      NA       1100      NA
    ## 71362 2008     9          6         6      NA       1740      NA
    ## 71607 2008     9          7         7      NA       1245      NA
    ## 71868 2008     9          8         1      NA       1800      NA
    ## 71869 2008     9          8         1      NA       1600      NA
    ## 72123 2008     9          9         2      NA        700      NA
    ## 72873 2008     9         12         5      NA       1855      NA
    ## 72874 2008     9         12         5      NA        830      NA
    ## 72875 2008     9         12         5      NA       1630      NA
    ## 72876 2008     9         12         5      NA       1400      NA
    ## 72877 2008     9         12         5      NA       1105      NA
    ## 72878 2008     9         12         5      NA       2025      NA
    ## 72879 2008     9         12         5      NA       1305      NA
    ## 72880 2008     9         12         5      NA       1250      NA
    ## 72881 2008     9         12         5      NA       1130      NA
    ## 72882 2008     9         12         5      NA       1830      NA
    ## 72883 2008     9         12         5      NA       1457      NA
    ## 72884 2008     9         12         5      NA       1630      NA
    ## 72885 2008     9         12         5      NA        815      NA
    ## 72886 2008     9         12         5      NA        905      NA
    ## 72887 2008     9         12         5      NA       1405      NA
    ## 72888 2008     9         12         5      NA       1630      NA
    ## 72889 2008     9         12         5      NA       1045      NA
    ## 72890 2008     9         12         5      NA       1815      NA
    ## 72891 2008     9         12         5      NA       2040      NA
    ## 72892 2008     9         12         5      NA       1430      NA
    ## 72893 2008     9         12         5      NA       1835      NA
    ## 72894 2008     9         12         5      NA       1503      NA
    ## 72895 2008     9         12         5      NA       1439      NA
    ## 72896 2008     9         12         5      NA       1535      NA
    ## 72897 2008     9         12         5      NA       1820      NA
    ## 72898 2008     9         12         5      NA       2250      NA
    ## 72899 2008     9         12         5      NA       1955      NA
    ## 72900 2008     9         12         5      NA       2100      NA
    ## 72901 2008     9         12         5      NA       1108      NA
    ## 73050 2008     9         13         6      NA       1240      NA
    ## 73051 2008     9         13         6      NA       1515      NA
    ## 73052 2008     9         13         6      NA        905      NA
    ## 73053 2008     9         13         6      NA       1910      NA
    ## 73054 2008     9         13         6      NA        745      NA
    ## 73055 2008     9         13         6      NA       1210      NA
    ## 73056 2008     9         13         6      NA       1615      NA
    ## 73057 2008     9         13         6      NA       1525      NA
    ## 73058 2008     9         13         6      NA        700      NA
    ## 73059 2008     9         13         6      NA       1110      NA
    ## 73060 2008     9         13         6      NA       1105      NA
    ## 73061 2008     9         13         6      NA       1635      NA
    ## 73062 2008     9         13         6      NA       1425      NA
    ## 73063 2008     9         13         6      NA       1200      NA
    ## 73064 2008     9         13         6      NA        545      NA
    ## 73065 2008     9         13         6      NA        815      NA
    ## 73066 2008     9         13         6      NA       1457      NA
    ## 73067 2008     9         13         6      NA        650      NA
    ## 73068 2008     9         13         6      NA        905      NA
    ## 73069 2008     9         13         6      NA       1250      NA
    ## 73070 2008     9         13         6      NA       1030      NA
    ## 73071 2008     9         13         6      NA       1625      NA
    ## 73072 2008     9         13         6      NA       1130      NA
    ## 73073 2008     9         13         6      NA        900      NA
    ## 73074 2008     9         13         6      NA       1615      NA
    ## 73075 2008     9         13         6      NA       1335      NA
    ## 73076 2008     9         13         6      NA       1035      NA
    ## 73077 2008     9         13         6      NA       1910      NA
    ## 73078 2008     9         13         6      NA       1400      NA
    ## 73079 2008     9         13         6      NA        830      NA
    ## 73080 2008     9         13         6      NA       1645      NA
    ## 73081 2008     9         13         6      NA        925      NA
    ## 73082 2008     9         13         6      NA       1245      NA
    ## 73083 2008     9         13         6      NA       1455      NA
    ## 73084 2008     9         13         6      NA       1710      NA
    ## 73085 2008     9         13         6      NA       2100      NA
    ## 73086 2008     9         13         6      NA       1108      NA
    ## 73087 2008     9         13         6      NA        847      NA
    ## 73088 2008     9         13         6      NA       1310      NA
    ## 73089 2008     9         13         6      NA       1805      NA
    ## 73090 2008     9         13         6      NA       1545      NA
    ## 73091 2008     9         13         6      NA       1439      NA
    ## 73092 2008     9         13         6      NA       1955      NA
    ## 73307 2008     9         14         7      NA       1855      NA
    ## 73308 2008     9         14         7      NA        830      NA
    ## 73309 2008     9         14         7      NA       1630      NA
    ## 73310 2008     9         14         7      NA       1400      NA
    ## 73311 2008     9         14         7      NA       1105      NA
    ## 73312 2008     9         14         7      NA       2025      NA
    ## 73313 2008     9         14         7      NA        905      NA
    ## 73314 2008     9         14         7      NA       1250      NA
    ## 73315 2008     9         14         7      NA        650      NA
    ## 73316 2008     9         14         7      NA       1130      NA
    ## 73317 2008     9         14         7      NA       1830      NA
    ## 73318 2008     9         14         7      NA        725      NA
    ## 73319 2008     9         14         7      NA       1457      NA
    ## 73320 2008     9         14         7      NA       1030      NA
    ## 73321 2008     9         14         7      NA       1620      NA
    ## 73322 2008     9         14         7      NA        815      NA
    ## 73323 2008     9         14         7      NA       1405      NA
    ## 73324 2008     9         14         7      NA       1630      NA
    ## 73325 2008     9         14         7      NA       1045      NA
    ## 73326 2008     9         14         7      NA       1815      NA
    ## 73327 2008     9         14         7      NA       2040      NA
    ## 73328 2008     9         14         7      NA       1108      NA
    ## 73329 2008     9         14         7      NA       2100      NA
    ## 73330 2008     9         14         7      NA       1439      NA
    ## 73331 2008     9         14         7      NA       1010      NA
    ## 73332 2008     9         14         7      NA       1535      NA
    ## 73333 2008     9         14         7      NA       1840      NA
    ## 73334 2008     9         14         7      NA       2250      NA
    ## 73335 2008     9         14         7      NA       1955      NA
    ## 73336 2008     9         14         7      NA        847      NA
    ## 73337 2008     9         14         7      NA       1310      NA
    ## 73584 2008     9         15         1      NA       1855      NA
    ## 73585 2008     9         15         1      NA        830      NA
    ## 73586 2008     9         15         1      NA       1630      NA
    ## 73587 2008     9         15         1      NA       1400      NA
    ## 73588 2008     9         15         1      NA       1105      NA
    ## 73589 2008     9         15         1      NA       2025      NA
    ## 73590 2008     9         15         1      NA        645      NA
    ## 73591 2008     9         15         1      NA        545      NA
    ## 73592 2008     9         15         1      NA        800      NA
    ## 73593 2008     9         15         1      NA       1405      NA
    ## 73594 2008     9         15         1      NA       1630      NA
    ## 73595 2008     9         15         1      NA        700      NA
    ## 73596 2008     9         15         1      NA       1045      NA
    ## 73597 2008     9         15         1      NA       1815      NA
    ## 73598 2008     9         15         1      NA       2040      NA
    ## 73599 2008     9         15         1      NA        847      NA
    ## 73848 2008     9         16         2      NA       1855      NA
    ## 73849 2008     9         16         2      NA       2025      NA
    ## 73850 2008     9         16         2      NA        645      NA
    ## 73851 2008     9         16         2      NA        800      NA
    ## 73852 2008     9         16         2      NA        700      NA
    ## 73853 2008     9         16         2      NA       2040      NA
    ## 74105 2008     9         17         3      NA       1855      NA
    ## 74106 2008     9         17         3      NA       2025      NA
    ## 74107 2008     9         17         3      NA        645      NA
    ## 74108 2008     9         17         3      NA        800      NA
    ## 74109 2008     9         17         3      NA        700      NA
    ## 74110 2008     9         17         3      NA       2040      NA
    ## 74367 2008     9         18         4      NA       1855      NA
    ## 74368 2008     9         18         4      NA       2025      NA
    ## 74369 2008     9         18         4      NA        645      NA
    ## 74370 2008     9         18         4      NA        800      NA
    ## 74371 2008     9         18         4      NA        700      NA
    ## 74372 2008     9         18         4      NA       2040      NA
    ## 74634 2008     9         19         5      NA       1150      NA
    ## 74635 2008     9         19         5      NA       1045      NA
    ## 76365 2008     9         26         5      NA       1310      NA
    ## 76366 2008     9         26         5      NA       2010      NA
    ## 77574 2008    10          1         3      NA       1145      NA
    ## 77925 2008    10          3         5    1050        855      NA
    ## 78095 2008    10          3         5      NA       1505      NA
    ## 78096 2008    10          3         5      NA       1815      NA
    ## 78097 2008    10          3         5      NA       1635      NA
    ## 79046 2008    10          7         2      NA       1317      NA
    ## 79047 2008    10          7         2      NA       2005      NA
    ## 79300 2008    10          8         3      NA       1505      NA
    ## 79301 2008    10          8         3      NA        855      NA
    ## 79562 2008    10          9         4      NA       1620      NA
    ## 79563 2008    10          9         4      NA       1345      NA
    ## 79923 2008    10         11         6    1310       1314      NA
    ## 80013 2008    10         11         6      NA       1651      NA
    ## 80772 2008    10         14         2      NA       1505      NA
    ## 80773 2008    10         14         2      NA        855      NA
    ## 81474 2008    10         17         5    1643       1651      NA
    ## 82245 2008    10         20         1      NA        810      NA
    ## 82496 2008    10         21         2      NA        925      NA
    ## 82746 2008    10         22         3      NA        825      NA
    ## 82747 2008    10         22         3      NA        805      NA
    ## 82748 2008    10         22         3      NA       1958      NA
    ## 82749 2008    10         22         3      NA       1805      NA
    ## 83010 2008    10         23         4      NA        545      NA
    ## 83011 2008    10         23         4      NA        655      NA
    ## 83463 2008    10         25         6      NA       2010      NA
    ## 83707 2008    10         26         7      NA        700      NA
    ## 84219 2008    10         28         2      NA       1505      NA
    ## 84220 2008    10         28         2      NA       1210      NA
    ## 84221 2008    10         28         2      NA        855      NA
    ## 84734 2008    10         30         4    2221       1848      NA
    ## 85913 2008    11          4         2      NA        600      NA
    ## 86411 2008    11          6         4      NA        910      NA
    ## 87334 2008    11         10         1      NA       1915      NA
    ## 87335 2008    11         10         1      NA       2040      NA
    ## 87336 2008    11         10         1      NA       1925      NA
    ## 87337 2008    11         10         1      NA       2325      NA
    ## 87580 2008    11         11         2      NA        720      NA
    ## 87581 2008    11         11         2      NA        645      NA
    ## 87582 2008    11         11         2      NA        855      NA
    ## 87583 2008    11         11         2      NA        610      NA
    ## 87828 2008    11         12         3      NA       1115      NA
    ## 87829 2008    11         12         3      NA        925      NA
    ## 88751 2008    11         16         7      NA       1035      NA
    ## 88752 2008    11         16         7      NA        920      NA
    ## 89493 2008    11         19         3      NA       1220      NA
    ## 89494 2008    11         19         3      NA       1200      NA
    ## 89495 2008    11         19         3      NA       1055      NA
    ## 89496 2008    11         19         3      NA        845      NA
    ## 89748 2008    11         20         4      NA        620      NA
    ## 91182 2008    11         26         3      NA       1751      NA
    ## 91762 2008    11         29         6      NA       1220      NA
    ## 91763 2008    11         29         6      NA       1055      NA
    ## 93003 2008    12          4         4      NA        645      NA
    ## 93251 2008    12          5         5      NA        910      NA
    ## 94150 2008    12          9         2      NA       1212      NA
    ## 94151 2008    12          9         2      NA       1628      NA
    ## 94152 2008    12          9         2      NA       1350      NA
    ## 94153 2008    12          9         2      NA        925      NA
    ## 94154 2008    12          9         2      NA       1955      NA
    ## 94155 2008    12          9         2      NA       2005      NA
    ## 94388 2008    12         10         3    2109       1915      NA
    ## 94392 2008    12         10         3      NA       1455      NA
    ## 94393 2008    12         10         3      NA        600      NA
    ## 94394 2008    12         10         3      NA        600      NA
    ## 94395 2008    12         10         3      NA        650      NA
    ## 94396 2008    12         10         3      NA       2245      NA
    ## 94397 2008    12         10         3      NA        855      NA
    ## 94644 2008    12         11         4      NA       1350      NA
    ## 94645 2008    12         11         4      NA        545      NA
    ## 94893 2008    12         12         5      NA        720      NA
    ## 94894 2008    12         12         5      NA        610      NA
    ## 95200 2008    12         14         7    1448       1445      NA
    ## 95308 2008    12         14         7      NA       1212      NA
    ## 95309 2008    12         14         7      NA       1630      NA
    ## 95310 2008    12         14         7      NA        925      NA
    ## 95471 2008    12         15         1    1559       1415      NA
    ## 95556 2008    12         15         1      NA       1115      NA
    ## 95557 2008    12         15         1      NA       1303      NA
    ## 95558 2008    12         15         1      NA       1650      NA
    ## 95559 2008    12         15         1      NA        925      NA
    ## 95560 2008    12         15         1      NA       1125      NA
    ## 95796 2008    12         16         2      NA       1900      NA
    ## 95797 2008    12         16         2      NA       1200      NA
    ## 95798 2008    12         16         2      NA        640      NA
    ## 95799 2008    12         16         2      NA       1730      NA
    ## 95800 2008    12         16         2      NA       1705      NA
    ## 95801 2008    12         16         2      NA       1153      NA
    ## 95802 2008    12         16         2      NA       1751      NA
    ## 95803 2008    12         16         2      NA       1750      NA
    ## 95804 2008    12         16         2      NA       1055      NA
    ## 95805 2008    12         16         2      NA       2005      NA
    ## 95806 2008    12         16         2      NA       1420      NA
    ## 96051 2008    12         17         3      NA       1945      NA
    ## 96052 2008    12         17         3      NA        715      NA
    ## 96301 2008    12         18         4      NA        914      NA
    ## 96302 2008    12         18         4      NA       1705      NA
    ## 96303 2008    12         18         4      NA       1745      NA
    ## 96304 2008    12         18         4      NA        730      NA
    ## 96549 2008    12         19         5      NA       1545      NA
    ## 96550 2008    12         19         5      NA       1330      NA
    ## 96551 2008    12         19         5      NA       1227      NA
    ## 96552 2008    12         19         5      NA        910      NA
    ## 96553 2008    12         19         5      NA        815      NA
    ## 96554 2008    12         19         5      NA       2005      NA
    ## 96555 2008    12         19         5      NA       1435      NA
    ## 96556 2008    12         19         5      NA       2308      NA
    ## 96745 2008    12         20         6      NA       1755      NA
    ## 96746 2008    12         20         6      NA       1655      NA
    ## 96747 2008    12         20         6      NA        805      NA
    ## 96748 2008    12         20         6      NA       1445      NA
    ## 96973 2008    12         21         7      NA       1035      NA
    ## 96974 2008    12         21         7      NA       1820      NA
    ## 96975 2008    12         21         7      NA       1220      NA
    ## 96976 2008    12         21         7      NA        920      NA
    ## 96977 2008    12         21         7      NA       1635      NA
    ## 96978 2008    12         21         7      NA        800      NA
    ## 97227 2008    12         22         1      NA       1655      NA
    ## 97228 2008    12         22         1      NA       1445      NA
    ## 97472 2008    12         23         2      NA       1900      NA
    ## 97473 2008    12         23         2      NA       1705      NA
    ## 97474 2008    12         23         2      NA       1750      NA
    ## 97475 2008    12         23         2      NA       1227      NA
    ## 97476 2008    12         23         2      NA       1705      NA
    ## 97678 2008    12         24         3      NA       1330      NA
    ## 97679 2008    12         24         3      NA       1300      NA
    ## 98125 2008    12         26         5      NA       1720      NA
    ## 98126 2008    12         26         5      NA        600      NA
    ## 98127 2008    12         26         5      NA       1850      NA
    ## 98128 2008    12         26         5      NA       2005      NA
    ## 98330 2008    12         27         6      NA       1440      NA
    ## 98331 2008    12         27         6      NA        600      NA
    ## 98332 2008    12         27         6      NA        750      NA
    ## 98556 2008    12         28         7      NA       1227      NA
    ## 98804 2008    12         29         1      NA       1615      NA
    ## 99048 2008    12         30         2      NA        945      NA
    ## 99049 2008    12         30         2      NA        825      NA
    ## 99050 2008    12         30         2      NA        800      NA
    ## 99259 2008    12         31         3      NA       1220      NA
    ## 99260 2008    12         31         3      NA       1300      NA
    ##       CRSArrTime UniqueCarrier FlightNum TailNum ActualElapsedTime
    ## 250          922            OO      5972  N726SK                NA
    ## 251         1835            WN      2880                        NA
    ## 252         1625            AA      1128                        NA
    ## 253         2015            AA      1566                        NA
    ## 254         1710            WN      2793                        NA
    ## 255         1835            AA       479                        NA
    ## 552         1925            WN       460                        NA
    ## 553         2145            MQ      3443                        NA
    ## 554         1810            WN       373                        NA
    ## 555         2020            MQ      3824                        NA
    ## 849         1900            UA      1291                        NA
    ## 850         1925            WN       460                        NA
    ## 851         1530            WN       112                        NA
    ## 852         1701            UA       374                        NA
    ## 853         1415            WN       112                        NA
    ## 854         1810            WN       373                        NA
    ## 1151        1015            MQ      3364                        NA
    ## 1152        1645            MQ      3374                        NA
    ## 1153        1525            MQ      3446                        NA
    ## 1154         855            MQ      3890                        NA
    ## 1674        2301            YV      2890  N918FJ                NA
    ## 1675        1235            AA       368                        NA
    ## 1676        2030            AA      2293                        NA
    ## 2259        2015            AA      2402                        NA
    ## 2550        1925            WN       460                        NA
    ## 2551        1810            WN       373                        NA
    ## 2846        1855            UA       509                        NA
    ## 2847         805            MQ      3366                        NA
    ## 2848        2226            UA       452                        NA
    ## 2849         640            MQ      3362                        NA
    ## 3147        2003            OO      5975  N716SK                NA
    ## 3370         750            9E      5743                        NA
    ## 3643        2200            CO       351                        NA
    ## 3929         805            MQ      3366                        NA
    ## 3930        2000            MQ      3441                        NA
    ## 3931        2055            AA      2059                        NA
    ## 3932        1804            B6      1264                        NA
    ## 3933        1133            B6      1060                        NA
    ## 3934         640            MQ      3362                        NA
    ## 3935        1840            MQ      3422                        NA
    ## 3936        1905            AA       563                        NA
    ## 3937        1235            B6      1263                        NA
    ## 3938        1122            B6      1061                        NA
    ## 3939        1747            CO       317                        NA
    ## 4222        1010            AA       379                        NA
    ## 4223         835            AA      1614                        NA
    ## 4224        1100            AA      1199                        NA
    ## 4225        1805            AA      1308                        NA
    ## 4514        1829            EV      4325  N860AS                NA
    ## 4515         805            AA       652                        NA
    ## 4516        2256            OH      5579                        NA
    ## 4517        2011            DL       693  N939DL                NA
    ## 4810        2003            OO      5975                        NA
    ## 4811        1024            DL      1590  N909DL                NA
    ## 4812        2251            OO      5978  N701SK                NA
    ## 4813        1148            EV      4179  N708EV                NA
    ## 4814        1009            EV      4677  N921EV                NA
    ## 4815        1140            AA      1477                        NA
    ## 5114         911            OO      5964  N701SK                NA
    ## 5320        1136            OH      5124                        NA
    ## 5321        1024            DL      1590  N921DL                NA
    ## 5322        1917            EV      4518  N821AS                NA
    ## 5323         909            EV      4788  N758EV                NA
    ## 5324        1627            DL      1533  N920DL                NA
    ## 5325        1148            EV      4179  N758EV                NA
    ## 5326        1446            EV      4882  N821AS                NA
    ## 5593        1820            OO      2933  N709BR                NA
    ## 5594        1840            AA       346                        NA
    ## 5595        1555            OO      2932  N709BR                NA
    ## 5596        1515            AA      1545                        NA
    ## 5892        2003            OO      5975  N712SK                NA
    ## 5893        2251            OO      5978  N746SK                NA
    ## 5894        2133            OO      6772  N746SK                NA
    ## 6175         909            EV      4788  N390CA                NA
    ## 6176         805            MQ      3366                        NA
    ## 6177        1840            AA       346                        NA
    ## 6178        1148            EV      4179  N707EV                NA
    ## 6179         640            MQ      3362                        NA
    ## 6180        1515            AA      1545                        NA
    ## 6470         735            F9       211  N929FR                NA
    ## 6471        2015            AA      2402                        NA
    ## 6472        1650            AA      1053                        NA
    ## 6767        1032            XE       311                        NA
    ## 6768        1902            XE       315                        NA
    ## 6769        2055            AA      2059                        NA
    ## 6770        1525            WN      2008                        NA
    ## 6771        1349            XE       531                        NA
    ## 6772        1723            XE        80                        NA
    ## 7066        1820            OO      2933  N479CA                NA
    ## 7067        1155            AA      2024                        NA
    ## 7068        1555            OO      2932  N479CA                NA
    ## 7069         900            AA      1109                        NA
    ## 7070        1140            AA      1477                        NA
    ## 7280        1024            DL      1590  N919DL                NA
    ## 7281        1619            DL      1533  N957DL                NA
    ## 7282        1148            EV      4179  N390CA                NA
    ## 7546        1015            MQ      3364                        NA
    ## 7547         805            MQ      3366                        NA
    ## 7548        1645            MQ      3374                        NA
    ## 7549        1220            MQ      3488                        NA
    ## 7550        2256            OH      5579  N656CA                NA
    ## 7551        1100            MQ      3355                        NA
    ## 7552        1525            MQ      3446                        NA
    ## 7553         855            MQ      3890                        NA
    ## 7842        2038            YV      7071  N502MJ                NA
    ## 7843         722            YV      2908  N928LR                NA
    ## 7844        2003            OO      5975  N702SK                NA
    ## 7845        2015            AA      2402                        NA
    ## 7846        1415            AA      2486                        NA
    ## 7847        1245            YV      7345  N502MJ                NA
    ## 7848        1650            AA      1053                        NA
    ## 7849          20            AA      2491                        NA
    ## 8122         840            WN      2350                        NA
    ## 8123        1500            MQ      3503                        NA
    ## 8124        1515            AA       422                        NA
    ## 8125        2015            AA      1566                        NA
    ## 8126        2015            AA      2402                        NA
    ## 8127        1415            AA      2486                        NA
    ## 8128        1929            CO       620                        NA
    ## 8129        2135            WN      3060                        NA
    ## 8130        1340            MQ      3412                        NA
    ## 8131        1835            AA       479                        NA
    ## 8132        1335            AA       668                        NA
    ## 8133        1650            AA      1053                        NA
    ## 8134          20            AA      2491                        NA
    ## 8135        1747            CO       317                        NA
    ## 8424        1220            MQ      3488                        NA
    ## 8425        1235            AA       368                        NA
    ## 8426        1100            MQ      3355                        NA
    ## 8427        1515            AA      1545                        NA
    ## 8719        2003            OO      5975  N719SK                NA
    ## 8720        1845            AA       346                        NA
    ## 8721        2015            AA      2402                        NA
    ## 8722        1415            AA      2486                        NA
    ## 8723        2251            OO      5978  N712SK                NA
    ## 8724        1100            AA      1199                        NA
    ## 8725        2030            AA      2293                        NA
    ## 8726          15            AA      2491                        NA
    ## 9023        1010            AA       379                        NA
    ## 9024        2359            B6      1069  N179JB                NA
    ## 9226        1605            YV      7276  N512MJ                NA
    ## 9227        1845            AA       346                        NA
    ## 9228        1415            AA      2486                        NA
    ## 9229        1133            B6      1060  N179JB                NA
    ## 9230        2030            YV      7281  N508MJ                NA
    ## 9231        1055            YV      7446  N512MJ                NA
    ## 9232        1025            AA       421                        NA
    ## 9233        1100            AA      1199                        NA
    ## 9499        1815            WN      3506                        NA
    ## 9500        1605            YV      7276  N508MJ                NA
    ## 9501         750            9E      5743                        NA
    ## 9502        1700            WN      3419                        NA
    ## 9503          15            AA      2491                        NA
    ## 9785        1545            WN      1353                        NA
    ## 9786        1835            WN      2976                        NA
    ## 9787        2003            OO      5975  N773SK                NA
    ## 9788        2145            MQ      3443                        NA
    ## 9789        1220            MQ      3488                        NA
    ## 9790        1845            AA       346                        NA
    ## 9791         835            AA      1614                        NA
    ## 9792        1415            AA      2486                        NA
    ## 9793        2230            WN      1505                        NA
    ## 9794        1115            WN      3916                        NA
    ## 9795        2251            OO      5978  N706SK                NA
    ## 9796        1100            MQ      3355                        NA
    ## 9797        2020            MQ      3824                        NA
    ## 9798        1100            AA      1199                        NA
    ## 9799        2030            AA      2293                        NA
    ## 10078       1815            WN      3506                        NA
    ## 10079        800            MQ      3366                        NA
    ## 10080       1825            MQ      3400                        NA
    ## 10081       2015            AA      2402                        NA
    ## 10082       1700            WN      3419                        NA
    ## 10083        640            MQ      3362                        NA
    ## 10084       1705            MQ      3375                        NA
    ## 10085       2115            9E      5746                        NA
    ## 10360       1150            WN      3585                        NA
    ## 10361       1320            OO      2931  N479CA                NA
    ## 10362       1220            MQ      3488                        NA
    ## 10363        750            9E      5743                        NA
    ## 10364       1845            AA       346                        NA
    ## 10365       1235            AA       368                        NA
    ## 10366       1910            AA       766                        NA
    ## 10367       2015            AA      2402                        NA
    ## 10368       1415            AA      2486                        NA
    ## 10369       2200            WN       224                        NA
    ## 10370       1100            OO      2930  N479CA                NA
    ## 10371       2251            OO      5978  N751SK                NA
    ## 10372       1100            MQ      3355                        NA
    ## 10373       1650            AA      1053                        NA
    ## 10374       1100            AA      1199                        NA
    ## 10375       1515            AA      1545                        NA
    ## 10376       2030            AA      2293                        NA
    ## 10377         15            AA      2491                        NA
    ## 10672       2301            YV      2890  N939LR                NA
    ## 10673       1320            OO      2931  N468CA                NA
    ## 10674       1936            YV      2889  N939LR                NA
    ## 10675       1100            OO      2930  N468CA                NA
    ## 10971       2003            OO      5975  N766SK                NA
    ## 10972       1350            9E      5745                        NA
    ## 10973       1125            9E      5744                        NA
    ## 11183       1000            AA      1743                        NA
    ## 11750       1825            MQ      3400                        NA
    ## 11751       1705            MQ      3375                        NA
    ## 12035       2003            OO      5975  N776SK                NA
    ## 12036       1845            AA       346                        NA
    ## 12037       2030            AA      2293                        NA
    ## 12038       2359            B6      1069  N183JB                NA
    ## 12328       2145            MQ      3443                        NA
    ## 12329       1756            9E      5747                        NA
    ## 12330       1900            AA      1646                        NA
    ## 12331       1415            AA      2486                        NA
    ## 12332       1133            B6      1060  N183JB                NA
    ## 12333       2020            MQ      3824                        NA
    ## 12334       1530            9E      5742                        NA
    ## 12335       1100            AA      1199                        NA
    ## 12336       1625            AA      1563                        NA
    ## 12632       1815            WN      3506                        NA
    ## 12633        840            WN      2350                        NA
    ## 12634       1559            YV      7276  N502MJ                NA
    ## 12635       2216            YV      2890  N956LR                NA
    ## 12636       2135            WN      3060                        NA
    ## 12637        805            WN      2805                        NA
    ## 12638       1936            YV      2889  N956LR                NA
    ## 12639       2150            9E      5746                        NA
    ## 12942        750            9E      5743                        NA
    ## 12943       2150            9E      5746                        NA
    ## 13149        750            9E      5743                        NA
    ## 13150       1345            9E      5745                        NA
    ## 13151       1755            9E      5747                        NA
    ## 13152       1805            AA       463                        NA
    ## 13153       1910            AA       766                        NA
    ## 13154       1625            AA      1128                        NA
    ## 13155       1349            XE       531                        NA
    ## 13156       1530            9E      5742                        NA
    ## 13157       1125            9E      5744                        NA
    ## 13158       1835            AA       479                        NA
    ## 13159       1615            AA       559                        NA
    ## 13160       2035            AA      1202                        NA
    ## 13161       1725            AA      1215                        NA
    ## 13422       1221            XE       532                        NA
    ## 13423       1320            OO      2931  N471CA                NA
    ## 13424       2015            AA      2402                        NA
    ## 13425       1415            AA      2486                        NA
    ## 13426        857            XE       541                        NA
    ## 13427       1555            OO      2932  N471CA                NA
    ## 13428       1859            DL      1255  N921DL                NA
    ## 13429       1650            AA      1053                        NA
    ## 13430         15            AA      2491                        NA
    ## 13725        902            DL      1278  N955DL                NA
    ## 13726       2213            B6      1068  N279JB                NA
    ## 14021        902            DL      1278  N985DL                NA
    ## 14022       2000            MQ      3441                        NA
    ## 14023       1415            AA      2486                        NA
    ## 14024       1840            MQ      3422                        NA
    ## 14025       1100            AA      1199                        NA
    ## 14325       1015            MQ      3364                        NA
    ## 14326        850            MQ      3890                        NA
    ## 14623       1320            OO      2931  N498CA                NA
    ## 14624       1900            AA      1646                        NA
    ## 14625       2215            CO       351                        NA
    ## 14626       2035            OH      5047                        NA
    ## 14627       1100            OO      2930  N498CA                NA
    ## 14628       1625            AA      1563                        NA
    ## 14629       2359            AA      2246                        NA
    ## 14630       2034            CO      1441                        NA
    ## 14918       1019            OH      5034                        NA
    ## 14919       1558            OH      5202                        NA
    ## 14920       1220            MQ      3488                        NA
    ## 14921        805            AA       652                        NA
    ## 14922       1133            B6      1060  N238JB                NA
    ## 14923       2213            B6      1068  N229JB                NA
    ## 14924       1101            CO       852                        NA
    ## 14925       1640            CO       251                        NA
    ## 14926       2035            OH      5047  N379CA                NA
    ## 14927       2334            OH      5203                        NA
    ## 14928       1050            OH      5527                        NA
    ## 14929       2329            OH      5579                        NA
    ## 14930       1100            MQ      3355                        NA
    ## 14931       1122            B6      1061  N203JB                NA
    ## 14932       1646            B6      1065  N229JB                NA
    ## 14933       1751            CO       317                        NA
    ## 14934       1115            CO       350                        NA
    ## 15154       1004            OH      5034                        NA
    ## 15155       1059            OH      5186                        NA
    ## 15156       1010            OO      4009  N668CA                NA
    ## 15436       2150            9E      5746                        NA
    ## 15728       1815            WN      3506                        NA
    ## 15729       1140            WN      1757                        NA
    ## 15730       1240            YV      7271  N514MJ                NA
    ## 15731       2021            EV      4243  N707EV                NA
    ## 15732        750            9E      5743                        NA
    ## 15733       1415            AA      2486                        NA
    ## 15734       1700            WN      3419                        NA
    ## 15735       1035            WN      1670                        NA
    ## 15736       2035            YV      7451  N504MJ                NA
    ## 15737       1639            EV      4161  N707EV                NA
    ## 15738       2030            AA      2293                        NA
    ## 16034       1559            YV      7276  N504MJ                NA
    ## 16035       1415            AA      2486                        NA
    ## 16036       2305            OO      5978  N738SK                NA
    ## 16037       2030            AA      2293                        NA
    ## 16318       2050            YV      7228  N518LR                NA
    ## 16596       1017            US       435                        NA
    ## 16597       1015            MQ      3364                        NA
    ## 16598       1010            AA       379                        NA
    ## 16599       1652            OO      6088  N715SK                NA
    ## 16600        850            MQ      3890                        NA
    ## 16882       2150            9E      5746                        NA
    ## 17105        750            9E      5743                        NA
    ## 17680       1645            MQ      3374                        NA
    ## 17681       1515            AA       422                        NA
    ## 17682       2015            AA      1566                        NA
    ## 17683       1900            AA      1646                        NA
    ## 17684       2005            AA      1703                        NA
    ## 17685       1609            CO      1706                        NA
    ## 17686       1520            MQ      3446                        NA
    ## 17687       1835            AA       479                        NA
    ## 17688       1735            AA      1024                        NA
    ## 17689       1945            AA      1113                        NA
    ## 17690       1625            AA      1563                        NA
    ## 17691       1430            CO       241                        NA
    ## 17977       1505            WN      3241                        NA
    ## 17978       1554            OH      5202                        NA
    ## 17979       1045            AA       511                        NA
    ## 17980       1900            AA      1646                        NA
    ## 17981       1415            AA      2486                        NA
    ## 17982       2011            DL      1808  N904DA                NA
    ## 17983       1350            WN      3241                        NA
    ## 17984       2331            OH      5231  N655CA                NA
    ## 17985        900            AA      1109                        NA
    ## 17986       1100            AA      1199                        NA
    ## 17987       1625            AA      1563                        NA
    ## 17988       1615            DL      1525  N904DA                NA
    ## 18286       1554            XE      2366                        NA
    ## 18287       1140            OH      5124  N693CA                NA
    ## 18288        948            OH      5534  N655CA                NA
    ## 18289       1127            XE      2252                        NA
    ## 18567       1645            MQ      3374                        NA
    ## 18568       1825            MQ      3400                        NA
    ## 18569       2000            MQ      3441                        NA
    ## 18570       1425            AA       418                        NA
    ## 18571       1515            AA       422                        NA
    ## 18572       1805            AA       463                        NA
    ## 18573       1910            AA       766                        NA
    ## 18574       1625            AA      1128                        NA
    ## 18575       1605            AA      1198                        NA
    ## 18576       2015            AA      1566                        NA
    ## 18577       2055            AA      2059                        NA
    ## 18578       1705            MQ      3375                        NA
    ## 18579       1840            MQ      3422                        NA
    ## 18580       1520            MQ      3446                        NA
    ## 18581       2230            AA       417                        NA
    ## 18582       1835            AA       479                        NA
    ## 18583       1615            AA       559                        NA
    ## 18584       1905            AA       563                        NA
    ## 18585       1335            AA       668                        NA
    ## 18586       1945            AA      1113                        NA
    ## 18587       2335            AA      1129                        NA
    ## 18588       2035            AA      1202                        NA
    ## 18589       1725            AA      1215                        NA
    ## 18590       1140            AA      1477                        NA
    ## 18591       1445            AA      1587                        NA
    ## 18592       1230            AA      1717                        NA
    ## 18593       2145            AA      1731                        NA
    ## 18883       2145            MQ      3443                        NA
    ## 18884        935            AA       450                        NA
    ## 18885       1045            AA       511                        NA
    ## 18886        805            AA       652                        NA
    ## 18887        655            AA      1157                        NA
    ## 18888        840            AA      1958                        NA
    ## 18889       1150            AA      2024                        NA
    ## 18890        805            WN      2805                        NA
    ## 18891       2259            XE      3033                        NA
    ## 18892       2249            OH      5085                        NA
    ## 18893       2331            OH      5231  N642CA                NA
    ## 18894       1504            OH      5339                        NA
    ## 18895       2020            MQ      3824                        NA
    ## 18896       2150            9E      5746                        NA
    ## 18897        900            AA      1109                        NA
    ## 19118       1122            XE      2144                        NA
    ## 19119       1131            OH      5534                        NA
    ## 19120       1529            OH      5574                        NA
    ## 19121        750            9E      5743                        NA
    ## 19122       1944            XE      2252                        NA
    ## 19123       2341            OH      5203                        NA
    ## 19406       1043            XE      2366                        NA
    ## 19407       1554            OH      5202                        NA
    ## 19408       2341            OH      5203  N693CA                NA
    ## 19409       2331            OH      5231  N371CA                NA
    ## 19705       1445            WN      2640                        NA
    ## 19706       1020            OH      5034                        NA
    ## 19707        948            OH      5534                        NA
    ## 19708       1645            MQ      3374                        NA
    ## 19709       2145            MQ      3443                        NA
    ## 19710       1335            WN       494                        NA
    ## 19711       1816            OH      5053                        NA
    ## 19712       1520            MQ      3446                        NA
    ## 19713       2020            MQ      3824                        NA
    ## 19999        725            WN      1306                        NA
    ## 20000        840            WN      2182                        NA
    ## 20001       1015            MQ      3364                        NA
    ## 20002        800            MQ      3366                        NA
    ## 20003       1645            MQ      3374                        NA
    ## 20004       2000            MQ      3441                        NA
    ## 20005       1105            WN      1090                        NA
    ## 20006        915            WN      1092                        NA
    ## 20007       1100            MQ      3355                        NA
    ## 20008        640            MQ      3362                        NA
    ## 20009       1840            MQ      3422                        NA
    ## 20010       1520            MQ      3446                        NA
    ## 20311        855            WN      3754                        NA
    ## 20611        800            MQ      3366                        NA
    ## 20612        825            AA       579                        NA
    ## 20613       1900            AA      1646                        NA
    ## 20614        640            MQ      3362                        NA
    ## 20615       1425            AA      1810                        NA
    ## 20918       1220            MQ      3488                        NA
    ## 20919       1100            MQ      3355                        NA
    ## 21147       1835            AA       479                        NA
    ## 21432       1240            WN      2967                        NA
    ## 21433       1816            OH      5053  N691CA                NA
    ## 21434       2150            9E      5746                        NA
    ## 21736       2000            MQ      3441                        NA
    ## 21737        750            9E      5743                        NA
    ## 21738       1515            AA       422                        NA
    ## 21739       1840            MQ      3422                        NA
    ## 21740       1335            AA       668                        NA
    ## 22000       1240            WN      2967                        NA
    ## 22001       1835            OO      5592                        NA
    ## 22002       2011            DL      1808  N905DA                NA
    ## 22003       1645            MQ      3374                        NA
    ## 22004       1825            MQ      3400                        NA
    ## 22005       2145            MQ      3443                        NA
    ## 22006       1220            MQ      3488                        NA
    ## 22007       1500            MQ      3503                        NA
    ## 22008       1425            AA       418                        NA
    ## 22009       1515            AA       422                        NA
    ## 22010       1805            AA       463                        NA
    ## 22011       1910            AA       766                        NA
    ## 22012       1625            AA      1128                        NA
    ## 22013       1325            AA      1526                        NA
    ## 22014       2015            AA      1566                        NA
    ## 22015       2055            AA      2059                        NA
    ## 22016       1011            CO       640                        NA
    ## 22017       2022            CO      1573                        NA
    ## 22018       1714            CO      1602                        NA
    ## 22019       1455            WN       420                        NA
    ## 22020       1335            WN       494                        NA
    ## 22021       1235            WN      1136                        NA
    ## 22022       2305            OO      5978  N776SK                NA
    ## 22023       1705            MQ      3375                        NA
    ## 22024       1335            MQ      3412                        NA
    ## 22025       1520            MQ      3446                        NA
    ## 22026       2020            MQ      3824                        NA
    ## 22027       2150            9E      5746                        NA
    ## 22028       2230            AA       417                        NA
    ## 22029       1835            AA       479                        NA
    ## 22030       1615            AA       559                        NA
    ## 22031       1905            AA       563                        NA
    ## 22032       1335            AA       668                        NA
    ## 22033       1945            AA      1113                        NA
    ## 22034       2335            AA      1129                        NA
    ## 22035       2035            AA      1202                        NA
    ## 22036       1725            AA      1215                        NA
    ## 22037       1445            AA      1587                        NA
    ## 22038       1230            AA      1717                        NA
    ## 22039       2145            AA      1731                        NA
    ## 22040       2014            CO      1441                        NA
    ## 22041       1528            CO      1541                        NA
    ## 22042       1834            CO      1603                        NA
    ## 22043        824            CO      1841                        NA
    ## 22338       1554            OH      5202                        NA
    ## 22339        800            MQ      3366                        NA
    ## 22340        750            9E      5743                        NA
    ## 22341       1755            9E      5747                        NA
    ## 22342        935            AA       450                        NA
    ## 22343        741            CO       340                        NA
    ## 22344       2331            OH      5231  N371CA                NA
    ## 22345        640            MQ      3362                        NA
    ## 22346       1530            9E      5742                        NA
    ## 22652        948            OH      5534                        NA
    ## 22952        622            YV      2819  N902FJ                NA
    ## 22953        948            OH      5534                        NA
    ## 22954        900            OO      6022  N719SK                NA
    ## 22955       1235            AA       368                        NA
    ## 22956       2259            XE      3033                        NA
    ## 22957       1603            YV      7207  N522LR                NA
    ## 22958       2305            OO      5978  N750SK                NA
    ## 22959       1515            AA      1545                        NA
    ## 23472       1015            MQ      3364                        NA
    ## 23473        850            MQ      3890                        NA
    ## 24077       1240            WN      2967                        NA
    ## 24078       2305            OO      5978  N730SK                NA
    ## 24367       1125            WN       729                        NA
    ## 24368       1645            MQ      3374                        NA
    ## 24369       1045            AA       511                        NA
    ## 24370       2015            AA      1566                        NA
    ## 24371       1900            AA      1646                        NA
    ## 24372        840            AA      1958                        NA
    ## 24373       1150            AA      2024                        NA
    ## 24374       2015            AA      2402                        NA
    ## 24375       1520            MQ      3446                        NA
    ## 24376       1025            AA       421                        NA
    ## 24377       1615            AA       559                        NA
    ## 24378       1655            AA      1053                        NA
    ## 24379        900            AA      1109                        NA
    ## 24380       1945            AA      1113                        NA
    ## 24381       1625            AA      1563                        NA
    ## 24682       1851            DL      1262  N961DL                NA
    ## 24683       1515            AA       422                        NA
    ## 24684       1900            AA      1646                        NA
    ## 24685       1500            DL      1135  N920DL                NA
    ## 24686       1335            AA       668                        NA
    ## 24687       1625            AA      1563                        NA
    ## 24992       2331            OH      5231  N398CA                NA
    ## 24993       2016            UA      1238                        NA
    ## 25218       1755            9E      5747                        NA
    ## 25219       1530            9E      5742                        NA
    ## 25792       2135            WN      1399                        NA
    ## 25793       1825            MQ      3400                        NA
    ## 25794       1500            MQ      3503                        NA
    ## 25795       1845            AA       346                        NA
    ## 25796       1235            AA       368                        NA
    ## 25797       2055            AA      2059                        NA
    ## 25798       2025            WN       581                        NA
    ## 25799       1705            MQ      3375                        NA
    ## 25800       1335            MQ      3412                        NA
    ## 25801       1905            AA       563                        NA
    ## 25802       1100            AA      1199                        NA
    ## 25803       2030            AA      2293                        NA
    ## 26088       1140            OH      5124  N689CA                NA
    ## 26667       1230            AA       813                        NA
    ## 26668       1025            AA       421                        NA
    ## 26959       1140            OH      5124  N695CA                NA
    ## 26960       1755            9E      5747                        NA
    ## 27448       2331            OH      5231  N398CA                NA
    ## 27732        948            OH      5534                        NA
    ## 27733       1820            OO      2933  N496CA                NA
    ## 27734       1515            AA       422                        NA
    ## 27735       2100            AA      2282                        NA
    ## 27736       1600            OO      2932  N496CA                NA
    ## 27737       1905            AA       526                        NA
    ## 27738       1335            AA       615                        NA
    ## 28004       1950            AA       427                        NA
    ## 28005       1940            AA       564                        NA
    ## 28006       1805            AA       608                        NA
    ## 28007       1630            AA      1128                        NA
    ## 28008       1740            AA      1619                        NA
    ## 28009       1850            AA      1626                        NA
    ## 28010       1950            AA      1703                        NA
    ## 28011       2100            AA      2282                        NA
    ## 28012       2241            OO      5978  N778SK                NA
    ## 28013       2025            AA       470                        NA
    ## 28014       2215            AA       502                        NA
    ## 28015       1905            AA       526                        NA
    ## 28016       1450            AA       721                        NA
    ## 28017       2255            AA       785                        NA
    ## 28018       2100            AA      1051                        NA
    ## 28019       2350            AA      1274                        NA
    ## 28020       2115            AA      1731                        NA
    ## 28021       2140            AA      1778                        NA
    ## 28022       1705            AA      1784                        NA
    ## 28023       1945            AA      2048                        NA
    ## 28024       2355            AA      2246                        NA
    ## 28025       1955            AA      2293                        NA
    ## 28026       1555            AA      2378                        NA
    ## 28027         10            AA      2491                        NA
    ## 28275        950            AA       304                        NA
    ## 28276       1515            AA       422                        NA
    ## 28277       1950            AA       427                        NA
    ## 28278        825            AA       461                        NA
    ## 28279       1940            AA       564                        NA
    ## 28280        815            AA       579                        NA
    ## 28281       1805            AA       608                        NA
    ## 28282       1220            AA       682                        NA
    ## 28283        700            AA       704                        NA
    ## 28284       1155            AA       813                        NA
    ## 28285       1605            AA      1023                        NA
    ## 28286        715            AA      1038                        NA
    ## 28287       1100            AA      1119                        NA
    ## 28288        840            AA      1163                        NA
    ## 28289       1610            AA      1198                        NA
    ## 28290       1355            AA      1464                        NA
    ## 28291        925            AA      1465                        NA
    ## 28292        835            AA      1614                        NA
    ## 28293       1740            AA      1619                        NA
    ## 28294       1850            AA      1626                        NA
    ## 28295       1315            AA      1656                        NA
    ## 28296       1950            AA      1703                        NA
    ## 28297       1040            AA      1759                        NA
    ## 28298       1405            AA      1888                        NA
    ## 28299       2100            AA      2282                        NA
    ## 28300       1625            AA       321                        NA
    ## 28301       2025            AA       470                        NA
    ## 28302       2215            AA       502                        NA
    ## 28303       1905            AA       526                        NA
    ## 28304       1335            AA       615                        NA
    ## 28305       1450            AA       721                        NA
    ## 28306       1725            AA      1024                        NA
    ## 28307       2100            AA      1051                        NA
    ## 28308        915            AA      1109                        NA
    ## 28309       1135            AA      1182                        NA
    ## 28310       1040            AA      1199                        NA
    ## 28311       1800            AA      1308                        NA
    ## 28312       1130            AA      1477                        NA
    ## 28313       1225            AA      1717                        NA
    ## 28314       2115            AA      1731                        NA
    ## 28315       2140            AA      1778                        NA
    ## 28316       1705            AA      1784                        NA
    ## 28317       1340            AA      1810                        NA
    ## 28318       1945            AA      2048                        NA
    ## 28319       1955            AA      2293                        NA
    ## 28320       1555            AA      2378                        NA
    ## 28321         10            AA      2491                        NA
    ## 28585        756            9E      5743                        NA
    ## 28586        950            AA       304                        NA
    ## 28587       1515            AA       422                        NA
    ## 28588        825            AA       461                        NA
    ## 28589       1940            AA       564                        NA
    ## 28590       1220            AA       682                        NA
    ## 28591        700            AA       704                        NA
    ## 28592       1155            AA       813                        NA
    ## 28593        715            AA      1038                        NA
    ## 28594       1100            AA      1119                        NA
    ## 28595       1630            AA      1128                        NA
    ## 28596        840            AA      1163                        NA
    ## 28597       1355            AA      1464                        NA
    ## 28598        835            AA      1614                        NA
    ## 28599       1740            AA      1619                        NA
    ## 28600       1850            AA      1626                        NA
    ## 28601       1315            AA      1656                        NA
    ## 28602       1405            AA      1888                        NA
    ## 28603       2100            AA      2282                        NA
    ## 28604       2241            OO      5978  N748SK                NA
    ## 28605       1625            AA       321                        NA
    ## 28606       1905            AA       526                        NA
    ## 28607        915            AA      1109                        NA
    ## 28608       1135            AA      1182                        NA
    ## 28609       1040            AA      1199                        NA
    ## 28610       2350            AA      1274                        NA
    ## 28611       1800            AA      1308                        NA
    ## 28612       1130            AA      1477                        NA
    ## 28613       1225            AA      1717                        NA
    ## 28614       1705            AA      1784                        NA
    ## 28615       1945            AA      2048                        NA
    ## 28616       1555            AA      2378                        NA
    ## 28617         10            AA      2491                        NA
    ## 28898        950            AA       304                        NA
    ## 28899       1940            AA       564                        NA
    ## 28900        700            AA       704                        NA
    ## 28901       1100            AA      1119                        NA
    ## 28902       1630            AA      1128                        NA
    ## 28903       1355            AA      1464                        NA
    ## 28904        835            AA      1614                        NA
    ## 28905       1850            AA      1626                        NA
    ## 28906       1405            AA      1888                        NA
    ## 28907       1625            AA       321                        NA
    ## 28908       1450            AA       721                        NA
    ## 28909       2255            AA       785                        NA
    ## 28910        915            AA      1109                        NA
    ## 28911       1040            AA      1199                        NA
    ## 28912       1225            AA      1717                        NA
    ## 28913       1705            AA      1784                        NA
    ## 28914         10            AA      2491                        NA
    ## 29131        700            AA       704                        NA
    ## 29132       1155            AA       813                        NA
    ## 29133       1100            AA      1119                        NA
    ## 29134        835            AA      1614                        NA
    ## 29135       1600            OO      2932  N710BR                NA
    ## 29136       1625            AA       321                        NA
    ## 29137        915            AA      1109                        NA
    ## 29138       1800            AA      1308                        NA
    ## 29139       1225            AA      1717                        NA
    ## 29417       1315            OO      2931  N710BR                NA
    ## 29418       1705            CO      1602                        NA
    ## 29419       2159            XE       318                        NA
    ## 29420        337            YV      2892  N906FJ                NA
    ## 29421       1526            CO      1541                        NA
    ## 29715        619            YV      2819  N906FJ                NA
    ## 30002       1015            MQ      3364                        NA
    ## 30003        845            MQ      3890                        NA
    ## 30297       1503            XE       308                        NA
    ## 30589       2145            MQ      3443                        NA
    ## 30590       2100            AA      2282                        NA
    ## 30591       2020            MQ      3824                        NA
    ## 30592       2255            AA       785                        NA
    ## 30593       2115            AA      1731                        NA
    ## 30890        700            AA       704                        NA
    ## 31113       1315            AA      1656                        NA
    ## 31114       1130            AA      1477                        NA
    ## 32270        800            MQ      3366                        NA
    ## 32271       2100            AA      2282                        NA
    ## 32272        640            MQ      3362                        NA
    ## 32273       2255            AA       785                        NA
    ## 32566        915            UA       657                        NA
    ## 32567       1645            MQ      3374                        NA
    ## 32568       2000            MQ      3441                        NA
    ## 32569       1707            UA       374                        NA
    ## 32570       1840            MQ      3422                        NA
    ## 32571       1525            MQ      3446                        NA
    ## 32863       1825            MQ      3400                        NA
    ## 32864       2000            MQ      3441                        NA
    ## 32865       1940            AA       564                        NA
    ## 32866       1805            AA       608                        NA
    ## 32867       2332            YV      7260  N508MJ                NA
    ## 32868       1705            MQ      3375                        NA
    ## 32869       1840            MQ      3422                        NA
    ## 32870         10            AA      2491                        NA
    ## 33095        838            YV      7258  N508MJ                NA
    ## 33096       1450            AA      1545                        NA
    ## 33674       1602            YV      7276  N508MJ                NA
    ## 33675       1100            YV      7156  N508MJ                NA
    ## 34259       2101            YV      7228  N502MJ                NA
    ## 34260       1345            9E      5745                        NA
    ## 34261       1120            9E      5744                        NA
    ## 34864       1825            MQ      3400                        NA
    ## 34865       1220            AA       682                        NA
    ## 34866       1355            AA      1464                        NA
    ## 34867       1705            MQ      3375                        NA
    ## 34868       1625            AA       321                        NA
    ## 35097        915            AA      1109                        NA
    ## 35386        835            AA      1614                        NA
    ## 35387       1625            AA       321                        NA
    ## 35686       1950            WN      3312                        NA
    ## 35687       1220            MQ      3488                        NA
    ## 35688       1950            AA      1703                        NA
    ## 35689       1100            MQ      3355                        NA
    ## 35690       1725            AA      1024                        NA
    ## 35691       1842            CO      1603                        NA
    ## 35980       1040            WN      1087                        NA
    ## 35981       1645            MQ      3374                        NA
    ## 35982       1220            MQ      3488                        NA
    ## 35983       1740            AA      1619                        NA
    ## 35984        756            CO      1573                        NA
    ## 35985        915            WN      1092                        NA
    ## 35986       1100            MQ      3355                        NA
    ## 35987       1525            MQ      3446                        NA
    ## 35988       1555            AA      2378                        NA
    ## 36287       2118            OH      5056  N678CA                NA
    ## 36288       1515            AA       422                        NA
    ## 36289       1205            AA      2431                        NA
    ## 36290       2318            OH      5231  N692CA                NA
    ## 36291       1025            AA       421                        NA
    ## 36292       1335            AA       615                        NA
    ## 36596        915            UA       346  N924UA                NA
    ## 36597       1645            MQ      3374                        NA
    ## 36598       1525            MQ      3446                        NA
    ## 36904        815            AA       579                        NA
    ## 36905       2146            B6      1068  N249JB                NA
    ## 37148       1650            F9       217  N932FR                NA
    ## 37149       1225            AA      1717                        NA
    ## 37436       1027            YV      2652  N934FJ                NA
    ## 37437       1220            AA       682                        NA
    ## 37438       1605            AA      1023                        NA
    ## 37439       1340            AA      1810                        NA
    ## 37440         10            AA      2491                        NA
    ## 38033       1100            AA      1119                        NA
    ## 38034       1247            CO      1758                        NA
    ## 38035       1220            WN      2076                        NA
    ## 38036       2115            AA      1731                        NA
    ## 38335       1230            WN      3765                        NA
    ## 38336        800            MQ      3366                        NA
    ## 38337       1220            MQ      3488                        NA
    ## 38338       1205            AA      2431                        NA
    ## 38339       1410            WN      3322                        NA
    ## 38340       1100            MQ      3355                        NA
    ## 38341        640            MQ      3362                        NA
    ## 38342       1025            AA       421                        NA
    ## 38650        815            WN       958                        NA
    ## 38958       1027            YV      2688  N904FJ                NA
    ## 38959       2159            XE       318                        NA
    ## 39204       1805            AA       608                        NA
    ## 39495       1825            MQ      3400                        NA
    ## 39496       1705            MQ      3375                        NA
    ## 40407       2140            WN      3685                        NA
    ## 40408        715            AA      1038                        NA
    ## 40716        619            YV      2819  N929LR                NA
    ## 41526       1820            OO      2933  N403SW                NA
    ## 41527       1605            AA      1023                        NA
    ## 41528       1600            OO      2932  N403SW                NA
    ## 41529       1340            AA      1810                        NA
    ## 41806        843            YV      7258  N502MJ                NA
    ## 41807       1500            MQ      3503                        NA
    ## 41808       1335            MQ      3412                        NA
    ## 42086       1749            YV      2710  N933LR                NA
    ## 42087       1645            MQ      3374                        NA
    ## 42088       1825            MQ      3400                        NA
    ## 42089       2000            MQ      3441                        NA
    ## 42090       1220            MQ      3488                        NA
    ## 42091       1500            MQ      3503                        NA
    ## 42092       1515            AA       422                        NA
    ## 42093       1630            AA      1128                        NA
    ## 42094       1740            AA      1619                        NA
    ## 42095       1405            AA      1888                        NA
    ## 42096       2020            AA      1946                        NA
    ## 42097       2100            AA      2282                        NA
    ## 42098       1255            WN      1609                        NA
    ## 42099       1632            YV      2888  N933LR                NA
    ## 42100       2228            OH      5203  N689CA                NA
    ## 42101       1100            MQ      3355                        NA
    ## 42102       1705            MQ      3375                        NA
    ## 42103       1335            MQ      3412                        NA
    ## 42104       1840            MQ      3422                        NA
    ## 42105       1525            MQ      3446                        NA
    ## 42106       2215            AA       502                        NA
    ## 42107       1335            AA       615                        NA
    ## 42108       1450            AA       721                        NA
    ## 42109       1840            AA      1188                        NA
    ## 42110       2350            AA      1274                        NA
    ## 42111       1705            AA      1784                        NA
    ## 42112       1945            AA      2048                        NA
    ## 42113       1555            AA      2378                        NA
    ## 42114       2311            CO       806                        NA
    ## 42413        915            UA       346  N925UA                NA
    ## 42414       2000            MQ      3441                        NA
    ## 42415        815            AA       579                        NA
    ## 42416       1605            AA      1023                        NA
    ## 42417        715            AA      1038                        NA
    ## 42418       1054            CO       852                        NA
    ## 42419       1840            MQ      3422                        NA
    ## 42420       2215            AA       502                        NA
    ## 42726       1155            AA       813                        NA
    ## 42727       1725            AA      1024                        NA
    ## 42728       1800            AA      1308                        NA
    ## 43034        845            OO      6060  N715SK                NA
    ## 43035        815            AA       579                        NA
    ## 43036       1355            AA      1464                        NA
    ## 43037       1955            AA      2293                        NA
    ## 43281       2020            AA      1946                        NA
    ## 43282       1840            AA      1188                        NA
    ## 43569       1645            MQ      3374                        NA
    ## 43570       1220            MQ      3488                        NA
    ## 43571       2020            AA      1946                        NA
    ## 43572       1100            MQ      3355                        NA
    ## 43573       1525            MQ      3446                        NA
    ## 43574       1840            AA      1188                        NA
    ## 43879       1825            MQ      3400                        NA
    ## 43880       1705            MQ      3375                        NA
    ## 44179       2100            AA      2282                        NA
    ## 44180       1905            AA       526                        NA
    ## 44485       1034            YV      2604  N906FJ                NA
    ## 44797        344            YV      2892  N956LR                NA
    ## 44798       1123            OH      5507  N658CA                NA
    ## 45113       1034            YV      2604  N956LR                NA
    ## 45114       1952            YV      7035  N522LR                NA
    ## 45363       1010            YV      7027  N522LR                NA
    ## 45364        905            US       365                        NA
    ## 45365       1520            AA       422                        NA
    ## 45366       1335            AA       615                        NA
    ## 45664       1810            AA       608                        NA
    ## 45665       1950            AA      2293                        NA
    ## 45970       1815            OO      2933  N709BR                NA
    ## 45971       1645            MQ      3374                        NA
    ## 45972       1220            MQ      3488                        NA
    ## 45973       2020            AA      1946                        NA
    ## 45974       2100            AA      2282                        NA
    ## 45975       1600            OO      2932  N709BR                NA
    ## 45976       1100            MQ      3355                        NA
    ## 45977       1525            MQ      3446                        NA
    ## 45978       1905            AA       526                        NA
    ## 45979       1840            AA      1188                        NA
    ## 46289       1015            MQ      3364                        NA
    ## 46290       1555            AA      1023                        NA
    ## 46291       2114            YV      7281  N522LR                NA
    ## 46292        840            MQ      3890                        NA
    ## 46293       2220            AA       502                        NA
    ## 46603       1111            YV      7062  N522LR                NA
    ## 46604       1015            MQ      3364                        NA
    ## 46605       1645            MQ      3374                        NA
    ## 46606       1525            MQ      3446                        NA
    ## 46607        840            MQ      3890                        NA
    ## 46921        622            YV      2819  N956LR                NA
    ## 46922       1015            MQ      3364                        NA
    ## 46923        840            MQ      3890                        NA
    ## 47239       1555            AA      1023                        NA
    ## 47240       1345            AA      1810                        NA
    ## 47797       2314            B6      1069  N258JB                NA
    ## 48108       2105            WN      1922                        NA
    ## 48109       1147            B6      1060  N258JB                NA
    ## 48110       2146            B6      1068  N183JB                NA
    ## 48111       2314            B6      1069  N178JB                NA
    ## 48414       2000            MQ      3441                        NA
    ## 48415       1500            MQ      3503                        NA
    ## 48416       1520            AA       422                        NA
    ## 48417       1635            AA      1128                        NA
    ## 48418       1745            AA      1619                        NA
    ## 48419       1850            AA      1626                        NA
    ## 48420       1335            MQ      3412                        NA
    ## 48421       1835            MQ      3422                        NA
    ## 48422       1335            AA       615                        NA
    ## 48423       1450            AA       721                        NA
    ## 48424       1705            AA      1784                        NA
    ## 48425       1600            AA      2378                        NA
    ## 48738       1220            MQ      3488                        NA
    ## 48739       1100            MQ      3355                        NA
    ## 49047       1645            MQ      3374                        NA
    ## 49048       2145            MQ      3443                        NA
    ## 49049       1100            AA      1119                        NA
    ## 49050       1315            AA      1656                        NA
    ## 49051       1525            MQ      3446                        NA
    ## 49052       2020            MQ      3824                        NA
    ## 49053       1450            AA       721                        NA
    ## 49054       1225            AA      1717                        NA
    ## 49368        947            XE       311                        NA
    ## 49369       1551            YV      7273  N521LR                NA
    ## 49370        831            XE       520                        NA
    ## 49371       1058            YV      7273  N521LR                NA
    ## 49922       1551            YV      7273  N515MJ                NA
    ## 49923        805            AA       579                        NA
    ## 49924       1410            AA      1888                        NA
    ## 49925       1802            B6      1062  N247JB                NA
    ## 49926       1058            YV      7273  N515MJ                NA
    ## 49927       1225            AA      1717                        NA
    ## 49928       1345            AA      1810                        NA
    ## 49929       2314            B6      1069  N192JB                NA
    ## 50556       2000            MQ      3441                        NA
    ## 50557       1835            MQ      3422                        NA
    ## 51176       1815            OO      2933  N698BR                NA
    ## 51177       1753            OO      6131  N745SK                NA
    ## 51178       1645            MQ      3374                        NA
    ## 51179       1745            9E      4702                        NA
    ## 51180       1720            CO      1602                        NA
    ## 51181       1600            OO      2932  N698BR                NA
    ## 51182       1612            OO      6130  N745SK                NA
    ## 51183       1525            MQ      3446                        NA
    ## 51184       1320            9E      4703                        NA
    ## 51185       1535            CO      1541                        NA
    ## 51501       1128            CO      1572                        NA
    ## 51502        939            CO      1583                        NA
    ## 51756       1850            AA      1626                        NA
    ## 51757       2025            AA       470                        NA
    ## 51758       2255            AA       785                        NA
    ## 51759       2350            AA      1274                        NA
    ## 51760       2110            AA      1731                        NA
    ## 51761       1940            AA      2048                        NA
    ## 52055        950            AA       304                        NA
    ## 52056        700            AA       704                        NA
    ## 52057        715            AA      1038                        NA
    ## 52058        840            AA      1163                        NA
    ## 52371       1355            AA      1464                        NA
    ## 52372       1035            AA      1199                        NA
    ## 52680       1355            AA      1464                        NA
    ## 52681       1935            AA      1703                        NA
    ## 52682       2114            YV      7281  N508MJ                NA
    ## 52683       1730            AA      1024                        NA
    ## 52684       1035            AA      1199                        NA
    ## 52981       1111            YV      7062  N508MJ                NA
    ## 53738       1810            AA       608                        NA
    ## 54024       2159            XE       318                        NA
    ## 54025       1035            AA      1199                        NA
    ## 54621       1230            OO      6202  N754SK                NA
    ## 54622       1220            AA       682                        NA
    ## 54623       2035            B6      1401  N229JB                NA
    ## 54624       2110            AA      1731                        NA
    ## 54906        715            AA      1038                        NA
    ## 54907       1832            B6      1402  N229JB                NA
    ## 54908       1405            B6      1402  N229JB                NA
    ## 55207        622            YV      2819  N909FJ                NA
    ## 55208       1945            AA       564                        NA
    ## 55209       2350            WN       195                        NA
    ## 55210         25            AA      2491                        NA
    ## 55511        815            WN       958                        NA
    ## 55512       1520            AA       422                        NA
    ## 55513       1335            AA       615                        NA
    ## 55769       1801            OO      6131  N766SK                NA
    ## 56049        805            AA       579                        NA
    ## 56050       1914            DL      1089  N944DL                NA
    ## 56051       2058            OH      5203                        NA
    ## 56052       1345            AA      1810                        NA
    ## 56053       2037            DL      1553  N987DL                NA
    ## 56054       1508            DL      1807  N960DL                NA
    ## 56353       1133            OH      5124                        NA
    ## 56354        851            DL       728  N943DL                NA
    ## 57559       2140            WN      3685                        NA
    ## 57560       2320            UA       526                        NA
    ## 57813        900            UA       657                        NA
    ## 57814       1145            AA       813                        NA
    ## 57815       1800            AA      1308                        NA
    ## 58095       1205            AA      2431                        NA
    ## 58096       2322            UA       526                        NA
    ## 58097       1020            AA       421                        NA
    ## 58391        913            UA       373                        NA
    ## 58392       1128            CO      1572                        NA
    ## 58393        939            CO      1583                        NA
    ## 58685       1415            OH      5574  N371CA                NA
    ## 58686        915            AA      1465                        NA
    ## 58687       1020            CO       640                        NA
    ## 58688       1002            OH      5309  N371CA                NA
    ## 58689        828            CO      1841                        NA
    ## 58979       1420            WN      1609                        NA
    ## 58980       2146            B6      1068  N289JB                NA
    ## 58981       1540            WN       435                        NA
    ## 58982       2058            OH      5203                        NA
    ## 58983       2320            UA       526                        NA
    ## 58984       1602            B6      1065  N289JB                NA
    ## 58985       2314            B6      1069  N266JB                NA
    ## 59276       1420            WN      1609                        NA
    ## 59277        902            UA       657                        NA
    ## 59278       1147            B6      1060  N266JB                NA
    ## 59279       1900            B6      1417  N216JB                NA
    ## 59280       1230            CO      1758                        NA
    ## 59281       1540            WN       435                        NA
    ## 59282       1500            B6      1416  N247JB                NA
    ## 59283       1034            CO       350                        NA
    ## 59579       1420            WN      1609                        NA
    ## 59580       1020            CO       640                        NA
    ## 59581       1540            WN       435                        NA
    ## 59582        828            CO      1841                        NA
    ## 59832       1640            WN      2216                        NA
    ## 59833       1106            CO      1572                        NA
    ## 59834       1800            WN      1170                        NA
    ## 59835        934            CO      1583                        NA
    ## 60112       1647            YV      7325  N508MJ                NA
    ## 60113       1558            OH      5202  N548CA                NA
    ## 60114       1802            B6      1062  N190JB                NA
    ## 60115       2146            B6      1068  N266JB                NA
    ## 60116       2058            OH      5203                        NA
    ## 60117       2314            B6      1069  N284JB                NA
    ## 60413       1224            B6      1061  N239JB                NA
    ## 60708       1520            AA       422                        NA
    ## 60709       1335            AA       615                        NA
    ## 60999       1555            AA      1023                        NA
    ## 61000       1410            AA      1888                        NA
    ## 61001       2100            AA      2282                        NA
    ## 61002       2025            AA       470                        NA
    ## 61003       1225            AA      1717                        NA
    ## 61004       2110            AA      1731                        NA
    ## 61005       1345            AA      1810                        NA
    ## 61301        700            AA       704                        NA
    ## 61302       1900            B6      1417  N273JB                NA
    ## 61303       1500            B6      1416  N239JB                NA
    ## 61599       2140            WN      3685                        NA
    ## 61842       1545            OH      5202  N549CA                NA
    ## 61843       1355            AA      1464                        NA
    ## 61844       2146            B6      1068  N183JB                NA
    ## 61845       2058            OH      5203  N549CA                NA
    ## 61846        820            WN       392                        NA
    ## 61847       1035            AA      1199                        NA
    ## 61848       2314            B6      1069  N288JB                NA
    ## 62125       2020            AA      1946                        NA
    ## 62126       1840            AA      1188                        NA
    ## 62413        950            AA       304                        NA
    ## 62414        840            AA      1163                        NA
    ## 62415       1355            AA      1464                        NA
    ## 62416       1445            AA      1545                        NA
    ## 62417       2110            AA      1731                        NA
    ## 62418       1940            AA      2048                        NA
    ## 62707       1545            CO      1706                        NA
    ## 62708       1341            CO      1540                        NA
    ## 62709       1355            CO       241                        NA
    ## 62710       1159            CO       741                        NA
    ## 63000       2045            YV      7228  N522LR                NA
    ## 63001       1635            AA      1128                        NA
    ## 63002       1450            AA       721                        NA
    ## 63294       2146            B6      1068  N197JB                NA
    ## 63295       2030            B6      1401  N239JB                NA
    ## 63296       2314            B6      1069  N206JB                NA
    ## 63589       1855            B6      1417  N279JB                NA
    ## 63590       1405            B6      1402  N239JB                NA
    ## 64113       1355            AA      1464                        NA
    ## 64114       1205            AA      2431                        NA
    ## 64115       2118            YV      7281  N506MJ                NA
    ## 64116       1020            AA       421                        NA
    ## 64117       1035            AA      1199                        NA
    ## 64404       1057            YV      7062  N506MJ                NA
    ## 64405       1850            AA      1626                        NA
    ## 64406       1802            B6      1062  N267JB                NA
    ## 64407       2058            OH      5203                        NA
    ## 64408       1705            AA      1784                        NA
    ## 64409       2314            B6      1069  N193JB                NA
    ## 64698       1810            AA       608                        NA
    ## 64699       1832            B6      1402  N294JB                NA
    ## 64700       1445            AA      1545                        NA
    ## 64701       1814            B6      1401  N178JB                NA
    ## 64990        622            YV      2819  N904FJ                NA
    ## 64991       2359            AA      2246                        NA
    ## 65282       1745            AA      1619                        NA
    ## 65283       2059            OH      5203                        NA
    ## 65284       1600            AA      2378                        NA
    ## 65285       2314            B6      1069  N184JB                NA
    ## 65571       1133            OH      5124                        NA
    ## 65572       2120            EV      4221  N659CA                NA
    ## 65573       1315            AA      1656                        NA
    ## 65574       1205            AA      2431                        NA
    ## 65575       1147            B6      1060  N184JB                NA
    ## 65576       1802            B6      1062  N266JB                NA
    ## 65577       1720            CO      1602                        NA
    ## 65578       1729            EV      4221  N659CA                NA
    ## 65579       1020            AA       421                        NA
    ## 65580       1130            AA      1477                        NA
    ## 65581       2314            B6      1069  N274JB                NA
    ## 65582       1535            CO      1541                        NA
    ## 65821       1100            AA      1119                        NA
    ## 66084       1935            AA      1703                        NA
    ## 66085       1730            AA      1024                        NA
    ## 66664       1555            AA      1023                        NA
    ## 66665       1345            AA      1810                        NA
    ## 66951       1230            WN      3765                        NA
    ## 66952       1555            AA      1023                        NA
    ## 66953       1345            AA      1810                        NA
    ## 67246       1128            CO      1572                        NA
    ## 67247        939            CO      1583                        NA
    ## 67541       1646            XE       312                        NA
    ## 67542       1433            XE       308                        NA
    ## 67767       1520            AA       422                        NA
    ## 67768       1335            AA       615                        NA
    ## 68034       2242            XE      3033                        NA
    ## 68316       1057            YV      7062  N502MJ                NA
    ## 68317       2025            YV      2621  N925FJ                NA
    ## 68590       1034            YV      2604  N920FJ                NA
    ## 68591       1516            DL       668   N3765                NA
    ## 68592       1555            AA      1023                        NA
    ## 68593       1119            DL      1685  N974DL                NA
    ## 68594       2220            AA       502                        NA
    ## 68595       1345            AA      1810                        NA
    ## 68874       1852            OH      5280                        NA
    ## 68875        805            AA       579                        NA
    ## 69159        900            UA       373                        NA
    ## 69160       1100            AA      1119                        NA
    ## 69161        915            AA      1109                        NA
    ## 69446        950            AA       304                        NA
    ## 69447       1850            AA      1626                        NA
    ## 69448       1705            AA      1784                        NA
    ## 69854       1945            AA       564                        NA
    ## 69855       1620            AA       321                        NA
    ## 69856       2220            AA       502                        NA
    ## 70120       2018            XE       612                        NA
    ## 70121        805            AA       579                        NA
    ## 70122       1915            XE       608                        NA
    ## 70123       1130            AA      1477                        NA
    ## 71169       1245            WN      1382                        NA
    ## 71170       2015            AA      1703                        NA
    ## 71171       1750            AA      1024                        NA
    ## 71361       1615            OH      6418                        NA
    ## 71362       2109            OH      6419                        NA
    ## 71607       1340            WN       678                        NA
    ## 71868       2121            9E      2009   9149E                NA
    ## 71869       1730            9E      2009   9149E                NA
    ## 72123        930            WN      2090                        NA
    ## 72873       1945            WN       340                        NA
    ## 72874        915            WN       343                        NA
    ## 72875       1720            WN       401                        NA
    ## 72876       1450            WN       411                        NA
    ## 72877       1155            WN       777                        NA
    ## 72878       2110            WN       943                        NA
    ## 72879       1405            WN      2136                        NA
    ## 72880       1349            CO      1540                        NA
    ## 72881       1230            CO      1758                        NA
    ## 72882       1930            CO       620                        NA
    ## 72883       1550            CO      1706                        NA
    ## 72884       1730            CO      1602                        NA
    ## 72885        915            CO      1786                        NA
    ## 72886       1005            CO       640                        NA
    ## 72887       1450            WN       290                        NA
    ## 72888       1715            WN       520                        NA
    ## 72889       1130            WN       933                        NA
    ## 72890       1900            WN      1083                        NA
    ## 72891       2125            WN      1137                        NA
    ## 72892       1530            WN      1612                        NA
    ## 72893       2031            YV      2621  N917FJ                NA
    ## 72894       1916            YV      2889  N939LR                NA
    ## 72895       1534            CO      1541                        NA
    ## 72896       1631            CO       341                        NA
    ## 72897       1915            CO      1603                        NA
    ## 72898       2340            CO      1561                        NA
    ## 72899       2055            CO      1441                        NA
    ## 72900       2156            CO      1533                        NA
    ## 72901       1201            CO       741                        NA
    ## 73050       1335            WN      2265                        NA
    ## 73051       1610            WN      2277                        NA
    ## 73052       1000            WN      2751                        NA
    ## 73053       2000            WN      2070                        NA
    ## 73054        830            WN      2121                        NA
    ## 73055       1300            WN      2347                        NA
    ## 73056       1710            WN      2494                        NA
    ## 73057       1625            WN      2780                        NA
    ## 73058       1034            YV      2604  N917FJ                NA
    ## 73059       1135            YV      2794  N939LR                NA
    ## 73060       1205            AA       540                        NA
    ## 73061       1740            AA       641                        NA
    ## 73062       1525            AA      1607                        NA
    ## 73063       1429            AA      2074                        NA
    ## 73064        637            CO       240                        NA
    ## 73065        913            CO      1786                        NA
    ## 73066       1548            CO      1706                        NA
    ## 73067        750            CO      1573                        NA
    ## 73068       1005            CO       640                        NA
    ## 73069       1347            CO      1540                        NA
    ## 73070       1126            CO      1572                        NA
    ## 73071       1723            CO      1602                        NA
    ## 73072       1220            WN       431                        NA
    ## 73073        950            WN      1143                        NA
    ## 73074       1705            WN      1496                        NA
    ## 73075       1425            WN      1691                        NA
    ## 73076       1120            WN      1874                        NA
    ## 73077       1955            WN      2061                        NA
    ## 73078       1445            WN      2296                        NA
    ## 73079        915            WN      2927                        NA
    ## 73080       1745            WN      2465                        NA
    ## 73081       1020            AA       405                        NA
    ## 73082       1340            AA      1391                        NA
    ## 73083       1550            AA      2038                        NA
    ## 73084       1950            AA      2293                        NA
    ## 73085       2151            CO      1533                        NA
    ## 73086       1156            CO       741                        NA
    ## 73087        934            CO      1583                        NA
    ## 73088       1359            CO       241                        NA
    ## 73089       1855            CO      1603                        NA
    ## 73090       1636            CO       341                        NA
    ## 73091       1534            CO      1541                        NA
    ## 73092       2050            CO      1441                        NA
    ## 73307       1945            WN       340                        NA
    ## 73308        915            WN       343                        NA
    ## 73309       1720            WN       401                        NA
    ## 73310       1450            WN       411                        NA
    ## 73311       1155            WN       777                        NA
    ## 73312       2110            WN       943                        NA
    ## 73313       1005            CO       640                        NA
    ## 73314       1349            CO      1540                        NA
    ## 73315        750            CO      1573                        NA
    ## 73316       1228            CO      1758                        NA
    ## 73317       1930            CO       620                        NA
    ## 73318        825            CO       340                        NA
    ## 73319       1550            CO      1706                        NA
    ## 73320       1126            CO      1572                        NA
    ## 73321       1720            CO      1602                        NA
    ## 73322        913            CO      1786                        NA
    ## 73323       1450            WN       290                        NA
    ## 73324       1715            WN       520                        NA
    ## 73325       1130            WN       933                        NA
    ## 73326       1900            WN      1083                        NA
    ## 73327       2125            WN      1137                        NA
    ## 73328       1156            CO       741                        NA
    ## 73329       2156            CO      1533                        NA
    ## 73330       1534            CO      1541                        NA
    ## 73331       1059            CO       441                        NA
    ## 73332       1631            CO       341                        NA
    ## 73333       1935            CO      1603                        NA
    ## 73334       2340            CO      1561                        NA
    ## 73335       2055            CO      1441                        NA
    ## 73336        934            CO      1583                        NA
    ## 73337       1404            CO       241                        NA
    ## 73584       1945            WN       340                        NA
    ## 73585        915            WN       343                        NA
    ## 73586       1720            WN       401                        NA
    ## 73587       1450            WN       411                        NA
    ## 73588       1155            WN       777                        NA
    ## 73589       2110            WN       943                        NA
    ## 73590        730            WN      1714                        NA
    ## 73591        637            CO       240                        NA
    ## 73592        845            WN        79                        NA
    ## 73593       1450            WN       290                        NA
    ## 73594       1715            WN       520                        NA
    ## 73595        745            WN       565                        NA
    ## 73596       1130            WN       933                        NA
    ## 73597       1900            WN      1083                        NA
    ## 73598       2125            WN      1137                        NA
    ## 73599        939            CO      1583                        NA
    ## 73848       1945            WN       340                        NA
    ## 73849       2110            WN       943                        NA
    ## 73850        730            WN      1714                        NA
    ## 73851        845            WN        79                        NA
    ## 73852        745            WN       565                        NA
    ## 73853       2125            WN      1137                        NA
    ## 74105       1945            WN       340                        NA
    ## 74106       2110            WN       943                        NA
    ## 74107        730            WN      1714                        NA
    ## 74108        845            WN        79                        NA
    ## 74109        745            WN       565                        NA
    ## 74110       2125            WN      1137                        NA
    ## 74367       1945            WN       340                        NA
    ## 74368       2110            WN       943                        NA
    ## 74369        730            WN      1714                        NA
    ## 74370        845            WN        79                        NA
    ## 74371        745            WN       565                        NA
    ## 74372       2125            WN      1137                        NA
    ## 74634       1245            WN      1382                        NA
    ## 74635       1130            WN       933                        NA
    ## 76365       1800            B6      1062  N267JB                NA
    ## 76366       2333            B6      1069  N187JB                NA
    ## 77574       1240            WN      1382  N510SW                NA
    ## 77925       1415            AA      1810  N525AA                NA
    ## 78095       1645            AA      1023  N529AA                NA
    ## 78096       1915            AA      1160  N423AA                NA
    ## 78097       1730            AA      1556  N423AA                NA
    ## 79046       1556            OO      5847  N764SK                NA
    ## 79047       2244            OO      5978  N763SK                NA
    ## 79300       1645            AA      1023  N248AA                NA
    ## 79301       1415            AA      1810  N248AA                NA
    ## 79562       1805            9E      5747                        NA
    ## 79563       1540            9E      5737                        NA
    ## 79923       1617            YV      7123  N505MJ                NA
    ## 80013       2101            YV      7228  N505MJ                NA
    ## 80772       1645            AA      1023  N455AA                NA
    ## 80773       1415            AA      1810  N455AA                NA
    ## 81474       2101            YV      7228  N522LR                NA
    ## 82245        910            AA      1347  N468AA                NA
    ## 82496       1020            AA       405  N493AA                NA
    ## 82746        940            WN        94  N623SW                NA
    ## 82747        915            WN       565  N373SW                NA
    ## 82748       2058            CO      1441                        NA
    ## 82749       1900            CO      1603                        NA
    ## 83010        635            CO       240                        NA
    ## 83011        755            CO      1573                        NA
    ## 83463       2333            B6      1069  N178JB                NA
    ## 83707       1142            B6      1060  N178JB                NA
    ## 84219       1645            AA      1023  N208AA                NA
    ## 84220       1440            AA      1545  N4XSAA                NA
    ## 84221       1415            AA      1810  N208AA                NA
    ## 84734       2112            YV      7281  N514MJ                NA
    ## 85913        705            AA      1336  N4XCAA                NA
    ## 86411       1010            AA       379  N445AA                NA
    ## 87334       2020            AA      1278  N258AA                NA
    ## 87335       2140            AA      1347  N521AA                NA
    ## 87336       2025            AA      1486  N426AA                NA
    ## 87337         20            AA      1532  N451AA                NA
    ## 87580        855            WN      1146                        NA
    ## 87581        750            AA      2065  N241AA                NA
    ## 87582       1000            AA      2454  N496AA                NA
    ## 87583        700            WN      1146                        NA
    ## 87828       1220            AA      1296  N4WNAA                NA
    ## 87829       1025            AA      1477  N4WNAA                NA
    ## 88751       1130            WN      1063                        NA
    ## 88752       1010            WN      1195                        NA
    ## 89493       1315            WN      1302                        NA
    ## 89494       1250            WN      2359                        NA
    ## 89495       1140            WN      2272                        NA
    ## 89496       1155            WN      1302                        NA
    ## 89748        857            YV      7131  N505MJ                NA
    ## 91182       1910            UA       373                        NA
    ## 91762       1315            WN      3697                        NA
    ## 91763       1145            WN      1555                        NA
    ## 93003        750            AA      2065  N498AA                NA
    ## 93251       1010            AA       379  N460AA                NA
    ## 94150       1350            9E      2245                        NA
    ## 94151       1805            9E      2247                        NA
    ## 94152       1542            9E      2242                        NA
    ## 94153       1120            9E      2244                        NA
    ## 94154       2055            AA       623  N516AA                NA
    ## 94155       2300            AA      2491  N568AA                NA
    ## 94388       2018            CO      1441  N36207                NA
    ## 94392       1650            AA      1023  N593AA                NA
    ## 94393        705            AA      1336  N4XTAA                NA
    ## 94394        835            AA      1614  N424AA                NA
    ## 94395        755            CO       340                        NA
    ## 94396       2333            XE      2639                        NA
    ## 94397       1410            AA      1810  N593AA                NA
    ## 94644       1450            XE      2638                        NA
    ## 94645        634            CO       240                        NA
    ## 94893        855            WN      1146  N382SW                NA
    ## 94894        700            WN      1146  N382SW                NA
    ## 95200       1545            AA      1096  N298AA                NA
    ## 95308       1350            9E      2245                        NA
    ## 95309       1735            AA      2040  N4WMAA                NA
    ## 95310       1120            9E      2244                        NA
    ## 95471       1750            9E      2152  89869E                NA
    ## 95556       1220            AA      1296  N4WDAA                NA
    ## 95557       1400            CO      1540                        NA
    ## 95558       2033            OO      4476  N613SK                NA
    ## 95559       1025            AA      1477  N4WDAA                NA
    ## 95560       1218            CO       741                        NA
    ## 95796       1945            WN       639  N282WN                NA
    ## 95797       1250            WN      2359  N742SW                NA
    ## 95798        850            OO      4875  N613SK                NA
    ## 95799       2014            OO      5870  N758SK                NA
    ## 95800       1950            AA      2402  N507AA                NA
    ## 95801       1656            CO       251                        NA
    ## 95802       2239            CO       351                        NA
    ## 95803       1835            WN      1673  N282WN                NA
    ## 95804       1140            WN      2272  N742SW                NA
    ## 95805       2300            AA      2491  N4YUAA                NA
    ## 95806       1736            CO       450                        NA
    ## 96051         20            WN      1663  N648SW                NA
    ## 96052       1025            CO       350                        NA
    ## 96301       1011            XE      3084                        NA
    ## 96302       1950            AA      2402  N573AA                NA
    ## 96303       2225            CO       351                        NA
    ## 96304        824            XE      3081                        NA
    ## 96549       1701            OO      6010  N727SK                NA
    ## 96550       1814            B6      1062  N239JB                NA
    ## 96551       1512            OO      6079  N727SK                NA
    ## 96552       1010            AA       379  N585AA                NA
    ## 96553       1105            AA      1199  N441AA                NA
    ## 96554       2344            B6      1069  N216JB                NA
    ## 96555       1755            CO       450                        NA
    ## 96556       2358            CO      1561                        NA
    ## 96745       2105            OH      6527  N398CA                NA
    ## 96746       2030            9E      2102                        NA
    ## 96747        902            CO      1786                        NA
    ## 96748       1615            9E      2103                        NA
    ## 96973       1130            WN      1063  N903WN                NA
    ## 96974       1925            AA      2080  N4XBAA                NA
    ## 96975       1711            B6      1264  N274JB                NA
    ## 96976       1010            WN      1195  N903WN                NA
    ## 96977       1735            AA      1245  N4XBAA                NA
    ## 96978       1141            B6      1263  N274JB                NA
    ## 97227       2030            9E      2102                        NA
    ## 97228       1615            9E      2103                        NA
    ## 97472       1945            WN       639  N493WN                NA
    ## 97473       1950            AA      2402  N556AA                NA
    ## 97474       1835            WN      1673  N493WN                NA
    ## 97475       1512            OO      6079  N708SK                NA
    ## 97476       1955            AA      2293  N509AA                NA
    ## 97678       1814            B6      1062  N273JB                NA
    ## 97679       1617            B6      1065  N193JB                NA
    ## 98125       1955            WN      2859  N708SW                NA
    ## 98126        705            AA      1336  N571AA                NA
    ## 98127       2135            WN      3543  N215WN                NA
    ## 98128       2300            AA      2491  N4YTAA                NA
    ## 98330       1535            WN      1907  N263WN                NA
    ## 98331        835            AA      1614  N422AA                NA
    ## 98332       1035            WN      3607                        NA
    ## 98556       1512            OO      6079  N751SK                NA
    ## 98804       1710            WN       529  N526SW                NA
    ## 99048       1035            WN      1341  N338SW                NA
    ## 99049        930            AA       482  N450AA                NA
    ## 99050        850            WN      3400  N714CB                NA
    ## 99259       1711            B6      1264  N179JB                NA
    ## 99260       1617            B6      1065  N193JB                NA
    ##       CRSElapsedTime AirTime ArrDelay DepDelay Origin Dest Distance TaxiIn
    ## 250              156      NA       NA       NA    AUS  ORD      978     NA
    ## 251               55      NA       NA       NA    AUS  DAL      189     NA
    ## 252               60      NA       NA       NA    AUS  DFW      190     NA
    ## 253               60      NA       NA       NA    AUS  DFW      190     NA
    ## 254               50      NA       NA       NA    DAL  AUS      189     NA
    ## 255               60      NA       NA       NA    DFW  AUS      190     NA
    ## 552               55      NA       NA       NA    AUS  DAL      189     NA
    ## 553               55      NA       NA       NA    AUS  DAL      189     NA
    ## 554               50      NA       NA       NA    DAL  AUS      189     NA
    ## 555               50      NA       NA       NA    DAL  AUS      189     NA
    ## 849              132      NA       NA       NA    AUS  DEN      775     NA
    ## 850               55      NA       NA       NA    AUS  DAL      189     NA
    ## 851               50      NA       NA       NA    AUS  HOU      148     NA
    ## 852              205      NA       NA       NA    SFO  AUS     1504     NA
    ## 853               50      NA       NA       NA    DAL  AUS      189     NA
    ## 854               50      NA       NA       NA    DAL  AUS      189     NA
    ## 1151              55      NA       NA       NA    AUS  DAL      189     NA
    ## 1152              55      NA       NA       NA    AUS  DAL      189     NA
    ## 1153              55      NA       NA       NA    DAL  AUS      189     NA
    ## 1154              55      NA       NA       NA    DAL  AUS      189     NA
    ## 1674             181      NA       NA       NA    AUS  LAS     1090     NA
    ## 1675             150      NA       NA       NA    AUS  ORD      978     NA
    ## 1676             170      NA       NA       NA    ORD  AUS      978     NA
    ## 2259             160      NA       NA       NA    AUS  ORD      978     NA
    ## 2550              55      NA       NA       NA    AUS  DAL      189     NA
    ## 2551              50      NA       NA       NA    DAL  AUS      189     NA
    ## 2846             137      NA       NA       NA    AUS  DEN      775     NA
    ## 2847              60      NA       NA       NA    AUS  DAL      189     NA
    ## 2848             124      NA       NA       NA    DEN  AUS      775     NA
    ## 2849              55      NA       NA       NA    DAL  AUS      189     NA
    ## 3147             159      NA       NA       NA    AUS  ORD      978     NA
    ## 3370             105      NA       NA       NA    AUS  MEM      559     NA
    ## 3643             225      NA       NA       NA    AUS  EWR     1504     NA
    ## 3929              60      NA       NA       NA    AUS  DAL      189     NA
    ## 3930              55      NA       NA       NA    AUS  DAL      189     NA
    ## 3931              60      NA       NA       NA    AUS  DFW      190     NA
    ## 3932             229      NA       NA       NA    AUS  BOS     1698     NA
    ## 3933             213      NA       NA       NA    AUS  JFK     1522     NA
    ## 3934              55      NA       NA       NA    DAL  AUS      189     NA
    ## 3935              55      NA       NA       NA    DAL  AUS      189     NA
    ## 3936              60      NA       NA       NA    DFW  AUS      190     NA
    ## 3937             285      NA       NA       NA    BOS  AUS     1698     NA
    ## 3938             267      NA       NA       NA    JFK  AUS     1522     NA
    ## 3939             252      NA       NA       NA    EWR  AUS     1504     NA
    ## 4222             200      NA       NA       NA    AUS  LAX     1242     NA
    ## 4223             155      NA       NA       NA    AUS  ORD      978     NA
    ## 4224             165      NA       NA       NA    ORD  AUS      978     NA
    ## 4225             180      NA       NA       NA    LAX  AUS     1242     NA
    ## 4514             129      NA       NA       NA    AUS  ATL      813     NA
    ## 4515              60      NA       NA       NA    AUS  DFW      190     NA
    ## 4516             156      NA       NA       NA    ATL  AUS      813     NA
    ## 4517             151      NA       NA       NA    ATL  AUS      813     NA
    ## 4810             159      NA       NA       NA    AUS  ORD      978     NA
    ## 4811             124      NA       NA       NA    AUS  ATL      813     NA
    ## 4812             171      NA       NA       NA    ORD  AUS      978     NA
    ## 4813             161      NA       NA       NA    ATL  AUS      813     NA
    ## 4814             159      NA       NA       NA    ATL  AUS      813     NA
    ## 4815              60      NA       NA       NA    DFW  AUS      190     NA
    ## 5114             159      NA       NA       NA    AUS  ORD      978     NA
    ## 5320             126      NA       NA       NA    AUS  ATL      813     NA
    ## 5321             124      NA       NA       NA    AUS  ATL      813     NA
    ## 5322             132      NA       NA       NA    AUS  ATL      813     NA
    ## 5323             129      NA       NA       NA    AUS  ATL      813     NA
    ## 5324             157      NA       NA       NA    ATL  AUS      813     NA
    ## 5325             161      NA       NA       NA    ATL  AUS      813     NA
    ## 5326             152      NA       NA       NA    ATL  AUS      813     NA
    ## 5593             110      NA       NA       NA    AUS  MCI      650     NA
    ## 5594             160      NA       NA       NA    AUS  ORD      978     NA
    ## 5595             115      NA       NA       NA    MCI  AUS      650     NA
    ## 5596             165      NA       NA       NA    ORD  AUS      978     NA
    ## 5892             159      NA       NA       NA    AUS  ORD      978     NA
    ## 5893             171      NA       NA       NA    ORD  AUS      978     NA
    ## 5894              79      NA       NA       NA    ABQ  AUS      619     NA
    ## 6175             129      NA       NA       NA    AUS  ATL      813     NA
    ## 6176              60      NA       NA       NA    AUS  DAL      189     NA
    ## 6177             160      NA       NA       NA    AUS  ORD      978     NA
    ## 6178             161      NA       NA       NA    ATL  AUS      813     NA
    ## 6179              55      NA       NA       NA    DAL  AUS      189     NA
    ## 6180             165      NA       NA       NA    ORD  AUS      978     NA
    ## 6470             135      NA       NA       NA    AUS  DEN      775     NA
    ## 6471             160      NA       NA       NA    AUS  ORD      978     NA
    ## 6472             165      NA       NA       NA    ORD  AUS      978     NA
    ## 6767             127      NA       NA       NA    AUS  ABQ      619     NA
    ## 6768             127      NA       NA       NA    AUS  ABQ      619     NA
    ## 6769              60      NA       NA       NA    AUS  DFW      190     NA
    ## 6770              60      NA       NA       NA    HRL  AUS      273     NA
    ## 6771             104      NA       NA       NA    ABQ  AUS      619     NA
    ## 6772              88      NA       NA       NA    TUL  AUS      426     NA
    ## 7066             110      NA       NA       NA    AUS  MCI      650     NA
    ## 7067              65      NA       NA       NA    AUS  DFW      190     NA
    ## 7068             115      NA       NA       NA    MCI  AUS      650     NA
    ## 7069              60      NA       NA       NA    DFW  AUS      190     NA
    ## 7070              60      NA       NA       NA    DFW  AUS      190     NA
    ## 7280             124      NA       NA       NA    AUS  ATL      813     NA
    ## 7281             154      NA       NA       NA    ATL  AUS      813     NA
    ## 7282             161      NA       NA       NA    ATL  AUS      813     NA
    ## 7546              55      NA       NA       NA    AUS  DAL      189     NA
    ## 7547              60      NA       NA       NA    AUS  DAL      189     NA
    ## 7548              55      NA       NA       NA    AUS  DAL      189     NA
    ## 7549              55      NA       NA       NA    AUS  DAL      189     NA
    ## 7550             156      NA       NA       NA    ATL  AUS      813     NA
    ## 7551              50      NA       NA       NA    DAL  AUS      189     NA
    ## 7552              55      NA       NA       NA    DAL  AUS      189     NA
    ## 7553              55      NA       NA       NA    DAL  AUS      189     NA
    ## 7842             178      NA       NA       NA    AUS  IAD     1297     NA
    ## 7843             152      NA       NA       NA    AUS  PHX      872     NA
    ## 7844             159      NA       NA       NA    AUS  ORD      978     NA
    ## 7845             160      NA       NA       NA    AUS  ORD      978     NA
    ## 7846             155      NA       NA       NA    AUS  ORD      978     NA
    ## 7847             170      NA       NA       NA    ORD  AUS      978     NA
    ## 7848             165      NA       NA       NA    ORD  AUS      978     NA
    ## 7849             165      NA       NA       NA    ORD  AUS      978     NA
    ## 8122             155      NA       NA       NA    AUS  PHX      872     NA
    ## 8123              55      NA       NA       NA    AUS  DAL      189     NA
    ## 8124              60      NA       NA       NA    AUS  DFW      190     NA
    ## 8125              60      NA       NA       NA    AUS  DFW      190     NA
    ## 8126             160      NA       NA       NA    AUS  ORD      978     NA
    ## 8127             155      NA       NA       NA    AUS  ORD      978     NA
    ## 8128              59      NA       NA       NA    AUS  IAH      140     NA
    ## 8129              50      NA       NA       NA    DAL  AUS      189     NA
    ## 8130              55      NA       NA       NA    DAL  AUS      189     NA
    ## 8131              60      NA       NA       NA    DFW  AUS      190     NA
    ## 8132              60      NA       NA       NA    DFW  AUS      190     NA
    ## 8133             165      NA       NA       NA    ORD  AUS      978     NA
    ## 8134             165      NA       NA       NA    ORD  AUS      978     NA
    ## 8135             252      NA       NA       NA    EWR  AUS     1504     NA
    ## 8424              55      NA       NA       NA    AUS  DAL      189     NA
    ## 8425             150      NA       NA       NA    AUS  ORD      978     NA
    ## 8426              50      NA       NA       NA    DAL  AUS      189     NA
    ## 8427             165      NA       NA       NA    ORD  AUS      978     NA
    ## 8719             159      NA       NA       NA    AUS  ORD      978     NA
    ## 8720             165      NA       NA       NA    AUS  ORD      978     NA
    ## 8721             160      NA       NA       NA    AUS  ORD      978     NA
    ## 8722             155      NA       NA       NA    AUS  ORD      978     NA
    ## 8723             171      NA       NA       NA    ORD  AUS      978     NA
    ## 8724             165      NA       NA       NA    ORD  AUS      978     NA
    ## 8725             165      NA       NA       NA    ORD  AUS      978     NA
    ## 8726             165      NA       NA       NA    ORD  AUS      978     NA
    ## 9023             200      NA       NA       NA    AUS  LAX     1242     NA
    ## 9024             254      NA       NA       NA    JFK  AUS     1522     NA
    ## 9226             184      NA       NA       NA    AUS  IAD     1297     NA
    ## 9227             165      NA       NA       NA    AUS  ORD      978     NA
    ## 9228             155      NA       NA       NA    AUS  ORD      978     NA
    ## 9229             213      NA       NA       NA    AUS  JFK     1522     NA
    ## 9230             225      NA       NA       NA    IAD  AUS     1297     NA
    ## 9231             225      NA       NA       NA    IAD  AUS     1297     NA
    ## 9232              60      NA       NA       NA    DFW  AUS      190     NA
    ## 9233             165      NA       NA       NA    ORD  AUS      978     NA
    ## 9499              55      NA       NA       NA    AUS  DAL      189     NA
    ## 9500             184      NA       NA       NA    AUS  IAD     1297     NA
    ## 9501             105      NA       NA       NA    AUS  MEM      559     NA
    ## 9502              50      NA       NA       NA    DAL  AUS      189     NA
    ## 9503             165      NA       NA       NA    ORD  AUS      978     NA
    ## 9785              55      NA       NA       NA    AUS  DAL      189     NA
    ## 9786             150      NA       NA       NA    AUS  MDW      972     NA
    ## 9787             159      NA       NA       NA    AUS  ORD      978     NA
    ## 9788              55      NA       NA       NA    AUS  DAL      189     NA
    ## 9789              55      NA       NA       NA    AUS  DAL      189     NA
    ## 9790             165      NA       NA       NA    AUS  ORD      978     NA
    ## 9791             155      NA       NA       NA    AUS  ORD      978     NA
    ## 9792             155      NA       NA       NA    AUS  ORD      978     NA
    ## 9793             165      NA       NA       NA    MDW  AUS      972     NA
    ## 9794             165      NA       NA       NA    MDW  AUS      972     NA
    ## 9795             171      NA       NA       NA    ORD  AUS      978     NA
    ## 9796              50      NA       NA       NA    DAL  AUS      189     NA
    ## 9797              50      NA       NA       NA    DAL  AUS      189     NA
    ## 9798             165      NA       NA       NA    ORD  AUS      978     NA
    ## 9799             165      NA       NA       NA    ORD  AUS      978     NA
    ## 10078             55      NA       NA       NA    AUS  DAL      189     NA
    ## 10079             55      NA       NA       NA    AUS  DAL      189     NA
    ## 10080             55      NA       NA       NA    AUS  DAL      189     NA
    ## 10081            160      NA       NA       NA    AUS  ORD      978     NA
    ## 10082             50      NA       NA       NA    DAL  AUS      189     NA
    ## 10083             55      NA       NA       NA    DAL  AUS      189     NA
    ## 10084             55      NA       NA       NA    DAL  AUS      189     NA
    ## 10085            120      NA       NA       NA    MEM  AUS      559     NA
    ## 10360             55      NA       NA       NA    AUS  DAL      189     NA
    ## 10361            110      NA       NA       NA    AUS  MCI      650     NA
    ## 10362             55      NA       NA       NA    AUS  DAL      189     NA
    ## 10363            105      NA       NA       NA    AUS  MEM      559     NA
    ## 10364            165      NA       NA       NA    AUS  ORD      978     NA
    ## 10365            160      NA       NA       NA    AUS  ORD      978     NA
    ## 10366             65      NA       NA       NA    AUS  DFW      190     NA
    ## 10367            160      NA       NA       NA    AUS  ORD      978     NA
    ## 10368            155      NA       NA       NA    AUS  ORD      978     NA
    ## 10369             45      NA       NA       NA    HOU  AUS      148     NA
    ## 10370            115      NA       NA       NA    MCI  AUS      650     NA
    ## 10371            171      NA       NA       NA    ORD  AUS      978     NA
    ## 10372             50      NA       NA       NA    DAL  AUS      189     NA
    ## 10373            165      NA       NA       NA    ORD  AUS      978     NA
    ## 10374            165      NA       NA       NA    ORD  AUS      978     NA
    ## 10375            165      NA       NA       NA    ORD  AUS      978     NA
    ## 10376            165      NA       NA       NA    ORD  AUS      978     NA
    ## 10377            165      NA       NA       NA    ORD  AUS      978     NA
    ## 10672            181      NA       NA       NA    AUS  LAS     1090     NA
    ## 10673            110      NA       NA       NA    AUS  MCI      650     NA
    ## 10674            132      NA       NA       NA    PHX  AUS      872     NA
    ## 10675            115      NA       NA       NA    MCI  AUS      650     NA
    ## 10971            159      NA       NA       NA    AUS  ORD      978     NA
    ## 10972            105      NA       NA       NA    AUS  MEM      559     NA
    ## 10973            120      NA       NA       NA    MEM  AUS      559     NA
    ## 11183             60      NA       NA       NA    AUS  DFW      190     NA
    ## 11750             55      NA       NA       NA    AUS  DAL      189     NA
    ## 11751             55      NA       NA       NA    DAL  AUS      189     NA
    ## 12035            159      NA       NA       NA    AUS  ORD      978     NA
    ## 12036            165      NA       NA       NA    AUS  ORD      978     NA
    ## 12037            165      NA       NA       NA    ORD  AUS      978     NA
    ## 12038            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 12328             55      NA       NA       NA    AUS  DAL      189     NA
    ## 12329            106      NA       NA       NA    AUS  MEM      559     NA
    ## 12330            115      NA       NA       NA    AUS  STL      722     NA
    ## 12331            155      NA       NA       NA    AUS  ORD      978     NA
    ## 12332            213      NA       NA       NA    AUS  JFK     1522     NA
    ## 12333             50      NA       NA       NA    DAL  AUS      189     NA
    ## 12334            115      NA       NA       NA    MEM  AUS      559     NA
    ## 12335            165      NA       NA       NA    ORD  AUS      978     NA
    ## 12336            135      NA       NA       NA    STL  AUS      722     NA
    ## 12632             55      NA       NA       NA    AUS  DAL      189     NA
    ## 12633            155      NA       NA       NA    AUS  PHX      872     NA
    ## 12634            184      NA       NA       NA    AUS  IAD     1297     NA
    ## 12635            181      NA       NA       NA    AUS  LAS     1090     NA
    ## 12636             50      NA       NA       NA    DAL  AUS      189     NA
    ## 12637             60      NA       NA       NA    MAF  AUS      294     NA
    ## 12638            132      NA       NA       NA    PHX  AUS      872     NA
    ## 12639            120      NA       NA       NA    MEM  AUS      559     NA
    ## 12942            105      NA       NA       NA    AUS  MEM      559     NA
    ## 12943            120      NA       NA       NA    MEM  AUS      559     NA
    ## 13149            105      NA       NA       NA    AUS  MEM      559     NA
    ## 13150            100      NA       NA       NA    AUS  MEM      559     NA
    ## 13151            105      NA       NA       NA    AUS  MEM      559     NA
    ## 13152             65      NA       NA       NA    AUS  DFW      190     NA
    ## 13153             65      NA       NA       NA    AUS  DFW      190     NA
    ## 13154             60      NA       NA       NA    AUS  DFW      190     NA
    ## 13155            104      NA       NA       NA    ABQ  AUS      619     NA
    ## 13156            115      NA       NA       NA    MEM  AUS      559     NA
    ## 13157            120      NA       NA       NA    MEM  AUS      559     NA
    ## 13158             60      NA       NA       NA    DFW  AUS      190     NA
    ## 13159             55      NA       NA       NA    DFW  AUS      190     NA
    ## 13160             60      NA       NA       NA    DFW  AUS      190     NA
    ## 13161             60      NA       NA       NA    DFW  AUS      190     NA
    ## 13422            111      NA       NA       NA    AUS  MCI      650     NA
    ## 13423            110      NA       NA       NA    AUS  MCI      650     NA
    ## 13424            160      NA       NA       NA    AUS  ORD      978     NA
    ## 13425            155      NA       NA       NA    AUS  ORD      978     NA
    ## 13426            117      NA       NA       NA    MCI  AUS      650     NA
    ## 13427            115      NA       NA       NA    MCI  AUS      650     NA
    ## 13428            155      NA       NA       NA    ATL  AUS      813     NA
    ## 13429            165      NA       NA       NA    ORD  AUS      978     NA
    ## 13430            165      NA       NA       NA    ORD  AUS      978     NA
    ## 13725            122      NA       NA       NA    AUS  ATL      813     NA
    ## 13726            223      NA       NA       NA    AUS  JFK     1522     NA
    ## 14021            122      NA       NA       NA    AUS  ATL      813     NA
    ## 14022             55      NA       NA       NA    AUS  DAL      189     NA
    ## 14023            155      NA       NA       NA    AUS  ORD      978     NA
    ## 14024             55      NA       NA       NA    DAL  AUS      189     NA
    ## 14025            165      NA       NA       NA    ORD  AUS      978     NA
    ## 14325             55      NA       NA       NA    AUS  DAL      189     NA
    ## 14326             55      NA       NA       NA    DAL  AUS      189     NA
    ## 14623            110      NA       NA       NA    AUS  MCI      650     NA
    ## 14624            115      NA       NA       NA    AUS  STL      722     NA
    ## 14625            235      NA       NA       NA    AUS  EWR     1504     NA
    ## 14626            155      NA       NA       NA    ATL  AUS      813     NA
    ## 14627            115      NA       NA       NA    MCI  AUS      650     NA
    ## 14628            135      NA       NA       NA    STL  AUS      722     NA
    ## 14629            174      NA       NA       NA    LAX  AUS     1242     NA
    ## 14630             59      NA       NA       NA    IAH  AUS      140     NA
    ## 14918            124      NA       NA       NA    AUS  ATL      813     NA
    ## 14919            218      NA       NA       NA    AUS  JFK     1522     NA
    ## 14920             55      NA       NA       NA    AUS  DAL      189     NA
    ## 14921             60      NA       NA       NA    AUS  DFW      190     NA
    ## 14922            213      NA       NA       NA    AUS  JFK     1522     NA
    ## 14923            223      NA       NA       NA    AUS  JFK     1522     NA
    ## 14924            211      NA       NA       NA    AUS  EWR     1504     NA
    ## 14925            230      NA       NA       NA    AUS  EWR     1504     NA
    ## 14926            155      NA       NA       NA    ATL  AUS      813     NA
    ## 14927            319      NA       NA       NA    JFK  AUS     1522     NA
    ## 14928            170      NA       NA       NA    CVG  AUS      958     NA
    ## 14929            154      NA       NA       NA    ATL  AUS      813     NA
    ## 14930             50      NA       NA       NA    DAL  AUS      189     NA
    ## 14931            267      NA       NA       NA    JFK  AUS     1522     NA
    ## 14932            251      NA       NA       NA    JFK  AUS     1522     NA
    ## 14933            256      NA       NA       NA    EWR  AUS     1504     NA
    ## 14934            260      NA       NA       NA    EWR  AUS     1504     NA
    ## 15154            124      NA       NA       NA    AUS  ATL      813     NA
    ## 15155            149      NA       NA       NA    AUS  CVG      958     NA
    ## 15156            190      NA       NA       NA    AUS  SLC     1085     NA
    ## 15436            120      NA       NA       NA    MEM  AUS      559     NA
    ## 15728             55      NA       NA       NA    AUS  DAL      189     NA
    ## 15729             45      NA       NA       NA    AUS  HOU      148     NA
    ## 15730            130      NA       NA       NA    AUS  DEN      775     NA
    ## 15731            131      NA       NA       NA    AUS  ATL      813     NA
    ## 15732            105      NA       NA       NA    AUS  MEM      559     NA
    ## 15733            155      NA       NA       NA    AUS  ORD      978     NA
    ## 15734             50      NA       NA       NA    DAL  AUS      189     NA
    ## 15735             45      NA       NA       NA    HOU  AUS      148     NA
    ## 15736            225      NA       NA       NA    IAD  AUS     1297     NA
    ## 15737            159      NA       NA       NA    ATL  AUS      813     NA
    ## 15738            165      NA       NA       NA    ORD  AUS      978     NA
    ## 16034            184      NA       NA       NA    AUS  IAD     1297     NA
    ## 16035            155      NA       NA       NA    AUS  ORD      978     NA
    ## 16036            168      NA       NA       NA    ORD  AUS      978     NA
    ## 16037            165      NA       NA       NA    ORD  AUS      978     NA
    ## 16318            178      NA       NA       NA    AUS  IAD     1297     NA
    ## 16596            162      NA       NA       NA    AUS  PHX      872     NA
    ## 16597             55      NA       NA       NA    AUS  DAL      189     NA
    ## 16598            200      NA       NA       NA    AUS  LAX     1242     NA
    ## 16599            128      NA       NA       NA    DEN  AUS      775     NA
    ## 16600             55      NA       NA       NA    DAL  AUS      189     NA
    ## 16882            120      NA       NA       NA    MEM  AUS      559     NA
    ## 17105            105      NA       NA       NA    AUS  MEM      559     NA
    ## 17680             55      NA       NA       NA    AUS  DAL      189     NA
    ## 17681             60      NA       NA       NA    AUS  DFW      190     NA
    ## 17682             60      NA       NA       NA    AUS  DFW      190     NA
    ## 17683            115      NA       NA       NA    AUS  STL      722     NA
    ## 17684            225      NA       NA       NA    AUS  SJC     1476     NA
    ## 17685             59      NA       NA       NA    AUS  IAH      140     NA
    ## 17686             50      NA       NA       NA    DAL  AUS      189     NA
    ## 17687             60      NA       NA       NA    DFW  AUS      190     NA
    ## 17688            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 17689             60      NA       NA       NA    DFW  AUS      190     NA
    ## 17690            135      NA       NA       NA    STL  AUS      722     NA
    ## 17691             55      NA       NA       NA    IAH  AUS      140     NA
    ## 17977             50      NA       NA       NA    AUS  HOU      148     NA
    ## 17978            224      NA       NA       NA    AUS  JFK     1522     NA
    ## 17979             60      NA       NA       NA    AUS  DFW      190     NA
    ## 17980            115      NA       NA       NA    AUS  STL      722     NA
    ## 17981            155      NA       NA       NA    AUS  ORD      978     NA
    ## 17982            131      NA       NA       NA    AUS  ATL      813     NA
    ## 17983             50      NA       NA       NA    DAL  AUS      189     NA
    ## 17984            160      NA       NA       NA    ATL  AUS      813     NA
    ## 17985             55      NA       NA       NA    DFW  AUS      190     NA
    ## 17986            165      NA       NA       NA    ORD  AUS      978     NA
    ## 17987            135      NA       NA       NA    STL  AUS      722     NA
    ## 17988            145      NA       NA       NA    ATL  AUS      813     NA
    ## 18286            174      NA       NA       NA    AUS  CLE     1174     NA
    ## 18287            125      NA       NA       NA    AUS  ATL      813     NA
    ## 18288            148      NA       NA       NA    AUS  CVG      958     NA
    ## 18289            197      NA       NA       NA    CLE  AUS     1174     NA
    ## 18567             55      NA       NA       NA    AUS  DAL      189     NA
    ## 18568             55      NA       NA       NA    AUS  DAL      189     NA
    ## 18569             55      NA       NA       NA    AUS  DAL      189     NA
    ## 18570             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18571             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18572             65      NA       NA       NA    AUS  DFW      190     NA
    ## 18573             65      NA       NA       NA    AUS  DFW      190     NA
    ## 18574             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18575            160      NA       NA       NA    AUS  RDU     1162     NA
    ## 18576             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18577             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18578             55      NA       NA       NA    DAL  AUS      189     NA
    ## 18579             55      NA       NA       NA    DAL  AUS      189     NA
    ## 18580             50      NA       NA       NA    DAL  AUS      189     NA
    ## 18581             55      NA       NA       NA    DFW  AUS      190     NA
    ## 18582             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18583             55      NA       NA       NA    DFW  AUS      190     NA
    ## 18584             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18585             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18586             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18587             55      NA       NA       NA    DFW  AUS      190     NA
    ## 18588             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18589             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18590             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18591             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18592             60      NA       NA       NA    DFW  AUS      190     NA
    ## 18593             55      NA       NA       NA    DFW  AUS      190     NA
    ## 18883             55      NA       NA       NA    AUS  DAL      189     NA
    ## 18884             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18885             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18886             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18887             55      NA       NA       NA    AUS  DFW      190     NA
    ## 18888             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18889             60      NA       NA       NA    AUS  DFW      190     NA
    ## 18890             60      NA       NA       NA    MAF  AUS      294     NA
    ## 18891            194      NA       NA       NA    CLE  AUS     1174     NA
    ## 18892            174      NA       NA       NA    CVG  AUS      958     NA
    ## 18893            160      NA       NA       NA    ATL  AUS      813     NA
    ## 18894            174      NA       NA       NA    CVG  AUS      958     NA
    ## 18895             50      NA       NA       NA    DAL  AUS      189     NA
    ## 18896            120      NA       NA       NA    MEM  AUS      559     NA
    ## 18897             55      NA       NA       NA    DFW  AUS      190     NA
    ## 19118            167      NA       NA       NA    AUS  CLE     1174     NA
    ## 19119            151      NA       NA       NA    AUS  CVG      958     NA
    ## 19120            149      NA       NA       NA    AUS  CVG      958     NA
    ## 19121            105      NA       NA       NA    AUS  MEM      559     NA
    ## 19122            194      NA       NA       NA    CLE  AUS     1174     NA
    ## 19123            311      NA       NA       NA    JFK  AUS     1522     NA
    ## 19406            173      NA       NA       NA    AUS  CLE     1174     NA
    ## 19407            224      NA       NA       NA    AUS  JFK     1522     NA
    ## 19408            311      NA       NA       NA    JFK  AUS     1522     NA
    ## 19409            160      NA       NA       NA    ATL  AUS      813     NA
    ## 19705             50      NA       NA       NA    AUS  HOU      148     NA
    ## 19706            130      NA       NA       NA    AUS  ATL      813     NA
    ## 19707            148      NA       NA       NA    AUS  CVG      958     NA
    ## 19708             55      NA       NA       NA    AUS  DAL      189     NA
    ## 19709             55      NA       NA       NA    AUS  DAL      189     NA
    ## 19710             50      NA       NA       NA    DAL  AUS      189     NA
    ## 19711            151      NA       NA       NA    ATL  AUS      813     NA
    ## 19712             50      NA       NA       NA    DAL  AUS      189     NA
    ## 19713             50      NA       NA       NA    DAL  AUS      189     NA
    ## 19999             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20000             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20001             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20002             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20003             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20004             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20005             50      NA       NA       NA    DAL  AUS      189     NA
    ## 20006             50      NA       NA       NA    DAL  AUS      189     NA
    ## 20007             50      NA       NA       NA    DAL  AUS      189     NA
    ## 20008             55      NA       NA       NA    DAL  AUS      189     NA
    ## 20009             55      NA       NA       NA    DAL  AUS      189     NA
    ## 20010             50      NA       NA       NA    DAL  AUS      189     NA
    ## 20311             45      NA       NA       NA    HOU  AUS      148     NA
    ## 20611             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20612            225      NA       NA       NA    AUS  SJC     1476     NA
    ## 20613            115      NA       NA       NA    AUS  STL      722     NA
    ## 20614             55      NA       NA       NA    DAL  AUS      189     NA
    ## 20615            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 20918             55      NA       NA       NA    AUS  DAL      189     NA
    ## 20919             50      NA       NA       NA    DAL  AUS      189     NA
    ## 21147             60      NA       NA       NA    DFW  AUS      190     NA
    ## 21432             55      NA       NA       NA    AUS  DAL      189     NA
    ## 21433            151      NA       NA       NA    ATL  AUS      813     NA
    ## 21434            120      NA       NA       NA    MEM  AUS      559     NA
    ## 21736             55      NA       NA       NA    AUS  DAL      189     NA
    ## 21737            105      NA       NA       NA    AUS  MEM      559     NA
    ## 21738             60      NA       NA       NA    AUS  DFW      190     NA
    ## 21739             55      NA       NA       NA    DAL  AUS      189     NA
    ## 21740             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22000             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22001            180      NA       NA       NA    AUS  ORF     1319     NA
    ## 22002            131      NA       NA       NA    AUS  ATL      813     NA
    ## 22003             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22004             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22005             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22006             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22007             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22008             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22009             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22010             65      NA       NA       NA    AUS  DFW      190     NA
    ## 22011             65      NA       NA       NA    AUS  DFW      190     NA
    ## 22012             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22013             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22014             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22015             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22016             56      NA       NA       NA    AUS  IAH      140     NA
    ## 22017             62      NA       NA       NA    AUS  IAH      140     NA
    ## 22018             59      NA       NA       NA    AUS  IAH      140     NA
    ## 22019             50      NA       NA       NA    DAL  AUS      189     NA
    ## 22020             50      NA       NA       NA    DAL  AUS      189     NA
    ## 22021             50      NA       NA       NA    DAL  AUS      189     NA
    ## 22022            168      NA       NA       NA    ORD  AUS      978     NA
    ## 22023             55      NA       NA       NA    DAL  AUS      189     NA
    ## 22024             50      NA       NA       NA    DAL  AUS      189     NA
    ## 22025             50      NA       NA       NA    DAL  AUS      189     NA
    ## 22026             50      NA       NA       NA    DAL  AUS      189     NA
    ## 22027            120      NA       NA       NA    MEM  AUS      559     NA
    ## 22028             55      NA       NA       NA    DFW  AUS      190     NA
    ## 22029             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22030             55      NA       NA       NA    DFW  AUS      190     NA
    ## 22031             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22032             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22033             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22034             55      NA       NA       NA    DFW  AUS      190     NA
    ## 22035             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22036             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22037             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22038             60      NA       NA       NA    DFW  AUS      190     NA
    ## 22039             55      NA       NA       NA    DFW  AUS      190     NA
    ## 22040             59      NA       NA       NA    IAH  AUS      140     NA
    ## 22041             58      NA       NA       NA    IAH  AUS      140     NA
    ## 22042             56      NA       NA       NA    IAH  AUS      140     NA
    ## 22043             59      NA       NA       NA    IAH  AUS      140     NA
    ## 22338            224      NA       NA       NA    AUS  JFK     1522     NA
    ## 22339             55      NA       NA       NA    AUS  DAL      189     NA
    ## 22340            105      NA       NA       NA    AUS  MEM      559     NA
    ## 22341            105      NA       NA       NA    AUS  MEM      559     NA
    ## 22342             60      NA       NA       NA    AUS  DFW      190     NA
    ## 22343             56      NA       NA       NA    AUS  IAH      140     NA
    ## 22344            160      NA       NA       NA    ATL  AUS      813     NA
    ## 22345             55      NA       NA       NA    DAL  AUS      189     NA
    ## 22346            115      NA       NA       NA    MEM  AUS      559     NA
    ## 22652            148      NA       NA       NA    AUS  CVG      958     NA
    ## 22952            152      NA       NA       NA    AUS  PHX      872     NA
    ## 22953            148      NA       NA       NA    AUS  CVG      958     NA
    ## 22954            159      NA       NA       NA    AUS  ORD      978     NA
    ## 22955            160      NA       NA       NA    AUS  ORD      978     NA
    ## 22956            194      NA       NA       NA    CLE  AUS     1174     NA
    ## 22957            168      NA       NA       NA    ORD  AUS      978     NA
    ## 22958            168      NA       NA       NA    ORD  AUS      978     NA
    ## 22959            165      NA       NA       NA    ORD  AUS      978     NA
    ## 23472             55      NA       NA       NA    AUS  DAL      189     NA
    ## 23473             55      NA       NA       NA    DAL  AUS      189     NA
    ## 24077             55      NA       NA       NA    AUS  DAL      189     NA
    ## 24078            168      NA       NA       NA    ORD  AUS      978     NA
    ## 24367            225      NA       NA       NA    AUS  PHL     1430     NA
    ## 24368             55      NA       NA       NA    AUS  DAL      189     NA
    ## 24369             60      NA       NA       NA    AUS  DFW      190     NA
    ## 24370             60      NA       NA       NA    AUS  DFW      190     NA
    ## 24371            115      NA       NA       NA    AUS  STL      722     NA
    ## 24372             60      NA       NA       NA    AUS  DFW      190     NA
    ## 24373             60      NA       NA       NA    AUS  DFW      190     NA
    ## 24374            160      NA       NA       NA    AUS  ORD      978     NA
    ## 24375             50      NA       NA       NA    DAL  AUS      189     NA
    ## 24376             60      NA       NA       NA    DFW  AUS      190     NA
    ## 24377             55      NA       NA       NA    DFW  AUS      190     NA
    ## 24378            165      NA       NA       NA    ORD  AUS      978     NA
    ## 24379             55      NA       NA       NA    DFW  AUS      190     NA
    ## 24380             60      NA       NA       NA    DFW  AUS      190     NA
    ## 24381            135      NA       NA       NA    STL  AUS      722     NA
    ## 24682            131      NA       NA       NA    AUS  ATL      813     NA
    ## 24683             60      NA       NA       NA    AUS  DFW      190     NA
    ## 24684            115      NA       NA       NA    AUS  STL      722     NA
    ## 24685            145      NA       NA       NA    ATL  AUS      813     NA
    ## 24686             60      NA       NA       NA    DFW  AUS      190     NA
    ## 24687            135      NA       NA       NA    STL  AUS      722     NA
    ## 24992            160      NA       NA       NA    ATL  AUS      813     NA
    ## 24993            126      NA       NA       NA    DEN  AUS      775     NA
    ## 25218            105      NA       NA       NA    AUS  MEM      559     NA
    ## 25219            115      NA       NA       NA    MEM  AUS      559     NA
    ## 25792             50      NA       NA       NA    AUS  DAL      189     NA
    ## 25793             55      NA       NA       NA    AUS  DAL      189     NA
    ## 25794             55      NA       NA       NA    AUS  DAL      189     NA
    ## 25795            165      NA       NA       NA    AUS  ORD      978     NA
    ## 25796            160      NA       NA       NA    AUS  ORD      978     NA
    ## 25797             60      NA       NA       NA    AUS  DFW      190     NA
    ## 25798             50      NA       NA       NA    DAL  AUS      189     NA
    ## 25799             55      NA       NA       NA    DAL  AUS      189     NA
    ## 25800             50      NA       NA       NA    DAL  AUS      189     NA
    ## 25801             60      NA       NA       NA    DFW  AUS      190     NA
    ## 25802            165      NA       NA       NA    ORD  AUS      978     NA
    ## 25803            165      NA       NA       NA    ORD  AUS      978     NA
    ## 26088            125      NA       NA       NA    AUS  ATL      813     NA
    ## 26667            200      NA       NA       NA    AUS  LAX     1242     NA
    ## 26668             60      NA       NA       NA    DFW  AUS      190     NA
    ## 26959            125      NA       NA       NA    AUS  ATL      813     NA
    ## 26960            105      NA       NA       NA    AUS  MEM      559     NA
    ## 27448            160      NA       NA       NA    ATL  AUS      813     NA
    ## 27732            148      NA       NA       NA    AUS  CVG      958     NA
    ## 27733            110      NA       NA       NA    AUS  MCI      650     NA
    ## 27734             60      NA       NA       NA    AUS  DFW      190     NA
    ## 27735             60      NA       NA       NA    AUS  DFW      190     NA
    ## 27736            120      NA       NA       NA    MCI  AUS      650     NA
    ## 27737             55      NA       NA       NA    DFW  AUS      190     NA
    ## 27738             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28004            190      NA       NA       NA    AUS  LAX     1242     NA
    ## 28005            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28006            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28007             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28008             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28009             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28010            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 28011             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28012            161      NA       NA       NA    ORD  AUS      978     NA
    ## 28013             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28014            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 28015             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28016             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28017             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28018            185      NA       NA       NA    RDU  AUS     1162     NA
    ## 28019             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28020             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28021            240      NA       NA       NA    SEA  AUS     1770     NA
    ## 28022             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28023             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28024            175      NA       NA       NA    LAX  AUS     1242     NA
    ## 28025            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28026             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28027            155      NA       NA       NA    ORD  AUS      978     NA
    ## 28275             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28276             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28277            190      NA       NA       NA    AUS  LAX     1242     NA
    ## 28278            195      NA       NA       NA    AUS  LAX     1242     NA
    ## 28279            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28280            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 28281            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28282            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28283             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28284            195      NA       NA       NA    AUS  LAX     1242     NA
    ## 28285            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 28286             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28287             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28288             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28289            165      NA       NA       NA    AUS  RDU     1162     NA
    ## 28290            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28291            190      NA       NA       NA    AUS  SNA     1209     NA
    ## 28292            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28293             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28294             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28295             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28296            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 28297            255      NA       NA       NA    AUS  SEA     1770     NA
    ## 28298             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28299             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28300            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28301             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28302            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 28303             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28304             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28305             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28306            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 28307            185      NA       NA       NA    RDU  AUS     1162     NA
    ## 28308             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28309            175      NA       NA       NA    LAX  AUS     1242     NA
    ## 28310            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28311            180      NA       NA       NA    LAX  AUS     1242     NA
    ## 28312             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28313             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28314             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28315            240      NA       NA       NA    SEA  AUS     1770     NA
    ## 28316             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28317            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 28318             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28319            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28320             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28321            155      NA       NA       NA    ORD  AUS      978     NA
    ## 28585            106      NA       NA       NA    AUS  MEM      559     NA
    ## 28586             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28587             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28588            195      NA       NA       NA    AUS  LAX     1242     NA
    ## 28589            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28590            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28591             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28592            195      NA       NA       NA    AUS  LAX     1242     NA
    ## 28593             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28594             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28595             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28596             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28597            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28598            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28599             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28600             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28601             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28602             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28603             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28604            161      NA       NA       NA    ORD  AUS      978     NA
    ## 28605            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28606             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28607             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28608            175      NA       NA       NA    LAX  AUS     1242     NA
    ## 28609            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28610             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28611            180      NA       NA       NA    LAX  AUS     1242     NA
    ## 28612             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28613             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28614             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28615             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28616             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28617            155      NA       NA       NA    ORD  AUS      978     NA
    ## 28898             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28899            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28900             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28901             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28902             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28903            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28904            155      NA       NA       NA    AUS  ORD      978     NA
    ## 28905             65      NA       NA       NA    AUS  DFW      190     NA
    ## 28906             60      NA       NA       NA    AUS  DFW      190     NA
    ## 28907            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28908             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28909             50      NA       NA       NA    DFW  AUS      190     NA
    ## 28910             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28911            160      NA       NA       NA    ORD  AUS      978     NA
    ## 28912             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28913             55      NA       NA       NA    DFW  AUS      190     NA
    ## 28914            155      NA       NA       NA    ORD  AUS      978     NA
    ## 29131             60      NA       NA       NA    AUS  DFW      190     NA
    ## 29132            195      NA       NA       NA    AUS  LAX     1242     NA
    ## 29133             60      NA       NA       NA    AUS  DFW      190     NA
    ## 29134            155      NA       NA       NA    AUS  ORD      978     NA
    ## 29135            120      NA       NA       NA    MCI  AUS      650     NA
    ## 29136            160      NA       NA       NA    ORD  AUS      978     NA
    ## 29137             55      NA       NA       NA    DFW  AUS      190     NA
    ## 29138            180      NA       NA       NA    LAX  AUS     1242     NA
    ## 29139             55      NA       NA       NA    DFW  AUS      190     NA
    ## 29417            110      NA       NA       NA    AUS  MCI      650     NA
    ## 29418             60      NA       NA       NA    AUS  IAH      140     NA
    ## 29419            104      NA       NA       NA    ABQ  AUS      619     NA
    ## 29420            157      NA       NA       NA    LAS  AUS     1090     NA
    ## 29421             56      NA       NA       NA    IAH  AUS      140     NA
    ## 29715            149      NA       NA       NA    AUS  PHX      872     NA
    ## 30002             55      NA       NA       NA    AUS  DAL      189     NA
    ## 30003             50      NA       NA       NA    DAL  AUS      189     NA
    ## 30297            153      NA       NA       NA    JAX  AUS      954     NA
    ## 30589             55      NA       NA       NA    AUS  DAL      189     NA
    ## 30590             60      NA       NA       NA    AUS  DFW      190     NA
    ## 30591             50      NA       NA       NA    DAL  AUS      189     NA
    ## 30592             50      NA       NA       NA    DFW  AUS      190     NA
    ## 30593             55      NA       NA       NA    DFW  AUS      190     NA
    ## 30890             60      NA       NA       NA    AUS  DFW      190     NA
    ## 31113             65      NA       NA       NA    AUS  DFW      190     NA
    ## 31114             55      NA       NA       NA    DFW  AUS      190     NA
    ## 32270             55      NA       NA       NA    AUS  DAL      189     NA
    ## 32271             60      NA       NA       NA    AUS  DFW      190     NA
    ## 32272             55      NA       NA       NA    DAL  AUS      189     NA
    ## 32273             50      NA       NA       NA    DFW  AUS      190     NA
    ## 32566            230      NA       NA       NA    AUS  SFO     1504     NA
    ## 32567             55      NA       NA       NA    AUS  DAL      189     NA
    ## 32568             55      NA       NA       NA    AUS  DAL      189     NA
    ## 32569            208      NA       NA       NA    SFO  AUS     1504     NA
    ## 32570             55      NA       NA       NA    DAL  AUS      189     NA
    ## 32571             55      NA       NA       NA    DAL  AUS      189     NA
    ## 32863             55      NA       NA       NA    AUS  DAL      189     NA
    ## 32864             55      NA       NA       NA    AUS  DAL      189     NA
    ## 32865            155      NA       NA       NA    AUS  ORD      978     NA
    ## 32866            155      NA       NA       NA    AUS  ORD      978     NA
    ## 32867            162      NA       NA       NA    ORD  AUS      978     NA
    ## 32868             55      NA       NA       NA    DAL  AUS      189     NA
    ## 32869             55      NA       NA       NA    DAL  AUS      189     NA
    ## 32870            155      NA       NA       NA    ORD  AUS      978     NA
    ## 33095            158      NA       NA       NA    AUS  ORD      978     NA
    ## 33096            155      NA       NA       NA    ORD  AUS      978     NA
    ## 33674            192      NA       NA       NA    AUS  IAD     1297     NA
    ## 33675            210      NA       NA       NA    IAD  AUS     1297     NA
    ## 34259            192      NA       NA       NA    AUS  IAD     1297     NA
    ## 34260            105      NA       NA       NA    AUS  MEM      559     NA
    ## 34261            115      NA       NA       NA    MEM  AUS      559     NA
    ## 34864             55      NA       NA       NA    AUS  DAL      189     NA
    ## 34865            155      NA       NA       NA    AUS  ORD      978     NA
    ## 34866            155      NA       NA       NA    AUS  ORD      978     NA
    ## 34867             55      NA       NA       NA    DAL  AUS      189     NA
    ## 34868            160      NA       NA       NA    ORD  AUS      978     NA
    ## 35097             55      NA       NA       NA    DFW  AUS      190     NA
    ## 35386            155      NA       NA       NA    AUS  ORD      978     NA
    ## 35387            160      NA       NA       NA    ORD  AUS      978     NA
    ## 35686             55      NA       NA       NA    AUS  DAL      189     NA
    ## 35687             55      NA       NA       NA    AUS  DAL      189     NA
    ## 35688            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 35689             50      NA       NA       NA    DAL  AUS      189     NA
    ## 35690            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 35691             57      NA       NA       NA    IAH  AUS      140     NA
    ## 35980             55      NA       NA       NA    AUS  DAL      189     NA
    ## 35981             55      NA       NA       NA    AUS  DAL      189     NA
    ## 35982             55      NA       NA       NA    AUS  DAL      189     NA
    ## 35983             65      NA       NA       NA    AUS  DFW      190     NA
    ## 35984             61      NA       NA       NA    AUS  IAH      140     NA
    ## 35985             50      NA       NA       NA    DAL  AUS      189     NA
    ## 35986             50      NA       NA       NA    DAL  AUS      189     NA
    ## 35987             55      NA       NA       NA    DAL  AUS      189     NA
    ## 35988             55      NA       NA       NA    DFW  AUS      190     NA
    ## 36287            133      NA       NA       NA    AUS  ATL      813     NA
    ## 36288             60      NA       NA       NA    AUS  DFW      190     NA
    ## 36289             60      NA       NA       NA    AUS  DFW      190     NA
    ## 36290            145      NA       NA       NA    ATL  AUS      813     NA
    ## 36291             55      NA       NA       NA    DFW  AUS      190     NA
    ## 36292             55      NA       NA       NA    DFW  AUS      190     NA
    ## 36596            130      NA       NA       NA    AUS  DEN      775     NA
    ## 36597             55      NA       NA       NA    AUS  DAL      189     NA
    ## 36598             55      NA       NA       NA    DAL  AUS      189     NA
    ## 36904            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 36905            241      NA       NA       NA    AUS  JFK     1522     NA
    ## 37148            130      NA       NA       NA    AUS  DEN      775     NA
    ## 37149             55      NA       NA       NA    DFW  AUS      190     NA
    ## 37436            147      NA       NA       NA    AUS  CLT     1033     NA
    ## 37437            155      NA       NA       NA    AUS  ORD      978     NA
    ## 37438            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 37439            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 37440            155      NA       NA       NA    ORD  AUS      978     NA
    ## 38033             60      NA       NA       NA    AUS  DFW      190     NA
    ## 38034             62      NA       NA       NA    AUS  IAH      140     NA
    ## 38035            135      NA       NA       NA    PHX  AUS      872     NA
    ## 38036             55      NA       NA       NA    DFW  AUS      190     NA
    ## 38335             55      NA       NA       NA    AUS  DAL      189     NA
    ## 38336             55      NA       NA       NA    AUS  DAL      189     NA
    ## 38337             55      NA       NA       NA    AUS  DAL      189     NA
    ## 38338             60      NA       NA       NA    AUS  DFW      190     NA
    ## 38339             50      NA       NA       NA    DAL  AUS      189     NA
    ## 38340             50      NA       NA       NA    DAL  AUS      189     NA
    ## 38341             55      NA       NA       NA    DAL  AUS      189     NA
    ## 38342             55      NA       NA       NA    DFW  AUS      190     NA
    ## 38650             55      NA       NA       NA    AUS  DAL      189     NA
    ## 38958            147      NA       NA       NA    AUS  CLT     1033     NA
    ## 38959            104      NA       NA       NA    ABQ  AUS      619     NA
    ## 39204            155      NA       NA       NA    AUS  ORD      978     NA
    ## 39495             55      NA       NA       NA    AUS  DAL      189     NA
    ## 39496             55      NA       NA       NA    DAL  AUS      189     NA
    ## 40407             55      NA       NA       NA    AUS  DAL      189     NA
    ## 40408             60      NA       NA       NA    AUS  DFW      190     NA
    ## 40716            149      NA       NA       NA    AUS  PHX      872     NA
    ## 41526            110      NA       NA       NA    AUS  MCI      650     NA
    ## 41527            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 41528            120      NA       NA       NA    MCI  AUS      650     NA
    ## 41529            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 41806            158      NA       NA       NA    AUS  ORD      978     NA
    ## 41807             55      NA       NA       NA    AUS  DAL      189     NA
    ## 41808             50      NA       NA       NA    DAL  AUS      189     NA
    ## 42086            149      NA       NA       NA    AUS  PHX      872     NA
    ## 42087             55      NA       NA       NA    AUS  DAL      189     NA
    ## 42088             55      NA       NA       NA    AUS  DAL      189     NA
    ## 42089             55      NA       NA       NA    AUS  DAL      189     NA
    ## 42090             55      NA       NA       NA    AUS  DAL      189     NA
    ## 42091             55      NA       NA       NA    AUS  DAL      189     NA
    ## 42092             60      NA       NA       NA    AUS  DFW      190     NA
    ## 42093             60      NA       NA       NA    AUS  DFW      190     NA
    ## 42094             65      NA       NA       NA    AUS  DFW      190     NA
    ## 42095             60      NA       NA       NA    AUS  DFW      190     NA
    ## 42096             60      NA       NA       NA    AUS  DFW      190     NA
    ## 42097             60      NA       NA       NA    AUS  DFW      190     NA
    ## 42098             50      NA       NA       NA    DAL  AUS      189     NA
    ## 42099            136      NA       NA       NA    PHX  AUS      872     NA
    ## 42100            278      NA       NA       NA    JFK  AUS     1522     NA
    ## 42101             50      NA       NA       NA    DAL  AUS      189     NA
    ## 42102             55      NA       NA       NA    DAL  AUS      189     NA
    ## 42103             50      NA       NA       NA    DAL  AUS      189     NA
    ## 42104             55      NA       NA       NA    DAL  AUS      189     NA
    ## 42105             55      NA       NA       NA    DAL  AUS      189     NA
    ## 42106            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 42107             55      NA       NA       NA    DFW  AUS      190     NA
    ## 42108             50      NA       NA       NA    DFW  AUS      190     NA
    ## 42109             55      NA       NA       NA    DFW  AUS      190     NA
    ## 42110             50      NA       NA       NA    DFW  AUS      190     NA
    ## 42111             55      NA       NA       NA    DFW  AUS      190     NA
    ## 42112             55      NA       NA       NA    DFW  AUS      190     NA
    ## 42113             55      NA       NA       NA    DFW  AUS      190     NA
    ## 42114            266      NA       NA       NA    EWR  AUS     1504     NA
    ## 42413            130      NA       NA       NA    AUS  DEN      775     NA
    ## 42414             55      NA       NA       NA    AUS  DAL      189     NA
    ## 42415            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 42416            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 42417             60      NA       NA       NA    AUS  DFW      190     NA
    ## 42418            219      NA       NA       NA    AUS  EWR     1504     NA
    ## 42419             55      NA       NA       NA    DAL  AUS      189     NA
    ## 42420            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 42726            195      NA       NA       NA    AUS  LAX     1242     NA
    ## 42727            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 42728            180      NA       NA       NA    LAX  AUS     1242     NA
    ## 43034            158      NA       NA       NA    AUS  ORD      978     NA
    ## 43035            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 43036            155      NA       NA       NA    AUS  ORD      978     NA
    ## 43037            160      NA       NA       NA    ORD  AUS      978     NA
    ## 43281             60      NA       NA       NA    AUS  DFW      190     NA
    ## 43282             55      NA       NA       NA    DFW  AUS      190     NA
    ## 43569             55      NA       NA       NA    AUS  DAL      189     NA
    ## 43570             55      NA       NA       NA    AUS  DAL      189     NA
    ## 43571             60      NA       NA       NA    AUS  DFW      190     NA
    ## 43572             50      NA       NA       NA    DAL  AUS      189     NA
    ## 43573             50      NA       NA       NA    DAL  AUS      189     NA
    ## 43574             55      NA       NA       NA    DFW  AUS      190     NA
    ## 43879             55      NA       NA       NA    AUS  DAL      189     NA
    ## 43880             55      NA       NA       NA    DAL  AUS      189     NA
    ## 44179             60      NA       NA       NA    AUS  DFW      190     NA
    ## 44180             55      NA       NA       NA    DFW  AUS      190     NA
    ## 44485            154      NA       NA       NA    AUS  CLT     1033     NA
    ## 44797            164      NA       NA       NA    LAS  AUS     1090     NA
    ## 44798            142      NA       NA       NA    ATL  AUS      813     NA
    ## 45113            154      NA       NA       NA    AUS  CLT     1033     NA
    ## 45114            152      NA       NA       NA    ORD  AUS      978     NA
    ## 45363            161      NA       NA       NA    AUS  ORD      978     NA
    ## 45364            145      NA       NA       NA    AUS  PHX      872     NA
    ## 45365             65      NA       NA       NA    AUS  DFW      190     NA
    ## 45366             55      NA       NA       NA    DFW  AUS      190     NA
    ## 45664            160      NA       NA       NA    AUS  ORD      978     NA
    ## 45665            155      NA       NA       NA    ORD  AUS      978     NA
    ## 45970            110      NA       NA       NA    AUS  MCI      650     NA
    ## 45971             55      NA       NA       NA    AUS  DAL      189     NA
    ## 45972             55      NA       NA       NA    AUS  DAL      189     NA
    ## 45973             60      NA       NA       NA    AUS  DFW      190     NA
    ## 45974             60      NA       NA       NA    AUS  DFW      190     NA
    ## 45975            120      NA       NA       NA    MCI  AUS      650     NA
    ## 45976             50      NA       NA       NA    DAL  AUS      189     NA
    ## 45977             50      NA       NA       NA    DAL  AUS      189     NA
    ## 45978             55      NA       NA       NA    DFW  AUS      190     NA
    ## 45979             55      NA       NA       NA    DFW  AUS      190     NA
    ## 46289             55      NA       NA       NA    AUS  DAL      189     NA
    ## 46290            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 46291            199      NA       NA       NA    IAD  AUS     1297     NA
    ## 46292             50      NA       NA       NA    DAL  AUS      189     NA
    ## 46293            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 46603            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 46604             55      NA       NA       NA    AUS  DAL      189     NA
    ## 46605             55      NA       NA       NA    AUS  DAL      189     NA
    ## 46606             50      NA       NA       NA    DAL  AUS      189     NA
    ## 46607             50      NA       NA       NA    DAL  AUS      189     NA
    ## 46921            142      NA       NA       NA    AUS  PHX      872     NA
    ## 46922             55      NA       NA       NA    AUS  DAL      189     NA
    ## 46923             50      NA       NA       NA    DAL  AUS      189     NA
    ## 47239            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 47240            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 47797            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 48108             55      NA       NA       NA    AUS  DAL      189     NA
    ## 48109            227      NA       NA       NA    AUS  JFK     1522     NA
    ## 48110            241      NA       NA       NA    AUS  JFK     1522     NA
    ## 48111            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 48414             55      NA       NA       NA    AUS  DAL      189     NA
    ## 48415             55      NA       NA       NA    AUS  DAL      189     NA
    ## 48416             65      NA       NA       NA    AUS  DFW      190     NA
    ## 48417             65      NA       NA       NA    AUS  DFW      190     NA
    ## 48418             65      NA       NA       NA    AUS  DFW      190     NA
    ## 48419             65      NA       NA       NA    AUS  DFW      190     NA
    ## 48420             50      NA       NA       NA    DAL  AUS      189     NA
    ## 48421             50      NA       NA       NA    DAL  AUS      189     NA
    ## 48422             55      NA       NA       NA    DFW  AUS      190     NA
    ## 48423             55      NA       NA       NA    DFW  AUS      190     NA
    ## 48424             55      NA       NA       NA    DFW  AUS      190     NA
    ## 48425             55      NA       NA       NA    DFW  AUS      190     NA
    ## 48738             55      NA       NA       NA    AUS  DAL      189     NA
    ## 48739             50      NA       NA       NA    DAL  AUS      189     NA
    ## 49047             55      NA       NA       NA    AUS  DAL      189     NA
    ## 49048             55      NA       NA       NA    AUS  DAL      189     NA
    ## 49049             60      NA       NA       NA    AUS  DFW      190     NA
    ## 49050             65      NA       NA       NA    AUS  DFW      190     NA
    ## 49051             50      NA       NA       NA    DAL  AUS      189     NA
    ## 49052             50      NA       NA       NA    DAL  AUS      189     NA
    ## 49053             55      NA       NA       NA    DFW  AUS      190     NA
    ## 49054             55      NA       NA       NA    DFW  AUS      190     NA
    ## 49368            107      NA       NA       NA    AUS  ABQ      619     NA
    ## 49369            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 49370             86      NA       NA       NA    MSY  AUS      445     NA
    ## 49371            191      NA       NA       NA    IAD  AUS     1297     NA
    ## 49922            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 49923            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 49924             65      NA       NA       NA    AUS  DFW      190     NA
    ## 49925            237      NA       NA       NA    AUS  JFK     1522     NA
    ## 49926            191      NA       NA       NA    IAD  AUS     1297     NA
    ## 49927             55      NA       NA       NA    DFW  AUS      190     NA
    ## 49928            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 49929            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 50556             55      NA       NA       NA    AUS  DAL      189     NA
    ## 50557             50      NA       NA       NA    DAL  AUS      189     NA
    ## 51176            110      NA       NA       NA    AUS  MCI      650     NA
    ## 51177            190      NA       NA       NA    AUS  LAX     1242     NA
    ## 51178             55      NA       NA       NA    AUS  DAL      189     NA
    ## 51179            165      NA       NA       NA    AUS  IND      920     NA
    ## 51180             60      NA       NA       NA    AUS  IAH      140     NA
    ## 51181            120      NA       NA       NA    MCI  AUS      650     NA
    ## 51182            175      NA       NA       NA    LAX  AUS     1242     NA
    ## 51183             50      NA       NA       NA    DAL  AUS      189     NA
    ## 51184            160      NA       NA       NA    IND  AUS      920     NA
    ## 51185             55      NA       NA       NA    IAH  AUS      140     NA
    ## 51501             58      NA       NA       NA    AUS  IAH      140     NA
    ## 51502             55      NA       NA       NA    IAH  AUS      140     NA
    ## 51756             65      NA       NA       NA    AUS  DFW      190     NA
    ## 51757             50      NA       NA       NA    DFW  AUS      190     NA
    ## 51758             50      NA       NA       NA    DFW  AUS      190     NA
    ## 51759             50      NA       NA       NA    DFW  AUS      190     NA
    ## 51760             50      NA       NA       NA    DFW  AUS      190     NA
    ## 51761             50      NA       NA       NA    DFW  AUS      190     NA
    ## 52055             60      NA       NA       NA    AUS  DFW      190     NA
    ## 52056             60      NA       NA       NA    AUS  DFW      190     NA
    ## 52057             60      NA       NA       NA    AUS  DFW      190     NA
    ## 52058             60      NA       NA       NA    AUS  DFW      190     NA
    ## 52371            155      NA       NA       NA    AUS  ORD      978     NA
    ## 52372            155      NA       NA       NA    ORD  AUS      978     NA
    ## 52680            155      NA       NA       NA    AUS  ORD      978     NA
    ## 52681            205      NA       NA       NA    AUS  SJC     1476     NA
    ## 52682            199      NA       NA       NA    IAD  AUS     1297     NA
    ## 52683            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 52684            155      NA       NA       NA    ORD  AUS      978     NA
    ## 52981            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 53738            160      NA       NA       NA    AUS  ORD      978     NA
    ## 54024            104      NA       NA       NA    ABQ  AUS      619     NA
    ## 54025            155      NA       NA       NA    ORD  AUS      978     NA
    ## 54621            128      NA       NA       NA    AUS  DEN      775     NA
    ## 54622            155      NA       NA       NA    AUS  ORD      978     NA
    ## 54623            220      NA       NA       NA    AUS  SFO     1504     NA
    ## 54624             50      NA       NA       NA    DFW  AUS      190     NA
    ## 54906             60      NA       NA       NA    AUS  DFW      190     NA
    ## 54907            172      NA       NA       NA    AUS  FLL     1105     NA
    ## 54908            210      NA       NA       NA    SFO  AUS     1504     NA
    ## 55207            142      NA       NA       NA    AUS  PHX      872     NA
    ## 55208            160      NA       NA       NA    AUS  ORD      978     NA
    ## 55209            135      NA       NA       NA    PHX  AUS      872     NA
    ## 55210            150      NA       NA       NA    ORD  AUS      978     NA
    ## 55511             55      NA       NA       NA    AUS  DAL      189     NA
    ## 55512             65      NA       NA       NA    AUS  DFW      190     NA
    ## 55513             55      NA       NA       NA    DFW  AUS      190     NA
    ## 55769            190      NA       NA       NA    AUS  LAX     1242     NA
    ## 56049            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 56050            146      NA       NA       NA    AUS  ATL      813     NA
    ## 56051            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 56052            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 56053            141      NA       NA       NA    ATL  AUS      813     NA
    ## 56054            138      NA       NA       NA    ATL  AUS      813     NA
    ## 56353            133      NA       NA       NA    AUS  ATL      813     NA
    ## 56354            141      NA       NA       NA    AUS  ATL      813     NA
    ## 57559             55      NA       NA       NA    AUS  DAL      189     NA
    ## 57560            126      NA       NA       NA    DEN  AUS      775     NA
    ## 57813            225      NA       NA       NA    AUS  SFO     1504     NA
    ## 57814            185      NA       NA       NA    AUS  LAX     1242     NA
    ## 57815            185      NA       NA       NA    LAX  AUS     1242     NA
    ## 58095             60      NA       NA       NA    AUS  DFW      190     NA
    ## 58096            128      NA       NA       NA    DEN  AUS      775     NA
    ## 58097             55      NA       NA       NA    DFW  AUS      190     NA
    ## 58391            130      NA       NA       NA    AUS  DEN      775     NA
    ## 58392             58      NA       NA       NA    AUS  IAH      140     NA
    ## 58393             55      NA       NA       NA    IAH  AUS      140     NA
    ## 58685            150      NA       NA       NA    AUS  CVG      958     NA
    ## 58686            180      NA       NA       NA    AUS  SNA     1209     NA
    ## 58687             57      NA       NA       NA    AUS  IAH      140     NA
    ## 58688            138      NA       NA       NA    ATL  AUS      813     NA
    ## 58689             58      NA       NA       NA    IAH  AUS      140     NA
    ## 58979             60      NA       NA       NA    AUS  HRL      273     NA
    ## 58980            241      NA       NA       NA    AUS  JFK     1522     NA
    ## 58981             60      NA       NA       NA    HRL  AUS      273     NA
    ## 58982            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 58983            126      NA       NA       NA    DEN  AUS      775     NA
    ## 58984            232      NA       NA       NA    JFK  AUS     1522     NA
    ## 58985            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 59276             60      NA       NA       NA    AUS  HRL      273     NA
    ## 59277            225      NA       NA       NA    AUS  SFO     1504     NA
    ## 59278            227      NA       NA       NA    AUS  JFK     1522     NA
    ## 59279            185      NA       NA       NA    AUS  LGB     1226     NA
    ## 59280             65      NA       NA       NA    AUS  IAH      140     NA
    ## 59281             60      NA       NA       NA    HRL  AUS      273     NA
    ## 59282            180      NA       NA       NA    LGB  AUS     1226     NA
    ## 59283            245      NA       NA       NA    EWR  AUS     1504     NA
    ## 59579             60      NA       NA       NA    AUS  HRL      273     NA
    ## 59580             57      NA       NA       NA    AUS  IAH      140     NA
    ## 59581             60      NA       NA       NA    HRL  AUS      273     NA
    ## 59582             58      NA       NA       NA    IAH  AUS      140     NA
    ## 59832             60      NA       NA       NA    AUS  HRL      273     NA
    ## 59833             56      NA       NA       NA    AUS  IAH      140     NA
    ## 59834             60      NA       NA       NA    HRL  AUS      273     NA
    ## 59835             50      NA       NA       NA    IAH  AUS      140     NA
    ## 60112            133      NA       NA       NA    AUS  DEN      775     NA
    ## 60113            233      NA       NA       NA    AUS  JFK     1522     NA
    ## 60114            237      NA       NA       NA    AUS  JFK     1522     NA
    ## 60115            241      NA       NA       NA    AUS  JFK     1522     NA
    ## 60116            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 60117            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 60413            244      NA       NA       NA    JFK  AUS     1522     NA
    ## 60708             65      NA       NA       NA    AUS  DFW      190     NA
    ## 60709             55      NA       NA       NA    DFW  AUS      190     NA
    ## 60999            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 61000             65      NA       NA       NA    AUS  DFW      190     NA
    ## 61001             60      NA       NA       NA    AUS  DFW      190     NA
    ## 61002             50      NA       NA       NA    DFW  AUS      190     NA
    ## 61003             55      NA       NA       NA    DFW  AUS      190     NA
    ## 61004             50      NA       NA       NA    DFW  AUS      190     NA
    ## 61005            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 61301             60      NA       NA       NA    AUS  DFW      190     NA
    ## 61302            185      NA       NA       NA    AUS  LGB     1226     NA
    ## 61303            180      NA       NA       NA    LGB  AUS     1226     NA
    ## 61599             55      NA       NA       NA    AUS  DAL      189     NA
    ## 61842            233      NA       NA       NA    AUS  JFK     1522     NA
    ## 61843            155      NA       NA       NA    AUS  ORD      978     NA
    ## 61844            241      NA       NA       NA    AUS  JFK     1522     NA
    ## 61845            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 61846             50      NA       NA       NA    DAL  AUS      189     NA
    ## 61847            155      NA       NA       NA    ORD  AUS      978     NA
    ## 61848            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 62125             60      NA       NA       NA    AUS  DFW      190     NA
    ## 62126             55      NA       NA       NA    DFW  AUS      190     NA
    ## 62413             60      NA       NA       NA    AUS  DFW      190     NA
    ## 62414             60      NA       NA       NA    AUS  DFW      190     NA
    ## 62415            155      NA       NA       NA    AUS  ORD      978     NA
    ## 62416            150      NA       NA       NA    ORD  AUS      978     NA
    ## 62417             50      NA       NA       NA    DFW  AUS      190     NA
    ## 62418             50      NA       NA       NA    DFW  AUS      190     NA
    ## 62707             60      NA       NA       NA    AUS  IAH      140     NA
    ## 62708             56      NA       NA       NA    AUS  IAH      140     NA
    ## 62709             60      NA       NA       NA    IAH  AUS      140     NA
    ## 62710             54      NA       NA       NA    IAH  AUS      140     NA
    ## 63000            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 63001             65      NA       NA       NA    AUS  DFW      190     NA
    ## 63002             55      NA       NA       NA    DFW  AUS      190     NA
    ## 63294            241      NA       NA       NA    AUS  JFK     1522     NA
    ## 63295            220      NA       NA       NA    AUS  SFO     1504     NA
    ## 63296            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 63589            185      NA       NA       NA    AUS  LGB     1226     NA
    ## 63590            210      NA       NA       NA    SFO  AUS     1504     NA
    ## 64113            155      NA       NA       NA    AUS  ORD      978     NA
    ## 64114             60      NA       NA       NA    AUS  DFW      190     NA
    ## 64115            199      NA       NA       NA    IAD  AUS     1297     NA
    ## 64116             55      NA       NA       NA    DFW  AUS      190     NA
    ## 64117            155      NA       NA       NA    ORD  AUS      978     NA
    ## 64404            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 64405             65      NA       NA       NA    AUS  DFW      190     NA
    ## 64406            237      NA       NA       NA    AUS  JFK     1522     NA
    ## 64407            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 64408             55      NA       NA       NA    DFW  AUS      190     NA
    ## 64409            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 64698            160      NA       NA       NA    AUS  ORD      978     NA
    ## 64699            172      NA       NA       NA    AUS  FLL     1105     NA
    ## 64700            150      NA       NA       NA    ORD  AUS      978     NA
    ## 64701            179      NA       NA       NA    FLL  AUS     1105     NA
    ## 64990            142      NA       NA       NA    AUS  PHX      872     NA
    ## 64991            179      NA       NA       NA    LAX  AUS     1242     NA
    ## 65282             65      NA       NA       NA    AUS  DFW      190     NA
    ## 65283            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 65284             55      NA       NA       NA    DFW  AUS      190     NA
    ## 65285            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 65571            133      NA       NA       NA    AUS  ATL      813     NA
    ## 65572            141      NA       NA       NA    AUS  ATL      813     NA
    ## 65573             65      NA       NA       NA    AUS  DFW      190     NA
    ## 65574             60      NA       NA       NA    AUS  DFW      190     NA
    ## 65575            227      NA       NA       NA    AUS  JFK     1522     NA
    ## 65576            237      NA       NA       NA    AUS  JFK     1522     NA
    ## 65577             60      NA       NA       NA    AUS  IAH      140     NA
    ## 65578            144      NA       NA       NA    ATL  AUS      813     NA
    ## 65579             55      NA       NA       NA    DFW  AUS      190     NA
    ## 65580             55      NA       NA       NA    DFW  AUS      190     NA
    ## 65581            254      NA       NA       NA    JFK  AUS     1522     NA
    ## 65582             55      NA       NA       NA    IAH  AUS      140     NA
    ## 65821             60      NA       NA       NA    AUS  DFW      190     NA
    ## 66084            205      NA       NA       NA    AUS  SJC     1476     NA
    ## 66085            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 66664            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 66665            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 66951             55      NA       NA       NA    AUS  DAL      189     NA
    ## 66952            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 66953            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 67246             58      NA       NA       NA    AUS  IAH      140     NA
    ## 67247             55      NA       NA       NA    IAH  AUS      140     NA
    ## 67541            141      NA       NA       NA    AUS  JAX      954     NA
    ## 67542            153      NA       NA       NA    JAX  AUS      954     NA
    ## 67767             65      NA       NA       NA    AUS  DFW      190     NA
    ## 67768             55      NA       NA       NA    DFW  AUS      190     NA
    ## 68034            177      NA       NA       NA    CLE  AUS     1174     NA
    ## 68316            196      NA       NA       NA    AUS  IAD     1297     NA
    ## 68317            170      NA       NA       NA    CLT  AUS     1033     NA
    ## 68590            154      NA       NA       NA    AUS  CLT     1033     NA
    ## 68591            137      NA       NA       NA    AUS  ATL      813     NA
    ## 68592            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 68593            143      NA       NA       NA    ATL  AUS      813     NA
    ## 68594            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 68595            210      NA       NA       NA    SJC  AUS     1476     NA
    ## 68874            161      NA       NA       NA    AUS  CVG      958     NA
    ## 68875            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 69159            130      NA       NA       NA    AUS  DEN      775     NA
    ## 69160             60      NA       NA       NA    AUS  DFW      190     NA
    ## 69161             55      NA       NA       NA    DFW  AUS      190     NA
    ## 69446             60      NA       NA       NA    AUS  DFW      190     NA
    ## 69447             65      NA       NA       NA    AUS  DFW      190     NA
    ## 69448             55      NA       NA       NA    DFW  AUS      190     NA
    ## 69854            160      NA       NA       NA    AUS  ORD      978     NA
    ## 69855            155      NA       NA       NA    ORD  AUS      978     NA
    ## 69856            205      NA       NA       NA    SJC  AUS     1476     NA
    ## 70120             78      NA       NA       NA    AUS  MSY      445     NA
    ## 70121            210      NA       NA       NA    AUS  SJC     1476     NA
    ## 70122             90      NA       NA       NA    MSY  AUS      445     NA
    ## 70123             55      NA       NA       NA    DFW  AUS      190     NA
    ## 71169             55      NA       NA       NA    AUS  DAL      189     NA
    ## 71170            215      NA       NA       NA    AUS  SJC     1476     NA
    ## 71171            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 71361            255      NA       NA       NA    AUS  JFK     1522     NA
    ## 71362            269      NA       NA       NA    JFK  AUS     1522     NA
    ## 71607             55      NA       NA       NA    AUS  DAL      189     NA
    ## 71868             NA      NA       NA       NA    AUS  ATL      813     NA
    ## 71869             NA      NA       NA       NA    ATL  AUS      813     NA
    ## 72123             90      NA       NA       NA    ELP  AUS      528     NA
    ## 72873             50      NA       NA       NA    AUS  HOU      148     NA
    ## 72874             45      NA       NA       NA    AUS  HOU      148     NA
    ## 72875             50      NA       NA       NA    AUS  HOU      148     NA
    ## 72876             50      NA       NA       NA    AUS  HOU      148     NA
    ## 72877             50      NA       NA       NA    AUS  HOU      148     NA
    ## 72878             45      NA       NA       NA    AUS  HOU      148     NA
    ## 72879             60      NA       NA       NA    AUS  HRL      273     NA
    ## 72880             59      NA       NA       NA    AUS  IAH      140     NA
    ## 72881             60      NA       NA       NA    AUS  IAH      140     NA
    ## 72882             60      NA       NA       NA    AUS  IAH      140     NA
    ## 72883             53      NA       NA       NA    AUS  IAH      140     NA
    ## 72884             60      NA       NA       NA    AUS  IAH      140     NA
    ## 72885             60      NA       NA       NA    AUS  IAH      140     NA
    ## 72886             60      NA       NA       NA    AUS  IAH      140     NA
    ## 72887             45      NA       NA       NA    HOU  AUS      148     NA
    ## 72888             45      NA       NA       NA    HOU  AUS      148     NA
    ## 72889             45      NA       NA       NA    HOU  AUS      148     NA
    ## 72890             45      NA       NA       NA    HOU  AUS      148     NA
    ## 72891             45      NA       NA       NA    HOU  AUS      148     NA
    ## 72892             60      NA       NA       NA    HRL  AUS      273     NA
    ## 72893            176      NA       NA       NA    CLT  AUS     1033     NA
    ## 72894            133      NA       NA       NA    PHX  AUS      872     NA
    ## 72895             55      NA       NA       NA    IAH  AUS      140     NA
    ## 72896             56      NA       NA       NA    IAH  AUS      140     NA
    ## 72897             55      NA       NA       NA    IAH  AUS      140     NA
    ## 72898             50      NA       NA       NA    IAH  AUS      140     NA
    ## 72899             60      NA       NA       NA    IAH  AUS      140     NA
    ## 72900             56      NA       NA       NA    IAH  AUS      140     NA
    ## 72901             53      NA       NA       NA    IAH  AUS      140     NA
    ## 73050             55      NA       NA       NA    AUS  DAL      189     NA
    ## 73051             55      NA       NA       NA    AUS  DAL      189     NA
    ## 73052             55      NA       NA       NA    AUS  DAL      189     NA
    ## 73053             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73054             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73055             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73056             55      NA       NA       NA    AUS  HOU      148     NA
    ## 73057             60      NA       NA       NA    AUS  HRL      273     NA
    ## 73058            154      NA       NA       NA    AUS  CLT     1033     NA
    ## 73059            145      NA       NA       NA    AUS  PHX      872     NA
    ## 73060             60      NA       NA       NA    AUS  DFW      190     NA
    ## 73061             65      NA       NA       NA    AUS  DFW      190     NA
    ## 73062             60      NA       NA       NA    AUS  DFW      190     NA
    ## 73063            149      NA       NA       NA    AUS  ORD      978     NA
    ## 73064             52      NA       NA       NA    AUS  IAH      140     NA
    ## 73065             58      NA       NA       NA    AUS  IAH      140     NA
    ## 73066             51      NA       NA       NA    AUS  IAH      140     NA
    ## 73067             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73068             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73069             57      NA       NA       NA    AUS  IAH      140     NA
    ## 73070             56      NA       NA       NA    AUS  IAH      140     NA
    ## 73071             58      NA       NA       NA    AUS  IAH      140     NA
    ## 73072             50      NA       NA       NA    DAL  AUS      189     NA
    ## 73073             50      NA       NA       NA    DAL  AUS      189     NA
    ## 73074             50      NA       NA       NA    DAL  AUS      189     NA
    ## 73075             50      NA       NA       NA    DAL  AUS      189     NA
    ## 73076             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73077             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73078             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73079             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73080             60      NA       NA       NA    HRL  AUS      273     NA
    ## 73081             55      NA       NA       NA    DFW  AUS      190     NA
    ## 73082             55      NA       NA       NA    DFW  AUS      190     NA
    ## 73083             55      NA       NA       NA    DFW  AUS      190     NA
    ## 73084            160      NA       NA       NA    ORD  AUS      978     NA
    ## 73085             51      NA       NA       NA    IAH  AUS      140     NA
    ## 73086             48      NA       NA       NA    IAH  AUS      140     NA
    ## 73087             47      NA       NA       NA    IAH  AUS      140     NA
    ## 73088             49      NA       NA       NA    IAH  AUS      140     NA
    ## 73089             50      NA       NA       NA    IAH  AUS      140     NA
    ## 73090             51      NA       NA       NA    IAH  AUS      140     NA
    ## 73091             55      NA       NA       NA    IAH  AUS      140     NA
    ## 73092             55      NA       NA       NA    IAH  AUS      140     NA
    ## 73307             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73308             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73309             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73310             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73311             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73312             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73313             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73314             59      NA       NA       NA    AUS  IAH      140     NA
    ## 73315             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73316             58      NA       NA       NA    AUS  IAH      140     NA
    ## 73317             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73318             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73319             53      NA       NA       NA    AUS  IAH      140     NA
    ## 73320             56      NA       NA       NA    AUS  IAH      140     NA
    ## 73321             60      NA       NA       NA    AUS  IAH      140     NA
    ## 73322             58      NA       NA       NA    AUS  IAH      140     NA
    ## 73323             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73324             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73325             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73326             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73327             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73328             48      NA       NA       NA    IAH  AUS      140     NA
    ## 73329             56      NA       NA       NA    IAH  AUS      140     NA
    ## 73330             55      NA       NA       NA    IAH  AUS      140     NA
    ## 73331             49      NA       NA       NA    IAH  AUS      140     NA
    ## 73332             56      NA       NA       NA    IAH  AUS      140     NA
    ## 73333             55      NA       NA       NA    IAH  AUS      140     NA
    ## 73334             50      NA       NA       NA    IAH  AUS      140     NA
    ## 73335             60      NA       NA       NA    IAH  AUS      140     NA
    ## 73336             47      NA       NA       NA    IAH  AUS      140     NA
    ## 73337             54      NA       NA       NA    IAH  AUS      140     NA
    ## 73584             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73585             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73586             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73587             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73588             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73589             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73590             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73591             52      NA       NA       NA    AUS  IAH      140     NA
    ## 73592             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73593             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73594             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73595             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73596             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73597             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73598             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73599             52      NA       NA       NA    IAH  AUS      140     NA
    ## 73848             50      NA       NA       NA    AUS  HOU      148     NA
    ## 73849             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73850             45      NA       NA       NA    AUS  HOU      148     NA
    ## 73851             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73852             45      NA       NA       NA    HOU  AUS      148     NA
    ## 73853             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74105             50      NA       NA       NA    AUS  HOU      148     NA
    ## 74106             45      NA       NA       NA    AUS  HOU      148     NA
    ## 74107             45      NA       NA       NA    AUS  HOU      148     NA
    ## 74108             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74109             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74110             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74367             50      NA       NA       NA    AUS  HOU      148     NA
    ## 74368             45      NA       NA       NA    AUS  HOU      148     NA
    ## 74369             45      NA       NA       NA    AUS  HOU      148     NA
    ## 74370             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74371             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74372             45      NA       NA       NA    HOU  AUS      148     NA
    ## 74634             55      NA       NA       NA    AUS  DAL      189     NA
    ## 74635             45      NA       NA       NA    HOU  AUS      148     NA
    ## 76365            230      NA       NA       NA    AUS  JFK     1522     NA
    ## 76366            263      NA       NA       NA    JFK  AUS     1522     NA
    ## 77574             55      NA       NA       NA    AUS  DAL      189     NA
    ## 77925            200      NA       NA      115    SJC  AUS     1476     NA
    ## 78095            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 78096             60      NA       NA       NA    AUS  DFW      190     NA
    ## 78097             55      NA       NA       NA    DFW  AUS      190     NA
    ## 79046            159      NA       NA       NA    ORD  AUS      978     NA
    ## 79047            159      NA       NA       NA    ORD  AUS      978     NA
    ## 79300            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 79301            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 79562            105      NA       NA       NA    AUS  MEM      559     NA
    ## 79563            115      NA       NA       NA    MEM  AUS      559     NA
    ## 79923            123      NA       NA       -4    DEN  AUS      775     NA
    ## 80013            190      NA       NA       NA    AUS  IAD     1297     NA
    ## 80772            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 80773            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 81474            190      NA       NA       -8    AUS  IAD     1297     NA
    ## 82245             60      NA       NA       NA    AUS  DFW      190     NA
    ## 82496             55      NA       NA       NA    DFW  AUS      190     NA
    ## 82746            135      NA       NA       NA    AUS  DEN      775     NA
    ## 82747             70      NA       NA       NA    AUS  LBB      341     NA
    ## 82748             60      NA       NA       NA    IAH  AUS      140     NA
    ## 82749             55      NA       NA       NA    IAH  AUS      140     NA
    ## 83010             50      NA       NA       NA    AUS  IAH      140     NA
    ## 83011             60      NA       NA       NA    AUS  IAH      140     NA
    ## 83463            263      NA       NA       NA    JFK  AUS     1522     NA
    ## 83707            222      NA       NA       NA    AUS  JFK     1522     NA
    ## 84219            220      NA       NA       NA    AUS  SJC     1476     NA
    ## 84220            150      NA       NA       NA    ORD  AUS      978     NA
    ## 84221            200      NA       NA       NA    SJC  AUS     1476     NA
    ## 84734            204      NA       NA      213    IAD  AUS     1297     NA
    ## 85913             65      NA       NA       NA    AUS  DFW      190     NA
    ## 86411             60      NA       NA       NA    DFW  AUS      190     NA
    ## 87334             65      NA       NA       NA    AUS  DFW      190     NA
    ## 87335             60      NA       NA       NA    DFW  AUS      190     NA
    ## 87336             60      NA       NA       NA    DFW  AUS      190     NA
    ## 87337             55      NA       NA       NA    DFW  AUS      190     NA
    ## 87580            155      NA       NA       NA    AUS  PHX      872     NA
    ## 87581             65      NA       NA       NA    AUS  DFW      190     NA
    ## 87582             65      NA       NA       NA    AUS  DFW      190     NA
    ## 87583             50      NA       NA       NA    DAL  AUS      189     NA
    ## 87828             65      NA       NA       NA    AUS  DFW      190     NA
    ## 87829             60      NA       NA       NA    DFW  AUS      190     NA
    ## 88751             55      NA       NA       NA    AUS  DAL      189     NA
    ## 88752             50      NA       NA       NA    DAL  AUS      189     NA
    ## 89493             55      NA       NA       NA    AUS  DAL      189     NA
    ## 89494             50      NA       NA       NA    AUS  HOU      148     NA
    ## 89495             45      NA       NA       NA    HOU  AUS      148     NA
    ## 89496            130      NA       NA       NA    PHX  AUS      872     NA
    ## 89748            157      NA       NA       NA    AUS  ORD      978     NA
    ## 91182            139      NA       NA       NA    AUS  DEN      775     NA
    ## 91762             55      NA       NA       NA    AUS  DAL      189     NA
    ## 91763             50      NA       NA       NA    DAL  AUS      189     NA
    ## 93003             65      NA       NA       NA    AUS  DFW      190     NA
    ## 93251             60      NA       NA       NA    DFW  AUS      190     NA
    ## 94150             98      NA       NA       NA    AUS  MEM      559     NA
    ## 94151             97      NA       NA       NA    AUS  MEM      559     NA
    ## 94152            112      NA       NA       NA    MEM  AUS      559     NA
    ## 94153            115      NA       NA       NA    MEM  AUS      559     NA
    ## 94154             60      NA       NA       NA    DFW  AUS      190     NA
    ## 94155            175      NA       NA       NA    ORD  AUS      978     NA
    ## 94388             63      NA       NA      114    IAH  AUS      140     NA
    ## 94392            235      NA       NA       NA    AUS  SJC     1476     NA
    ## 94393             65      NA       NA       NA    AUS  DFW      190     NA
    ## 94394            155      NA       NA       NA    AUS  ORD      978     NA
    ## 94395             65      NA       NA       NA    AUS  IAH      140     NA
    ## 94396             48      NA       NA       NA    IAH  AUS      140     NA
    ## 94397            195      NA       NA       NA    SJC  AUS     1476     NA
    ## 94644             60      NA       NA       NA    AUS  IAH      140     NA
    ## 94645             49      NA       NA       NA    AUS  IAH      140     NA
    ## 94893            155      NA       NA       NA    AUS  PHX      872     NA
    ## 94894             50      NA       NA       NA    DAL  AUS      189     NA
    ## 95200             60      NA       NA        3    DFW  AUS      190     NA
    ## 95308             98      NA       NA       NA    AUS  MEM      559     NA
    ## 95309             65      NA       NA       NA    AUS  DFW      190     NA
    ## 95310            115      NA       NA       NA    MEM  AUS      559     NA
    ## 95471            155      NA       NA      104    AUS  IND      920     NA
    ## 95556             65      NA       NA       NA    AUS  DFW      190     NA
    ## 95557             57      NA       NA       NA    AUS  IAH      140     NA
    ## 95558            163      NA       NA       NA    SLC  AUS     1085     NA
    ## 95559             60      NA       NA       NA    DFW  AUS      190     NA
    ## 95560             53      NA       NA       NA    IAH  AUS      140     NA
    ## 95796             45      NA       NA       NA    AUS  HOU      148     NA
    ## 95797             50      NA       NA       NA    AUS  HOU      148     NA
    ## 95798            190      NA       NA       NA    AUS  SLC     1085     NA
    ## 95799            164      NA       NA       NA    AUS  ORD      978     NA
    ## 95800            165      NA       NA       NA    AUS  ORD      978     NA
    ## 95801            243      NA       NA       NA    AUS  EWR     1504     NA
    ## 95802            228      NA       NA       NA    AUS  EWR     1504     NA
    ## 95803             45      NA       NA       NA    HOU  AUS      148     NA
    ## 95804             45      NA       NA       NA    HOU  AUS      148     NA
    ## 95805            175      NA       NA       NA    ORD  AUS      978     NA
    ## 95806            256      NA       NA       NA    EWR  AUS     1504     NA
    ## 96051            155      NA       NA       NA    LAS  AUS     1090     NA
    ## 96052            250      NA       NA       NA    EWR  AUS     1504     NA
    ## 96301             57      NA       NA       NA    AUS  IAH      140     NA
    ## 96302            165      NA       NA       NA    AUS  ORD      978     NA
    ## 96303            220      NA       NA       NA    AUS  EWR     1504     NA
    ## 96304             54      NA       NA       NA    IAH  AUS      140     NA
    ## 96549            136      NA       NA       NA    AUS  DEN      775     NA
    ## 96550            224      NA       NA       NA    AUS  JFK     1522     NA
    ## 96551            165      NA       NA       NA    ORD  AUS      978     NA
    ## 96552             60      NA       NA       NA    DFW  AUS      190     NA
    ## 96553            170      NA       NA       NA    ORD  AUS      978     NA
    ## 96554            279      NA       NA       NA    JFK  AUS     1522     NA
    ## 96555            260      NA       NA       NA    EWR  AUS     1504     NA
    ## 96556             50      NA       NA       NA    IAH  AUS      140     NA
    ## 96745            130      NA       NA       NA    AUS  ATL      813     NA
    ## 96746            155      NA       NA       NA    AUS  IND      920     NA
    ## 96747             57      NA       NA       NA    AUS  IAH      140     NA
    ## 96748            150      NA       NA       NA    IND  AUS      920     NA
    ## 96973             55      NA       NA       NA    AUS  DAL      189     NA
    ## 96974             65      NA       NA       NA    AUS  DFW      190     NA
    ## 96975            231      NA       NA       NA    AUS  BOS     1698     NA
    ## 96976             50      NA       NA       NA    DAL  AUS      189     NA
    ## 96977             60      NA       NA       NA    DFW  AUS      190     NA
    ## 96978            281      NA       NA       NA    BOS  AUS     1698     NA
    ## 97227            155      NA       NA       NA    AUS  IND      920     NA
    ## 97228            150      NA       NA       NA    IND  AUS      920     NA
    ## 97472             45      NA       NA       NA    AUS  HOU      148     NA
    ## 97473            165      NA       NA       NA    AUS  ORD      978     NA
    ## 97474             45      NA       NA       NA    HOU  AUS      148     NA
    ## 97475            165      NA       NA       NA    ORD  AUS      978     NA
    ## 97476            170      NA       NA       NA    ORD  AUS      978     NA
    ## 97678            224      NA       NA       NA    AUS  JFK     1522     NA
    ## 97679            257      NA       NA       NA    JFK  AUS     1522     NA
    ## 98125            155      NA       NA       NA    AUS  MDW      972     NA
    ## 98126             65      NA       NA       NA    AUS  DFW      190     NA
    ## 98127            165      NA       NA       NA    MDW  AUS      972     NA
    ## 98128            175      NA       NA       NA    ORD  AUS      978     NA
    ## 98330             55      NA       NA       NA    AUS  HOU      148     NA
    ## 98331            155      NA       NA       NA    AUS  ORD      978     NA
    ## 98332            165      NA       NA       NA    MDW  AUS      972     NA
    ## 98556            165      NA       NA       NA    ORD  AUS      978     NA
    ## 98804             55      NA       NA       NA    AUS  DAL      189     NA
    ## 99048             50      NA       NA       NA    AUS  HOU      148     NA
    ## 99049             65      NA       NA       NA    AUS  DFW      190     NA
    ## 99050             50      NA       NA       NA    HOU  AUS      148     NA
    ## 99259            231      NA       NA       NA    AUS  BOS     1698     NA
    ## 99260            257      NA       NA       NA    JFK  AUS     1522     NA
    ##       TaxiOut Cancelled CancellationCode Diverted CarrierDelay
    ## 250        NA         1                B        0           NA
    ## 251        NA         1                A        0           NA
    ## 252        NA         1                A        0           NA
    ## 253        NA         1                A        0           NA
    ## 254        NA         1                A        0           NA
    ## 255        NA         1                A        0           NA
    ## 552        NA         1                A        0           NA
    ## 553        NA         1                A        0           NA
    ## 554        NA         1                A        0           NA
    ## 555        NA         1                A        0           NA
    ## 849        NA         1                A        0           NA
    ## 850        NA         1                A        0           NA
    ## 851        NA         1                A        0           NA
    ## 852        NA         1                A        0           NA
    ## 853        NA         1                A        0           NA
    ## 854        NA         1                A        0           NA
    ## 1151       NA         1                A        0           NA
    ## 1152       NA         1                A        0           NA
    ## 1153       NA         1                A        0           NA
    ## 1154       NA         1                A        0           NA
    ## 1674       NA         1                A        0           NA
    ## 1675       NA         1                C        0           NA
    ## 1676       NA         1                C        0           NA
    ## 2259       NA         1                C        0           NA
    ## 2550       NA         1                A        0           NA
    ## 2551       NA         1                A        0           NA
    ## 2846       NA         1                A        0           NA
    ## 2847       NA         1                A        0           NA
    ## 2848       NA         1                C        0           NA
    ## 2849       NA         1                A        0           NA
    ## 3147       NA         1                A        0           NA
    ## 3370       NA         1                A        0           NA
    ## 3643       NA         1                B        0           NA
    ## 3929       NA         1                A        0           NA
    ## 3930       NA         1                A        0           NA
    ## 3931       NA         1                A        0           NA
    ## 3932       NA         1                A        0           NA
    ## 3933       NA         1                A        0           NA
    ## 3934       NA         1                A        0           NA
    ## 3935       NA         1                A        0           NA
    ## 3936       NA         1                A        0           NA
    ## 3937       NA         1                A        0           NA
    ## 3938       NA         1                A        0           NA
    ## 3939       NA         1                B        0           NA
    ## 4222       NA         1                A        0           NA
    ## 4223       NA         1                A        0           NA
    ## 4224       NA         1                A        0           NA
    ## 4225       NA         1                A        0           NA
    ## 4514       NA         1                B        0           NA
    ## 4515       NA         1                A        0           NA
    ## 4516       NA         1                B        0           NA
    ## 4517       NA         1                B        0           NA
    ## 4810       NA         1                C        0           NA
    ## 4811       NA         1                B        0           NA
    ## 4812       NA         1                A        0           NA
    ## 4813       NA         1                B        0           NA
    ## 4814       NA         1                B        0           NA
    ## 4815       NA         1                A        0           NA
    ## 5114       NA         1                A        0           NA
    ## 5320       NA         1                B        0           NA
    ## 5321       NA         1                B        0           NA
    ## 5322       NA         1                B        0           NA
    ## 5323       NA         1                B        0           NA
    ## 5324       NA         1                B        0           NA
    ## 5325       NA         1                B        0           NA
    ## 5326       NA         1                B        0           NA
    ## 5593       NA         1                A        0           NA
    ## 5594       NA         1                A        0           NA
    ## 5595       NA         1                A        0           NA
    ## 5596       NA         1                A        0           NA
    ## 5892       NA         1                C        0           NA
    ## 5893       NA         1                C        0           NA
    ## 5894       NA         1                B        0           NA
    ## 6175       NA         1                B        0           NA
    ## 6176       NA         1                B        0           NA
    ## 6177       NA         1                A        0           NA
    ## 6178       NA         1                B        0           NA
    ## 6179       NA         1                B        0           NA
    ## 6180       NA         1                A        0           NA
    ## 6470       NA         1                A        0           NA
    ## 6471       NA         1                A        0           NA
    ## 6472       NA         1                A        0           NA
    ## 6767       NA         1                A        0           NA
    ## 6768       NA         1                A        0           NA
    ## 6769       NA         1                B        0           NA
    ## 6770       NA         1                B        0           NA
    ## 6771       NA         1                A        0           NA
    ## 6772       NA         1                A        0           NA
    ## 7066       NA         1                A        0           NA
    ## 7067       NA         1                B        0           NA
    ## 7068       NA         1                A        0           NA
    ## 7069       NA         1                B        0           NA
    ## 7070       NA         1                B        0           NA
    ## 7280       NA         1                B        0           NA
    ## 7281       NA         1                B        0           NA
    ## 7282       NA         1                B        0           NA
    ## 7546       NA         1                B        0           NA
    ## 7547       NA         1                B        0           NA
    ## 7548       NA         1                B        0           NA
    ## 7549       NA         1                B        0           NA
    ## 7550       NA         1                A        0           NA
    ## 7551       NA         1                B        0           NA
    ## 7552       NA         1                B        0           NA
    ## 7553       NA         1                B        0           NA
    ## 7842       NA         1                A        0           NA
    ## 7843       NA         1                B        0           NA
    ## 7844       NA         1                C        0           NA
    ## 7845       NA         1                C        0           NA
    ## 7846       NA         1                C        0           NA
    ## 7847       NA         1                A        0           NA
    ## 7848       NA         1                C        0           NA
    ## 7849       NA         1                C        0           NA
    ## 8122       NA         1                A        0           NA
    ## 8123       NA         1                B        0           NA
    ## 8124       NA         1                C        0           NA
    ## 8125       NA         1                C        0           NA
    ## 8126       NA         1                C        0           NA
    ## 8127       NA         1                C        0           NA
    ## 8128       NA         1                A        0           NA
    ## 8129       NA         1                A        0           NA
    ## 8130       NA         1                B        0           NA
    ## 8131       NA         1                C        0           NA
    ## 8132       NA         1                C        0           NA
    ## 8133       NA         1                C        0           NA
    ## 8134       NA         1                B        0           NA
    ## 8135       NA         1                A        0           NA
    ## 8424       NA         1                A        0           NA
    ## 8425       NA         1                B        0           NA
    ## 8426       NA         1                A        0           NA
    ## 8427       NA         1                C        0           NA
    ## 8719       NA         1                C        0           NA
    ## 8720       NA         1                C        0           NA
    ## 8721       NA         1                C        0           NA
    ## 8722       NA         1                A        0           NA
    ## 8723       NA         1                A        0           NA
    ## 8724       NA         1                A        0           NA
    ## 8725       NA         1                C        0           NA
    ## 8726       NA         1                C        0           NA
    ## 9023       NA         1                A        0           NA
    ## 9024       NA         1                B        0           NA
    ## 9226       NA         1                A        0           NA
    ## 9227       NA         1                A        0           NA
    ## 9228       NA         1                A        0           NA
    ## 9229       NA         1                B        0           NA
    ## 9230       NA         1                A        0           NA
    ## 9231       NA         1                A        0           NA
    ## 9232       NA         1                B        0           NA
    ## 9233       NA         1                A        0           NA
    ## 9499       NA         1                A        0           NA
    ## 9500       NA         1                A        0           NA
    ## 9501       NA         1                A        0           NA
    ## 9502       NA         1                A        0           NA
    ## 9503       NA         1                B        0           NA
    ## 9785       NA         1                A        0           NA
    ## 9786       NA         1                B        0           NA
    ## 9787       NA         1                C        0           NA
    ## 9788       NA         1                A        0           NA
    ## 9789       NA         1                B        0           NA
    ## 9790       NA         1                B        0           NA
    ## 9791       NA         1                B        0           NA
    ## 9792       NA         1                B        0           NA
    ## 9793       NA         1                B        0           NA
    ## 9794       NA         1                B        0           NA
    ## 9795       NA         1                B        0           NA
    ## 9796       NA         1                B        0           NA
    ## 9797       NA         1                A        0           NA
    ## 9798       NA         1                B        0           NA
    ## 9799       NA         1                B        0           NA
    ## 10078      NA         1                A        0           NA
    ## 10079      NA         1                A        0           NA
    ## 10080      NA         1                C        0           NA
    ## 10081      NA         1                B        0           NA
    ## 10082      NA         1                A        0           NA
    ## 10083      NA         1                A        0           NA
    ## 10084      NA         1                C        0           NA
    ## 10085      NA         1                A        0           NA
    ## 10360      NA         1                A        0           NA
    ## 10361      NA         1                B        0           NA
    ## 10362      NA         1                B        0           NA
    ## 10363      NA         1                A        0           NA
    ## 10364      NA         1                B        0           NA
    ## 10365      NA         1                B        0           NA
    ## 10366      NA         1                A        0           NA
    ## 10367      NA         1                B        0           NA
    ## 10368      NA         1                B        0           NA
    ## 10369      NA         1                B        0           NA
    ## 10370      NA         1                B        0           NA
    ## 10371      NA         1                C        0           NA
    ## 10372      NA         1                B        0           NA
    ## 10373      NA         1                B        0           NA
    ## 10374      NA         1                B        0           NA
    ## 10375      NA         1                B        0           NA
    ## 10376      NA         1                B        0           NA
    ## 10377      NA         1                B        0           NA
    ## 10672      NA         1                A        0           NA
    ## 10673      NA         1                A        0           NA
    ## 10674      NA         1                A        0           NA
    ## 10675      NA         1                A        0           NA
    ## 10971      NA         1                A        0           NA
    ## 10972      NA         1                A        0           NA
    ## 10973      NA         1                A        0           NA
    ## 11183      NA         1                A        0           NA
    ## 11750      NA         1                A        0           NA
    ## 11751      NA         1                A        0           NA
    ## 12035      NA         1                C        0           NA
    ## 12036      NA         1                C        0           NA
    ## 12037      NA         1                C        0           NA
    ## 12038      NA         1                B        0           NA
    ## 12328      NA         1                A        0           NA
    ## 12329      NA         1                A        0           NA
    ## 12330      NA         1                A        0           NA
    ## 12331      NA         1                A        0           NA
    ## 12332      NA         1                B        0           NA
    ## 12333      NA         1                A        0           NA
    ## 12334      NA         1                A        0           NA
    ## 12335      NA         1                A        0           NA
    ## 12336      NA         1                A        0           NA
    ## 12632      NA         1                A        0           NA
    ## 12633      NA         1                A        0           NA
    ## 12634      NA         1                A        0           NA
    ## 12635      NA         1                A        0           NA
    ## 12636      NA         1                A        0           NA
    ## 12637      NA         1                A        0           NA
    ## 12638      NA         1                A        0           NA
    ## 12639      NA         1                A        0           NA
    ## 12942      NA         1                A        0           NA
    ## 12943      NA         1                A        0           NA
    ## 13149      NA         1                A        0           NA
    ## 13150      NA         1                A        0           NA
    ## 13151      NA         1                B        0           NA
    ## 13152      NA         1                B        0           NA
    ## 13153      NA         1                B        0           NA
    ## 13154      NA         1                B        0           NA
    ## 13155      NA         1                B        0           NA
    ## 13156      NA         1                B        0           NA
    ## 13157      NA         1                A        0           NA
    ## 13158      NA         1                B        0           NA
    ## 13159      NA         1                B        0           NA
    ## 13160      NA         1                B        0           NA
    ## 13161      NA         1                B        0           NA
    ## 13422      NA         1                B        0           NA
    ## 13423      NA         1                B        0           NA
    ## 13424      NA         1                C        0           NA
    ## 13425      NA         1                C        0           NA
    ## 13426      NA         1                B        0           NA
    ## 13427      NA         1                B        0           NA
    ## 13428      NA         1                A        0           NA
    ## 13429      NA         1                C        0           NA
    ## 13430      NA         1                C        0           NA
    ## 13725      NA         1                A        0           NA
    ## 13726      NA         1                A        0           NA
    ## 14021      NA         1                A        0           NA
    ## 14022      NA         1                A        0           NA
    ## 14023      NA         1                A        0           NA
    ## 14024      NA         1                A        0           NA
    ## 14025      NA         1                A        0           NA
    ## 14325      NA         1                A        0           NA
    ## 14326      NA         1                A        0           NA
    ## 14623      NA         1                A        0           NA
    ## 14624      NA         1                B        0           NA
    ## 14625      NA         1                B        0           NA
    ## 14626      NA         1                B        0           NA
    ## 14627      NA         1                A        0           NA
    ## 14628      NA         1                B        0           NA
    ## 14629      NA         1                B        0           NA
    ## 14630      NA         1                B        0           NA
    ## 14918      NA         1                B        0           NA
    ## 14919      NA         1                B        0           NA
    ## 14920      NA         1                A        0           NA
    ## 14921      NA         1                B        0           NA
    ## 14922      NA         1                B        0           NA
    ## 14923      NA         1                B        0           NA
    ## 14924      NA         1                B        0           NA
    ## 14925      NA         1                B        0           NA
    ## 14926      NA         1                B        0           NA
    ## 14927      NA         1                B        0           NA
    ## 14928      NA         1                B        0           NA
    ## 14929      NA         1                B        0           NA
    ## 14930      NA         1                A        0           NA
    ## 14931      NA         1                B        0           NA
    ## 14932      NA         1                B        0           NA
    ## 14933      NA         1                B        0           NA
    ## 14934      NA         1                B        0           NA
    ## 15154      NA         1                B        0           NA
    ## 15155      NA         1                B        0           NA
    ## 15156      NA         1                A        0           NA
    ## 15436      NA         1                A        0           NA
    ## 15728      NA         1                A        0           NA
    ## 15729      NA         1                B        0           NA
    ## 15730      NA         1                A        0           NA
    ## 15731      NA         1                A        0           NA
    ## 15732      NA         1                A        0           NA
    ## 15733      NA         1                C        0           NA
    ## 15734      NA         1                A        0           NA
    ## 15735      NA         1                B        0           NA
    ## 15736      NA         1                A        0           NA
    ## 15737      NA         1                A        0           NA
    ## 15738      NA         1                C        0           NA
    ## 16034      NA         1                A        0           NA
    ## 16035      NA         1                C        0           NA
    ## 16036      NA         1                C        0           NA
    ## 16037      NA         1                C        0           NA
    ## 16318      NA         1                A        0           NA
    ## 16596      NA         1                A        0           NA
    ## 16597      NA         1                A        0           NA
    ## 16598      NA         1                A        0           NA
    ## 16599      NA         1                A        0           NA
    ## 16600      NA         1                A        0           NA
    ## 16882      NA         1                A        0           NA
    ## 17105      NA         1                A        0           NA
    ## 17680      NA         1                B        0           NA
    ## 17681      NA         1                B        0           NA
    ## 17682      NA         1                B        0           NA
    ## 17683      NA         1                B        0           NA
    ## 17684      NA         1                A        0           NA
    ## 17685      NA         1                A        0           NA
    ## 17686      NA         1                B        0           NA
    ## 17687      NA         1                B        0           NA
    ## 17688      NA         1                A        0           NA
    ## 17689      NA         1                B        0           NA
    ## 17690      NA         1                B        0           NA
    ## 17691      NA         1                A        0           NA
    ## 17977      NA         1                B        0           NA
    ## 17978      NA         1                B        0           NA
    ## 17979      NA         1                C        0           NA
    ## 17980      NA         1                A        0           NA
    ## 17981      NA         1                A        0           NA
    ## 17982      NA         1                A        0           NA
    ## 17983      NA         1                A        0           NA
    ## 17984      NA         1                A        0           NA
    ## 17985      NA         1                C        0           NA
    ## 17986      NA         1                A        0           NA
    ## 17987      NA         1                A        0           NA
    ## 17988      NA         1                A        0           NA
    ## 18286      NA         1                B        0           NA
    ## 18287      NA         1                A        0           NA
    ## 18288      NA         1                A        0           NA
    ## 18289      NA         1                B        0           NA
    ## 18567      NA         1                B        0           NA
    ## 18568      NA         1                B        0           NA
    ## 18569      NA         1                B        0           NA
    ## 18570      NA         1                A        0           NA
    ## 18571      NA         1                B        0           NA
    ## 18572      NA         1                B        0           NA
    ## 18573      NA         1                B        0           NA
    ## 18574      NA         1                B        0           NA
    ## 18575      NA         1                B        0           NA
    ## 18576      NA         1                B        0           NA
    ## 18577      NA         1                B        0           NA
    ## 18578      NA         1                B        0           NA
    ## 18579      NA         1                B        0           NA
    ## 18580      NA         1                B        0           NA
    ## 18581      NA         1                B        0           NA
    ## 18582      NA         1                B        0           NA
    ## 18583      NA         1                B        0           NA
    ## 18584      NA         1                B        0           NA
    ## 18585      NA         1                B        0           NA
    ## 18586      NA         1                B        0           NA
    ## 18587      NA         1                B        0           NA
    ## 18588      NA         1                B        0           NA
    ## 18589      NA         1                B        0           NA
    ## 18590      NA         1                A        0           NA
    ## 18591      NA         1                B        0           NA
    ## 18592      NA         1                B        0           NA
    ## 18593      NA         1                B        0           NA
    ## 18883      NA         1                B        0           NA
    ## 18884      NA         1                B        0           NA
    ## 18885      NA         1                B        0           NA
    ## 18886      NA         1                B        0           NA
    ## 18887      NA         1                B        0           NA
    ## 18888      NA         1                B        0           NA
    ## 18889      NA         1                B        0           NA
    ## 18890      NA         1                A        0           NA
    ## 18891      NA         1                B        0           NA
    ## 18892      NA         1                B        0           NA
    ## 18893      NA         1                A        0           NA
    ## 18894      NA         1                B        0           NA
    ## 18895      NA         1                B        0           NA
    ## 18896      NA         1                C        0           NA
    ## 18897      NA         1                B        0           NA
    ## 19118      NA         1                B        0           NA
    ## 19119      NA         1                A        0           NA
    ## 19120      NA         1                B        0           NA
    ## 19121      NA         1                C        0           NA
    ## 19122      NA         1                B        0           NA
    ## 19123      NA         1                B        0           NA
    ## 19406      NA         1                B        0           NA
    ## 19407      NA         1                B        0           NA
    ## 19408      NA         1                B        0           NA
    ## 19409      NA         1                A        0           NA
    ## 19705      NA         1                A        0           NA
    ## 19706      NA         1                A        0           NA
    ## 19707      NA         1                A        0           NA
    ## 19708      NA         1                A        0           NA
    ## 19709      NA         1                A        0           NA
    ## 19710      NA         1                A        0           NA
    ## 19711      NA         1                A        0           NA
    ## 19712      NA         1                A        0           NA
    ## 19713      NA         1                A        0           NA
    ## 19999      NA         1                B        0           NA
    ## 20000      NA         1                B        0           NA
    ## 20001      NA         1                B        0           NA
    ## 20002      NA         1                B        0           NA
    ## 20003      NA         1                B        0           NA
    ## 20004      NA         1                B        0           NA
    ## 20005      NA         1                B        0           NA
    ## 20006      NA         1                B        0           NA
    ## 20007      NA         1                B        0           NA
    ## 20008      NA         1                B        0           NA
    ## 20009      NA         1                B        0           NA
    ## 20010      NA         1                B        0           NA
    ## 20311      NA         1                B        0           NA
    ## 20611      NA         1                A        0           NA
    ## 20612      NA         1                A        0           NA
    ## 20613      NA         1                A        0           NA
    ## 20614      NA         1                A        0           NA
    ## 20615      NA         1                A        0           NA
    ## 20918      NA         1                B        0           NA
    ## 20919      NA         1                B        0           NA
    ## 21147      NA         1                A        0           NA
    ## 21432      NA         1                A        0           NA
    ## 21433      NA         1                A        0           NA
    ## 21434      NA         1                A        0           NA
    ## 21736      NA         1                A        0           NA
    ## 21737      NA         1                A        0           NA
    ## 21738      NA         1                A        0           NA
    ## 21739      NA         1                A        0           NA
    ## 21740      NA         1                A        0           NA
    ## 22000      NA         1                B        0           NA
    ## 22001      NA         1                B        0           NA
    ## 22002      NA         1                A        0           NA
    ## 22003      NA         1                B        0           NA
    ## 22004      NA         1                B        0           NA
    ## 22005      NA         1                B        0           NA
    ## 22006      NA         1                B        0           NA
    ## 22007      NA         1                B        0           NA
    ## 22008      NA         1                B        0           NA
    ## 22009      NA         1                B        0           NA
    ## 22010      NA         1                B        0           NA
    ## 22011      NA         1                B        0           NA
    ## 22012      NA         1                B        0           NA
    ## 22013      NA         1                B        0           NA
    ## 22014      NA         1                B        0           NA
    ## 22015      NA         1                B        0           NA
    ## 22016      NA         1                B        0           NA
    ## 22017      NA         1                B        0           NA
    ## 22018      NA         1                B        0           NA
    ## 22019      NA         1                B        0           NA
    ## 22020      NA         1                B        0           NA
    ## 22021      NA         1                B        0           NA
    ## 22022      NA         1                B        0           NA
    ## 22023      NA         1                B        0           NA
    ## 22024      NA         1                B        0           NA
    ## 22025      NA         1                B        0           NA
    ## 22026      NA         1                B        0           NA
    ## 22027      NA         1                B        0           NA
    ## 22028      NA         1                B        0           NA
    ## 22029      NA         1                B        0           NA
    ## 22030      NA         1                B        0           NA
    ## 22031      NA         1                B        0           NA
    ## 22032      NA         1                B        0           NA
    ## 22033      NA         1                B        0           NA
    ## 22034      NA         1                B        0           NA
    ## 22035      NA         1                B        0           NA
    ## 22036      NA         1                B        0           NA
    ## 22037      NA         1                B        0           NA
    ## 22038      NA         1                B        0           NA
    ## 22039      NA         1                B        0           NA
    ## 22040      NA         1                A        0           NA
    ## 22041      NA         1                B        0           NA
    ## 22042      NA         1                B        0           NA
    ## 22043      NA         1                B        0           NA
    ## 22338      NA         1                B        0           NA
    ## 22339      NA         1                B        0           NA
    ## 22340      NA         1                B        0           NA
    ## 22341      NA         1                A        0           NA
    ## 22342      NA         1                B        0           NA
    ## 22343      NA         1                A        0           NA
    ## 22344      NA         1                B        0           NA
    ## 22345      NA         1                B        0           NA
    ## 22346      NA         1                A        0           NA
    ## 22652      NA         1                B        0           NA
    ## 22952      NA         1                A        0           NA
    ## 22953      NA         1                A        0           NA
    ## 22954      NA         1                C        0           NA
    ## 22955      NA         1                B        0           NA
    ## 22956      NA         1                A        0           NA
    ## 22957      NA         1                C        0           NA
    ## 22958      NA         1                C        0           NA
    ## 22959      NA         1                B        0           NA
    ## 23472      NA         1                A        0           NA
    ## 23473      NA         1                A        0           NA
    ## 24077      NA         1                A        0           NA
    ## 24078      NA         1                C        0           NA
    ## 24367      NA         1                A        0           NA
    ## 24368      NA         1                A        0           NA
    ## 24369      NA         1                A        0           NA
    ## 24370      NA         1                A        0           NA
    ## 24371      NA         1                A        0           NA
    ## 24372      NA         1                A        0           NA
    ## 24373      NA         1                A        0           NA
    ## 24374      NA         1                A        0           NA
    ## 24375      NA         1                A        0           NA
    ## 24376      NA         1                A        0           NA
    ## 24377      NA         1                A        0           NA
    ## 24378      NA         1                A        0           NA
    ## 24379      NA         1                A        0           NA
    ## 24380      NA         1                A        0           NA
    ## 24381      NA         1                A        0           NA
    ## 24682      NA         1                A        0           NA
    ## 24683      NA         1                A        0           NA
    ## 24684      NA         1                A        0           NA
    ## 24685      NA         1                A        0           NA
    ## 24686      NA         1                A        0           NA
    ## 24687      NA         1                A        0           NA
    ## 24992      NA         1                A        0           NA
    ## 24993      NA         1                A        0           NA
    ## 25218      NA         1                A        0           NA
    ## 25219      NA         1                A        0           NA
    ## 25792      NA         1                A        0           NA
    ## 25793      NA         1                B        0           NA
    ## 25794      NA         1                C        0           NA
    ## 25795      NA         1                C        0           NA
    ## 25796      NA         1                C        0           NA
    ## 25797      NA         1                B        0           NA
    ## 25798      NA         1                A        0           NA
    ## 25799      NA         1                B        0           NA
    ## 25800      NA         1                C        0           NA
    ## 25801      NA         1                B        0           NA
    ## 25802      NA         1                C        0           NA
    ## 25803      NA         1                C        0           NA
    ## 26088      NA         1                A        0           NA
    ## 26667      NA         1                A        0           NA
    ## 26668      NA         1                A        0           NA
    ## 26959      NA         1                A        0           NA
    ## 26960      NA         1                A        0           NA
    ## 27448      NA         1                A        0           NA
    ## 27732      NA         1                A        0           NA
    ## 27733      NA         1                A        0           NA
    ## 27734      NA         1                A        0           NA
    ## 27735      NA         1                A        0           NA
    ## 27736      NA         1                A        0           NA
    ## 27737      NA         1                A        0           NA
    ## 27738      NA         1                A        0           NA
    ## 28004      NA         1                A        0           NA
    ## 28005      NA         1                A        0           NA
    ## 28006      NA         1                A        0           NA
    ## 28007      NA         1                A        0           NA
    ## 28008      NA         1                A        0           NA
    ## 28009      NA         1                A        0           NA
    ## 28010      NA         1                A        0           NA
    ## 28011      NA         1                A        0           NA
    ## 28012      NA         1                C        0           NA
    ## 28013      NA         1                A        0           NA
    ## 28014      NA         1                A        0           NA
    ## 28015      NA         1                A        0           NA
    ## 28016      NA         1                A        0           NA
    ## 28017      NA         1                A        0           NA
    ## 28018      NA         1                A        0           NA
    ## 28019      NA         1                A        0           NA
    ## 28020      NA         1                A        0           NA
    ## 28021      NA         1                A        0           NA
    ## 28022      NA         1                A        0           NA
    ## 28023      NA         1                A        0           NA
    ## 28024      NA         1                A        0           NA
    ## 28025      NA         1                A        0           NA
    ## 28026      NA         1                A        0           NA
    ## 28027      NA         1                A        0           NA
    ## 28275      NA         1                A        0           NA
    ## 28276      NA         1                A        0           NA
    ## 28277      NA         1                A        0           NA
    ## 28278      NA         1                A        0           NA
    ## 28279      NA         1                A        0           NA
    ## 28280      NA         1                A        0           NA
    ## 28281      NA         1                A        0           NA
    ## 28282      NA         1                A        0           NA
    ## 28283      NA         1                A        0           NA
    ## 28284      NA         1                A        0           NA
    ## 28285      NA         1                A        0           NA
    ## 28286      NA         1                A        0           NA
    ## 28287      NA         1                A        0           NA
    ## 28288      NA         1                A        0           NA
    ## 28289      NA         1                A        0           NA
    ## 28290      NA         1                A        0           NA
    ## 28291      NA         1                A        0           NA
    ## 28292      NA         1                A        0           NA
    ## 28293      NA         1                A        0           NA
    ## 28294      NA         1                A        0           NA
    ## 28295      NA         1                A        0           NA
    ## 28296      NA         1                A        0           NA
    ## 28297      NA         1                A        0           NA
    ## 28298      NA         1                A        0           NA
    ## 28299      NA         1                A        0           NA
    ## 28300      NA         1                A        0           NA
    ## 28301      NA         1                A        0           NA
    ## 28302      NA         1                A        0           NA
    ## 28303      NA         1                A        0           NA
    ## 28304      NA         1                A        0           NA
    ## 28305      NA         1                A        0           NA
    ## 28306      NA         1                A        0           NA
    ## 28307      NA         1                A        0           NA
    ## 28308      NA         1                A        0           NA
    ## 28309      NA         1                A        0           NA
    ## 28310      NA         1                A        0           NA
    ## 28311      NA         1                A        0           NA
    ## 28312      NA         1                A        0           NA
    ## 28313      NA         1                A        0           NA
    ## 28314      NA         1                A        0           NA
    ## 28315      NA         1                A        0           NA
    ## 28316      NA         1                A        0           NA
    ## 28317      NA         1                A        0           NA
    ## 28318      NA         1                A        0           NA
    ## 28319      NA         1                A        0           NA
    ## 28320      NA         1                A        0           NA
    ## 28321      NA         1                A        0           NA
    ## 28585      NA         1                A        0           NA
    ## 28586      NA         1                A        0           NA
    ## 28587      NA         1                A        0           NA
    ## 28588      NA         1                A        0           NA
    ## 28589      NA         1                A        0           NA
    ## 28590      NA         1                A        0           NA
    ## 28591      NA         1                A        0           NA
    ## 28592      NA         1                A        0           NA
    ## 28593      NA         1                A        0           NA
    ## 28594      NA         1                A        0           NA
    ## 28595      NA         1                A        0           NA
    ## 28596      NA         1                A        0           NA
    ## 28597      NA         1                A        0           NA
    ## 28598      NA         1                A        0           NA
    ## 28599      NA         1                A        0           NA
    ## 28600      NA         1                A        0           NA
    ## 28601      NA         1                A        0           NA
    ## 28602      NA         1                A        0           NA
    ## 28603      NA         1                A        0           NA
    ## 28604      NA         1                C        0           NA
    ## 28605      NA         1                A        0           NA
    ## 28606      NA         1                A        0           NA
    ## 28607      NA         1                A        0           NA
    ## 28608      NA         1                A        0           NA
    ## 28609      NA         1                A        0           NA
    ## 28610      NA         1                A        0           NA
    ## 28611      NA         1                A        0           NA
    ## 28612      NA         1                A        0           NA
    ## 28613      NA         1                A        0           NA
    ## 28614      NA         1                A        0           NA
    ## 28615      NA         1                A        0           NA
    ## 28616      NA         1                A        0           NA
    ## 28617      NA         1                A        0           NA
    ## 28898      NA         1                A        0           NA
    ## 28899      NA         1                A        0           NA
    ## 28900      NA         1                A        0           NA
    ## 28901      NA         1                A        0           NA
    ## 28902      NA         1                A        0           NA
    ## 28903      NA         1                A        0           NA
    ## 28904      NA         1                A        0           NA
    ## 28905      NA         1                A        0           NA
    ## 28906      NA         1                A        0           NA
    ## 28907      NA         1                A        0           NA
    ## 28908      NA         1                A        0           NA
    ## 28909      NA         1                A        0           NA
    ## 28910      NA         1                A        0           NA
    ## 28911      NA         1                A        0           NA
    ## 28912      NA         1                A        0           NA
    ## 28913      NA         1                A        0           NA
    ## 28914      NA         1                A        0           NA
    ## 29131      NA         1                A        0           NA
    ## 29132      NA         1                A        0           NA
    ## 29133      NA         1                A        0           NA
    ## 29134      NA         1                A        0           NA
    ## 29135      NA         1                A        0           NA
    ## 29136      NA         1                A        0           NA
    ## 29137      NA         1                A        0           NA
    ## 29138      NA         1                A        0           NA
    ## 29139      NA         1                A        0           NA
    ## 29417      NA         1                A        0           NA
    ## 29418      NA         1                A        0           NA
    ## 29419      NA         1                A        0           NA
    ## 29420      NA         1                A        0           NA
    ## 29421      NA         1                A        0           NA
    ## 29715      NA         1                A        0           NA
    ## 30002      NA         1                A        0           NA
    ## 30003      NA         1                A        0           NA
    ## 30297      NA         1                A        0           NA
    ## 30589      NA         1                B        0           NA
    ## 30590      NA         1                B        0           NA
    ## 30591      NA         1                B        0           NA
    ## 30592      NA         1                B        0           NA
    ## 30593      NA         1                B        0           NA
    ## 30890      NA         1                B        0           NA
    ## 31113      NA         1                A        0           NA
    ## 31114      NA         1                A        0           NA
    ## 32270      NA         1                A        0           NA
    ## 32271      NA         1                B        0           NA
    ## 32272      NA         1                A        0           NA
    ## 32273      NA         1                B        0           NA
    ## 32566      NA         1                A        0           NA
    ## 32567      NA         1                A        0           NA
    ## 32568      NA         1                A        0           NA
    ## 32569      NA         1                A        0           NA
    ## 32570      NA         1                A        0           NA
    ## 32571      NA         1                A        0           NA
    ## 32863      NA         1                B        0           NA
    ## 32864      NA         1                B        0           NA
    ## 32865      NA         1                B        0           NA
    ## 32866      NA         1                B        0           NA
    ## 32867      NA         1                A        0           NA
    ## 32868      NA         1                B        0           NA
    ## 32869      NA         1                B        0           NA
    ## 32870      NA         1                B        0           NA
    ## 33095      NA         1                A        0           NA
    ## 33096      NA         1                B        0           NA
    ## 33674      NA         1                A        0           NA
    ## 33675      NA         1                A        0           NA
    ## 34259      NA         1                A        0           NA
    ## 34260      NA         1                C        0           NA
    ## 34261      NA         1                C        0           NA
    ## 34864      NA         1                B        0           NA
    ## 34865      NA         1                A        0           NA
    ## 34866      NA         1                A        0           NA
    ## 34867      NA         1                B        0           NA
    ## 34868      NA         1                A        0           NA
    ## 35097      NA         1                A        0           NA
    ## 35386      NA         1                A        0           NA
    ## 35387      NA         1                A        0           NA
    ## 35686      NA         1                A        0           NA
    ## 35687      NA         1                A        0           NA
    ## 35688      NA         1                A        0           NA
    ## 35689      NA         1                A        0           NA
    ## 35690      NA         1                A        0           NA
    ## 35691      NA         1                A        0           NA
    ## 35980      NA         1                B        0           NA
    ## 35981      NA         1                B        0           NA
    ## 35982      NA         1                B        0           NA
    ## 35983      NA         1                B        0           NA
    ## 35984      NA         1                A        0           NA
    ## 35985      NA         1                B        0           NA
    ## 35986      NA         1                B        0           NA
    ## 35987      NA         1                B        0           NA
    ## 35988      NA         1                B        0           NA
    ## 36287      NA         1                A        0           NA
    ## 36288      NA         1                B        0           NA
    ## 36289      NA         1                B        0           NA
    ## 36290      NA         1                A        0           NA
    ## 36291      NA         1                B        0           NA
    ## 36292      NA         1                B        0           NA
    ## 36596      NA         1                A        0           NA
    ## 36597      NA         1                A        0           NA
    ## 36598      NA         1                A        0           NA
    ## 36904      NA         1                A        0           NA
    ## 36905      NA         1                A        0           NA
    ## 37148      NA         1                A        0           NA
    ## 37149      NA         1                A        0           NA
    ## 37436      NA         1                A        0           NA
    ## 37437      NA         1                A        0           NA
    ## 37438      NA         1                A        0           NA
    ## 37439      NA         1                A        0           NA
    ## 37440      NA         1                A        0           NA
    ## 38033      NA         1                A        0           NA
    ## 38034      NA         1                A        0           NA
    ## 38035      NA         1                A        0           NA
    ## 38036      NA         1                A        0           NA
    ## 38335      NA         1                A        0           NA
    ## 38336      NA         1                B        0           NA
    ## 38337      NA         1                B        0           NA
    ## 38338      NA         1                B        0           NA
    ## 38339      NA         1                A        0           NA
    ## 38340      NA         1                B        0           NA
    ## 38341      NA         1                B        0           NA
    ## 38342      NA         1                B        0           NA
    ## 38650      NA         1                A        0           NA
    ## 38958      NA         1                A        0           NA
    ## 38959      NA         1                A        0           NA
    ## 39204      NA         1                A        0           NA
    ## 39495      NA         1                A        0           NA
    ## 39496      NA         1                A        0           NA
    ## 40407      NA         1                A        0           NA
    ## 40408      NA         1                C        0           NA
    ## 40716      NA         1                A        0           NA
    ## 41526      NA         1                A        0           NA
    ## 41527      NA         1                A        0           NA
    ## 41528      NA         1                A        0           NA
    ## 41529      NA         1                A        0           NA
    ## 41806      NA         1                A        0           NA
    ## 41807      NA         1                B        0           NA
    ## 41808      NA         1                B        0           NA
    ## 42086      NA         1                A        0           NA
    ## 42087      NA         1                B        0           NA
    ## 42088      NA         1                C        0           NA
    ## 42089      NA         1                A        0           NA
    ## 42090      NA         1                B        0           NA
    ## 42091      NA         1                B        0           NA
    ## 42092      NA         1                B        0           NA
    ## 42093      NA         1                B        0           NA
    ## 42094      NA         1                B        0           NA
    ## 42095      NA         1                B        0           NA
    ## 42096      NA         1                B        0           NA
    ## 42097      NA         1                B        0           NA
    ## 42098      NA         1                B        0           NA
    ## 42099      NA         1                A        0           NA
    ## 42100      NA         1                B        0           NA
    ## 42101      NA         1                B        0           NA
    ## 42102      NA         1                C        0           NA
    ## 42103      NA         1                B        0           NA
    ## 42104      NA         1                A        0           NA
    ## 42105      NA         1                B        0           NA
    ## 42106      NA         1                A        0           NA
    ## 42107      NA         1                B        0           NA
    ## 42108      NA         1                B        0           NA
    ## 42109      NA         1                B        0           NA
    ## 42110      NA         1                B        0           NA
    ## 42111      NA         1                B        0           NA
    ## 42112      NA         1                B        0           NA
    ## 42113      NA         1                B        0           NA
    ## 42114      NA         1                B        0           NA
    ## 42413      NA         1                A        0           NA
    ## 42414      NA         1                A        0           NA
    ## 42415      NA         1                A        0           NA
    ## 42416      NA         1                A        0           NA
    ## 42417      NA         1                B        0           NA
    ## 42418      NA         1                B        0           NA
    ## 42419      NA         1                A        0           NA
    ## 42420      NA         1                A        0           NA
    ## 42726      NA         1                A        0           NA
    ## 42727      NA         1                A        0           NA
    ## 42728      NA         1                A        0           NA
    ## 43034      NA         1                A        0           NA
    ## 43035      NA         1                A        0           NA
    ## 43036      NA         1                A        0           NA
    ## 43037      NA         1                A        0           NA
    ## 43281      NA         1                C        0           NA
    ## 43282      NA         1                C        0           NA
    ## 43569      NA         1                A        0           NA
    ## 43570      NA         1                A        0           NA
    ## 43571      NA         1                A        0           NA
    ## 43572      NA         1                A        0           NA
    ## 43573      NA         1                A        0           NA
    ## 43574      NA         1                A        0           NA
    ## 43879      NA         1                A        0           NA
    ## 43880      NA         1                A        0           NA
    ## 44179      NA         1                A        0           NA
    ## 44180      NA         1                A        0           NA
    ## 44485      NA         1                A        0           NA
    ## 44797      NA         1                A        0           NA
    ## 44798      NA         1                A        0           NA
    ## 45113      NA         1                A        0           NA
    ## 45114      NA         1                C        0           NA
    ## 45363      NA         1                C        0           NA
    ## 45364      NA         1                A        0           NA
    ## 45365      NA         1                C        0           NA
    ## 45366      NA         1                C        0           NA
    ## 45664      NA         1                B        0           NA
    ## 45665      NA         1                B        0           NA
    ## 45970      NA         1                A        0           NA
    ## 45971      NA         1                B        0           NA
    ## 45972      NA         1                B        0           NA
    ## 45973      NA         1                A        0           NA
    ## 45974      NA         1                B        0           NA
    ## 45975      NA         1                A        0           NA
    ## 45976      NA         1                B        0           NA
    ## 45977      NA         1                B        0           NA
    ## 45978      NA         1                B        0           NA
    ## 45979      NA         1                A        0           NA
    ## 46289      NA         1                A        0           NA
    ## 46290      NA         1                A        0           NA
    ## 46291      NA         1                C        0           NA
    ## 46292      NA         1                A        0           NA
    ## 46293      NA         1                A        0           NA
    ## 46603      NA         1                C        0           NA
    ## 46604      NA         1                A        0           NA
    ## 46605      NA         1                A        0           NA
    ## 46606      NA         1                A        0           NA
    ## 46607      NA         1                A        0           NA
    ## 46921      NA         1                A        0           NA
    ## 46922      NA         1                A        0           NA
    ## 46923      NA         1                A        0           NA
    ## 47239      NA         1                A        0           NA
    ## 47240      NA         1                A        0           NA
    ## 47797      NA         1                C        0           NA
    ## 48108      NA         1                A        0           NA
    ## 48109      NA         1                C        0           NA
    ## 48110      NA         1                B        0           NA
    ## 48111      NA         1                B        0           NA
    ## 48414      NA         1                C        0           NA
    ## 48415      NA         1                B        0           NA
    ## 48416      NA         1                B        0           NA
    ## 48417      NA         1                B        0           NA
    ## 48418      NA         1                B        0           NA
    ## 48419      NA         1                B        0           NA
    ## 48420      NA         1                B        0           NA
    ## 48421      NA         1                C        0           NA
    ## 48422      NA         1                B        0           NA
    ## 48423      NA         1                B        0           NA
    ## 48424      NA         1                B        0           NA
    ## 48425      NA         1                B        0           NA
    ## 48738      NA         1                A        0           NA
    ## 48739      NA         1                A        0           NA
    ## 49047      NA         1                B        0           NA
    ## 49048      NA         1                B        0           NA
    ## 49049      NA         1                B        0           NA
    ## 49050      NA         1                B        0           NA
    ## 49051      NA         1                B        0           NA
    ## 49052      NA         1                B        0           NA
    ## 49053      NA         1                B        0           NA
    ## 49054      NA         1                B        0           NA
    ## 49368      NA         1                A        0           NA
    ## 49369      NA         1                A        0           NA
    ## 49370      NA         1                B        0           NA
    ## 49371      NA         1                A        0           NA
    ## 49922      NA         1                A        0           NA
    ## 49923      NA         1                A        0           NA
    ## 49924      NA         1                A        0           NA
    ## 49925      NA         1                B        0           NA
    ## 49926      NA         1                A        0           NA
    ## 49927      NA         1                A        0           NA
    ## 49928      NA         1                A        0           NA
    ## 49929      NA         1                B        0           NA
    ## 50556      NA         1                A        0           NA
    ## 50557      NA         1                A        0           NA
    ## 51176      NA         1                A        0           NA
    ## 51177      NA         1                A        0           NA
    ## 51178      NA         1                A        0           NA
    ## 51179      NA         1                A        0           NA
    ## 51180      NA         1                B        0           NA
    ## 51181      NA         1                A        0           NA
    ## 51182      NA         1                A        0           NA
    ## 51183      NA         1                A        0           NA
    ## 51184      NA         1                A        0           NA
    ## 51185      NA         1                B        0           NA
    ## 51501      NA         1                A        0           NA
    ## 51502      NA         1                A        0           NA
    ## 51756      NA         1                B        0           NA
    ## 51757      NA         1                B        0           NA
    ## 51758      NA         1                B        0           NA
    ## 51759      NA         1                B        0           NA
    ## 51760      NA         1                B        0           NA
    ## 51761      NA         1                B        0           NA
    ## 52055      NA         1                B        0           NA
    ## 52056      NA         1                B        0           NA
    ## 52057      NA         1                B        0           NA
    ## 52058      NA         1                B        0           NA
    ## 52371      NA         1                A        0           NA
    ## 52372      NA         1                A        0           NA
    ## 52680      NA         1                A        0           NA
    ## 52681      NA         1                A        0           NA
    ## 52682      NA         1                A        0           NA
    ## 52683      NA         1                A        0           NA
    ## 52684      NA         1                A        0           NA
    ## 52981      NA         1                A        0           NA
    ## 53738      NA         1                A        0           NA
    ## 54024      NA         1                A        0           NA
    ## 54025      NA         1                A        0           NA
    ## 54621      NA         1                A        0           NA
    ## 54622      NA         1                A        0           NA
    ## 54623      NA         1                A        0           NA
    ## 54624      NA         1                B        0           NA
    ## 54906      NA         1                B        0           NA
    ## 54907      NA         1                A        0           NA
    ## 54908      NA         1                A        0           NA
    ## 55207      NA         1                A        0           NA
    ## 55208      NA         1                B        0           NA
    ## 55209      NA         1                C        0           NA
    ## 55210      NA         1                B        0           NA
    ## 55511      NA         1                A        0           NA
    ## 55512      NA         1                A        0           NA
    ## 55513      NA         1                A        0           NA
    ## 55769      NA         1                A        0           NA
    ## 56049      NA         1                A        0           NA
    ## 56050      NA         1                B        0           NA
    ## 56051      NA         1                B        0           NA
    ## 56052      NA         1                A        0           NA
    ## 56053      NA         1                C        0           NA
    ## 56054      NA         1                B        0           NA
    ## 56353      NA         1                B        0           NA
    ## 56354      NA         1                C        0           NA
    ## 57559      NA         1                A        0           NA
    ## 57560      NA         1                A        0           NA
    ## 57813      NA         1                A        0           NA
    ## 57814      NA         1                A        0           NA
    ## 57815      NA         1                A        0           NA
    ## 58095      NA         1                C        0           NA
    ## 58096      NA         1                A        0           NA
    ## 58097      NA         1                C        0           NA
    ## 58391      NA         1                A        0           NA
    ## 58392      NA         1                A        0           NA
    ## 58393      NA         1                A        0           NA
    ## 58685      NA         1                A        0           NA
    ## 58686      NA         1                A        0           NA
    ## 58687      NA         1                A        0           NA
    ## 58688      NA         1                A        0           NA
    ## 58689      NA         1                A        0           NA
    ## 58979      NA         1                B        0           NA
    ## 58980      NA         1                B        0           NA
    ## 58981      NA         1                B        0           NA
    ## 58982      NA         1                B        0           NA
    ## 58983      NA         1                B        0           NA
    ## 58984      NA         1                B        0           NA
    ## 58985      NA         1                A        0           NA
    ## 59276      NA         1                B        0           NA
    ## 59277      NA         1                B        0           NA
    ## 59278      NA         1                A        0           NA
    ## 59279      NA         1                A        0           NA
    ## 59280      NA         1                B        0           NA
    ## 59281      NA         1                B        0           NA
    ## 59282      NA         1                A        0           NA
    ## 59283      NA         1                B        0           NA
    ## 59579      NA         1                A        0           NA
    ## 59580      NA         1                A        0           NA
    ## 59581      NA         1                A        0           NA
    ## 59582      NA         1                A        0           NA
    ## 59832      NA         1                B        0           NA
    ## 59833      NA         1                C        0           NA
    ## 59834      NA         1                B        0           NA
    ## 59835      NA         1                C        0           NA
    ## 60112      NA         1                C        0           NA
    ## 60113      NA         1                B        0           NA
    ## 60114      NA         1                B        0           NA
    ## 60115      NA         1                B        0           NA
    ## 60116      NA         1                B        0           NA
    ## 60117      NA         1                B        0           NA
    ## 60413      NA         1                B        0           NA
    ## 60708      NA         1                A        0           NA
    ## 60709      NA         1                A        0           NA
    ## 60999      NA         1                A        0           NA
    ## 61000      NA         1                A        0           NA
    ## 61001      NA         1                B        0           NA
    ## 61002      NA         1                B        0           NA
    ## 61003      NA         1                A        0           NA
    ## 61004      NA         1                B        0           NA
    ## 61005      NA         1                A        0           NA
    ## 61301      NA         1                B        0           NA
    ## 61302      NA         1                A        0           NA
    ## 61303      NA         1                A        0           NA
    ## 61599      NA         1                A        0           NA
    ## 61842      NA         1                B        0           NA
    ## 61843      NA         1                A        0           NA
    ## 61844      NA         1                B        0           NA
    ## 61845      NA         1                B        0           NA
    ## 61846      NA         1                A        0           NA
    ## 61847      NA         1                A        0           NA
    ## 61848      NA         1                B        0           NA
    ## 62125      NA         1                A        0           NA
    ## 62126      NA         1                A        0           NA
    ## 62413      NA         1                A        0           NA
    ## 62414      NA         1                A        0           NA
    ## 62415      NA         1                B        0           NA
    ## 62416      NA         1                B        0           NA
    ## 62417      NA         1                A        0           NA
    ## 62418      NA         1                A        0           NA
    ## 62707      NA         1                B        0           NA
    ## 62708      NA         1                B        0           NA
    ## 62709      NA         1                B        0           NA
    ## 62710      NA         1                B        0           NA
    ## 63000      NA         1                A        0           NA
    ## 63001      NA         1                A        0           NA
    ## 63002      NA         1                A        0           NA
    ## 63294      NA         1                B        0           NA
    ## 63295      NA         1                A        0           NA
    ## 63296      NA         1                B        0           NA
    ## 63589      NA         1                A        0           NA
    ## 63590      NA         1                A        0           NA
    ## 64113      NA         1                A        0           NA
    ## 64114      NA         1                A        0           NA
    ## 64115      NA         1                A        0           NA
    ## 64116      NA         1                A        0           NA
    ## 64117      NA         1                A        0           NA
    ## 64404      NA         1                A        0           NA
    ## 64405      NA         1                A        0           NA
    ## 64406      NA         1                B        0           NA
    ## 64407      NA         1                B        0           NA
    ## 64408      NA         1                A        0           NA
    ## 64409      NA         1                B        0           NA
    ## 64698      NA         1                C        0           NA
    ## 64699      NA         1                A        0           NA
    ## 64700      NA         1                C        0           NA
    ## 64701      NA         1                B        0           NA
    ## 64990      NA         1                A        0           NA
    ## 64991      NA         1                A        0           NA
    ## 65282      NA         1                A        0           NA
    ## 65283      NA         1                B        0           NA
    ## 65284      NA         1                A        0           NA
    ## 65285      NA         1                B        0           NA
    ## 65571      NA         1                B        0           NA
    ## 65572      NA         1                A        0           NA
    ## 65573      NA         1                B        0           NA
    ## 65574      NA         1                C        0           NA
    ## 65575      NA         1                B        0           NA
    ## 65576      NA         1                B        0           NA
    ## 65577      NA         1                B        0           NA
    ## 65578      NA         1                A        0           NA
    ## 65579      NA         1                C        0           NA
    ## 65580      NA         1                B        0           NA
    ## 65581      NA         1                B        0           NA
    ## 65582      NA         1                B        0           NA
    ## 65821      NA         1                C        0           NA
    ## 66084      NA         1                A        0           NA
    ## 66085      NA         1                A        0           NA
    ## 66664      NA         1                A        0           NA
    ## 66665      NA         1                A        0           NA
    ## 66951      NA         1                A        0           NA
    ## 66952      NA         1                A        0           NA
    ## 66953      NA         1                A        0           NA
    ## 67246      NA         1                B        0           NA
    ## 67247      NA         1                B        0           NA
    ## 67541      NA         1                B        0           NA
    ## 67542      NA         1                B        0           NA
    ## 67767      NA         1                A        0           NA
    ## 67768      NA         1                A        0           NA
    ## 68034      NA         1                A        0           NA
    ## 68316      NA         1                A        0           NA
    ## 68317      NA         1                A        0           NA
    ## 68590      NA         1                A        0           NA
    ## 68591      NA         1                A        0           NA
    ## 68592      NA         1                A        0           NA
    ## 68593      NA         1                A        0           NA
    ## 68594      NA         1                A        0           NA
    ## 68595      NA         1                A        0           NA
    ## 68874      NA         1                A        0           NA
    ## 68875      NA         1                A        0           NA
    ## 69159      NA         1                A        0           NA
    ## 69160      NA         1                A        0           NA
    ## 69161      NA         1                A        0           NA
    ## 69446      NA         1                A        0           NA
    ## 69447      NA         1                B        0           NA
    ## 69448      NA         1                B        0           NA
    ## 69854      NA         1                A        0           NA
    ## 69855      NA         1                A        0           NA
    ## 69856      NA         1                A        0           NA
    ## 70120      NA         1                B        0           NA
    ## 70121      NA         1                A        0           NA
    ## 70122      NA         1                B        0           NA
    ## 70123      NA         1                A        0           NA
    ## 71169      NA         1                A        0           NA
    ## 71170      NA         1                A        0           NA
    ## 71171      NA         1                A        0           NA
    ## 71361      NA         1                B        0           NA
    ## 71362      NA         1                B        0           NA
    ## 71607      NA         1                A        0           NA
    ## 71868      NA         1                C        0           NA
    ## 71869      NA         1                C        0           NA
    ## 72123      NA         1                A        0           NA
    ## 72873      NA         1                B        0           NA
    ## 72874      NA         1                B        0           NA
    ## 72875      NA         1                B        0           NA
    ## 72876      NA         1                B        0           NA
    ## 72877      NA         1                B        0           NA
    ## 72878      NA         1                B        0           NA
    ## 72879      NA         1                B        0           NA
    ## 72880      NA         1                B        0           NA
    ## 72881      NA         1                B        0           NA
    ## 72882      NA         1                B        0           NA
    ## 72883      NA         1                B        0           NA
    ## 72884      NA         1                B        0           NA
    ## 72885      NA         1                B        0           NA
    ## 72886      NA         1                B        0           NA
    ## 72887      NA         1                B        0           NA
    ## 72888      NA         1                B        0           NA
    ## 72889      NA         1                B        0           NA
    ## 72890      NA         1                B        0           NA
    ## 72891      NA         1                B        0           NA
    ## 72892      NA         1                B        0           NA
    ## 72893      NA         1                B        0           NA
    ## 72894      NA         1                B        0           NA
    ## 72895      NA         1                B        0           NA
    ## 72896      NA         1                B        0           NA
    ## 72897      NA         1                B        0           NA
    ## 72898      NA         1                B        0           NA
    ## 72899      NA         1                B        0           NA
    ## 72900      NA         1                B        0           NA
    ## 72901      NA         1                B        0           NA
    ## 73050      NA         1                B        0           NA
    ## 73051      NA         1                B        0           NA
    ## 73052      NA         1                B        0           NA
    ## 73053      NA         1                B        0           NA
    ## 73054      NA         1                B        0           NA
    ## 73055      NA         1                B        0           NA
    ## 73056      NA         1                B        0           NA
    ## 73057      NA         1                B        0           NA
    ## 73058      NA         1                B        0           NA
    ## 73059      NA         1                B        0           NA
    ## 73060      NA         1                B        0           NA
    ## 73061      NA         1                B        0           NA
    ## 73062      NA         1                B        0           NA
    ## 73063      NA         1                B        0           NA
    ## 73064      NA         1                B        0           NA
    ## 73065      NA         1                B        0           NA
    ## 73066      NA         1                B        0           NA
    ## 73067      NA         1                B        0           NA
    ## 73068      NA         1                B        0           NA
    ## 73069      NA         1                B        0           NA
    ## 73070      NA         1                B        0           NA
    ## 73071      NA         1                B        0           NA
    ## 73072      NA         1                B        0           NA
    ## 73073      NA         1                B        0           NA
    ## 73074      NA         1                B        0           NA
    ## 73075      NA         1                B        0           NA
    ## 73076      NA         1                B        0           NA
    ## 73077      NA         1                B        0           NA
    ## 73078      NA         1                B        0           NA
    ## 73079      NA         1                B        0           NA
    ## 73080      NA         1                B        0           NA
    ## 73081      NA         1                B        0           NA
    ## 73082      NA         1                B        0           NA
    ## 73083      NA         1                B        0           NA
    ## 73084      NA         1                B        0           NA
    ## 73085      NA         1                B        0           NA
    ## 73086      NA         1                B        0           NA
    ## 73087      NA         1                B        0           NA
    ## 73088      NA         1                B        0           NA
    ## 73089      NA         1                B        0           NA
    ## 73090      NA         1                B        0           NA
    ## 73091      NA         1                B        0           NA
    ## 73092      NA         1                B        0           NA
    ## 73307      NA         1                B        0           NA
    ## 73308      NA         1                B        0           NA
    ## 73309      NA         1                B        0           NA
    ## 73310      NA         1                B        0           NA
    ## 73311      NA         1                B        0           NA
    ## 73312      NA         1                B        0           NA
    ## 73313      NA         1                B        0           NA
    ## 73314      NA         1                B        0           NA
    ## 73315      NA         1                B        0           NA
    ## 73316      NA         1                B        0           NA
    ## 73317      NA         1                B        0           NA
    ## 73318      NA         1                B        0           NA
    ## 73319      NA         1                B        0           NA
    ## 73320      NA         1                B        0           NA
    ## 73321      NA         1                B        0           NA
    ## 73322      NA         1                B        0           NA
    ## 73323      NA         1                B        0           NA
    ## 73324      NA         1                B        0           NA
    ## 73325      NA         1                B        0           NA
    ## 73326      NA         1                B        0           NA
    ## 73327      NA         1                B        0           NA
    ## 73328      NA         1                B        0           NA
    ## 73329      NA         1                B        0           NA
    ## 73330      NA         1                B        0           NA
    ## 73331      NA         1                B        0           NA
    ## 73332      NA         1                B        0           NA
    ## 73333      NA         1                B        0           NA
    ## 73334      NA         1                B        0           NA
    ## 73335      NA         1                B        0           NA
    ## 73336      NA         1                B        0           NA
    ## 73337      NA         1                B        0           NA
    ## 73584      NA         1                B        0           NA
    ## 73585      NA         1                B        0           NA
    ## 73586      NA         1                B        0           NA
    ## 73587      NA         1                B        0           NA
    ## 73588      NA         1                B        0           NA
    ## 73589      NA         1                B        0           NA
    ## 73590      NA         1                B        0           NA
    ## 73591      NA         1                B        0           NA
    ## 73592      NA         1                B        0           NA
    ## 73593      NA         1                B        0           NA
    ## 73594      NA         1                B        0           NA
    ## 73595      NA         1                B        0           NA
    ## 73596      NA         1                B        0           NA
    ## 73597      NA         1                B        0           NA
    ## 73598      NA         1                B        0           NA
    ## 73599      NA         1                B        0           NA
    ## 73848      NA         1                B        0           NA
    ## 73849      NA         1                B        0           NA
    ## 73850      NA         1                B        0           NA
    ## 73851      NA         1                B        0           NA
    ## 73852      NA         1                B        0           NA
    ## 73853      NA         1                B        0           NA
    ## 74105      NA         1                B        0           NA
    ## 74106      NA         1                B        0           NA
    ## 74107      NA         1                B        0           NA
    ## 74108      NA         1                B        0           NA
    ## 74109      NA         1                B        0           NA
    ## 74110      NA         1                B        0           NA
    ## 74367      NA         1                B        0           NA
    ## 74368      NA         1                B        0           NA
    ## 74369      NA         1                B        0           NA
    ## 74370      NA         1                B        0           NA
    ## 74371      NA         1                B        0           NA
    ## 74372      NA         1                B        0           NA
    ## 74634      NA         1                B        0           NA
    ## 74635      NA         1                B        0           NA
    ## 76365      NA         1                B        0           NA
    ## 76366      NA         1                B        0           NA
    ## 77574      NA         1                A        0           NA
    ## 77925      NA         1                A        0           NA
    ## 78095      NA         1                A        0           NA
    ## 78096      NA         1                A        0           NA
    ## 78097      NA         1                A        0           NA
    ## 79046      NA         1                A        0           NA
    ## 79047      NA         1                C        0           NA
    ## 79300      NA         1                A        0           NA
    ## 79301      NA         1                A        0           NA
    ## 79562      NA         1                A        0           NA
    ## 79563      NA         1                A        0           NA
    ## 79923      NA         1                A        0           NA
    ## 80013      NA         1                A        0           NA
    ## 80772      NA         1                C        0           NA
    ## 80773      NA         1                C        0           NA
    ## 81474      NA         1                A        0           NA
    ## 82245      NA         1                A        0           NA
    ## 82496      NA         1                A        0           NA
    ## 82746      NA         1                B        0           NA
    ## 82747      NA         1                B        0           NA
    ## 82748      NA         1                A        0           NA
    ## 82749      NA         1                B        0           NA
    ## 83010      NA         1                A        0           NA
    ## 83011      NA         1                B        0           NA
    ## 83463      NA         1                B        0           NA
    ## 83707      NA         1                B        0           NA
    ## 84219      NA         1                A        0           NA
    ## 84220      NA         1                A        0           NA
    ## 84221      NA         1                A        0           NA
    ## 84734      12         1                A        0           NA
    ## 85913      NA         1                A        0           NA
    ## 86411      NA         1                A        0           NA
    ## 87334      NA         1                B        0           NA
    ## 87335      NA         1                B        0           NA
    ## 87336      NA         1                B        0           NA
    ## 87337      NA         1                B        0           NA
    ## 87580      NA         1                A        0           NA
    ## 87581      NA         1                B        0           NA
    ## 87582      NA         1                B        0           NA
    ## 87583      NA         1                A        0           NA
    ## 87828      NA         1                A        0           NA
    ## 87829      NA         1                A        0           NA
    ## 88751      NA         1                A        0           NA
    ## 88752      NA         1                A        0           NA
    ## 89493      NA         1                A        0           NA
    ## 89494      NA         1                B        0           NA
    ## 89495      NA         1                B        0           NA
    ## 89496      NA         1                A        0           NA
    ## 89748      NA         1                A        0           NA
    ## 91182      NA         1                A        0           NA
    ## 91762      NA         1                A        0           NA
    ## 91763      NA         1                A        0           NA
    ## 93003      NA         1                A        0           NA
    ## 93251      NA         1                A        0           NA
    ## 94150      NA         1                B        0           NA
    ## 94151      NA         1                B        0           NA
    ## 94152      NA         1                B        0           NA
    ## 94153      NA         1                B        0           NA
    ## 94154      NA         1                A        0           NA
    ## 94155      NA         1                B        0           NA
    ## 94388      NA         1                A        0           NA
    ## 94392      NA         1                A        0           NA
    ## 94393      NA         1                A        0           NA
    ## 94394      NA         1                B        0           NA
    ## 94395      NA         1                A        0           NA
    ## 94396      NA         1                B        0           NA
    ## 94397      NA         1                A        0           NA
    ## 94644      NA         1                B        0           NA
    ## 94645      NA         1                A        0           NA
    ## 94893      NA         1                A        0           NA
    ## 94894      NA         1                A        0           NA
    ## 95200      NA         1                A        0           NA
    ## 95308      NA         1                A        0           NA
    ## 95309      NA         1                A        0           NA
    ## 95310      NA         1                A        0           NA
    ## 95471      NA         1                A        0           NA
    ## 95556      NA         1                A        0           NA
    ## 95557      NA         1                B        0           NA
    ## 95558      NA         1                B        0           NA
    ## 95559      NA         1                A        0           NA
    ## 95560      NA         1                B        0           NA
    ## 95796      NA         1                A        0           NA
    ## 95797      NA         1                A        0           NA
    ## 95798      NA         1                B        0           NA
    ## 95799      NA         1                C        0           NA
    ## 95800      NA         1                B        0           NA
    ## 95801      NA         1                B        0           NA
    ## 95802      NA         1                B        0           NA
    ## 95803      NA         1                A        0           NA
    ## 95804      NA         1                A        0           NA
    ## 95805      NA         1                B        0           NA
    ## 95806      NA         1                B        0           NA
    ## 96051      NA         1                B        0           NA
    ## 96052      NA         1                B        0           NA
    ## 96301      NA         1                B        0           NA
    ## 96302      NA         1                B        0           NA
    ## 96303      NA         1                B        0           NA
    ## 96304      NA         1                B        0           NA
    ## 96549      NA         1                A        0           NA
    ## 96550      NA         1                B        0           NA
    ## 96551      NA         1                A        0           NA
    ## 96552      NA         1                A        0           NA
    ## 96553      NA         1                B        0           NA
    ## 96554      NA         1                B        0           NA
    ## 96555      NA         1                B        0           NA
    ## 96556      NA         1                A        0           NA
    ## 96745      NA         1                A        0           NA
    ## 96746      NA         1                A        0           NA
    ## 96747      NA         1                A        0           NA
    ## 96748      NA         1                A        0           NA
    ## 96973      NA         1                A        0           NA
    ## 96974      NA         1                B        0           NA
    ## 96975      NA         1                B        0           NA
    ## 96976      NA         1                A        0           NA
    ## 96977      NA         1                B        0           NA
    ## 96978      NA         1                B        0           NA
    ## 97227      NA         1                A        0           NA
    ## 97228      NA         1                A        0           NA
    ## 97472      NA         1                A        0           NA
    ## 97473      NA         1                C        0           NA
    ## 97474      NA         1                A        0           NA
    ## 97475      NA         1                C        0           NA
    ## 97476      NA         1                C        0           NA
    ## 97678      NA         1                B        0           NA
    ## 97679      NA         1                B        0           NA
    ## 98125      NA         1                B        0           NA
    ## 98126      NA         1                A        0           NA
    ## 98127      NA         1                B        0           NA
    ## 98128      NA         1                B        0           NA
    ## 98330      NA         1                A        0           NA
    ## 98331      NA         1                B        0           NA
    ## 98332      NA         1                B        0           NA
    ## 98556      NA         1                A        0           NA
    ## 98804      NA         1                A        0           NA
    ## 99048      NA         1                A        0           NA
    ## 99049      NA         1                A        0           NA
    ## 99050      NA         1                A        0           NA
    ## 99259      NA         1                B        0           NA
    ## 99260      NA         1                B        0           NA
    ##       WeatherDelay NASDelay SecurityDelay LateAircraftDelay
    ## 250             NA       NA            NA                NA
    ## 251             NA       NA            NA                NA
    ## 252             NA       NA            NA                NA
    ## 253             NA       NA            NA                NA
    ## 254             NA       NA            NA                NA
    ## 255             NA       NA            NA                NA
    ## 552             NA       NA            NA                NA
    ## 553             NA       NA            NA                NA
    ## 554             NA       NA            NA                NA
    ## 555             NA       NA            NA                NA
    ## 849             NA       NA            NA                NA
    ## 850             NA       NA            NA                NA
    ## 851             NA       NA            NA                NA
    ## 852             NA       NA            NA                NA
    ## 853             NA       NA            NA                NA
    ## 854             NA       NA            NA                NA
    ## 1151            NA       NA            NA                NA
    ## 1152            NA       NA            NA                NA
    ## 1153            NA       NA            NA                NA
    ## 1154            NA       NA            NA                NA
    ## 1674            NA       NA            NA                NA
    ## 1675            NA       NA            NA                NA
    ## 1676            NA       NA            NA                NA
    ## 2259            NA       NA            NA                NA
    ## 2550            NA       NA            NA                NA
    ## 2551            NA       NA            NA                NA
    ## 2846            NA       NA            NA                NA
    ## 2847            NA       NA            NA                NA
    ## 2848            NA       NA            NA                NA
    ## 2849            NA       NA            NA                NA
    ## 3147            NA       NA            NA                NA
    ## 3370            NA       NA            NA                NA
    ## 3643            NA       NA            NA                NA
    ## 3929            NA       NA            NA                NA
    ## 3930            NA       NA            NA                NA
    ## 3931            NA       NA            NA                NA
    ## 3932            NA       NA            NA                NA
    ## 3933            NA       NA            NA                NA
    ## 3934            NA       NA            NA                NA
    ## 3935            NA       NA            NA                NA
    ## 3936            NA       NA            NA                NA
    ## 3937            NA       NA            NA                NA
    ## 3938            NA       NA            NA                NA
    ## 3939            NA       NA            NA                NA
    ## 4222            NA       NA            NA                NA
    ## 4223            NA       NA            NA                NA
    ## 4224            NA       NA            NA                NA
    ## 4225            NA       NA            NA                NA
    ## 4514            NA       NA            NA                NA
    ## 4515            NA       NA            NA                NA
    ## 4516            NA       NA            NA                NA
    ## 4517            NA       NA            NA                NA
    ## 4810            NA       NA            NA                NA
    ## 4811            NA       NA            NA                NA
    ## 4812            NA       NA            NA                NA
    ## 4813            NA       NA            NA                NA
    ## 4814            NA       NA            NA                NA
    ## 4815            NA       NA            NA                NA
    ## 5114            NA       NA            NA                NA
    ## 5320            NA       NA            NA                NA
    ## 5321            NA       NA            NA                NA
    ## 5322            NA       NA            NA                NA
    ## 5323            NA       NA            NA                NA
    ## 5324            NA       NA            NA                NA
    ## 5325            NA       NA            NA                NA
    ## 5326            NA       NA            NA                NA
    ## 5593            NA       NA            NA                NA
    ## 5594            NA       NA            NA                NA
    ## 5595            NA       NA            NA                NA
    ## 5596            NA       NA            NA                NA
    ## 5892            NA       NA            NA                NA
    ## 5893            NA       NA            NA                NA
    ## 5894            NA       NA            NA                NA
    ## 6175            NA       NA            NA                NA
    ## 6176            NA       NA            NA                NA
    ## 6177            NA       NA            NA                NA
    ## 6178            NA       NA            NA                NA
    ## 6179            NA       NA            NA                NA
    ## 6180            NA       NA            NA                NA
    ## 6470            NA       NA            NA                NA
    ## 6471            NA       NA            NA                NA
    ## 6472            NA       NA            NA                NA
    ## 6767            NA       NA            NA                NA
    ## 6768            NA       NA            NA                NA
    ## 6769            NA       NA            NA                NA
    ## 6770            NA       NA            NA                NA
    ## 6771            NA       NA            NA                NA
    ## 6772            NA       NA            NA                NA
    ## 7066            NA       NA            NA                NA
    ## 7067            NA       NA            NA                NA
    ## 7068            NA       NA            NA                NA
    ## 7069            NA       NA            NA                NA
    ## 7070            NA       NA            NA                NA
    ## 7280            NA       NA            NA                NA
    ## 7281            NA       NA            NA                NA
    ## 7282            NA       NA            NA                NA
    ## 7546            NA       NA            NA                NA
    ## 7547            NA       NA            NA                NA
    ## 7548            NA       NA            NA                NA
    ## 7549            NA       NA            NA                NA
    ## 7550            NA       NA            NA                NA
    ## 7551            NA       NA            NA                NA
    ## 7552            NA       NA            NA                NA
    ## 7553            NA       NA            NA                NA
    ## 7842            NA       NA            NA                NA
    ## 7843            NA       NA            NA                NA
    ## 7844            NA       NA            NA                NA
    ## 7845            NA       NA            NA                NA
    ## 7846            NA       NA            NA                NA
    ## 7847            NA       NA            NA                NA
    ## 7848            NA       NA            NA                NA
    ## 7849            NA       NA            NA                NA
    ## 8122            NA       NA            NA                NA
    ## 8123            NA       NA            NA                NA
    ## 8124            NA       NA            NA                NA
    ## 8125            NA       NA            NA                NA
    ## 8126            NA       NA            NA                NA
    ## 8127            NA       NA            NA                NA
    ## 8128            NA       NA            NA                NA
    ## 8129            NA       NA            NA                NA
    ## 8130            NA       NA            NA                NA
    ## 8131            NA       NA            NA                NA
    ## 8132            NA       NA            NA                NA
    ## 8133            NA       NA            NA                NA
    ## 8134            NA       NA            NA                NA
    ## 8135            NA       NA            NA                NA
    ## 8424            NA       NA            NA                NA
    ## 8425            NA       NA            NA                NA
    ## 8426            NA       NA            NA                NA
    ## 8427            NA       NA            NA                NA
    ## 8719            NA       NA            NA                NA
    ## 8720            NA       NA            NA                NA
    ## 8721            NA       NA            NA                NA
    ## 8722            NA       NA            NA                NA
    ## 8723            NA       NA            NA                NA
    ## 8724            NA       NA            NA                NA
    ## 8725            NA       NA            NA                NA
    ## 8726            NA       NA            NA                NA
    ## 9023            NA       NA            NA                NA
    ## 9024            NA       NA            NA                NA
    ## 9226            NA       NA            NA                NA
    ## 9227            NA       NA            NA                NA
    ## 9228            NA       NA            NA                NA
    ## 9229            NA       NA            NA                NA
    ## 9230            NA       NA            NA                NA
    ## 9231            NA       NA            NA                NA
    ## 9232            NA       NA            NA                NA
    ## 9233            NA       NA            NA                NA
    ## 9499            NA       NA            NA                NA
    ## 9500            NA       NA            NA                NA
    ## 9501            NA       NA            NA                NA
    ## 9502            NA       NA            NA                NA
    ## 9503            NA       NA            NA                NA
    ## 9785            NA       NA            NA                NA
    ## 9786            NA       NA            NA                NA
    ## 9787            NA       NA            NA                NA
    ## 9788            NA       NA            NA                NA
    ## 9789            NA       NA            NA                NA
    ## 9790            NA       NA            NA                NA
    ## 9791            NA       NA            NA                NA
    ## 9792            NA       NA            NA                NA
    ## 9793            NA       NA            NA                NA
    ## 9794            NA       NA            NA                NA
    ## 9795            NA       NA            NA                NA
    ## 9796            NA       NA            NA                NA
    ## 9797            NA       NA            NA                NA
    ## 9798            NA       NA            NA                NA
    ## 9799            NA       NA            NA                NA
    ## 10078           NA       NA            NA                NA
    ## 10079           NA       NA            NA                NA
    ## 10080           NA       NA            NA                NA
    ## 10081           NA       NA            NA                NA
    ## 10082           NA       NA            NA                NA
    ## 10083           NA       NA            NA                NA
    ## 10084           NA       NA            NA                NA
    ## 10085           NA       NA            NA                NA
    ## 10360           NA       NA            NA                NA
    ## 10361           NA       NA            NA                NA
    ## 10362           NA       NA            NA                NA
    ## 10363           NA       NA            NA                NA
    ## 10364           NA       NA            NA                NA
    ## 10365           NA       NA            NA                NA
    ## 10366           NA       NA            NA                NA
    ## 10367           NA       NA            NA                NA
    ## 10368           NA       NA            NA                NA
    ## 10369           NA       NA            NA                NA
    ## 10370           NA       NA            NA                NA
    ## 10371           NA       NA            NA                NA
    ## 10372           NA       NA            NA                NA
    ## 10373           NA       NA            NA                NA
    ## 10374           NA       NA            NA                NA
    ## 10375           NA       NA            NA                NA
    ## 10376           NA       NA            NA                NA
    ## 10377           NA       NA            NA                NA
    ## 10672           NA       NA            NA                NA
    ## 10673           NA       NA            NA                NA
    ## 10674           NA       NA            NA                NA
    ## 10675           NA       NA            NA                NA
    ## 10971           NA       NA            NA                NA
    ## 10972           NA       NA            NA                NA
    ## 10973           NA       NA            NA                NA
    ## 11183           NA       NA            NA                NA
    ## 11750           NA       NA            NA                NA
    ## 11751           NA       NA            NA                NA
    ## 12035           NA       NA            NA                NA
    ## 12036           NA       NA            NA                NA
    ## 12037           NA       NA            NA                NA
    ## 12038           NA       NA            NA                NA
    ## 12328           NA       NA            NA                NA
    ## 12329           NA       NA            NA                NA
    ## 12330           NA       NA            NA                NA
    ## 12331           NA       NA            NA                NA
    ## 12332           NA       NA            NA                NA
    ## 12333           NA       NA            NA                NA
    ## 12334           NA       NA            NA                NA
    ## 12335           NA       NA            NA                NA
    ## 12336           NA       NA            NA                NA
    ## 12632           NA       NA            NA                NA
    ## 12633           NA       NA            NA                NA
    ## 12634           NA       NA            NA                NA
    ## 12635           NA       NA            NA                NA
    ## 12636           NA       NA            NA                NA
    ## 12637           NA       NA            NA                NA
    ## 12638           NA       NA            NA                NA
    ## 12639           NA       NA            NA                NA
    ## 12942           NA       NA            NA                NA
    ## 12943           NA       NA            NA                NA
    ## 13149           NA       NA            NA                NA
    ## 13150           NA       NA            NA                NA
    ## 13151           NA       NA            NA                NA
    ## 13152           NA       NA            NA                NA
    ## 13153           NA       NA            NA                NA
    ## 13154           NA       NA            NA                NA
    ## 13155           NA       NA            NA                NA
    ## 13156           NA       NA            NA                NA
    ## 13157           NA       NA            NA                NA
    ## 13158           NA       NA            NA                NA
    ## 13159           NA       NA            NA                NA
    ## 13160           NA       NA            NA                NA
    ## 13161           NA       NA            NA                NA
    ## 13422           NA       NA            NA                NA
    ## 13423           NA       NA            NA                NA
    ## 13424           NA       NA            NA                NA
    ## 13425           NA       NA            NA                NA
    ## 13426           NA       NA            NA                NA
    ## 13427           NA       NA            NA                NA
    ## 13428           NA       NA            NA                NA
    ## 13429           NA       NA            NA                NA
    ## 13430           NA       NA            NA                NA
    ## 13725           NA       NA            NA                NA
    ## 13726           NA       NA            NA                NA
    ## 14021           NA       NA            NA                NA
    ## 14022           NA       NA            NA                NA
    ## 14023           NA       NA            NA                NA
    ## 14024           NA       NA            NA                NA
    ## 14025           NA       NA            NA                NA
    ## 14325           NA       NA            NA                NA
    ## 14326           NA       NA            NA                NA
    ## 14623           NA       NA            NA                NA
    ## 14624           NA       NA            NA                NA
    ## 14625           NA       NA            NA                NA
    ## 14626           NA       NA            NA                NA
    ## 14627           NA       NA            NA                NA
    ## 14628           NA       NA            NA                NA
    ## 14629           NA       NA            NA                NA
    ## 14630           NA       NA            NA                NA
    ## 14918           NA       NA            NA                NA
    ## 14919           NA       NA            NA                NA
    ## 14920           NA       NA            NA                NA
    ## 14921           NA       NA            NA                NA
    ## 14922           NA       NA            NA                NA
    ## 14923           NA       NA            NA                NA
    ## 14924           NA       NA            NA                NA
    ## 14925           NA       NA            NA                NA
    ## 14926           NA       NA            NA                NA
    ## 14927           NA       NA            NA                NA
    ## 14928           NA       NA            NA                NA
    ## 14929           NA       NA            NA                NA
    ## 14930           NA       NA            NA                NA
    ## 14931           NA       NA            NA                NA
    ## 14932           NA       NA            NA                NA
    ## 14933           NA       NA            NA                NA
    ## 14934           NA       NA            NA                NA
    ## 15154           NA       NA            NA                NA
    ## 15155           NA       NA            NA                NA
    ## 15156           NA       NA            NA                NA
    ## 15436           NA       NA            NA                NA
    ## 15728           NA       NA            NA                NA
    ## 15729           NA       NA            NA                NA
    ## 15730           NA       NA            NA                NA
    ## 15731           NA       NA            NA                NA
    ## 15732           NA       NA            NA                NA
    ## 15733           NA       NA            NA                NA
    ## 15734           NA       NA            NA                NA
    ## 15735           NA       NA            NA                NA
    ## 15736           NA       NA            NA                NA
    ## 15737           NA       NA            NA                NA
    ## 15738           NA       NA            NA                NA
    ## 16034           NA       NA            NA                NA
    ## 16035           NA       NA            NA                NA
    ## 16036           NA       NA            NA                NA
    ## 16037           NA       NA            NA                NA
    ## 16318           NA       NA            NA                NA
    ## 16596           NA       NA            NA                NA
    ## 16597           NA       NA            NA                NA
    ## 16598           NA       NA            NA                NA
    ## 16599           NA       NA            NA                NA
    ## 16600           NA       NA            NA                NA
    ## 16882           NA       NA            NA                NA
    ## 17105           NA       NA            NA                NA
    ## 17680           NA       NA            NA                NA
    ## 17681           NA       NA            NA                NA
    ## 17682           NA       NA            NA                NA
    ## 17683           NA       NA            NA                NA
    ## 17684           NA       NA            NA                NA
    ## 17685           NA       NA            NA                NA
    ## 17686           NA       NA            NA                NA
    ## 17687           NA       NA            NA                NA
    ## 17688           NA       NA            NA                NA
    ## 17689           NA       NA            NA                NA
    ## 17690           NA       NA            NA                NA
    ## 17691           NA       NA            NA                NA
    ## 17977           NA       NA            NA                NA
    ## 17978           NA       NA            NA                NA
    ## 17979           NA       NA            NA                NA
    ## 17980           NA       NA            NA                NA
    ## 17981           NA       NA            NA                NA
    ## 17982           NA       NA            NA                NA
    ## 17983           NA       NA            NA                NA
    ## 17984           NA       NA            NA                NA
    ## 17985           NA       NA            NA                NA
    ## 17986           NA       NA            NA                NA
    ## 17987           NA       NA            NA                NA
    ## 17988           NA       NA            NA                NA
    ## 18286           NA       NA            NA                NA
    ## 18287           NA       NA            NA                NA
    ## 18288           NA       NA            NA                NA
    ## 18289           NA       NA            NA                NA
    ## 18567           NA       NA            NA                NA
    ## 18568           NA       NA            NA                NA
    ## 18569           NA       NA            NA                NA
    ## 18570           NA       NA            NA                NA
    ## 18571           NA       NA            NA                NA
    ## 18572           NA       NA            NA                NA
    ## 18573           NA       NA            NA                NA
    ## 18574           NA       NA            NA                NA
    ## 18575           NA       NA            NA                NA
    ## 18576           NA       NA            NA                NA
    ## 18577           NA       NA            NA                NA
    ## 18578           NA       NA            NA                NA
    ## 18579           NA       NA            NA                NA
    ## 18580           NA       NA            NA                NA
    ## 18581           NA       NA            NA                NA
    ## 18582           NA       NA            NA                NA
    ## 18583           NA       NA            NA                NA
    ## 18584           NA       NA            NA                NA
    ## 18585           NA       NA            NA                NA
    ## 18586           NA       NA            NA                NA
    ## 18587           NA       NA            NA                NA
    ## 18588           NA       NA            NA                NA
    ## 18589           NA       NA            NA                NA
    ## 18590           NA       NA            NA                NA
    ## 18591           NA       NA            NA                NA
    ## 18592           NA       NA            NA                NA
    ## 18593           NA       NA            NA                NA
    ## 18883           NA       NA            NA                NA
    ## 18884           NA       NA            NA                NA
    ## 18885           NA       NA            NA                NA
    ## 18886           NA       NA            NA                NA
    ## 18887           NA       NA            NA                NA
    ## 18888           NA       NA            NA                NA
    ## 18889           NA       NA            NA                NA
    ## 18890           NA       NA            NA                NA
    ## 18891           NA       NA            NA                NA
    ## 18892           NA       NA            NA                NA
    ## 18893           NA       NA            NA                NA
    ## 18894           NA       NA            NA                NA
    ## 18895           NA       NA            NA                NA
    ## 18896           NA       NA            NA                NA
    ## 18897           NA       NA            NA                NA
    ## 19118           NA       NA            NA                NA
    ## 19119           NA       NA            NA                NA
    ## 19120           NA       NA            NA                NA
    ## 19121           NA       NA            NA                NA
    ## 19122           NA       NA            NA                NA
    ## 19123           NA       NA            NA                NA
    ## 19406           NA       NA            NA                NA
    ## 19407           NA       NA            NA                NA
    ## 19408           NA       NA            NA                NA
    ## 19409           NA       NA            NA                NA
    ## 19705           NA       NA            NA                NA
    ## 19706           NA       NA            NA                NA
    ## 19707           NA       NA            NA                NA
    ## 19708           NA       NA            NA                NA
    ## 19709           NA       NA            NA                NA
    ## 19710           NA       NA            NA                NA
    ## 19711           NA       NA            NA                NA
    ## 19712           NA       NA            NA                NA
    ## 19713           NA       NA            NA                NA
    ## 19999           NA       NA            NA                NA
    ## 20000           NA       NA            NA                NA
    ## 20001           NA       NA            NA                NA
    ## 20002           NA       NA            NA                NA
    ## 20003           NA       NA            NA                NA
    ## 20004           NA       NA            NA                NA
    ## 20005           NA       NA            NA                NA
    ## 20006           NA       NA            NA                NA
    ## 20007           NA       NA            NA                NA
    ## 20008           NA       NA            NA                NA
    ## 20009           NA       NA            NA                NA
    ## 20010           NA       NA            NA                NA
    ## 20311           NA       NA            NA                NA
    ## 20611           NA       NA            NA                NA
    ## 20612           NA       NA            NA                NA
    ## 20613           NA       NA            NA                NA
    ## 20614           NA       NA            NA                NA
    ## 20615           NA       NA            NA                NA
    ## 20918           NA       NA            NA                NA
    ## 20919           NA       NA            NA                NA
    ## 21147           NA       NA            NA                NA
    ## 21432           NA       NA            NA                NA
    ## 21433           NA       NA            NA                NA
    ## 21434           NA       NA            NA                NA
    ## 21736           NA       NA            NA                NA
    ## 21737           NA       NA            NA                NA
    ## 21738           NA       NA            NA                NA
    ## 21739           NA       NA            NA                NA
    ## 21740           NA       NA            NA                NA
    ## 22000           NA       NA            NA                NA
    ## 22001           NA       NA            NA                NA
    ## 22002           NA       NA            NA                NA
    ## 22003           NA       NA            NA                NA
    ## 22004           NA       NA            NA                NA
    ## 22005           NA       NA            NA                NA
    ## 22006           NA       NA            NA                NA
    ## 22007           NA       NA            NA                NA
    ## 22008           NA       NA            NA                NA
    ## 22009           NA       NA            NA                NA
    ## 22010           NA       NA            NA                NA
    ## 22011           NA       NA            NA                NA
    ## 22012           NA       NA            NA                NA
    ## 22013           NA       NA            NA                NA
    ## 22014           NA       NA            NA                NA
    ## 22015           NA       NA            NA                NA
    ## 22016           NA       NA            NA                NA
    ## 22017           NA       NA            NA                NA
    ## 22018           NA       NA            NA                NA
    ## 22019           NA       NA            NA                NA
    ## 22020           NA       NA            NA                NA
    ## 22021           NA       NA            NA                NA
    ## 22022           NA       NA            NA                NA
    ## 22023           NA       NA            NA                NA
    ## 22024           NA       NA            NA                NA
    ## 22025           NA       NA            NA                NA
    ## 22026           NA       NA            NA                NA
    ## 22027           NA       NA            NA                NA
    ## 22028           NA       NA            NA                NA
    ## 22029           NA       NA            NA                NA
    ## 22030           NA       NA            NA                NA
    ## 22031           NA       NA            NA                NA
    ## 22032           NA       NA            NA                NA
    ## 22033           NA       NA            NA                NA
    ## 22034           NA       NA            NA                NA
    ## 22035           NA       NA            NA                NA
    ## 22036           NA       NA            NA                NA
    ## 22037           NA       NA            NA                NA
    ## 22038           NA       NA            NA                NA
    ## 22039           NA       NA            NA                NA
    ## 22040           NA       NA            NA                NA
    ## 22041           NA       NA            NA                NA
    ## 22042           NA       NA            NA                NA
    ## 22043           NA       NA            NA                NA
    ## 22338           NA       NA            NA                NA
    ## 22339           NA       NA            NA                NA
    ## 22340           NA       NA            NA                NA
    ## 22341           NA       NA            NA                NA
    ## 22342           NA       NA            NA                NA
    ## 22343           NA       NA            NA                NA
    ## 22344           NA       NA            NA                NA
    ## 22345           NA       NA            NA                NA
    ## 22346           NA       NA            NA                NA
    ## 22652           NA       NA            NA                NA
    ## 22952           NA       NA            NA                NA
    ## 22953           NA       NA            NA                NA
    ## 22954           NA       NA            NA                NA
    ## 22955           NA       NA            NA                NA
    ## 22956           NA       NA            NA                NA
    ## 22957           NA       NA            NA                NA
    ## 22958           NA       NA            NA                NA
    ## 22959           NA       NA            NA                NA
    ## 23472           NA       NA            NA                NA
    ## 23473           NA       NA            NA                NA
    ## 24077           NA       NA            NA                NA
    ## 24078           NA       NA            NA                NA
    ## 24367           NA       NA            NA                NA
    ## 24368           NA       NA            NA                NA
    ## 24369           NA       NA            NA                NA
    ## 24370           NA       NA            NA                NA
    ## 24371           NA       NA            NA                NA
    ## 24372           NA       NA            NA                NA
    ## 24373           NA       NA            NA                NA
    ## 24374           NA       NA            NA                NA
    ## 24375           NA       NA            NA                NA
    ## 24376           NA       NA            NA                NA
    ## 24377           NA       NA            NA                NA
    ## 24378           NA       NA            NA                NA
    ## 24379           NA       NA            NA                NA
    ## 24380           NA       NA            NA                NA
    ## 24381           NA       NA            NA                NA
    ## 24682           NA       NA            NA                NA
    ## 24683           NA       NA            NA                NA
    ## 24684           NA       NA            NA                NA
    ## 24685           NA       NA            NA                NA
    ## 24686           NA       NA            NA                NA
    ## 24687           NA       NA            NA                NA
    ## 24992           NA       NA            NA                NA
    ## 24993           NA       NA            NA                NA
    ## 25218           NA       NA            NA                NA
    ## 25219           NA       NA            NA                NA
    ## 25792           NA       NA            NA                NA
    ## 25793           NA       NA            NA                NA
    ## 25794           NA       NA            NA                NA
    ## 25795           NA       NA            NA                NA
    ## 25796           NA       NA            NA                NA
    ## 25797           NA       NA            NA                NA
    ## 25798           NA       NA            NA                NA
    ## 25799           NA       NA            NA                NA
    ## 25800           NA       NA            NA                NA
    ## 25801           NA       NA            NA                NA
    ## 25802           NA       NA            NA                NA
    ## 25803           NA       NA            NA                NA
    ## 26088           NA       NA            NA                NA
    ## 26667           NA       NA            NA                NA
    ## 26668           NA       NA            NA                NA
    ## 26959           NA       NA            NA                NA
    ## 26960           NA       NA            NA                NA
    ## 27448           NA       NA            NA                NA
    ## 27732           NA       NA            NA                NA
    ## 27733           NA       NA            NA                NA
    ## 27734           NA       NA            NA                NA
    ## 27735           NA       NA            NA                NA
    ## 27736           NA       NA            NA                NA
    ## 27737           NA       NA            NA                NA
    ## 27738           NA       NA            NA                NA
    ## 28004           NA       NA            NA                NA
    ## 28005           NA       NA            NA                NA
    ## 28006           NA       NA            NA                NA
    ## 28007           NA       NA            NA                NA
    ## 28008           NA       NA            NA                NA
    ## 28009           NA       NA            NA                NA
    ## 28010           NA       NA            NA                NA
    ## 28011           NA       NA            NA                NA
    ## 28012           NA       NA            NA                NA
    ## 28013           NA       NA            NA                NA
    ## 28014           NA       NA            NA                NA
    ## 28015           NA       NA            NA                NA
    ## 28016           NA       NA            NA                NA
    ## 28017           NA       NA            NA                NA
    ## 28018           NA       NA            NA                NA
    ## 28019           NA       NA            NA                NA
    ## 28020           NA       NA            NA                NA
    ## 28021           NA       NA            NA                NA
    ## 28022           NA       NA            NA                NA
    ## 28023           NA       NA            NA                NA
    ## 28024           NA       NA            NA                NA
    ## 28025           NA       NA            NA                NA
    ## 28026           NA       NA            NA                NA
    ## 28027           NA       NA            NA                NA
    ## 28275           NA       NA            NA                NA
    ## 28276           NA       NA            NA                NA
    ## 28277           NA       NA            NA                NA
    ## 28278           NA       NA            NA                NA
    ## 28279           NA       NA            NA                NA
    ## 28280           NA       NA            NA                NA
    ## 28281           NA       NA            NA                NA
    ## 28282           NA       NA            NA                NA
    ## 28283           NA       NA            NA                NA
    ## 28284           NA       NA            NA                NA
    ## 28285           NA       NA            NA                NA
    ## 28286           NA       NA            NA                NA
    ## 28287           NA       NA            NA                NA
    ## 28288           NA       NA            NA                NA
    ## 28289           NA       NA            NA                NA
    ## 28290           NA       NA            NA                NA
    ## 28291           NA       NA            NA                NA
    ## 28292           NA       NA            NA                NA
    ## 28293           NA       NA            NA                NA
    ## 28294           NA       NA            NA                NA
    ## 28295           NA       NA            NA                NA
    ## 28296           NA       NA            NA                NA
    ## 28297           NA       NA            NA                NA
    ## 28298           NA       NA            NA                NA
    ## 28299           NA       NA            NA                NA
    ## 28300           NA       NA            NA                NA
    ## 28301           NA       NA            NA                NA
    ## 28302           NA       NA            NA                NA
    ## 28303           NA       NA            NA                NA
    ## 28304           NA       NA            NA                NA
    ## 28305           NA       NA            NA                NA
    ## 28306           NA       NA            NA                NA
    ## 28307           NA       NA            NA                NA
    ## 28308           NA       NA            NA                NA
    ## 28309           NA       NA            NA                NA
    ## 28310           NA       NA            NA                NA
    ## 28311           NA       NA            NA                NA
    ## 28312           NA       NA            NA                NA
    ## 28313           NA       NA            NA                NA
    ## 28314           NA       NA            NA                NA
    ## 28315           NA       NA            NA                NA
    ## 28316           NA       NA            NA                NA
    ## 28317           NA       NA            NA                NA
    ## 28318           NA       NA            NA                NA
    ## 28319           NA       NA            NA                NA
    ## 28320           NA       NA            NA                NA
    ## 28321           NA       NA            NA                NA
    ## 28585           NA       NA            NA                NA
    ## 28586           NA       NA            NA                NA
    ## 28587           NA       NA            NA                NA
    ## 28588           NA       NA            NA                NA
    ## 28589           NA       NA            NA                NA
    ## 28590           NA       NA            NA                NA
    ## 28591           NA       NA            NA                NA
    ## 28592           NA       NA            NA                NA
    ## 28593           NA       NA            NA                NA
    ## 28594           NA       NA            NA                NA
    ## 28595           NA       NA            NA                NA
    ## 28596           NA       NA            NA                NA
    ## 28597           NA       NA            NA                NA
    ## 28598           NA       NA            NA                NA
    ## 28599           NA       NA            NA                NA
    ## 28600           NA       NA            NA                NA
    ## 28601           NA       NA            NA                NA
    ## 28602           NA       NA            NA                NA
    ## 28603           NA       NA            NA                NA
    ## 28604           NA       NA            NA                NA
    ## 28605           NA       NA            NA                NA
    ## 28606           NA       NA            NA                NA
    ## 28607           NA       NA            NA                NA
    ## 28608           NA       NA            NA                NA
    ## 28609           NA       NA            NA                NA
    ## 28610           NA       NA            NA                NA
    ## 28611           NA       NA            NA                NA
    ## 28612           NA       NA            NA                NA
    ## 28613           NA       NA            NA                NA
    ## 28614           NA       NA            NA                NA
    ## 28615           NA       NA            NA                NA
    ## 28616           NA       NA            NA                NA
    ## 28617           NA       NA            NA                NA
    ## 28898           NA       NA            NA                NA
    ## 28899           NA       NA            NA                NA
    ## 28900           NA       NA            NA                NA
    ## 28901           NA       NA            NA                NA
    ## 28902           NA       NA            NA                NA
    ## 28903           NA       NA            NA                NA
    ## 28904           NA       NA            NA                NA
    ## 28905           NA       NA            NA                NA
    ## 28906           NA       NA            NA                NA
    ## 28907           NA       NA            NA                NA
    ## 28908           NA       NA            NA                NA
    ## 28909           NA       NA            NA                NA
    ## 28910           NA       NA            NA                NA
    ## 28911           NA       NA            NA                NA
    ## 28912           NA       NA            NA                NA
    ## 28913           NA       NA            NA                NA
    ## 28914           NA       NA            NA                NA
    ## 29131           NA       NA            NA                NA
    ## 29132           NA       NA            NA                NA
    ## 29133           NA       NA            NA                NA
    ## 29134           NA       NA            NA                NA
    ## 29135           NA       NA            NA                NA
    ## 29136           NA       NA            NA                NA
    ## 29137           NA       NA            NA                NA
    ## 29138           NA       NA            NA                NA
    ## 29139           NA       NA            NA                NA
    ## 29417           NA       NA            NA                NA
    ## 29418           NA       NA            NA                NA
    ## 29419           NA       NA            NA                NA
    ## 29420           NA       NA            NA                NA
    ## 29421           NA       NA            NA                NA
    ## 29715           NA       NA            NA                NA
    ## 30002           NA       NA            NA                NA
    ## 30003           NA       NA            NA                NA
    ## 30297           NA       NA            NA                NA
    ## 30589           NA       NA            NA                NA
    ## 30590           NA       NA            NA                NA
    ## 30591           NA       NA            NA                NA
    ## 30592           NA       NA            NA                NA
    ## 30593           NA       NA            NA                NA
    ## 30890           NA       NA            NA                NA
    ## 31113           NA       NA            NA                NA
    ## 31114           NA       NA            NA                NA
    ## 32270           NA       NA            NA                NA
    ## 32271           NA       NA            NA                NA
    ## 32272           NA       NA            NA                NA
    ## 32273           NA       NA            NA                NA
    ## 32566           NA       NA            NA                NA
    ## 32567           NA       NA            NA                NA
    ## 32568           NA       NA            NA                NA
    ## 32569           NA       NA            NA                NA
    ## 32570           NA       NA            NA                NA
    ## 32571           NA       NA            NA                NA
    ## 32863           NA       NA            NA                NA
    ## 32864           NA       NA            NA                NA
    ## 32865           NA       NA            NA                NA
    ## 32866           NA       NA            NA                NA
    ## 32867           NA       NA            NA                NA
    ## 32868           NA       NA            NA                NA
    ## 32869           NA       NA            NA                NA
    ## 32870           NA       NA            NA                NA
    ## 33095           NA       NA            NA                NA
    ## 33096           NA       NA            NA                NA
    ## 33674           NA       NA            NA                NA
    ## 33675           NA       NA            NA                NA
    ## 34259           NA       NA            NA                NA
    ## 34260           NA       NA            NA                NA
    ## 34261           NA       NA            NA                NA
    ## 34864           NA       NA            NA                NA
    ## 34865           NA       NA            NA                NA
    ## 34866           NA       NA            NA                NA
    ## 34867           NA       NA            NA                NA
    ## 34868           NA       NA            NA                NA
    ## 35097           NA       NA            NA                NA
    ## 35386           NA       NA            NA                NA
    ## 35387           NA       NA            NA                NA
    ## 35686           NA       NA            NA                NA
    ## 35687           NA       NA            NA                NA
    ## 35688           NA       NA            NA                NA
    ## 35689           NA       NA            NA                NA
    ## 35690           NA       NA            NA                NA
    ## 35691           NA       NA            NA                NA
    ## 35980           NA       NA            NA                NA
    ## 35981           NA       NA            NA                NA
    ## 35982           NA       NA            NA                NA
    ## 35983           NA       NA            NA                NA
    ## 35984           NA       NA            NA                NA
    ## 35985           NA       NA            NA                NA
    ## 35986           NA       NA            NA                NA
    ## 35987           NA       NA            NA                NA
    ## 35988           NA       NA            NA                NA
    ## 36287           NA       NA            NA                NA
    ## 36288           NA       NA            NA                NA
    ## 36289           NA       NA            NA                NA
    ## 36290           NA       NA            NA                NA
    ## 36291           NA       NA            NA                NA
    ## 36292           NA       NA            NA                NA
    ## 36596           NA       NA            NA                NA
    ## 36597           NA       NA            NA                NA
    ## 36598           NA       NA            NA                NA
    ## 36904           NA       NA            NA                NA
    ## 36905           NA       NA            NA                NA
    ## 37148           NA       NA            NA                NA
    ## 37149           NA       NA            NA                NA
    ## 37436           NA       NA            NA                NA
    ## 37437           NA       NA            NA                NA
    ## 37438           NA       NA            NA                NA
    ## 37439           NA       NA            NA                NA
    ## 37440           NA       NA            NA                NA
    ## 38033           NA       NA            NA                NA
    ## 38034           NA       NA            NA                NA
    ## 38035           NA       NA            NA                NA
    ## 38036           NA       NA            NA                NA
    ## 38335           NA       NA            NA                NA
    ## 38336           NA       NA            NA                NA
    ## 38337           NA       NA            NA                NA
    ## 38338           NA       NA            NA                NA
    ## 38339           NA       NA            NA                NA
    ## 38340           NA       NA            NA                NA
    ## 38341           NA       NA            NA                NA
    ## 38342           NA       NA            NA                NA
    ## 38650           NA       NA            NA                NA
    ## 38958           NA       NA            NA                NA
    ## 38959           NA       NA            NA                NA
    ## 39204           NA       NA            NA                NA
    ## 39495           NA       NA            NA                NA
    ## 39496           NA       NA            NA                NA
    ## 40407           NA       NA            NA                NA
    ## 40408           NA       NA            NA                NA
    ## 40716           NA       NA            NA                NA
    ## 41526           NA       NA            NA                NA
    ## 41527           NA       NA            NA                NA
    ## 41528           NA       NA            NA                NA
    ## 41529           NA       NA            NA                NA
    ## 41806           NA       NA            NA                NA
    ## 41807           NA       NA            NA                NA
    ## 41808           NA       NA            NA                NA
    ## 42086           NA       NA            NA                NA
    ## 42087           NA       NA            NA                NA
    ## 42088           NA       NA            NA                NA
    ## 42089           NA       NA            NA                NA
    ## 42090           NA       NA            NA                NA
    ## 42091           NA       NA            NA                NA
    ## 42092           NA       NA            NA                NA
    ## 42093           NA       NA            NA                NA
    ## 42094           NA       NA            NA                NA
    ## 42095           NA       NA            NA                NA
    ## 42096           NA       NA            NA                NA
    ## 42097           NA       NA            NA                NA
    ## 42098           NA       NA            NA                NA
    ## 42099           NA       NA            NA                NA
    ## 42100           NA       NA            NA                NA
    ## 42101           NA       NA            NA                NA
    ## 42102           NA       NA            NA                NA
    ## 42103           NA       NA            NA                NA
    ## 42104           NA       NA            NA                NA
    ## 42105           NA       NA            NA                NA
    ## 42106           NA       NA            NA                NA
    ## 42107           NA       NA            NA                NA
    ## 42108           NA       NA            NA                NA
    ## 42109           NA       NA            NA                NA
    ## 42110           NA       NA            NA                NA
    ## 42111           NA       NA            NA                NA
    ## 42112           NA       NA            NA                NA
    ## 42113           NA       NA            NA                NA
    ## 42114           NA       NA            NA                NA
    ## 42413           NA       NA            NA                NA
    ## 42414           NA       NA            NA                NA
    ## 42415           NA       NA            NA                NA
    ## 42416           NA       NA            NA                NA
    ## 42417           NA       NA            NA                NA
    ## 42418           NA       NA            NA                NA
    ## 42419           NA       NA            NA                NA
    ## 42420           NA       NA            NA                NA
    ## 42726           NA       NA            NA                NA
    ## 42727           NA       NA            NA                NA
    ## 42728           NA       NA            NA                NA
    ## 43034           NA       NA            NA                NA
    ## 43035           NA       NA            NA                NA
    ## 43036           NA       NA            NA                NA
    ## 43037           NA       NA            NA                NA
    ## 43281           NA       NA            NA                NA
    ## 43282           NA       NA            NA                NA
    ## 43569           NA       NA            NA                NA
    ## 43570           NA       NA            NA                NA
    ## 43571           NA       NA            NA                NA
    ## 43572           NA       NA            NA                NA
    ## 43573           NA       NA            NA                NA
    ## 43574           NA       NA            NA                NA
    ## 43879           NA       NA            NA                NA
    ## 43880           NA       NA            NA                NA
    ## 44179           NA       NA            NA                NA
    ## 44180           NA       NA            NA                NA
    ## 44485           NA       NA            NA                NA
    ## 44797           NA       NA            NA                NA
    ## 44798           NA       NA            NA                NA
    ## 45113           NA       NA            NA                NA
    ## 45114           NA       NA            NA                NA
    ## 45363           NA       NA            NA                NA
    ## 45364           NA       NA            NA                NA
    ## 45365           NA       NA            NA                NA
    ## 45366           NA       NA            NA                NA
    ## 45664           NA       NA            NA                NA
    ## 45665           NA       NA            NA                NA
    ## 45970           NA       NA            NA                NA
    ## 45971           NA       NA            NA                NA
    ## 45972           NA       NA            NA                NA
    ## 45973           NA       NA            NA                NA
    ## 45974           NA       NA            NA                NA
    ## 45975           NA       NA            NA                NA
    ## 45976           NA       NA            NA                NA
    ## 45977           NA       NA            NA                NA
    ## 45978           NA       NA            NA                NA
    ## 45979           NA       NA            NA                NA
    ## 46289           NA       NA            NA                NA
    ## 46290           NA       NA            NA                NA
    ## 46291           NA       NA            NA                NA
    ## 46292           NA       NA            NA                NA
    ## 46293           NA       NA            NA                NA
    ## 46603           NA       NA            NA                NA
    ## 46604           NA       NA            NA                NA
    ## 46605           NA       NA            NA                NA
    ## 46606           NA       NA            NA                NA
    ## 46607           NA       NA            NA                NA
    ## 46921           NA       NA            NA                NA
    ## 46922           NA       NA            NA                NA
    ## 46923           NA       NA            NA                NA
    ## 47239           NA       NA            NA                NA
    ## 47240           NA       NA            NA                NA
    ## 47797           NA       NA            NA                NA
    ## 48108           NA       NA            NA                NA
    ## 48109           NA       NA            NA                NA
    ## 48110           NA       NA            NA                NA
    ## 48111           NA       NA            NA                NA
    ## 48414           NA       NA            NA                NA
    ## 48415           NA       NA            NA                NA
    ## 48416           NA       NA            NA                NA
    ## 48417           NA       NA            NA                NA
    ## 48418           NA       NA            NA                NA
    ## 48419           NA       NA            NA                NA
    ## 48420           NA       NA            NA                NA
    ## 48421           NA       NA            NA                NA
    ## 48422           NA       NA            NA                NA
    ## 48423           NA       NA            NA                NA
    ## 48424           NA       NA            NA                NA
    ## 48425           NA       NA            NA                NA
    ## 48738           NA       NA            NA                NA
    ## 48739           NA       NA            NA                NA
    ## 49047           NA       NA            NA                NA
    ## 49048           NA       NA            NA                NA
    ## 49049           NA       NA            NA                NA
    ## 49050           NA       NA            NA                NA
    ## 49051           NA       NA            NA                NA
    ## 49052           NA       NA            NA                NA
    ## 49053           NA       NA            NA                NA
    ## 49054           NA       NA            NA                NA
    ## 49368           NA       NA            NA                NA
    ## 49369           NA       NA            NA                NA
    ## 49370           NA       NA            NA                NA
    ## 49371           NA       NA            NA                NA
    ## 49922           NA       NA            NA                NA
    ## 49923           NA       NA            NA                NA
    ## 49924           NA       NA            NA                NA
    ## 49925           NA       NA            NA                NA
    ## 49926           NA       NA            NA                NA
    ## 49927           NA       NA            NA                NA
    ## 49928           NA       NA            NA                NA
    ## 49929           NA       NA            NA                NA
    ## 50556           NA       NA            NA                NA
    ## 50557           NA       NA            NA                NA
    ## 51176           NA       NA            NA                NA
    ## 51177           NA       NA            NA                NA
    ## 51178           NA       NA            NA                NA
    ## 51179           NA       NA            NA                NA
    ## 51180           NA       NA            NA                NA
    ## 51181           NA       NA            NA                NA
    ## 51182           NA       NA            NA                NA
    ## 51183           NA       NA            NA                NA
    ## 51184           NA       NA            NA                NA
    ## 51185           NA       NA            NA                NA
    ## 51501           NA       NA            NA                NA
    ## 51502           NA       NA            NA                NA
    ## 51756           NA       NA            NA                NA
    ## 51757           NA       NA            NA                NA
    ## 51758           NA       NA            NA                NA
    ## 51759           NA       NA            NA                NA
    ## 51760           NA       NA            NA                NA
    ## 51761           NA       NA            NA                NA
    ## 52055           NA       NA            NA                NA
    ## 52056           NA       NA            NA                NA
    ## 52057           NA       NA            NA                NA
    ## 52058           NA       NA            NA                NA
    ## 52371           NA       NA            NA                NA
    ## 52372           NA       NA            NA                NA
    ## 52680           NA       NA            NA                NA
    ## 52681           NA       NA            NA                NA
    ## 52682           NA       NA            NA                NA
    ## 52683           NA       NA            NA                NA
    ## 52684           NA       NA            NA                NA
    ## 52981           NA       NA            NA                NA
    ## 53738           NA       NA            NA                NA
    ## 54024           NA       NA            NA                NA
    ## 54025           NA       NA            NA                NA
    ## 54621           NA       NA            NA                NA
    ## 54622           NA       NA            NA                NA
    ## 54623           NA       NA            NA                NA
    ## 54624           NA       NA            NA                NA
    ## 54906           NA       NA            NA                NA
    ## 54907           NA       NA            NA                NA
    ## 54908           NA       NA            NA                NA
    ## 55207           NA       NA            NA                NA
    ## 55208           NA       NA            NA                NA
    ## 55209           NA       NA            NA                NA
    ## 55210           NA       NA            NA                NA
    ## 55511           NA       NA            NA                NA
    ## 55512           NA       NA            NA                NA
    ## 55513           NA       NA            NA                NA
    ## 55769           NA       NA            NA                NA
    ## 56049           NA       NA            NA                NA
    ## 56050           NA       NA            NA                NA
    ## 56051           NA       NA            NA                NA
    ## 56052           NA       NA            NA                NA
    ## 56053           NA       NA            NA                NA
    ## 56054           NA       NA            NA                NA
    ## 56353           NA       NA            NA                NA
    ## 56354           NA       NA            NA                NA
    ## 57559           NA       NA            NA                NA
    ## 57560           NA       NA            NA                NA
    ## 57813           NA       NA            NA                NA
    ## 57814           NA       NA            NA                NA
    ## 57815           NA       NA            NA                NA
    ## 58095           NA       NA            NA                NA
    ## 58096           NA       NA            NA                NA
    ## 58097           NA       NA            NA                NA
    ## 58391           NA       NA            NA                NA
    ## 58392           NA       NA            NA                NA
    ## 58393           NA       NA            NA                NA
    ## 58685           NA       NA            NA                NA
    ## 58686           NA       NA            NA                NA
    ## 58687           NA       NA            NA                NA
    ## 58688           NA       NA            NA                NA
    ## 58689           NA       NA            NA                NA
    ## 58979           NA       NA            NA                NA
    ## 58980           NA       NA            NA                NA
    ## 58981           NA       NA            NA                NA
    ## 58982           NA       NA            NA                NA
    ## 58983           NA       NA            NA                NA
    ## 58984           NA       NA            NA                NA
    ## 58985           NA       NA            NA                NA
    ## 59276           NA       NA            NA                NA
    ## 59277           NA       NA            NA                NA
    ## 59278           NA       NA            NA                NA
    ## 59279           NA       NA            NA                NA
    ## 59280           NA       NA            NA                NA
    ## 59281           NA       NA            NA                NA
    ## 59282           NA       NA            NA                NA
    ## 59283           NA       NA            NA                NA
    ## 59579           NA       NA            NA                NA
    ## 59580           NA       NA            NA                NA
    ## 59581           NA       NA            NA                NA
    ## 59582           NA       NA            NA                NA
    ## 59832           NA       NA            NA                NA
    ## 59833           NA       NA            NA                NA
    ## 59834           NA       NA            NA                NA
    ## 59835           NA       NA            NA                NA
    ## 60112           NA       NA            NA                NA
    ## 60113           NA       NA            NA                NA
    ## 60114           NA       NA            NA                NA
    ## 60115           NA       NA            NA                NA
    ## 60116           NA       NA            NA                NA
    ## 60117           NA       NA            NA                NA
    ## 60413           NA       NA            NA                NA
    ## 60708           NA       NA            NA                NA
    ## 60709           NA       NA            NA                NA
    ## 60999           NA       NA            NA                NA
    ## 61000           NA       NA            NA                NA
    ## 61001           NA       NA            NA                NA
    ## 61002           NA       NA            NA                NA
    ## 61003           NA       NA            NA                NA
    ## 61004           NA       NA            NA                NA
    ## 61005           NA       NA            NA                NA
    ## 61301           NA       NA            NA                NA
    ## 61302           NA       NA            NA                NA
    ## 61303           NA       NA            NA                NA
    ## 61599           NA       NA            NA                NA
    ## 61842           NA       NA            NA                NA
    ## 61843           NA       NA            NA                NA
    ## 61844           NA       NA            NA                NA
    ## 61845           NA       NA            NA                NA
    ## 61846           NA       NA            NA                NA
    ## 61847           NA       NA            NA                NA
    ## 61848           NA       NA            NA                NA
    ## 62125           NA       NA            NA                NA
    ## 62126           NA       NA            NA                NA
    ## 62413           NA       NA            NA                NA
    ## 62414           NA       NA            NA                NA
    ## 62415           NA       NA            NA                NA
    ## 62416           NA       NA            NA                NA
    ## 62417           NA       NA            NA                NA
    ## 62418           NA       NA            NA                NA
    ## 62707           NA       NA            NA                NA
    ## 62708           NA       NA            NA                NA
    ## 62709           NA       NA            NA                NA
    ## 62710           NA       NA            NA                NA
    ## 63000           NA       NA            NA                NA
    ## 63001           NA       NA            NA                NA
    ## 63002           NA       NA            NA                NA
    ## 63294           NA       NA            NA                NA
    ## 63295           NA       NA            NA                NA
    ## 63296           NA       NA            NA                NA
    ## 63589           NA       NA            NA                NA
    ## 63590           NA       NA            NA                NA
    ## 64113           NA       NA            NA                NA
    ## 64114           NA       NA            NA                NA
    ## 64115           NA       NA            NA                NA
    ## 64116           NA       NA            NA                NA
    ## 64117           NA       NA            NA                NA
    ## 64404           NA       NA            NA                NA
    ## 64405           NA       NA            NA                NA
    ## 64406           NA       NA            NA                NA
    ## 64407           NA       NA            NA                NA
    ## 64408           NA       NA            NA                NA
    ## 64409           NA       NA            NA                NA
    ## 64698           NA       NA            NA                NA
    ## 64699           NA       NA            NA                NA
    ## 64700           NA       NA            NA                NA
    ## 64701           NA       NA            NA                NA
    ## 64990           NA       NA            NA                NA
    ## 64991           NA       NA            NA                NA
    ## 65282           NA       NA            NA                NA
    ## 65283           NA       NA            NA                NA
    ## 65284           NA       NA            NA                NA
    ## 65285           NA       NA            NA                NA
    ## 65571           NA       NA            NA                NA
    ## 65572           NA       NA            NA                NA
    ## 65573           NA       NA            NA                NA
    ## 65574           NA       NA            NA                NA
    ## 65575           NA       NA            NA                NA
    ## 65576           NA       NA            NA                NA
    ## 65577           NA       NA            NA                NA
    ## 65578           NA       NA            NA                NA
    ## 65579           NA       NA            NA                NA
    ## 65580           NA       NA            NA                NA
    ## 65581           NA       NA            NA                NA
    ## 65582           NA       NA            NA                NA
    ## 65821           NA       NA            NA                NA
    ## 66084           NA       NA            NA                NA
    ## 66085           NA       NA            NA                NA
    ## 66664           NA       NA            NA                NA
    ## 66665           NA       NA            NA                NA
    ## 66951           NA       NA            NA                NA
    ## 66952           NA       NA            NA                NA
    ## 66953           NA       NA            NA                NA
    ## 67246           NA       NA            NA                NA
    ## 67247           NA       NA            NA                NA
    ## 67541           NA       NA            NA                NA
    ## 67542           NA       NA            NA                NA
    ## 67767           NA       NA            NA                NA
    ## 67768           NA       NA            NA                NA
    ## 68034           NA       NA            NA                NA
    ## 68316           NA       NA            NA                NA
    ## 68317           NA       NA            NA                NA
    ## 68590           NA       NA            NA                NA
    ## 68591           NA       NA            NA                NA
    ## 68592           NA       NA            NA                NA
    ## 68593           NA       NA            NA                NA
    ## 68594           NA       NA            NA                NA
    ## 68595           NA       NA            NA                NA
    ## 68874           NA       NA            NA                NA
    ## 68875           NA       NA            NA                NA
    ## 69159           NA       NA            NA                NA
    ## 69160           NA       NA            NA                NA
    ## 69161           NA       NA            NA                NA
    ## 69446           NA       NA            NA                NA
    ## 69447           NA       NA            NA                NA
    ## 69448           NA       NA            NA                NA
    ## 69854           NA       NA            NA                NA
    ## 69855           NA       NA            NA                NA
    ## 69856           NA       NA            NA                NA
    ## 70120           NA       NA            NA                NA
    ## 70121           NA       NA            NA                NA
    ## 70122           NA       NA            NA                NA
    ## 70123           NA       NA            NA                NA
    ## 71169           NA       NA            NA                NA
    ## 71170           NA       NA            NA                NA
    ## 71171           NA       NA            NA                NA
    ## 71361           NA       NA            NA                NA
    ## 71362           NA       NA            NA                NA
    ## 71607           NA       NA            NA                NA
    ## 71868           NA       NA            NA                NA
    ## 71869           NA       NA            NA                NA
    ## 72123           NA       NA            NA                NA
    ## 72873           NA       NA            NA                NA
    ## 72874           NA       NA            NA                NA
    ## 72875           NA       NA            NA                NA
    ## 72876           NA       NA            NA                NA
    ## 72877           NA       NA            NA                NA
    ## 72878           NA       NA            NA                NA
    ## 72879           NA       NA            NA                NA
    ## 72880           NA       NA            NA                NA
    ## 72881           NA       NA            NA                NA
    ## 72882           NA       NA            NA                NA
    ## 72883           NA       NA            NA                NA
    ## 72884           NA       NA            NA                NA
    ## 72885           NA       NA            NA                NA
    ## 72886           NA       NA            NA                NA
    ## 72887           NA       NA            NA                NA
    ## 72888           NA       NA            NA                NA
    ## 72889           NA       NA            NA                NA
    ## 72890           NA       NA            NA                NA
    ## 72891           NA       NA            NA                NA
    ## 72892           NA       NA            NA                NA
    ## 72893           NA       NA            NA                NA
    ## 72894           NA       NA            NA                NA
    ## 72895           NA       NA            NA                NA
    ## 72896           NA       NA            NA                NA
    ## 72897           NA       NA            NA                NA
    ## 72898           NA       NA            NA                NA
    ## 72899           NA       NA            NA                NA
    ## 72900           NA       NA            NA                NA
    ## 72901           NA       NA            NA                NA
    ## 73050           NA       NA            NA                NA
    ## 73051           NA       NA            NA                NA
    ## 73052           NA       NA            NA                NA
    ## 73053           NA       NA            NA                NA
    ## 73054           NA       NA            NA                NA
    ## 73055           NA       NA            NA                NA
    ## 73056           NA       NA            NA                NA
    ## 73057           NA       NA            NA                NA
    ## 73058           NA       NA            NA                NA
    ## 73059           NA       NA            NA                NA
    ## 73060           NA       NA            NA                NA
    ## 73061           NA       NA            NA                NA
    ## 73062           NA       NA            NA                NA
    ## 73063           NA       NA            NA                NA
    ## 73064           NA       NA            NA                NA
    ## 73065           NA       NA            NA                NA
    ## 73066           NA       NA            NA                NA
    ## 73067           NA       NA            NA                NA
    ## 73068           NA       NA            NA                NA
    ## 73069           NA       NA            NA                NA
    ## 73070           NA       NA            NA                NA
    ## 73071           NA       NA            NA                NA
    ## 73072           NA       NA            NA                NA
    ## 73073           NA       NA            NA                NA
    ## 73074           NA       NA            NA                NA
    ## 73075           NA       NA            NA                NA
    ## 73076           NA       NA            NA                NA
    ## 73077           NA       NA            NA                NA
    ## 73078           NA       NA            NA                NA
    ## 73079           NA       NA            NA                NA
    ## 73080           NA       NA            NA                NA
    ## 73081           NA       NA            NA                NA
    ## 73082           NA       NA            NA                NA
    ## 73083           NA       NA            NA                NA
    ## 73084           NA       NA            NA                NA
    ## 73085           NA       NA            NA                NA
    ## 73086           NA       NA            NA                NA
    ## 73087           NA       NA            NA                NA
    ## 73088           NA       NA            NA                NA
    ## 73089           NA       NA            NA                NA
    ## 73090           NA       NA            NA                NA
    ## 73091           NA       NA            NA                NA
    ## 73092           NA       NA            NA                NA
    ## 73307           NA       NA            NA                NA
    ## 73308           NA       NA            NA                NA
    ## 73309           NA       NA            NA                NA
    ## 73310           NA       NA            NA                NA
    ## 73311           NA       NA            NA                NA
    ## 73312           NA       NA            NA                NA
    ## 73313           NA       NA            NA                NA
    ## 73314           NA       NA            NA                NA
    ## 73315           NA       NA            NA                NA
    ## 73316           NA       NA            NA                NA
    ## 73317           NA       NA            NA                NA
    ## 73318           NA       NA            NA                NA
    ## 73319           NA       NA            NA                NA
    ## 73320           NA       NA            NA                NA
    ## 73321           NA       NA            NA                NA
    ## 73322           NA       NA            NA                NA
    ## 73323           NA       NA            NA                NA
    ## 73324           NA       NA            NA                NA
    ## 73325           NA       NA            NA                NA
    ## 73326           NA       NA            NA                NA
    ## 73327           NA       NA            NA                NA
    ## 73328           NA       NA            NA                NA
    ## 73329           NA       NA            NA                NA
    ## 73330           NA       NA            NA                NA
    ## 73331           NA       NA            NA                NA
    ## 73332           NA       NA            NA                NA
    ## 73333           NA       NA            NA                NA
    ## 73334           NA       NA            NA                NA
    ## 73335           NA       NA            NA                NA
    ## 73336           NA       NA            NA                NA
    ## 73337           NA       NA            NA                NA
    ## 73584           NA       NA            NA                NA
    ## 73585           NA       NA            NA                NA
    ## 73586           NA       NA            NA                NA
    ## 73587           NA       NA            NA                NA
    ## 73588           NA       NA            NA                NA
    ## 73589           NA       NA            NA                NA
    ## 73590           NA       NA            NA                NA
    ## 73591           NA       NA            NA                NA
    ## 73592           NA       NA            NA                NA
    ## 73593           NA       NA            NA                NA
    ## 73594           NA       NA            NA                NA
    ## 73595           NA       NA            NA                NA
    ## 73596           NA       NA            NA                NA
    ## 73597           NA       NA            NA                NA
    ## 73598           NA       NA            NA                NA
    ## 73599           NA       NA            NA                NA
    ## 73848           NA       NA            NA                NA
    ## 73849           NA       NA            NA                NA
    ## 73850           NA       NA            NA                NA
    ## 73851           NA       NA            NA                NA
    ## 73852           NA       NA            NA                NA
    ## 73853           NA       NA            NA                NA
    ## 74105           NA       NA            NA                NA
    ## 74106           NA       NA            NA                NA
    ## 74107           NA       NA            NA                NA
    ## 74108           NA       NA            NA                NA
    ## 74109           NA       NA            NA                NA
    ## 74110           NA       NA            NA                NA
    ## 74367           NA       NA            NA                NA
    ## 74368           NA       NA            NA                NA
    ## 74369           NA       NA            NA                NA
    ## 74370           NA       NA            NA                NA
    ## 74371           NA       NA            NA                NA
    ## 74372           NA       NA            NA                NA
    ## 74634           NA       NA            NA                NA
    ## 74635           NA       NA            NA                NA
    ## 76365           NA       NA            NA                NA
    ## 76366           NA       NA            NA                NA
    ## 77574           NA       NA            NA                NA
    ## 77925           NA       NA            NA                NA
    ## 78095           NA       NA            NA                NA
    ## 78096           NA       NA            NA                NA
    ## 78097           NA       NA            NA                NA
    ## 79046           NA       NA            NA                NA
    ## 79047           NA       NA            NA                NA
    ## 79300           NA       NA            NA                NA
    ## 79301           NA       NA            NA                NA
    ## 79562           NA       NA            NA                NA
    ## 79563           NA       NA            NA                NA
    ## 79923           NA       NA            NA                NA
    ## 80013           NA       NA            NA                NA
    ## 80772           NA       NA            NA                NA
    ## 80773           NA       NA            NA                NA
    ## 81474           NA       NA            NA                NA
    ## 82245           NA       NA            NA                NA
    ## 82496           NA       NA            NA                NA
    ## 82746           NA       NA            NA                NA
    ## 82747           NA       NA            NA                NA
    ## 82748           NA       NA            NA                NA
    ## 82749           NA       NA            NA                NA
    ## 83010           NA       NA            NA                NA
    ## 83011           NA       NA            NA                NA
    ## 83463           NA       NA            NA                NA
    ## 83707           NA       NA            NA                NA
    ## 84219           NA       NA            NA                NA
    ## 84220           NA       NA            NA                NA
    ## 84221           NA       NA            NA                NA
    ## 84734           NA       NA            NA                NA
    ## 85913           NA       NA            NA                NA
    ## 86411           NA       NA            NA                NA
    ## 87334           NA       NA            NA                NA
    ## 87335           NA       NA            NA                NA
    ## 87336           NA       NA            NA                NA
    ## 87337           NA       NA            NA                NA
    ## 87580           NA       NA            NA                NA
    ## 87581           NA       NA            NA                NA
    ## 87582           NA       NA            NA                NA
    ## 87583           NA       NA            NA                NA
    ## 87828           NA       NA            NA                NA
    ## 87829           NA       NA            NA                NA
    ## 88751           NA       NA            NA                NA
    ## 88752           NA       NA            NA                NA
    ## 89493           NA       NA            NA                NA
    ## 89494           NA       NA            NA                NA
    ## 89495           NA       NA            NA                NA
    ## 89496           NA       NA            NA                NA
    ## 89748           NA       NA            NA                NA
    ## 91182           NA       NA            NA                NA
    ## 91762           NA       NA            NA                NA
    ## 91763           NA       NA            NA                NA
    ## 93003           NA       NA            NA                NA
    ## 93251           NA       NA            NA                NA
    ## 94150           NA       NA            NA                NA
    ## 94151           NA       NA            NA                NA
    ## 94152           NA       NA            NA                NA
    ## 94153           NA       NA            NA                NA
    ## 94154           NA       NA            NA                NA
    ## 94155           NA       NA            NA                NA
    ## 94388           NA       NA            NA                NA
    ## 94392           NA       NA            NA                NA
    ## 94393           NA       NA            NA                NA
    ## 94394           NA       NA            NA                NA
    ## 94395           NA       NA            NA                NA
    ## 94396           NA       NA            NA                NA
    ## 94397           NA       NA            NA                NA
    ## 94644           NA       NA            NA                NA
    ## 94645           NA       NA            NA                NA
    ## 94893           NA       NA            NA                NA
    ## 94894           NA       NA            NA                NA
    ## 95200           NA       NA            NA                NA
    ## 95308           NA       NA            NA                NA
    ## 95309           NA       NA            NA                NA
    ## 95310           NA       NA            NA                NA
    ## 95471           NA       NA            NA                NA
    ## 95556           NA       NA            NA                NA
    ## 95557           NA       NA            NA                NA
    ## 95558           NA       NA            NA                NA
    ## 95559           NA       NA            NA                NA
    ## 95560           NA       NA            NA                NA
    ## 95796           NA       NA            NA                NA
    ## 95797           NA       NA            NA                NA
    ## 95798           NA       NA            NA                NA
    ## 95799           NA       NA            NA                NA
    ## 95800           NA       NA            NA                NA
    ## 95801           NA       NA            NA                NA
    ## 95802           NA       NA            NA                NA
    ## 95803           NA       NA            NA                NA
    ## 95804           NA       NA            NA                NA
    ## 95805           NA       NA            NA                NA
    ## 95806           NA       NA            NA                NA
    ## 96051           NA       NA            NA                NA
    ## 96052           NA       NA            NA                NA
    ## 96301           NA       NA            NA                NA
    ## 96302           NA       NA            NA                NA
    ## 96303           NA       NA            NA                NA
    ## 96304           NA       NA            NA                NA
    ## 96549           NA       NA            NA                NA
    ## 96550           NA       NA            NA                NA
    ## 96551           NA       NA            NA                NA
    ## 96552           NA       NA            NA                NA
    ## 96553           NA       NA            NA                NA
    ## 96554           NA       NA            NA                NA
    ## 96555           NA       NA            NA                NA
    ## 96556           NA       NA            NA                NA
    ## 96745           NA       NA            NA                NA
    ## 96746           NA       NA            NA                NA
    ## 96747           NA       NA            NA                NA
    ## 96748           NA       NA            NA                NA
    ## 96973           NA       NA            NA                NA
    ## 96974           NA       NA            NA                NA
    ## 96975           NA       NA            NA                NA
    ## 96976           NA       NA            NA                NA
    ## 96977           NA       NA            NA                NA
    ## 96978           NA       NA            NA                NA
    ## 97227           NA       NA            NA                NA
    ## 97228           NA       NA            NA                NA
    ## 97472           NA       NA            NA                NA
    ## 97473           NA       NA            NA                NA
    ## 97474           NA       NA            NA                NA
    ## 97475           NA       NA            NA                NA
    ## 97476           NA       NA            NA                NA
    ## 97678           NA       NA            NA                NA
    ## 97679           NA       NA            NA                NA
    ## 98125           NA       NA            NA                NA
    ## 98126           NA       NA            NA                NA
    ## 98127           NA       NA            NA                NA
    ## 98128           NA       NA            NA                NA
    ## 98330           NA       NA            NA                NA
    ## 98331           NA       NA            NA                NA
    ## 98332           NA       NA            NA                NA
    ## 98556           NA       NA            NA                NA
    ## 98804           NA       NA            NA                NA
    ## 99048           NA       NA            NA                NA
    ## 99049           NA       NA            NA                NA
    ## 99050           NA       NA            NA                NA
    ## 99259           NA       NA            NA                NA
    ## 99260           NA       NA            NA                NA

``` r
table1 = xtabs(~Cancelled + CancellationCode, data = cancel)

a1 = cancel %>%
  group_by(CancellationCode) %>%
  summarize(count = sum(Cancelled))
a1
```

    ## # A tibble: 3 x 2
    ##   CancellationCode count
    ##   <fct>            <int>
    ## 1 A                  719
    ## 2 B                  605
    ## 3 C                   96

``` r
ggbarplot(a1, x = 'CancellationCode', y = 'count',
   fill = 'CancellationCode', color = 'CancellationCode', palette = "jco", label = round(a1$count)) 
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
# ggplot(data = a1) +
#   geom_bar(mapping = aes(x=CancellationCode, y=count), stat='identity')
```

There are a totle of 1420 cancellations. 719 of them are type A cancellations which is cancellations related to the unique carriers,. 605 of them are cancellations due to wheater (Type B), and 96 of them are NAS(Type C) which are delays or cancellations within the control of National Airspace System.

``` r
b1 = cancel %>%
  group_by(DayOfWeek) %>%
  summarize(total = sum(Cancelled))
b1
```

    ## # A tibble: 7 x 2
    ##   DayOfWeek total
    ##       <int> <int>
    ## 1         1   183
    ## 2         2   289
    ## 3         3   224
    ## 4         4   218
    ## 5         5   199
    ## 6         6   151
    ## 7         7   156

``` r
ggbarplot(b1, x = 'DayOfWeek', y = 'total',
   fill ='DayOfWeek', color = 'DayOfWeek', label = round(b1$total))
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-16-1.png) Next we plotted cancellations by days of week and found that cancellations mostly happen on Tuesdays.

``` r
e1 = cancel %>%
  group_by(Origin) %>%
  summarize(count = sum(Cancelled))

subset(e1, Origin == 'AUS')
```

    ## # A tibble: 1 x 2
    ##   Origin count
    ##   <fct>  <int>
    ## 1 AUS      732

``` r
c1 = cancel %>%
  group_by(Origin, CancellationCode) %>%
  summarize(count = sum(Cancelled))
c1
```

    ## # A tibble: 71 x 3
    ## # Groups:   Origin [37]
    ##    Origin CancellationCode count
    ##    <fct>  <fct>            <int>
    ##  1 ABQ    A                    4
    ##  2 ABQ    B                    2
    ##  3 ATL    A                   17
    ##  4 ATL    B                   15
    ##  5 ATL    C                    2
    ##  6 AUS    A                  390
    ##  7 AUS    B                  294
    ##  8 AUS    C                   48
    ##  9 BOS    A                    1
    ## 10 BOS    B                    1
    ## # ... with 61 more rows

``` r
Austin = subset(c1, Origin == 'AUS')
Austin
```

    ## # A tibble: 3 x 3
    ## # Groups:   Origin [1]
    ##   Origin CancellationCode count
    ##   <fct>  <fct>            <int>
    ## 1 AUS    A                  390
    ## 2 AUS    B                  294
    ## 3 AUS    C                   48

``` r
ggdotchart(Austin, x = "CancellationCode", y = "count",
           color = "CancellationCode", add = 'segment', dot.size = 6, label = round(Austin$count),
           rotate = TRUE, title = 'Austin Airport Flight Cancellations')    
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-19-1.png)

Since we are looking at datasets in the Austin Airport. We are also interested in looking at the amount of flights that got cancelled departing from the Austin Airport. The result shows that Austin Airport has a total of 732 cancellations and the most common cancellation type is Type A which is controlled by carrier.

``` r
a = sort(table(flight$Origin), decreasing = TRUE)
b =  sort(table(cancel$Origin), decreasing = TRUE)
```

``` r
all_air = data.frame(a)
cancel_air = data.frame(b)
merged = merge(all_air, cancel_air, by = "Var1")
merged$percentdec = merged$Freq.y/merged$Freq.x

merged$percent = (merged$percentdec)*100


merged1 = subset(merged, Freq.y != 0)
merged1
```

    ##    Var1 Freq.x Freq.y   percentdec    percent
    ## 1   ABQ    433      6 0.0138568129 1.38568129
    ## 2   ATL   2255     34 0.0150776053 1.50776053
    ## 3   AUS  49623    732 0.0147512242 1.47512242
    ## 6   BOS    368      2 0.0054347826 0.54347826
    ## 8   CLE    380      5 0.0131578947 1.31578947
    ## 9   CLT    660      2 0.0030303030 0.30303030
    ## 10  CVG    653      3 0.0045941807 0.45941807
    ## 11  DAL   5583    115 0.0205982447 2.05982447
    ## 12  DEN   2719      7 0.0025744759 0.25744759
    ## 13  DFW   5508    159 0.0288671024 2.88671024
    ## 14  ELP   1344      1 0.0007440476 0.07440476
    ## 15  EWR    939      9 0.0095846645 0.95846645
    ## 16  FLL    481      1 0.0020790021 0.20790021
    ## 17  HOU   2310     39 0.0168831169 1.68831169
    ## 18  HRL    366      7 0.0191256831 1.91256831
    ## 19  IAD    631     10 0.0158478605 1.58478605
    ## 20  IAH   3704     51 0.0137688985 1.37688985
    ## 21  IND    218      3 0.0137614679 1.37614679
    ## 22  JAX    229      2 0.0087336245 0.87336245
    ## 23  JFK   1356     33 0.0243362832 2.43362832
    ## 24  LAS   1232      3 0.0024350649 0.24350649
    ## 25  LAX   1732     12 0.0069284065 0.69284065
    ## 27  LGB    245      2 0.0081632653 0.81632653
    ## 28  MAF    471      2 0.0042462845 0.42462845
    ## 29  MCI    459     12 0.0261437908 2.61437908
    ## 31  MDW    713      4 0.0056100982 0.56100982
    ## 32  MEM    835     19 0.0227544910 2.27544910
    ## 34  MSY    443      2 0.0045146727 0.45146727
    ## 38  ORD   2515     90 0.0357852883 3.57852883
    ## 40  PHX   2786      7 0.0025125628 0.25125628
    ## 41  RDU    231      2 0.0086580087 0.86580087
    ## 44  SEA    147      2 0.0136054422 1.36054422
    ## 45  SFO    609      4 0.0065681445 0.65681445
    ## 46  SJC    968     30 0.0309917355 3.09917355
    ## 47  SLC    550      1 0.0018181818 0.18181818
    ## 49  STL     95      6 0.0631578947 6.31578947
    ## 51  TUL     90      1 0.0111111111 1.11111111

``` r
merged1 = merged1[order(merged1$percent),]
merged1$Var1=factor(merged1$Var1,levels=merged1$Var1)
ggplot(data = merged1) +
  geom_bar(mapping = aes(x=factor(Var1), y=percent), stat='identity', ascending=FALSE) +
  coord_flip()
```

    ## Warning: Ignoring unknown parameters: ascending

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-22-1.png)

Finally, we are interested in looking at how Austin Airport's cancellations is compare to other airports. We normalized the number of cancellations into percentage of cancellation out of all flights in each airport, and we graphed the result. In turns out that St. Louis Airport is most frequent in canceling its flights with 6.32% cancelled flights out of all flights. Austin is actually ranked number 12th with 1.48% cancelled flights out of all flights.

Please clear your Environment before running the next problem. May have repeated variable names\# \#\#\#\#\#Portfolio Modeling\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

Please also see the report for more details of the portfolio, in (.pdf) format attached in the same folder!

``` r
knitr::opts_chunk$set(echo = TRUE)
library(mosaic)
library(quantmod)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

``` r
library(foreach)
```

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

### Develop 3 different portfolios

First one: "Invest in China" ! not diversified, focus on 3 equities ETF and 1 money with RMB
============================================================================================

CQQQ: Invesco China Technology ETF
==================================

CYB: WisdomTree Chinese Yuan Fund
=================================

EWH:iShares MSCI Hong Kong ETF
==============================

\#CXSE: WisdomTree China ex-State-Owned Enterprises Fund
--------------------------------------------------------

### Extract data and adjust

``` r
# Import a few stocks
myETF = c("CQQQ", "CYB", "EWH", "CXSE")
myprices = getSymbols(myETF, from = "2014-08-13") #five years ago
```

    ## 'getSymbols' currently uses auto.assign=TRUE by default, but will
    ## use auto.assign=FALSE in 0.5-0. You will still be able to use
    ## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
    ## and getOption("getSymbols.auto.assign") will still be checked for
    ## alternate defaults.
    ## 
    ## This message is shown once per session and may be disabled by setting 
    ## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.

    ## Warning: CYB contains missing values. Some functions will not work if
    ## objects contain missing values in the middle of the series. Consider using
    ## na.omit(), na.approx(), na.fill(), etc to remove or replace them.

``` r
# A chunk of code for adjusting all stocks
for(ticker in myETF) {
    expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
    eval(parse(text=expr))
}
# Combine all the returns in a matrix
all_returns = cbind(    ClCl(CQQQa),
                                ClCl(CYBa),
                                ClCl(EWHa),
                                ClCl(CXSEa))
all_returns = as.matrix(na.omit(all_returns))
head(all_returns)
```

    ##              ClCl.CQQQa     ClCl.CYBa    ClCl.EWHa   ClCl.CXSEa
    ## 2014-08-14 -0.005689087  0.0007914919  0.002262443 -0.008080081
    ## 2014-08-15  0.000260026  0.0011862000 -0.005417562 -0.004072941
    ## 2014-08-18  0.011440536  0.0019747630  0.003177440  0.011201938
    ## 2014-08-19  0.006940771 -0.0007883721  0.015837149  0.004571866
    ## 2014-08-20 -0.005616467 -0.0007889546  0.009799510 -0.005426238
    ## 2014-08-21 -0.009756123 -0.0007896171 -0.013233304 -0.007039810

``` r
# Compute the returns from the closing prices
```

### Doing the simulation

``` r
# Run simulation 5000 times 
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
    total_wealth = initial_wealth
    n_days = 20
    wealthtracker = rep(0, n_days) #initialize a list of 0
    for(today in 1:n_days) {
      weights = c(0.25, 0.25, 0.25, 0.25) #4 ETF in total
        holdings = weights * total_wealth
        
        return.today = resample(all_returns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        total_wealth = sum(holdings)
        wealthtracker[today] = total_wealth
    }
    wealthtracker
}
# Profit/loss
hist(sim1[,n_days]- initial_wealth, breaks=30, main = "Invest In China Portfolio GAIN&LOSS", xlab = "US dollar", col = "red")
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-25-1.png) \#\#\#Compute VaR with 5% cut.

``` r
quantile((sim1[,n_days]- initial_wealth), probs = c(0.05, 0.95))
```

    ##        5%       95% 
    ## -6812.402  7752.283

<table>
<colgroup>
<col width="100%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="left">#Portfolio Number 2:</td>
</tr>
<tr class="even">
<td align="left">VNG: Vanguard Real Estate Index Fund USO: United States Oil Fund SHV: iShares Short Treasury Bond ETF VCSH: Vanguard Short-Term Corporate Bond ETF JPST: JPMorgan Ultra-Short Income ETF SCZ: iShares MSCI EAFE Small-Cap ETF XLV: Health Care Select Sector SPDR Fund GLD: SPDR Gold Trust</td>
</tr>
<tr class="odd">
<td align="left">run similar simulation as previous portfolio</td>
</tr>
<tr class="even">
<td align="left"><code>r myETF = c(&quot;VNQ&quot;, &quot;USO&quot;, &quot;SHV&quot;, &quot;VCSH&quot;,&quot;JPST&quot;, &quot;SCZ&quot;, &quot;XLV&quot;, &quot;GLD&quot;) myprices = getSymbols(myETF, from = &quot;2014-08-13&quot;)</code></td>
</tr>
<tr class="odd">
<td align="left"><code>## pausing 1 second between requests for more than 5 symbols ## pausing 1 second between requests for more than 5 symbols ## pausing 1 second between requests for more than 5 symbols ## pausing 1 second between requests for more than 5 symbols</code></td>
</tr>
<tr class="even">
<td align="left"><code>r for(ticker in myETF) { expr = paste0(ticker, &quot;a = adjustOHLC(&quot;, ticker, &quot;)&quot;) eval(parse(text=expr)) } all_returns = cbind(    ClCl(VNQ), ClCl(USO), ClCl(SHV), ClCl(VCSH), ClCl(JPST), ClCl(SCZ), ClCl(XLV), ClCl(GLD)) all_returns = as.matrix(na.omit(all_returns)) head(all_returns)</code></td>
</tr>
<tr class="odd">
<td align="left"><code>##                ClCl.VNQ     ClCl.USO      ClCl.SHV     ClCl.VCSH ## 2017-06-27 -0.008967575  0.019036954  0.000000e+00 -0.0011233649 ## 2017-06-28  0.002381284  0.010989011  9.061650e-05  0.0009996501 ## 2017-06-29 -0.011402767  0.001086957  9.067174e-05 -0.0008737985 ## 2017-06-30  0.000000000  0.031487514  9.066352e-05  0.0000000000 ## 2017-07-03  0.012735768  0.012631579 -6.344602e-04 -0.0021239130 ## 2017-07-05 -0.014117962 -0.038461538  0.000000e+00 -0.0006260548 ##               ClCl.JPST     ClCl.SCZ     ClCl.XLV      ClCl.GLD ## 2017-06-27 0.0000000000 -0.001557363 -0.009080669  0.0038019347 ## 2017-06-28 0.0000000000  0.005372565  0.005272382  0.0007575457 ## 2017-06-29 0.0000000000 -0.008102000 -0.009115934 -0.0048780655 ## 2017-06-30 0.0001999201  0.005213747 -0.001386263 -0.0025355223 ## 2017-07-03 0.0001998002 -0.007434302  0.001766772 -0.0163531694 ## 2017-07-05 0.0000000000  0.002612803  0.005920900  0.0037040315</code></td>
</tr>
<tr class="even">
<td align="left"><code>r initial_wealth = 100000 sim1 = foreach(i=1:5000, .combine='rbind') %do% { total_wealth = initial_wealth n_days = 20 wealthtracker = rep(0, n_days) for(today in 1:n_days) { weights = c(0.125, 0.125, 0.125, 0.125,0.125, 0.125, 0.125, 0.125) #8 ETF total holdings = weights * total_wealth return.today = resample(all_returns, 1, orig.ids=FALSE) holdings = holdings + holdings*return.today total_wealth = sum(holdings) wealthtracker[today] = total_wealth } wealthtracker } # Profit/loss hist(sim1[,n_days]- initial_wealth, breaks=30, main = &quot;Diversified Portfolio GAIN&amp;LOSS&quot;, xlab = &quot;US dollar&quot;, col = &quot;blue&quot;)</code></td>
</tr>
<tr class="odd">
<td align="left"><img src="FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-28-1.png" /> ###Compute VaR with 5% cut.</td>
</tr>
<tr class="even">
<td align="left"><code>r quantile((sim1[,n_days]- initial_wealth), probs = c(0.05, 0.95))</code></td>
</tr>
<tr class="odd">
<td align="left"><code>##        5%       95% ## -2659.815  3345.696</code></td>
</tr>
</tbody>
</table>

Portfolio Number 3:
===================

XWEB: SPDR S&P Internet ETF IBUY: Amplify Online Retail ETF JSMD: Janus Henderson Small/Md Cp Gr Alpha ETF IFLY: ETFMG Drone Economy Strategy ETF

``` r
myETF = c("XWEB", "IBUY", "JSMD", "IFLY")
myprices = getSymbols(myETF, from = "2014-08-13")
for(ticker in myETF) {
    expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
    eval(parse(text=expr))
}
all_returns = cbind(    ClCl(XWEB),
                                ClCl(IBUY),
                                ClCl(JSMD),
                                ClCl(IFLY))
all_returns = as.matrix(na.omit(all_returns))

initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
    total_wealth = initial_wealth
    n_days = 20
    wealthtracker = rep(0, n_days) 
    for(today in 1:n_days) {
      weights = c(0.25, 0.25, 0.25, 0.25)
        holdings = weights * total_wealth
        return.today = resample(all_returns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        total_wealth = sum(holdings)
        wealthtracker[today] = total_wealth
    }
    wealthtracker
}
# Profit/loss
hist(sim1[,n_days]- initial_wealth, breaks=30, main = "Invest In Online/Growing Industries", xlab = "US dollar", col = "green")
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
quantile((sim1[,n_days]- initial_wealth), probs = c(0.05, 0.95))
```

    ##        5%       95% 
    ## -6109.559  8760.465

Please clear your Environment before running the next problem. May have repeated variable names\# \#\#\#\#\#Market Segmentation\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(foreach)
library(mvtnorm)
library(LICORS)
library(readr)
library(ggplot2)
library(ggthemes)
social = read.csv("social_marketing.csv", row.names=1)
```

``` r
sm_clean = select(social, -matches("uncategorized"))
sm_cleaned = subset(sm_clean, adult < 5)
```

The Median number of adult tags is 5, so we decide to take out data points containing more than 5 adult tweets per week. also we found out for spam, it ranges from 0-2 with mean around 0.3, so we conclude that the filter has done a pretty good job already. We not gonna touch the spam column or drop anything. Becasue uncategorized tags don't help in segmentation, we drop that whole column.

``` r
set.seed(1)
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(sm_cleaned, k, nstart=50)
  cluster_k$tot.withinss
}
```

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

``` r
plot(k_grid, SSE_grid)
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-34-1.png) After seeing the results from k-means, we decide we will pick k = 7

``` r
k = 7
cluster_k = kmeans(sm_cleaned, k, nstart=50)
```

K-means clustering with 7 clusters of sizes 917, 583, 475, 3533, 544, 376, 1164

``` r
for(i in 1:k) { 
  index_temp = which(cluster_k$cluster == i)
  df_temp = sm_cleaned[c(index_temp), ]
  assign(paste("ddat",i,sep="_"),df_temp) 
  assign(paste("index",i,sep="_"),index_temp) 
} 
```

Now we have 7 different df for each cluster

### A quick try on Hie-clustering

``` r
sm_distance_matrix = dist(social, method='euclidean')
hmin = hclust(sm_distance_matrix, method='complete')
cluster3 = cutree(hmin, k=10)
summary(factor(cluster3))
```

    ##    1    2    3    4    5    6    7    8    9   10 
    ##  347 6155  489   70  110  420  218   31   22   20

We have tried method "complete" and "single", also k = 5-10. The results have been disappointing because there's always one big group that takes over majority of data points. We are showing the best one we've got. So we decide to stick with results from K-means for now.

<table>
<colgroup>
<col width="100%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="left">###PCA We need to label each data with a group number (7 different segmentations!) for later The group assignments are based on results from k means</td>
</tr>
<tr class="even">
<td align="left">```r sm_cleaned['group'] = NA</td>
</tr>
<tr class="odd">
<td align="left">sm_cleaned<span class="math inline"><em>g</em><em>r</em><em>o</em><em>u</em><em>p</em>[<em>i</em><em>n</em><em>d</em><em>e</em><em>x</em><sub>1</sub>]=′<em>g</em><em>r</em><em>o</em><em>u</em><em>p</em>1′<em>s</em><em>m</em><sub><em>c</em></sub><em>l</em><em>e</em><em>a</em><em>n</em><em>e</em><em>d</em></span>group[index_2] = 'group2' sm_cleaned<span class="math inline"><em>g</em><em>r</em><em>o</em><em>u</em><em>p</em>[<em>i</em><em>n</em><em>d</em><em>e</em><em>x</em><sub>3</sub>]=′<em>g</em><em>r</em><em>o</em><em>u</em><em>p</em>3′<em>s</em><em>m</em><sub><em>c</em></sub><em>l</em><em>e</em><em>a</em><em>n</em><em>e</em><em>d</em></span>group[index_4] = 'group4' sm_cleaned<span class="math inline"><em>g</em><em>r</em><em>o</em><em>u</em><em>p</em>[<em>i</em><em>n</em><em>d</em><em>e</em><em>x</em><sub>5</sub>]=′<em>g</em><em>r</em><em>o</em><em>u</em><em>p</em>5′<em>s</em><em>m</em><sub><em>c</em></sub><em>l</em><em>e</em><em>a</em><em>n</em><em>e</em><em>d</em></span>group[index_6] = 'group6' sm_cleaned$group[index_7] = 'group7' ```</td>
</tr>
<tr class="even">
<td align="left"><code>r z = sm_cleaned[,1:35] pc1 = prcomp(z, scale.=TRUE)</code></td>
</tr>
<tr class="odd">
<td align="left"><code>r loadings = pc1$rotation scores = pc1$x qplot(scores[,1], scores[,2], color = sm_cleaned$group, xlab = 'Component 1', ylab = 'Component 2') +scale_color_colorblind()</code></td>
</tr>
<tr class="even">
<td align="left"><img src="FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-40-1.png" /></td>
</tr>
<tr class="odd">
<td align="left">It's obvious that there are some distinct groups such as 3, 5 and 6. Let's do some head &amp; tail to analyze what component 1 &amp; 2 represents.</td>
</tr>
<tr class="even">
<td align="left"><code>r o1 = order(loadings[,1], decreasing = TRUE) colnames(z)[head(o1,10)]</code></td>
</tr>
<tr class="odd">
<td align="left"><code>##  [1] &quot;religion&quot;      &quot;food&quot;          &quot;parenting&quot;     &quot;sports_fandom&quot; ##  [5] &quot;school&quot;        &quot;family&quot;        &quot;beauty&quot;        &quot;crafts&quot; ##  [9] &quot;cooking&quot;       &quot;fashion&quot;</code></td>
</tr>
<tr class="even">
<td align="left"><code>r colnames(z)[tail(o1,10)]</code></td>
</tr>
<tr class="odd">
<td align="left"><code>##  [1] &quot;health_nutrition&quot; &quot;home_and_garden&quot;  &quot;dating&quot; ##  [4] &quot;current_events&quot;   &quot;art&quot;              &quot;tv_film&quot; ##  [7] &quot;college_uni&quot;      &quot;online_gaming&quot;    &quot;adult&quot; ## [10] &quot;spam&quot;</code></td>
</tr>
<tr class="even">
<td align="left"><code>r colnames(z)[o1[20:25]]</code></td>
</tr>
<tr class="odd">
<td align="left"><code>## [1] &quot;shopping&quot;       &quot;sports_playing&quot; &quot;chatter&quot;        &quot;music&quot; ## [5] &quot;small_business&quot; &quot;travel&quot;</code></td>
</tr>
<tr class="even">
<td align="left"><code>r o2 = order(loadings[,2], decreasing = TRUE) colnames(z)[head(o2,10)]</code></td>
</tr>
<tr class="odd">
<td align="left"><code>##  [1] &quot;religion&quot;      &quot;sports_fandom&quot; &quot;parenting&quot;     &quot;food&quot; ##  [5] &quot;school&quot;        &quot;family&quot;        &quot;news&quot;          &quot;automotive&quot; ##  [9] &quot;adult&quot;         &quot;crafts&quot;</code></td>
</tr>
<tr class="even">
<td align="left"><code>r colnames(z)[tail(o2,10)]</code></td>
</tr>
<tr class="odd">
<td align="left"><code>##  [1] &quot;outdoors&quot;         &quot;health_nutrition&quot; &quot;personal_fitness&quot; ##  [4] &quot;music&quot;            &quot;chatter&quot;          &quot;beauty&quot; ##  [7] &quot;shopping&quot;         &quot;fashion&quot;          &quot;cooking&quot; ## [10] &quot;photo_sharing&quot;</code></td>
</tr>
<tr class="even">
<td align="left"><code>r colnames(z)[o2[20:25]]</code></td>
</tr>
<tr class="odd">
<td align="left"><code>## [1] &quot;online_gaming&quot;  &quot;eco&quot;            &quot;small_business&quot; &quot;sports_playing&quot; ## [5] &quot;business&quot;       &quot;college_uni&quot;</code></td>
</tr>
<tr class="even">
<td align="left">Seperate them into individual plots to see lcearly</td>
</tr>
<tr class="odd">
<td align="left">```r sm.group = sm_cleaned$group</td>
</tr>
<tr class="even">
<td align="left">qplot(scores[,1], scores[,2], facets=~sm.group, xlab = 'Component 1', ylab = 'Component 2') ```</td>
</tr>
<tr class="odd">
<td align="left"><img src="FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-42-1.png" /> Group3 is closer to head of PC1 and PC2 than any other groups. &quot;religion&quot; &quot;food&quot; &quot;parenting&quot; &quot;sports_fandom&quot; &quot;school&quot; &quot;family&quot; &quot;beauty&quot; &quot;crafts&quot; &quot;cooking&quot; &quot;fashion&quot;</td>
</tr>
<tr class="even">
<td align="left">&quot;religion&quot; &quot;sports_fandom&quot; &quot;parenting&quot; &quot;food&quot; &quot;school&quot; &quot;family&quot; &quot;news&quot; &quot;automotive&quot; &quot;adult&quot; &quot;crafts&quot;</td>
</tr>
<tr class="odd">
<td align="left">We conclude that this group should be more family oriented, older adults who's already married.</td>
</tr>
</tbody>
</table>

Group6 is close to PC1 tail and PC2 middle: "dating" "current\_events" "art" "tv\_film" "college\_uni" "online\_gaming" "adult" "spam" "computers" "travel" "tv\_film" "dating" "current\_events" "online\_gaming"

We conclude this group is close to college students, young single adults.
-------------------------------------------------------------------------

Group 5 is close to tail of PC 2: "health\_nutrition" "personal\_fitness" "music" "chatter" "beauty" "shopping" "fashion" "cooking" "photo\_sharing"

We conclude this group as working woman, or our "mum" group
-----------------------------------------------------------

Other clusters that were kinda in the middles, pretty similar:

They do contain some unique features that stood out, such as "business","small business", and "news" that were not as common as other groups. So we identify these groups as various working class, hard workers, or entrepreneur.

Please clear your Environment before running the next problem. May have repeated variable names\# \#\#\#\#\#Author Attribution\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
library(tm) 
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

    ## 
    ## Attaching package: 'tm'

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     inspect

``` r
library(magrittr)
library(dplyr)
library(slam)
library(proxy)
```

    ## 
    ## Attaching package: 'proxy'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     as.matrix

    ## The following objects are masked from 'package:stats':
    ## 
    ##     as.dist, dist

    ## The following object is masked from 'package:base':
    ## 
    ##     as.matrix

``` r
library(stringr)
library(tidyverse)


readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
```

### EXTRACTING THE DATA

``` r
a_fold= list.dirs(path = "C:/Users/Cleme/Documents/GitHub/STA380/data/ReutersC50/C50train", full.names = TRUE, recursive = TRUE)
a_fold = as.list(a_fold)
a_fold = a_fold[-1]


str2 = '/*.txt'
str1  = 'C:/Users/Cleme/Documents/GitHub/STA380/data/ReutersC50/C50train/'
for (i in seq_along(a_fold)) {
  tempstring = strsplit((as.character(a_fold[i])),"/" )
  tempstring = tempstring[[1]][10]
  stringtemp = paste(str1, tempstring,str2, sep = '')
  temp = Sys.glob(stringtemp)
  assign(paste("txtfile", i , sep= "_"), temp)
}
#extracting all the files from each folder in the training set
```

### cleaning all the documents inside each author folder and creating a DTM for each document

stopwords were removed and a TFIDF matrix was created and then converted to a dataframe to be used in a classification model.

``` r
for (j in 1:50){

x = eval(parse(text=paste("txtfile", j , sep= "_")))

aaron = lapply(x, readerPlain)
mynames = x %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist
 names(aaron) = mynames

documents_raw = Corpus(VectorSource(aaron))
my_documents = documents_raw
my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space

my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))
DTM_temp = DocumentTermMatrix(my_documents)
DTM_temp = removeSparseTerms(DTM_temp, 0.95)

tfidf_temp = weightTfIdf(DTM_temp)
X = as.matrix(tfidf_temp)

df = as.data.frame(X)
assign(paste("df", j , sep= "_"), df)
}
```

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

``` r
#cleaning the words and then calculating the TFIDF for each term in the documents for the authors
#assigning this into a dataframe
```

``` r
library(tidyverse)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following object is masked from 'package:slam':
    ## 
    ##     rollup

    ## The following objects are masked from 'package:xts':
    ## 
    ##     first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
name_list = list()

for (i in seq(1:50)) {
  x_df = eval(parse(text=paste("df", i , sep= "_")))

  
  tempstring = strsplit((as.character(a_fold[i])),"/" )
  tempstring = tempstring[[1]][10]

  setDF(x_df)[]
  y_df = x_df

  y_df[,"author_name"] = tempstring

  name_list[[i]] = y_df
  assign(paste("df", i , "t", sep= "_"), y_df)
  
}
```

``` r
library(plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:ggpubr':
    ## 
    ##     mutate

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     count

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
a_w_list = rbind.fill(name_list)

a_w_list = select(a_w_list, -cusersclemedocumentsgithubstadatareuterscctrainalancrosbynewsmltxt)
#cleaning up wrong extraction
a_w_list[is.na(a_w_list)] <- 0
```

### TESTSET

``` r
a_fold2= list.dirs(path = "C:/Users/Cleme/Documents/GitHub/STA380/data/ReutersC50/C50test", full.names = TRUE, recursive = TRUE)
a_fold2 = as.list(a_fold2)
a_fold2 = a_fold2[-1]
str2 = '/*.txt'
str1a  = 'C:/Users/Cleme/Documents/GitHub/STA380/data/ReutersC50/C50test/'
for (i in seq_along(a_fold)) {
  tempstring = strsplit((as.character(a_fold2[i])),"/" )
  tempstring = tempstring[[1]][10]
  stringtemp = paste(str1a, tempstring,str2, sep = '')
  temp = Sys.glob(stringtemp)
  assign(paste("t_file", i , sep= "_"), temp)
}

for (j in 1:50){

x = eval(parse(text=paste("t_file", j , sep= "_")))

aaron = lapply(x, readerPlain)
mynames = x %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist
 names(aaron) = mynames

documents_raw = Corpus(VectorSource(aaron))
my_documents = documents_raw
my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space

my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))
DTM_temp = DocumentTermMatrix(my_documents)
DTM_temp = removeSparseTerms(DTM_temp, 0.95)

tfidf_temp = weightTfIdf(DTM_temp)
X = as.matrix(tfidf_temp)


df = as.data.frame(X)
assign(paste("df_test", j , sep= "_"), df)
}
```

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents, content_transformer(tolower)):
    ## transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeNumbers)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removePunctuation)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(stripWhitespace)): transformation drops documents

    ## Warning in tm_map.SimpleCorpus(my_documents,
    ## content_transformer(removeWords), : transformation drops documents

``` r
name_list2 = list()

for (i in seq(1:50)) {
  x_df = eval(parse(text=paste("df_test", i , sep= "_")))

  
  tempstring = strsplit((as.character(a_fold[i])),"/" )
  tempstring = tempstring[[1]][10]

  setDF(x_df)[]
  y_df = x_df
  y_df[,"author_name"] = tempstring
  name_list2[[i]] = y_df
  assign(paste("df_test", i , "t", sep= "_"), y_df)
  
}

a_w_listtest = rbind.fill(name_list2)
a_w_listtest[is.na(a_w_listtest)] <- 0
a_w_listtest = select(a_w_listtest, -cusersclemedocumentsgithubstadatareuterscctestaaronpressmannewsmltxt)
```

### KNN WITH COSINE DISTANCE

``` r
library(class)

a_w_listtrain = select(a_w_list, -author_name)
a_w_listtest2 = select(a_w_listtest, -author_name)
#creating pseudo words
for (i in seq(1:59)){
pseud_count <- 1
a_w_listtest2[ , paste0("psued_word", i)] <-pseud_count
#pseudoword for all discrepancy between test/train
}
dim(a_w_listtest2)
```

    ## [1] 2500 9979

``` r
dim(a_w_listtrain)
```

    ## [1] 2500 9979

``` r
cosine_dist_mat = proxy::dist(as.matrix(a_w_listtrain), method='cosine')
cosine_dist_mat2 = proxy::dist(as.matrix(a_w_listtest2), method='cosine')
#creating cosine distance matrix for training and testing 

model1<- knn(cosine_dist_mat, cosine_dist_mat2, a_w_list$author_name, k=70)
mean(a_w_list$author_name== model1)
```

    ## [1] 0.02

``` r
#KNN run on the cosine distance matrices; returns an accuracy of around 2%
```

Please clear your Environment before running the next problem. May have repeated variable names\# \#\#\#\#\#Association Rule Mining\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
```

    ## 
    ## Attaching package: 'arules'

    ## The following object is masked from 'package:tm':
    ## 
    ##     inspect

    ## The following objects are masked from 'package:mosaic':
    ## 
    ##     inspect, lhs, rhs

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(arulesViz)
```

    ## Loading required package: grid

    ## Registered S3 methods overwritten by 'registry':
    ##   method               from 
    ##   print.registry_field proxy
    ##   print.registry_entry proxy

    ## Registered S3 method overwritten by 'seriation':
    ##   method         from 
    ##   reorder.hclust gclus

Read in the groceries transactions This is in "long" format -- every row is a basket with multiple items per row separated by commas. We separated the items in each basket by commas

``` r
groceries_raw = read.transactions("groceries.txt", sep=',')
```

Checking if our items are separated correctly

``` r
inspect(head(groceries_raw, 2))
```

    ##     items                
    ## [1] {citrus fruit,       
    ##      margarine,          
    ##      ready soups,        
    ##      semi-finished bread}
    ## [2] {coffee,             
    ##      tropical fruit,     
    ##      yogurt}

``` r
LIST(head(groceries_raw, 3))
```

    ## [[1]]
    ## [1] "citrus fruit"        "margarine"           "ready soups"        
    ## [4] "semi-finished bread"
    ## 
    ## [[2]]
    ## [1] "coffee"         "tropical fruit" "yogurt"        
    ## 
    ## [[3]]
    ## [1] "whole milk"

Checking if our items are separated correctly

``` r
inspect(head(groceries_raw, 2))
```

    ##     items                
    ## [1] {citrus fruit,       
    ##      margarine,          
    ##      ready soups,        
    ##      semi-finished bread}
    ## [2] {coffee,             
    ##      tropical fruit,     
    ##      yogurt}

``` r
LIST(head(groceries_raw, 3))
```

    ## [[1]]
    ## [1] "citrus fruit"        "margarine"           "ready soups"        
    ## [4] "semi-finished bread"
    ## 
    ## [[2]]
    ## [1] "coffee"         "tropical fruit" "yogurt"        
    ## 
    ## [[3]]
    ## [1] "whole milk"

``` r
str(groceries_raw)
```

    ## Formal class 'transactions' [package "arules"] with 3 slots
    ##   ..@ data       :Formal class 'ngCMatrix' [package "Matrix"] with 5 slots
    ##   .. .. ..@ i       : int [1:43367] 29 88 118 132 33 157 167 166 38 91 ...
    ##   .. .. ..@ p       : int [1:9836] 0 4 7 8 12 16 21 22 27 28 ...
    ##   .. .. ..@ Dim     : int [1:2] 169 9835
    ##   .. .. ..@ Dimnames:List of 2
    ##   .. .. .. ..$ : NULL
    ##   .. .. .. ..$ : NULL
    ##   .. .. ..@ factors : list()
    ##   ..@ itemInfo   :'data.frame':  169 obs. of  1 variable:
    ##   .. ..$ labels: chr [1:169] "abrasive cleaner" "artif. sweetener" "baby cosmetics" "baby food" ...
    ##   ..@ itemsetInfo:'data.frame':  0 obs. of  0 variables

``` r
summary(groceries_raw)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

Counting the frequencies of each of the grocery items and plotting the top 20 items that appear most frequently.

``` r
frequent = eclat(groceries_raw, parameter = list(supp = 0.07, maxlen = 15))
```

    ## Eclat
    ## 
    ## parameter specification:
    ##  tidLists support minlen maxlen            target   ext
    ##     FALSE    0.07      1     15 frequent itemsets FALSE
    ## 
    ## algorithmic control:
    ##  sparse sort verbose
    ##       7   -2    TRUE
    ## 
    ## Absolute minimum support count: 688 
    ## 
    ## create itemset ... 
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [18 item(s)] done [0.01s].
    ## creating sparse bit matrix ... [18 row(s), 9835 column(s)] done [0.00s].
    ## writing  ... [19 set(s)] done [0.00s].
    ## Creating S4 object  ... done [0.00s].

``` r
inspect(frequent)
```

    ##      items                         support    count
    ## [1]  {other vegetables,whole milk} 0.07483477  736 
    ## [2]  {whole milk}                  0.25551601 2513 
    ## [3]  {other vegetables}            0.19349263 1903 
    ## [4]  {rolls/buns}                  0.18393493 1809 
    ## [5]  {yogurt}                      0.13950178 1372 
    ## [6]  {soda}                        0.17437722 1715 
    ## [7]  {root vegetables}             0.10899847 1072 
    ## [8]  {tropical fruit}              0.10493137 1032 
    ## [9]  {bottled water}               0.11052364 1087 
    ## [10] {sausage}                     0.09395018  924 
    ## [11] {shopping bags}               0.09852567  969 
    ## [12] {citrus fruit}                0.08276563  814 
    ## [13] {pastry}                      0.08896797  875 
    ## [14] {pip fruit}                   0.07564820  744 
    ## [15] {whipped/sour cream}          0.07168277  705 
    ## [16] {fruit/vegetable juice}       0.07229283  711 
    ## [17] {newspapers}                  0.07981698  785 
    ## [18] {bottled beer}                0.08052872  792 
    ## [19] {canned beer}                 0.07768175  764

``` r
itemFrequencyPlot(groceries_raw, topN=20, type="absolute", main="Item Frequency")
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-55-1.png)

Creating a list of baskets: vectors of items by consumer Analagous to bags of words

Cast this variable as a special arules "transactions" class.

``` r
grotrans = as(groceries_raw, "transactions")
summary(grotrans)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

Running the 'apriori' algorithm Looking at rules with support &gt; .005 & confidence &gt;.1 & length (\# artists) &lt;= 5

``` r
shoppingrules = apriori(grotrans, 
                     parameter=list(support=.005, confidence=.1, maxlen=5))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.1    0.1    1 none FALSE            TRUE       5   0.005      1
    ##  maxlen target   ext
    ##       5  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 49 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [120 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.02s].
    ## writing ... [1582 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

Looking at the output

``` r
#inspect(shoppingrules)
```

Choosing a subset For our thresholds for lift and confidence, we chose confidence to be greater than 0.5 and lift to be greater than 3. When we initially inspected when lift &gt; 4, only four connections were returned. These returns had very low confidence ranging from 0.12 to 0.44, although the lifts were high. This means that these connections should not be correct most of the time. When we inspected for confidence &gt; 0.6, 22 connections were returned. These returns had lifts that were below 3, which means that there is a greater chance of the connections being a coincidence compared to when lift is greater than 3. Thus, when we inspected for our threshold, 8 connections that were high in both confidence and lift were returned. These connections are therefore not just a confidence and they are correct most of the time.

Our choice of threshold is also supported by the Gephi diagram (using Force Atlas). The grocery items that have the most connections are found from the connections returned by applying the threshold.

The results from the table returned and the gephi diagram(uploaded as another file on github) makes sense. For example, it is reasonable to claim that customers purchasing onions and root vegetables also purchase other vegetables. Also, it makes sense for customers purchasing curd and tropical fruits to purchase yogurt (a combination of ingredients people usually have).

``` r
inspect(subset(shoppingrules, subset=lift > 4))
```

    ##     lhs                   rhs                      support confidence     lift count
    ## [1] {ham}              => {white bread}        0.005083884  0.1953125 4.639851    50
    ## [2] {white bread}      => {ham}                0.005083884  0.1207729 4.639851    50
    ## [3] {butter,                                                                        
    ##      other vegetables} => {whipped/sour cream} 0.005795628  0.2893401 4.036397    57
    ## [4] {citrus fruit,                                                                  
    ##      other vegetables,                                                              
    ##      whole milk}       => {root vegetables}    0.005795628  0.4453125 4.085493    57

``` r
inspect(subset(shoppingrules, subset=confidence > 0.6))
```

    ##      lhs                        rhs                    support confidence     lift count
    ## [1]  {onions,                                                                           
    ##       root vegetables}       => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [2]  {curd,                                                                             
    ##       tropical fruit}        => {whole milk}       0.006507372  0.6336634 2.479936    64
    ## [3]  {domestic eggs,                                                                    
    ##       margarine}             => {whole milk}       0.005185562  0.6219512 2.434099    51
    ## [4]  {butter,                                                                           
    ##       domestic eggs}         => {whole milk}       0.005998983  0.6210526 2.430582    59
    ## [5]  {butter,                                                                           
    ##       whipped/sour cream}    => {whole milk}       0.006710727  0.6600000 2.583008    66
    ## [6]  {bottled water,                                                                    
    ##       butter}                => {whole milk}       0.005388917  0.6022727 2.357084    53
    ## [7]  {butter,                                                                           
    ##       tropical fruit}        => {whole milk}       0.006202339  0.6224490 2.436047    61
    ## [8]  {butter,                                                                           
    ##       root vegetables}       => {whole milk}       0.008235892  0.6377953 2.496107    81
    ## [9]  {butter,                                                                           
    ##       yogurt}                => {whole milk}       0.009354347  0.6388889 2.500387    92
    ## [10] {domestic eggs,                                                                    
    ##       pip fruit}             => {whole milk}       0.005388917  0.6235294 2.440275    53
    ## [11] {domestic eggs,                                                                    
    ##       tropical fruit}        => {whole milk}       0.006914082  0.6071429 2.376144    68
    ## [12] {pip fruit,                                                                        
    ##       whipped/sour cream}    => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [13] {pip fruit,                                                                        
    ##       whipped/sour cream}    => {whole milk}       0.005998983  0.6483516 2.537421    59
    ## [14] {fruit/vegetable juice,                                                            
    ##       other vegetables,                                                                 
    ##       yogurt}                => {whole milk}       0.005083884  0.6172840 2.415833    50
    ## [15] {other vegetables,                                                                 
    ##       root vegetables,                                                                  
    ##       whipped/sour cream}    => {whole milk}       0.005185562  0.6071429 2.376144    51
    ## [16] {other vegetables,                                                                 
    ##       pip fruit,                                                                        
    ##       root vegetables}       => {whole milk}       0.005490595  0.6750000 2.641713    54
    ## [17] {pip fruit,                                                                        
    ##       root vegetables,                                                                  
    ##       whole milk}            => {other vegetables} 0.005490595  0.6136364 3.171368    54
    ## [18] {other vegetables,                                                                 
    ##       pip fruit,                                                                        
    ##       yogurt}                => {whole milk}       0.005083884  0.6250000 2.446031    50
    ## [19] {citrus fruit,                                                                     
    ##       root vegetables,                                                                  
    ##       whole milk}            => {other vegetables} 0.005795628  0.6333333 3.273165    57
    ## [20] {root vegetables,                                                                  
    ##       tropical fruit,                                                                   
    ##       yogurt}                => {whole milk}       0.005693950  0.7000000 2.739554    56
    ## [21] {other vegetables,                                                                 
    ##       tropical fruit,                                                                   
    ##       yogurt}                => {whole milk}       0.007625826  0.6198347 2.425816    75
    ## [22] {other vegetables,                                                                 
    ##       root vegetables,                                                                  
    ##       yogurt}                => {whole milk}       0.007829181  0.6062992 2.372842    77

``` r
inspect(subset(shoppingrules, subset=lift > 3 & confidence > 0.5))
```

    ##     lhs                     rhs                    support confidence     lift count
    ## [1] {onions,                                                                        
    ##      root vegetables}    => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [2] {curd,                                                                          
    ##      tropical fruit}     => {yogurt}           0.005287239  0.5148515 3.690645    52
    ## [3] {pip fruit,                                                                     
    ##      whipped/sour cream} => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [4] {citrus fruit,                                                                  
    ##      root vegetables}    => {other vegetables} 0.010371124  0.5862069 3.029608   102
    ## [5] {root vegetables,                                                               
    ##      tropical fruit}     => {other vegetables} 0.012302999  0.5845411 3.020999   121
    ## [6] {pip fruit,                                                                     
    ##      root vegetables,                                                               
    ##      whole milk}         => {other vegetables} 0.005490595  0.6136364 3.171368    54
    ## [7] {citrus fruit,                                                                  
    ##      root vegetables,                                                               
    ##      whole milk}         => {other vegetables} 0.005795628  0.6333333 3.273165    57
    ## [8] {root vegetables,                                                               
    ##      tropical fruit,                                                                
    ##      whole milk}         => {other vegetables} 0.007015760  0.5847458 3.022057    69

ploting all the rules in the (support, confidence) space Higher lift rules tend to have lower support

``` r
plot(shoppingrules)
```

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-60-1.png)

Swapping the axes and color scales

``` r
plot(shoppingrules, measure = c("support", "lift"), shading = "confidence")
```

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-61-1.png)

"two key" plot: coloring is by size (order) of item set

``` r
plot(shoppingrules, method='two-key plot')
```

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-62-1.png)

looking at subsets driven by the plot

``` r
inspect(subset(shoppingrules, support > 0.035))
```

    ##      lhs                   rhs                support    confidence
    ## [1]  {}                 => {bottled water}    0.11052364 0.1105236 
    ## [2]  {}                 => {tropical fruit}   0.10493137 0.1049314 
    ## [3]  {}                 => {root vegetables}  0.10899847 0.1089985 
    ## [4]  {}                 => {soda}             0.17437722 0.1743772 
    ## [5]  {}                 => {yogurt}           0.13950178 0.1395018 
    ## [6]  {}                 => {rolls/buns}       0.18393493 0.1839349 
    ## [7]  {}                 => {other vegetables} 0.19349263 0.1934926 
    ## [8]  {}                 => {whole milk}       0.25551601 0.2555160 
    ## [9]  {tropical fruit}   => {other vegetables} 0.03589222 0.3420543 
    ## [10] {other vegetables} => {tropical fruit}   0.03589222 0.1854966 
    ## [11] {tropical fruit}   => {whole milk}       0.04229792 0.4031008 
    ## [12] {whole milk}       => {tropical fruit}   0.04229792 0.1655392 
    ## [13] {root vegetables}  => {other vegetables} 0.04738180 0.4347015 
    ## [14] {other vegetables} => {root vegetables}  0.04738180 0.2448765 
    ## [15] {root vegetables}  => {whole milk}       0.04890696 0.4486940 
    ## [16] {whole milk}       => {root vegetables}  0.04890696 0.1914047 
    ## [17] {soda}             => {rolls/buns}       0.03833249 0.2198251 
    ## [18] {rolls/buns}       => {soda}             0.03833249 0.2084024 
    ## [19] {soda}             => {whole milk}       0.04006101 0.2297376 
    ## [20] {whole milk}       => {soda}             0.04006101 0.1567847 
    ## [21] {yogurt}           => {other vegetables} 0.04341637 0.3112245 
    ## [22] {other vegetables} => {yogurt}           0.04341637 0.2243826 
    ## [23] {yogurt}           => {whole milk}       0.05602440 0.4016035 
    ## [24] {whole milk}       => {yogurt}           0.05602440 0.2192598 
    ## [25] {rolls/buns}       => {other vegetables} 0.04260295 0.2316197 
    ## [26] {other vegetables} => {rolls/buns}       0.04260295 0.2201787 
    ## [27] {rolls/buns}       => {whole milk}       0.05663447 0.3079049 
    ## [28] {whole milk}       => {rolls/buns}       0.05663447 0.2216474 
    ## [29] {other vegetables} => {whole milk}       0.07483477 0.3867578 
    ## [30] {whole milk}       => {other vegetables} 0.07483477 0.2928770 
    ##      lift      count
    ## [1]  1.0000000 1087 
    ## [2]  1.0000000 1032 
    ## [3]  1.0000000 1072 
    ## [4]  1.0000000 1715 
    ## [5]  1.0000000 1372 
    ## [6]  1.0000000 1809 
    ## [7]  1.0000000 1903 
    ## [8]  1.0000000 2513 
    ## [9]  1.7677896  353 
    ## [10] 1.7677896  353 
    ## [11] 1.5775950  416 
    ## [12] 1.5775950  416 
    ## [13] 2.2466049  466 
    ## [14] 2.2466049  466 
    ## [15] 1.7560310  481 
    ## [16] 1.7560310  481 
    ## [17] 1.1951242  377 
    ## [18] 1.1951242  377 
    ## [19] 0.8991124  394 
    ## [20] 0.8991124  394 
    ## [21] 1.6084566  427 
    ## [22] 1.6084566  427 
    ## [23] 1.5717351  551 
    ## [24] 1.5717351  551 
    ## [25] 1.1970465  419 
    ## [26] 1.1970465  419 
    ## [27] 1.2050318  557 
    ## [28] 1.2050318  557 
    ## [29] 1.5136341  736 
    ## [30] 1.5136341  736

``` r
inspect(subset(shoppingrules, confidence > 0.5 & lift > 3))
```

    ##     lhs                     rhs                    support confidence     lift count
    ## [1] {onions,                                                                        
    ##      root vegetables}    => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [2] {curd,                                                                          
    ##      tropical fruit}     => {yogurt}           0.005287239  0.5148515 3.690645    52
    ## [3] {pip fruit,                                                                     
    ##      whipped/sour cream} => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [4] {citrus fruit,                                                                  
    ##      root vegetables}    => {other vegetables} 0.010371124  0.5862069 3.029608   102
    ## [5] {root vegetables,                                                               
    ##      tropical fruit}     => {other vegetables} 0.012302999  0.5845411 3.020999   121
    ## [6] {pip fruit,                                                                     
    ##      root vegetables,                                                               
    ##      whole milk}         => {other vegetables} 0.005490595  0.6136364 3.171368    54
    ## [7] {citrus fruit,                                                                  
    ##      root vegetables,                                                               
    ##      whole milk}         => {other vegetables} 0.005795628  0.6333333 3.273165    57
    ## [8] {root vegetables,                                                               
    ##      tropical fruit,                                                                
    ##      whole milk}         => {other vegetables} 0.007015760  0.5847458 3.022057    69

graph-based visualization For the visualization, we used the threshold that we chose earlier (confidence &gt; 0.5 & lift &gt; 3). After plotting the subset, we are able to clearly see strong connections between the grocery items. We tried using a confidence that was higher, which did not show as many interesting and insightful connections.

``` r
sub1 = subset(shoppingrules, subset=confidence > 0.5 & lift > 3)
summary(sub1)
```

    ## set of 8 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ## 3 4 
    ## 5 3 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.000   3.000   3.000   3.375   4.000   4.000 
    ## 
    ## summary of quality measures:
    ##     support           confidence          lift           count       
    ##  Min.   :0.005287   Min.   :0.5149   Min.   :3.021   Min.   : 52.00  
    ##  1st Qu.:0.005567   1st Qu.:0.5847   1st Qu.:3.028   1st Qu.: 54.75  
    ##  Median :0.005745   Median :0.5942   Median :3.118   Median : 56.50  
    ##  Mean   :0.007194   Mean   :0.5905   Mean   :3.180   Mean   : 70.75  
    ##  3rd Qu.:0.007855   3rd Qu.:0.6067   3rd Qu.:3.197   3rd Qu.: 77.25  
    ##  Max.   :0.012303   Max.   :0.6333   Max.   :3.691   Max.   :121.00  
    ## 
    ## mining info:
    ##      data ntransactions support confidence
    ##  grotrans          9835   0.005        0.1

``` r
plot(sub1, method='graph')
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-64-1.png)

``` r
?plot.rules
```

    ## starting httpd help server ... done

``` r
plot(head(sub1, 100, by='lift'), method='graph')
```

![](FinalReport_Markdown_files/figure-markdown_github/unnamed-chunk-65-1.png)

export \#There is a file on github showing the connections after applying 'Force Alas' called 'Gephi\_ForceAtlas.png'

``` r
saveAsGraph(head(shoppingrules, n = 1000, by = "lift"), file = "shoppingrules.graphml")
```
