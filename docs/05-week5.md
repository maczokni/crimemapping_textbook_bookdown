
# Week 5 : Studying spatial point patterns


## What we'll do today

We have now covered quite a bit. You've learnt about spatial objects and various formats in which they come and are stored by R, how to produce maps using a variety of packages, and also provided you with a brief introduction to common spatial operations. In what remains of the semester we are going to shift the emphasis and start focusing a bit more on spatial statistics. First we will focus on techniques that are used to explore and analyse points in a geographical space and in subsequent sessions we will cover techniques that are used to analyse spatial data when our unit of analysis are polygons (e.g., postal code areas, census areas, police beats, etc).

We will introduce a new R package called **spatstat**, that was developed for spatial point pattern analysis and modelling. It was written by Adrian Baddeley and Rolf Turner. There is a [webpage](http://spatstat.org) dedicated to this package. The [thickest book](https://www.crcpress.com/Spatial-Point-Patterns-Methodology-and-Applications-with-R/Baddeley-Rubak-Turner/p/book/9781482210200) in my library, at 810 pages, is dedicated to this package. So as you can imagine the theory and practice of spatial pattern analysis is something one could devote an entire course to. You can get a pdf document used in a course the authors of this package develop [here](https://research.csiro.au/software/wp-content/uploads/sites/6/2015/02/Rspatialcourse_CMIS_PDF-Standard.pdf). In our course we are only going to provide you with an introductory practical entry into this field of techniques. If this package is not installed in your machine, make sure you install it before we carry on.


```r
library(sf)
library(tmap)
library(dplyr)
library(spatstat)
```

## Getting the data

We will using the crime data from Greater Manchester police we have been using so far. Let's focus on burglary in the Fallowfield area. The code below has already been explained and used in previous sessions, so we won't go over the detail again. But rather than cut and paste automatically, try to remember what each line of code is doing.

By the way, the police data for Manchester we have used in previous sessions correspond to only one month of the year. Here we are using a full year worth of data, so the data import will take a bit longer.


```r
#Read a geojson file with Manchester wards
manchester_ward <- st_read("https://raw.githubusercontent.com/RUMgroup/Spatial-data-in-R/master/rumgroup/data/wards.geojson")
```

```
## Reading layer `OGRGeoJSON' from data source `https://raw.githubusercontent.com/RUMgroup/Spatial-data-in-R/master/rumgroup/data/wards.geojson' using driver `GeoJSON'
## Simple feature collection with 215 features and 12 fields
## geometry type:  POLYGON
## dimension:      XY
## bbox:           xmin: 351664 ymin: 381168.6 xmax: 406087.5 ymax: 421039.8
## epsg (SRID):    27700
## proj4string:    +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs
```

```r
#Create a new object that only has the city centre ward
df1 <- manchester_ward %>%
  filter(wd16nm == "Fallowfield")
#Change coordinate systems
fallowfield <- st_transform(df1, 4326)
#Get rid of objects we no longer need
rm(manchester_ward)
rm(df1)

#Read Greater Manchester police data
crimes <- read.csv("https://raw.githubusercontent.com/jjmedinaariza/CrimeMapping/master/gmpcrime.csv")
burglary <- filter(crimes, crime_type == "Burglary")

#Transform the dataframe with crime information into a sf object
burglary_spatial = st_as_sf(burglary, coords = c("long", "lat"), 
                 crs = 4326, agr = "constant")

#Select only the crimes that take place within the space defined by the Ward boundaries
# intersection
bur_fal <- st_intersects(fallowfield, burglary_spatial)
```

```
## although coordinates are longitude/latitude, st_intersects assumes that they are planar
```

```r
# subsetting
bur_fal <- burglary_spatial[unlist(bur_fal),]
rm(crimes)
rm(burglary)

#Let's see the results
tm_shape(fallowfield) + 
  tm_fill() +
  tm_shape(bur_fal) +
  tm_dots()
```

<img src="05-week5_files/figure-html/unnamed-chunk-2-1.png" width="672" />

In the point pattern analysis literature each point is often referred to as an **event** and these events can have **marks**, attributes or characteristics that are also encoded in the data. In our spatial object one of these *marks* is the type of crime (altough in this case it's of little interest since we have filtered on it).

## Getting the data into spatstat: the problem with duplicates

So let's start using spatstat.The first thing we need to do is to transform our sf object into a **ppp** object which is how spatstat likes to store its point patterns. Unfortunately, spatstat and many other packages for analysis of spatial data precede sf, so the transformation is a bit awkard. Also before we do that, it is important to realise that a point pattern is defined as a series of events in a given area, or window, of observation. It is therefore extremely important to precisely define this window. In *spatstat* the function *owin()* is used to set the observation window. However, the standard function takes the coordinates of a rectangle or of a polygon from a matrix, and therefore it may be a bit tricky to use. Luckily the package *maptools* provides a way to transform a SpatialPolygons into an object of class owin, using the function as.owin 


```r
#First we transform our Falllowfield polygon into a sp object
fallowfield_sp <-as(fallowfield, "Spatial")
#Then we use the as.owin function to define the window
window <- maptools::as.owin.SpatialPolygons(fallowfield_sp)
#Check that this worked
class(window)
```

```
## [1] "owin"
```

```r
window
```

```
## window: polygonal boundary
## enclosing rectangle: [-2.2581074, -2.2141921] x [53.43904, 53.45145] units
```

Now that we have created the window as an owin object let's get the points:


```r
#First we will extract the coordinates from our sf point data into a matrix
sf_bur_fal_coords <- matrix(unlist(bur_fal$geometry), ncol = 2, byrow = T)
#Then we use the ppp function to create the object using the information from our matrix and the window that we created
bur_ppp <- ppp(x = sf_bur_fal_coords[,1], y = sf_bur_fal_coords[,2],
                   window = window, check = T)
```

```
## Warning: data contain duplicated points
```

```r
plot(bur_ppp)
```

<img src="05-week5_files/figure-html/unnamed-chunk-4-1.png" width="672" />


Notice the warning message about duplicates. In spatial point pattern analysis an issue of significance is the presence of duplicates. The statistical methodology used for spatial point pattern processes is based largely on the assumption that processes are simple, that is, that the points cannot be coincident. That assumption may be unreasonable in many contexts (for example, the literature on repeat victimisation indeed suggests that we should expect the same households to be at a higher risk of being hit again). Even so the point (no pun intended) is that "when the data has coincidence points, some statistical procedures will be severely affected. So it is always strongly advisable to check for duplicate points and to decide on a strategy for dealing with them if they are present" (Baddeley et al., 2016: 60).

We can check the duplication in a ppp object with the following syntax:


```r
any(duplicated(bur_ppp))
```

```
## [1] TRUE
```

To count the number of coincidence points we use the multiplicity() function. This will return a vector of integers, with one entry for each observation in our dataset, giving the number of points that are identical to the point in question (including itself).


```r
multiplicity(bur_ppp)
```

If you want to know how many locations have more than one event you can use:


```r
sum(multiplicity(bur_ppp) > 1)
```

```
## [1] 190
```

That's quite something. 190 points out of 223 here share coordinates.


```r
tm_shape(fallowfield) + 
  tm_fill() +
  tm_shape(bur_fal) +
  tm_dots(alpha=0.4, size=1)
```

<img src="05-week5_files/figure-html/unnamed-chunk-8-1.png" width="672" />

In the case of crime, as we have hinted some of this may be linked to the nature of crime itself. Hint: repeat victimisation. However, this pattern of duplication is fairly obvious across all crime categories in the police.uk website and, although I have not explored this in detail, I strongly suspect it is a function of the anonimisation process used to create these maps. The coordinates provided in the open data are not the exact locations of crimes, but they come from a list of points generated for purposes of data publication. You can see the details [here](https://data.police.uk/about/#anonymisation). This process is likely inflating the amount of duplication we observe. So keep in mind when analysing and working with this data that it is not the same as working with the real locations.

What to do about duplicates in spatial point pattern analysis is not always clear. You could simply delete the duplicates, but of course that may ignore issues such as repeat victimisation. You could also use jittering, which will add a small perturbation to the duplicate points so that they do not occupy the exact same space. Which again, may ignore things like repeat victimisation. Another alternative is to make each point "unique" and then attach the multiplicites of the points to the patterns as *marks*, as attributes of the points. Then you would need analytical techniques that take into account these marks.

If you were to be doing this for real you would want access to the real thing, not this public version of the data and then go for the second solution suggested above. We don't have access to the source data, so for the sake of simplicity and so that we can illustrate how spatstat works we will add some jittering to the data. The first argument for the function is the object, retry asks whether we want the algorithm to have another go if the jittering places a point outside the window (we want this so that we don't loose points), and the drop argument is used to ensure we get a ppp object as a result of running this function.


```r
jitter_bur <- rjitter(bur_ppp, retry=TRUE, nsim=1, drop=TRUE)
plot(jitter_bur)
```

<img src="05-week5_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Notice the difference with the original plot. Can you see how the circumferences do not overlap perfectly now?

## Inspecting our data with spatstat

This package supports all kind of exploratory point pattern analysis. One example of this is quadrant counting. One could divide the window of observation into quadrants and count the number of points into each of these quadrants. For example, if we want four quadrants along the X axis and 3 along the Y axis we could used those parameters in the quadratcount() function. Then we just use standard plotting functions from R base.


```r
Q <- quadratcount(jitter_bur, nx = 4, ny = 3)
plot(jitter_bur)
plot(Q, add = TRUE, cex = 2)
```

<img src="05-week5_files/figure-html/unnamed-chunk-10-1.png" width="672" />

In the video lectures by Luc Anselin would were exposed to the notion of complete spatial randomness (CSR). When we look at a point pattern process the first step in the process is whether it has been generated in a random manner. Under CSR, points are independent of each other and have the same propensity to be found at any location. We can generate data that conform to complete spatial randomness using the *rpoispp()* function. The r at the beginning is used to denote we are simulating data (you will this is common in R) and we are using a Poisson point process. Let's generate 223 points in a random manner:


```r
plot(rpoispp(223))
```

<img src="05-week5_files/figure-html/unnamed-chunk-11-1.png" width="672" />

You will notice that the points in a homogeneous Poisson process are not ‘uniformly spread’: there are empty gaps and clusters of points. Run the previous command a few times. You will see the map generated is different each time.

In classical literature, the homogeneous Poisson process (CSR) is usually taken as the appropriate ‘null’ model for a point pattern. Our basic task in analysing a point pattern is to find evidence against CSR. We can run a Chi Square test to check this. So, for example:


```r
quadrat.test(jitter_bur, nx = 3, ny = 2)
```

```
## 
## 	Chi-squared test of CSR using quadrat counts
## 	Pearson X2 statistic
## 
## data:  jitter_bur
## X2 = 113.09, df = 5, p-value < 2.2e-16
## alternative hypothesis: two.sided
## 
## Quadrats: 6 tiles (irregular windows)
```

Observing the results we see that the p value is well below convential standards for rejection of the null hypothesis. Observing data as the one we have for burglary in Fallowfield would be extremely rare if the null hypothesis was true. We can then conclude that the burglary data is not randomly distributed in the observed space. But no cop nor criminologist would really question this. We do know that crime is not randomly distributed in space. 

## Density estimates

In the presentations by Luc Anselin and the recommended reading materials we introduced the notion of density maps. Kernel density estimation involves applying a function (known as a “kernel”) to each data point, which averages the location of that point with respect to the location of other data points. 


```r
ds <- density(jitter_bur)
class(ds)
```

```
## [1] "im"
```

```r
plot(ds, main='Burglary density in Fallowfield')
```

<img src="05-week5_files/figure-html/unnamed-chunk-13-1.png" width="672" />

The density function is estimating a kernel density estimate. Density is nothing but the number of points per unit area. This method computes the intensity continuously across the study area and the object return is a raster image. To perform this analysis in R we need to define the bandwidth of the density estimation, which basically determines the area of influence of the estimation. There is no general rule to determine the correct bandwidth; generally speaking if h is too small the estimate is too noisy, while if h is too high the estimate may miss crucial elements of the point pattern due to oversmoothing (Scott, 2009). 

The key argument to pass to the density method for point patterm objects is sigma=, which determines the bandwidth of the kernel. In spatstat the functions bw.diggle, bw.ppl, and bw.scott can be used to estimate the bandwidth according to difference methods. The helpfiles recommend the use of the first two. These functions run algorithms that aim to select an appropriate bandwith.


```r
bw.diggle(jitter_bur)
```

```
##       sigma 
## 5.46379e-05
```

```r
bw.ppl(jitter_bur)
```

```
##      sigma 
## 0.00050307
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040690343 0.0008436845
```

You can see the Diggle algorithm gives you the narrower bandwith. We can test how they work with our dataset using the following code:


```r
par(mfrow=c(2,2))
plot(density.ppp(jitter_bur, sigma = bw.diggle(jitter_bur),edge=T),
     main = paste("h = 0.000003"))

plot(density.ppp(jitter_bur, sigma = bw.ppl(jitter_bur),edge=T),
     main=paste("h =0.0005"))

plot(density.ppp(jitter_bur, sigma = bw.scott(jitter_bur)[2],edge=T),
     main=paste("h = 0.0008"))

plot(density.ppp(jitter_bur, sigma = bw.scott(jitter_bur)[1],edge=T),
     main=paste("h = 0.004"))
```

<img src="05-week5_files/figure-html/unnamed-chunk-15-1.png" width="672" />

Baddeley et (2016) suggest the use of the bw.ppl algorithm because in their experience it tends to produce the more appropriate values when the pattern consists predominantly of tight clusters. But they also insist that to detect a single tight cluster in the midst of random noise the bw.diggle method seems to work best.

## Adding some context

Often it is convenient to use a basemap to provide context. In order to do that we first need to turn the image object generated by the spatstat package into a raster object, a more generic format for raster image used in R. 

```r
library(raster)
```

```
## Loading required package: sp
```

```
## 
## Attaching package: 'raster'
```

```
## The following objects are masked from 'package:spatstat':
## 
##     area, rotate, shift
```

```
## The following object is masked from 'package:nlme':
## 
##     getData
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
dmap1 <- density.ppp(jitter_bur, sigma = bw.ppl(jitter_bur),edge=T)
r1 <- raster(dmap1)
#remove very low density values
r1[r1 < 0.0001 ] <- NA
class(r1)
```

```
## [1] "RasterLayer"
## attr(,"package")
## [1] "raster"
```

Now that we have the raster we can add it to a basemap. 

Two-dimensional RasterLayer objects (from the raster package) can be turned into images and added to Leaflet maps using the addRasterImage function.

The addRasterImage function works by projecting the RasterLayer object to EPSG:3857 and encoding each cell to an RGBA color, to produce a PNG image. That image is then embedded in the map widget.

It’s important that the RasterLayer object is tagged with a proper coordinate reference system. Many raster files contain this information, but some do not. Here is how you’d tag a raster layer object “r” which contains WGS84 data:


```r
library(leaflet)

#make sure we have right CRS. We need the sp package for setting CRS to this raster: 
library(sp)
crs(r1) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#we also create a colour palet
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r1),
  na.color = "transparent")

#and then make map!
leaflet() %>% 
  setView(lng = -2.225814, lat = 53.441315, zoom = 14) %>% 
  addTiles() %>%
  addRasterImage(r1, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r1),
    title = "Burglary map")
```

```
## Warning in colors(.): Some values were outside the color scale and will be
## treated as NA
```

<!--html_preserve--><div id="htmlwidget-500cd3ceec2cd8e937e6" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-500cd3ceec2cd8e937e6">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2B5498 15.2416605111007%, #397EAB 30.4833210229658%, #40A9BF 45.7249815348309%, #79C6C6 60.966642046696%, #B2DCC9 76.2083025585611%, #E4F2CB 91.4499630704262%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000","12,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.152416605111007,"p_n":0.914499630704262},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
