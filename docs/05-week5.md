
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
##        sigma 
## 4.249614e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0003616265
```

```r
bw.scott(jitter_bur)
```

```
##     sigma.x     sigma.y 
## 0.004069941 0.000844521
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

<!--html_preserve--><div id="htmlwidget-25e95795676f10ffc7cc" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-25e95795676f10ffc7cc">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #30619E 19.8638447380367%, #3E98B7 39.7276894764936%, #73C4C6 59.5915342149505%, #BDE1C9 79.4553789534074%, #FDFECC 99.3192236918643%, #FFFFCC "],"labels":["5,000,000","10,000,000","15,000,000","20,000,000","25,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.198638447380367,"p_n":0.993192236918643},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
