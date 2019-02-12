
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
## 0.0005710959
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040697596 0.0008446352
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

<!--html_preserve--><div id="htmlwidget-aaf7dfbd3b9d87c8a73e" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-aaf7dfbd3b9d87c8a73e">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2F5F9D 19.3995787499968%, #3E95B6 38.7991575009688%, #6DC2C6 58.1987362519407%, #B7DEC9 77.5983150029127%, #F6FBCC 96.9978937538846%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.193995787499968,"p_n":0.969978937538846},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
