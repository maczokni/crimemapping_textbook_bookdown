
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
## 5.160246e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0003701702
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040700180 0.0008414985
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

<!--html_preserve--><div id="htmlwidget-153008fe13800394cbb5" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-153008fe13800394cbb5">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #31639F 20.6978032217848%, #3F9DB9 41.3956064439849%, #7EC7C6 62.0934096661851%, #C8E6CA 82.7912128883852%, #FFFFCC "],"labels":["5,000,000","10,000,000","15,000,000","20,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.206978032217848,"p_n":0.827912128883852},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
