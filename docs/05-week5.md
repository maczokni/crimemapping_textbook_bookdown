
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
## 0.0003471564
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040710271 0.0008435412
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

<!--html_preserve--><div id="htmlwidget-f0b89786d25d6eff44a7" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-f0b89786d25d6eff44a7">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2E5C9C 18.2284266395372%, #3D8FB3 36.4568532794527%, #5CBDC5 54.6852799193683%, #A6D7C8 72.9137065592838%, #E3F2CB 91.1421331991994%, #FFFFCC "],"labels":["5,000,000","10,000,000","15,000,000","20,000,000","25,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.182284266395372,"p_n":0.911421331991994},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
