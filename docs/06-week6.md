
# Studying spatial point patterns


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

<img src="06-week6_files/figure-html/unnamed-chunk-2-1.png" width="672" />

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

<img src="06-week6_files/figure-html/unnamed-chunk-4-1.png" width="672" />


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

<img src="06-week6_files/figure-html/unnamed-chunk-8-1.png" width="672" />

In the case of crime, as we have hinted some of this may be linked to the nature of crime itself. Hint: repeat victimisation. However, this pattern of duplication is fairly obvious across all crime categories in the police.uk website and, although I have not explored this in detail, I strongly suspect it is a function of the anonimisation process used to create these maps. The coordinates provided in the open data are not the exact locations of crimes, but they come from a list of points generated for purposes of data publication. You can see the details [here](https://data.police.uk/about/#anonymisation). This process is likely inflating the amount of duplication we observe. So keep in mind when analysing and working with this data that it is not the same as working with the real locations.

What to do about duplicates in spatial point pattern analysis is not always clear. You could simply delete the duplicates, but of course that may ignore issues such as repeat victimisation. You could also use jittering, which will add a small perturbation to the duplicate points so that they do not occupy the exact same space. Which again, may ignore things like repeat victimisation. Another alternative is to make each point "unique" and then attach the multiplicites of the points to the patterns as *marks*, as attributes of the points. Then you would need analytical techniques that take into account these marks.

If you were to be doing this for real you would want access to the real thing, not this public version of the data and then go for the second solution suggested above. We don't have access to the source data, so for the sake of simplicity and so that we can illustrate how spatstat works we will add some jittering to the data. The first argument for the function is the object, retry asks whether we want the algorithm to have another go if the jittering places a point outside the window (we want this so that we don't loose points), and the drop argument is used to ensure we get a ppp object as a result of running this function.


```r
jitter_bur <- rjitter(bur_ppp, retry=TRUE, nsim=1, drop=TRUE)
plot(jitter_bur)
```

<img src="06-week6_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Notice the difference with the original plot. Can you see how the circumferences do not overlap perfectly now?

## Inspecting our data with spatstat

This package supports all kind of exploratory point pattern analysis. One example of this is quadrant counting. One could divide the window of observation into quadrants and count the number of points into each of these quadrants. For example, if we want four quadrants along the X axis and 3 along the Y axis we could used those parameters in the quadratcount() function. Then we just use standard plotting functions from R base.


```r
Q <- quadratcount(jitter_bur, nx = 4, ny = 3)
plot(jitter_bur)
plot(Q, add = TRUE, cex = 2)
```

<img src="06-week6_files/figure-html/unnamed-chunk-10-1.png" width="672" />

In the video lectures by Luc Anselin would were exposed to the notion of complete spatial randomness (CSR). When we look at a point pattern process the first step in the process is whether it has been generated in a random manner. Under CSR, points are independent of each other and have the same propensity to be found at any location. We can generate data that conform to complete spatial randomness using the *rpoispp()* function. The r at the beginning is used to denote we are simulating data (you will this is common in R) and we are using a Poisson point process. Let's generate 223 points in a random manner:


```r
plot(rpoispp(223))
```

<img src="06-week6_files/figure-html/unnamed-chunk-11-1.png" width="672" />

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

<img src="06-week6_files/figure-html/unnamed-chunk-13-1.png" width="672" />

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
## 0.0004174102
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040706120 0.0008437792
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

<img src="06-week6_files/figure-html/unnamed-chunk-15-1.png" width="672" />

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

<!--html_preserve--><div id="htmlwidget-f77852d67f49cad85a28" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-f77852d67f49cad85a28">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #244892 10.530370278914%, #31649F 21.0607405583634%, #3A81AD 31.5911108378128%, #3F9FBA 42.1214811172623%, #52BAC5 52.6518513967117%, #82C9C7 63.1822216761611%, #A9D8C8 73.7125919556106%, #CDE8CA 84.24296223506%, #EFF7CB 94.7733325145094%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000","12,000,000","14,000,000","16,000,000","18,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.10530370278914,"p_n":0.947733325145094},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
