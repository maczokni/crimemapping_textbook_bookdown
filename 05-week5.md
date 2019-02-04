
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
## X2 = 109.07, df = 5, p-value < 2.2e-16
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
## 4.856702e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0003242956
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040723250 0.0008440882
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

<!--html_preserve--><div id="htmlwidget-e00ca6312869a0b498a0" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-e00ca6312869a0b498a0">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2C5699 15.9993673679999%, #3A82AD 31.9987347363228%, #41B0C1 47.9981021046456%, #85CAC7 63.9974694729685%, #BFE2C9 79.9968368412914%, #F2F9CC 95.9962042096142%, #FFFFCC "],"labels":["5,000,000","10,000,000","15,000,000","20,000,000","25,000,000","30,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.159993673679999,"p_n":0.959962042096142},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
