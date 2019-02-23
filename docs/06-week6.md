
#Studying spatial point patterns


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
##       sigma 
## 0.000419315
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040709499 0.0008431261
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

<!--html_preserve--><div id="htmlwidget-54bf6764a1a0c222eb3d" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-54bf6764a1a0c222eb3d">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #244892 10.6387376777028%, #3164A0 21.2774753559628%, #3A82AD 31.9162130342228%, #3FA0BB 42.5549507124828%, #55BBC5 53.1936883907428%, #85CAC7 63.8324260690028%, #ACD9C9 74.4711637472628%, #D0E9CA 85.1099014255228%, #F2F9CC 95.7486391037827%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000","12,000,000","14,000,000","16,000,000","18,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.106387376777028,"p_n":0.957486391037827},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
