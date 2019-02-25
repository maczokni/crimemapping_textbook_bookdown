
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
## 0.0005474603
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040703690 0.0008433577
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

```
## Warning in colors(.): Some values were outside the color scale and will be
## treated as NA
```

<!--html_preserve--><div id="htmlwidget-f16f7f99fb7c38b9f31b" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-f16f7f99fb7c38b9f31b">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2E5B9B 17.9165332725092%, #3D8DB2 35.8330665460102%, #58BBC5 53.7495998195111%, #A2D5C8 71.6661330930121%, #DEF0CB 89.582666366513%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.179165332725092,"p_n":0.89582666366513},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And therw you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.

### Homework 2
Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?
