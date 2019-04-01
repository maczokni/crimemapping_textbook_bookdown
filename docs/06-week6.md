
# Studying spatial point patterns


## What we'll do today

We have now covered quite a bit! You've learnt about spatial objects and various formats in which they come and are stored by R, how to produce maps using a variety of packages, and also provided you with a brief introduction to common spatial operations. In what remains of the semester we are going to shift the emphasis and start focusing a bit more on spatial statistics. First we will focus on techniques that are used to explore and analyse points in a geographical space and in subsequent sessions we will cover techniques that are used to analyse spatial data when our unit of analysis are polygons (e.g., postal code areas, census areas, police beats, etc).

We will introduce a new R package called `spatstat`, that was developed for spatial point pattern analysis and modelling. It was written by Adrian Baddeley and Rolf Turner. There is a [webpage](http://spatstat.org) dedicated to this package. The [thickest book](https://www.crcpress.com/Spatial-Point-Patterns-Methodology-and-Applications-with-R/Baddeley-Rubak-Turner/p/book/9781482210200) in my library, at 810 pages, is dedicated to this package. So as you can imagine the theory and practice of spatial pattern analysis is something one could devote an entire course to. You can get a pdf document used in a course the authors of this package develop [here](https://research.csiro.au/software/wp-content/uploads/sites/6/2015/02/Rspatialcourse_CMIS_PDF-Standard.pdf). In our course we are only going to provide you with an introductory practical entry into this field of techniques. If this package is not installed in your machine, make sure you install it before we carry on.


```r
library(sf)
library(tmap)
library(dplyr)
library(spatstat)
```

## Getting the data

We will be using the crime data from Greater Manchester police we have been using so far. Let's focus on burglary in the Fallowfield area. The code below has already been explained and used in previous sessions, so we won't go over the detail again. But rather than cut and paste automatically, try to remember what each line of code is doing.

By the way, the police data for Manchester we have used in previous sessions correspond to only one month of the year. Here we are using a full year worth of data, so the data import will take a bit longer.


```r
#Read a geojson file with Manchester wards (remember we learned about geojson files in week 4)
manchester_ward <- st_read("https://raw.githubusercontent.com/RUMgroup/Spatial-data-in-R/master/rumgroup/data/wards.geojson")
```

```
## Reading layer `wards' from data source `https://raw.githubusercontent.com/RUMgroup/Spatial-data-in-R/master/rumgroup/data/wards.geojson' using driver `GeoJSON'
## Simple feature collection with 215 features and 12 fields
## geometry type:  POLYGON
## dimension:      XY
## bbox:           xmin: 351664 ymin: 381168.6 xmax: 406087.5 ymax: 421039.8
## epsg (SRID):    27700
## proj4string:    +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs
```

```r
#Create a new object that only has the fallowfield ward
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
#again remove things we don't need
rm(crimes)
rm(burglary)
```

Now we have all our data cleaned and all our files prepared. Let's see the results!


```r
tm_shape(fallowfield) + 
  tm_fill() +
  tm_shape(bur_fal) +
  tm_dots()
```

<img src="06-week6_files/figure-html/unnamed-chunk-3-1.png" width="672" />

In the point pattern analysis literature each point is often referred to as an **event** and these events can have **marks**, attributes or characteristics that are also encoded in the data. In our spatial object one of these *marks* is the type of crime (altough in this case it's of little interest since we have filtered on it).

## Getting the data into spatstat: the problem with duplicates

So let's start using spatstat.The first thing we need to do is to transform our `sf` object into a `ppp` object which is how `spatstat` likes to store its point patterns. Unfortunately, spatstat and many other packages for analysis of spatial data precede sf, so the transformation is a bit awkard. Also before we do that, it is important to realise that a point pattern is defined as a series of events in a given area, or window, of observation. It is therefore extremely important to precisely define this window. In `spatstat` the function `owin()` is used to set the observation window. However, the standard function takes the coordinates of a rectangle or of a polygon from a matrix, and therefore it may be a bit tricky to use. Luckily the package `maptools` provides a way to transform a `SpatialPolygons` into an object of class `owin`, using the function `as.owin()`. Here are the steps: 


First we transform our Falllowfield polygon into a sp object:


```r
fallowfield_sp <-as(fallowfield, "Spatial")
```



Then we use the as.owin function to define the window. Note that by adding two colons (`::`) we can reference a function in a library explicitly without loading the library. So if you receive an error message, make sure you have the relevant package installed.


```r
window <- maptools::as.owin.SpatialPolygons(fallowfield_sp)
```



Now, use the class function and print the window object to check that this worked:


```r
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

Now that we have created the window as an `owin` object let's get the points. First we will extract the coordinates from our sf point data into a matrix:


```r
sf_bur_fal_coords <- matrix(unlist(bur_fal$geometry), ncol = 2, byrow = T)
```

Then we use the `ppp` function to create the object using the information from our matrix and the window that we created.


```r
bur_ppp <- ppp(x = sf_bur_fal_coords[,1], y = sf_bur_fal_coords[,2],
                   window = window, check = T)
```

```
## Warning: data contain duplicated points
```

```r
plot(bur_ppp)
```

<img src="06-week6_files/figure-html/unnamed-chunk-8-1.png" width="672" />



Notice the warning message about duplicates. In spatial point pattern analysis an issue of significance is the presence of duplicates. The statistical methodology used for spatial point pattern processes is based largely on the assumption that processes are *simple*, that is, that the points cannot be coincident. That assumption may be unreasonable in many contexts (for example, the literature on repeat victimisation indeed suggests that we should expect the same households to be at a higher risk of being hit again). Even so the point (no pun intended) is that *"when the data has coincidence points, some statistical procedures will be severely affected. So it is always strongly advisable to check for duplicate points and to decide on a strategy for dealing with them if they are present"* (Baddeley et al., 2016: 60).

We can check the duplication in a `ppp` object with the following syntax:


```r
any(duplicated(bur_ppp))
```

```
## [1] TRUE
```

To count the number of coincidence points we use the `multiplicity()` function. This will return a vector of integers, with one entry for each observation in our dataset, giving the number of points that are identical to the point in question (including itself).


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

<img src="06-week6_files/figure-html/unnamed-chunk-12-1.png" width="672" />

In the case of crime, as we have hinted some of this may be linked to the nature of crime itself. Hint: repeat victimisation. However, this pattern of duplication is fairly obvious across all crime categories in the police.uk website.

This is due to the way in which spatial anonymisation of police.uk data is carried out. This is done using geomasking, whereby there exist a pre-determined list of points that each crime event gets "snapped" to its nearest one. So, the coordinates provided in the open data are not the exact locations of crimes, but they come from a list of points generated for purposes of data publication. You can see the details [here](https://data.police.uk/about/#anonymisation). This process is likely inflating the amount of duplication we observe, because each snap point might have many crimes near it, resulting in those crimes being geo-coded to the same exact location. So keep in mind when analysing and working with this data set that it is not the same as working with the real locations. If you are interested in the effects of this read the paper [Lisa Tompson, Shane Johnson, Matthew Ashby, Chloe Perkins & Phillip Edwards (2015) UK open source crime data: accuracy and possibilities for research, Cartography and Geographic Information Science, 42:2, 97-111, DOI: 10.1080/15230406.2014.972456](https://www.tandfonline.com/doi/abs/10.1080/15230406.2014.972456).

What to do about duplicates in spatial point pattern analysis is not always clear. You could simply delete the duplicates, but of course that may ignore issues such as repeat victimisation. You could also use jittering, which will add a small perturbation to the duplicate points so that they do not occupy the exact same space. Which again, may ignore things like repeat victimisation. Another alternative is to make each point "unique" and then attach the multiplicites of the points to the patterns as *marks*, as attributes of the points. Then you would need analytical techniques that take into account these marks.

If you were to be doing this for real you would want access to the real thing, not this public version of the data and then go for the latter solution suggested above. We don't have access to the source data, so for the sake of simplicity and so that we can illustrate how `spatstat` works we will instead add some jittering to the data. The first argument for the function is the object, `retry` asks whether we want the algorithm to have another go if the jittering places a point outside the window (we want this so that we don't loose points), and the `drop` argument is used to ensure we get a `ppp` object as a result of running this function (which we do).


```r
jitter_bur <- rjitter(bur_ppp, retry=TRUE, nsim=1, drop=TRUE)
plot(jitter_bur)
```

<img src="06-week6_files/figure-html/unnamed-chunk-13-1.png" width="672" />

Notice the difference with the original plot. Can you see how the circumferences do not overlap perfectly now?

## Inspecting our data with spatstat

This package supports all kind of exploratory point pattern analysis. One example of this is **quadrant counting**. One could divide the window of observation into quadrants and count the number of points into each of these quadrants. For example, if we want four quadrants along the X axis and 3 along the Y axis we could used those parameters in the `quadratcount()` function. Then we just use standard plotting functions from R base.


```r
Q <- quadratcount(jitter_bur, nx = 4, ny = 3)
plot(jitter_bur)
plot(Q, add = TRUE, cex = 2)
```

<img src="06-week6_files/figure-html/unnamed-chunk-14-1.png" width="672" />

In the video lectures for this week, Luc Anselin  introduced the notion of **complete spatial randomness** (CSR for short). When we look at a point pattern process the first step in the process is to ask whether it has been generated in a random manner. Under CSR, points are independent of each other and have the same propensity to be found at any location. We can generate data that conform to complete spatial randomness using the *rpoispp()* function. The r at the beginning is used to denote we are simulating data (you will see this is common in R) and we are using a Poisson point process, a good probability distribution for these purposes. Let's generate 223 points in a random manner:


```r
plot(rpoispp(223))
```

<img src="06-week6_files/figure-html/unnamed-chunk-15-1.png" width="672" />

You will notice that the points in a homogeneous Poisson process are not ‘uniformly spread’: there are empty gaps and clusters of points. Run the previous command a few times. You will see the map generated is different each time.

In classical literature, the *homogeneous Poisson process* (CSR) is usually taken as the appropriate ‘null’ model for a point pattern. Our basic task in analysing a point pattern is to find evidence against CSR. We can run a Chi Square test to check this. So, for example:


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

Observing the results we see that the p value is well below convential standards for rejection of the null hypothesis. Observing our data of burglary in Fallowfield would be extremely rare if the null hypothesis was true. We can then conclude that the burglary data is not randomly distributed in the observed space. But no cop nor criminologist would really question this. They would rarely be surprised by your findings! We do know that crime is not randomly distributed in space. 

## Density estimates

In the presentations by Luc Anselin and the recommended reading materials we introduced the notion of density maps. **Kernel density estimation** involves applying a function (known as a “kernel”) to each data point, which averages the location of that point with respect to the location of other data points.  The surface that results from this model allows us to produce **isarithmic maps**, also referred to in common parlor as heatmaps. Beware though, cartographers [really dislike](http://cartonerd.blogspot.com/2015/02/when-is-heat-map-not-heat-map.html) this common parlor. We saw this kind of maps when covering the various types of thematic maps. 

Kernel density estimation maps are very popular among crime analysts. According to Chainey (2012), 9 out of 10 intelligence professionals prefer it to other techniques for hot spot analysis. As compared to visualisations of crime that relies on point maps or thematic maps of geographic administrative units (such as LSOAs), kernel density estimation maps are considered best for location, size, shape and orientation of the hotspot (Chainey, 2012). [Spencer Chainey and his colleagues (2008)](http://discovery.ucl.ac.uk/112873/1/PREPRINT_-_Chainey%2C_Tompson_%26_Uhlig_2008.pdf) have also suggested that this method produces some of the best prediction accuracy. The areas identified as hotspots by KDE (using historical data) tend to be the ones that better identify the areas that will have high levels of crime in the future. Yet, producing these maps (as with any map, really) requires you to take a number of decisions that will significantly affect the resulting product and the conveyed message. Like any other data visualisation technique they can be powerful, but they have to be handled with great care.

Essentially this method uses a statistical technique (kernel density estimation) to generate a smooth continuous surface aiming to represent the density or volume of crimes across the target area. The technique, in one of its implementations (quartic kernel), is described in this way by Eck and colleagues (2005):

+ *“a fine grid is generated over the point distribution;*
+ *a moving three-dimensional function of a specified radius visits each cell and calculates weights for each point within the kernel’s radius. Points closer to the centre will receive a higher weight, and therefore contribute more to the cell’s total density value;*
+ *and final grid cell values are calculated by summing the values of all kernel estimates for each location”*

![](img/kde.png)
(Reproduced from Eck et al. 2012)

The values that we attribute to the cells in crime mapping will typically refer to the number of crimes within the area’s unit of measurement. We don’t have the time to elaborate further on this technique now, but if you did the required reading you should have at least a notion of how this works.

Let's produce one of this density maps:


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

<img src="06-week6_files/figure-html/unnamed-chunk-17-1.png" width="672" />

The density function is estimating a kernel density estimate. Density is nothing but the number of points per unit area. This method computes the intensity continuously across the study area and the object returns a raster image. 

To perform this analysis in R we need to define the **bandwidth** of the density estimation, which basically determines the area of influence of the estimation. There is no general rule to determine the correct bandwidth; generally speaking if the bandwidth is too small the estimate is too noisy, while if bandwidth is too high the estimate may miss crucial elements of the point pattern due to oversmoothing (Scott, 2009). 

The key argument to pass to the density method for point patterm objects is `sigma=`, which determines the bandwidth of the kernel. In spatstat the functions `bw.diggle()`, `bw.ppl()`, and `bw.scott()` can be used to estimate the bandwidth according to difference methods. The helpfiles recommend the use of the first two. These functions run algorithms that aim to select an appropriate bandwith.


```r
bw.diggle(jitter_bur)
```

```
##       sigma 
## 3.94607e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0004302944
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040697620 0.0008425728
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

<img src="06-week6_files/figure-html/unnamed-chunk-19-1.png" width="672" />

Baddeley et (2016) suggest the use of the `bw.ppl()` algorithm because in their experience it tends to produce the more appropriate values when the pattern consists predominantly of tight clusters. But they also insist that if your purpose it to detect a single tight cluster in the midst of random noise then the `bw.diggle()` method seems to work best.

Apart from selecting the bandwidth we also need to specify the particular kernel we will use. In density estimation there are different types of kernel (as illustrated below):

![](img/kerneltypes.png)
Source: wikepedia

You can read more about kernel types in the Wikipedia [entry](https://en.wikipedia.org/wiki/Kernel_(statistics)). This relates to the type of kernel drawn around each point in the process of counting points around each point. The use of these functions will result in slightly different estimations. They relate to the way we weight points within the radius: *“The normal distribution weighs all points in the study area, though near points are weighted more highly than distant points.  The other four techniques use a circumscribed circle around the grid cell.    The uniform distribution weighs all points within the circle equally.  The quartic function weighs near points more than far points, but the fall off is gradual. The triangular function weighs near points more than far points within the circle, but the fall off is more rapid. Finally, the negative exponential weighs near points much more highly than far points within the circle and the decay is very rapid.”* (Levine, 2013: 10.10).

Which one to use? Levine (2013) produces the following guidance: *“The use of any of one of these depends on how much the user wants to weigh near points relative to far points.  Using a kernel function which has a big difference in the weights of near versus far points (e.g., the negative exponential or the triangular) tends to produce finer variations within the surface than functions which weight more evenly (e.g., the normal distribution, the quartic, or the uniform); these latter ones tend to smooth the distribution more*. However, Silverman (1986) has argued that it does not make that much difference as long as the kernel is symmetrical. Chainey (2013) suggest that in his experience most crime mappers prefer the quartic function, since it applies greater weight to crimes closer to the centre of the grid. The authors of the CrimeStat workbook (Smith and Bruce, 2008), on the other hand, suggest that the choice of the kernel should be based in our theoretical understanding of the data generating mechanisms. By this they mean that the processes behind spatial autocorrelation may be different according to various crime patterns and that this is something that we may want to take into account when selecting a particular function. They provide a table with some examples that may help you to understand what they mean:

![](img/kerneltips.png)
(Source: Smith and Bruce, 2008.)

The default kernel in `density.ppp()` is the `gaussian`. But there are other options. We can use the `epanechnikov`, `quartic` or `disc`. There are also further options for customisation. We can compare these kernels:


```r
par(mfrow=c(2,2))
plot(density.ppp(jitter_bur, sigma = bw.ppl(jitter_bur),edge=T),
     main=paste("Gaussian"))
plot(density.ppp(jitter_bur, kernel = "epanechnikov", sigma = bw.ppl(jitter_bur),edge=T),
     main=paste("Epanechnikov"))
plot(density.ppp(jitter_bur, kernel = "quartic", sigma = bw.ppl(jitter_bur),edge=T),
     main=paste("Quartic"))
plot(density.ppp(jitter_bur, kernel = "disc", sigma = bw.ppl(jitter_bur),edge=T),
     main=paste("Disc"))
```

<img src="06-week6_files/figure-html/unnamed-chunk-20-1.png" width="672" />

When reading these maps you need to understand you are only looking at counts of crime in a smooth surface. Nothing more, nothing less. Unlike with choropleth maps we are not normalising the data. We are simply showing the areas where there is more crime, but we are not adjusting for anything (like number of people in the area, or number of houses to burgle). So, it is important you keep this in the back of your mind. As [this comic](https://xkcd.com/1138/) suggests you may end up reading too much into it if you don’t remember this. There are ways to produce density maps adjusting for a second variable, such as population size, but we do not have the time to cover this. 


There are also general considerations to keep in mind. Hot spots of crime are a simply a convenient perceptual construct. As Ned Levine (2013: 7.1) highlights *“Hot spots do not exist in reality, but are areas where there is sufficient clustering of certain activities (in this case, crime) such that they get labeled such. There is not a border around these incidents, but a gradient where people draw an imaginary line to indicate the location at which the hot spot starts.”*  Equally, there is not a unique solution to the identification of hot spots. Different techniques and algorithms will give you different answers. As Levine (2013: 7.7) emphasises: *“It would be very naive to expect that a single technique can reveal the existence of hot spots in a jurisdiction that are unequivocally clear. In most cases, analysts are not sure why there are hot spots in the first place. Until that is solved, it would be unreasonable to expect a mathematical or statistical routine to solve that problem.”* So, as with most data analysis exercises one has to try different approaches and use professional judgement to select a particular representation that may work best for a particular use. Equally, we should not reify what we produce and, instead, take the maps as a starting point for trying to understand the underlying patterns that are being revealed. Critically you want to try several different methods. You will be more persuaded a location is a hot spot if several methods for hot spot analysis point to the same location.

## Adding some context

Often it is convenient to use a basemap to provide context. In order to do that we first need to turn the image object generated by the `spatstat` package into a raster object, a more generic format for raster image used in R. Remember rasters from the first week? Now we finally get to use them a bit!


```r
library(raster)
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

Two-dimensional `RasterLayer` objects (from the `raster` package) can be turned into images and added to `Leaflet` maps using the `addRasterImage()` function.

The `addRasterImage()` function works by projecting the `RasterLayer` object to EPSG:3857 and encoding each cell to an RGBA color, to produce a PNG image. That image is then embedded in the map widget.

It’s important that the `RasterLayer` object is tagged with a proper coordinate reference system. Many raster files contain this information, but some do not. Here is how you’d tag a raster layer object “r” which contains WGS84 data:


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

<!--html_preserve--><div id="htmlwidget-1d25cdf321e5797bd63b" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-1d25cdf321e5797bd63b">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAAgAElEQVR4nO19zY9s23XXb+19TlV333ufn+OnlwgbgyFmgmyDgyO+pEQIEcLfECHEh4SEokgJQYFMmDwcQxQie8IgCAkBMyYMGDAiQohIgJU4AxAfQhBwgp5tnv3uu7erztlrMVgfe59TVd1V1dXvdr3bS+q+t+vjfOz1/Vtr7UMigkd6fSm96gt4pFdLjwLwmtOjALzm9CgArzk9CsBrTo8C8JrTowC85vQoAK85PQrAa06PAnAG9Oxz78izz71zL5Btdx8HfZX06R/5JRnXBavViHEUsAje/82fp1d9XcfQfTG9pQctAL4AiQiLPuHySY9ukTGuC65fjgCAi8sO3aKDMIN5c70emX8zPVgXMF8AEYCLgEcBs0BEf8ooKGMBF4EwgPjWWfIdwG7m34dQPGgL4CRQZjMLSmFwEUBMKJhR1BiAiEDny/dXQmchAIAqNhcBkUBYIFDB8NcAICUAiaAV7vMsc39Ypt/pwbqAObkFYGZw08PAYlaBzTWwmYYHQB82M4+h87AAbu5FQGWq28IAQ0AEEOnn5BXLwLGMfxUCcxYWQOyHWUL7ExEI1FiG5ucVcn/OxFMz9dTHOw8LAIQVEDFfTwSCgAtMQgRMBMAzBI0RPkw6B5M/p7MRAM0EAIBAREiJAJCmhiIQEIgFDHULAsH3vvHhYQBvfP5uzH9VwnMWLmBOKRFyTioE4fdNEFhjhbsq/w/88N/d+wgf/0NfFjpT3OF8BMB4SlABSJ0KAVnir17AhEDkTkHg93/pK0J7rszbf+QrYsboLOlsBKACfISUE3KXkbICP572e6ZwFwPw9pe+IpTUzXzyj//ijYd52wSFSPX/w5KBU7qLs4kBnIiAlAm5T+CSYvEZAoix4Egg6Pf+6N+XRE2McQtHHXmkJABDP39mYeDZWABA19cFIHUpLICTB4qCw03AH/6L/0i1OX4o3Ms2evtLXxEidUdkKemx9Cqzh7MSAEC1LmVlfmpigAALjPmHrOif+Nv/XOzgevzQ7O1M/dSf/EVR4Klai5tqEA85PTxDAbCFN+bP+X+o///hn/qnwiMDImiZ6sf+fX/6lyeH+8Ef+2q4CRWSxhWcYSR4dgIAtGZ692f26QP4wR/7qpRVgayL1hBAekz7JmHqBj7741/TANE03gUmmRB8mPw/lVU5QwEg+02Tvw+lt774C8Ijo1yPKNcFPLoXoMrMJrT/7J/9ajX7Td7nf6d7ygQ87rkv2TpDARD7LZO/D6FPfPHLIlZFLKsRfD2CR8WUt/UUfPbHv1aHqEPz9Q8iq0sk3BgHHE13k/Nb6QwFwPw8688uuslEDgNDBCgjo1wXjC8H8Fpf28pEsR6ELUfUeMQCQeynqvuab7UoNcu4Dxk4OxxAmS+QwjsRP1NOvPH5d2ReD/DF92aScTWCMqEMHgcgmBgBJts5WeprAm1EcQuwh51uGb+XEJhPIVSI49Q4w9lZABEBW1sYuxDMP0Txa0LtoosApTDGVcF4PWL0QLDJBLTQgEmzibD3Guh5PV1Mt+AGxwVthGRYQ9pyT6cIBM9KADzN0+ZQFYK5BahmUxnpVbqtTaYsGIeC4XpEGVSYgBrYOcws3mdQGMxc3YFJwCQzOKGhJr8WF8qTHbnSebkAr/pNBGCW+DdBmpePnfktUusNJuOggYSXkHPDTMBb0RhcGIQEQDbaz/cBgw6liP7NArC5H7p7oXNC5yUAcAvAKCOZC5i+70FTSmbmWSaaUxdQwEwoI8dxU9LvK8pI0YYurEKngD82AkL3FvcRqjsyqTHA6QHFM3MBEua4jFz9si1M1AqS+82p2ZxE6WZNShGMI5t514PkLiH3Wm2EuxwTAm9A3UYnZb9rf4szbAEa7hoHPFgB2IXkRW+gMWVy97ZoOSVtGMltCkUWUVehcAtRiqDYsAkRIfUJeZGRcjX57UzCJF2UGg+cvgWt8f/3ZGHO0wWwzgLwltxci0WErksK9IwAU7UObc0AVj1kNoYaY3OfkZcZZeQ4X/hgqh3I0YXMW2KRUxFhq+afih6sBdhNEuZ7s+6vGp9zQu7VClCiJpqelW6ltpNFGTmpBUgLcwGw91nAhgcA3i9AFh/wpjU6AYXWN6jjNrqLGzg7C3ATEdT/546Q+wzhCtKkpP9nEbBMg0GNsMy0A6CckHoVHhjqqBCBNZ8mIKWkYJIx3jOSU4pBTDg113dqOkMLgFYpNl8nbRbJXULuKkafkkX3s2DKNV/aY6Rabo4+w6bJIJmbifc9IL0PF2BW6r46jc5QAGY4bSMFburdBXjDSLI83QPDNJccD+JiofU/0V8QQZ5ZGW9GSeoCSpGKSZyUZOKibpKCY93AGQoAJkFYvNa+lyqDKphimUFKahUaIWjxYa0z2A8bOhgIYVP+9YZU0QnlsACnkgE7LXv8ISc+vtHZCcBtgZFH+y2m75mBtpLXwk37VXcDzGbS1yVAIGmPbRgDWfXPQaK7diPvorbV/T7oLINAan70b4LQtgWS+EIyqyAsVrqN2Kp+2gpNZV1AicBDmYA+tVmkSo9rKfOJA0BU9+PW5T4E7MFbgG2Zz4YlpNl7Xr4VL9vWil1Kyf7dPLJWCCUqhGXNNm7uph+1TuDBo1h6eNK7bu5Fmjjgls8fEwc8aAtQ0bb5O5oaaVLm/K85OzNbsait8Hl0X0u3HsXHIc0CjKsCiFYK2XLDtjLn5/HSsG9Xc3orLWAhUDD/9GL2oAUA8GCtkf6I2GtYTlRtuQjAo9UKZpG5p4M5J+TMKDyrELoFWBetEwwcXUfUxBQiALgJzg66n0q3fs9l0y/yHszMgxYAr4HPM6Dwj/BS7DRiLoVBg1b6pJ3YIQJlKEo4EtJIYEOE/JjeIyCsRSJB7RMM129mnyNLwP7MOUACKlC118ePogcuAGYBxE09BTNdsxMRkAAuHjFrrx+R/ssiSKBIoVJO6BZAGRjDwKBCaA2sVxq96mjo8KRbWLjpEpoDSTfdz+wesM/3HHy4JyDoQQuAo3YslUlzBNAROYD1c9btC2jLlwdPYhLgQ6VlKEgrmi2uanaxtK51MZFewqL+sr0j6UaaFXZuwXb2/sxd6GELgBdy0PTfNYEYEZBjRAxWq0c0b2jJWHN3FQAt9GgTKCN/MEzSSUDXurAg2QmzIYlt2gepW9EcUgVsW9UEAmoE+1i6q3H4UAWgTVPe/82fp21pCxGh7xKWy4yLiw7DUECFQJHOqWC45nnzBgCkNYGhwZ2b82oBABC0zr/IKKsSgjNJI+3zDMuRLf1raVIdxH4McMvlDR7CAJMc3eLVWsK7tImdTAAOzUF3fd5NrQM3qXDVeFDk8CEAfUa3yNrMkQjwWICBOXqWEiEvO+TLDuXl2DSMNAEXAvmFULU68Zo0nzwQmSHUohQDoCNLyO166HUeLwEnEYBTT78q5KrQbRoTiMoE03dED6QRfbfMkKIIn2t75M52ZUQAdQlpmZGvOqTnuYJBEd2TBXQeeembu+rwB900ecaiaSiELag8XIj0fppqhpeLD7keo4eJBFLV9BrkoWL6mQLZy11CXmZt4rCYwQsprrVhOfyzyy7q/e7e2yAPdgxgFh+EMNW08MDbiuuf722w8zvNj7+gFc621lE/cKgynswFnCxTcW105K7NwRvXAFEUJ3kDZ1cqSje5EmvgsBIx+U+zsUNt79oMzNzHV0Rx+l3s7ce9MUWrksIA0R7bioSzr6mwD4uQhcfMODqgPIkFCIbNX7vDAVvp9lwgSrHRu08B71KaqaRzzo6Vu4TUZ1CXmo9JpGZtnj/pHjYgxnGEtrlEy8r73exGbNNYn53fmWk7mmMomtlMJN1yrF10GgsQEqDacIroNL5o5o0mP435JNcLP+3UHybYovcJaWFNHF7vtw/5dC87NNwsZm36RHAxZULiFK3nvGey7lkMufDcwjHV9qbvwI6RU0LXqe4SC0SO70k8iQC4xkjLfNBx0alpmlfaptacJtoZmLzUdG8beayQLF2UoYAHjmFQsvQMpB3EkHYIVNFFrSpSBKI5C3JOGJNowLkvFhAu7Wbuu8VwLWcWoAgSEXJH6HrdycJrHqEBB673aQTANJOhSFz04B8ZnfoASNtu3QY7bYmUC0MGhpQm6vPr8mtLhNRnpE6zB14VlHWt9cdsnwBkJszNs9YH9Kog5nYyIYm2l+VEKOWEMVBz8Slpe3vukrWoqwB0XUa/dNYJxtFt4OFXcBIB8HSKrPjuCx8gyYHX5UUZH/7Qc9TztMLBI6MMxSaFth2tdgRRJsgoKC9H3RiimADABCwJiOvfASB57p8ai5GVEWNmpEJ7707aCu9NRFDhWixzPCbHZxO6RUJ/0cU6pXW51Z3sor0F4Nnn3pFd0zqhnWJbuoePPhzujCaLIuBUzXTbxOGFGmEt2dKqhIYAm9pApH4XiSBjQXk5YFwVsEOGTbBJIMD9s/l/l6ssFJ/XvQoF3ZAwDGW/exNNXqS0Y+bbiQjouoTlVY/+qsfwYohB1m7Rob/q9f7HstHcclMqOOfhhgDchNDtom9//eforR/6Bd1njy1qTWSzdofHAS7ZZZTwbzmrKSSaWodxUPtbBq3hR0rXXrv7XAF4YIwvR4yrouNgll61CbdH2uybT0u9rnBJfUIGkNdlkpLtvCe0lo2bSuImuQXqFhnLN5ZYvnmB9N411i9HMAu6i4zuaQ8pgnw9TkGhA2mSBt4IIuxxhnZ/PdfYGp3vT24BfBCT4Jh/qjg666x+GRjjusQYVwVotpyVBby2TSG8388CTvdX4Q48LYSbbW//0n9TIuRF1vbz2a6iEXtM7yq6lYrvM+C55ZxIXV6/zLj4xCUuPvkUF5+4RH/RGfDVoX+6QPekR15kS4H3W9s5j0+GBLa7ZaWk/itm5g+RAJnFAIbx5y6hW3QKA3s1ziZ7x4FRxjZYnGqEp3JcGDxoABjTxc05o+RMVZjt7dBgfyQNZUJeGAg118BIi2e3JjZpHHMEu5chEaG/6LH8/ifoP/MxLH/gCfpLFYDuIiOHAKSdG1ruolYI0rYX51Qz7RsuONV8egp2HHZxYSrFJ3arOewuNJIHakNGKWoF2HoAwgrR9KDCAhlNAIYaMzhk3Pb01f1+phIQ3b+sVi5bZuEMqJpvbhBTuHfy5LNbBkpTIvRXHfpPPcXv/91X6D/5FP1Vb3B2h/y0R3660OrmsREg9g0CTYs/8cUvy+Vlj6dvXWHxdIHh+Rov3rsGF8Fi0WEcygSvF9kP756Tw5uJACSb91tkdBddBIUqJNBfBgtHjj3p926CyoHBA+vuYAHumJWx6mHgTzMF1uCUQvBApDOEXlFsXUDk+Y2WaxYZVqedN9hcbjIB6NF/6in+6FsZv7V6qhr/fI20zEhPe2BgpEW+ccPM22jvr7pvnFs2VY5wnmgrXh4cHROiuBWAuLZZIccCwdYnF9dKu8ZEc81DaD8PtVnUXXAdvpjfV/2/HsctgD6hctImbotU73+6cZS7Grcifm87hSAR8lWPLzxL+IPfl/CFZwmdWYC0zEhPetCTHmmRcZSWGe1lAQhVykMIWt/e3IUjZakjbeQ4xgKYZgJkWZcBOQtzARGc6akTRCuiEbwJHJppASPXfu8U8rLvBOzBNgBD4ndNQRECNFkrgsY/WcXec3ePMcTT4ibm2LrmiZAvMj79hPCZjxE+/YSQLrsQAHrSg9ZFq5rH839/F1ADo5mDFU+TdDU8hdIdvfngGGByXD+9AzldnfePoE0AgTdsbA86hXWvnzJwTPLG0T21i3PVDqIoAKEyjS2Qk8JWT2hiB8Puc6cVv0Lqahxwmmj8jgTADqTK1md8fEF464rw8SWpAvRazqarDtJMMR9Lt7oAX09vZdqsgNUUCc6Edr/9Y/JAtDrXXMuOY1UG1F9zF+AZADfBn2vhNAicmbWJrEttBxtnx7PPKFSb0C8yui7F4+38iHGMfeAxAroE9PZD2Ypalx2eLRNomQ/OAOa0fwzgQ5EBkuvrYWIbZCvKtOkY77+N6sLLrNWrTd8in29UTbW5pl7xdNHJ0dtr33Sp80i+FLUmbTrp3iNlQtdrxtL1efPJI/PAYvctA0XwcgQ+GAB7WLpG/Vcd3uwI6PTY+xaittHtLsDNURtYReqk/+eIzAXZC0HzOOEY8vMwIKNG8WLDHnP/WwM6bARXLQ4gRTatr9T7bCFnIon+Q2qsBbPEAGlpq4qwAs4io7/oALa29WF6qn2YLwLwuuDda8HvPBd861pL2GmZQU97vNHpBTsusQtTuo32tADTALBqo2smauXumKvYQa7XzKK5+7Xm8BMf3nx6asqlfades7ur6e3pPxbAeQbjgFZuIn2BopDjoIhisflBQRWgvMzoLjvNWo400SKCcj3ivz9n/Nf3GP/jA4EMjHTRIT1b4KqDlsLHzY0r9yHHffbMAuy3mX4pCqpIYdtzpw2o6r+nkIdW48r1GGXcOYbiaSMxNlI6oGreJAhrwBrPvR1yrr0CApANndhTSpmBMmoBqK1CerDqmEVZlUnnj2DTIG5bHhUywfhiwH/7zoBff5rwn94bweuCfNXh8kmHZQJgcYhsWY996Nnn3pH9LEDjxmKG3ocvvSzbDEpwayFuOOw+T/UIjVvrnr5aFuXND/m1bdPwHffkls3h65QSuj6jtxJs12cL5qovd2uiEHTR+UFpXYBtT7PMyL0HabPUufnZHsDreg4vBoy/9Ry/+q2C8r+fg9cF6aLD2wtClwiyKrqRxVaLuB/tZQFCuswfl5FBySLgZkTac2rxnbxvgTv3IrMA45pBNGJcb9+QyS3OJBhFdBPO4Hn38QgzTwbedEvVXh65Rtg0YhwJ41i1sxRdjNhgEqj9ijkhdVlbv+Yo4bagiDbvRwVgxPqbz0EdYfidD8DrArro8LFOG2/kZUFZlcNH1BravyEkkDcGj4RCpJi2tLmwNDtrb9/I8VAS04bRTO44Np08G5cok3/jA6FtFcn0t7KZfSJts+oWGf1ljzIyUlfg9ntYFZDixSEEMLfj/r/dRGJb+juvDQRANN+qxI4/XI9YvfsCgGD47ho8MKhPuMjAmgH5YNDGFuajdWxPAajRLwGxwXILqZoBiJy7buZ8SDvIdmKzOpA6/xfwc73CCcoGIIoz+n+a/AvU7tquVwHwGYN82YFGhsRegaJzhB4I2lr4Bs56ntqtXD+k7/t5a42gZkmeofhX4n4EGNYF1//vGlL0ySZpoVYFAN4fAf5gCAtw7CLfHgNEQCcxeFmaDRhqvl01oth27qVUy7CLbo0DpObeYxyzHq9laAgipsjlNA2v0uFM7xfq83OftcZ/0aG76NBd9eifLdBf9dpUGk7c0cIKHmVzIZRs78DBdyhBxQiarGKSbYSQohEy3cp+9f4K19+5xur5GjxqwPeyAO+uBOX9tXY2Nc2t88WkHa877e0CNMIWa5nWkdt5sNVWzErZ7qsPJV3oxtrMfTwB7UgXCQVzJzuHYBqPRM7eJ3TLrjayWPu4mP9OywweOLqR4oiCaB9PBgDlThkqRcDrotAze8WhbjAJVLyBI4hGbHTlIlsKaxdQUYi9v+wgA+Pba8H/fVFQ3l9rVtQusl+jG0jLcHZ1aN8qAM4AN3vhQ0Pq2lVxS6F/3Fbz3pcEtr2rB5y2pNOya9XMWOxEFXhrcQv7VHbQ5rILS0MpgXICSJAWOkeYX4617m8CBzTa5YMavc8tMsoKypyiJqCtEsb/rdu3FNZ004XXjissGAz3SJkUCHsx4psvCvjb1xierzEOPFEIx2la5ruSbAs298sCYIyFImJMQEYT9ARAoOfxRsqT7W1nMtR6E9dg9aMUCJ8udorpHd/jRxiTwJRca3vrMzCz7epDiZAWirunZX1S+TSbMDfiHUveel40aA3MAo2VsA0s/fOUNJXU7Wg2O5HU5TGyKOpYPliDv3WN8q2XGD8YajNsc20A7VCSzaU9IAh0DSNYd3R9KMPUyVYTeXfl90M2B1Zq83dmATfSTc1iM3n5l0EFkTK5AOVFQr7obFposM4hAXXQ6uMi1ypk41Zcs9ysewaR+qTB6mq0vkMNXr2Wku1Y/TKr60mEYaWNnQlS2989wzD3QNDG1/H9Nbp3X2D4zjWGF4PtglLvx7gFNmsyVZImBjLaDwiKQND+oGY+LqZcp2FGm4HtikAOmmSV5gc1h0/ReOHno5rfdxXXjy7jGPMxn9/Z1vCWCXjjiHgJ14ta5Ka7BY6sDzKlZkpZ9yoog7WqzVDC1GnW4UFmd5E1GLTj55TQ5RSvqU3XSy4jY3h/jeHdlyoAL+tsQwtd15lKvdacUm2Vmy3r3i5gEvUAsV9uKh5pm2y5D0tA4iPHw3Zeg50bdUFzTgB0s6dIuZxBOdV4xEwplzCOMTKeOop9iMrASKuiMwSCGD1Toa8jZJ7upYaxPn1UVvZQyg2U0CL/3jp7rzpAxOILIImNfXW5bj8XEqTHXD8fQN1LjM8HDKvR5jDMFWUVGIGft47YAxpDtfEacBAQVPlfy70JKXGVqmC+BWCkGzzV8Ow4ml2zvtZoufce1ly/sQ7FLICIugBv645eBYrcyx9IWVbKUBSrPhruoZtTaIDli4o4V1JG+nOGrO8wCjWemVi/pOINPXgU/S7penVdRrfQ8XGIoJRqpB0eJgKGl4qK6ixETSvdGos9JmWXkjjtLQAqT1Pf549Ob+2KX0zXJUMECeUOViCCruYQ3qLm27YnlprymdXExGdPS9YOWkWjBosVuBTfoMzIS5s5XBXIoH5cx8H0BO3TRKL3wbrVojYyqYVQXFukm8uMtKqNpYl8x5MuNqEk0q4qf17hcD3qHobrUv0/IeIQ384mXEOeKsmcDpwN1DuofYGNp/f1J9uMocsgVuCG7jI82QYT03R3ouWB0bRfa6JpB7NaZFIguiW8dQt73yDnopXOUcfO2Ea/UpeQTeN80FRYGvQPgexF97DnpU0qExbULIYbodpM0un5C4PNmpaRMKxLzAg6EOfKqIimNog6DiOio+StkjTOGkQHCsDEYk6kqQIYPtHaLRK4EMaBt0re3udrhGviSKhZSNd2oGHu9I+ArP11+5HRSs3rUvcXLqnOEaxG8FpdQPaZfEMQfU9igml9UbCsVklRwSm/huibaO9SFzalhLzwPY+StZup4NFqDMyg+GPsRGLNc5diSkhcQFjC5aQyRUTdZR5mAXzRyW6oBXrcrBJiiodHRsojbtoM6dZxNOe/X/1EAur1bMhj0/whqPUKMTfmRaYyMtJKdwb3HUL9nmQUsJgFMPPs58x90l1GjdE6d6Cfq4+QkekluVvwDSoK20Ow7Y5S3cdIiiAPylBn7HA9YhimQqTb31jwaLUCsf2OAY4yN/nYMxqjeogFcKVvYqbaIt18LlG9CUpqgo40AADq9igigiLKOjv7ZmrQvMwyrUiqjHo+oxGtsDKtrKiidnZMEShzRh0odbMPS6dynwHSPYllrM8ZALA5dkYS2s+WHfBQwKvZRhW2l1HqMySJBop9RnfVq4X43krTQa5TULGZllsAA6LyKkWACN1LolpTVMXpgAPycfI0yCJdnkkjYCiXaogjasf6gMg4zKHP528nZr593ZmfZs8XdpdMVSPLwAB5b4MtmAeOo/YQ8mgbUhlETISIAQik6eXAKKkoejfW5wx4CCCiqF4e1W2UlXU4+Zi6B9hJMwWQTUNd9ejfWEJYdE3RPOcwMAnFIdIiIy8SpDByn/R5xzTRDfhKJtK0dn8XMMuaBAKJtKqaViJoemiSGG3kB1J7rvbhyaH/rQsC4txt3cKBH7++ifDY94sx1/02deZORGMAnyUAoI0etlsAJYKX8YR1OglUAZv5NvLtXGAaVADGF7pRRRnrNfqNUwZSn9E9XaB7cwkeigbWejSodawC44+6S4uMXAS5zyhrlZSqKNOUNOd0eBbQxgCThyW1n0l1geZp4qHko17tw5MjqncXFKUvJbcAvl/wRk3CFsPdhD6FtEKqzkgeOMx0jKR5vGFqXcfFam+Ej6pXf9IIZRHQWDCuRlAmjNdjfbCF3ZPndqlLyE965DeXWpCyGMStSgBejRtIy4xs1iL2ImyCYL/06IU4jBmIQE+A2DZ9usASN3ynYoBrv0l5EsTNANOFbwNDRcFoVpHExnWoAKkQUzwRtLmFwihM0ftPAfxIY31QHxrFDGb9TOw2Xg9XXU7ReGJca7dR8Xzeh1MtSPTdxPKTHunNJfJ31yaEVQLU2lL9NxFSl4GFNM8+9uPWNSBCYDUH7xFEbptRpWqyti5tTU/g8UXBiuv7I1snWtU0nraZqQeBsaiza3SGTAdEm/fdgojuP8QjgyPvh2q+RfEe2etOJpYRmDXZ0AvxphrGuGYA4+TJJsKWQQys5rxPOgT6xgLpqqvoI1U+tPwAQV2Y2BBtZ+tWvHXN3KXD6P1MAKppu1lxW4s+TwMjJRq3zOEdSHFzke616ykxaTsfQRPRG04C8Ib3b4+BTeYDEa2LMYoLIxswVPsKjFmlTij7AbjRtvZ61TIBhbS7CdCmDx9Vj0fWDTr0iUSgyw7fd5Hx7kWeTiJjqozti2RzlCknCGoXVbgA0i13ukWu1UBqf+3ps1vTGmZONLAqbUfM0RbA+T+7JqnB3vZhFAkh2Hnu+NosfnCTPuj1l7HUNvj4sfcdOGqQP+8S3hC65nrdFYxjmWAPjkuUdcHXf+UvTLhA/c1zgO4+ANQtenwH1cn9brEA1YLYQu/Y36+CKVNzK40UeGQ9tsjaHQRg5w37TTf+v33THYPcZs7QOBZnULE4Iip6QB6lAjxmdfzvaWf0VDHa64XjD+FiEA+zBqrlcTzh33/tJ+KmfvQf/qvdZsxjBxbYU65rGTsngEpA4G40dO/kPO0HmMC8u4RtIknztyQswLguTQ//HYJBiV8bL3mguTXIRxXMm4xAa/ECrCm1nl8c5Ck1JXTBmEeELikAAAMySURBVA7HBC92Ox3xNfItcDYRQwD4z//ir22s/r/+S3+GvvHP/soWrti5rZjlsReAySPyJnfsgbUHgVFGNV8rUNRtsnTiSyoVYcKmnHgalKyC5fX3Y/gfiym0sUjxfmvKp+ty87G3fMj9MMAWy9S9h3hkFOv41WcVUIPJ12vd59xzeu83/tbeifJv/9rfiM/+rj/29/TO3QVZAEk5AR4bZVQBt+95CknZawHUvBiM3e0CbrpBbwsnKhrxTtK0Q8kxfNkarAGNvz2M/6iOrH5SRAAGiqmyMxjQ1O6QlGmfsbe70jf/3c8SAPyeH/mluAkeGEmqm4KX7IGJBHjqaBZAf012zSZM6jfxb/i7rdYo0qAy2nZurWYcQX6uCbBiJ/PX7+Jh9qXf/rWfvXeGHkv/81d/Oq7th/7qPw6r4NkTgJ0L1ASByvw56tZS+LhtJtdeIfEuJq7aewxJPVdt597yseb9Q6Xg21//mw+WqcfSf/wHf54A4As/8Ss1PN5xl8Jm1ZiBnJvgQJpgcKp0EchstQDw92xIEze7jF1PDpscr4mS96HvfeP+Te850G/8k78c6/AH/tzXNqJoj2W6j33+70wkZb7L1pxuTask5CQ+f1fz/O5/+LlHpt6B/su//EkCgDe/UHntm1bTG6aBORMWvT1VszDWa0bhaQ5PROg7wnLZxVTLajViGOefA1pIcZsAtEGSW4EPI3B6JKXP/KlflquPX2wRgI5QRh1JGssmY/WhjrcIQPyaRu5Oj4x+ODTJbKT9dYvdpo2/mnQKkz8fGf6AqQMaXnl6t09ATU32sMPPPzL+4VNynK4truxK8zB5WdOEXRx+ZP55UOMCakGjrZPfSFtqBo+MPy8KAdDyKfSpGjcw/iaZeGT++VEX9QtycIACddvF7LmL+O439i9mPNLDoggCyUx/bOxwMz4HCPB//u1ff2T8mVM3Qey8EeSmAo69/L/+zc88Mv8jQFMcQKb/bqPv/PpHr4DyOlPtCIoiz268/7HQ8tGjmgXA8TyJv50eo/uPLk33CJp3f+CR+R912loLIDwy/nWhjTa3R8a/XjRxAY/Mf/2oAx4Z/zrT/wcRmzb1lKIWiQAAAABJRU5ErkJggg==",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050652]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #254992 11.1279739144206%, #3267A1 22.2559478294052%, #3B86AF 33.3839217443897%, #40A6BD 44.5118956593743%, #61BEC5 55.6398695743588%, #90CEC7 66.7678434893434%, #B8DEC9 77.8958174043279%, #DCEFCB 89.0237913193125%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000","12,000,000","14,000,000","16,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.111279739144206,"p_n":0.890237913193125},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050652]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And there you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
*Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.*

### Homework 2
*Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?*

## Spatial point patterns along networks

Have a look at this maps.  Can we say that the spatial point process is random here? Can you identify the areas where we have hotspots of crime? Think about these questions for a little while.

![](img/nonrandompoints.png)
(Source: Okabe and Sugihara, 2012)

Ok, so most likely you concluded that the process wasn't random, which it isn't in truth. It is also likely that you identified a number of potential hotspots?

Now, look at the two maps below:

![](img/randompoints.png)
(Source: Okabe and Sugihara, 2012)

We are representing the same spatial point pattern process in each of them. But we do have additional information in map B. We now know the street layout. The structure we observed in the map is accounted by the street layout. So what look like a non random spatial point process when we considered the full two dimensional space, now looks less random when we realise that the points can only appear alongside the linear network. 

This problem is common in criminal justice applications. Crime is geocoded alongside a linear street network. Even if in physical space crime can take place along a spatial continuum, once crime is geocoded it will only be possible alongside the street network used for the geocoding process. 

For exploring this kind of spatial point pattern processes along networks we need special techniques. Some researchers have developed special applications, such as [SANET](http://sanet.csis.u-tokyo.ac.jp/sub_en/manual.html). The `spatstat` package also provides some functionality for this kind of data structures.

In `spatstat` a point pattern on a linear network is represented by an object of class `lpp`. The functions `lpp()` and `as.lpp()` convert raw data into an object of class `lpp` (but they require a specification of the underlying network of lines, which is represented by an object of class `linnet`). For simplicity and illustration purposes we will use the `chicago` dataset that is distributed as part of the `spatstat` package. The `chicago` data is of class `lpp` and contains information on crime in an area of Chicago. 


```r
data("chicago")
plot(chicago)
```

<img src="06-week6_files/figure-html/unnamed-chunk-23-1.png" width="672" />

```r
summary(chicago)
```

```
## Multitype point pattern on linear network
## 116 points
## Linear network with 338 vertices and 503 lines
## Total length 31150.21 feet
## Average intensity 0.003723891 points per foot
## Types of points:
##          frequency proportion    intensity
## assault         21 0.18103450 0.0006741528
## burglary         5 0.04310345 0.0001605126
## cartheft         7 0.06034483 0.0002247176
## damage          35 0.30172410 0.0011235880
## robbery          4 0.03448276 0.0001284100
## theft           38 0.32758620 0.0012198950
## trespass         6 0.05172414 0.0001926151
## Enclosing window: rectangle = [0.3894, 1281.9863] x [153.1035, 1276.5602] 
## feet
```

An `lpp` object contains the linear network information, the spatial coordinates of the data points, and any number of columns of *marks* (in this case the mark is telling us the type of crime we are dealing with). It also contains the local coordinates `seg` and `tp` for the data points. The local coordinate `seg` is an integer identifying the particular street segment the data point is located in. A segment is each of the sections of a street between two vertices (marking the intersection with another segment). The local coordinate `tp` is a real number between 0 and 1 indicating the position of the point within the segement: `tp=0` corresponds to the first endpoint and `tp=1` correspond to the second endpoint.

The visual inspection of the map suggest that the intensity of crime along the network is not spatially uniform. Crime seems to be concentrated in particular segments. Like we did before we can estimate the density of data points along the networks using Kernel estimation (with the `density.lpp()` function), only now we only look at the street segments (rather than areas of the space that are outside the segments). The authors of the package are planning to introduce methods for automatic bandwidth selection but for now this is not possible, so we have to select a bandwidth. We could for example select 60 feet.


```r
d60 <- density.lpp(unmark(chicago), 60)
```

We use `unmark()` to ignore the fact the data points are marked (that is they provide marks with informtation, in this case about the crime type). By using `unmark()` in this example we will run density estimation for all crimes (rather than by type of crime). We can see the results below:
 

```r
plot(d60)
```

<img src="06-week6_files/figure-html/unnamed-chunk-25-1.png" width="672" />
 
If rather than colour you want to use the thickness of the street segment to identify hotpspots you would need to modify the code as shown below:
 

```r
plot(d60, style="width", adjust=2.5)
```

<img src="06-week6_files/figure-html/unnamed-chunk-26-1.png" width="672" />



This is very important for crime research, as offending will be constrained by all sorts of networks. Traditionally, hotspot analysis has been directed at crimes that are assumed to be situated across an infinite homogeneous environment (e.g., theft of motor vehicle), we must develop an increased awareness of perceptible geographical restrictions. There has been increasing recognition in recent years that the spatial existence of many phenomena is constrained by networks. 

These networks may be roads or rail networks, but there may be many more: 

> Environmental crimes could exist along waterways such as streams, canals, and rivers; and thefts of metal could occur along utility networks such as pipelines. Those
sociologically inclined might be able to offer more examples in the way of interpersonal networks. 

- [Tompson, Lisa, Henry Partridge, and Naomi Shepherd. "Hot routes: Developing a new technique for the spatial analysis of crime." Crime Mapping: A Journal of Research and Practice 1, no. 1 (2009): 77-96.](http://discovery.ucl.ac.uk/20057/)


While sometimes there may be issues with linking points to routes due to problems such as bad geocoding, as we had discusses in great detail in week 4, there are obivious advantages to considering crime as distributed along networks, rather than continuous space. 

