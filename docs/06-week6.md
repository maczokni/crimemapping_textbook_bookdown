
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
## Reading layer `OGRGeoJSON' from data source `https://raw.githubusercontent.com/RUMgroup/Spatial-data-in-R/master/rumgroup/data/wards.geojson' using driver `GeoJSON'
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
## X2 = 109.07, df = 5, p-value < 2.2e-16
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
##        sigma 
## 4.553158e-05
```

```r
bw.ppl(jitter_bur)
```

```
##       sigma 
## 0.000537835
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040703128 0.0008439943
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

<!--html_preserve--><div id="htmlwidget-455abaa4b227553c1265" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-455abaa4b227553c1265">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2D5A9A 17.3544682885175%, #3C8AB1 34.7089365779342%, #4EB9C4 52.0634048673509%, #9AD2C8 69.4178731567676%, #D5EBCA 86.7723414461843%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.173544682885175,"p_n":0.867723414461843},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

