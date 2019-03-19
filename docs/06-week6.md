
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
##        sigma 
## 4.856702e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0005335797
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040704208 0.0008431932
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

<!--html_preserve--><div id="htmlwidget-d7b44f5873a2916aa83d" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-d7b44f5873a2916aa83d">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050652]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2D599A 17.058511410612%, #3C88B0 34.1170228221755%, #49B8C4 51.1755342337391%, #96D0C7 68.2340456453026%, #D0E9CA 85.2925570568661%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.17058511410612,"p_n":0.852925570568661},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050652]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

