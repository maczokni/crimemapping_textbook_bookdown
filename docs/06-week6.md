
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
## 4.249614e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0003785111
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040701841 0.0008415872
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

<!--html_preserve--><div id="htmlwidget-06514598783ab01539d5" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-06514598783ab01539d5">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #3265A0 21.6318496812633%, #40A2BC 43.2636993629639%, #89CBC7 64.8955490446646%, #D4EBCA 86.5273987263652%, #FFFFCC "],"labels":["5,000,000","10,000,000","15,000,000","20,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.216318496812633,"p_n":0.865273987263652},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

