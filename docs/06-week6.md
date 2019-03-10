
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
## 4.553158e-05
```

```r
bw.ppl(jitter_bur)
```

```
##        sigma 
## 0.0005722337
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040697211 0.0008446619
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

<!--html_preserve--><div id="htmlwidget-2999efc3fbf52dfae162" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-2999efc3fbf52dfae162">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050652]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #305F9D 19.4497664714423%, #3E96B6 38.8995329439485%, #6EC2C6 58.3492994164548%, #B7DEC9 77.799065888961%, #F6FBCC 97.2488323614672%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.194497664714423,"p_n":0.972488323614672},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050652]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

