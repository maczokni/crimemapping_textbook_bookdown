
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
#Read a geojson file with Manchester wards
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

So let's start using spatstat.The first thing we need to do is to transform our `sf` object into a `ppp` object which is how `spatstat` likes to store its point patterns. Unfortunately, spatstat and many other packages for analysis of spatial data precede sf, so the transformation is a bit awkard. Also before we do that, it is important to realise that a point pattern is defined as a series of events in a given area, or window, of observation. It is therefore extremely important to precisely define this window. In `spatstat` the function `owin()` is used to set the observation window. However, the standard function takes the coordinates of a rectangle or of a polygon from a matrix, and therefore it may be a bit tricky to use. Luckily the package `maptools` provides a way to transform a `SpatialPolygons` into an object of class `owin`, using the function `as.owin()`. 


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

<img src="06-week6_files/figure-html/unnamed-chunk-5-1.png" width="672" />



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

<img src="06-week6_files/figure-html/unnamed-chunk-9-1.png" width="672" />

In the case of crime, as we have hinted some of this may be linked to the nature of crime itself. Hint: repeat victimisation. However, this pattern of duplication is fairly obvious across all crime categories in the police.uk website and, although I have not explored this in detail, I strongly suspect it is a function of the anonimisation process used to create these maps. The coordinates provided in the open data are not the exact locations of crimes, but they come from a list of points generated for purposes of data publication. You can see the details [here](https://data.police.uk/about/#anonymisation). This process is likely inflating the amount of duplication we observe. So keep in mind when analysing and working with this data set that it is not the same as working with the real locations.

What to do about duplicates in spatial point pattern analysis is not always clear. You could simply delete the duplicates, but of course that may ignore issues such as repeat victimisation. You could also use jittering, which will add a small perturbation to the duplicate points so that they do not occupy the exact same space. Which again, may ignore things like repeat victimisation. Another alternative is to make each point "unique" and then attach the multiplicites of the points to the patterns as *marks*, as attributes of the points. Then you would need analytical techniques that take into account these marks.

If you were to be doing this for real you would want access to the real thing, not this public version of the data and then go for the latter solution suggested above. We don't have access to the source data, so for the sake of simplicity and so that we can illustrate how `spatstat` works we will instead add some jittering to the data. The first argument for the function is the object, `retry` asks whether we want the algorithm to have another go if the jittering places a point outside the window (we want this so that we don't loose points), and the `drop` argument is used to ensure we get a `ppp` object as a result of running this function (which we do).


```r
jitter_bur <- rjitter(bur_ppp, retry=TRUE, nsim=1, drop=TRUE)
plot(jitter_bur)
```

<img src="06-week6_files/figure-html/unnamed-chunk-10-1.png" width="672" />

Notice the difference with the original plot. Can you see how the circumferences do not overlap perfectly now?

## Inspecting our data with spatstat

This package supports all kind of exploratory point pattern analysis. One example of this is **quadrant counting**. One could divide the window of observation into quadrants and count the number of points into each of these quadrants. For example, if we want four quadrants along the X axis and 3 along the Y axis we could used those parameters in the `quadratcount()` function. Then we just use standard plotting functions from R base.


```r
Q <- quadratcount(jitter_bur, nx = 4, ny = 3)
plot(jitter_bur)
plot(Q, add = TRUE, cex = 2)
```

<img src="06-week6_files/figure-html/unnamed-chunk-11-1.png" width="672" />

In the video lectures for this week, Luc Anselin  introduced the notion of **complete spatial randomness** (CSR for short). When we look at a point pattern process the first step in the process is to ask whether it has been generated in a random manner. Under CSR, points are independent of each other and have the same propensity to be found at any location. We can generate data that conform to complete spatial randomness using the *rpoispp()* function. The r at the beginning is used to denote we are simulating data (you will see this is common in R) and we are using a Poisson point process, a good probability distribution for these purposes. Let's generate 223 points in a random manner:


```r
plot(rpoispp(223))
```

<img src="06-week6_files/figure-html/unnamed-chunk-12-1.png" width="672" />

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

Kernel density estimation maps are very popular among crime analysts. According to Chainey (2012), 9 out of 10 intelligence professionals prefer it1 to other techniques for hot spot analysis. As compared to visualisations of crime that relies on point maps or thematic maps of geographic administrative units (such as LSOAs), kernel density estimation maps are considered best for location, size, shape and orientation of the hotspot (Chainey, 2012). [Spencer Chainey and his colleagues (2008)](http://discovery.ucl.ac.uk/112873/1/PREPRINT_-_Chainey%2C_Tompson_%26_Uhlig_2008.pdf) have also suggested that this method produces some of the best prediction accuracy. The areas identified as hotspots by KDE (using historical data) tend to be the ones that better identify the areas that will have high levels of crime in the future. Yet, producing these maps (as with any map, really) requires you to take a number of decisions that will significantly affect the resulting product and the conveyed message. Like any other data visualisation technique they can be powerful, but they have to be handled with great care.

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

<img src="06-week6_files/figure-html/unnamed-chunk-14-1.png" width="672" />

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
## 0.0005520984
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040695837 0.0008407289
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

<img src="06-week6_files/figure-html/unnamed-chunk-16-1.png" width="672" />

Baddeley et (2016) suggest the use of the `bw.ppl()` algorithm because in their experience it tends to produce the more appropriate values when the pattern consists predominantly of tight clusters. But they also insist that if your purpose it to detect a single tight cluster in the midst of random noise then the `bw.diggle()` method seems to work best.

Apart from selecting the bandwidth we also need to specify the particular kernel we will use. In density estimation there are different types of kernel (as illustrated below):

![](img/kerneltypes.png)
Source: wikepedia

You can read more about kernel types in the Wikepedia [entry](https://en.wikipedia.org/wiki/Kernel_(statistics)). This relates to the type of kernel drawn around each point in the process of counting points around each point. The use of these functions will result in slightly different estimations. They relate to the way we weight points within the radius: *“The normal distribution weighs all points in the study area, though near points are weighted more highly than distant points.  The other four techniques use a circumscribed circle around the grid cell.    The uniform distribution weighs all points within the circle equally.  The quartic function weighs near points more than far points, but the fall off is gradual. The triangular function weighs near points more than far points within the circle, but the fall off is more rapid. Finally, the negative exponential weighs near points much more highly than far points within the circle and the decay is very rapid.”* (Levine, 2013: 10.10).

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

<img src="06-week6_files/figure-html/unnamed-chunk-17-1.png" width="672" />

When reading these maps you need to understand you are only looking at counts of crime in a smooth surface. Nothing more, nothing less. Unlike with choropleth maps we are not normalising the data. We are simply showing the areas where there is more crime, but we are not adjusting for anything (like number of people in the area). So, it is important you keep this in the back of your mind. As [this comic](https://xkcd.com/1138/) suggests you may end up reading too much into it if you don’t remember this. There are ways to produce density maps adjusting for a second variable, such as population size, but we do not have the time to cover this. 

There are also general considerations to keep in mind. Hot spots of crime are a simply a convenient perceptual construct. As Ned Levine (2013: 7.1) highlights *“Hot spots do not exist in reality, but are areas where there is sufficient clustering of certain activities (in this case, crime) such that they get labeled such. There is not a border around these incidents, but a gradient where people draw an imaginary line to indicate the location at which the hot spot starts.”*  Equally, there is not a unique solution to the identification of hot spots. Different techniques and algorithms will give you different answers. As Levine (2013: 7.7) emphasises: *“It would be very naive to expect that a single technique can reveal the existence of hot spots in a jurisdiction that are unequivocally clear. In most cases, analysts are not sure why there are hot spots in the first place. Until that is solved, it would be unreasonable to expect a mathematical or statistical routine to solve that problem.”* So, as with most data analysis exercises one has to try different approaches and use professional judgement to select a particular representation that may work best for a particular use. Equally, we should not reify what we produce and, instead, take the maps as a starting point for trying to understand the underlying patterns that are being revealed. Critically you want to try several different methods. You will be more persuaded a location is a hot spot if several methods for hot spot analysis point to the same location.

## Adding some context

Often it is convenient to use a basemap to provide context. In order to do that we first need to turn the image object generated by the `spatstat` package into a raster object, a more generic format for raster image used in R. 


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

<!--html_preserve--><div id="htmlwidget-b23fe5d4ff78702bc2d1" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-b23fe5d4ff78702bc2d1">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050652]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #2E5C9C 18.2149247262139%, #3D8FB3 36.4298494533835%, #5CBDC5 54.6447741805531%, #A6D7C8 72.8596989077226%, #E3F2CB 91.0746236348922%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.182149247262139,"p_n":0.910746236348922},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050652]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

And there you have it. Perhaps those familiar with Fallowfield have some guesses as to what may be going on there?

### Homework 1
*Ok, so see if you can do something like what we have done today, but for violent crime in the city centre. Produce the density estimates and then plot the density plot. In addition add a layer of points with the licenced premises we looked at last week.*

### Homework 2
*Produce a kernel density estimate for burglary across the whole of the city. Where is burglary more concentrated?*

##Spatial point patterns along networks

Have a look at this maps.  Can we say that the spatial point process is random here? Can you identify the areas where we have hotspots of crime? Think about these questions for a little while.

![](img/nonrandompoints.PNG)
(Source: Okabe and Sugihara, 2012)

Ok, so most likely you concluded that the process wasn't random, which it isn't in truth. It is also likely that you identified a number of potential hotspots?

Now, look at the two maps below:

![](img/randompoints.PNG)
(Source: Okabe and Sugihara, 2012)

We are representing the same spatial point pattern process in each of them. But we do have additional information in map B. We now know the street layout. The structure we observed in the map is accounted by the street layout. So what look like a non random spatial point process when we considered the full two dimensional space, now looks less random when we realise that the points can only appear alongside the linear network. 

This problem is common in criminal justice applications. Crime is geocoded alongside a linear street network. Even if in physical space crime can take place along a spatial continuum, once crime is geocoded it will only be possible alongside the street network used for the geocoding process. 

For exploring this kind of spatial point pattern processes along networks we need special techniques. Some researchers have developed special applications, such as [SANET](http://sanet.csis.u-tokyo.ac.jp/sub_en/manual.html). The `spatstat` package also provides some functionality for this kind of data structures.

In `spatstat` a point pattern on a linear network is represented by an object of class `lpp`. The functions `lpp()` and `as.lpp()` convert raw data into an object of class `lpp` (but they require a specification of the underlying network of lines, which is represented by an object of class `linnet`). For simplicity and illustration purposes we will use the `chicago` dataset that is distributed as part of the `spatstat` package. The `chicago` data is of class `lpp` and contains information on crime in an area of Chicago. 


```r
data("chicago")
plot(chicago)
```

<img src="06-week6_files/figure-html/unnamed-chunk-20-1.png" width="672" />

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

<img src="06-week6_files/figure-html/unnamed-chunk-22-1.png" width="672" />
 
If rather than colour you want to use the thickness of the street segment to identify hotpspots you would need to modify the code as shown below:
 

```r
plot(d60, style="width", adjust=2.5)
```

<img src="06-week6_files/figure-html/unnamed-chunk-23-1.png" width="672" />

