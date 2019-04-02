
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
## 0.0004407553
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040715493 0.0008423847
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

<!--html_preserve--><div id="htmlwidget-88fae23b62e5a389d0bc" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-88fae23b62e5a389d0bc">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050652]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #264B93 11.6976164886922%, #336AA2 23.3952329779917%, #3C8BB1 35.0928494672911%, #41ADC0 46.7904659565905%, #6EC2C6 58.48808244589%, #9DD3C8 70.1856989351894%, #C5E4CA 81.8833154244888%, #EBF5CB 93.5809319137883%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000","12,000,000","14,000,000","16,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.116976164886922,"p_n":0.935809319137883},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050652]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

