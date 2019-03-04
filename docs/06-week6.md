
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



Then we use the as.owin function to define the window:


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

In the case of crime, as we have hinted some of this may be linked to the nature of crime itself. Hint: repeat victimisation. However, this pattern of duplication is fairly obvious across all crime categories in the police.uk website and.

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
##        sigma 
## 0.0004387607
```

```r
bw.scott(jitter_bur)
```

```
##      sigma.x      sigma.y 
## 0.0040708492 0.0008412524
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

<!--html_preserve--><div id="htmlwidget-5d022c33aad75dbd17de" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-5d022c33aad75dbd17de">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[53.441315,-2.225814],14,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addRasterImage","args":["data:image/png;base64,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",[[53.4514517532155,-2.25810740373087],[53.4390428798791,-2.21419214050653]],0.8,null,null,null]},{"method":"addLegend","args":[{"colors":["#0C2C84 , #264B93 11.6341634849983%, #336AA2 23.2683269705849%, #3C8AB1 34.9024904561715%, #41ACC0 46.5366539417581%, #6DC2C6 58.1708174273446%, #9BD3C8 69.8049809129312%, #C4E4CA 81.4391443985178%, #E9F5CB 93.0733078841044%, #FFFFCC "],"labels":["2,000,000","4,000,000","6,000,000","8,000,000","10,000,000","12,000,000","14,000,000","16,000,000"],"na_color":null,"na_label":"NA","opacity":0.5,"position":"topright","type":"numeric","title":"Burglary map","extra":{"p_1":0.116341634849983,"p_n":0.930733078841044},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[53.4390428798791,53.4514517532155],"lng":[-2.25810740373087,-2.21419214050653]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

