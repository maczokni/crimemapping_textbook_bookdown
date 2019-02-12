
# Week 3: Thematic maps in R



## Intro and recap

Last week we showed you fairly quickly how to create maps of spatial point patterns using `leaflet` and we also introduced the `tmap` package for thematic maps. Besides doing that we introduced a set of key concepts we hope you have continued studying over the week. We also discussed the *sf* package for storing spatial objects in R. 

This week we will carry on where we left the session last week. In the presentations last week we introduced various kind of thematic maps and in our lecture this week we discuss in detail issues with choropleth maps. So the focus of today's lab is going to be around thematic maps and some of the choices we discussed in our presentation last week and also this week. 

We will also introduce faceting and small multiples, which is a format for comparing the geographical distribution of different social phenomena. For this session we will be using the spatial object that you created last week and complement it with additional information from the census. So first of all you will have to rerun the code you used to create the "manchester_lsoa" `sf` object. Apart from doing so, you want to start your session loading the libraries you know for sure you will need:


```r
library(sf)
library(tmap)
library(dplyr)
```

You may not remember all of what you did to generate that file so let's not waste time and just cut and paste from below (but try to remember what each of the lines of code is doing and if you are not clear look at the notes from last week). Imagine you had to do all of this again by pointing and clicking in a graphical user interface rather than just sending the code to the console! As you will see time and time again, code in the end is a much more efficient way of talking to a computer.











































































