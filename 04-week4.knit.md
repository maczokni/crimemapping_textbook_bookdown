# Performing spatial operations in R

By now you have come a long way in terms of taking your spatial data, and visualising it using maps, and being able to present the values of a variable using thematic maps. You have had some practice in taking data which has a spatial component, and joining it to a shapefile, using the common column, in order to be able to visually demonstrate variation on something, such as the crime rate, across space. 

I hope that you are finding this to be really exciting stuff, and an opportunity to get yourselves accustomed to spatial data. If there is anything you are unsure about, or want to catch up on, please do not hesitate to revisit older material, and ask us questions about it. We build on each week acquiring knowledge umulatively, so don't let yourself get stuck anywhere down the line. But, if you're ready, today we will go a step further, and get your hands dirty with **spatial manipulation of your data**. 

Thus far, our data manipulation exercises were such that you might be familiar with, from your earlier exposures to data analysis. Linking datasets using a common column, calculating a new variable (new column) from values of existing variables, these are all tasks which you can perform on spatial or non-spatial data. However today we will explore some exercises in data manipulation which are specific to *spatial* data analysis. After this session you can truly say you are masters of spatial data manipulation. So let's get started with that!

The main objectives for this session are that by the end you will have:

- used **geocoding** methods to translate postcodes into geographic coordinates 
- made interactive point map with leaflet
- met a new format of spatial shape file called **geojson**
- subset points that are within a certain area using a **spatial operation**
- created new polygons by generating **buffers** around points
- counted the number of points that fall within a polygon (known as **points in polygon**)

These are all very useful tools for the spatial crime analyst, and we will hope to demonstrate this by working through an example project, where you would make use of all of these tools. 

Let's consider the assumption that licenced premises which serve alcohol are associated with increased crimes. We might have some hypotheses about why this may be. 

One theory might be that some of these serve as *crime attractors*. 

> Crime attractors are particular places, areas, neighbourhoods, districts which create well-known criminal opportunities to which strongly motivated, intending criminal offenders are attracted because of the known opportunities for particular types of crime. Examples might include bar districts; prostitution areas; drug markets; large shopping malls, particularly those near major public transit exchanges; large, insecure parking lots in business or commercial areas. The intending offender goes to rough bars looking for fights or other kinds of 'action'. 

On the other hand, it is possible that these areas are *crime generators*. 

> Crime generators are particular areas to which large numbers of people are attracted for reasons unrelated to any particular level of criminal motivation they might have or to any particular crime they might end up committing. Typical examples might include shopping precincts; entertainment districts; office concentrations; or sports stadiums. 

(If you are interested further in crime attractors vs crime generators I recommend a read of [Brantingham, P., & Brantingham, P. (1995). Criminality of place. European journal on criminal policy and research, 3(3), 5-26.](https://link.springer.com/content/pdf/10.1007/BF02242925.pdf))


It's possible that some licensed premises attract crimes, due to their reputation. However it is also possible that some of them are simply located in areas that are busy, attracts lots of people for lots of reasons, and crimes occurr as a result of an abundance of opportunities instead. 

In any case, what we want to do is to examine whether certain outlets have more crimes near them than others. We can do this using open data, some R code, and the spatial operations discussed above. So let's get to it!


## Getting some (more) data

Manchester City Council have an [Open Data Catalogue](http://open.manchester.gov.uk/open/homepage/3/manchester_open_data_catalogue) on their website, which you can use to browse through what sorts of data they release to the public. There are a some more and some less interesting data sets made available here. It's not quite as impressive as the open data from some of the cities in the US such as [New York](https://opendata.cityofnewyork.us/) or [Dallas ](https://www.dallasopendata.com/) but we'll take it. 


One interesting data set, especially for our questions about the different alcohol outlets is the [Licensed Premises](http://www.manchester.gov.uk/open/downloads/file/169/licensed_premises) data set. This details all the currently active licenced premises in Manchester. You can see there is a link to download now. 


As always, there are a few ways you can download this data set. On the manual side of things, you can simply right click on the download link from the website, save it to your computer, and read it in from there, by specifying the file path. Remember, if you save it in your *working directory*, then you just need to specify the file name, as the working directory folder is where R will first look for this file. If however you've saved this elsewhere, you will need to work out the file path. 


*Note* my favourite shortcut to finding the file path is to simply run the `file.choose()` function, and use the popup window to navigate to the file. When you open this file through the popup window, if you're not assigning this to an object, it will simply print out the filepath to your console window. Like so: 



![](img/file_choose_path.png)



You can then copy and paste this path to whatever fuction you are assigning it to, to read in your data. 



### Reading data in from the web


But, programmers are lazy, and the whole point of using code-based interfaces is that we get to avoid doing unneccessary work, like point-and-click downloading of files. And when data exists online in a suitable format, we can tell R to read the data in from the web directly, and cut out the middle man (that being ourseves in our pointing-and-clicking activity). 


How can we do this? Well think about what we do when we read in a file. We say, hello R, i would like to create a new object please and I will call this new object `my_data`. We do this by typing the name we are giving the object and the assignment function `<-`. Right? Then on the right hand side of the assignment function, there is the value that we are assigning the variable. So it could be a bit of text (such as when you're creating a `shp_name` object and you pass it the string `"path to my file"`), or it could be some function, for example when you read a csv file with the `read.csv()` function. 


So if we're reading a csv, we also need to specity *where* to read the csv from. Where should R look to find this data? This is where normally you are putting in the path to your file, right? Something like: 



```r
my_data <- read.csv("path to my file here")
```


Well what if your data does not live on your laptop or PC? Well, if there is a way that R can still access this data just by following a path, then this approach will still work! So how can we apply this to getting the Licensed Premises data from the web? 


You know when you right click on the link, and select "Save As..." or whatever you click on to save? You could, also select "Copy Link Address". This just copies the webpage where this data is stored. Give it a go! Copy the address, and then paste it into your browser. It will take you to a blank page where a forced download of the data will begin. So what if you pasted this into the `read.csv()` function? 



```r
my_data <- read.csv("www.data.com/the_data_i_want")
```



Well in this case, the my_data object would be assigned the value returned from the read.csv() function reading in the file from the url you provided. File path is no mysterious thing, file path is simply the *path* to the *file* you want to read. If this is a website, then so be it. 


So without dragging this on any further, let's read in the licensed premises data directly from the web: 



```r
lic_prem <- read.csv("http://www.manchester.gov.uk/open/download/downloads/id/169/licensed_premises.csv")
```


You can always check if this worked by looking to your global environment on the righ hand side and seeing if this 'lic_prem' object has appeared. If it has, you should see it has 65535 observations (rows), and 36 variables (columns). 


Let's have a look at what this data set looks like. You can use the `View()` function for this: 



```r
View(lic_prem)
```


We can see there are some interesting and perhaps less interesting columns in there. There are quite a lot of venues in this list as well. Let's think about subsetting them.  Let's say we're interested in city centre manchester. We can see that there is a column for postcodes. We know (from our local domain knowledge) That city centre postcodes are M1-M4. So let's start by subsetting the data to include these. 


### Subsetting using pattern matching

We could use spatial operations here, and geocode all the postcodes at this point, then use a spatial file of city centre to select only the points contained in this area. The only reason we're not doing this is because the geocode function takes a bit of time to geocode each address. It would only be about 10 - 15 minutes, but we don't want to leave you sitting around in the lab for this long, so instead we will try to subset the data using pattern matching in text. In particular we will be using the `grepl()` function. This function takes a **pattern** and looks for it in some text. If it finds the pattern, it returns TRUE, and if it does not, it returns FALSE. So you have to pass two parameters to the `grepl()` function, one of them being the pattern that you want it to look for, and the other being the object in which to search. 

So for example, if we have an object that is some text, and we want to find if it contains the letter "a", we would pass those inside the grepl() function, which would tell us TRUE (yes it's there) or FALSE (no it's not there):




```r
some_text <- "this is some text that has some letter 'a's"

grepl("a", some_text)
```

```
## [1] TRUE
```


You can see this returns TRUE, because there is at least one occurrence of the letter a. If there wasn't, we'd get FALSE: 



```r
some_text <- "this is some text tht hs some letters"

grepl("a", some_text)
```

```
## [1] FALSE
```


So we can use this, to select all the cases where we find the pattern "M1 " in the postcode. *NOTICE* the space in our search pattern. It's not "M1" it's "M1 ". Can you guess why?


Well, M1 will be found in M1 but also in M13, which is the University of Manchester's postcode, and not the part of city centre in which we are interested. 


So let's subset our data by creating a new object `city_centre_prems`, and using the piping (`%>%`) and `filter()` functions from the `dplyr` package:



```r
#remember to load dplyr package if you haven't already: 
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#then create the city_centre_prems object:
city_centre_prems <- lic_prem %>%
  filter(grepl("M1 ", POSTCODE) )
```


Now we only have 353 observations (see your global environment), which is a much more manageable number. 


### Geocoding from an address


Great OK so we have this list of licensed premises, and we have their address, which is clearly *some* sort of spatial information, but how would you put this on a map? 


Any ideas? 



We can use the `geocode()` function from the `ggmap` package to turn our addresses into mappable coordinates. `geocode()` geocodes a location (find latitude and longitude) using either (1) the [Data Science Toolkit](http://www.datasciencetoolkit.org/about) or (2) Google Maps. Note that when using Google you are agreeing to the Google Maps API Terms of Service at [https://developers.google.com/maps/terms](https://developers.google.com/maps/terms) (this link is also loaded when you load the ggmap package). One of the conditions of using Google API to geocode your points is that you *have to* display them using a Google basemap for example! Also, to use the Google Maps API you need to get an API key, which we won't be messing around with today. Instead, we will use the Data Science Toolkit approach. 


We can, at the most basic, geocode the postcode. This will put all the establisments to the centroid of the postcode. Postcodes are used in the United Kingdom as alphanumeric codes, that were devised by Royal Mail. A full postcode is known as a "postcode unit" and designates an area with a number of addresses or a single major delivery point. [You can search the Royal Mail for information on post codes here.](https://www.royalmail.com/business/search/google/POSTCODE). 

Here is a map of the postcode areas in Greater Manchester: 

![](img/750px-M_postcode_area_map.svg.png)


Now the centroid of the post code area represents the central point of the shapefile. For example, here you can see some polygons with their centroids illustrated by points: 


![](img/6R9sn.png)



This is not quite as precise as geocoding the actual address, and we will return to this in the homework, but let's just stick with this approach for now.



So `geocode()` will help us get the coordinates for the relevant post code centroid. First though, we have to specify in the address *where* our postcode is. Just like when you mail a postcard (people still do this, right?), you have to specify what country you want it to go to first, and then specify the postcode and address. So we will create a new variable (column) in our dataframe that pastes together the postcode with a suffix to specify our country, in this case ", UK". To do this, we use the `paste()` function. `paste()` just *pastes* together two or more text values, separating them by whatever separator you want. For example, if I wanted to have people's name displayed in different names I could use paste in this way: 


```r
firstname <- "Kurt"
lastname <- "Cobain"

#this way will paste lastname and firstname, and separate them by a comma
paste(lastname, firstname, sep = ",")
```

```
## [1] "Cobain,Kurt"
```

```r
#this way will paste firstname then lastname, and separate them by a space
paste(firstname, lastname, sep = " ")
```

```
## [1] "Kurt Cobain"
```


So in the same vein, we will now create a new column, call it `postcodes2` and use the paste function to put together the postcode with the ", UK" suffix. We want them to be separated by *nothing* so we use `sep = ""`. (note, if you are separating by nothing, you could use the `paste0()` function, you can read about this uising the help function if you want)


```r
city_centre_prems$postcodes2 <- paste(city_centre_prems$POSTCODE, ", UK", sep="")
```


Now we can use this new column to geocode our addresses. I mentioned that the `geocode()` function is part of the `ggmap` package. This means we have to load up this package to use this function: 



```r
library(ggmap)
```

```
## Loading required package: ggplot2
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```


Now we can use `geocode()`. To use this, you have to specify **what** you want to geocode (in this case our newly created column of postcode2) and also the method. I mentioned above you can use Google or the Data Science Toolkit. Google puts a restriction on the number of geocodes you can perform in a day, so I normally use dsk, but both have advantages and limitations, so read up on this if you're interested. But for now, I'm sticking to dsk. 

So let's create a new column, call it `postcode_coords`, use the geocode function to populate it with values: 






















































































