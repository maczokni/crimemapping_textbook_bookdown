#Spatial regression models


## Introduction

Last week we provided you with an introduction to regression analysis with R. the data we used had a spatial component. We were modelling the geographical distribution of homicide across US counties. However, we did not incorporated this spatial component into our models. As we have explained througout the semester criminal events often cluster geographically in space. So if we want to develop a regression model for crime we may have to recognise this spatial component. Remember as well, from last week, that regression models assume independence between the observations. That is, a regression model is formally assuming that what happens in area Xi is not in any way related (it is independent) of what happens in area Xii. But if those two areas are adjacent in geographical space we know that there is a good chance that this assumption may be violated. In previous weeks we covered formal tests for spatial autocorrelation, which allow us to test whether this assumption is met or not. So before we fit a regression model with spatial data we need to explore the issue of autocorrelation. We already know how to do this. In this session, we will examine the data from last week, explore whether autocorrelation is an issue, and then introduce models that allow us to take into account spatial autocorrelation. We will see that there are two basic ways of adjusting for spatial autocorrelation: through a spatial lag model or through a spatial error model.

Before we do any of this, we need to load the libraries we will use today:


```r
library(sf)
```

```
## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
```

```r
library(tmap)
library(sp)
library(spdep)
```

```
## Loading required package: Matrix
```

```
## Loading required package: spData
```

```
## To access larger datasets in this package, install the spDataLarge
## package with: `install.packages('spDataLarge',
## repos='https://nowosad.github.io/drat/', type='source'))`
```


Then we will bring back the data from last week:


```r
##R in Windows have some problems with https addresses, that's why we need to do this first:
urlfile<-'https://s3.amazonaws.com/geoda/data/ncovr.zip'
download.file(urlfile, 'ncovr.zip')
#Let's unzip and create a new directory (ncovr) in our working directory to place the files
unzip('ncovr.zip', exdir = 'ncovr')
```

Last week we did not treated the data as spatial and, consequently, relied on the csv file. But notice that in the unzip ncovr file there is also a shapefile that we can load as a spatial object into R:


```r
shp_name <- "ncovr/ncovr/NAT.shp"
ncovr_sf <- st_read(shp_name)
```

```
## Reading layer `NAT' from data source `/Users/reka/Dropbox (The University of Manchester)/crimemapping_textbook_bookdown/ncovr/ncovr/NAT.shp' using driver `ESRI Shapefile'
## Simple feature collection with 3085 features and 69 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: -124.7314 ymin: 24.95597 xmax: -66.96985 ymax: 49.37173
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

We can indeed represent our variable of interest using a choropleth map.


```r
current_style <- tmap_style("col_blind")
```

```
## tmap style set to "col_blind"
```

```
## other available styles are: "white", "gray", "natural", "cobalt", "albatross", "beaver", "bw", "classic", "watercolor"
```

```r
tm_shape(ncovr_sf) + 
  tm_fill("HR90", title = "Homicide Rate (Quantiles)", style="quantile", palette = "Reds") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Homicide Rate across US Counties, 1990", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)
```

<img src="09-week9_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Do you think there is spatial patterning to homicide?

## Looking at the residuals and testing for spatial autocorrelation in regression

Residuals, as we have explained, give you an idea of the distance between our observed Y values and the predicted Y values. So in essence they are deviations of reality from your model. Your regression line or hyperplane is optimised to be the one that best represent your data if those assumptions are met. Residuals are very helpful to diagnose, then, whether your model is a good representation of reality or not. Most diagnostics of the assumptions for OLS regression, which is if you remember from last year the technique we are using, rely on exploring the residuals. 

In order to explore the residuals we need to fit our model first. Let's look at one of the models from last week.


```r
fit_1 <- lm(HR90 ~ RD90 + SOUTH + DV90 + MA90 + PS90 +UE90, data=ncovr_sf)
```

Now that we have fitted the model we can extract the residuals. If you look at the fit_1 object in your RStudio environment or if you run the str() function to look inside this object you will see that this object is a list with differet elements, one of which is the residuals. An element of this object then includes the residual for each of your observations (the difference between the observed value and the value predicted by your model). We can extract the residuals using the residuals() function and add them to our spatial data set.


```r
ncovr_sf$res_fit1 <- residuals(fit_1)
```

If you now look at the dataset you will see that there is a new variable with the residuals. In those cases where the residual is negative this is telling us that the observed value is lower than the predicted (that is, our model is overpredicting the level of homicide for that observation) when the residual is positive the observed value is higher than the predicted (that is, our model is underpredicting the level of homicide for that observation).

We could also extract the predicted values if we wanted. We would use the fitted() function.


```r
ncovr_sf$fitted_fit1 <- fitted(fit_1)
```

Now look at the second county in the dataset. It has a homice rate in 1990 of 15.88. This is the observed value. If we look at the new column we have created (fitted_fit1), our model predicts a homicide rate of 2.41. That is, knowing the level unemployment, whether the county is North or South, the level of resource deprivation, etc., we are predicting a homicide rate of 2.41. Now, this is lower than the observed value, so our model is underpredicting the level of homicide in this case. If you observed the residual you will see that it has a value of 13.46, which is simply the difference between the observed and the predicted value.

With spatial data one useful thing to do is to look at any spatial patterning in the distribution of the residuals. Notice that the residuals are the difference between the observed values for homicide and the predicted values for homicide, so you want your residual to NOT display any spatial patterning. If, on  the  other  hand,  your  model  display  a  patterning  in  the  areas of  the  study  region  where  it performs  predicts  badly,  then  you  may have  a  problem.  This  is  telling  your  model  is  not  a  good representation  of  the  social  phenomena  you  are  studying  across  the full  study  area:  there  is systematically more distortion in some areas than in others. 

We are going to produce a choropleth map for the residuals, but we will use a common classification method we haven't covered yet: standard deviations. Standard deviation is a statistical technique type of map based on how much the data differs from the mean. You measure the mean and standard deviation for your data. Then, each standard deviation becomes a class in your choropleth maps.

In order to do that we will compute the mean and the standard deviation for the variable we want to plot and break the variable according to these values. The following code creates a new variable in which we will express the residuals in terms of standard deviations away from the mean. So, for each observation, we substract the mean and divide by the standard deviation. 


```r
ncovr_sf$sd_breaks <- (ncovr_sf$res_fit1 - mean(ncovr_sf$res_fit1))/sd(ncovr_sf$res_fit1)
summary(ncovr_sf$sd_breaks)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -3.5370 -0.5238 -0.1404  0.0000  0.3314 13.7407
```

Next we use a new style, fixed, within the tm_fill function. When we break the variable into classes using the fixed argument we need to specify the boundaries of the classes. We do this using the breaks argument. In this case we are going to ask R to create 7 classes based on standard deviations away from the mean. 


```r
my_breaks <- c(-14,-3,-2,-1,1,2,3,14)

tm_shape(ncovr_sf) + 
  tm_fill("sd_breaks", title = "Residuals", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)
```

```
## Variable "sd_breaks" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.
```

<img src="09-week9_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Notice  the  spatial  patterning  of  areas  of  over-prediction  (negative residuals,  or  blue  tones)  and under-prediction (positive residuals, or brown tones). This visual inspection of the residuals is telling you that spatial autocorrelation may be present here. This, however, would require a more formal test.

Remember from week 6 that in order to do this first we need to turn our sf object into a sp class object and then create the spatial weight matrix. If the code below and what it does is not clear to you, revise the notes from week 6 -when we first introduced it.


```r
#We coerce the sf object into a new sp object
ncovr_sp <- as(ncovr_sf, "Spatial")
#Then we create a list of neighbours using the Queen criteria
w <- poly2nb(ncovr_sp, row.names=ncovr_sp$FIPSNO)
summary(w)
```

```
## Neighbour list object:
## Number of regions: 3085 
## Number of nonzero links: 18168 
## Percentage nonzero weights: 0.190896 
## Average number of links: 5.889141 
## Link number distribution:
## 
##    1    2    3    4    5    6    7    8    9   10   11   13   14 
##   24   36   91  281  620 1037  704  227   50   11    2    1    1 
## 24 least connected regions:
## 53009 53029 25001 44005 36103 51840 51660 6041 51790 51820 51540 51560 6075 51580 51530 51131 51115 51770 51720 51690 51590 27031 26083 55029 with 1 link
## 1 most connected region:
## 49037 with 14 links
```

This should give you an idea of the distribution of connectedness across the data, with counties having on average nearly 6 neighbours. Now we can generate the row standardise spatial weight matrix and the Moran Scatterplot.


```r
wm <- nb2mat(w, style='B')
rwm <- mat2listw(wm, style='W')
```

We obtain the Moran's test for regression residuals using the function lm.morantest() as below. It is important to realize that the Moran's I test statistic for residual spatial autocorrelation takes into account the fact that the variable under consideration is a residual, computed from a regression. The usual Moran's I test statistic does not. It is therefore incorrect to simply apply a Moran's I test to the residuals from the regression without correcting for the fact that these are residuals. 


```r
lm.morantest(fit_1, rwm, alternative="two.sided")
```

```
## 
## 	Global Moran I for regression residuals
## 
## data:  
## model: lm(formula = HR90 ~ RD90 + SOUTH + DV90 + MA90 + PS90 +
## UE90, data = ncovr_sf)
## weights: rwm
## 
## Moran I statistic standard deviate = 10.321, p-value < 2.2e-16
## alternative hypothesis: two.sided
## sample estimates:
## Observed Moran I      Expectation         Variance 
##     0.1093062514    -0.0014498532     0.0001151682
```

You will notice we obtain a statistically significant value for Moran's I. The value of the Moran's I test is not too high, but we still need to keep it in mind. If we diagnose that spatial autocorrelation is an issue, that is, that the errors (the residuals) are related systematically among themselves, then we have a problem and need to use a more appropriate approach: a spatial regression model.

## What to do now?

If the test is significant (as in this case), then we possibly need 
to  think  of  a  more  suitable  model  to  represent  our  data:  a  spatial  regression  model.  Remember spatial  dependence  means  that  there  will  be  areas  of  spatial  clustering  for  the  residuals  in  our regression  model.  So our  predicted  line  (or  hyperplane)  will systematically  under-predict  or  over-predict in areas that are close to each other. That's not good. We want a better model that does not 
display any spatial clustering in the residuals.  
 
There are  two  general  ways  of  incorporating  spatial  dependence  in  a regression model, through what we called a **spatial error model** or by means of a **spatially lagged model**. There are *spdep* functions that provides us with some tools to help us make a decision as to which of these two is most appropriate: the **Lagrange Multiplier tests**.  
 
The difference between these two models is both technical and conceptual. The **spatial error model** treats the spatial autocorrelation as a nuisance that needs to be dealt with.  A spatial error model basically implies that the: 
 
*spatial  dependence  observed  in  our  data  does  not  reflect  a  truly spatial  process,  but  merely  the geographical clustering of the sources of the behaviour of interest. For example, citizens in adjoining neighbohoods may favour the same (political) candidate not because they talk to their neighbors, but because citizens  with  similar  incomes  tend to  cluster  geographically,  and  income  also  predicts  vote  choice. Such spatial dependence can be termed attributional dependence"* (Darmofal, 2015: 4)
 
The **spatially lagged model**, on the other hand, incorporates spatial dependence explicitly by adding a  "spatially  lagged"  variable  *y*  on  the  right  hand  side  of  our  regression  equation.  Its  distinctive 
characteristic  is  that  it  includes  a  spatially  lagged  "dependent"  variable  among  the  explanatory factors. It's basically explicitly saying that the values of y in the neighbouring areas of observation *n~i* is an important predictor of y on each individual area *n~i* . This is one way of saying that the spatial dependence  may  be  produced  by  a  spatial  process  such  as  the  diffusion  of  behaviour  between neighboring units:  
 
*"If so the behaviour is likely to be highly social in nature, and understanding the interactions between interdependent units is critical to understanding the behaviour in question. For example, citizens may discuss politics across adjoining neighbours such that an increase in support for a candidate in one neighbourhood  directly  leads  to  an  increase  in  support  for  the  candidate  in    adjoining neighbourhoods"* (Darmofal, 2015: 4)

## Spatial Regimes

Before  we  proceed  to  a  more  detailed  description  of  these  two models  it  is  important  that  we examine another aspect of our model that also links to geography. Remember that when we brought up our data into R, we decided to test for the presence of an interaction. We looked at whether the role of unemployment was different in Southern and Northern states. We found that this interaction was indeed significant. Unemployment had a more significant effect in Southern than in Northern states.  This was  particularly  obvious  during  the  1970s,  when  unemployment  did not  affected homicide rates in the Northern states, but it did led to a decrease in homicide in the Southern states. 

We could have attempted to test other interaction effects between some of our other predictors and their geographical location in the South or the North. But we did not. 
 
If you have read the Ballen et al. (2001) paper that we are replicating in last week and this week lab, you will have noticed that they decided that they needed to run separate models for the South and the  North.  This kind of  situation,  where  sub-regions,  seem  to  display  different  patterns often  is alluded  with  the  name  of  spatial  regimes.  In  the  context of  regression  analysis,  spatial  regimes relates to the possibility that we may need to split our data into two (or more sub-regions) in order to run our models, because we presume that the relationship of the predictors to the outcome may play out differently in these sub-regions (spatial regimes). 
 
So  how  can  we  assess  whether  this  is  an  issue  in  our  data?  As with  many  other  diagnostics  of regression,  you  may  want  to  start  by  looking  at  your  residuals.  Look  at  the  residual  map  we produced earlier. Do you think that the residuals look different in the South and in the North? If the pattern is not clear to you, you may want to run other forms of visualisation. 
 

```r
library(ggplot2)

ggplot(ncovr_sf, aes(x = res_fit1, colour = as.factor(SOUTH))) + 
  geom_density() 
```

<img src="09-week9_files/figure-html/unnamed-chunk-13-1.png" width="672" />

### HOMEWORK 1

*What do you see in this plot? And, critically, what does it mean? What is this telling you about the predicted values that result from our model? (Remember what a residual is: the difference between the observed values and the predicted values).* 

There are formal tests that one can use to further explore these issues. The paper by Bollen et al. (2001) mentions them (Chow tests). But those are beyond the scope of this course. Sufficient to say that,  as  Bollen  et  al.  (2001),  we  are  going  to  split  our  analysis  and  run  them  separately  for  the Southern and the Northern states. We have covered the filter() function from dplyr to split datasets based on values of a variable. But to split sf objects it is better to rely on the more generic subset function, since filter() doesn't accommodate well the column with the geographic information that sf provides.


```r
ncovr_s_sf <- subset(ncovr_sf, SOUTH == 1)
ncovr_n_sf <- subset(ncovr_sf, SOUTH == 0)
```


## Lagrange multipliers

The Moran's I test statistic has high power against a range of spatial alternatives. However, it does not provide much help in terms of which alternative model would be most appropriate. The Lagrange Multiplier test statistics do allow a distinction between spatial error models and spatial lag models. 

In order to practice their computation and interpretation, let's run two separate OLS regression models (one for the South and one for the North), using the same predictors as we used last week and, first, focusing on homicide in the northern counties in the earliest year for which we have data (1960). We have split the data in two, so that means that before we do this we need to create new files for the spatial weight matrix: in particular we will create one using first order queen criteria.  


```r
#We coerce the sf object into a new sp object
ncovr_n_sp <- as(ncovr_n_sf, "Spatial")
#Then we create a list of neighbours using the Queen criteria
w_n <- poly2nb(ncovr_n_sp, row.names=ncovr_n_sp$FIPSNO)
wm_n <- nb2mat(w_n, style='B')
rwm_n <- mat2listw(wm_n, style='W')

fit_2 <- lm(HR60 ~ RD60 + DV60 + MA60 + PS60 +UE60, data=ncovr_n_sf)
```

First  look  at  the  Moran's  I. 


```r
lm.morantest(fit_2, rwm_n, alternative="two.sided")
```

```
## 
## 	Global Moran I for regression residuals
## 
## data:  
## model: lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data
## = ncovr_n_sf)
## weights: rwm_n
## 
## Moran I statistic standard deviate = 2.84, p-value = 0.004511
## alternative hypothesis: two.sided
## sample estimates:
## Observed Moran I      Expectation         Variance 
##     0.0394654814    -0.0022539192     0.0002157889
```

The  p  (probability)  value  associated  with  this  Moran's  I  is  below our standard threshold. So we will say that we have an issue with spatial autocorrelation that we need to deal with. OLS regression won't do. In order to decide whether to fit a spatial error or a spatially lagged model we need to run the Lagrange Multipliers. 

Both Lagrange multiplier tests (for the error and the lagged models, LMerr and LMlag respectively), as well as their robust forms (RLMerr and RLMLag, also respectively) are included in the lm.LMtests function. Again, a regression object and a spatial listw object must be passed as arguments. In addition, the tests must be specified as a character vector (the default is only LMerror), using the c( ) operator (concatenate), as illustrated below. 


```r
lm.LMtests(fit_2, rwm_n, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
```

```
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data
## = ncovr_n_sf)
## weights: rwm_n
## 
## LMerr = 7.1326, df = 1, p-value = 0.007569
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data
## = ncovr_n_sf)
## weights: rwm_n
## 
## LMlag = 16.595, df = 1, p-value = 4.627e-05
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data
## = ncovr_n_sf)
## weights: rwm_n
## 
## RLMerr = 8.2906, df = 1, p-value = 0.003985
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data
## = ncovr_n_sf)
## weights: rwm_n
## 
## RLMlag = 17.753, df = 1, p-value = 2.515e-05
## 
## 
## 	Lagrange multiplier diagnostics for spatial dependence
## 
## data:  
## model: lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data
## = ncovr_n_sf)
## weights: rwm_n
## 
## SARMA = 24.886, df = 2, p-value = 3.946e-06
```

How do we interpret  the  Lagrange  Multipliers?  First  we  look  at  the standard  ones (LMerr and LMlag).  **If  both**  are  below the .05 level this means we need to have a look at the robust version of these tests (Robust LM). If the non-robust version is not significant, the mathematical properties of the robust tests may not hold, so we don't look at them in those scenarios. It is fairly common to find that both the lag (LMlag) and the error (LMerr) non-robust LM are significant. If only one of them were, problem solved we would choose a spatial lag or a spatial error model according to this (i.e., if the lag M was significant and the error LM was not we would run a spatial lag model or viceversa). Here we see that both are significant, thus, we need to inspect their robust versions.

Yet we look at the robust Lagrange multipliers (RLMlag and RLMerr) and encounter that both are significant again. What do we do? Luc Anselin (2008: 199-200) proposes the following criteria: 

*"When both LM test statistics reject the null hypothesis, proceed to the bottom part of the graph and consider the Robust forms of the test statistics. Typically, only one of them will be significant, or one will be orders of magnitude more significant than the other (e.g., p < 0.00000 compared to p < 0.03). In  that  case  the  decision  is  simple:  estimate  the  spatial  regression  model  matching  the  (most) significant" robust "statistic. In the rare instance that both would be highly significant, go with the model with the largest value for the test statistic. However, in this situation, some caution is needed, since there may be other sources of misspecification. One obvious action to take is to consider the results for different spatial weight and/or change the basic (i.e., not the spatial part) specification of the model. there are also rare instances where neither of the Robust LM test statistics are significant. In those cases, more serious specification problems are likely present and those should be addressed first".*

By other specification errors Prof. Anselin refers to problems with some of the other assumptions of regression that we covered last week. 
 
Ok, so let's go back to our results and keep this criteria in mind. Both Robust LM are significant, but we can see that the test for the lag model is several orders more significant than for the error model (p < 0.0000251 vs p < 0.004). Notice as well that the test value is higher for the lag model (18 versus 8). In this case we would say that the Lagrange Multiplier tests suggest we estimate a spatial lag model rather than a spatial error model.

### HOMEWORK 2
*???Run  a  OLS regression  model  for  homicide  rate  in  1980  for  the  Southern  states  using  the covariates we have been using so far and make sure that you ask for the spatial dependence tests and the Lagrange Multipliers (attach the results). What model do you think need to be estimated in this case? The spatial lag or the spatial error model?*

## Fitting and interpreting a spatially lagged model

Maximum Likelihood (ML) estimation of the spatial lag model is carried out with the lagsarlm() function. The required arguments are a regression "formula", a data set and a listw spatial weights object. The default method uses Ord's eigenvalue decomposition of the spatial weights matrix. 


```r
fit_2_lag <- lagsarlm(HR60 ~ RD60 + DV60 + MA60 + PS60 +UE60, data=ncovr_n_sf, rwm_n)
summary(fit_2_lag)
```

```
## 
## Call:
## lagsarlm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data = ncovr_n_sf, 
##     listw = rwm_n)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -11.86076  -1.54308  -0.55531   0.76832  28.30435 
## 
## Type: lag 
## Coefficients: (asymptotic standard errors) 
##              Estimate Std. Error z value  Pr(>|z|)
## (Intercept)  5.802071   0.677236  8.5673 < 2.2e-16
## RD60         1.734746   0.157771 10.9954 < 2.2e-16
## DV60         1.002757   0.077951 12.8640 < 2.2e-16
## MA60        -0.179328   0.020024 -8.9557 < 2.2e-16
## PS60         0.382956   0.077193  4.9610 7.014e-07
## UE60         0.087963   0.030109  2.9215  0.003483
## 
## Rho: 0.13749, LR test value: 15.139, p-value: 9.9874e-05
## Asymptotic standard error: 0.035631
##     z-value: 3.8587, p-value: 0.000114
## Wald statistic: 14.89, p-value: 0.000114
## 
## Log likelihood: -4273.43 for lag model
## ML residual variance (sigma squared): 9.6542, (sigma: 3.1071)
## Number of observations: 1673 
## Number of parameters estimated: 8 
## AIC: 8562.9, (AIC for lm: 8576)
## LM test for residual autocorrelation
## test value: 10.659, p-value: 0.0010954
```

As expected, the spatial autoregressive parameter (Rho) is highly significant, as indicated by the p-value of 0.000114 on an asymptotic t-test (based on the asymptotic variance matrix). The Likelihood Ratio test (LR) on this parameter is also highly significant (p value 9.9874e-05).

Just to reiterate, we run the spatial lag model not only because the Lagrange Multiplier suggests this may be appropriate, but also because in this case we believe that the values of y in one county, i, are directly influenced by the values of y that exist in the "neighbors" of i. This is an influence that goes beyond  other  explanatory  variables  that  are specific  to  i.  Remember  what  we  said  earlier  in  the spatial lag model we are simply adding as an additional explanatory variable the values of y in the surrounding area. What we mean by "surrounding" will be defined by our spatial weight matrix. It's important to emphasise that one has to think very carefully and explore appropriate definitions of "surrounding" (as we discussed, though just superficially, in the section on spatial clustering a few weeks ago). We are using here the first order queen criteria, but in real practice you would need to explore whether this is the best definition and one that makes theoretical sense. 
 
How  do  you  interpret  these  results?  First  you  need  to  look  at the  general measures  of  fit  of  the model. I know what you are thinking. Look at the R Square and compare them, right? Well, don't. This R Square is not a real R Square, but a pseudo-R Square and therefore is not comparable to the one  we  obtain  from  the  OLS  regression  model. Instead  we  can look at the Akaike Information Criterion. We see that the lag model has a AIC of 8562.9 whereas the linear model with no lags has a AIC of 8576, so this is telling us there is a better fit when we include the spatial lag.

In our spatial lag model you will notice that there is a new term Rho. What is this? This is our spatial lag. It is a variable that measures the homicide rate in the counties that are defined as surrounding each county in our spatial weight matrix. We are simply using this variable as an additional explanatory variable to our model, so that we can appropriately take into account  the  spatial  clustering  detected  by our  Moran's  I  test.  You will  notice  that  the  estimated coefficient  for  this  termis  both  positive  and  statistically  significant.  In  other  words, when  the homicide rate in surrounding areas increases, so does the homicide rate in each country, even when we adjust for the other explanatory variables in our model. The fact the lag is significant adds further evidence that this is a better model than the OLS regression specification.

You also see at the bottom further tests for spatial dependence, a likelihood ratio test. This is not a test for residual spatial autocorrelation after we introduce our spatial lag. What you want is for this test to be significant because in essence is further evidence that the spatial lag model is a good fit. 
 
How about the coefficients? It may be tempting to look at the regression coefficients for the other explanatory variables for the original OLS model and compare them to those in the spatial lag model. But  you  should  be  careful  when  doing  this.  Their  meaning  now  has  changed:

"Interpreting  the substantive  effects  of  each  predictor  in  a spatial lag  model  is  much  more  complex  than  in  a nonspatial model (or in a spatial error model) because of the presence of the spatial multiplier that links  the  independent  variables  to  the  dependent.  In  the nonspatial model,  it  does  not  matter which unit is experiencing the change on the independent variable. The effect" in the dependent variable "of a change" in the value of an independent variable "is constant across all observations" (Darmofal, 2015: 107). Remember we say, when interpreting a regression coefficient for variable X~i,  that they indicate how much Y goes up or down for every one unit increase in X~i  when holding all other variables in the model constant. In our example, for the nonspatial model this effect is the same  for  every  county  in  our  dataset.  But  in  the  spatial  lag  model  things  are  not  the  same.  We cannot interpret the regression coefficients for the substantive predictors in the same way because the "substantive effects of the independent variables vary by observation as a result of the different neighbors for each unit in the data" (Darmofal, 2015: 107).

In  the  OLS  regression  model,  the  coefficients  for  any  of  the  explanatory  variables  measure  the absolute impact of these variables. It is a simpler scenario. We look at the effect of X in Y within each county. So X in county A affects Y in count A. In the spatial lag model there are two components to how  X  affect  Y.  X  affects  Y  within  each  county  directly  but  remember  we  are  also  including  the spatial, the measure of Y in the surrounding counties (call them B, C, and D). So our model includes not only the effect of X in county A in the level of Y in county A. By virtues of including the spatial lag (a measure of Y in county B, C and D) we are indirectly incorporating as well the effects that X has on Y in  counties  B,  C,  and  D.  So  the  effect  of  a  covariate  (independent  variable)  is  the  sum  of  two particular effects: a direct, local effect of the covariate in that unit, and an indirect, spillover effect due to the spatial lag.

In, other words, in the spatial lag model, the coefficients only focus on the "short-run impact" of x~i on y~i , rather than the net effect. As Ward and Gleditsch (2008) explain "Since the value of  y~i  will influence the level of" homicide "in other" counties y~j  and these y~j , in turn, feedback on to y~i , we need to take into account the additional effects that the short impact of x~i  exerts on y~i  through its impact on the level of" homicide "in other" counties. You can still read the coefficients in the same way but need to keep in mind that they are not measuring the net effect. Part of their effect will be captured  by  the  spatial  lag.  Yet,  you  may  still  want  to  have  a  look  at  whether  things  change dramatically, particularly in terms of their significance (which is not the case in this example).

## Fitting an interpreting a spatial error model

We saw (well you should have seen!) that for the case of homicide in the 80s for Southern states the spatial error model was more appropriate when using the 1 st  order contiguity queen criteria. In this case then, we need to run a spatial error model.

Maximum likelihood estimation of the spatial error model is similar to the lag procedure and implemented in the errorsarlm() function. Again, the formula, data set and a listw spatial weights object must be specified, as illustrated below.


```r
#We coerce the sf object into a new sp object
ncovr_s_sp <- as(ncovr_s_sf, "Spatial")
#Then we create a list of neighbours using the Queen criteria
w_s <- poly2nb(ncovr_s_sp, row.names=ncovr_s_sp$FIPSNO)
wm_s <- nb2mat(w_s, style='B')
rwm_s <- mat2listw(wm_s, style='W')

fit_3_err <- errorsarlm(HR80 ~ RD80 + DV80 + MA80 + PS80 +UE80, data=ncovr_s_sf, rwm_s)
summary(fit_3_err)
```

```
## 
## Call:errorsarlm(formula = HR80 ~ RD80 + DV80 + MA80 + PS80 + UE80, 
##     data = ncovr_s_sf, listw = rwm_s)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -17.70984  -3.46135  -0.56817   2.55031  24.84717 
## 
## Type: error 
## Coefficients: (asymptotic standard errors) 
##              Estimate Std. Error z value  Pr(>|z|)
## (Intercept) 11.395096   1.525679  7.4689 8.082e-14
## RD80         3.286154   0.189085 17.3793 < 2.2e-16
## DV80         0.732640   0.133462  5.4895 4.030e-08
## MA80        -0.177938   0.044032 -4.0411 5.319e-05
## PS80         1.540084   0.214637  7.1753 7.216e-13
## UE80        -0.174869   0.066194 -2.6418  0.008248
## 
## Lambda: 0.3026, LR test value: 61.006, p-value: 5.6621e-15
## Asymptotic standard error: 0.037726
##     z-value: 8.0209, p-value: 1.1102e-15
## Wald statistic: 64.335, p-value: 9.992e-16
## 
## Log likelihood: -4384.823 for error model
## ML residual variance (sigma squared): 28.654, (sigma: 5.353)
## Number of observations: 1412 
## Number of parameters estimated: 8 
## AIC: 8785.6, (AIC for lm: 8844.7)
```

The  spatial  lag  model  is  probably  the  most  common  specification  and  maybe  the  most  generally useful way to think about spatial dependence. But we can also enter the spatial dependence as, we mentioned trough the error term in our regression equation. Whereas the spatial lag model sees the spatial dependence as substantively meaningful (in that y, say homicide, in county i is influenced by homicide  in  its  neighbours), the  spatial  error  model  simply  treats  the  spatial  dependence  as  a nuisance. This model focuses on estimating the regression parameters for the explanatory variables of interest and disregards the possibility that the spatial clustering, the spatial autocorrelation, may reflect something meaningful (other than attributional dependence as explained earlier). So instead of assuming that a spatial lag influences the dependent variable we estimate a model that relaxes the  standard  regression model  assumption  about  the  need  for the  errors  to  be  independent.  It's beyond the scope of this introductory course to cover the mathematical details of this procedure, though you can use the suggested reading (particularly the highly accessible Ward and Gleditsch,2008, book or the more recent Darmofal, 2015) or some of the video lectures in the matter that are available in the GeoDa website.

As before the AIC is better for the spatial model (8786) than for the non spatial model (8845). In this case, you can compare the regression coefficients with those from the OLS model, since we don't have a spatial lag capturing some of their effect. Notice how one of the most notable differences is the fact that unemployment nearly halves its impact in the new model. You will see the table includes a new parameter  (lambda)  but  you  don't  need  to  worry about  this  for  the  purpose  of  the  course.  It  is something you would understand if you get into the mathematical estimation details.

### HOMEWORK 3

*Estimate an appropriate regression model for the homicide rate for the 1970s for the Northern States. Justify and interpret the model that you have selected.*




## Interpreting Spatial Lag Coefficiences

Today we are mostly going to go back to some issues about geographical representation (mapping rates, bins) and other more general issues that are quite revelant when we talk about spatial analysis, and are integral part of a spatial analysis course (e.g., the modifiable area unit problem). But before we do any of that we need to go back to something we mentioned last week about the spatial lag models.

Remember that last week we fitted the following spatial lag model after creating a spatial weight matrix based on Queen first order neighbours:


```r
#Split the data into Southern and Northern counties
ncovr_s_sf <- subset(ncovr_sf, SOUTH == 1)
ncovr_n_sf <- subset(ncovr_sf, SOUTH == 0)
#We coerce the sf object into a new sp object
ncovr_n_sp <- as(ncovr_n_sf, "Spatial")
#Then we create a list of neighbours using the Queen criteria
w_n <- poly2nb(ncovr_n_sp, row.names=ncovr_n_sp$FIPSNO)
wm_n <- nb2mat(w_n, style='B')
rwm_n <- mat2listw(wm_n, style='W')
#Fit model
fit_1_lag <- lagsarlm(HR60 ~ RD60 + DV60 + MA60 + PS60 +UE60, data=ncovr_n_sf, rwm_n)
summary(fit_1_lag)
```

```
## 
## Call:
## lagsarlm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data = ncovr_n_sf, 
##     listw = rwm_n)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -11.86076  -1.54308  -0.55531   0.76832  28.30435 
## 
## Type: lag 
## Coefficients: (asymptotic standard errors) 
##              Estimate Std. Error z value  Pr(>|z|)
## (Intercept)  5.802071   0.677236  8.5673 < 2.2e-16
## RD60         1.734746   0.157771 10.9954 < 2.2e-16
## DV60         1.002757   0.077951 12.8640 < 2.2e-16
## MA60        -0.179328   0.020024 -8.9557 < 2.2e-16
## PS60         0.382956   0.077193  4.9610 7.014e-07
## UE60         0.087963   0.030109  2.9215  0.003483
## 
## Rho: 0.13749, LR test value: 15.139, p-value: 9.9874e-05
## Asymptotic standard error: 0.035631
##     z-value: 3.8587, p-value: 0.000114
## Wald statistic: 14.89, p-value: 0.000114
## 
## Log likelihood: -4273.43 for lag model
## ML residual variance (sigma squared): 9.6542, (sigma: 3.1071)
## Number of observations: 1673 
## Number of parameters estimated: 8 
## AIC: 8562.9, (AIC for lm: 8576)
## LM test for residual autocorrelation
## test value: 10.659, p-value: 0.0010954
```

Remember what we said last week:

"Interpreting  the substantive  effects  of  each  predictor  in  a spatial lag  model  is  much  more  complex  than  in  a nonspatial model (or in a spatial error model) because of the presence of the spatial multiplier that links  the  independent  variables  to  the  dependent.  In  the nonspatial model,  it  does  not  matter which unit is experiencing the change on the independent variable. The effect" in the dependent variable "of a change" in the value of an independent variable "is constant across all observations" (Darmofal, 2015: 107). Remember we say, when interpreting a regression coefficient for variable X~i,  that they indicate how much Y goes up or down for every one unit increase in X~i  when holding all other variables in the model constant. In our example, for the nonspatial model this effect is the same  for  every  county  in  our  dataset.  But  in  the  spatial  lag  model  things  are  not  the  same.  We cannot interpret the regression coefficients for the substantive predictors in the same way because the "substantive effects of the independent variables vary by observation as a result of the different neighbors for each unit in the data" (Darmofal, 2015: 107).

In  the  OLS  regression  model,  the  coefficients  for  any  of  the  explanatory  variables  measure  the absolute impact of these variables. It is a simpler scenario. We look at the effect of X in Y within each county. So X in county A affects Y in count A. In the spatial lag model there are two components to how  X  affect  Y.  X  affects  Y  within  each  county  directly  but  remember  we  are  also  including  the spatial, the measure of Y in the surrounding counties (call them B, C, and D). So our model includes not only the effect of X in county A in the level of Y in county A. By virtues of including the spatial lag (a measure of Y in county B, C and D) we are indirectly incorporating as well the effects that X has on Y in  counties  B,  C,  and  D.  So  the  effect  of  a  covariate  (independent  variable)  is  the  sum  of  two particular effects: a direct, local effect of the covariate in that unit, and an indirect, spillover effect due to the spatial lag.

This implies that a change in the ith region’s predictor can affect the jth region’s outcome. We have 2 situations: (a) the direct impact of an observation’s predictor on its own outcome, and (b) the indirect impact of an observation’s neighbor’s predictor on its outcome.This leads to three quantities that we want to know:

-Average Direct Impact, which is similar to a traditional interpretation
-Average Total impact, which would be the total of direct and indirect impacts of a predictor on one’s outcome
-Average Indirect impact, which would be the average impact of one’s neighbors on one’s outcome

These quantities can be found using the impacts() function in the spdep library. We follow the example that converts the spatial weight matrix into a “sparse” matrix, and power it up using the trW() function. This follows the approximation methods described in Lesage and Pace, 2009. Here, we use Monte Carlo simulation to obtain simulated distributions of the various impacts. 


```r
W <- as(rwm_n, "CsparseMatrix")
trMC <- trW(W, type="MC")
im<-impacts(fit_1_lag, tr=trMC, R=100)
sums<-summary(im,  zstats=T)
#To print the coefficients
data.frame(sums$res)
```

```
##        direct    indirect      total
## 1  1.74081572  0.27045500  2.0112707
## 2  1.00626530  0.15633446  1.1625998
## 3 -0.17995505 -0.02795801 -0.2079131
## 4  0.38429552  0.05970457  0.4440001
## 5  0.08827116  0.01371390  0.1019851
```


```r
#To print the p values
data.frame(sums$pzmat)
```

```
##            Direct     Indirect        Total
## RD60 0.000000e+00 0.0001243143 0.000000e+00
## DV60 0.000000e+00 0.0001031767 0.000000e+00
## MA60 0.000000e+00 0.0002102746 0.000000e+00
## PS60 1.238746e-07 0.0015203493 1.308510e-07
## UE60 5.451867e-03 0.0326507844 5.973215e-03
```

We see that all the variables have signficant direct, indirect and total effects. You may want to have a look at how things differ when you just run a non spatial model.


```r
fit_1_OLS <- lm(HR60 ~ RD60 + DV60 + MA60 + PS60 +UE60, data=ncovr_n_sf)
summary(fit_1_OLS)
```

```
## 
## Call:
## lm(formula = HR60 ~ RD60 + DV60 + MA60 + PS60 + UE60, data = ncovr_n_sf)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.1581  -1.5986  -0.5770   0.7949  28.4121 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.39963    0.66312   9.651  < 2e-16 ***
## RD60         1.85730    0.15692  11.836  < 2e-16 ***
## DV60         1.11883    0.07604  14.713  < 2e-16 ***
## MA60        -0.19537    0.01976  -9.889  < 2e-16 ***
## PS60         0.37748    0.07782   4.851 1.34e-06 ***
## UE60         0.09277    0.03021   3.071  0.00217 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.132 on 1667 degrees of freedom
## Multiple R-squared:  0.1833,	Adjusted R-squared:  0.1809 
## F-statistic: 74.83 on 5 and 1667 DF,  p-value: < 2.2e-16
```


