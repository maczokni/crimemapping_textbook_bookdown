#Load data

gmcrime16_02 <- read.csv("D:/crime/2016-02/2016-02-greater-manchester-street.csv") 
gmcrime16_03 <- read.csv("D:/crime/2016-03/2016-03-greater-manchester-street.csv") 
gmcrime16_04 <- read.csv("D:/crime/2016-04/2016-04-greater-manchester-street.csv") 
gmcrime16_05 <- read.csv("D:/crime/2016-05/2016-05-greater-manchester-street.csv") 
gmcrime16_06 <- read.csv("D:/crime/2016-06/2016-06-greater-manchester-street.csv") 
gmcrime16_07 <- read.csv("D:/crime/2016-07/2016-07-greater-manchester-street.csv") 
gmcrime16_08 <- read.csv("D:/crime/2016-08/2016-08-greater-manchester-street.csv") 
gmcrime16_09 <- read.csv("D:/crime/2016-09/2016-09-greater-manchester-street.csv") 
gmcrime16_10 <- read.csv("D:/crime/2016-10/2016-10-greater-manchester-street.csv") 
gmcrime16_11 <- read.csv("D:/crime/2016-11/2016-11-greater-manchester-street.csv") 
gmcrime16_12 <- read.csv("D:/crime/2016-12/2016-12-greater-manchester-street.csv")
gmcrime17_01 <- read.csv("D:/crime/2017-01/2017-01-greater-manchester-street.csv")
gmcrime17_02 <- read.csv("D:/crime/2017-02/2017-02-greater-manchester-street.csv")
gmcrime17_03 <- read.csv("D:/crime/2017-03/2017-03-greater-manchester-street.csv")
gmcrime17_04 <- read.csv("D:/crime/2017-04/2017-04-greater-manchester-street.csv")
gmcrime17_05 <- read.csv("D:/crime/2017-05/2017-05-greater-manchester-street.csv")
gmcrime17_06 <- read.csv("D:/crime/2017-06/2017-06-greater-manchester-street.csv")
gmcrime17_07 <- read.csv("D:/crime/2017-07/2017-07-greater-manchester-street.csv")
gmcrime17_08 <- read.csv("D:/crime/2017-08/2017-08-greater-manchester-street.csv")
gmcrime17_09 <- read.csv("D:/crime/2017-09/2017-09-greater-manchester-street.csv")
gmcrime17_10 <- read.csv("D:/crime/2017-10/2017-10-greater-manchester-street.csv")
gmcrime17_11 <- read.csv("D:/crime/2017-11/2017-11-greater-manchester-street.csv")
gmcrime17_12 <- read.csv("D:/crime/2017-12/2017-12-greater-manchester-street.csv")
gmcrime18_01 <- read.csv("D:/crime/2018-01/2018-01-greater-manchester-street.csv")
gmcrime18_02 <- read.csv("D:/crime/2018-02/2018-02-greater-manchester-street.csv")
gmcrime18_03 <- read.csv("D:/crime/2018-03/2018-03-greater-manchester-street.csv")
gmcrime18_04 <- read.csv("D:/crime/2018-04/2018-04-greater-manchester-street.csv")
gmcrime18_05 <- read.csv("D:/crime/2018-05/2018-05-greater-manchester-street.csv")
gmcrime18_06 <- read.csv("D:/crime/2018-06/2018-06-greater-manchester-street.csv")
gmcrime18_07 <- read.csv("D:/crime/2018-07/2018-07-greater-manchester-street.csv")
gmcrime18_08 <- read.csv("D:/crime/2018-08/2018-08-greater-manchester-street.csv")
gmcrime18_09 <- read.csv("D:/crime/2018-09/2018-09-greater-manchester-street.csv")
gmcrime18_10 <- read.csv("D:/crime/2018-10/2018-10-greater-manchester-street.csv")
gmcrime18_11 <- read.csv("D:/crime/2018-11/2018-11-greater-manchester-street.csv")
gmcrime18_12 <- read.csv("D:/crime/2018-12/2018-12-greater-manchester-street.csv")
gmcrime19_01 <- read.csv("D:/crime/2019-01/2019-01-greater-manchester-street.csv")

#Merge data
library(dplyr)
#This will create a list called dsf with the 12 datasets we created earlier 
dfs <- list(gmcrime16_02, gmcrime16_03, gmcrime16_04, 
            gmcrime16_05, gmcrime16_06, gmcrime16_07, gmcrime16_08,
            gmcrime16_09, gmcrime16_10, gmcrime16_11, gmcrime16_12, 
            gmcrime17_01, gmcrime17_02, gmcrime17_03, gmcrime17_04,
            gmcrime17_05, gmcrime17_06, gmcrime17_07, gmcrime17_08,
            gmcrime17_09, gmcrime17_10, gmcrime17_11, gmcrime17_12,
            gmcrime18_01, gmcrime18_02, gmcrime18_03, gmcrime18_04,
            gmcrime18_05, gmcrime18_06, gmcrime18_07, gmcrime18_08,
            gmcrime18_09, gmcrime18_10, gmcrime18_11, gmcrime18_12,
            gmcrime19_01) 
#This will use a function from the dplyr package to join all these datasets into gmpcrime 
gmpcrime <- bind_rows(dfs) 

crime14 <- read.csv("D:/crime/crime_open_database_core_2014.csv")
crime15 <- read.csv("D:/crime/crime_open_database_core_2015.csv")
crime16 <- read.csv("D:/crime/crime_open_database_core_2016.csv")
crime17 <- read.csv("D:/crime/crime_open_database_core_2017.csv")
crime18 <- read.csv("D:/crime/crime_open_database_core_2018.csv")

dfs <- list(crime14,crime15,crime16,crime17,crime18)
crime <- bind_rows(dfs)
names(crime)
table(crime$city_name)
crimeny <- filter(crime, city_name == "New York")
agassault_ny <- filter(crimeny, offense_type == "aggravated assault")
agassault_ny <- select(agassault_ny, uid, date_single, longitude, latitude, location_type, location_category, census_block, date_start, date_end)
library(readr)
write_csv(crimeny, path = "D:/crime/crimeny.csv")
write_csv(agassault_ny, path = "D:/crime/agassault.csv")
agassault_ny<-read_csv("D:/crime/agassault.csv")

set.seed(1)
library(data.table)
dtData = data.table(
  DateCol = seq(
    as.Date("1/01/2014", "%d/%m/%Y"),
    as.Date("31/12/2015", "%d/%m/%Y"),
    "days"
  ),
  ValueCol = runif(730)
)
dtData[, ValueCol := ValueCol + (strftime(DateCol,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dtData[, ValueCol := ValueCol + (abs(as.numeric(strftime(DateCol,"%m")) - 6.5)) * runif(1) * 0.75, .I]


library(readr)
write_csv(gmpcrime, path = "C:/Users/Juanjo Medina/Dropbox/1_Teaching/1 Manchester courses/Data for students/GMP/gmp.csv")
head(read_csv("C:/Users/Juanjo Medina/Dropbox/1_Teaching/1 Manchester courses/Data for students/GMP/gmp.csv"))
x <-  read_csv("C:/Users/Juanjo Medina/Dropbox/1_Teaching/1 Manchester courses/Data for students/GMP/gmp.csv")
by_month <- group_by(gmpcrime, as.factor(Month))
#Then we run the summarise function to provide some useful
#summaries of the groups we are using: the number of cases
#and the mean of the response variable
gmp_month <- summarise(by_month,
                       count = n())
write_csv(gmp_month, path = "C:/Users/Juanjo Medina/Dropbox/1_Teaching/1 Manchester courses/31152_60142 GIS and Crime Mapping/crime_mapping_text/data/gmp_month.csv")

First we will need to get the data into shape. Unfortunately `lubridate` doesnt provide a helpful function to extract both month and year simultaneously. So we need to do some computing as below:
  
  ```{r}

dallas$monthyear <-  month(dallas$Date.of.Occurrence) + (year(dallas$Date.of.Occurrence) - min(year(dallas$Date.of.Occurrence)))*12

```

Now we have a column that orders the month by year and now we can count the number of instances of burglaries at that level of aggregations:
  
  ```{r}
library(dplyr)
by_month <- group_by(dallas, as.factor(monthyear))
#Then we run the summarise function to provide some useful
#summaries of the groups we are using: the number of cases
#and the mean of the response variable
dallas_month <- summarise(by_month,
                          count = n())
dallas_month <- select(dallas_month, count)

dallas_timeseries <- ts(dallas_month, frequency=12, start=c(2014,1))
dallas_timeseries
```





          