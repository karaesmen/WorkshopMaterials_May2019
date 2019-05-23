library(tidyverse)

### IMPORTANT !!! ###

## you can unzip the downloaded file in R first
unzip("Session4-20190501T150706Z-001.zip")

# change the path to your Session4 directory
# or open your RStudio project

## ---- eval=F-------------------------------------------------------------
## # this is for Mac/Linux
## setwd("~/change/this/path/to your/Session 3 folder path")
## 
## # this is for windows
## setwd("C:/change/this/path/to your/Session 3 folder path") # note / instead of \ in windows


## ------------------------------------------------------------------------
weather <- read.table("data/weather.txt", # path to file
                      sep="\t",        # delimiter set to tab
                      header = T,      # file has header, T is same as TRUE
                      stringsAsFactors = F) # set strings as character
head(weather, 3) # show first 3 rows


## ------------------------------------------------------------------------
weather <- read.table("data/weather.txt", # path to file
                      sep="\t",        # delimiter set to tab
                      header = T,      # file has header, T is same as TRUE
                      stringsAsFactors = F, # set strings as character
                      na.strings = ".") # periods are missing values, read them as NA !
head(weather, 3) # show first 3 rows


## ------------------------------------------------------------------------
weather <- read_delim("data/weather.txt", delim = "\t", na = c(".", "NA"))
head(weather, 3)


## ------------------------------------------------------------------------
weather <- read_tsv("data/weather.txt", na = c(".", "NA"))
head(weather, 3)


## ------------------------------------------------------------------------
wages <- read_csv("data/wages.csv") 
head(wages, 3)


# **Your turn:**  (5 mins) {.bigger}

# 1. Check `?read_delim` help file. What are the other base R functions that read specific types of data? What about the arguments and the default settings?      
 
# 2. Load `crime.csv` using `read_delim()` or `read_csv()` and assign it to variable `crime`.    
crime <- read_csv("data/crime.csv")

# 3. What is the mean murder rate in the US according to `crime` data?    
head(crime)
mean(crime$murder)

# 4. Load the first sheet of `titanic.xlsx` using readxl library
library(readxl)
titanic <- read_excel("data/titanic.xlsx")

# 5. In total how many females perished in titanic? 
head(titanic)
titanic %>%
  filter(fate == "perished") %>%
  summarise(total_female=sum(female))
