

## ------------------------------------------------------------------------
library(tidyverse)
pew <- read_csv("data/pew.csv")
head(pew)



## ------------------------------------------------------------------------
pew %>%
    gather(key=income, value=n, -religion) %>%
    head


## ------------------------------------------------------------------------
billboard <- read_csv("data/billboard.csv")
billboard[1:3, 1:10]


## ------------------------------------------------------------------------
billboard2 <- billboard %>% 
  gather(week, rank, wk1:wk76, na.rm = TRUE)
billboard2 %>% head


## ------------------------------------------------------------------------
billboard3 <- billboard2 %>%
  mutate(
    week = parse_number(week),
    date = as.Date(date.entered) + 7 * (week - 1)) %>%
  select(-date.entered)
billboard3 %>% head



## ------------------------------------------------------------------------
billboard3 %>% arrange(date, rank) %>% head


## ------------------------------------------------------------------------
tb <- read_csv("data/tb.csv")
tb[1:3, 1:10]


## ------------------------------------------------------------------------
tb2 <- tb %>% 
  gather(key = demo, value = n, -iso2, -year, na.rm = TRUE)
tb2 %>% head


## ------------------------------------------------------------------------
tb3 <- tb2 %>% 
  separate(demo, c("sex", "age"), 1)
tb3 %>% head


## ------------------------------------------------------------------------
weather <- read_csv("data/weather.csv")
weather[1:3, 1:15]


## ------------------------------------------------------------------------
weather2 <- weather %>%
  gather(key = day, value = value, d1:d31, na.rm = TRUE)
weather2 %>% head


## ------------------------------------------------------------------------
weather3 <- weather2 %>% 
  mutate(day = parse_number(day)) %>%
  select(id, year, month, day, element, value) %>%
  arrange(id, year, month, day)
weather3 %>% head


## ------------------------------------------------------------------------
weather4 <- weather3 %>% 
    spread(key = element, value = value)
weather4 %>% head


## ------------------------------------------------------------------------
billboard3 %>%
    head


## ------------------------------------------------------------------------
song <- billboard3 %>% 
  select(artist, track, year, time) %>%
  unique() %>%
  mutate(song_id = row_number())
song %>% head


## ------------------------------------------------------------------------
rank <- billboard3 %>%
  left_join(song, c("artist", "track", "year", "time")) %>%
  select(song_id, date, week, rank) %>%
  arrange(song_id, date)
rank %>% head


## ------------------------------------------------------------------------
(paths <- dir("data", pattern = "iris*", full.names = TRUE))
map_df(paths, read_tsv) %>% head


## ------------------------------------------------------------------------
basename(paths)
read_files <- function(x) {
    read_tsv(x) %>%
        mutate(file=basename(x))
}
map_df(paths, read_files) %>% head


## ------------------------------------------------------------------------
IceCream <- read_tsv("data/IceCream.txt")
IceCream


## ----icecream------------------------------------------------------------
IceCream_fixed <- 
  IceCream %>% 
  separate(Ice_cream_scoops, paste0("flavor", 1:3), sep=",") %>% 
  gather(key= "scoop_order", value="scoop_flavor", flavor1:flavor3 ) %>% 
  # remove rows with NA
  na.omit %>% 
  # who cares about scoop order...
  select(-scoop_order) %>% 
  separate(scoop_flavor, c("n_scoop", "flavor"), "_") %>% 
  # set n_scoop to numeric -- mutate is a dplyr function
  mutate(n_scoop=as.numeric(n_scoop))

IceCream_fixed

# average ice cream consumption by flavor (units: scoop)
#this is also from dplyr
IceCream_fixed %>% group_by(flavor) %>%
  summarise(mean=mean(n_scoop)) %>% arrange(mean)

