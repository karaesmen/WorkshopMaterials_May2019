## ----setup, message = FALSE----------------------------------------------
library(nycflights13)
library(tidyverse)
options(tibble.width = Inf)

## ---- eval=FALSE---------------------------------------------------------
## install.packages("tidyverse")


## ------------------------------------------------------------------------
flights


## ------------------------------------------------------------------------
filter(flights, month == 1, day == 1)


## ------------------------------------------------------------------------
jan1 <- filter(flights, month == 1, day == 1)


## ------------------------------------------------------------------------
(dec25 <- filter(flights, month == 12, day == 25))


## ---- error = TRUE-------------------------------------------------------
filter(flights, month = 1)


## ------------------------------------------------------------------------
sqrt(2) ^ 2 == 2
1/49 * 49 == 1


## ------------------------------------------------------------------------
near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)


## ----bool-ops, echo = FALSE, fig.cap = "Complete set of boolean operations. `x` is the left-hand circle, `y` is the right-hand circle, and the shaded region show which parts each operator selects."----
knitr::include_graphics("diagrams/transform-logical.png")


## ---- eval = FALSE-------------------------------------------------------
## filter(flights, month == 11 | month == 12)


## ---- eval = FALSE-------------------------------------------------------
## nov_dec <- filter(flights, month %in% c(11, 12))


## ---- eval = FALSE-------------------------------------------------------
## filter(flights, !(arr_delay > 120 | dep_delay > 120))
## filter(flights, arr_delay <= 120, dep_delay <= 120)


## ------------------------------------------------------------------------
NA > 5
10 == NA
NA + 10
NA / 2


## ------------------------------------------------------------------------
NA == NA


## ------------------------------------------------------------------------
# Let x be Mary's age. We don't know how old she is.
x <- NA

# Let y be John's age. We don't know how old he is.
y <- NA

# Are John and Mary the same age?
x == y
# We don't know!


## ------------------------------------------------------------------------
is.na(x)


## ------------------------------------------------------------------------
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)


## ------------------------------------------------------------------------
arrange(flights, year, month, day)


## ------------------------------------------------------------------------
arrange(flights, desc(arr_delay))


## ------------------------------------------------------------------------
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))


## ------------------------------------------------------------------------
# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))


## ------------------------------------------------------------------------
rename(flights, tail_num = tailnum)


## ------------------------------------------------------------------------
select(flights, time_hour, air_time, everything())


## ------------------------------------------------------------------------
vars <- c("year", "month", "day", "dep_delay", "arr_delay")


## ---- eval = FALSE-------------------------------------------------------
## select(flights, contains("TIME"))


## ------------------------------------------------------------------------
flights_sml <- select(flights, 
  year:day, 
  ends_with("delay"), 
  distance, 
  air_time
)
mutate(flights_sml,
  gain = arr_delay - dep_delay,
  speed = distance / air_time * 60
)


## ------------------------------------------------------------------------
mutate(flights_sml,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)


## ------------------------------------------------------------------------
transmute(flights,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)


## ------------------------------------------------------------------------
transmute(flights,
  dep_time,
  hour = dep_time %/% 100,
  minute = dep_time %% 100
)


## ------------------------------------------------------------------------
(x <- 1:10)
lag(x)
lead(x)


## ------------------------------------------------------------------------
x
cumsum(x)
cummean(x)


## ------------------------------------------------------------------------
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))


## ------------------------------------------------------------------------
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)


## ---- eval = FALSE, echo = FALSE-----------------------------------------
## flights <- flights %>% mutate(
##   dep_time = hour * 60 + minute,
##   arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
##   airtime2 = arr_time - dep_time,
##   dep_sched = dep_time + dep_delay
## )
## 
## ggplot(flights, aes(dep_sched)) + geom_histogram(binwidth = 60)
## ggplot(flights, aes(dep_sched %% 60)) + geom_histogram(binwidth = 1)
## ggplot(flights, aes(air_time - airtime2)) + geom_histogram()


## ------------------------------------------------------------------------
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))


## ------------------------------------------------------------------------
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


## ---- fig.width = 6------------------------------------------------------
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
  count = n(),
  dist = mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)


## ------------------------------------------------------------------------
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")


## ------------------------------------------------------------------------
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


## ------------------------------------------------------------------------
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))


## ------------------------------------------------------------------------
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

flights %>% 
  drop_na(dep_delay, arr_delay) %>% nrow()

flights %>% 
  drop_na() %>% nrow()

head(flights)

flights %>%
  group_by(year, month, day) %>%
  summarise(n= n())

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


## ------------------------------------------------------------------------
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)


## ------------------------------------------------------------------------
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


## ------------------------------------------------------------------------
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
    geom_point(alpha = 1/10)


## ------------------------------------------------------------------------
# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point() + 
    geom_smooth(se = FALSE)


## ------------------------------------------------------------------------
batters %>% 
  arrange(desc(ba))


## ------------------------------------------------------------------------
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )


## ------------------------------------------------------------------------
# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))


## ------------------------------------------------------------------------
# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )


## ------------------------------------------------------------------------
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )


## ------------------------------------------------------------------------
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(rmin = min_rank(desc(dep_time))) %>% 
  select(dep_time, rmin, everything()) %>%
  filter(rmin == 1)


## ------------------------------------------------------------------------
# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))


## ------------------------------------------------------------------------
not_cancelled %>% 
  count(dest)


## ------------------------------------------------------------------------
not_cancelled %>% 
  count(tailnum, wt = distance)


## ------------------------------------------------------------------------
# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))


## ------------------------------------------------------------------------
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))


## ------------------------------------------------------------------------
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights


## ------------------------------------------------------------------------
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)


## ------------------------------------------------------------------------
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests


## ------------------------------------------------------------------------
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

