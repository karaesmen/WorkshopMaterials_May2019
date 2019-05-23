
#### Subsetting ####     
  
#### 1. Integers #### 
# Positive integers behave just like *ij* notation in linear algebra.
# Lets work with an example data frame `df` inspired from the Beatles.


df <- data.frame(name=c("John","Paul","George","Ringo"),
                 birth=c(1940, 1942, 1943, 1940),
                 insturment=c("guitar", "bass","guitar","drums") )
df

#  The following gives the element in 2nd row and 3rd column.
df[2,3]

#### Multiple integers to subset #### 
# By giving vectors as indices, we can also get multiple rows and columns, giving multiple elements.
# The following gives the elements that are in 2nd and 4th rows and the 3rd column.
df[c(2,4),3]

# The following gives the elements that are in 2nd & 4th rows and 2nd & 3rd columns.
df[c(2,4),c(2,3)]


# When selecting a series of rows and columns in a sequential manner, `:` becomes very handy.    
# (Remember: `start:end` gives us a sequence of integers. For example `1:10` gives a vector of integers from 1 to 10.)
# Select rows from 1 to 3 and columns from 1 to 2.
df[1:3, 1:2]


#### Repeating integers #### 
# repeating input repeats output.   
df[c(1,1,1,2,2), 1:3]

#### Integer `0` ####      
# As an index, **zero will return nothing** from a dimension. This creates an empty object.
df[1:2,0]

#### Negative integers #### 
# Negative integers return **everything but** the elements at the specified locations.    
# You cannot use both negative and positive integers in the same dimension     
# Exclude rows 2 to 4 and select columns 2 to 3.
df[-c(2:4), 2:3]

# Exclude rows 2 to 4 and select columns 2 to 3.
df[-c(2:4), 2:3]


#### 2. Blank Spaces #### 
# Blank spaces return **everything**
# (i.e., no subsetting occurs on that dimension)
vec <- c(6,1,3,6,10,5)
vec[]

# Return every element on row 1
df[1,]

# Return every element on column 2
df[,2]

#### 3. Names #### 
# If your object has names, you can ask for elements or columns back by name. (We talked about this in the first session)
names(vec) <- c("a","b","c","d","e","f")
vec

# Now I can call elements in `vec` by their names
vec[c("a","b","d")]

# Same applies to columns and column names
df[ ,"birth"]
df[ ,c("name","birth")]

#### 4. Logical #### 
# You can subset with a logical vector of the **same length as the dimension** you are subsetting. Each element that corresponds to a TRUE will be returned.
# The following will return 2nd, 4th and 5th elements in `vec`
vec[c(FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)]

# The following will return **2nd, 3rd rows** in `df`
df[c(FALSE,TRUE,TRUE,FALSE), ]

#### Subsetting Lists #### 
# Subsetting lists can get tricky, since a list can contain objects that has one or more dimensions.
lst <- list(c(1,2), TRUE, c("a", "b", "c"))
lst

# What is the difference?
lst[c(1,2)]  # outputs a list with 2 objects
lst[1]       # outputs a list with 1 object
lst[[1]]     # outputs a vector


### Easiest way to subset data frames and lists: `$` sign
# The most common syntax for subsetting lists and data frames:
  
# When we have a names list such as
names(lst) <- c("alpha", "beta", "gamma")
lst

# then we can subset objects with `$`
lst$alpha
lst$beta
lst$gamma

# It also works to subset columns of a data frame 
df$birth

####  R Packages #### 
install.packages(c("ggplot2", "maps", "RColorBrewer"))
library("ggplot2")
library("maps")
library("RColorBrewer")

####  Diamonds #### 
diamonds <- data.frame(diamonds)
# first 6 rows
diamonds[1:6,]

# last 6 rows
dim(diamonds)
ncol(diamonds) # number of columns

#one way
nrow(diamonds) # number of rows
nrow(diamonds) - 6
diamonds[53934:53940]

# another way
diamonds[(nrow(diamonds)-6):nrow(diamonds),]

# easiest way to check the first and last 5 rows
head(diamonds) 
tail(diamonds) 

# view data
View(diamonds) # notice: Capital V

## Help pages ##
# You can open the help page for any R object (including functions) by typing `?` 
# followed by the object's name
?diamonds

####  Logical tests #### 

#### Logical comparisons #### 

# What will these return?
1< 3
1> 3
c(1, 2, 3, 4, 5) > 3

#### %in% #### 
# What does this do?
1 %in% c(1, 2, 3, 4)
1 %in% c(2, 3, 4)
c(3,4,5,6) %in% c(2, 3, 4)

#### Boolean operators ####
?Logic
x <- 4

# & -- and
x > 2 & x < 9
x > 5 & x < 9
x > 5 & x < 2

TRUE & TRUE
TRUE & TRUE & TRUE & TRUE
TRUE & TRUE & FALSE & TRUE

# | -- or
TRUE | TRUE
TRUE | FALSE
FALSE |  FALSE

x > 2 | x < 9
x > 5 | x < 9
x > 5 | x < 2

# xor -- Is either condition 1 or condition 2 true, but not both?
xor(TRUE, TRUE)
xor(TRUE, FALSE)
xor(FALSE, TRUE)
xor(FALSE, FALSE)

xor(x > 2, x < 9)
xor(x > 5, x < 9)
xor(x > 5, x < 2)

# ! -- Negation
!(TRUE, TRUE)
!(TRUE, FALSE)
!(FALSE, FALSE)

!(x > 2, x < 9)
!(x > 5, x < 9)
!(x > 5, x < 2)

# any -- is any condition true?
any(c(TRUE, FALSE, FALSE))
any(c(FALSE, FALSE, FALSE))

any(x > 5, x < 2)
any(x > 2, x < 9)

# all -- is every condition TRUE?
all(c(TRUE, TRUE, TRUE))
all(c(TRUE, FALSE, TRUE))

all(x > 5, x < 2)
all(x > 2, x < 9)

#### Your turn ####
w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

# Is w positive?
# Is x greater than 10 and less than 20? Is object y the word February?
# Is every value in z a day of the week?

## Answers

# Is w positive?
w> 0

# Is x greater than 10 and less than 20? 
10 < x & x < 20

#Is object y the word February?
y == "February"

# Is every value in z a day of the week?
all(z %in% c("Monday", "Tuesday", "Wednesday",
             "Thursday", "Friday", "Saturday", "Sunday"))

# Common mistakes
x > 10 & < 20
y = "February"
all(z == "Monday" | "Tuesday" | "Wednesday"...)

#### Logical subsetting #####
x_zeroes <- diamonds$x == 0
head(x_zeroes)

# What will this return?
diamonds[x_zeroes, ]

#### Saving results ####
# Prints to screen
diamonds[diamonds$x > 10, ]

# Saves to new data frame
big <- diamonds[diamonds$x > 10, ]

# Overwrites existing data frame. Dangerous! 
diamonds <- diamonds[diamonds$x < 10,]
diamonds <- diamonds[1, 1]
diamonds # Uh oh!
rm(diamonds)
diamonds <- data.frame(diamonds) # Phew!

#### NA behavior ####
a <- c(1, NA)
a == NA
is.na(a)

b <- c(1, 2, 3, 4, NA)
sum(b)
sum(b, na.rm = TRUE)

#### NA Assignment ####
# Before removing outliers
qplot(x, y, data = diamonds)

## Let's remove outliers ##
?diamonds
# Let's start with x
summary(diamonds$x) 

# there are 0s in x 
#but x is supposed to be the lenght of the diamond, it can't be 0, a diamond is a 3d object in real life
diamonds$x[diamonds$x == 0]

# we can assign NA to 0s to let qplot know that these are missing values
diamonds$x[diamonds$x == 0] <- NA  
 
# now 0s gone from the Min. and we have NA's
summary(diamonds$x)

# Now let's do y
summary(diamonds$y)

# we can assign NA to 0s to let qplot know that these are missing values
diamonds$y[diamonds$y == 0] <- NA

# very big diamonds
y_big <- diamonds$y > 20 
diamonds$y[y_big] <- NA

# outliers are assigned as NA!
summary(diamonds$y)

# let's look at our plot again
qplot(x, y, data = diamonds)

