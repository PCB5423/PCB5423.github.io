---
title: "Workshop 1: Introduction to R"
author: "BSC 6926 B53"
date: "8/23/2022"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: yeti
---



## Getting to know the basics 

R is a programming language that has become the standard in Ecology due to its flexibility and open source nature. R can be used from simple math to complex models and is very useful for generating figures. R, like all computer languages, uses a specific syntax to run commands that it is programmed to do. In other words, R will only do what it is commanded to do, and therefore, many common errors are due to errors in syntax (e.g. misspellings, missed commas, or unclosed brackets). 

This example gives a basic intro into R syntax that can be useful for ecological research. This script gives examples of how to:

1.  Basic operations in R 
2.  Assigning objects
3.  Types of data structures in R
4.  Functions in R
5.  Using Packages in R
      + How to install and load packages
6.  `tidyverse`
      + tidy data
      + piping 

R script: [canvas](), [github](https://github.com/PCB5423/BSC6926_workshopScripts/blob/master/workshop1.R)
      
[R script of workshop 1](workshop1.R)

## Basic operations in R

R is useful for basic operations and follows math rules (i.e. PEMDAS). R will all code on a line unless there is a `#` to the left.


```r
# addition 
1+1 
## [1] 2

1+1 # + 2 (won't run anything to right of #)
## [1] 2

# subtraction
5-2 
## [1] 3

# multiplication
4*5
## [1] 20

# division
33/5
## [1] 6.6

# exponents can be done 2 ways
2^2
## [1] 4
2**2
## [1] 4

# follows PEMDAS
1+5*4
## [1] 21
# different answer than above
(1+5)*4
## [1] 24
```

Note the `[1]` appears next to your result. R is just letting you know that this line begins with the first value in your result. Some commands return more than one value, and their results may fill up multiple lines. For example, the command 100:130 returns 31 values; it creates a sequence of integers from 100 to 130. Notice that new bracketed numbers appear at the start of the first and second lines of output. These numbers just mean that the second line begins with that value. You can mostly ignore the numbers that appear in brackets:


```r
100:130
##  [1] 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
## [20] 119 120 121 122 123 124 125 126 127 128 129 130
```
## Assigning objects
When working in R it is useful to store data as an object. Assigning objects can be done in multiple ways, but the most common are `<-` and `=`. These objects are stored in the R environment and can be called. Objects can be assigned multiple times, but only the last assignment is what is stored. Also it is important to know that R is case sensative and capital and lower case numbers are different.


```r
# assign an object
a = 4 
a
## [1] 4

b <- 23

a+3 
## [1] 7

b/2
## [1] 11.5

a*b
## [1] 92

c = 8
c = 14
c
## [1] 14

d = 15 
D = 1 
d
## [1] 15
D
## [1] 1
```

## Types of data structures in R
R has 6 basic data types. (In addition to the five listed below, there is also raw which will not be discussed in this workshop.)

  + integer
  + numeric (real or decimal)
  + character
  + logical
  + complex
  
integers are whole numbers

numeric are numbers with decimals. Integers and numeric are different because of how the underlying data is stored. Other programming languages can use something similar as decimal, float, or double data types, which all slightly differ in how data is stored but are numbers that include decimals.

characters are strings of letters and numbers (e.g. `"abc"` and `"b1x"`) and are designated in R by `" "`. When using characters, `" "` are required because in R letters without quotations are objects and `c = 'd'` is different than `c = d`

logical is `TRUE` or `FALSE`. One thing to note is that `T` is the same as `TRUE` and `F` is the same as `FALSE`. Because `T` and `F` are special in R they cannot be used to name objects (but `t` and `f` are ok because R is case sensative). This is true for other cases as well like `NA` and `NULL`.  

complex numbers have both real and imaginary parts (`1+4i`)

Elements of these data types may be combined to form data structures, such as atomic vectors. When we call a vector atomic, we mean that the vector only holds data of a single data type. A vector is the most common and basic data structure in R and is pretty much the workhorse of R. Technically, vectors can be one of two types:
  + atomic vectors
  + lists
although the term “vector” most commonly refers to the atomic types not to lists. Lists differ because they can take on different data structures and can be more complex.

There are different ways to make vectors


```r

# make a numeric vector
a = c(1.1,5,3,4)
a
## [1] 1.1 5.0 3.0 4.0

# make a integer vector
b = 1:15
b
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

# make a character vector 
c = c('a', 'b', 'c')
c 
## [1] "a" "b" "c"
```

Because characters can be both letters and numbers, numbers in a vector with letters are stored as a character. These cannot be used for math operations, but integers and numeric data types can be used for math. 


```r
a = 4.4
a / 1 
## [1] 4.4


b = 6L # L can be used to keep a numeric as an integer, R typically defaults to numeric
b*3
## [1] 18

# character
c = '1'
c*4
## Error in c * 4: non-numeric argument to binary operator
```

Another common way to store data is in a dataframe or tibble (special type of dataframe from the `tidyverse` package we will see below). This is a collection of atomic vectors with the same length. 


```r
b = data.frame(c1 = c(1,2,3), c2 = c('a','b','c'))
b
##   c1 c2
## 1  1  a
## 2  2  b
## 3  3  c
```

## Functions in R
R comes with functions that are used to do tasks. Functions take arguments to complete a task. Functions have the general format `function(argument1 = , argument2,...)` The types of data used and output of the function is specific to that function. Below are just a few useful examples. 


```r
# summary statistics of sequence of numbers
a = c(1.1,5,3,4)
mean(a) #mean
## [1] 3.275
median(a) #median
## [1] 3.5
sd(a) #standard deviation
## [1] 1.664081
quantile(a, 0.5) # quantile at 0.5 (median)
## 50% 
## 3.5

# make a sequence of numbers
b = 1:15
b
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
c = seq(1,15,1) #more flexibility than :
c
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
seq(4,20,2)
## [1]  4  6  8 10 12 14 16 18 20

# information about objects
d = c('a', 'b', 'c')
typeof(d) 
## [1] "character"
typeof(c)
## [1] "double"
length(d)
## [1] 3

# dataframe/tibble specific functions
e = data.frame(c1 = c(1,2,3), c2 = c('a','b','c'))
names(e) # column names
## [1] "c1" "c2"
nrow(e) # number of rows
## [1] 3
length(e) # for dataframe number of columns
## [1] 2
str(e)# structure of data
## 'data.frame':	3 obs. of  2 variables:
##  $ c1: num  1 2 3
##  $ c2: chr  "a" "b" "c"
```

## Using Packages in R
R comes with a lot of base functions that are available for use when you open R, but this does not contain all of the functions useful to your tasks in R. Since R is open source, many R users have created Packages that contain functions that can be downloaded. Which includes the very common `tidyverse`.

### How to install and load packages
Packages can be downloaded from CRAN or from Github. To download directly from Github other packages are needed. 

```r
install.packages('tidyverse') #from cran
```

Once downloaded, packages can be loaded into the R environment with `library()` function. Packages have to be loaded each R session. In addition functions can be called directly from a package with `::` in the format of `packageName::function()`. 


```r
library(tidyverse)
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

## tidyverse
[tidyverse](https://www.tidyverse.org/) is a collection of packages that use similar syntax and are used for data science in R. Coding in tidyverse is typically easy to read and understand, and has useful functions that have been adopted into newer versions of base R (e.g. piping). Tibbles are the tidyverse version of a dataframe.

```r
c = tibble(c1 = c(1,2,3), c2 = c('a','b','c'))
c
## # A tibble: 3 × 2
##      c1 c2   
##   <dbl> <chr>
## 1     1 a    
## 2     2 b    
## 3     3 c
```

### tidy data
Data is collected and stored in many different ways, which can make it difficult to analyze. One of the goals of tidyverse is to easily turn messy data into tidy data which can easily be analyzed. In tidy data:

1. Every column is a variable.
2. Every row is an observation.
3. Every cell is a single value.

Two functions `pivot_longer()` and `pivot_wider()` are useful in manipulating data stored in rows and columns. ***Note that `pivot_longer()` and `pivot_wider()` have replaced `gather()` and `spread()` in newer versions of `tidyverse`



```r
#tidying data 
stock = tibble(name = c('GOOG', 'AMC', 'GME'),
               Jan = c(1000, 2, 4),
               Feb = c(1010, 15, 30),
               March = c(1005, 25, 180))

df = pivot_longer(stock,
               cols = Jan:March, 
               names_to = 'Month',
               values_to = 'Price')

df
## # A tibble: 9 × 3
##   name  Month Price
##   <chr> <chr> <dbl>
## 1 GOOG  Jan    1000
## 2 GOOG  Feb    1010
## 3 GOOG  March  1005
## 4 AMC   Jan       2
## 5 AMC   Feb      15
## 6 AMC   March    25
## 7 GME   Jan       4
## 8 GME   Feb      30
## 9 GME   March   180

# wide format
fish = tibble(species = rep(c('Salmon', 'Cod'),times = 3),
              year = rep(c(1999,2005,2020), each = 2),
              catch = c(50, 60, 40, 50, 60, 100))
fish 
## # A tibble: 6 × 3
##   species  year catch
##   <chr>   <dbl> <dbl>
## 1 Salmon   1999    50
## 2 Cod      1999    60
## 3 Salmon   2005    40
## 4 Cod      2005    50
## 5 Salmon   2020    60
## 6 Cod      2020   100


pivot_wider(fish,
            id_cols = species,
            names_from = year,
            values_from = catch)
## # A tibble: 2 × 4
##   species `1999` `2005` `2020`
##   <chr>    <dbl>  <dbl>  <dbl>
## 1 Salmon      50     40     60
## 2 Cod         60     50    100
```

### piping  
Tidyverse has an operator `%>%` known as a pipe that is useful for when you want to do multiple actions to the same data. It takes the output of the left of the `%>%` and makes it the first argument of what is on the right. Allowing to reduce code and make things tidier. In newer versions of R, there is a base pipe `|>` that can be used as well.


```r
# this code
df = as_tibble(mtcars)
df = filter(df, mpg > 20)
df = mutate(df, color = 'red')
df = select(df, mpg, cyl, color)

head(df)
## # A tibble: 6 × 3
##     mpg   cyl color
##   <dbl> <dbl> <chr>
## 1  21       6 red  
## 2  21       6 red  
## 3  22.8     4 red  
## 4  21.4     6 red  
## 5  24.4     4 red  
## 6  22.8     4 red

# can become

df = mtcars %>%
  as_tibble()%>%
  filter(mpg > 20)%>%
  mutate(color = 'red')%>%
  select(mpg, cyl, color)

head(df)
## # A tibble: 6 × 3
##     mpg   cyl color
##   <dbl> <dbl> <chr>
## 1  21       6 red  
## 2  21       6 red  
## 3  22.8     4 red  
## 4  21.4     6 red  
## 5  24.4     4 red  
## 6  22.8     4 red

# or with base r
df = mtcars |>
  as_tibble()|>
  filter(mpg > 20)|>
  mutate(color = 'red')|>
  select(mpg, cyl, color)

head(df)
## # A tibble: 6 × 3
##     mpg   cyl color
##   <dbl> <dbl> <chr>
## 1  21       6 red  
## 2  21       6 red  
## 3  22.8     4 red  
## 4  21.4     6 red  
## 5  24.4     4 red  
## 6  22.8     4 red
```
