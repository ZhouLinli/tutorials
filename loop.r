library(tidyverse)
#' *Loops*
#' A loop in programming is a structure that repeats the same code, like
#' 
#' Loop start:
#'   Do process P
#'   Do process Q
#'   Check for end of loop condition
#' Go back to loop start
#' 
#' This will go on forever unless there is a condition that terminates 
#' the loop. 
#' 
#' In R, most loops are implicit, in that they are built into other functions.
#' Generally, explicit loops should be used as a last resort.
#' 
#' *Jargon*
#' The most common data structure in R is an *array*, like
c(1,2,3)
#' or
c("a","b","c")
letters[1:3] # built-in letters var makes this easier

#' The two-dimensional version of an array is a matrix
matrix(1, nrow = 3, ncol = 2) # constant matrix

#' Because arrays can be compared with distance measures, they 
#' create a kind of shape-space (i.e. topology) and are often 
#' called *vectors*. For our purposes, vectors and arrays refer
#' to the same thing. Note that in R, a *list* is a special 
#' data type that resembles an array, but is treated differently.

#' *Important considerations*
#' 1) iterating over rows is different from iterating over rows
#' 2) in R, it's more natural to iterate over the rows of one
#'    or more columns. Many built-in functions exist for this.  

#' We already get a lot of 'looping' built in. 
1:5 # same as c(1,2,3,4,5)

#' The repeat function rep() can create arrays
rep(1, 4) # same as c(1,1,1,1)
rep("a",3)

#' can be combined with c()
c(rep("Treatment",10), rep("Control",8))

#' Sequences can be created similarly
seq(from = 1, to = 5, by = .5) # or just seq(1,5,.5)

#' expand two or more vectors into all combinations.
#' expand.grid() is the base R version
expand_grid(Die1 = 1:6, Die2 = 1:6) # all possibilities of rolling two dice
expand_grid(Die1 = 1:6, Die2 = 1:6, Die3 = 1:6) # all possibilities of rolling three dice

#' *functions that return arrays*
#' Random numbers
rnorm(n=10, mean = 0, sd = 1) # 10 random numbers, normal distribution with mean zero, SD = 1

#' Samples
sample(1:6, 20, replace = TRUE) # simulate dice rolls

#' So to plot the distribution of sums of two dice, we can
N <- 1000 # sample size
data.frame(Die1 = sample(1:6, N, replace = TRUE),
           Die2 = sample(1:6, N, replace = TRUE)) %>% 
    mutate(Sum = Die1 + Die2) %>% 
    ggplot(aes(x = Sum)) +
    geom_histogram(binwidth = 1, color = "white", fill = "steelblue") +
    scale_x_continuous(breaks = 2:12) +
    theme_bw()

#' # *R's natural iteration over rows*
x <- 1:4 # counts from 1 to 4 and puts in an array
y <- 3:6
x + y # adds each element of x to the corresponding one for y

1:4 + 3:6 # same thing

#' This is how mutate() operates when we modify a data frame
data.frame(x = 1:4, y = 3:6) %>% 
      mutate(x + y)

#' conceptually, this works across each row
#' 1 + 3 -> 4
#' 2 + 4 -> 5
#' 3 + 5 -> 6
#' 4 + 6 -> 7
#' 
#' We'll call such functions 'vectorized', thinking of a single 
#' column of data as a vector. Examples of vectorized functions
#' include arithmetic (as we just saw), and 
#' pmin(a, b) row-wise minimum of a and b,
#' sign(a)    arithmetic sign of numbers
#' sqrt(a)    square root
#' logical functions
data.frame(x = 1:4, y = 3:6) %>% 
  mutate(pmin(x,y), # vectorized minimum of x and y
         pmax(x,y), # vectorized maximum of x and y
         sign(x),
         sqrt(x),
         x == 2)

#' if we create new functions to use in mutate, we need to be sure
#' the function is vectorized
#' 
#' Example: This function only works for single inputs because "if" isn't
#' vectorized. 
is.zero <- function(z){
  if(z == 0) return(TRUE)
  return(FALSE)
}

is.zero(1) # works fine for single inputs
is.zero(0)
is.zero(0:2) # fails for multiple values
             # only returns one value instead of 3
             # gives a warning

#' Special 'vectorized' if-type functions include 
#' if_else() and case_when(). These automatically
#' handle multiple values.
is.zero <- function(z){ # fixed version
  if_else(z == 0, TRUE, FALSE) # base R has ifelse. See help for differences.
}

is.zero(1) # works fine for single inputs
is.zero(0)
is.zero(0:2) # works for multiple values because if_else is vectorized

#' *Looping over columns*
#' Problem: we want to find the standard deviation of each 
#' numerical column in a data frame.
#' 
#' We could just manually do by listing columns:
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  summarize(sd_x = sd(x),
            sd_y = sd(y),
            sd_z = sd(z))

#' OR we could use tidyverse's legacy function summarize_if
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  summarize_if(is.numeric, sd) # there are several variations of this

#' the updated version is across() to select columns
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  summarize(across(.fns = sd) ) 

#' A useful case is when creating a data table for a spreadsheet-like
#' presentation, e.g. to create proportions from counts.
enroll <- tribble(~Year, ~FTFT, ~Transfer, ~NonDegree,
                  2018,  1011,       210,         21,
                  2019,  1103,       199,         18,
                  2020,  1056,       221,         23,
                  2021,  1098,       231,         15)

#' *Problem:* create proportions of the count columns and create a total column
enroll %>% 
  mutate(Total = FTFT + Transfer + NonDegree) %>% 
  mutate(across(.cols = c("FTFT","Transfer","NonDegree"), 
                .fns  = function(x) x / Total))

#' we could also do this without across() doing the looping, but we have to 
#' spell everything out in the mutate
enroll %>% 
  mutate(Total     = FTFT + Transfer + NonDegree,
         FTFT      = FTFT / Total,
         Transfer  = Transfer/Total,
         NonDegree = NonDegree/Total) # this is easier to read!

#' base R has a set of apply() functions. The lapply works with lists,
#' and it's useful to know that a dataframe is a list of columns,
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  lapply(sd) # the output is a list, not a data frame

#' tidyverse also has a version of apply, in the purrr package. The main
#' functions are variations of map(). They iterate over a list, like
#' a data frame. We have more control over the output type with map() 
#' than with apply().
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  map_df(sd) # the _df tells it we want a dataframe as output

#' *Looping over categories* 
#' We have already seen group_by() used to iterate over
#' groups to produce categorized outputs. 
#' 
data.frame(x = 1:4, y = 3:6, z = c("a","a","b","b")) %>%
  group_by(z) %>% 
  summarize(sum(x + y))

#' We can also do sophisticated outputs using this 
#' method. 
#' *Problem:* regress y using x in each category and generat
#' model statistics
data.frame(x = 1:4, 
           y = 3:6 + rnorm(4), 
           z = c(rep("a",2), rep("b",2))) %>%
  group_by(z) %>% 
  summarize(m = lm(y ~ x, data = .) %>% broom::glance())

#' The data = . sets the current grouped data set as 
#' the input, and the broom package produces the 
#' statistics. We can get the coefficients the same way
data.frame(x = 1:4, 
           y = 3:6 + rnorm(4), 
           z = c(rep("a",2), rep("b",2))) %>%
  group_by(z) %>% 
  summarize(m = lm(y ~ x, data = .) %>% broom::tidy())

#' An alternative to group_by() is nest(), which 
#' creates individual data frames for each group, e.g.
data.frame(x = 1:4, 
           y = 3:6 + rnorm(4), 
           z = c(rep("a",2), rep("b",2))) %>%
  nest(new_df = c(x,y)) %>% # groups by the non-nested variables
  mutate(m = lapply(new_df, function(df) lm(y ~ x, data = df))) 
#' cf https://tidyr.tidyverse.org/reference/nest.html
#' The lapply creates a list, so the model column m is a list of 
#' models, one for each group in the z column

#' There is an inverse function unnest(), which unpacks embedded data frames

#' *General purpose loops*
#' Sometimes we want to have complete control over the loop.
#' We can make them explicit with for()

for(i in 1:4){
  print(i)
}

#' A pattern I often use is 
depts <- c("ART", "BIO", "CHM")
           
for(dept in depts){
  print(str_c("Running report for department ",dept ))
  # run report for that department, e.g.
  # using an Rmd template
}

#' *Data accumulation loops*
#' Sometimes we want to accumulate results over many cases, like simulation
#' results. 
output <- tibble() #empty data frame
for(i in 1:10){
  new_results <- tibble(simulation = i, x = 1:100, y = rnorm(100))
  output <- bind_rows(output, new_results)
}

#' that method can be slow for large data sets, because each time the output
#' dataframe is modified, a new memory allocation process has to run. A better way 
#' comes from https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop
#' 
datalist <- list() #empty list
for(i in 1:10){
  new_results <- tibble(simulation = i, x = 1:100, y = rnorm(100))
  datalist[[i]] <- new_results 
}
output <- bind_rows(datalist)

#' *Advanced/Specialized row-wise operations*
#' The Tidyverse now includes *rowwise()*, which you can read about here:
#' https://dplyr.tidyverse.org/articles/rowwise.html
#' It is a special kind of group_by, and can be 'turned off' with
#' ungroup()
#' 
#' Compare without rowwise...
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  mutate(Mean = mean(c(x,y,z))) # average of all the data

#' ...to with it
data.frame(x = 1:4, y = 3:6, z = -1:2) %>%
  rowwise() %>% 
  mutate(Mean = mean(c(x,y,z))) # average of each row

#' There's a lot more we can do with rowwise(). See the link above for
#' applications to modeling and simulation.

