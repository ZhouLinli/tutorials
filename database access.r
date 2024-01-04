library(tidyverse)
library(odbc) # install.packages("odbc")

source("code/ipeds_utilities.R")

# This script uses the IPEDS database for provisional data 2020-21
# available from https://nces.ed.gov/ipeds/use-the-data/download-access-database
# Uncompressed, it is 555MB, so allow some time to download. I did not put 
# it in the box folder for this project because I didn't want to burden
# my file sync across machines and to the cloud unnecessarily.

# We will use Open Data Base Connectivity (ODBC) to link R scripts to 
# the data. 
# Windows 10: https://support.microsoft.com/en-us/office/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7
# Mac: https://db.rstudio.com/best-practices/drivers/
# Note that depending on what type of database you want to connect to, you
# may need to first install a driver. Microsoft provides drivers for its
# products (Access, MS-SQL).

# create a connection to the database
dbc <- odbc::dbConnect(odbc::odbc(), "MyData")

# test it out by getting list of data tables (omit meta-data)
ipeds_tables <- odbc::dbListTables(dbc, table_type =  "TABLE")

# In IPEDS there is a more detailed way to get table information, since
# there is an IPEDS meta-data table
ipeds_tables <- tbl(dbc,"Tables20") %>% 
                   collect()

# Suppose we want to find tables that might have retention information in them
ret_tables <- ipeds_tables %>% 
                 filter(str_detect(Description,"retention"))

# let's look at a sample of that table EF2020D
ret_data   <- tbl(dbc, "EF2020D") %>% 
                 collect()

# the column names are meaningless, so let's change those to something better
# using the IPEDS column name index here:
ret_cols <- tbl(dbc,"vartable20") %>% 
                 filter(TableName == "EF2020D") %>% 
                 select(varName, varTitle) %>% 
                 collect()

# match up the column names in ret_data to the descriptions and swap them
  # 1. get the column names as they are now
  old_cols <- names(ret_data)
  
  # 2. try to match the old col names to the list we have
  new_cols <- ret_cols$varTitle[match(old_cols, ret_cols$varName)]
  
  # 3. if there are missing values, use the original version
  new_cols[is.na(new_cols)] <- old_cols[is.na(new_cols)]
  
  # 4. replace the old column names
  names(ret_data) <- new_cols

  # look at ret_data to see the change
  
# Question: how well can we predict retention using some of the information in 
# this table?

model_data <- ret_data %>% 
  mutate(Cohort = `Full-time adjusted fall 2019 cohort`,
         Retain = `Full-time retention rate, 2020`,
         PTRate = `Part-time adjusted fall 2019 cohort`/(`Part-time adjusted fall 2019 cohort`+ Cohort),
         SFRatio = `Student-to-faculty ratio`) %>% 
  select(UNITID, Cohort, Retain, PTRate, SFRatio) %>% 
  na.omit() # only complete cases

# plot size vs retention
model_data %>% 
  ggplot(aes(x = Cohort, y = Retain)) +
  geom_point(alpha = .1) +
  geom_smooth(se = FALSE) +
  theme_bw()

# too much variation, try log scale on x-axis
model_data %>% 
  ggplot(aes(x = Cohort, y = Retain)) +
  geom_point(alpha = .1) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  scale_x_log10()
         
# conclusion: really small programs are flaky. Use log of size for rest.
model_data <- model_data %>% 
                filter(Cohort >= 200)

# create the linear model and look at results
# to apply a function to an input inside a formula, use I()
ret_model <- lm(Retain ~ I(log(Cohort)) + PTRate + SFRatio, 
                data = model_data ) # weights = model_data$Cohort
summary(ret_model)           

# plot the predicted versus actuals 
model_data %>% 
  mutate(Predicted = fitted.values(ret_model)) %>% 
  ggplot(aes(x = Predicted, y = Retain)) +
  geom_point(alpha = .1) +
  geom_smooth(se = FALSE) +
  theme_bw()

# this is interesting, but rather crude. We don't know what type of 
# institution each point represents. Might make more sense to categorize
# them by type, like public/private. 

# where can we find characteristics?
char_tables <- ipeds_tables %>% 
  filter(str_detect(Description,"characteristics|control|public|Carnegie"))

# Looks like HD2020 will do the trick. Rather than including all the 
# code to rename the columns each time we need to do that, I wrote a 
# function to retrieve the data with renamed cols. See the code folder
inst_char <- get_ipeds_table(dbc, "HD2020")

# Since we kept the UNITID in the model_data, all we need to do is join to
# selected data elements from the characteristics
model_data <- inst_char %>% 
  select(UNITID, 
         Level = `Level of institution`, 
         Control = `Control of institution`) %>% 
  filter(Level == "Four or more years",
         Control %in% c("Public","Private not-for-profit")) %>% 
  left_join(model_data) %>% 
  na.omit()

# redo the model with control included
ret_model <- lm(Retain ~ I(log(Cohort)) + PTRate + SFRatio + Control, 
                data = model_data ) # weights = model_data$Cohort
summary(ret_model)           

# plot the predicted versus actuals 
model_data %>% 
  mutate(Predicted = fitted.values(ret_model)) %>% 
  ggplot(aes(x = Predicted, y = Retain, group = Control, color = Control)) +
  geom_point(alpha = .3) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")
