# this script demonstrates how to download a survey from Qualtrics
# using the qualtRics package. If you haven't installed it, you'll need
# to run install.packages("qualtRics") first.

# You'll also need an API licence to Qualtrics. The documentation is here
# https://api.qualtrics.com/ 

# Your institution's ID can be found using instructions here
# https://api.qualtrics.com/ZG9jOjg3NjYzMw-base-url-and-datacenter-i-ds

# Your personal API key can be found using instructions here:
# https://api.qualtrics.com/ZG9jOjg3NjYzMg-api-key-authentication

# Note: downloading data from the internet using an API is a generally
# useful skill, e.g. for Workday or many other sources of information.

library(qualtRics)
library(tidyverse)
source("scale_map.R") # see script in folder

# see instructions here: https://github.com/ropensci/qualtRics

# this has to be run once to provide your credentials:
qualtrics_api_credentials(api_key = "<your API key>", 
                          base_url = "sjc1.qualtrics.com", # yours may be different
                          install = TRUE) # or overwrite = TRUE if editing

# retrieve a list of all the surveys you have access to
surveys <- all_surveys()

# pick a survey to work with
id <- surveys$id[1] # first one in the list

# retrieve the data from a specific survey, using the choice values 
text_data <- fetch_survey(surveyID = id,
                               label = TRUE,
                               verbose = TRUE,
                               force_request = TRUE) %>% # forces download even if survey exists on your HD
  # for confidentiality I'm deleting student info from this survey
  select(-starts_with("Q2_"))

# retrieve the data from a specific survey, using the choice codes
code_data <-  fetch_survey(surveyID = id,
                               label = FALSE,
                               convert = FALSE,
                               verbose = TRUE,
                               force_request = TRUE) %>% # forces download even if survey exists on your HD
  # for confidentiality I'm deleting student info from this survey
  select(-starts_with("Q2_"))

mymap <- column_map(surveyID = id)

# top level information about the survey
mymeta <- metadata(surveyID = id) # retrieves metadata about your survey, such as questions, survey flow, number of responses etc.
 
# item information
myitems <- survey_questions(surveyID = id) # retrieves a data frame containing questions and question IDs for a survey; 

# generate the scales for discrete item responses
myscales <- scale_map(text_data, code_data, max_codes = 100, missing_value = "")

############## package up and save the survey #################
survey <- list(meta = mymeta, items = myitems, scales = myscales, data = code_data)

write_rds(survey, str_c(id,".rds"))
