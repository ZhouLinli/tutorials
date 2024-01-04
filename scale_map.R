########################## Create item response map ##############################
# For nominal scales, like 1 = Female, 2 = Male, or ordinal scales like
# 1 = Strongly Disagree, ..., 5 = Strongly Agree, the survey can return either the 
# numerical encoding (found above in code_data) or the text prompts that the
# respondent sees (found in text_data). To effectively use these, we need to be
# able to associate the two, to know that 1 = Strongly Disagree, for example. To
# create this map, we can use the two versions of the downloaded survey

scale_map <- function(text_data, code_data, max_codes = 100, missing_value = ""){
  # find the integer-valued columns
  int_cols  <- code_data %>% select_if(is.numeric) %>% names()
  text_cols <- text_data %>% select_if(is.character) %>% names()
  item_cols <- intersect(int_cols, text_cols)
  
  # loop through these, comparing the text to numerical values
  item_map <- tibble(ItemID = character(0), Code = integer(0), Text = character(0))
  for(i in seq_along(item_cols)){
    mycol <- item_cols[i]
    
    item_data <- tibble(ItemID = mycol,
                        Code = code_data[[mycol]],
                        Text = text_data[[mycol]]) %>% 
      count(ItemID, Code, Text) %>% 
      select(-n) # don't need the count
    
    # ensure validity: we can only have one code per text value
    if(sum(duplicated(item_data$Code)) > 0) {
      message(str_c("Skipping ",mycol," because of duplicated values"))
      next # skips rest of the loop
    }
    
    # make sure we don't have codes that exceed the max length
    if(nrow(item_data) > max_codes) {
      message(str_c("Skipping ",mycol," because of too many values"))
      next
    }
    
    # add this information to the accumulating output
    item_map <- item_map %>% 
      add_row(item_data)
  }
  
  # clean up by checking for blanks
  item_map <- item_map %>% 
    filter(!is.na(ItemID), !is.na(Code)) %>% 
    replace_na(list(Text = missing_value))
  
  return(item_map)
}