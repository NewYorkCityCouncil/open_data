library(tidyverse)
library(jsonlite)
library(httr)
library(data.table)
source("../../tokens.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 11/24/2026
#
# Get the history of legislative items (meaning introductions and resolutions)
########################################################################################

reformat_data = function(d) {
  d = d %>% 
    rename(matter_id = MatterId, 
           file_num = MatterFile, 
           enacted_num = MatterEnactmentNumber, 
           type = MatterTypeName, 
           status = MatterStatusName, 
           committee = MatterBodyName, 
           name = MatterName, 
           title = MatterTitle, 
           intro_date = MatterIntroDate, 
           agenda_date = MatterAgendaDate, 
           passed_date = MatterPassedDate, 
           enacted_date = MatterEnactmentDate, 
           previous_file_num = MatterEXText1, 
           modified_date = MatterLastModifiedUtc)  %>% 
    select(matter_id, file_num, enacted_num, 
           committee, name, title,
           intro_date, agenda_date, enacted_date, 
           previous_file_num, modified_date)
  
  return(d)
}

################################################################################
# set up for loop 
################################################################################

months = seq(as.Date("1990/1/1"), Sys.Date(), "months")
months = paste0(months, "T00:00:00")

list_data = as.list(rep(NA, length(months)-1))

################################################################################
# begin loop
################################################################################

pb = txtProgressBar(min = 0, max = length(list_data), style = 3)
for (i in 1:length(list_data)) {
  
  start_date = months[i]
  end_date = months[i+1]
  
  # get data
  matters = GET(paste0("https://webapi.legistar.com/v1/nyc/matters/",
                       "?$filter=MatterIntroDate+ge+datetime'",
                       start_date,
                       "'+and+MatterIntroDate+lt+datetime'",
                       end_date,
                       "'+and+(MatterTypeName+eq+'Introduction'", 
                       "+or+MatterTypeName+eq+'Land%20Use%20Application'",
                       "+or+MatterTypeName+eq+'Land%20Use%20Call-Up'",
                       "+or+MatterTypeName+eq+'Motion'",
                       "+or+MatterTypeName+eq+'Resolution'",
                       "+or+MatterTypeName+eq+'SLR')",
                       "&token=", legistar_key)) %>%
    `[[`('content') %>%
    rawToChar() %>%
    fromJSON() 
  
  # check that we didn't get nothing
  if (is.null(nrow(matters))) {next}
  
  
  # check that we didn't get 1000
  if (nrow(matters) == 1000) {
    print(paste0("WE HIT 1000!!!! For start_date = ", start_date))
    break
  }
  
  # format response
  matters = reformat_data(matters)
  
  # add data to list
  list_data[[i]] = matters
  setTxtProgressBar(pb, i)
}
close(pb)

################################################################################
# merge the historic data
################################################################################

empty = !lengths(list_data) > 1
historic_data = rbindlist(list_data[!empty])
historic_data = historic_data %>% 
  arrange(desc(intro_date))


################################################################################
# save out the historic data
################################################################################

write_csv(historic_data, file.path("..", "data", "output", "legistar", 
                                   "legislative_items",
                                   paste0(Sys.Date(), "_legistar_legislative_items.csv")))
