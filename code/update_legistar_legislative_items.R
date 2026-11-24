library(jsonlite)
library(httr)
library(dplyr)
library(readr)
legistar_key = Sys.getenv("LEGISTAR_KEY", unset = "LEGISTAR_KEY_PLACEHOLDER")
if (legistar_key == "LEGISTAR_KEY_PLACEHOLDER" || legistar_key == "") stop("Legistar key not set")

########################################################################################
# Created on: 11/12/2026
#
# Update the history of legislative items (introductions, resolutions, etc.)
# using items modified in the last few days
########################################################################################

reformat_data = function(d) {
  d = d %>% 
    rename(matter_id = MatterId, 
           file_num = MatterFile, 
           enacted_num = MatterEnactmentNumber, 
           committee = MatterBodyName, 
           name = MatterName, 
           title = MatterTitle, 
           intro_date = MatterIntroDate, 
           agenda_date = MatterAgendaDate, 
           enacted_date = MatterEnactmentDate, 
           previous_file_num = MatterEXText1, 
           modified_date = MatterLastModifiedUtc) %>% 
    select(matter_id, file_num, enacted_num, 
           committee, name, title,
           intro_date, agenda_date, enacted_date, 
           previous_file_num, modified_date) %>% 
    mutate(across(everything(), as.character))
  
  return(d)
}

################################################################################
# get recently modified items
################################################################################

start_date = Sys.Date() - 1

matters = GET(paste0("https://webapi.legistar.com/v1/nyc/matters/",
                     "?$filter=MatterLastModifiedUtc+ge+datetime'",
                     start_date,
                     "T00:00:00'+and+(MatterTypeName+eq+'Introduction'", 
                     "+or+MatterTypeName+eq+'Land%20Use%20Application'",
                     "+or+MatterTypeName+eq+'Land%20Use%20Call-Up'",
                     "+or+MatterTypeName+eq+'Motion'",
                     "+or+MatterTypeName+eq+'Resolution'",
                     "+or+MatterTypeName+eq+'SLR')",
                     "&token=", legistar_key)) %>%
  `[[`('content') %>%
  rawToChar() %>%
  fromJSON() 

if (length(matters) > 0) {
  
  if (nrow(matters) >= 1000) {
    stop("Hit the 1000 row limit; increase paging or shorten start_date window")
  }
  
  matters = reformat_data(matters)
  
  dir = "../data/output/legistar/legislative_items"
  files = list.files(dir)
  files = files[grepl("[0-9]", files)]
  file = max(files)
  
  past_records = read_csv(file.path(dir, file), 
                          col_types = cols(.default = col_character())) %>% 
    filter(!matter_id %in% matters$matter_id)
  
  comb_records = bind_rows(matters, past_records) %>% 
    arrange(desc(modified_date))
  
  new_file = file.path(dir, paste0(Sys.Date(), "_legistar_legislative_items.csv"))
  write_csv(comb_records, new_file)
  write_csv(comb_records, file.path(dir, "cumulative_legistar_legislative_items.csv"))
  
  message("Updated ", nrow(matters), " items; wrote ", nrow(comb_records), 
          " rows to ", normalizePath(new_file))
} else {
  message("No legislative items modified since ", start_date, "; nothing to update")
}