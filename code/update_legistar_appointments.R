library(readr)
library(jsonlite)
library(httr)
library(dplyr)

legistar_key = Sys.getenv("LEGISTAR_KEY", unset = "LEGISTAR_KEY_PLACEHOLDER")
if (legistar_key == "LEGISTAR_KEY_PLACEHOLDER" || legistar_key == "") stop("Legistar key not set")

########################################################################################
# Created by: Anne Driscoll
# Created on: 4/6/2026
# Last edited on: 11/24/2026
#
# Update the history of CM appointments
########################################################################################

reformat_data = function(d) {
  d = d %>% 
    rename(id = OfficeRecordId, 
           member_id = OfficeRecordPersonId,
           committee_id = OfficeRecordBodyId, 
           first_name = OfficeRecordFirstName, 
           last_name = OfficeRecordLastName, 
           full_name = OfficeRecordFullName,
           email = OfficeRecordEmail, 
           start_date = OfficeRecordStartDate, 
           end_date = OfficeRecordEndDate,
           committee = OfficeRecordBodyName, 
           modified_date = OfficeRecordLastModifiedUtc, 
           appointment_type = OfficeRecordMemberType,
           district = OfficeRecordExtraText)  %>% 
    select(member_id, first_name, last_name, full_name,  
           committee, appointment_type, start_date, end_date, 
           modified_date, committee_id, id, district)
  
  return(d)
}

################################################################################
# set up for loop 
################################################################################

start_date = Sys.Date() - 1

office_records = GET(paste0("https://webapi.legistar.com/v1/nyc/officerecords/", 
                            "?$filter=OfficeRecordLastModifiedUtc+ge+datetime'",
                            start_date,
                            "T00:00:00'&token=", legistar_key)) %>%
  `[[`('content') %>%
  rawToChar() %>%
  fromJSON() 

if (length(office_records) > 0) {
  
  # format response
  office_records = reformat_data(office_records)
  
  # get past appointments and reformat
  dir = "../data/output/legistar/appointments/"
  files = list.files(dir)
  files = files[grepl("[0-9]", files)]
  file = max(files)
  
  past_records = read.csv(paste0(dir, file)) %>% 
    filter(!office_id %in% office_records$OfficeRecordId)
  comb_records = office_records %>% 
    rename(office_id = id) %>% 
    select(-district) %>% 
    rbind(past_records) %>% 
    arrange(desc(start_date))
  
  new_file = paste0("../data/output/legistar/appointments/", 
                    format(Sys.Date(), "%Y-%m-%d"), 
                    "_legistar_appointments.csv")
  write_csv(comb_records, new_file)
  write_csv(comb_records,  paste0("../data/output/legistar/appointments/", 
                                  "cumulative_legistar_appointments.csv"))
  
  # get past councilmembers and reformat
  dir = "../data/output/legistar/councilmembers/"
  files = list.files(dir)
  files = files[grepl("[0-9]", files)]
  file = max(files)
  
  past_records = read.csv(paste0(dir, file)) %>% 
    filter(!office_id %in% office_records$OfficeRecordId)
  comb_records = office_records %>% 
    filter(committee == "City Council") %>% 
    mutate(district = as.numeric(gsub("District ", "", district))) %>% 
    select(full_name, member_id, start_date, end_date, district, id) %>%
    rename(office_id = id) %>% 
    rbind(past_records) %>% 
    arrange(desc(start_date))
  
  new_file = paste0("../data/output/legistar/councilmembers/", 
                    format(Sys.Date(), "%Y-%m-%d"), 
                    "_legistar_councilmembers.csv")
  write_csv(comb_records, new_file)
  write_csv(comb_records,  paste0("../data/output/legistar/councilmembers/", 
                                  "cumulative_legistar_councilmembers.csv"))
} else {
  message("No office records modified since ", start_date, "; nothing to update")
}