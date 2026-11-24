library(tidyverse)
library(jsonlite)
library(httr)
library(data.table)
source("../../tokens.R")

########################################################################################
# Created by: Anne Driscoll
# Created on: 12/17/2025
# Last edited on: 11/14/2026
#
# Get the history of CM Committee appointments
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

months = seq(as.Date("1980/1/1"), as.Date(Sys.Date()), "months")
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
  officerecords = GET(paste0("https://webapi.legistar.com/v1/nyc/officerecords/",
                             "?$filter=OfficeRecordStartDate+ge+datetime'",
                             start_date,
                             "'+and+OfficeRecordStartDate+lt+datetime'",
                             end_date,
                             "'&token=", legistar_key)) %>%
    `[[`('content') %>%
    rawToChar() %>%
    fromJSON() 
  
  # check that we didn't get nothing
  if (is.null(nrow(officerecords))) {next}
  
  
  # check that we didn't get 1000
  if (nrow(officerecords) == 1000) {
    print(paste0("WE HIT 1000!!!! For start_date = ", start_date))
    break
  }
  
  # format response
  officerecords = reformat_data(officerecords)
  
  # add data to list
  list_data[[i]] = officerecords
  setTxtProgressBar(pb, i)
}
close(pb)


################################################################################
# merge the historic data
################################################################################

empty = !lengths(list_data) > 1
historic_data = rbindlist(list_data[!empty])

################################################################################
# save out appointments
################################################################################

appointments = historic_data %>% 
  select(member_id, first_name, last_name, full_name, committee, 
         appointment_type, start_date, end_date, modified_date, committee_id, 
         id) %>%
  rename(office_id = id) %>% 
  arrange(desc(end_date))
write_csv(appointments, file.path("..", "data", "output", "legistar", 
                                  "appointments",
                                  paste0(Sys.Date(), "_legistar_appointments.csv")))


################################################################################
# save out council members
################################################################################

members = historic_data %>% 
  filter(committee == "City Council") %>% 
  mutate(district = as.numeric(gsub("District ", "", district))) %>% 
  select(full_name, member_id, start_date, end_date, district, id) %>%
  rename(office_id = id) %>% 
  arrange(desc(end_date))

write_csv(members, file.path("..", "data", "output", "legistar", "councilmembers",
                             paste0(Sys.Date(), "_legistar_councilmembers.csv")))


