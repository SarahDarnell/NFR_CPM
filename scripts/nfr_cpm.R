#NFR/CPM Analysis - CRAMPP2 and NSAID-HEAL (baseline)
#Written by Sarah Darnell

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/NFR_CPM")

token_crampp2 <- Sys.getenv("CRAMPP2_REDCAP_TOKEN")
token_nsaid <- Sys.getenv("NSAIDHEAL_REDCAP_TOKEN")

library(jsonlite)
library(dplyr)
library(lubridate)
library(ggdist)
library(ggplot2)
library(tidyr)
library(readr)

###################
## Data imports ##
##################

#CRAMPP2 nfr/cpm data from redcap
url <- "https://survey.northshore.org/api/"
formData <- list("token"=token_crampp2,
                 content='report',
                 format='json',
                 report_id='4540',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
print(result)

#convert to dateframe
response_text <- httr::content(response, as = "text")
crampp2_cpm <- fromJSON(response_text, flatten = TRUE)

#CRAMPP2 group data from redcap
formData <- list("token"=token_crampp2,
                 content='report',
                 format='json',
                 report_id='4106',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
print(result)

#convert to dateframe
response_text <- httr::content(response, as = "text")
crampp2_groups <- fromJSON(response_text, flatten = TRUE)

#merge, remove unnecessary rows
crampp2 <- merge(crampp2_groups, crampp2_cpm, by = "record_id") %>%
  select(-c("redcap_event_name.x", "redcap_event_name.y")) %>%
  #convert group to readable names
  mutate(group_arm2 = group_arm2 %>%
           recode_values(
             "1" ~ "DYS",
             "2" ~ "C",
             "3" ~ "DYSB",
             "4" ~ "CP",
             "5" ~ "Grey zone",
             "6" ~ "Unusable"
           )) %>%
  #rename cols
  rename(group = group_arm2) %>%
  #remove record_id
  select(-record_id) %>%
  #convert subid col to numeric
  mutate(subid_arm2 = as.numeric(subid_arm2)) %>%
  #add study and visit # cols
  mutate(study = "crampp2") %>%
  mutate(visit_number = 0)
  
         
#nsaid nfr/cpm data from redcap
formData <- list("token"=token_nsaid,
                 content='report',
                 format='json',
                 report_id='4875',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
print(result)

#convert to dateframe
response_text <- httr::content(response, as = "text")
nsaid_cpm <- fromJSON(response_text, flatten = TRUE)

#nsaid group data from redcap
formData <- list("token"=token_nsaid,
                 content='report',
                 format='json',
                 report_id='4812',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
print(result)

#convert to dateframe
response_text <- httr::content(response, as = "text")
nsaid_groups <- fromJSON(response_text, flatten = TRUE)

#merge, remove unnecessary rows
nsaid <- merge(nsaid_groups, nsaid_cpm, by = "record_id") %>%
  select(-c("redcap_event_name.x", "redcap_event_name.y")) %>%
  #convert group to readable names
  mutate(group = group %>%
           recode_values(
             "1" ~ "DYS",
             "2" ~ "DYSB"
           )) %>%
  #rename cols
  rename(subid_arm2 = record_id) %>%
  #convert subid col to numeric
  mutate(subid_arm2 = as.numeric(subid_arm2)) %>%
  #add study and visit # cols
  mutate(study = "nsaid") %>%
  mutate(visit_number = 0)

#merge nsaid and crampp2
crampp2_nsaid <- rbind(crampp2, nsaid)

#emg data
emg <- read_csv("raw files/OKSNAPIII_CPMratings_ANALYSIS.csv")

#convert to wide
emg <- emg %>%
  pivot_wider(
    id_cols = c(subid_arm2, visit_number, study, task),
    names_from = phase,
    values_from = -c(subid_arm2, visit_number, study, task, phase),
    names_sep = "_"
  )  

#merge
crampp2_nsaid_emg <- left_join(crampp2_nsaid, emg, by = c("subid_arm2", "study", "visit_number"))

#expectation data
expectation <- read_csv("raw files/OKSNAPIII_CPMExpectratings_ANALYSIS.csv")

#merge
crampp2_nsaid_emg <- left_join(crampp2_nsaid_emg, expectation, by = c("subid_arm2", "study", "visit_number"))

##crampp2 participants 41-0 and 153-0 have two expecation ratings, so merged two rows

#water pain data
pain <- read_csv("raw files/OKSNAPIII_CSTaskRatings_ANALYSIS.csv")

#convert to wide
pain <- pain %>%
  pivot_wider(
    id_cols = c(subid_arm2, visit_number, study),
    names_from = phase,
    values_from = water_pain,
    names_sep = "_"
  )  

#rename cols
pain <- pain %>%
  rename(warm_pain = Warm) %>%
  rename(cold_pain = Cold) 

#merge
crampp2_nsaid_emg <- left_join(crampp2_nsaid_emg, pain, by = c("subid_arm2", "study", "visit_number"))

#save file
write_csv(crampp2_nsaid_emg, "edited files/crampp2_nsaid_emg_bl.csv")



