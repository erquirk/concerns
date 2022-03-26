#Load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(stringi)
library(readxl)
library(here)

#Import 4 files - English and French raw data files, a list of variable names, and a list of places in Montreal and Quebec 
place_names <- read_excel(here("data/place_names.xlsx"))
langatt_variable_names <- read_excel(here("data/langatt_variable_names.xlsx"))
RMI_QData_English_Raw_2021_03_22 <- read_excel(here("data/RMI_QData_English_Raw_2021.03.22.xlsx"))
RMI_QData_French_Raw_2021_03_22 <- read_excel(here("data/RMI_QData_French_Raw_2021.03.22.xlsx"))

#Record number of rows in raw data file to keep track of how many data points are removed at each step
data_points_n_per_step_eng <- tibble(step = c("raw data"),
                                     n_rows = c(nrow(RMI_QData_English_Raw_2021_03_22)))
data_points_n_per_step_fr <- tibble(step = c("raw data"),
                                    n_rows = c(nrow(RMI_QData_French_Raw_2021_03_22)))

#Step 1 Tidy data
#Rename and make data frames from imported files 
langatt_df_eng<- as_tibble(RMI_QData_English_Raw_2021_03_22)
langatt_df_fr<-as_tibble(RMI_QData_French_Raw_2021_03_22)
langatt_variable_names<-as_tibble(langatt_variable_names)

#Change column names
colnames(langatt_df_eng)<-langatt_variable_names$newname
colnames(langatt_df_fr)<-langatt_variable_names$newname

#Remove duplicate responses at different timestamps
langatt_df_eng <-langatt_df_eng %>%
  distinct(across(consent:ol3_used), .keep_all = TRUE)
langatt_df_fr <-langatt_df_fr %>%
  distinct(across(consent:ol3_used), .keep_all = TRUE)

#Add participant IDs so there is a unique identifier for each entry
langatt_df_eng<-langatt_df_eng %>% 
  mutate(id = paste0("eng_", row_number()))
langatt_df_fr<-langatt_df_fr %>% 
  mutate(id = paste0("fr_", row_number()))

#Keep track of n by steps and save a copy of data files at the end of each step 
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "1", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "1", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step1<-langatt_df_eng #save copy of dataset at this step
langatt_df_fr_step1<-langatt_df_fr

#Step 2: Exclusion of ineligible participants based on their own age
#Change any erroneously entered parent age as year of birth to age in years  
langatt_df_eng<- langatt_df_eng %>%
  mutate(parent_age=ifelse(parent_age>1000, lubridate::year(timestamp)-parent_age, parent_age))

langatt_df_fr<- langatt_df_fr %>%
  mutate(parent_age=ifelse(parent_age>1000, lubridate::year(timestamp)-parent_age, parent_age))

#Eliminate participants with missing parent age and impossibly young/old parents 
langatt_df_eng<-filter(langatt_df_eng, is.na(parent_age)==FALSE & !(parent_age<15|parent_age>65))
langatt_df_fr<-filter(langatt_df_fr, is.na(parent_age)==FALSE & !(parent_age<15|parent_age>65))

#Keep track of n by steps and make a copy for set comparison at each step
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "2", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "2", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step2<-langatt_df_eng
langatt_df_fr_step2<-langatt_df_fr

#Step 3a: Exclusion of ineligible participants based on their child's age
#Eliminate participants with missing child DOB
langatt_df_eng<-filter(langatt_df_eng, is.na(child_dob)==FALSE)
langatt_df_fr<-filter(langatt_df_fr, is.na(child_dob)==FALSE)

#Convert date to consistent format for child_dob
#Create a new variable "clean_dob" with strings converted dates, unparsed are NAs
langatt_df_eng$clean_dobs<-parse_date_time(langatt_df_eng$child_dob,c("mY","mdY","my","dmY"))
langatt_df_fr$clean_dobs<-parse_date_time(langatt_df_fr$child_dob,c("mY","mdY","my","dmY"))

#Create a df of new variables and descriptions, add clean_dobs 
new_vars<-tibble(new_var_name=c('clean_dobs'),
                 new_var_description=c('Clean child date of birth in month-year format'))

#Create vectors of the bad DOBs participant IDs
na_ids_eng<-langatt_df_eng %>% filter(is.na(clean_dobs)) %>% 
  dplyr::select(id)
na_ids_fr<-langatt_df_fr %>% filter(is.na(clean_dobs)) %>% 
  dplyr::select(id)

#Create df of unparseable DOBs, inspect and then change on a case-by-case basis. 
#Note that some bad dobs were siblings. Kept the younger one in these cases.
#Any date that did not have a day, we set the day as the first of the month to match the default action of parse_date_time.

bad_DOBs_eng<- langatt_df_eng %>% 
  filter(id %in% na_ids_eng$id)

dplyr::select(bad_DOBs_eng, id, child_dob) %>% view()

#View and make any adjustments to dates of birth 
bad_DOBs_eng<- bad_DOBs_eng %>%
  mutate(clean_dobs = case_when(id=="eng_64" ~mdy("8/8/16"),
                                id=="eng_66" ~mdy("12/01/2019"),
                                id == "eng_75"~mdy("08/01/2020"),
                                id == "eng_128"~mdy("02/01/2017"),
                                id == "eng_288"~mdy("07/01/2020"),
                                id == "eng_293"~mdy("08/08/2016"),
                                id == "eng_508"~mdy("04/03/2012"),
                                id == "eng_518"~mdy("10/29/2020"),
                                id == "eng_561"~mdy("03/01/2018")))
#same for French
bad_DOBs_fr<- langatt_df_fr %>% 
  filter(id %in% na_ids_fr$id)

dplyr::select(bad_DOBs_fr, id, child_dob) %>% view()

#View and make any adjustments to ID numbers 
bad_DOBs_fr<- bad_DOBs_fr %>% 
  mutate(clean_dobs = case_when(id=="fr_104" ~mdy("02/02/2017"),
                                id=="fr_124" ~mdy("11/09/2019"),
                                id == "fr_148"~mdy("01/01/2020"),
                                id == "fr_209"~mdy("10/01/2017"),
                                id == "fr_276"~mdy("01/01/2017"),
                                id == "fr_335"~mdy("03/01/2017"),
                                id == "fr_344"~mdy("07/01/2016"),
                                id == "fr_385"~mdy("05/01/2017"),
                                id == "fr_402"~mdy("05/29/2017"),
                                id == "fr_426"~mdy("09/01/2020")))


#Merge the two data frames 

langatt_df_eng<-langatt_df_eng %>% 
  bind_rows(bad_DOBs_eng) %>%
  filter(!is.na(clean_dobs)) %>%
  mutate(clean_dobs = as.Date(clean_dobs))

langatt_df_fr<-langatt_df_fr %>% 
  bind_rows(bad_DOBs_fr) %>%
  filter(!is.na(clean_dobs)) %>%
  mutate(clean_dobs = as.Date(clean_dobs))

#Remove remaining uninterpretable DOBs and fix century

langatt_df_eng$clean_dobs <-as.Date(ifelse(langatt_df_eng$clean_dobs< "1900-01-01" | langatt_df_eng$clean_dobs > "3000-01-01",
                                           format(langatt_df_eng$clean_dobs, "20%y-%m-%d"),
                                           format(langatt_df_eng$clean_dobs)))

langatt_df_fr$clean_dobs <-as.Date(ifelse(langatt_df_fr$clean_dobs< "1900-01-01" | langatt_df_fr$clean_dobs > "3000-01-01",
                                          format(langatt_df_fr$clean_dobs, "20%y-%m-%d"),
                                          format(langatt_df_fr$clean_dobs)))
#Remove DOBs in the future
langatt_df_eng<-filter(langatt_df_eng, clean_dobs<timestamp)
langatt_df_fr<-filter(langatt_df_fr, clean_dobs<timestamp)

#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "3a", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "3a", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step3a<-langatt_df_eng
langatt_df_fr_step3a<-langatt_df_fr

# #Step 3b: Exclude children beyond 4 years of age
# #Age calculation
# langatt_df_eng$age_days_at_test = with(langatt_df_eng,
#                                        day(as.period(as.Date(timestamp) - as.Date(clean_dobs) , unit="months")))
# langatt_df_fr$age_days_at_test = with(langatt_df_fr,
#                                       day(as.period(as.Date(timestamp) - as.Date(clean_dobs) , unit="months")))
# 
# # Remove children with age_at_test of more than 1461 days (4 years)
# langatt_df_eng<-langatt_df_eng %>%
#   filter(age_days_at_test<=1461)
# 
# langatt_df_fr<-langatt_df_fr %>%
#   filter(age_days_at_test<=1461)

#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "3b", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "3b", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step3b<-langatt_df_eng
langatt_df_fr_step3b<-langatt_df_fr

#Step 3c: Remove duplicate children and siblings entered by the same parents
#Make dates month-year; some duplicate children have dobs with and without days 
#Note this step converts clean_dobs to character class. Changed back to date below.
langatt_df_eng$clean_dobs<- format(langatt_df_eng$clean_dobs, "%m/%Y")
langatt_df_fr$clean_dobs <- format(langatt_df_fr$clean_dobs, "%m/%Y")
langatt_df_eng$clean_dobs <- lubridate::my(langatt_df_eng$clean_dobs)
langatt_df_fr$clean_dobs <- lubridate::my(langatt_df_fr$clean_dobs)

#Remove children with same dob and parent email

`%notin%` <- negate(`%in%`) #creates an operator %notin% which does the opposite of the %in% operator

#---------------------------ENGLISH

eng_duplicate_parents <- langatt_df_eng %>% #take full dataframe
  mutate(email = tolower(email), #make emails all lower case
         amazon_email = tolower(amazon_email), 
         na_count = rowSums(is.na(.))) %>% #add count of NAs per row
  dplyr::select(id, clean_dobs, email, amazon_email, timestamp, na_count) %>% #choose only the columns that matter for this step
  pivot_longer(c("email", "amazon_email"), names_to = "email_type", values_to = "email_address") %>% #pivot so all emails are in one column
  filter(!is.na(email_address)) %>% #remove NA emails
  distinct(id, clean_dobs, email_address, .keep_all = TRUE) %>% #keep only distinct sets of id and email
  group_by(email_address) %>%
  mutate(duplicated = length(unique(id))-1) %>% #count # emails per ID, anything higher than 0 will be a duplicate
  filter(duplicated > 0) %>% arrange(email_address)

remove_dups_eng <- eng_duplicate_parents %>% #make dataframe of the specific duplicates to keep
  group_by(email_address, clean_dobs) %>% 
  filter(na_count == min(na_count)) %>% #filter to only the lowest # of NAs per child
  group_by(email_address) %>%
  filter(clean_dobs == max(clean_dobs)) %>% #filter to only the youngest child per parent
  distinct(email_address, na_count, .keep_all = TRUE)

eng_duplicate_parents <- eng_duplicate_parents %>% #make data frame of the specific duplicates to get rid of
  filter(id %notin% remove_dups_eng$id) 

langatt_df_eng <- langatt_df_eng %>%
  filter(id %notin% eng_duplicate_parents$id) #this step filters out the duplicate rows that are either older siblings or the same child but with more NAs

#---------------------------FRENCH
fr_duplicate_parents <- langatt_df_fr %>% #take full dataframe
  mutate(email = tolower(email), #make emails all lower case
         amazon_email = tolower(amazon_email), 
         na_count = rowSums(is.na(.))) %>% #add count of NAs per row
  dplyr::select(id, clean_dobs, email, amazon_email, timestamp, na_count) %>% #choose only the columns that matter for this step
  pivot_longer(email:amazon_email, names_to = "email_type", values_to = "email_address") %>% #pivot so all emails are in one column
  filter(!is.na(email_address)) %>% #remove NA emails
  distinct(id, clean_dobs, email_address, .keep_all = TRUE) %>% #keep only distinct sets of id and email
  group_by(email_address) %>%
  mutate(duplicated = length(unique(id))-1) %>% #count # emails per ID, anything higher than 0 will be a duplicate
  filter(duplicated > 0) %>% arrange(email_address)

remove_dups_fr <- fr_duplicate_parents %>% #make dataframe of which specific duplicates to get rid of
  group_by(email_address, clean_dobs) %>% 
  filter(na_count == min(na_count)) %>% #filter to only the lowest # of NAs per child
  group_by(email_address) %>%
  filter(clean_dobs == max(clean_dobs)) %>% #filter to only the youngest child per parent
  distinct(email_address, na_count, .keep_all = TRUE)

fr_duplicate_parents <- fr_duplicate_parents %>%
  filter(id %notin% remove_dups_fr$id) 

langatt_df_fr <- langatt_df_fr %>%
  filter(id %notin% fr_duplicate_parents$id) #this step filters out the duplicate rows that are either older siblings or the same child but with more NAs


#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "3c", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "3c", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step3c<-cbind(langatt_df_eng)
langatt_df_fr_step3c<-cbind(langatt_df_fr)

langatt_df_eng<-langatt_df_eng%>%
  dplyr::select(-c(use_determiners,use_determiners_detail,view_open,resources_general, 
            resources_info,resources_info_used, resources_info_want, resources_help, 
            resources_help_used, resources_help_want, resources_open,q1_expert,
            q2_expert,comment_qaire,receive_summ,email,amazon,amazon_email))

langatt_df_fr<-langatt_df_fr%>%
  dplyr::select(-c(use_determiners,use_determiners_detail,view_open,resources_general, 
                   resources_info,resources_info_used, resources_info_want, resources_help, 
                   resources_help_used, resources_help_want, resources_open,q1_expert,
                   q2_expert,comment_qaire,receive_summ,email,amazon,amazon_email))

write_xlsx(langatt_df_eng, "concerns/data/langatt_df_eng_cleandobs_ids_new.xlsx")
write_xlsx(langatt_df_fr, "concerns/data/langatt_df_fr_cleandobs_ids_new.xlsx")

write_csv(langatt_df_eng, "concerns/data/input_files/langatt_df_eng_cleandobs_ids.csv")
write_csv(langatt_df_fr, "concerns/data/input_files/langatt_df_fr_cleandobs_ids.csv")
