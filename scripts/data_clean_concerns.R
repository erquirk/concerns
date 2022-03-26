library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(stringi)
library(readxl)
library(here)

#Not in function
`%notin%` <- negate(`%in%`)


#Import 4 files - English and French raw data files and a list of places in Montreal and Quebec 
place_names <- read_csv(here("concerns/data/input_files/place_names.csv"))
eng_df <- read_csv(here("concerns/data/input_files/langatt_df_eng_cleandobs_ids.csv"))
fr_df <- read_csv(here("concerns/data/input_files/langatt_df_fr_cleandobs_ids.csv"))

#Record number of rows in raw data file to keep track of how many data points are removed at each step
data_points_n_per_step_eng <- tibble(step = c("raw data"),
                                     n_rows = c(nrow(eng_df)))
data_points_n_per_step_fr <- tibble(step = c("raw data"),
                                    n_rows = c(nrow(fr_df)))

#Step 1 Tidy data
#Make data frames from imported files
langatt_df_eng<- as_tibble(eng_df)
langatt_df_fr<-as_tibble(fr_df)

#Keep track of n by steps and save a copy of data files at the end of each step 
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "1", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "1", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step1<-langatt_df_eng #save copy of dataset at this step
langatt_df_fr_step1<-langatt_df_fr

#Translate French responses to demographic and languages used questions to English 
#Standardize the spelling/language of English and French in other language used columns 
langatt_df_eng <- langatt_df_eng %>%
  mutate(child_languages=str_replace_all(child_languages, "One language other than English or French","One other language" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Several languages other than English or French","Several other languages" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "One language other than English or French","One other language" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Several languages other than English or French","Several other languages" ))

langatt_df_fr<-langatt_df_fr%>%
  mutate(gender=str_replace_all(gender,"Féminin","Female"))%>%
  mutate(gender=str_replace_all(gender, "Masculin","Male" ))%>%
  mutate(gender=str_replace_all(gender, "Autre","Other" ))%>%
  mutate(child_sex=str_replace_all(child_sex,"Féminin","Female"))%>%
  mutate(child_sex=str_replace_all(child_sex, "Masculin","Male" ))%>%
  mutate(child_sex=str_replace_all(child_sex, "Autre","Other" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Une autre langue que l’anglais ou le français","One other language" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Plusieurs langues autres que le français ou l’anglais","Several other languages" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Anglais","English" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Français","French" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Une langue autre que l’anglais ou le français","One other language" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Plusieurs langues autres que l’anglais ou le français","Several other languages" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Non applicable parce que j’élève mon enfant seul\\(e\\)","Does not apply because I am raising my child alone" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Anglais","English" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Français","French" ))%>%
  mutate(ol1_used=tolower(ol1_used))%>%
  mutate(ol1_used=str_replace_all(ol1_used, "anglais","English" ))%>%
  mutate(ol1_used=str_replace_all(ol1_used, "english","English" ))%>%
  mutate(ol1_used=str_replace_all(ol1_used, "fran[çc]ai.","French" ))%>%
  mutate(ol1_used=str_replace_all(ol1_used, "french","French" ))%>%
  mutate(ol2_used=tolower(ol2_used))%>%
  mutate(ol2_used=str_replace_all(ol2_used, "anglais","English" ))%>%
  mutate(ol2_used=str_replace_all(ol2_used, "english","English" ))%>%
  mutate(ol2_used=str_replace_all(ol2_used, "fran[çc]ai.","French" ))%>%
  mutate(ol2_used=str_replace_all(ol2_used, "french","French" ))%>%
  mutate(ol3_used=tolower(ol3_used))%>%
  mutate(ol3_used=str_replace_all(ol3_used, "anglais","English" ))%>%
  mutate(ol3_used=str_replace_all(ol3_used, "english","English" ))%>%
  mutate(ol3_used=str_replace_all(ol3_used, "fran[çc]ai.","French" ))%>%
  mutate(ol3_used=str_replace_all(ol3_used, "french","French" ))


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

#Step 3: Exclude children beyond 4 years of age
#Age calculation 
langatt_df_eng$age_days_at_test = with(langatt_df_eng, 
                                       day(as.period(as.Date(timestamp) - as.Date(clean_dobs) , unit="months"))) 
langatt_df_fr$age_days_at_test = with(langatt_df_fr, 
                                      day(as.period(as.Date(timestamp) - as.Date(clean_dobs) , unit="months"))) 


# Remove children with age_at_test of more than 1461 days (4 years)
langatt_df_eng<-langatt_df_eng %>%
  filter(age_days_at_test<=1461)

langatt_df_fr<-langatt_df_fr %>%
  filter(age_days_at_test<=1461)

#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "3", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "3", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step3<-langatt_df_eng
langatt_df_fr_step3<-langatt_df_fr


#Create a df of new variables and descriptions, add clean_dobs 
new_vars<-tibble(new_var_name=c('clean_dobs'),
                 new_var_description=c('Clean child date of birth in month-year format'))

#Step 4: Coding for location + exclusion of ineligible participants based on location

#Remove participants who did not answer the question
langatt_df_eng<-filter(langatt_df_eng, is.na(address)==FALSE)
langatt_df_fr<-filter(langatt_df_fr, is.na(address)==FALSE)

#Create new variable with place names in lowercase with dashes replaced with spaces and accented characters removed
#remove leading and trailing spaces
langatt_df_eng<- langatt_df_eng %>%
  mutate(clean_address=tolower(address))%>%
  mutate(clean_address=str_replace_all(clean_address,"-"," "))%>%
  mutate(clean_address=str_replace_all(clean_address,"\\."," "))%>%
  mutate(clean_address=str_replace_all(clean_address, "^\\s+|\\s+$", ""))

langatt_df_eng$clean_address<-stri_trans_general(langatt_df_eng$clean_address, "Latin-ASCII")

langatt_df_eng<-langatt_df_eng %>%
  mutate(clean_address=str_replace_all(clean_address,"\\(qc\\)",""))%>%
  mutate(clean_address=str_replace_all(clean_address, ", qc", ""))%>%
  mutate(clean_address=str_replace_all(clean_address,".* montreal","montreal"))%>%
  mutate(clean_address=str_replace_all(clean_address,"\\bst\\s","saint ")) %>% #this line was messing up some addresses before so changed it to st followed by a space
  mutate(clean_address=str_replace_all(clean_address,"\\bst[:punct:]","saint ")) %>% #now do the same but for st followed by any punctuation
  mutate(clean_address=str_replace_all(clean_address,"\\bsaint\\s\\s","saint ")) %>% #now fix the ones that got 2 white spaces
  mutate(clean_address=str_replace_all(clean_address,"\\bste\\s","sainte ")) %>% #now fix ste to be sainte
  mutate(clean_address=str_replace(clean_address, " \\s*\\([^\\)]+\\)", "")) %>% #HK added these lines to fix a couple others
  mutate(clean_address=gsub(",.*", "", clean_address)) %>%
  mutate(clean_address=str_replace(clean_address, "qc city", "quebec city")) %>%
  mutate(clean_address=str_replace(clean_address, "mtl", "montreal")) %>%
  mutate(clean_address = case_when(str_detect(clean_address, "montreal") ~ "montreal",
                                   TRUE ~ clean_address))

#Same for French
langatt_df_fr<- langatt_df_fr %>%
  mutate(clean_address=tolower(address)) %>%
  mutate(clean_address=str_replace_all(clean_address,"-"," "))%>%
  mutate(clean_address=str_replace_all(clean_address,"\\."," "))%>%
  mutate(clean_address=str_replace_all(clean_address, "^\\s+|\\s+$", ""))

langatt_df_fr$clean_address<-stri_trans_general(langatt_df_fr$clean_address, "Latin-ASCII")

langatt_df_fr<-langatt_df_fr %>%
  mutate(clean_address=str_replace_all(clean_address,"\\(qc\\)",""))%>%
  mutate(clean_address=str_replace_all(clean_address, ", qc", ""))%>%
  mutate(clean_address=str_replace_all(clean_address,".* montreal","montreal"))%>%
  mutate(clean_address=str_replace_all(clean_address,"\\bst\\s","saint ")) %>% #this line was messing up some addresses before so changed it to st followed by a space
  mutate(clean_address=str_replace_all(clean_address,"\\bst[:punct:]","saint ")) %>% #now do the same but for st followed by any punctuation
  mutate(clean_address=str_replace_all(clean_address,"\\bsaint\\s\\s","saint ")) %>% #now fix the ones that got 2 white spaces
  mutate(clean_address=str_replace_all(clean_address,"\\bste\\s","sainte ")) %>% #now fix ste to be sainte
  mutate(clean_address=str_replace(clean_address, " \\s*\\([^\\)]+\\)", "")) %>% #HK added these lines to fix a couple others
  mutate(clean_address=gsub(",.*", "", clean_address)) %>%
  mutate(clean_address=str_replace(clean_address, "qc city", "quebec city")) %>%
  mutate(clean_address=str_replace(clean_address, "mtl", "montreal")) %>%
  mutate(clean_address = case_when(str_detect(clean_address, "montreal") ~ "montreal",
                                   TRUE ~ clean_address))

#Clean places in place names df 
place_names <- place_names %>%
  mutate(across(everything(), tolower))

#Note: It is possible that this step won't work on Mac systems
#remove accented characters
place_names$mtl_arr<-stri_trans_general(place_names$mtl_arr, "Latin-ASCII")
place_names$mtl_neigh<-stri_trans_general(place_names$mtl_neigh, "Latin-ASCII")
place_names$montreal_mun<-stri_trans_general(place_names$montreal_mun, "Latin-ASCII")
place_names$greater_mtl_mun<-stri_trans_general(place_names$greater_mtl_mun, "Latin-ASCII")
place_names$quebec_places<-stri_trans_general(place_names$quebec_places, "Latin-ASCII")

#replace hyphens with spaces:
place_names <- place_names %>%
  mutate(mtl_neigh=str_replace_all(mtl_neigh,"-"," "))%>%
  mutate(mtl_arr=str_replace_all(mtl_arr,"-"," "))%>% 
  mutate(montreal_mun=str_replace_all(montreal_mun,"-"," "))%>%
  mutate(greater_mtl_mun=str_replace_all(greater_mtl_mun,"-"," "))%>%
  mutate(quebec_places=str_replace_all(quebec_places,"-"," "))

#Create four new binary variables based on address: 1) is in Montreal neighborhoods / arrondissements 
#2) is in Greater Montreal and 3) is outside MTL but in Quebec and 4) outside of Quebec
langatt_df_eng<- langatt_df_eng %>%
  mutate(in_MTL=ifelse(clean_address %in% place_names$mtl_arr | clean_address %in% place_names$mtl_neigh | clean_address %in% place_names$montreal_mun, TRUE, FALSE))%>%
  mutate(great_MTL=ifelse(clean_address %in% place_names$greater_mtl_mun & in_MTL==FALSE,TRUE, FALSE))%>%
  mutate(in_QC=ifelse(clean_address %in%  place_names$quebec_places & great_MTL==FALSE & in_MTL==FALSE,TRUE, FALSE))%>%
  mutate(out_QC=ifelse(in_QC==FALSE & great_MTL==FALSE & in_MTL==FALSE, TRUE, FALSE))

langatt_df_fr<- langatt_df_fr %>%
  mutate(in_MTL=ifelse(clean_address %in% place_names$mtl_arr | clean_address %in% place_names$mtl_neigh |clean_address %in% place_names$montreal_mun, TRUE, FALSE))%>%
  mutate(great_MTL=ifelse(clean_address %in% place_names$greater_mtl_mun & in_MTL==FALSE, TRUE, FALSE))%>%
  mutate(in_QC=ifelse(clean_address %in%  place_names$quebec_places & great_MTL==FALSE & in_MTL==FALSE,TRUE, FALSE))%>%
  mutate(out_QC=ifelse(in_QC==FALSE & great_MTL==FALSE & in_MTL==FALSE, TRUE, FALSE))

#Visually inspect and hand correct categorization of places deemed outside Quebec 

#-----------------------------------ENGLISH
langatt_df_eng %>% 
  filter(out_QC == TRUE) %>%
  dplyr::select(id, clean_address, in_MTL, great_MTL, in_QC, out_QC) %>% View() #check IDs to fix

out_QC_df_eng_fixed<-langatt_df_eng %>%
  filter(out_QC==TRUE)%>%
  mutate(out_QC = case_when(id %in% c("eng_5") ~ TRUE, # keep this one listed as TRUE for "out_QC" since they are in Poland
                            TRUE ~ FALSE), #the rest should be labeled FALSE since they are all places in the province
         in_MTL = case_when(id %in% c("eng_89", "eng_435") ~ TRUE, #these ones are in Montreal so label them TRUE
                            TRUE ~ FALSE), #these ones are not in Montreal so label them FALSE
         great_MTL = case_when(id %in% c("eng_200", "eng_222", "eng_258", "eng_290", "eng_412", "eng_447", "eng_468", 
                                         "eng_527", "eng_538", "eng_543", "eng_560", "eng_597", "eng_627") ~ TRUE, #these ones are in Greater Montreal so label them TRUE
                               TRUE ~ FALSE), #These ones are not in Greater Montreal so label them FALSE
         in_QC = case_when(id %in% c("eng_210", "eng_220", "eng_227", "eng_229", "eng_256", "eng_355", "eng_376", "eng_466", 
                                     "eng_449", "eng_513", "eng_530", "eng_566", "eng_582", "eng_622", "eng_625", "eng_659", "eng_578") ~ TRUE, #these ones are in the provice of Quebec, so TRUE
                           TRUE ~ FALSE))#not in Quebec, so label them FALSE

#-----------------------------------FRENCH

langatt_df_fr %>% 
  filter(out_QC == TRUE) %>%
  dplyr::select(id, clean_address, in_MTL, great_MTL, in_QC, out_QC) %>% View() #check IDs to fix

out_QC_df_fr_fixed<-langatt_df_fr %>%
  filter(out_QC==TRUE)%>%
  mutate(out_QC = FALSE, #all participants are in Quebec, so make this whole column false
         in_MTL = case_when(id %in% c("fr_24", "fr_78", "fr_164", "fr_241") ~ TRUE, #these ones are in Montreal so label them TRUE
                            TRUE ~ FALSE), #these ones are not in Montreal so label them FALSE
         great_MTL = case_when(id %in% c("fr_129", "fr_", "fr_142", "fr_162", "fr_163", "fr_228", 
                                         "fr_277", "fr_278", "fr_286", "fr_305", "fr_379", "fr_435") ~ TRUE, #these ones are in Greater Montreal so label them TRUE
                               TRUE ~ FALSE), #These ones are not in Greater Montreal so label them FALSE
         in_QC = case_when(id %in% c("fr_1", "fr_31", "fr_53", "fr_64", "fr_99", "fr_175", "fr_214", "fr_226", 
                                     "fr_255", "fr_266", "fr_307", "fr_333", "fr_334", "fr_337", "fr_339", "fr_357", "fr_427", "fr_433") ~ TRUE, #these ones are in the provice of Quebec, so TRUE
                           TRUE ~ FALSE))#not in Quebec, so label them FALSE


#From main data frame, remove the rows from the fixed file, then bind the corrected data frame back into the main one
langatt_df_eng<-langatt_df_eng%>%
  filter(id %notin% out_QC_df_eng_fixed$id)%>%
  bind_rows(out_QC_df_eng_fixed)

langatt_df_fr<-langatt_df_fr%>%
  filter(id %notin% out_QC_df_fr_fixed$id)%>%
  bind_rows(out_QC_df_fr_fixed)

#delete participants who do not currently live in the province of Quebec
langatt_df_eng<-filter(langatt_df_eng, out_QC==FALSE)
langatt_df_fr<-filter(langatt_df_fr, out_QC==FALSE)


#Add clean_address to the variable names document 
new_vars <- new_vars %>%
  bind_rows(tribble(
    ~new_var_name,     ~new_var_description,
    "clean_address",   "address made lowercase special characters removed",
    "in_MTL",          "binary variable for address in MTL neighborhoods or arrondissements",
    "great_MTL",       "binary variable for address in Greater Montreal",
    "in_QC",           "binary variable for address outside of MTL but in QC",
    "out_QC",          "binary variable for address outside of QC"))


#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "4", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "4", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step4<-langatt_df_eng
langatt_df_fr_step4<-langatt_df_fr

#Step 5: Coding for languages with which the child is being raised + exclusion of participants based on their child being raised monolingually
#Remove NAs for Q18 (child_languages)
langatt_df_eng<-filter(langatt_df_eng, is.na(child_languages)==FALSE)
langatt_df_fr<-filter(langatt_df_fr, is.na(child_languages)==FALSE)

langatt_df_eng <- langatt_df_eng %>%
  
  #------------------------------Does child have English?
  mutate(has_en = case_when(eng_freq > 2 ~ 1, #if En frequency is > 2, then child has English
                            str_detect(caregiver2_used, "English") ~ 1, #if En listed for caregiver 2, then child has English
                            (ol1_used == "English" & ol1_freq > 2) | (ol2_used == "English" & ol2_freq > 2) | (ol3_used == "English" & ol3_freq > 2) ~ 1, #if English is listed as an "other" language AND the frequency is > 2, then child has English
                            TRUE ~ 0), #otherwise, child does not have English
         
         #------------------------------Does child have French?
         has_fr = case_when(fr_freq > 2 ~ 1, #if Fr listed under child_languages OR Fr frequency is > 2, then child has French
                            str_detect(caregiver2_used, "French") ~ 1, #if Fr listed for caregiver 2, then child has French
                            (ol1_used == "French" & ol1_freq > 2) | (ol2_used == "French" & ol2_freq > 2) | (ol3_used == "French" & ol3_freq > 2) ~ 1, #if French is listed as an "other" language AND the frequency is > 2, then child has French
                            TRUE ~ 0), #Otherwise, child does not have French
         
         #------------------------------Does child have Other?
         has_OL = case_when(((!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2) |
                               (!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2) |
                               (!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2)) ~ 1, #if ol1, ol2 and ol3 are not NA and are not English or French AND the frequence is > 2, then child has Other
                            #if Other listed under child_languages, then child has Other
                            str_detect(caregiver2_used, "other") ~ 1, #if Other listed under caregiver2, then child has Other
                            (!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2) | 
                              (!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2) |(!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2) ~ 1, #if ol1, ol2 and ol3 are not NA and are not English or French AND the frequence is > 2, then child has Other
                            TRUE ~ 0),
         has_OL1 = case_when(!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2 ~1,
                             child_languages == "One other language" ~ 1,
                             TRUE~0),
         has_OL2 = case_when(!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2~1,
                             TRUE~0),
         has_OL3 = case_when(!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2~1,
                             TRUE~0),
         num_OL_caregiver1 = has_OL1 + has_OL2 + has_OL3,
         caregiver2_OLonly = case_when(str_detect(caregiver2_used, "other") == FALSE ~ 0,
                                       caregiver2_used =="One other language" ~ 1,
                                       caregiver2_used =="Several other languages" ~ 0,
                                       TRUE~0)) #otherwise, child does not have Other


langatt_df_fr <- langatt_df_fr %>%
  #------------------------------Does child have English?
  mutate(has_en = case_when(eng_freq > 2 ~ 1, #if En frequency is > 2, then child has English
                            str_detect(caregiver2_used, "English") ~ 1, #if En listed for caregiver 2, then child has English
                            (ol1_used == "English" & ol1_freq > 2) | (ol2_used == "English" & ol2_freq > 2) | (ol3_used == "English" & ol3_freq > 2) ~ 1, #if English is listed as an "other" language AND the frequency is > 2, then child has English
                            TRUE ~ 0), #otherwise, child does not have English
         
         #------------------------------Does child have French?
         has_fr = case_when(fr_freq > 2 ~ 1, #if Fr listed under child_languages OR Fr frequency is > 2, then child has French
                            str_detect(caregiver2_used, "French") ~ 1, #if Fr listed for caregiver 2, then child has French
                            (ol1_used == "French" & ol1_freq > 2) | (ol2_used == "French" & ol2_freq > 2) | (ol3_used == "French" & ol3_freq > 2) ~ 1, #if French is listed as an "other" language AND the frequency is > 2, then child has French
                            TRUE ~ 0), #Otherwise, child does not have French
         
         #------------------------------Does child have Other?
         has_OL = case_when(((!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2) |
                               (!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2) |
                               (!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2)) ~ 1, #if ol1, ol2 and ol3 are not NA and are not English or French AND the frequence is > 2, then child has Other
                            #if Other listed under child_languages, then child has Other
                            str_detect(caregiver2_used, "other") ~ 1, #if Other listed under caregiver2, then child has Other
                            (!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2) | 
                              (!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2) |(!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2) ~ 1, #if ol1, ol2 and ol3 are not NA and are not English or French AND the frequence is > 2, then child has Other
                            TRUE ~ 0),
         has_OL1 = case_when(!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2 ~1,
                             child_languages == "One other language" ~ 1,
                             TRUE~0),
         has_OL2 = case_when(!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2~1,
                             TRUE~0),
         has_OL3 = case_when(!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2~1,
                             TRUE~0),
         num_OL_caregiver1 = has_OL1 + has_OL2 + has_OL3,
         caregiver2_OLonly = case_when(str_detect(caregiver2_used, "other") == FALSE ~ 0,
                                       caregiver2_used =="One other language" ~ 1,
                                       caregiver2_used =="Several other languages" ~ 0,
                                       TRUE~0))

langatt_df_eng<- langatt_df_eng %>%
  mutate(mono_en=ifelse(has_en==TRUE & has_fr==FALSE & has_OL==FALSE, TRUE,FALSE)) 

langatt_df_fr<- langatt_df_fr %>%
  mutate(mono_en=ifelse(has_en==TRUE & has_fr==FALSE & has_OL==FALSE, TRUE,FALSE)) 


langatt_df_eng<- langatt_df_eng %>%
  mutate(mono_fr=ifelse(has_en==FALSE & has_fr==TRUE & has_OL==FALSE, TRUE,FALSE)) 

langatt_df_fr<- langatt_df_fr %>%
  mutate(mono_fr=ifelse(has_en==FALSE & has_fr==TRUE & has_OL==FALSE, TRUE,FALSE)) 


langatt_df_eng<- langatt_df_eng %>%
  mutate(mono_OL=ifelse(has_en==FALSE & has_fr==FALSE & num_OL_caregiver1==1 & caregiver2_OLonly ==1, TRUE,FALSE)) 

langatt_df_fr<- langatt_df_fr %>%
  mutate(mono_OL=ifelse(has_en==FALSE & has_fr==FALSE & num_OL_caregiver1==1 & caregiver2_OLonly ==1, TRUE,FALSE)) 


#Filter for children who do not have monolingual exposure, i.e. mono_fr_eng or mono_OL are both false. 
langatt_df_eng<-filter(langatt_df_eng, mono_fr==FALSE & mono_OL==FALSE & mono_en==FALSE)
langatt_df_fr<-filter(langatt_df_fr, mono_fr==FALSE & mono_OL==FALSE & mono_en==FALSE)


#Add new column names to the variable names document 
new_vars <- new_vars %>%
  bind_rows(tribble(
    ~new_var_name,            ~new_var_description,
    "has_OL",                 "being raised with a language other than French or English",
    "has_eng", "has exposure to English",
    "has_fr",            "has exposure to French",
    "mono_OL",               "being rasied monolingually in a language other than French or English",
    "mono_fr", "being raised with French only",
    "mono_en", "being raised with English only"))


#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "5", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "5", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step5<-langatt_df_eng
langatt_df_fr_step5<-langatt_df_fr


#Step 6: Coding for developmental issues that could affect language development
#Delete participants who did not answer Q15 - Does your child have any developmental or health issues that might affect their language
#development? 
langatt_df_eng<-filter(langatt_df_eng, is.na(langdevissue_yn)==FALSE)
langatt_df_fr<-filter(langatt_df_fr, is.na(langdevissue_yn)==FALSE)

#Make a df of children with dev issues for visual inspection 
eng_dev_issues<-subset(langatt_df_eng, langatt_df_eng$langdevissue_yn %in% c("Yes", "I am not sure"), drop = TRUE)
fr_dev_issues<-subset(langatt_df_fr, langatt_df_fr$langdevissue_yn %in% c("Oui", "Je ne suis pas certain(e)"), drop = TRUE)
alldevissues <- rbind(eng_dev_issues, fr_dev_issues)

dplyr::select(alldevissues, id, langdevissue_yn,langdevissue_details) %>% view()

#Make a new variable has_DL for children who have a developmental issue (eng_635, eng_641 and fr_265 excluded by other criteria): 
dev_issue_ids<-c("eng_67", "eng_126", "eng_182", "eng_441", "eng_627", "eng_635",
                 "eng_641", "fr_19", "fr_97", "fr_132", "fr_140", "fr_225", "fr_241",
                 "fr_265", "fr_318", "fr_348","fr_354", "fr_398", "fr_403", "fr_429")

langatt_df_eng<- langatt_df_eng %>%
  mutate(has_DL=ifelse(id %in% dev_issue_ids, 1, 0))

langatt_df_fr<- langatt_df_fr %>%
  mutate(has_DL=ifelse(id %in% dev_issue_ids, 1, 0))


#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "6", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "6", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step6<-langatt_df_eng
langatt_df_fr_step6<-langatt_df_fr

#Step 7: Exclusion of ineligible participants based on largely incomplete questionnaires
#Exclude participants who have not completed 7 of 9 concerns items
concern_column_names<-c("concern_general", "concern_exposure", "concern_help", "concern_challenging","concern_confused",
                        "concern_distinguish","concern_shrt_delay","concern_lng_prob","concern_prep_school","concern_fluent")
langatt_df_eng<-filter(langatt_df_eng,(apply(is.na(langatt_df_eng[concern_column_names]),1,sum)<3))
langatt_df_fr<-filter(langatt_df_fr,(apply(is.na(langatt_df_fr[concern_column_names]),1,sum)<3))

#Exclude participants who have not answered demographic and Fr-Eng background questions
oblig_column_names<-c("address","parent_age", "child_dob", "parent1_L1", "eng_spk", "fr_spk",
                      "eng_freq", "fr_freq", "caregiver2_used")

langatt_df_eng<-filter(langatt_df_eng,(apply(is.na(langatt_df_eng[oblig_column_names]),1,sum)<1))
langatt_df_fr<-filter(langatt_df_fr,(apply(is.na(langatt_df_fr[oblig_column_names]),1,sum)<1))

#Exclude participants without proficiency in HL info, create a new variable "no_HL_prof"
langatt_df_eng <- langatt_df_eng %>%
  mutate(no_HL_prof = case_when(!is.na(parent1_OL1) & is.na(ol1_spk) & is.na(ol1_und) ~ 1,
                                TRUE ~ 0)) %>%
  filter(no_HL_prof == 0)

langatt_df_fr <- langatt_df_fr %>%
  mutate(no_HL_prof = case_when(!is.na(parent1_OL1) & is.na(ol1_spk) & is.na(ol1_und) ~ 1,
                                TRUE ~ 0)) %>%
  filter(no_HL_prof == 0)
# #No one excluded for this step

#Keep track of n by steps
data_points_n_per_step_eng <- data_points_n_per_step_eng %>%
  bind_rows(tibble(step = "7", n_rows = nrow(langatt_df_eng)))
data_points_n_per_step_fr <- data_points_n_per_step_fr %>%
  bind_rows(tibble(step = "7", n_rows = nrow(langatt_df_fr)))
langatt_df_eng_step7<-langatt_df_eng
langatt_df_fr_step7<-langatt_df_fr


#Write all intermediate dfs to xl files 

#write the n per step files
write_csv(data_points_n_per_step_eng, here("concerns/data/data_points_n_per_step_eng.csv"))
write_csv(data_points_n_per_step_fr, here("concerns/data/data_points_n_per_step_fr.csv"))

#save final clean dataset to be loaded into analysis script
write_csv(langatt_df_eng_step6, here("concerns/data/langatt_df_eng_step_6.csv"))
write_csv(langatt_df_fr_step6, here("concerns/data/langatt_df_fr_step_6.csv"))
write_csv(langatt_df_eng_step7, here("concerns/data/langatt_df_eng_clean_concerns.csv"))
write_csv(langatt_df_fr_step7, here("concerns/data/langatt_df_fr_clean_concerns.csv"))
langatt_df_eng %>% 
  bind_rows(langatt_df_fr)%>%
  write_csv(here("concerns/data/langatt_df_both_clean_concerns.csv"))

#Write new_vars file 
write_csv(new_vars, here("concerns/data/new_vars_concerns.csv"))

