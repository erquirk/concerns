library(dplyr)
library(stringr)
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(SmartEDA)
library(lubridate)

#Load dataframe with combined French and English version data after all cleaning steps
langatt_df_concerns_clean <- read_csv(here("concerns/data/langatt_df_both_clean_concerns.csv"))

#keep the old child languages data to distinguish one from many other languages in OPOL calculation
langatt_df_concerns_clean$child_languages_old<-langatt_df_concerns_clean$child_languages
langatt_df_concerns_clean$caregiver2_old<-langatt_df_concerns_clean$caregiver2_used

#Collapse several and one other language categories
langatt_df_concerns_clean<-langatt_df_concerns_clean%>%
  mutate(child_languages=str_replace_all(child_languages, "One other language","Other" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Several other languages","Other" ))%>%
  mutate(child_languages=str_replace_all(child_languages, "Other;Other","Other" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "One other language","Other" ))%>%
  mutate(caregiver2_used=str_replace_all(caregiver2_used, "Several other languages","Other" ))


##FREQUENCY OF LANGUAGES HEARD BY CHILD

langatt_df_concerns_clean_langgroups <- langatt_df_concerns_clean %>%
  #dplyr::select(id,gender, child_languages,caregiver2_used,ol1_used,ol2_used,ol3_used,eng_freq,fr_freq,ol1_freq,ol2_freq,ol3_freq) %>%
  
  #------------------------------Does child have English?
  mutate(has_en = case_when(eng_freq > 2 ~ 1, #if En listed under child_languages AND En frequency is > 2, then child has English
                            str_detect(caregiver2_used, "English") ~ 1, #if En listed for caregiver 2, then child has English
                            (ol1_used == "English" & ol1_freq > 2) | (ol2_used == "English" & ol2_freq > 2) | (ol3_used == "English" & ol3_freq > 2) ~ 1, #if English is listed as an "other" language AND the frequency is > 2, then child has English
                            TRUE ~ 0), #otherwise, child does not have English
         
         #------------------------------Does child have French?
         has_fr = case_when(fr_freq > 2 ~ 1, #if Fr listed under child_languages OR Fr frequency is > 2, then child has French
                            str_detect(caregiver2_used, "French") ~ 1, #if Fr listed for caregiver 2, then child has French
                            (ol1_used == "French" & ol1_freq > 2) | (ol2_used == "French" & ol2_freq > 2) | (ol3_used == "French" & ol3_freq > 2) ~ 1, #if French is listed as an "other" language AND the frequency is > 2, then child has French
                            TRUE ~ 0), #Otherwise, child does not have French
         
         #------------------------------Does child have Other?
         has_other = case_when((!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2) |
                                  (!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2) |
                                  (!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2) ~ 1, #if ol1, ol2 and ol3 are not NA and are not English or French AND the frequence is > 2, then child has Other
                               #if Other listed under child_languages, then child has Other
                               str_detect(caregiver2_used, "Other") ~ 1, #if Other listed under caregiver2, then child has Other
                               TRUE ~ 0), #otherwise, child does not have Other
         lang_group = case_when(has_en + has_fr == 2 & has_other == 0 ~ "en_fr",
                                has_en + has_other == 2 & has_fr == 0 ~ "en_other",
                                has_fr + has_other == 2 & has_en == 0 ~ "fr_other",
                                has_other == 1 & has_en + has_fr == 0 ~ "other_other",
                                has_en + has_fr + has_other == 3 ~ "en_fr_other"))


exposure_stats<-langatt_df_concerns_clean_langgroups %>%
  group_by(lang_group) %>%
  summarise(num = length(lang_group)) %>%
  mutate(pct = num/sum(num),
         pct_rounded = paste0(round(pct*100, digits = 1), "%")) %>%
  ungroup() %>%
  arrange(desc(num)) %>%
  add_row(lang_group = "TOTAL", num = sum(.$num), pct = sum(.$pct), pct_rounded = paste0(round(pct*100, digits = 1), "%"))

#Generate stats for how parents are raising their children (Q16)
raising_stats<-langatt_df_concerns_clean %>%
  mutate(family_config=tolower(family_config))%>%
  mutate(family_config=str_replace_all(family_config, "seul\\(e\\)", "alone"))%>%
  mutate(family_config=str_replace_all(family_config, "avec un\\(e\\) partenaire qui vit dans le même foyer","with a partner who lives in the same household"))%>%
  mutate(family_config=str_replace_all(family_config, "avec un\\(e\\) partenaire qui vit dans un autre foyer", "with a partner who lives in a different household"))%>%
  mutate(family_config=str_replace_all(family_config, "avec mon/mes parent\\(s\\) qui vit/vivent dans le même foyer", "with my parent(s) who live(s) in the same household"))%>%
  mutate(family_config=str_replace_all(family_config, "avec mon/mes parent\\(s\\) qui vit/vivent dans un autre foyer", "with my parent(s) who live(s) in a different household"))%>%
  count(family_config, name = "num") %>%
  mutate(pct = num/sum(num),
         pct_rounded = paste0(round(pct*100, digits = 1), "%")) %>%
  ungroup() %>%
  arrange(desc(num)) %>%
  add_row(family_config = "TOTAL", num = sum(.$num), pct = sum(.$pct), pct_rounded = paste0(round(pct*100, digits = 1), "%"))

#One parent one language
langatt_df_concerns_clean <- langatt_df_concerns_clean %>%
  mutate(caregiver1_eng=case_when(eng_freq>2|(ol1_used == "English" & ol1_freq > 2) | (ol2_used == "English" & ol2_freq > 2) | (ol3_used == "English" & ol3_freq > 2)~1,TRUE~0))%>%
  mutate(caregiver1_fr=case_when(fr_freq>2|(ol1_used == "French" & ol1_freq > 2) | (ol2_used == "French" & ol2_freq > 2) | (ol3_used == "French" & ol3_freq > 2)~1,TRUE~0))%>%
  mutate(caregiver1_other1=case_when(!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2~1, TRUE~0))%>%
  mutate(caregiver1_other2=case_when(!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2~1, TRUE~0))%>% 
  mutate(caregiver1_other3=case_when(!is.na(ol3_used) & ol3_used != "English" & ol3_used != "French" & ol3_freq > 2~1, TRUE~0))%>%
  mutate(caregiver1_onelang=case_when(caregiver1_eng+caregiver1_fr+caregiver1_other1+caregiver1_other2+caregiver1_other3==1~1,TRUE~0))%>%
  mutate(caregiver1_language=case_when(caregiver1_onelang==1 & caregiver1_eng==1~"English", caregiver1_onelang==1 & caregiver1_fr==1~"French",
                                       caregiver1_onelang==1 & caregiver1_other1==1~ol1_used, caregiver1_onelang==1 & caregiver1_other2==1~ol2_used,
                                       caregiver1_onelang==1 & caregiver1_other3==1~ol3_used,TRUE~"Multiple"))%>%
  mutate(caregiver2_onelang=case_when(caregiver2_used=="English"|caregiver2_used=="French"|caregiver2_used=="Other"~1,TRUE~0))%>%
  mutate(same_caregiver1_2 = if_else(caregiver1_language == caregiver2_used, 1, 0))%>%
  mutate(one_otherlang_only=case_when(caregiver2_used=="Other" & caregiver1_eng==0 & caregiver1_fr==0 & caregiver1_other1+caregiver1_other2+caregiver1_other3==1 
                                      & child_languages_old=="One other language" ~1,TRUE~0))%>%
  mutate(opol=case_when(caregiver2_onelang==1 & caregiver1_onelang==1 & same_caregiver1_2==0 & one_otherlang_only==0~1,TRUE~0))


#Add language group to the main data fram
langatt_df_concerns_clean <- langatt_df_concerns_clean %>%
  mutate(lang_group = case_when(has_en + has_fr == 2 & has_OL == 0 ~ "en_fr",
                                has_en + has_OL == 2 & has_fr == 0 ~ "en_other",
                                has_fr + has_OL == 2 & has_en == 0 ~ "fr_other",
                                has_OL == 1 & has_en + has_fr == 0 ~ "other_other",
                                has_en + has_fr + has_OL == 3 ~ "en_fr_other"))


#Clean languages data for determining multilingualism of the parent
langatt_df_concerns_clean <- langatt_df_concerns_clean %>%
  mutate(parent1_L1=str_replace_all(parent1_L1, "Swiss-German", "Swiss German")) %>%
  mutate(parent1_L1=str_replace_all(parent1_L1, "Chinese///Mandarin", "Mandarin"))%>%
  mutate(parent1_L1=str_replace_all(parent1_L1, '\\(born in Montreal, raised in Ontario, UK parents\\)',''))%>%
  mutate(multilingual_parent = case_when(str_detect(parent1_L1,';|,| and | et | puis |Français anglais|&|-|:|\\/') ~ "TRUE",TRUE~"FALSE"))

# Determine if French, English and other in parents L1
langatt_df_concerns_clean_stats <- langatt_df_concerns_clean %>%
#clean up parent1_L1
  mutate(parent1_L1=tolower(parent1_L1))%>%
  mutate(parent1_L1=str_replace_all(parent1_L1, ";|,| and | puis | et |&|-|:|\\/", ";"))%>%
  mutate(parent1_L1=str_replace_all(parent1_L1, "français anglais", "francais; anglais"))%>%
#make variables to base final categories on
  mutate(parent1_L1_fr=case_when(str_detect(parent1_L1,'fran|fren') ~ "1", TRUE~"0"))%>%
  mutate(parent1_L1_en=case_when(str_detect(parent1_L1,'engl|angl') ~ "1", TRUE~"0"))%>%
  mutate(parent1_L1_en_nofr=case_when(parent1_L1_en=="1" & parent1_L1_fr=="0"~"1",TRUE~"0"))%>%
  mutate(parent1_L1_fr_noen=case_when(parent1_L1_en=="0" & parent1_L1_fr=="1"~"1",TRUE~"0"))%>%
  mutate(number_add_languages=str_count(parent1_L1,";"))%>%
#make final categories 
  mutate(parent1_ol_only=case_when(parent1_L1_en=="0" & parent1_L1_fr=="0"~"1",TRUE~"0"))%>%
  mutate(parent1_L1_en_only=case_when(parent1_L1_en=="1" & number_add_languages=="0"~"1",TRUE~"0"))%>%
  mutate(parent1_L1_fr_only=case_when(parent1_L1_fr=="1" & number_add_languages=="0"~"1",TRUE~"0"))%>%
  mutate(parent1_L1_en_ol=case_when(parent1_L1_en_nofr=="1"& number_add_languages>0~"1",TRUE~"0"))%>%
  mutate(parent1_L1_fr_ol=case_when(parent1_L1_fr_noen=="1"& number_add_languages>0~"1",TRUE~"0"))%>%
  mutate(parent1_L1_en_fr_ol=case_when(parent1_L1_fr=="1"& parent1_L1_en=="1" & number_add_languages>1~"1", TRUE ~"0"))%>%
  mutate(parent1_L1_eng_fr_only=case_when(parent1_L1_fr=="1" & parent1_L1_en=="1"& number_add_languages=="1"~"1",TRUE~"0"))


parent_L1_stats<-dplyr::select(langatt_df_concerns_clean_stats, parent1_ol_only,parent1_L1_en_only, parent1_L1_fr_only,parent1_L1_en_ol, parent1_L1_fr_ol, parent1_L1_en_fr_ol,parent1_L1_eng_fr_only)

L1_stats<-ExpCustomStat(parent_L1_stats,Cvar=c('parent1_ol_only', 'parent1_L1_en_only', 'parent1_L1_fr_only','parent1_L1_en_ol', 'parent1_L1_fr_ol', 'parent1_L1_en_fr_ol','parent1_L1_eng_fr_only'),gpby=FALSE)

#rename binary True False to descriptive terms
langatt_df_concerns_clean<-langatt_df_concerns_clean%>%
  mutate(in_MTL=factor(in_MTL,c("TRUE","FALSE"),labels=c("Inside of Montreal","Elsewhere")))%>%
  mutate(great_MTL=factor(great_MTL,c("TRUE","FALSE"),labels=c("Inside Greater Montreal","Elsewhere")))%>%
  mutate(in_QC=factor(in_QC,c("TRUE","FALSE"),labels=c("In Quebec outside of Greater Montreal","Elsewhere")))%>%
  mutate(multilingual_parent=factor(multilingual_parent,c("TRUE","FALSE"),labels=c("Multilingual","Monolingual")))

#Sociodemographic descriptive stats for categorical
#Counts and proportions table
sociodemo_cat_df<-dplyr::select(langatt_df_concerns_clean,in_MTL, great_MTL, in_QC,gender,child_sex,multilingual_parent,child_languages,has_OL,education, opol, has_DL,family_config)
sociodemo_cat_df_stats<-ExpCustomStat(sociodemo_cat_df,Cvar=c('in_MTL', 'great_MTL', 'in_QC','gender','child_sex','multilingual_parent','child_languages','has_OL','education', 'opol', 'has_DL','family_config'),gpby=FALSE)

#Sociodemographic descriptive stats for numerical
#Convert age in days to months
langatt_df_concerns_clean<-langatt_df_concerns_clean%>%
  mutate(age_months_at_test=days(age_days_at_test)/months(1))
#Mean, SD, min, max table
sociodemo_num_df<-dplyr::select(langatt_df_concerns_clean, parent_age,age_months_at_test,eng_spk,fr_spk,ol1_spk, ol2_spk, ol3_spk)
sociodemo_num_df_stats<-ExpCustomStat(sociodemo_num_df,Nvar=c("parent_age","age_months_at_test",'eng_spk','fr_spk','ol1_spk','ol2_spk','ol3_spk'), stat=c('mean','sd','min','max'))

#Write tibbles and tables to files
write_csv(sociodemo_cat_df_stats, here("concerns/descriptive_stats/demo_stats_categorical_summary.csv"))
write_csv(sociodemo_num_df_stats, here("concerns/descriptive_stats/demo_stats_numerical_summary.csv"))
write_csv(langatt_df_concerns_clean, here("concerns/data/langatt_df_foranalyses.csv"))
write_csv(L1_stats, here("concerns/descriptive_stats/parent_L1_stats.csv"))


