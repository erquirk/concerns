library(dplyr)
library(here)
library(tidyverse)
library(nFactors)
library(psych)
library(broom)
library(GPArotation)
library(ltm)
library(Hmisc)
library(sjPlot)

#Input df for analyses
langatt_df_foranalyses <- read_csv(here("concerns/data/langatt_df_foranalyses.csv"))


langatt_df_concerns<-dplyr::select(langatt_df_foranalyses,concern_exposure,concern_help,concern_challenging,	
                            concern_confused, concern_distinguish,concern_shrt_delay,concern_lng_prob,
                            concern_prep_school,concern_fluent)

langatt_df_concerns=langatt_df_concerns[complete.cases(langatt_df_concerns),]

langatt_df_environment<-dplyr::select(langatt_df_foranalyses,concern_exposure,concern_help,concern_challenging,	
                                      concern_confused, concern_distinguish)
langatt_df_outcomes<-dplyr::select(langatt_df_foranalyses,concern_shrt_delay,concern_lng_prob,
                                   concern_prep_school,concern_fluent)

langatt_df_environment=langatt_df_environment[complete.cases(langatt_df_environment),]
langatt_df_outcomes=langatt_df_outcomes[complete.cases(langatt_df_outcomes),]

#Test of sphericity
print(cortest.bartlett(langatt_df_concerns))

#Kaiser Meyer Olkin test
KMO(langatt_df_concerns)

# Number of factors
fa.parallel(langatt_df_concerns)

# Principal Comp Analysis
test<-principal(langatt_df_concerns, nfactors=2, rotate="oblimin")
summary(test)
loadings(test)

# Cronbach's Alpha
# Cronbach's Alpha test 
library(ltm)
cronbach.alpha(langatt_df_environment)
cronbach.alpha(langatt_df_outcomes)


# Create a new variable for each factor that is a mean of the items that load onto them and a mean for all concerns items
langatt_df_foranalyses <- langatt_df_foranalyses %>%
  #group by id so the following calculated means apply to individual participants
  group_by(id) %>%
  #pivot longer all factor 1 columns to easily calculate means
  pivot_longer(cols = c(concern_challenging, concern_confused, concern_distinguish,
               concern_shrt_delay, concern_lng_prob, concern_prep_school), names_to = "factor1_cols", values_to = "factor1_vals") %>%
  mutate(factor1_mean=(sum(factor1_vals, na.rm = TRUE)/sum(!is.na(factor1_vals)))) %>%
  #pivot wider back to how it was
  pivot_wider(names_from = "factor1_cols", values_from = "factor1_vals") %>%
  #pivot longer all factor 2 columns to easily calculate means
  pivot_longer(cols = c(concern_exposure, concern_help, concern_fluent), names_to = "factor2_cols", values_to = "factor2_vals") %>%
  mutate(factor2_mean=(sum(factor2_vals, na.rm = TRUE)/sum(!is.na(factor2_vals)))) %>%
  #pivot wider back to how it was
  pivot_wider(names_from = "factor2_cols", values_from = "factor2_vals") %>%
  #pivot longer all concern columns to easily calculate means
  pivot_longer(cols = c(concern_challenging, concern_confused, concern_distinguish,
                          concern_shrt_delay, concern_lng_prob, concern_prep_school,
                          concern_exposure, concern_help, concern_fluent), names_to = "concern_cols", values_to = "concern_vals") %>%
  mutate(concern_mean=(sum(concern_vals, na.rm = TRUE)/sum(!is.na(concern_vals)))) %>%
  #pivot wider back to how it was
  pivot_wider(names_from = "concern_cols", values_from = "concern_vals") %>%
  #remove grouping
  ungroup()

#Exclude participants who did not complete 50% of the items for each factor
outcomes_column_names<-c('concern_challenging', 'concern_confused', 'concern_distinguish','concern_shrt_delay','concern_lng_prob','concern_prep_school')
environment_column_names<-c('concern_exposure', 'concern_help', 'concern_fluent')
langatt_df_foranalyses<-filter(langatt_df_foranalyses,(apply(is.na(langatt_df_foranalyses[outcomes_column_names]),1,sum)/6)<.5)
langatt_df_foranalyses<-filter(langatt_df_foranalyses,(apply(is.na(langatt_df_foranalyses[environment_column_names]),1,sum)/3)<.5)

#T test for short term vs long term delay concerns
wilcox.test(langatt_df_foranalyses$concern_shrt_delay,langatt_df_foranalyses$concern_lng_prob)
mean(langatt_df_foranalyses$concern_shrt_delay,na.rm=T)
mean(langatt_df_foranalyses$concern_lng_prob,na.rm=T)


#Calculate overall means and SDs by factor
mean(langatt_df_foranalyses$factor1_mean,na.rm=T)
sd(langatt_df_foranalyses$factor1_mean,na.rm=T)
mean(langatt_df_foranalyses$factor2_mean,na.rm=T)
sd(langatt_df_foranalyses$factor2_mean,na.rm=T)

# Test for data being normally distributed
with(langatt_df_foranalyses, shapiro.test(factor1_mean))
with(langatt_df_foranalyses, shapiro.test(factor2_mean))

#Use non-parametric test for difference in two means 
w_test<-wilcox.test(langatt_df_foranalyses$factor1_mean,langatt_df_foranalyses$factor2_mean)
w_test


#Create missing predictor variables
langatt_df_foranalyses<-langatt_df_foranalyses%>%
  mutate(caregiver1_other1=case_when(!is.na(ol1_used) & ol1_used != "English" & ol1_used != "French" & ol1_freq > 2~1, TRUE~0),
         caregiver1_other2=case_when(!is.na(ol2_used) & ol2_used != "English" & ol2_used != "French" & ol2_freq > 2~1, TRUE~0),
         multiple_OL=case_when(str_detect(caregiver2_old, "Several")|  (caregiver1_other1==1 & caregiver1_other2==1)~1,TRUE~0),
         eng_mult_others=case_when(multiple_OL==1 & has_en==1~1,TRUE~0),
         fr_mult_others=case_when(multiple_OL==1 & has_fr==1~1, TRUE~0),
         eng_fr_other=case_when(has_en==1 & has_fr==1 & has_OL==1~1,TRUE~0),
         trilingual=case_when(eng_mult_others==1 | fr_mult_others==1 | eng_fr_other==1~1,TRUE~0))%>%
  #group by ID so the following values will be for each participant
  group_by(id) %>%
  #pivot longer to get all cognitive items in one column so we can calculate mean easily
  pivot_longer(cols = c(view_schoolsuccess, view_betterlearner, view_reasoning, view_better_lang_learning, 
                        view_flex_think), names_to = "cognitive_cols", values_to = "cognitive_vals") %>%
  mutate(cognitive_mean = sum(cognitive_vals, na.rm = TRUE)/sum(!is.na(cognitive_vals))) %>%
  #pivot wider back to the way it was
  pivot_wider(names_from = "cognitive_cols", values_from = "cognitive_vals") %>%
  #pivot longer to get all status items in one column so we can calculate mean easily
  pivot_longer(cols = c(view_employment, view_compete_jobs, view_success_workworld), names_to = "status_cols", values_to = "status_vals") %>%
  mutate(status_mean = sum(status_vals, na.rm = TRUE)/sum(!is.na(status_vals))) %>%
  #pivot wider back to the way it was
  pivot_wider(names_from = "status_cols", values_from = "status_vals") %>%
  #pivot longer to get all solidarity items in one column so we can calculate mean easily
  pivot_longer(cols = c(view_heritage, view_comm_family, view_full_comm_member), names_to = "solidarity_cols", values_to = "solidarity_vals") %>%
  mutate(solidarity_mean = sum(solidarity_vals, na.rm = TRUE)/sum(!is.na(solidarity_vals))) %>%
  #pivot wider back to the way it was
  pivot_wider(names_from = "solidarity_cols", values_from = "solidarity_vals") %>%
  #remove grouping
  ungroup()
  
#Get mean attitude levels by dimensions and sd  
mean(langatt_df_foranalyses$cognitive_mean, na.rm=TRUE)
sd(langatt_df_foranalyses$cognitive_mean, na.rm=TRUE)
mean(langatt_df_foranalyses$status_mean, na.rm=TRUE)
sd(langatt_df_foranalyses$status_mean, na.rm=TRUE)
mean(langatt_df_foranalyses$solidarity_mean, na.rm=TRUE)
sd(langatt_df_foranalyses$solidarity_mean, na.rm=TRUE)

#Get number of trilinguals
table(langatt_df_foranalyses$trilingual)


#Create correlation matrix between all predictors and concerns factors 
langatt_df_foranalyses%>%
  dplyr::select(concern_mean,factor1_mean,factor2_mean,has_OL,has_DL, trilingual,multilingual_parent,cognitive_mean,status_mean, solidarity_mean)%>%View()
#Recode Multilingual parent to be numeric
langatt_df_foranalyses<-langatt_df_foranalyses%>%
  mutate(multilingual_parent=recode(multilingual_parent,"Monolingual"=0,"Multilingual"=1))

langatt_df_corrs<-dplyr::select(langatt_df_foranalyses,concern_mean, factor1_mean,factor2_mean,has_OL,trilingual,has_DL,
                                multilingual_parent,cognitive_mean,status_mean, solidarity_mean)


raw_matrix <- rcorr(as.matrix(langatt_df_corrs))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flat_matrix<-flattenCorrMatrix(raw_matrix$r,raw_matrix$P)
write.csv(flat_matrix,"concerns/data/concerns_corr_matrix.csv")


#Test independent contribution of Trilingualism and HL transmission on concerns overall 
fit_trilingual_overall<-lm(concern_mean~trilingual, data=langatt_df_foranalyses)
summary(fit_trilingual_overall)
fit_trilingualHL_overall<-lm(concern_mean~trilingual+has_OL, data=langatt_df_foranalyses)
anova(fit_trilingual_overall,fit_trilingualHL_overall)

#Test independent contribution of Trilingualism and HL transmission on concerns factor 1
fit_trilingual_factor1<-lm(factor1_mean~trilingual, data=langatt_df_foranalyses)
summary(fit_trilingual_factor1)
fit_trilingualHL_factor1<-lm(factor1_mean~trilingual+has_OL, data=langatt_df_foranalyses)
anova(fit_trilingual_factor1,fit_trilingualHL_factor1)

#Test independent contribution of Trilingualism and HL transmission on concerns factor 2
fit_trilingual_factor2<-lm(factor2_mean~trilingual, data=langatt_df_foranalyses)
summary(fit_trilingual_factor2)
fit_trilingualHL_factor2<-lm(factor2_mean~trilingual+has_OL, data=langatt_df_foranalyses)
anova(fit_trilingual_factor2,fit_trilingualHL_factor2)

langatt_df_attitudes_anova<-dplyr::select(langatt_df_foranalyses, concern_mean, factor1_mean, factor2_mean, cognitive_mean, solidarity_mean, status_mean)
langatt_df_attitudes_anova<-langatt_df_attitudes_anova[complete.cases(langatt_df_attitudes_anova),]


#ANOVAs for overall concern ~ attitudes models
fit_attitudes_gen_cog<-lm(concern_mean~cognitive_mean, data=langatt_df_foranalyses)
fit_attitudes_gen_cog_stat<-lm(concern_mean~cognitive_mean+status_mean, data=langatt_df_foranalyses)
anova(fit_attitudes_gen_cog,fit_attitudes_gen_cog_stat)
fit_attitudes_gen_cog_sol<-lm(concern_mean~cognitive_mean+solidarity_mean, data=langatt_df_foranalyses)
anova(fit_attitudes_gen_cog,fit_attitudes_gen_cog_sol)

#ANOVAs for cognition concern ~ attitudes models
fit_attitudes_f1_cog<-lm(factor1_mean~cognitive_mean, data=langatt_df_foranalyses)
fit_attitudes_f1_cog_stat<-lm(factor1_mean~cognitive_mean+status_mean, data=langatt_df_foranalyses)
anova(fit_attitudes_f1_cog,fit_attitudes_f1_cog_stat)
fit_attitudes_f1_cog_sol<-lm(factor1_mean~cognitive_mean+solidarity_mean, data=langatt_df_foranalyses)
anova(fit_attitudes_f1_cog,fit_attitudes_f1_cog_sol)

#ANOVAs for exposure-fluency concern ~ attitudes models
fit_attitudes_f2_cog<-lm(factor2_mean~cognitive_mean, data=langatt_df_foranalyses)
fit_attitudes_f2_cog_stat<-lm(factor2_mean~cognitive_mean+status_mean, data=langatt_df_foranalyses)
anova(fit_attitudes_f2_cog,fit_attitudes_f2_cog_stat)
fit_attitudes_f2_cog_sol<-lm(factor2_mean~cognitive_mean+solidarity_mean, data=langatt_df_foranalyses)
anova(fit_attitudes_f2_cog,fit_attitudes_f2_cog_sol)


#write.csv(langatt_df_foranalyses,"concerns/data/langatt_df_forfigures.csv" )


#Make table for all models

#Concerns from Trilingualism and HL acquisition 
#Overall 
tab_model(
  fit_trilingual_overall,
  pred.labels=c("Intercept","Trilingual Transmission"),
  dv.labels=c("Trilingual Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Tri_gen.html"
)

tab_model(
  fit_trilingualHL_overall,
  pred.labels=c("Intercept","Trilingual transmission", "HL transmission"),
  dv.labels=c("Trilingual Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Tri_HL_gen.html"
)

#Factor 1
tab_model(
  fit_trilingual_factor1,
  pred.labels=c("Intercept","Trilingual Transmission"),
  dv.labels=c("Trilingual Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Tri_f1.html"
)

tab_model(
  fit_trilingualHL_factor1,
  pred.labels=c("Intercept","Trilingual transmission", "HL transmission"),
  dv.labels=c("Trilingual Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Tri_HL_f1.html"
)

#Factor 2
tab_model(
  fit_trilingual_factor2,
  pred.labels=c("Intercept","Trilingual Transmission"),
  dv.labels=c("Trilingual Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Tri_f2.html"
)

tab_model(
  fit_trilingualHL_factor2,
  pred.labels=c("Intercept","Trilingual transmission", "HL transmission"),
  dv.labels=c("Trilingual Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Tri_HL_f2.html"
)

#Overall concern from attitudes

tab_model(
  fit_attitudes_gen_cog,
  pred.labels=c("Intercept","Attitudes: Cognitive Development"),
  dv.labels=c("Cognitive Development \nModel"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cog_gen.html"
)

tab_model(
  fit_attitudes_gen_cog_stat,
  pred.labels=c("Intercept","Attitudes: Cognitive Development","Attitudes: Status"),
  dv.labels=c("Cognitive Development + Status \nModel"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cogstat_gen.html"
)

tab_model(
  fit_attitudes_gen_cog_sol,
  pred.labels=c("Intercept","Attitudes: Cognitive Development","Attitudes: Solidarity"),
  dv.labels=c("Cognitive Development + \nSolidarity Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cogsol_gen.html"
)

#Factor 1

tab_model(
  fit_attitudes_f1_cog,
  pred.labels=c("Intercept","Attitudes: Cognitive Development"),
  dv.labels=c("Cognitive Development \nModel"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cog_f1.html"
)

tab_model(
  fit_attitudes_f1_cog_stat,
  pred.labels=c("Intercept","Attitudes: Cognitive Development","Attitudes: Status"),
  dv.labels=c("Cognitive Development + Status \nModel"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cogstat_f1.html"
)

tab_model(
  fit_attitudes_f1_cog_sol,
  pred.labels=c("Intercept","Attitudes: Cognitive Development","Attitudes: Solidarity"),
  dv.labels=c("Cognitive Development + \nSolidarity Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cogsol_f1.html"
)

#Factor 2

tab_model(
  fit_attitudes_f2_cog,
  pred.labels=c("Intercept","Attitudes: Cognitive Development"),
  dv.labels=c("Cognitive Development \nModel"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cog_f2.html"
)

tab_model(
  fit_attitudes_f2_cog_stat,
  pred.labels=c("Intercept","Attitudes: Cognitive Development","Attitudes: Status"),
  dv.labels=c("Cognitive Development + \nStatus Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cogstat_f2.html"
)

tab_model(
  fit_attitudes_f2_cog_sol,
  pred.labels=c("Intercept","Attitudes: Cognitive Development","Attitudes: Solidarity"),
  dv.labels=c("Cognitive Development + \nSolidarity Model"),
  string.pred="Coefficient",
  string.ci="Conf. Int (95%)",
  string.p = "P-value",
  file = "Cogsol_f2.html"
)


