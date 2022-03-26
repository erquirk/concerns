library(ggplot2)
library(dplyr)
library(here)
library(tidyverse)
library(ggpubr)
require(gridExtra)
library(stringr)
library(coin)
library(ggpattern)

langgroups<-read_csv(here("concerns/data/paper3_langgroups.csv"))
langatt_df_forfigures<-read_csv(here("concerns/data/langatt_df_forfigures.csv"))

#Figure 1: Language groups histogram

ggplot(data.frame(langgroups), aes(x=lang_group)) +
  geom_bar(fill = "#154360")+
  scale_x_discrete(labels= c("English\n& French", "English,\nFrench\n& Other(s)","English\n& Other(s)", "French\n&Other(s)","Others"))+
  xlab("Language combinations")+
  ylab("Count")+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "lightgrey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )


ggsave(here("figures/paper_3/language_combinations.png"), dpi=600)



library(jtools)
library(ggstance)
library(broom.mixed)
langatt_df_forfigures<-langatt_df_forfigures%>%
  mutate(HL=as.factor(has_OL))%>%
  mutate(Triling=as.factor(trilingual))%>%
  mutate(Dev_Issue=as.factor(has_DL))%>%
  mutate(Parent_Multiling=as.factor(multilingual_parent))%>%
  mutate(Attitudes_Cog=cognitive_mean)%>%
  mutate(Attitudes_Status=status_mean)%>%
  mutate(Attitudes_Solid=solidarity_mean)


fit_1_1<-lm(factor1_mean~HL+Triling+Dev_Issue+Parent_Multiling+Attitudes_Cog+
                Attitudes_Status+Attitudes_Solid, data=langatt_df_forfigures)
fit_2_1<-lm(factor2_mean~HL+Triling+Dev_Issue+Parent_Multiling+Attitudes_Cog+
                Attitudes_Status+Attitudes_Solid, data=langatt_df_forfigures)
fit_1_2<-lm(factor1_mean~HL+Triling+Dev_Issue
              +Attitudes_Cog+Attitudes_Status, data=langatt_df_forfigures)
fit_2_2<-lm(factor2_mean~HL+Attitudes_Cog, data=langatt_df_forfigures)

fit_tri<-lm(concern_mean~Triling, data = langatt_df_forfigures)
fit_tri_hl<-lm(concern_mean~HL+Triling, data = langatt_df_forfigures)
fit_tri<-lm(factor1_mean~Triling, data = langatt_df_forfigures)
fit_tri_hl<-lm(factor1_mean~HL+Triling, data = langatt_df_forfigures)
fit_tri<-lm(factor2_mean~Triling, data = langatt_df_forfigures)
fit_tri_hl<-lm(factor2_mean~HL+Triling, data = langatt_df_forfigures)

anova(fit_tri, fit_tri_hl)
summary(fit_tri_hl)
plot_coefs(fit_tri_hl)

plot_coefs(fit_1_1)
ggsave(here("figures/paper_3/fullmodel_factor1.png"), dpi=600)
plot_coefs(fit_2_1)
ggsave(here("figures/paper_3/fullmodel_factor2.png"), dpi=600)
plot_coefs(fit_1_2)
ggsave(here("figures/paper_3/prunedmodel_factor1.png"), dpi=600)
plot_coefs(fit_2_2)
ggsave(here("figures/paper_3/prunedmodel_factor2.png"), dpi=600)


#Figure 2


figure2_df<-langatt_df_forfigures%>%
  pivot_longer(c("factor2_mean","factor1_mean"), names_to="concern_type", values_to="strength")%>%
  mutate(concern_type=ifelse(concern_type=="factor1_mean","1","2"))

figure2_df$concern_type <- factor(figure2_df$concern_type, levels = c("1", "2"))


p<-ggboxplot(figure2_df, x="concern_type", y="strength", color="concern_type", ylab="Concern Strength", 
             xlab="Factor", legend = "none", na.rm=TRUE)

p+stat_summary(fun.y=mean, geom="point", shape=2, size=4)

ggsave(here("figures/paper_3/Concerns_strength.png"), dpi=600)

figure2_df%>%
  ggplot(aes(concern_type,strength,fill=concern_type,color=concern_type))+
  geom_violin(alpha=0.5)+
  geom_jitter(alpha=0.1)+
  theme(legend.position='none')+
  labs(x="Factor", y="Concern Level")+
  scale_fill_manual(values=c("#4d94ff","#ffbf80"))+
  scale_color_manual(values=c("#4d94ff","#ffbf80"))+
  scale_y_continuous(limits=c(1,5.25))

ggsave(here("figures/paper_3/Concerns_strength_violin.png"), dpi=600)

langatt_df_forfigures%>% 
  mutate(HL=ifelse(HL==1,"HL","Non-HL"),
         HL=as.factor(HL))%>%
  pivot_longer(c("factor2_mean","factor1_mean"), names_to="concern_type", values_to="strength")%>%
  mutate(concern_type=ifelse(concern_type=="factor1_mean","1","2"))%>%
  mutate(conern_type=as.factor(concern_type))%>%
  group_by(concern_type,HL)%>%
  summarise(mean_concern=mean(strength,na.rm=TRUE))%>%
  ungroup() %>%
  ggplot(aes(x = HL, y = mean_concern)) +
  geom_bar(aes(fill = HL), stat="identity", position="dodge") +
  #geom_signif(data = data.frame(measure = c("Awareness","Desire")),
   #           aes(y_position=c(2.75, 2.75), xmin=c(0.8, 0.8), xmax=c(2.2, 2.2),
    #              annotations=c("*", "**")), tip_length=0, manual = T) +
  scale_fill_manual(values = c("#154360","#A9CCE3")) +
  facet_grid(~ factor(concern_type, levels = c("1", "2")), scales = "free", switch = "both") +
  theme_minimal()+
  theme(legend.position='none',
        panel.background = element_rect(fill="white", colour="white"))+
  labs(x="HL", y="Concern Level")

ggsave(here("figures/paper_3/Concerns_by_HL.png"), dpi=600)

langatt_df_forfigures%>% 
  mutate(HL=ifelse(HL==1,"HL","Non-HL"),
         HL=as.factor(HL))%>%
  pivot_longer(c("factor2_mean","factor1_mean"), names_to="concern_type", values_to="strength")%>%
  mutate(concern_type=ifelse(concern_type=="factor1_mean","1","2"))%>%
  mutate(conern_type=as.factor(concern_type))%>%
  group_by(concern_type,HL)%>%
  summarise(mean_concern=mean(strength,na.rm=TRUE))%>%
  ungroup() %>%
  ggplot(aes(x = HL, y = mean_concern)) +
  geom_bar(aes(fill = HL), stat="identity", position="dodge") +
  #geom_signif(data = data.frame(measure = c("Awareness","Desire")),
  #           aes(y_position=c(2.75, 2.75), xmin=c(0.8, 0.8), xmax=c(2.2, 2.2),
  #              annotations=c("*", "**")), tip_length=0, manual = T) +
  scale_fill_manual(values = c("#154360","#A9CCE3")) +
  facet_grid(~ factor(concern_type, levels = c("1", "2")), scales = "free", switch = "both") +
  theme_minimal()+
  theme(legend.position='none',
        panel.background = element_rect(fill="white", colour="white"))+
  labs(x="HL", y="Concern Level")

t.test(factor1_mean, HL, data=langatt_df_forfigures)

