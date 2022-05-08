

####Created country list and filer mechanism. 
#### TO DO: Determine the tables needed for the reactive charts and how to display the "all countries" statistics.

library(tidyverse)
library(haven)
library(psych)
library(texreg)
library(dplyr)

data <- read_dta("/Users/nicholasparker/Dropbox/JPSM/MWDS/EVS/ZA7500_v4-0-0.dta")

data_clean <- data %>%
  select(v72,v80,v52,v261,v243_r,c_abrv,v225, age) %>%
  rename("suffering_mother"=v72) %>%
  rename("jobs_scarce"= v80) %>%
  rename("religion"= v52) %>%
  rename("sex"= v225) %>%
  rename("education"= v243_r) %>%
  rename("country"= c_abrv) %>%
  rename("income" = v261)

country_list<- data_clean %>%
  distinct(country)

all<- data.frame("Aggregate")%>%
  rename("country"="X.Aggregate.")

country_list<- rbind(country_list,all)

data_continuous <- data_clean %>%
  select(income, age)

data_categorical <- data_clean %>%
  select(-income, -age)

table_continuous <- describe(data_continuous)

table_categorical <- summary(data_categorical)

library(knitr)

table_categorical <- summary(data_categorical)

knitr::kable(table_continuous)

knitr::kable(table_categorical)

## Insert a filtering mechanism

outcome<- c("mother working","jobs to nationals")

controls<- c("sex","education")

age_polynomial< c("1","2","3","4")

data_clean_age_suffering<- data_clean %>%
    filter(country=="AL")%>%
    select(age,suffering_mother)%>%
    group_by(age) %>%
    summarise(mean_age_suffering = mean(suffering_mother)) %>%
    filter(age>0) %>%
    pivot_longer(
      cols = "mean_age_suffering",
      values_to = "values")

#AGE
# 
# 
# data_clean_age<- data_clean %>%
#   select(age,suffering_mother,jobs_scarce, country)
# 
# data_clean_age_suffering<- data_clean_age %>%
#   group_by(age) %>%
#   summarise(mean_age_suffering = mean(suffering_mother)) %>%
#   filter(age>0)
# 
# data_clean_age_jobs<- data_clean_age %>%
#   group_by(age) %>%
#   summarise(mean_age_jobs= mean(jobs_scarce)) %>%
#   filter(age>0)
# 
# data_long_age_suffering<- data_clean_age_suffering %>%
#     pivot_longer(
#       cols = "mean_age_suffering",
#       values_to = "values"
#   )
# 
# data_long_age_jobs<- data_clean_age_jobs %>% 
#   pivot_longer(
#     cols = "mean_age_jobs",
#     values_to = "values"
#   )
# 
# 
# #SEX
# 
# 
data_clean_sex_suffering<- data_clean %>%
  filter(country=="AL") %>%
  group_by(sex) %>%
  summarise(mean_sex_suffering = mean(suffering_mother))%>%
  mutate(sex=as.character(sex))%>%
  mutate(sex=replace(sex, sex=="2","Female")) %>%
  mutate(sex=replace(sex, sex=="1","Male"))

data_clean_sex_jobs<- data_clean %>%
  filter(country=="AL") %>%
  group_by(sex) %>%
  summarise(mean_sex_jobs = mean(jobs_scarce)) %>%
  mutate(sex=as.character(sex))%>%
  mutate(sex=replace(sex, sex=="2","Female")) %>%
  mutate(sex=replace(sex, sex=="1","Male"))


# #EDUCATION
# 
# 
# data_clean_education_suffering<- data_clean %>%
#   group_by(education) %>%
#   summarise(mean_education_suffering = mean(suffering_mother)) 


education_mother<- data_clean %>%
    group_by(education) %>%
    summarise(mean_education_suffering = mean(suffering_mother)) %>%
    mutate(education=as.character(education))%>%
    mutate(education=replace(education, education=="2","Secondary")) %>%
    mutate(education=replace(education, education=="1","Primary")) %>%
    mutate(education=replace(education, education=="3","College"))

education_mother<- data_clean %>%
    filter(country=="AL") %>%
    filter(education>0 & education!=66) %>%
    group_by(education) %>%
    summarise(mean_education_suffering = mean(suffering_mother)) %>%
    mutate(education=as.character(education))%>%
    mutate(education=replace(education, education=="2","Secondary")) %>%
    mutate(education=replace(education, education=="1","Primary")) %>%
    mutate(education=replace(education, education=="3","College"))


# data_clean_education_suffering$education <- c("Primary","Secondary", "College and more")
# 
data_clean_education_jobs<- data_clean %>%
  filter(country=="AL") %>%
  filter(education>0 & education!=66) %>%
  group_by(education) %>%
  summarise(mean_education_jobs = mean(jobs_scarce)) %>%
  mutate(education=as.character(education))%>%
  mutate(education=replace(education, education=="2","Secondary")) %>%
  mutate(education=replace(education, education=="1","Primary")) %>%
  mutate(education=replace(education, education=="3","College"))


data_clean_education_jobs$education <- c("Primary","Secondary", "College and more")


mother<- data_clean %>%
    select(suffering_mother)%>%
    summarise(mean_suffering = mean(suffering_mother)) %>%
    pivot_longer(
      cols = "mean_suffering",
      values_to = "values")

ggplot(mother, aes(y=name, x=values))+
    theme_minimal()+
    geom_bar(stat = "identity") +
    xlab("mean response") +
    ggtitle("Mean response to 'children suffering' question")

jobs<- data_clean %>%
    select(jobs_scarce, country)

ggplot(jobs, aes(x="jobs_scarce"))+
  theme_minimal()+
  geom_bar(aes(x = jobs_scarce)) +
             xlab("mean response") +
             ggtitle("Mean response to 'jobs scarce' question")


# 
# # tables needed for plots 
# 
# #### mother outcome variable
# 
# #data_long_age_suffering
# 
# #data_clean_education_suffering
# 
# #data_clean_sex_suffering
# 
# 
# #### Jobs variable 
# 
# #
# 
# 
# 
# 
# ###Sample plots
# 
# library(ggplot2) # load ggplot
# 
# ggplot(data = data_long_age_suffering, aes(age,values))+
#   theme_minimal()+
#   geom_point() +
#   xlab("age") +
#   ylab("mean response value")+
#   ggtitle("Figure 1: Mean response to 'children suffering' question by age")+
#   geom_smooth(method=lm) 
# 
# ggplot(data = data_long_age_jobs, aes(age,values))+
#   theme_minimal()+
#   geom_point() +
#   xlab("age") +
#   ylab("mean response value")+
#   ggtitle("Mean response to jobs question by age")+
#   geom_smooth(method=lm) 
# 
# ggplot(data_clean_education_suffering, aes(y=education, x=mean_education_suffering))+
#   theme_minimal()+
#   geom_bar(stat = "identity") +
#   xlab("mean response") +
#   ylab("educational attainment")+
#   ggtitle("Mean response to 'children suffering' question by educational attainment")
# 
ggplot(data = data_clean_education_jobs, aes(y=education, x=mean_education_jobs))+
  theme_minimal()+
  geom_bar(stat="identity") +
  xlab("mean response") +
  ylab("educational attainment")+
  ggtitle("Figure 1: Mean response to jobs question")
# 
# ggplot(data_clean_sex_suffering, aes(y=sex, x=mean_sex_suffering))+
#   theme_minimal()+
#   geom_bar(stat = "identity") +
#   xlab("sex") +
#   ylab("mean response value")+
#   ggtitle("Mean response to 'children suffering' question by educational attainment")
# 
# ggplot(data = data_clean_sex_jobs, aes(y=sex, x=mean_sex_jobs))+
#   theme_minimal()+
#   geom_bar(stat="identity") +
#   xlab("mean response") +
#   ylab("sex")+
#   ggtitle("Figure 1: Mean response to jobs question")
# 
suffering.mother.lm<-lm(suffering_mother ~ education + age + age^2 + sex, data = data_clean)

jobs.scarce.lm<-lm(jobs_scarce ~ education + age + age^2 + sex, data = data_clean)

reg_table_mother <- htmlreg(suffering.mother.lm, 
                     dcolumn = TRUE, 
                     booktabs = TRUE,
                     use.packages = TRUE, 
                     label = "tab:1", 
                     caption = "Regression Estimation Results", float.pos = "hb",
                     custom.model.names = "Working Mother")

reg_table_jobs <- htmlreg(jobs.scarce.lm, 
                     dcolumn = TRUE, 
                     booktabs = TRUE,
                     use.packages = TRUE, 
                     label = "tab:1", 
                     caption = "Regression Estimation Results", float.pos = "hb",
                     custom.model.names = "Job Scarcity")

reg_table_jobs

reg_table_mother

sessionInfo()
