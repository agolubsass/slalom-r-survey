# Slalom Boston - Intro to R 

# Install packages

lib_needs<-c("tidyverse","cowsay") # create list of packages needed by using c() to combine and separate with , 
install.packages(lib_needs) # Install packages that are needed

# Load necessary packages
library(tidyverse) #<- For data manipulation and visualization
library(cowsay) #<- For fun


# Reading in data
survey_df<-read.csv("./R Tutorial/R_survey.csv")
# vs.
survey_df<-read_csv("./R Tutorial/R_survey.csv")

cowsay_cw_df<-read_csv("./R Tutorial/Cowsay_CW.csv")

colnames(survey_df)<-c("start","end","name","gender","experience",
           "practice","date_hire",
           "title","commute","animal",
           "R_level","Reason")

survey_df$start<-as.POSIXct(survey_df$start,format="%m/%d/%y %H:%M:%S")
survey_df$end<-as.POSIXct(survey_df$end,format="%m/%d/%y %H:%M:%S")

survey_df$date_hire<-as.Date(survey_df$date_hire,format="%m/%d/%Y")



survey_df_ext<-survey_df %>% 
  separate(commute,c("commute1", "commute2"),sep=";",extra = "drop",fill="right") %>% 
  select(name,commute1,commute2) %>% 
  gather(commute1:commute2,commute,-name) %>% 
  select(name,commute) %>% 
  filter(commute!="") %>% 
  left_join((survey_df %>% select(-commute)))


survey_df %>% 
  ggplot(aes(practice,fill=practice)) +
  geom_bar()


survey_df %>% 
  mutate(slalom_pct=experience_slalom/experience) %>% 
  group_by(practice) %>% 
  summarise()



survey_df %>% 
  mutate(time_to_complete=end-start,
         experience_slalom=(Sys.Date()-date_hire)/365) %>%
  group_by(practice) %>% 
  summarise(time_to_complete=mean(time_to_complete),
            experience=mean(experience),
            experience_slalom=mean(experience_slalom),
            count=n()) ->survey_summary

survey_df<-left_join(survey_df,cowsay_cw_df)

for(i in 1:nrow(survey_df)){
  print(paste0(word(survey_df$name[i],1)," ",
               substr(word(survey_df$name[i],2),1,1),
               ". says:"))
  say(survey_df$Reason[i],survey_df$cowsay_animal[i])
}



