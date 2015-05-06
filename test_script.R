setwd("/Volumes/Groups/server/Documents/People/Jacob Dink/BodyImageProject/Analysis") # i'm sorry hadley
packs = lapply(X = c('readr', 'plyr', 'dplyr', 'pbapply', 'tidyr', 'ggplot2', 'broom', 'lme4'), FUN = require, character.only=TRUE)
source("~/Documents/Projects/ET/eyetrackingR/analyze-eyetracking.R")

## LOAD DATA:
df3 = readRDS("~/Desktop/cleaned_et_data_w_all_interest_areas.RDS")

# get bis info:
df_measures = read.csv("./data/measures.csv", stringsAsFactors = FALSE) %>%
  select(ID, edibodydistot, bmi, bodysurvobcs, edibdiscontinuous, stomach, thighs, hips, chest) %>% # <---- NOTE no arms
  rename(Participant = ID,
         waist  = stomach,
         bust   = chest) %>%
  mutate(DissOverall = waist+thighs+hips+bust)

## SET DATA OPTIONS:
data_options = set_data_options(sample_rate = 1000, 
                 default_columns = NULL,
                 default_dv = NULL,
                 item_columns = "Image",
                 trial_column = "Trial",
                 sample_column = "SampleIndex",
                 time_column = "TimeInTrial",
                 trackloss_column = "Trackloss",
                 participant_column = "Participant", 
                 active_aoi_column = "InterestArea",
                 message_column = "Message")

## GET INTO CORRECT FORMAT:
df3 = fix_data_types(data = df3, data_options)

## DESCRIBE:
described = describe_data(df3, data_options, dv = "IA_head", factors = c("Participant") )
View(described)

## CLEAN BY TRACKLOSS:
tl = trackloss_analysis(df3, data_options)
(exclude = unique(tl$Participant[tl$Part_ZScore>3]))
exclude_trials = paste(tl$Participant[tl$Trial_ZScore>3],tl$Trial[tl$Trial_MAD>3], sep="_")
# as yet unwritten 'clean_by_trackloss' function
df_fb = df3 %>%
  filter(! Participant %in% exclude,
         ! TrialID %in% exclude_trials,
         ! Trackloss,
           TimeInTrial > 0,
         ! PostTrial) %>%
  mutate(IA_body = IA_bust | IA_waist | IA_hips | IA_thighs,
         Participant = as.numeric(as.character(Participant)))

df_fb = left_join(df_fb, 
                  select(df_measures, -(waist:bust)), # individual measures will be added to each analysis, later
                  by="Participant") %>%
  filter( ! is.na(DissOverall),
          ! is.na(edibdiscontinuous) )

## SUBSETTING BY TIME-WINDOW AND BY FACTOR
...

## VERIFY DATA LOOKS SANE:
described = describe_data(df_fb, data_options, dv = "IA_head", factors = c("Participant") )
View(described)


## WINDOW ANAL:
df_fb$Phase = ifelse(as.numeric(as.character(df_fb$Trial)) > median(as.numeric(as.character(df_fb$Trial)), na.rm=TRUE), 'Late', 'Early' )
window_anal = window_analysis(df_fb,
                data_options, 
                dv = c('IA_head','IA_bust','IA_waist','IA_hips','IA_thighs'), 
                condition_columns = c("edibdiscontinuous", "Phase") )
window_anal$BodSurv = ifelse(window_anal$edibdiscontinuous > median(window_anal$edibdiscontinuous), 'High', 'Low')

plot(window_anal, data_options, x_axis_column= "Phase", group_column= "BodSurv") # two discrete vars
plot(window_anal, data_options, x_axis_column= "Phase", group_column= "edibdiscontinuous") # a continous grouping var gets median split
plot(window_anal, data_options, x_axis_column= "edibdiscontinuous", group_column= "Phase") # a continous x-axis var gets LM


## TIME ANAL
time_anal = time_analysis(df_fb, 
                    data_options, 
                    time_bin_size = 250, 
                    dv = c('IA_head','IA_bust','IA_waist','IA_hips','IA_thighs'), 
                    condition_columns = "edibdiscontinuous",  # <---- rename?
                    summarize_by = 'crossed')
plot(time_anal, data_options, condition_column= "edibdiscontinuous") 

## SEQUENTIAL BINS:
seq_anal = analyze_time_bins(time_anal, data_options, condition_column = 'edibdiscontinuous') # <--- elog vs. arcsin?
plot(seq_anal)




