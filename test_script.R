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
  gather(BodyPart, DissBP, waist:bust, convert = TRUE) %>%
  mutate(
    Participant = as.character(Participant),
    BodyPart = paste0("IA_", BodyPart)
    ) %>%
  group_by(Participant) %>%
  mutate(DissOverall = sum(DissBP) 
         ) %>%
  ungroup() %>%
  mutate(DissNoBP = DissOverall - DissBP)

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
  mutate(IA_body = IA_bust | IA_waist | IA_hips | IA_thighs)

df_omeas = df_measures %>%  # overall measures
  group_by(Participant) %>% 
  summarise(bodysurvobcs      = unique(bodysurvobcs), 
            edibdiscontinuous = unique(edibdiscontinuous),
            edibodydistot     = unique(edibodydistot),
            DissOverall       = unique(DissOverall)
            )

df_fb = left_join(df_fb, df_omeas, by="Participant") %>%
  filter( ! is.na(DissOverall),
          ! is.na(edibdiscontinuous) )

## SUBSETTING BY TIME-WINDOW AND BY FACTOR
...

## VERIFY DATA LOOKS SANE:
described = describe_data(df_fb, data_options, dv = "IA_head", factors = c("Participant") )
View(described)


# WINDOW ANAL:
window_anal = window_analysis(df_fb,
                data_options, 
                dv = c('IA_head','IA_bust','IA_waist','IA_hips','IA_thighs'), 
                condition_columns = "edibdiscontinuous")

window_anal$GroupFactor = ifelse(window_anal$edibdiscontinuous > median(window_anal$edibdiscontinuous), 'High', 'Low')

naive = window_anal %>%
  group_by(Participant, GroupFactor, AOI) %>%
  summarise(Prop = mean(Prop, na.rm=TRUE))
ggplot(naive, aes(x = GroupFactor, y=Prop, group=AOI, color=AOI)) +
  stat_summary(fun.dat = mean_se) +
  stat_summary(fun.y = mean, geom="line")

mine = summary_se_within(window_anal, measurevar = "Prop", betweenvars = c("GroupFactor","AOI"), idvar = "Participant")
ggplot(mine, aes(x = GroupFactor, y=Prop, group=AOI, color=AOI)) +
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymin = Prop - SE, ymax= Prop +SE, width=0) )

winst = summarySEwithin(naive, measurevar = "Prop", betweenvars = c("GroupFactor","AOI"), idvar = "Participant")
ggplot(winst, aes(x = GroupFactor, y=Prop, group=AOI, color=AOI)) +
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymin = Prop - se, ymax= Prop +se, width=0) )



# TIME ANAL
time_anal = time_analysis(df_fb, 
                    data_options, 
                    time_bin_size = 250, 
                    dv = c('IA_head','IA_bust','IA_waist','IA_hips','IA_thighs'), 
                    factors = "edibdiscontinuous",  # <---- rename?
                    summarize_by = 'crossed')
class(time_anal)
plot(time_anal, data_options, "edibdiscontinuous") 

# SEQUENTIAL BINS:
seq_anal = analyze_time_bins(time_anal, data_options, condition_column = 'edibdiscontinuous') # <--- elog vs. arcsin?
class(seq_anal)
plot(seq_anal)



ggplot(window_anal, aes(x = edibdiscontinuous, y = Prop, group= AOI, color= AOI)) +
  geom_smooth(method = "lm")

ggplot(window_anal, aes(x = GroupFactor, y = Prop, group= AOI, color= AOI)) +
  stat_summary(fun.dat = mean_cl_boot) +
  stat_summary(fun.y = mean, geom = 'line') +
  coord_cartesian(ylim = c(0,1)) 




