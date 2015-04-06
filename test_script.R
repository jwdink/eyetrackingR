source("./analyze-eyetracking.R")

df_ia = readRDS(file = "~/Desktop/BI/df_ia.rds")
df_ia = as.data.frame(df_ia) # remove old eyetrackR classes from object
unique( df_ia$InterestArea )
df_ia$AOI_Head = df_ia$InterestArea == "head"

data_options= set_data_options(sample_factor = "TimeInTrial", 
                               default_factors = "Condition",
                               default_dv = "AOI_Head",
                               trial_factor = "TRIAL_INDEX", 
                               time_factor = "TimeInTrial", 
                               sample_rate = 1000, 
                               participant_factor = "RECORDING_SESSION_LABEL", 
                               active_aoi_factor = "InterestArea", 
                               trackloss_factor = 'RIGHT_IN_BLINK')

## verify:
class(df_ia)
df_ia = verify_dataset(data = df_ia, data_options)
class(df_ia)

## describe:
describe_data(df_ia, data_options)

## plot:
plot(df_ia, data_options, dv = "AOI_Head", factor = "Condition") + 
  coord_cartesian(ylim = c(0,1))

plot(df_ia, data_options, dv = "AOI_Head", factor = "Condition", type = "smoothed", time_bin_size = 25) + 
  coord_cartesian(ylim = c(0,1) )

## analyses:
# window:
fit = window_analysis(data = df_ia, data_options)
drop1(fit, test= "Chisq")



