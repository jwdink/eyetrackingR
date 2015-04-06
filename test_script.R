source("./analyze-eyetracking.R")

df_ia = readRDS(file = "~/Desktop/BI/df_ia.rds")

str(df_ia)
data_options= set_data_options(sample_factor = "TimeInTrial", 
                 trial_factor = "TRIAL_INDEX", 
                 time_factor = "TimeInTrial", 
                 sample_rate = 1000, 
                 participant_factor = "RECORDING_SESSION_LABEL", 
                 active_aoi_factor = "InterestArea", 
                 trackloss_factor = 'RIGHT_IN_BLINK')
df_ia = verify_dataset(data = df_ia, data_options)
str(df_ia)

unique( df_ia$InterestArea )
df_ia$AOI_Head = df_ia$InterestArea == "head"

describe_data(df_ia, dv = 'AOI_Head', factors = 'Condition')


plot_data(data = df_ia, data_options, dv = "AOI_Head", factor = "Condition") + 
  coord_cartesian(ylim = c(0,1))

plot_data(data = df_ia, data_options, dv = "AOI_Head", factor = "Condition", type = "smoothed", time_bin_size = 25) + 
  coord_cartesian(ylim = c(0,1) )

# class(df_ia)
# foo = as.data.frame(df_ia, stringsAsFactors = FALSE)
# plot_data(data = foo, data_options = data_options, dv = "AOI_Head", factor = "Condition")
