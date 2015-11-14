library("eyetrackingR")
context("Custom DV")

data("word_recognition") 
data <- make_eyetrackingr_data(word_recognition, 
                               participant_column = "ParticipantName",
                               trial_column = "Trial",
                               time_column = "TimeFromTrialOnset",
                               trackloss_column = "TrackLoss",
                               aoi_columns = c('Animate','Inanimate'),
                               treat_non_aoi_looks_as_missing = TRUE
)
# subset to response window post word-onset
response_window <- subset_by_window(data, 
                                    window_start_time = 15500, 
                                    window_end_time = 21000, 
                                    rezero = FALSE)
# remove trials with > 25% of trackloss
response_window_clean <- clean_by_trackloss(data = response_window, trial_prop_thresh = .25)
# create Target condition column
response_window_clean$Target <- as.factor( ifelse(test = grepl('(Spoon|Bottle)', response_window_clean$Trial), 
                                                  yes = 'Inanimate', 
                                                  no  = 'Animate') )

set.seed(5)
response_window_clean$PupilDilation <- suppressWarnings( rnorm(n = nrow(response_window_clean), 
                                                               mean = response_window_clean$Inanimate 
                                                               & (response_window_clean$Target=="Inanimate"),
                                                               sd = 5))
response_window_clean$PupilDilation2 <- log(response_window_clean$PupilDilation-min(response_window_clean$PupilDilation, na.rm=TRUE)+1)


df_window <- make_time_window_data(response_window_clean, other_dv_columns = c("PupilDilation", "PupilDilation2"),
                                   aois = c("Animate"), 
                                   predictor_columns = "Target",
                                   summarize_by = "Trial")
test_that(desc = "Arbitrary DV in window data",code = {
  expect_true("PupilDilation" %in% colnames(df_window))
})

df_time <- make_time_sequence_data(response_window_clean, other_dv_columns = c("PupilDilation", "PupilDilation2"),
                                   time_bin_size = 100, 
                                   aois = c("Animate"), 
                                   predictor_columns = "Target",
                                   summarize_by = "ParticipantName")
test_that(desc = "Arbitrary DV in sequence data",code = {
  expect_true("PupilDilation" %in% colnames(df_time))
})

num_time_bins <- length(unique(df_time$TimeBin))
T <- TRUE
tb1 <- analyze_time_bins(df_time, 
                        formula = PupilDilation ~ Target,
                        predictor_column = "Target", 
                        test = "t.test", 
                        paired=T,
                        alpha = .05 / num_time_bins)

tb2 <- analyze_time_bins(df_time, 
                         formula = PupilDilation2 ~ Target,
                         predictor_column = "Target", 
                         test = "t.test", 
                         paired=T,
                         alpha = .05 / num_time_bins)
test_that(desc = "The function analyze_time_bins has necessary eyetrackingR attributes", code = {
  expect_true( all(class(tb1) %in% c("bin_analysis", "data.frame")) )
  expect_equal( nrow(tb1), 55 )
  expect_false( is.null( attr(tb1,"eyetrackingR") ) )
})
test_that(desc = "The function analyze_time_bins returns 3 negative runs for arbitrary 'PupilDilation' DV (with seed = 5)", code = {
  expect_equal( length(unique(tb1$NegativeRuns)), 3)
})


