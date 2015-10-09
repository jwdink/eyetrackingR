library("eyetrackingR")
context("Analyze Time Bins")

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

df_time <- make_time_sequence_data(response_window_clean, 
                                   time_bin_size = 100, 
                                   aois = c("Animate"), 
                                   predictor_columns = "Target",
                                   summarize_by = "ParticipantName")
num_time_bins <- length(unique(df_time$TimeBin))
tb <- analyze_time_bins(df_time, predictor_column = "Target", test = "t.test", paired=T, alpha = .05 / num_time_bins)



## Tests:
test_that(desc = "The function analyze_time_bins has necessary eyetrackingR attributes", code = {
  expect_true( all(class(tb) %in% c("bin_analysis", "data.frame")) )
  expect_true( all(dim(tb)==c(55,9)) )
  expect_false( is.null( attr(tb,"eyetrackingR") ) )
})
test_that(desc = "The function analyze_time_bins returns no NAs for Statistic on word_recognition dataset when running paired t.tests", code = {
  expect_false( any(is.na(tb$Statistic)) )
})








