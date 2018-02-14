library("eyetrackingR")
context("Time Sequence Data")

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

response_time <- make_time_sequence_data(response_window_clean, time_bin_size = 100, 
                                         predictor_columns = c("Target") )

test_that(desc = "The function make_time_sequence_data has necessary eyetrackingR attributes", code = {
  expect_true( all(c("time_sequence_data", "data.frame") %in% class(response_time)) )
  expect_equal( nrow(response_time), 13420 )
  expect_false( is.null( attr(response_time,"eyetrackingR") ) )
})
test_that(desc = "The function analyze_time_bins returns both AOIs", code = {
  expect_equal( length(unique(response_time$AOI))  , 2 )
})

