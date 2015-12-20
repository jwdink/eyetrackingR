library("eyetrackingR")
context("Time Cluster Analysis")

round.data.frame <- function(df, digits = 0) {
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- round(x = df[[col]], digits = digits)
    }
  }
  df
}

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

# Cluster Analysis 1: Vocab LM
response_time_by_ppt <- make_time_sequence_data(response_window_clean, time_bin_size = 100, 
                                         aois = "Animate",
                                         predictor_columns = c("MCDI_Nouns","MCDI_Verbs", "Sex"),
                                         summarize_by = "ParticipantName")
tclust_data_lm <- make_time_cluster_data(data = response_time_by_ppt, predictor_column = "MCDI_Verbs", test = "lm", 
                       threshold = -2, formula = Prop ~  MCDI_Verbs + MCDI_Nouns)
#dput(x = round(tclust_data_lm, 3), file = "clust_dat_output_lm.txt")
tclust_data_lm_check <- dget("clust_dat_output_lm.txt")

test_that(desc = "The function make_time_cluster_data gives necessary eyetrackingR attributes with lm", code = {
  expect_true( all(round(tclust_data_lm,3)==tclust_data_lm_check, na.rm=TRUE) )
  expect_true( all(class(tclust_data_lm) %in% c("time_cluster_data", "time_sequence_data", "data.frame")) )
  expect_false( is.null( attr(tclust_data_lm,"eyetrackingR") ) )
  expect_equal( ncol(attr(tclust_data_lm, "eyetrackingR")$clusters), 5 )
  expect_equal( nrow(attr(tclust_data_lm, "eyetrackingR")$clusters), 1 )
})

tclust_analysis_lm <- analyze_time_clusters(tclust_data_lm, within_subj = FALSE, samples = 10, 
                                            formula = Prop ~  MCDI_Verbs + MCDI_Nouns)
test_that(desc = "The function analyze_time_clusters gives necessary eyetrackingR class with lm", code = {
  expect_true( all(class(tclust_analysis_lm) %in% c("cluster_analysis")) )
  expect_equal( length( attr(tclust_analysis_lm$time_bin_summary, "eyetrackingR")$negative_runs ), 1 )
})

# Cluster Analysis 2: LMER
response_time <- make_time_sequence_data(response_window_clean, time_bin_size = 100, 
                                                aois = "Animate",
                                                predictor_columns = c("Age","Target"))

tclust_data_lmer <- make_time_cluster_data(data = response_time,
                                           predictor_column = "Age", 
                                           test = "lmer", 
                                           threshold = 2, 
                                           formula = ArcSin ~ Target + Age + (1|ParticipantName) + (1|Trial) ) 
test_that(desc = "The function make_time_cluster_data gives necessary eyetrackingR attributes with lmer", code = {
  expect_true( all(class(tclust_data_lmer) %in% c("time_cluster_data", "time_sequence_data", "data.frame")) )
  expect_equal( nrow(tclust_data_lmer), 6710 )
  expect_false( is.null( attr(tclust_data_lmer,"eyetrackingR") ) )
  expect_equal( nrow(attr(tclust_data_lmer, "eyetrackingR")$clusters), 2 )
  expect_equal( ncol(attr(tclust_data_lmer, "eyetrackingR")$clusters), 5 )
})
tclust_anal_lmer <- analyze_time_clusters(tclust_data_lmer, within_subj = FALSE, samples = 10)

# Cluster Analysis 3: t.test, var.equal, between
tclust_data_ttest <- make_time_cluster_data(data = response_time_by_ppt, predictor_column = "Sex", test = "t.test", 
                                         var.equal=TRUE, threshold = -1.5)
test_that(desc = "The function make_time_cluster_data gives necessary eyetrackingR attributes with t.test", code = {
  expect_true( all(class(tclust_data_ttest) %in% c("time_cluster_data", "time_sequence_data", "data.frame")) )
  expect_equal( nrow(tclust_data_ttest), 1485 )
  expect_false( is.null( attr(tclust_data_ttest,"eyetrackingR") ) )
  expect_equal( nrow(attr(tclust_data_ttest, "eyetrackingR")$clusters), 3 )
  expect_equal( ncol(attr(tclust_data_ttest, "eyetrackingR")$clusters), 5 )
})

tclust_analysis_ttest <- analyze_time_clusters(tclust_data_ttest, within_subj = FALSE, samples = 10, var.equal=TRUE)
test_that(desc = "The function analyze_time_clusters gives necessary eyetrackingR class with lm", code = {
  expect_true( all(class(tclust_analysis_ttest) %in% c("cluster_analysis")) )
  expect_equal( length( attr(tclust_analysis_ttest$time_bin_summary, "eyetrackingR")$negative_runs ), 2 )
})

# Cluster Analysis 4: boot-splines
response_time_by_ppt <- dplyr::filter(response_time_by_ppt, !is.na(Prop))
tclust_data_boot <- make_time_cluster_data(response_time_by_ppt, predictor_column = "Sex", test="boot_splines",
                                           within_subj = FALSE, smoother = "smooth.spline", alpha=.05)
tclust_data_boot2 <- make_time_cluster_data(response_time_by_ppt, predictor_column = "Sex", test="boot_splines",
                                           within_subj = FALSE, bs_samples = 100, smoother = "loess", alpha=.05)



