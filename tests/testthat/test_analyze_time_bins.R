library("eyetrackingR")
context("Analyze Time Bins")

TOL <- 0.0001
compare_dfs <- function(df1, df2, tol=TOL) {
  stopifnot(all(dim(df1)==dim(df2)))
  
  res <- matrix(nrow = nrow(df1), ncol = ncol(df1))
  for (i in 1:ncol(df1)) {
    col <- colnames(df1)[i]
    
    # Replace NAs with 0
    df1[is.na(df1)] <- 0
    df2[is.na(df2)] <- 0
    
    # check difference between elements
    if (is.numeric(df1[[col]])) {
      res[,i] <- df1[[col]] - df2[[col]]
      if (any(res[,i]>tol)) stop("Failed on column: ", col)
    } else {
      res[,i] <- df1[[col]] == df2[[col]]
      if (any(!res[,i])) stop("Failed on column: ", col)
    }
  }
  return(TRUE)
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

# Within Subjects -----------------------------------------------------------------------------
df_time_within <- make_time_sequence_data(response_window_clean, 
                                          time_bin_size = 100, 
                                          aois = c("Animate"), 
                                          predictor_columns = c("Target","Sex"),
                                          summarize_by = "ParticipantName")
num_time_bins <- length(unique(df_time_within$TimeBin))
tb_within <- analyze_time_bins(df_time_within, predictor_column = "Target", test = "t.test", 
                        paired=TRUE, alpha = .05 / num_time_bins)
#dput(tb_within, file = "tb_output_within_subj.txt")
tb_within_check <- dget(file = "tb_output_within_subj.txt")

test_that(desc = "The function analyze_time_bins has eyetrackingR attributes (within)", code = {
  expect_true( all(class(tb_within) %in% c("bin_analysis", "data.frame")) )
  expect_false( is.null( attr(tb_within,"eyetrackingR") ) )
  expect_equal(length(which(is.na(tb_within_check$PositiveRuns))), 38)
  expect_equal(length(which(is.na(tb_within_check$NegativeRuns))), 55)
})

test_that(desc = "The function analyze_time_bins has correct data (within)", code = {
  res <- compare_dfs(tb_within_check,tb_within)
  expect_true( res )
})

# Between Subjects -----------------------------------------------------------------------------
df_time_between <- make_time_sequence_data(response_window_clean, 
                                          time_bin_size = 100, 
                                          aois = c("Animate"), 
                                          predictor_columns = "Sex",
                                          summarize_by = "ParticipantName")
num_time_bins <- length(unique(df_time_between$TimeBin))
tb_between <- analyze_time_bins(df_time_between, predictor_column = "Sex", test = "t.test", 
                               paired=FALSE, alpha = .05 )
#dput(tb_between, file = "tb_output_between_subj.txt")
tb_between_check <- dget(file = "tb_output_between_subj.txt")

test_that(desc = "The function analyze_time_bins has eyetrackingR attributes (between)", code = {
  expect_true( all(class(tb_between) %in% c("bin_analysis", "data.frame")) )
  expect_false( is.null( attr(tb_between,"eyetrackingR") ) )
  expect_equal(length(which(is.na(tb_between_check$PositiveRuns))), 55)
  expect_equal(length(which(is.na(tb_between_check$NegativeRuns))), 54)
})

test_that(desc = "The function analyze_time_bins has correct data (between)", code = {
  res <- compare_dfs(tb_between_check,tb_between)
  expect_true( res )
})

# Interaction Term -----------------------------------------------------------------------------
tb_interaction1 <- analyze_time_bins(df_time_within, predictor_column = "SexM:TargetInanimate", test = "lmer", alpha=.05,
                                     formula = Prop ~ Sex*Target + (1|ParticipantName))
#dput(tb_interaction1, file = "tb_output_interaction.txt")
tb_interaction1_check <- dget(file = "tb_output_interaction.txt")

test_that(desc = "The function analyze_time_bins has correct data (interaction)", code = {
  res <- compare_dfs(tb_interaction1_check,tb_interaction1)
  expect_true(res)
})

# Intercept Model -----------------------------------------------------------------------------
df_time_intercept <- make_time_sequence_data(response_window_clean, 
                                           time_bin_size = 100, 
                                           aois = c("Animate"), 
                                           predictor_columns = c("Sex","Target"),
                                           summarize_by = "Trial")
tb_intercept <- analyze_time_bins(df_time_intercept, predictor_column = "(Intercept)", test = "lmer", alpha=.05,
                                  aoi = "Animate",
                                  formula = Prop ~ Sex*Target + (1|Trial))

# AOI as Predictor -----------------------------------------------------------------------------
df_time_maoi <- make_time_sequence_data(response_window_clean, 
                        time_bin_size = 100, 
                        aois = c("Animate","Inanimate"), 
                        predictor_columns = c("Sex","Target"),
                        summarize_by = "Trial")
tb_maoi <- analyze_time_bins(df_time_maoi, predictor_column = "(Intercept)", test = "lmer", alpha=.05, p_adjust_method = "holm",
                       formula = Prop ~ AOI + (1|Trial))
summary(tb_maoi)

# Boot-splines -----------------------------------------------------------------------------
tb_boot <- analyze_time_bins(df_time_within, predictor_column = "Target", test = "boot", alpha=.10, within_subj=TRUE)
summary(tb_boot)
