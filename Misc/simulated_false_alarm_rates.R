library(eyetrackingR)
library(ggplot2)
library(pbapply)
set.seed(42)



# Prelim --------------------------------------------------------------------------------------
##
tb_size = 10

df <- simulate_eyetrackingr_data(num_items_per_condition = 2, .75)
df_time <- make_time_sequence_data(df, time_bin_size = tb_size, predictor_columns = "Condition", aois = "AOI1")
plot(df_time, predictor_column = "Condition")


##
ggplot(df_time, aes(x = Time, y = Prop, group = Condition, color=Condition)) +
  stat_summary(fun.y = mean, geom="line") +
  facet_wrap(~ Participant)
ggplot(df_time, aes(x = Time, y = Prop, group = Condition, color=Condition)) +
  stat_summary(fun.y = mean, geom="line") +
  facet_wrap(~ Trial)

num_time_bins <- length(unique(df_time$TimeBin))

# Consecutive t-tests --------------------------------------------------------------------------------
set.seed(5)
tb_res_fa <- pbreplicate(40, expr = {
  df <- simulate_eyetrackingr_data()
  df_time_sub <- make_time_sequence_data(df, time_bin_size = tb_size, predictor_columns = "Condition", aois = "AOI1", 
                                         summarize_by = "Participant")
  tb_anal <- analyze_time_bins(df_time_sub, predictor_column = "Condition", test = "t.test", alpha = .05, quiet=TRUE)
  sum(!is.na(tb_anal$PositiveRuns))
})
cat("Average FA Rate: ", mean(tb_res_fa) / num_time_bins)
cat("Average Family-wise FA: ", mean(tb_res_fa!=0) )

# Boot-Splines --------------------------------------------------------------------------------
## Boot-splines, subjects, no alpha correction (FA rate)
bs_res_fa <- pbreplicate(100, expr = {
  df <- simulate_eyetrackingr_data(num_trials_per_condition=6)
  df_time_sub <- make_time_sequence_data(df, time_bin_size = tb_size, predictor_columns = "Condition", aois = "AOI1", 
                                         summarize_by = "Participant")
  bs_dat <- make_boot_splines_data(df_time_sub, predictor_column = "Condition", within_subj = FALSE, samples=1000)
  bs_anal <- analyze_boot_splines(bs_dat)
  sum(bs_anal$Significant)
})
cat("Average FA Rate: ", mean(bs_res_fa) / num_time_bins)
cat("Average Family-wise FA: ", mean(bs_res_fa!=0) )

## Boot-splines, subjects, no alpha correction (sensitivity)
# [ to do ]

# Boot-splines, subjects, bonferonni (FA rate):
bs_res_fa_bonf <- pbreplicate(20, expr = {
  df <- simulate_eyetrackingr_data()
  df_time_sub <- make_time_sequence_data(df, time_bin_size = tb_size, predictor_columns = "Condition", aois = "AOI1", 
                                         summarize_by = "Participant")
  bs_dat <- make_boot_splines_data(df_time_sub, predictor_column = "Condition", samples=200, within_subj=FALSE,
                                   alpha = .05 / num_time_bins)
  bs_anal <- analyze_boot_splines(bs_dat)
  sum(bs_anal$Significant)
})
cat("Average FA Rate: ", mean(bs_res_fa_bonf) / num_time_bins)
cat("Average Family-wise FA: ", mean(bs_res_fa_bonf!=0) )

## Boot-splines, subjects, bonferonni (sensitivity)
# [ to do ]

## Boot-splines, noisy-time-bin (FA)

## Boot-splines, abrupt shift-then-back (sensitivity)



# Cluster Analysis ----------------------------------------------------------------------------
## Cluster, subjects t-test, no alpha correction (FA rate)
set.seed(35)
cl_res_fa <- pbreplicate(100, expr = {
  df <- simulate_eyetrackingr_data()
  df_time_sub <- make_time_sequence_data(df, time_bin_size = tb_size, predictor_columns = "Condition", aois = "AOI1", 
                                         summarize_by = "Participant") 
  cl_dat <- make_time_cluster_data(df_time_sub, predictor_column = "Condition", test = "t.test", threshold = -2.07, quiet=TRUE)
  if (nrow(get_time_clusters(cl_dat)) > 0) {
    cl_anal <- analyze_time_clusters(cl_dat, within_subj = FALSE, parallel = TRUE, samples = 100)
    cl_clusts <- get_time_clusters(cl_anal)
    cl_clusts$NumBins <- with(cl_clusts, (EndTime - StartTime) / tb_size)
    return( with(cl_clusts, sum(NumBins[Probability<.05])) )
  } else {
    return( 0 )
  }
})
cat("Average FA Rate: ", mean(cl_res_fa) / num_time_bins)
cat("Average Family-wise FA: ", mean(cl_res_fa!=0) )

## Cluster, subjects t-test, no alpha cor. (sensitivity)
# [ to do ]

## Cluster, noisy-time-bin (FA)
