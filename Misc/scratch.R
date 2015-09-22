generate_a_trial = function(participant, experiment) {

  # Time Vector:
  time_vec = seq(from = experiment$trial_start, to = experiment$trial_end)
  len = length(time_vec)

  # Initial Looking is a RV determined by the participant's bias
  init_look = ifelse(rbinom(1, 1, participant$init_look_bias)==1, "Target", "Distractor")

  # True Look is a latent variable where the participant would be looking if there was no noise
  # determined by the time of shift for this experiment, plus an offset unique to each participant
  true_look = ifelse(time_vec > experiment$true_lookshift+participant$look_shift_delay, "Target", init_look)
  false_look = ifelse(true_look=="Target", "Distractor", "Target")

  # participants have a chance of wanting to fidget their eye-position, which increases as time-since-last-switch increases
  last_fidget = experiment$trial_start
  fidgets = c()
  for (i in seq_along(time_vec)) {
    p_fidget = (time_vec[i] - last_fidget)/(4*sd(time_vec))
    p_fidget = ifelse(p_fidget>1, 1, p_fidget)
    fidget_occurs = rbinom(n = 1, size= 1, prob = p_fidget)
    if (fidget_occurs) {
      last_fidget = time_vec[i]
      fidgets = c(fidgets, i)
    }
  }

  # For every time a fidget-desire comes along, we do a coin flip on which AOI they will be on.
  # participants' focus-level determines how likely they will look to the "true look" position
  actual_look = true_look
  fidgets = c(fidgets, length(time_vec))
  for (i in seq_along(fidgets)[-1]) {
    actual_look[fidgets[i-1]:fidgets[i]] = ifelse(test = rbinom(n = 1, prob = participant$focus_level, size = 1),
                                                  yes  = true_look[fidgets[i-1]:fidgets[i]],
                                                  no   = false_look[fidgets[i-1]:fidgets[i]])
    # Small chance for trackloss:
    actual_look[fidgets[i-1]:fidgets[i]] = ifelse(rbinom(n = 1, size = 1, prob = participant$focus_level/5), NA, actual_look[fidgets[i-1]:fidgets[i]])
  }

  actual_look
}

set.seed(7)
library(dplyr); library(ggplot2); library(pbapply);library(broom)
data = data.frame(stringsAsFactors = FALSE, Participant = c(), Trial = c(), AOI = c(), Gender = c(), VocabSize=c(), Time = c())
experiment = list(trial_start = 0,
                  trial_end = 1000,
                  true_lookshift = 250)
participants = list()
for (sub_i in 1:12) {
  cat('.')
  participants[[sub_i]] = list(init_look_bias = rbeta(1, 3, 3),
                     look_shift_delay = rnorm(1, 350, sd = 50),
                     focus_level = rbeta(1,7,4),
                     gender = sample(x = c("M","F"), size = 1))
  participants[[sub_i]]$vocab_size = round(10^(participants[[sub_i]]$focus_level + rbeta(1, 5, 5)))
  for (trial_i in 1:5) {
    aoi_vec = generate_a_trial(participants[[sub_i]], experiment)
    data = bind_rows(data,
                     data.frame(stringsAsFactors = FALSE,
                                Participant = sub_i,
                                Trial = trial_i,
                                AOI = aoi_vec,
                                Gender = participants[[sub_i]]$gender,
                                VocabSize = participants[[sub_i]]$vocab_size,
                                Time = seq(from = experiment$trial_start, to = experiment$trial_end) ))
  }

}

data$Target = data$AOI == "Target"
data$Trackloss = is.na(data$Target)

library("eyetrackingR")
dopts= set_data_options("Participant", "Trackloss", "Time", "Trial", aoi_columns = "Target")

# Window Analysis:
df_window = make_time_window_data(data, dopts, aoi = "Target", predictor_columns = c("VocabSize"), summarize_by = "Participant")
plot(df_window, predictor_columns = c("VocabSize"))
ggsave(width = 8.6, height = 4.19, "clust/corr.png")

fit = lm(Prop~VocabSize, df_window)
summary(fit)

# Time Data:
df_time = make_time_sequence_data(data, dopts, time_bin_size = 75, aoi = "Target", predictor_columns = "VocabSize",
                                  summarize_by = "Participant")
plot(df_time) +
  coord_cartesian(ylim = c(0,1))
df_timebins_one_sample = analyze_time_bins(df_time, predictor_column = "intercept", formula = Prop - .5 ~ 1, test = "lm", alpha = .05)

plot(df_time, predictor_column = "VocabSize") +
  coord_cartesian(ylim = c(0,1)) 
ggsave(width = 8.6, height = 4.19, "clust/timecourse.png")



# Time analysis with MCP
df_timebins_mcp = analyze_time_bins(df_time, predictor_column = "VocabSize", test = "lm", alpha = .05)
plot(df_timebins_mcp) + coord_cartesian(ylim = c(0,4))
ggsave(width = 8.6, height = 4.19, "clust/mcp.png")
summary(df_timebins_mcp)

# Time analysis with bonferonni
num_tests = length(unique(df_timebins_mcp$Time))
df_timebins_bonf = analyze_time_bins(df_time, predictor_column = "VocabSize", test = "lm", alpha = .05 / num_tests)
plot(df_timebins_bonf) + coord_cartesian(ylim = c(0,4))
ggsave(width = 8.6, height = 4.19, "clust/bonf.png")
summary(df_timebins_bonf)

# Cluster analysis
df_clust = make_time_cluster_data(data = df_time,
                                  predictor_column = "VocabSize",
                                  aoi = "Target",
                                  test = "lm",
                                  threshold = qt(p = 1-.025, df = length(participants)))
summary(df_clust)
plot(df_clust) + coord_cartesian(ylim = c(0,4))
ggsave(width = 8.6, height = 4.19, "clust/clust.png")
cluster_analysis = analyze_time_clusters(df_clust, within_subj = FALSE, samples = 1000)
plot(cluster_analysis) + xlab("Summed Test-Statistic")
ggsave(width = 8.6, height = 4.19, "clust/clust_dist.png")
summary(cluster_analysis)


data_window = subset_by_window(data, data_options = dopts, rezero=FALSE, remove=TRUE, window_start_time = 675, window_end_time = 1000)
df_window_late = make_time_window_data(data_window, dopts, aoi = "Target", predictor_columns = c("VocabSize"), summarize_by = "Participant")
plot(df_window_late, predictor_columns = "VocabSize")

fit2 = lm(Prop~VocabSize, df_window_late)
summary(fit2)

