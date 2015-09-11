


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
for (sub_i in 1:12) {
  cat('.')
  participant = list(init_look_bias = rbeta(1, 3, 3),
                     look_shift_delay = rnorm(1, 350, sd = 50),
                     focus_level = rbeta(1,5,3),
                     gender = sample(x = c("M","F"), size = 1))
  participant$vocab_size = round(10^(participant$focus_level + rbeta(1, 5, 5)))
  for (trial_i in 1:5) {
    aoi_vec = generate_a_trial(participant, experiment)
    data = bind_rows(data,
                     data.frame(stringsAsFactors = FALSE,
                                Participant = sub_i,
                                Trial = trial_i,
                                AOI = aoi_vec,
                                Gender = participant$gender,
                                VocabSize = participant$vocab_size,
                                Time = seq(from = experiment$trial_start, to = experiment$trial_end) ))
  }

}

data$Target = data$AOI == "Target"
data$Trackloss = is.na(data$Target)

source("../analyze-eyetracking.R")
dopts= set_data_options("Participant", "Trackloss", "Time", "Trial", aoi_columns = "Target")

# Window Analysis:
df_window = make_time_window_data(data, dopts, aoi = "Target", predictor_columns = c("VocabSize", "Gender"))
plot(df_window, dopts, predictor_columns = c("VocabSize", "Gender"))

# Time Analysis:
df_time = make_time_sequence_data(data, dopts, time_bin_size = 100, aoi = "Target", predictor_columns = "VocabSize",
                                  summarize_by = "Participant")
plot(df_time, dopts) +
  coord_cartesian(ylim = c(0,1))
plot(df_time, dopts, predictor_column = "VocabSize") +
  coord_cartesian(ylim = c(0,1))

# Cluster analysis
df_clust = make_time_cluster_data(data = df_time, data_options = dopts,
                                  predictor_column = "VocabSize",
                                  aoi = "Target",
                                  test = "lm",
                                  threshold = -.5)
summary(df_clust)
plot(df_clust)
cluster_analysis = analyze_time_clusters(df_clust, data_options = dopts, within_subj = FALSE, samples = 1000)
plot(cluster_analysis)
summary(cluster_analysis)

