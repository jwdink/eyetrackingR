#' .get_crit_val()
#'
#' Get the threshold, given critical value and test. For `analyze_time_bins`
#'
#' @param vec Logical
#' @return A numeric vector
.get_threshold <- function(alpha, test, dfs, quiet) {
    if (test %in% c("lmer","glmer")) {
      if (!quiet) message("Using the normal approximation for critical value on parameter in ", test)
      crit_pos =  qnorm(p=1-alpha/2)
    } else if (test=="t.test") {
      crit_pos <- qt(1-alpha/2, df = dfs)
    } else if (test=="wilcox.test") {
      crit_pos <- NA
    } else if (test=="lm" | test=="glm") {
      crit_pos <- qt(1-alpha/2, df = dfs)
    } 
  return(crit_pos)
}

#' .label_consecutive()
#'
#' A helper function to label/enumerate runs of TRUEs in a logical vector, with NA for FALSEs
#'
#' @param vec Logical
#' @return A numeric vector
.label_consecutive <- function(vec) {
  vec = c(0,vec)
  vec[is.na(vec)] = 0
  out = c(cumsum(diff(vec)==1))
  vec = vec[-1]
  out[!vec] = NA
  out
}

#' .make_proportion_looking_summary()
#'
#' A helper function for make_time_window_data and make_time_sequence_data. Takes a dataframe, groups it, and
#' returns proportion looking and relevant transformations
#'
#' @param data       The data
#' @param groups     The groups
#' @param aoi_col    Name of AOI column
#' @return dataframe Transformed dataframe
.make_proportion_looking_summary <-
  function(data, groups, aoi_col, other_dv_columns) {
    
    .logit_adj <- function(prop) {
      non_zero_ind <- which(prop!=0)
      if (length(non_zero_ind) < 1) return(NA)
      smallest <- min(prop[non_zero_ind], na.rm=TRUE)
      prop_adj <- prop
      prop_adj <- ifelse(prop_adj == 1, 1 - smallest/2, prop_adj)
      prop_adj <- ifelse(prop_adj == 0, smallest/2, prop_adj)
      return( log(prop_adj / (1-prop_adj)) )
    }
    
    # Group, summarize Samples
    
    summarize_expression <- list(
      SamplesInAOI = interp( ~ sum(AOI_COL, na.rm = TRUE), AOI_COL = aoi_col),
      SamplesTotal = interp( ~ sum(!is.na(AOI_COL)), AOI_COL = aoi_col) # ignore all NAs
      )
    for (other_dv in other_dv_columns) {
      summarize_expression[[other_dv]] <- interp( ~ mean(OTHER_DV, na.rm=TRUE), OTHER_DV = as.name(other_dv) )
    }
    df_grouped <- group_by_(data, .dots = groups)
    df_summarized <- summarize_(df_grouped, .dots = summarize_expression)
    
    # Calculate Proportion, Elog, etc.
    aoi <- as.character(aoi_col)
    out <- ungroup(df_summarized)
    out <- mutate(out,
                  AOI = aoi,
                  Elog = log((SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5)),
                  Weights = 1 / ( (1 / (SamplesInAOI + .5)) / (1 / (SamplesTotal - SamplesInAOI + .5)) ),
                  Prop = SamplesInAOI / SamplesTotal,
                  LogitAdjusted = .logit_adj(Prop),
                  ArcSin = asin(sqrt(Prop))
    )
    out
  }

#' Simulate an eyetrackingR dataset
#' 
#' This function creates an eyetrackingR dataset (i.e., already run through make_eyetrackingr_data).
#' This can be helpful for examining the false-alarm and sensitivity of analysis-techniques via 
#' simulations.
#' 
#' @param num_participants        Number of participants
#' @param num_items_per_condition Number of trials per-subject per-condition.
#' @param trial_length            How long is the trial (in ms)?
#' @param pref                    Their preference between the two AOIs in the "high" condition, 
#'   where 1 is 100% preference for AOI1, and 0 is 100% preference for AOI2. Default is .5 (equal 
#'   preference). In the "low" condition, their preference between the two AOIs is equal, so default
#'   is no effect of condition.
#' @param pref_window             Vector of length two, specifying start and end of time-window in
#'   which participants expressed the preference specified in \code{pref}. Default is the entire trial
#' @param noisy_window            Vector of length two, specifying start and end of time-window in
#'   which there was substantial trackloss during the trial.
#' @param ...                     Ignored
#' @export
#' @return Dataframe with eye-tracking data
simulate_eyetrackingr_data <- function(num_participants= 16, 
                                       num_items_per_condition = 6, 
                                       trial_length = 5000, 
                                       pref =.50, 
                                       pref_window = c(1,trial_length),
                                       noisy_window = NULL,
                                       ...) {
  
  if (is.null(noisy_window)) noisy_window <- c(trial_length, trial_length)
  
  .generate_random_trial <- function(potential_switches_per_trial, pref, this_pref_wind, baseline_pref =.50) {
    
    # Randomly generate potential switch times via poisson process:
    switch_diffs_unnormed <- rexp(potential_switches_per_trial+1)
    switch_diffs_normed <- round((switch_diffs_unnormed/sum(switch_diffs_unnormed))*trial_len) +1 
    switch_diffs_normed <- switch_diffs_normed[1:potential_switches_per_trial]
    switch_times_normed <- cumsum(c(0,switch_diffs_normed))
    
    # Determine preference/which-aoi based on time in trial:
    inside_pref_wind  <- (switch_times_normed>this_pref_wind[1] & switch_times_normed<this_pref_wind[2])
    which_aoi <- rbinom(length(switch_times_normed), size = 1, 
                        prob = ifelse(inside_pref_wind, yes = pref, no = baseline_pref))
    
    # Generate list of looks:
    out <- unlist(lapply(X = seq_along(switch_diffs_normed), FUN = function(i) rep(which_aoi[i], times = switch_diffs_normed[i])))
    last_ind <- length(out)
    out <- out[1:trial_len] # make sure vector is of length trial_len
    out[is.na(out)] <- out[last_ind] # if it was too short before, pad with last place they were looking
    as.logical(out)
  }
  
  .random_odds <- function(avg_odds = 1) {
    rnorm(1, mean = avg_odds, sd = .50)
  }
  
  noisy_wind <- noisy_window/10
  pref_wind <- pref_window/10 - 1
  pref_wind_len <- pref_wind[2] - pref_wind[1]
  trial_len = (trial_length/10) - 1
  switchiness <- round(trial_len / 60) # corresponds to roughly 8 possible switches in a 5-second window.
  
  dat <- data_frame(Participant = rep(1:num_participants, each = num_items_per_condition*trial_len) ) %>%
    group_by(Participant) %>%
    mutate(.NumSwitches = rpois(1, lambda = switchiness)+1,
           .SpeedOffset = rnorm(1, sd = 1/2),
           Trial = rep(1:num_items_per_condition, each = n() / num_items_per_condition),
           Item  = Trial,
           Condition = ifelse( (Participant%%2)==0, "High", "Low"),
           ParticipantLogOdds = ifelse(Condition == "High", .random_odds(qlogis(pref)), .random_odds(qlogis(.50)) ))  %>%
    group_by(Item) %>%
    mutate(ItemLogOdds = ifelse(Condition == "High", .random_odds(qlogis(pref)), .random_odds(qlogis(.50)) )) %>%
    group_by(Participant, Item) %>%
    mutate(TimeInTrial = (1:n())*10,
           RT = rgamma(1, shape = 2, scale = 3+.SpeedOffset)*100, #rexp(1, rate = abs( (50+.SpeedOffset)^-1 ) )*10, 
           .PrefOnset = pref_wind[1] + RT/10,
           .PrefOnset = ifelse(.PrefOnset>pref_wind[2], pref_wind[2], .PrefOnset),
           AOI1 = .generate_random_trial(unique(.NumSwitches), 
                                         pref = plogis( (unique(ItemLogOdds)+unique(ParticipantLogOdds))/2 ),
                                         # each trial pref is avg of that item "pref" w/ that participant pref
                                         this_pref_wind = c(unique(.PrefOnset), pref_wind[2])),
                                         # each subject is delayed by a certain amount in when their preference emerges
                                        
           AOI2 = !AOI1,
           Trackloss = .generate_random_trial(trial_len / 10, 
                                             pref = .66,
                                             this_pref_wind = noisy_wind,
                                             baseline_pref = .10
                                             ) ) %>%
    ungroup() %>% select(-.PrefOnset, -.NumSwitches, -.SpeedOffset) %>%
    mutate(Participant = factor(Participant),
           Condition = factor(Condition),
           Trial = factor(Trial),
           Item  = factor(Item))
  
  make_eyetrackingr_data(dat, 
                         treat_non_aoi_looks_as_missing = TRUE,
                         item_columns = "Item",
                         participant_column = "Participant", 
                         trackloss_column = "Trackloss", 
                         time_column = "TimeInTrial", 
                         trial_column = "Trial", 
                         aoi_columns = c("AOI1", "AOI2"))
}






# EyetrackingR-Friendly-Subset ----------------------------------------------------------------------------------

#' EyetrackingR friendly subset
#' @describeIn subset time_sequence_data friendly subset
#' @param x A dataframe to be subsetted
#' @param ... further arguments to be passed to or from other methods.
#' @export
#' @return A dataframe with only selected elements
subset.time_sequence_data <- function(x, ...) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 
      'boot_splines_analysis', "time_cluster_data", 'bin_analysis','eyetrackingR'
    )
  temp_remove <- class(x)[class(x) %in% potential_classes]
  class(x) <- class(x)[!class(x) %in% potential_classes]
  temp_attr <- attr(x, "eyetrackingR") # also attributes
  
  out <- subset(x, ...)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

#' @describeIn subset time_window_data friendly subset
#' @export
subset.time_window_data <- subset.time_sequence_data

#' @describeIn subset bin_analysis friendly subset
#' @export
subset.bin_analysis <- subset.time_sequence_data

#' @describeIn subset boot_splines friendly subset
#' @export
subset.boot_splines_data <- subset.time_sequence_data

#' @describeIn subset time_cluster_data friendly subset
#' @export
subset.time_cluster_data <- subset.time_sequence_data

#' @describeIn subset boot_splines_analysis friendly subset
#' @export
subset.boot_splines_analysis <- subset.time_sequence_data

#' @describeIn subset onset_data friendly subset
#' @export
subset.onset_data <- subset.time_sequence_data

#' @describeIn subset eyetrackingR friendly subset
#' @export
subset.eyetrackingR <- subset.time_sequence_data

# Friendly Dplyr Verbs: Mutate ----------------------------------------------------------------------------------
#' EyetrackingR friendly mutate
#' @describeIn mutate_ eyetrackingR friendly mutate
#' @param .data An eyetrackingR dataframe
#' @param ... name value pairs of expressions
#' @param .dots Used to work around non-standard evaluation. See vignette("nse") for details.
#' @export
#' @return A dataframe
mutate_.time_sequence_data <- function(.data, ..., .dots) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 
      'boot_splines_analysis', "time_cluster_data", 'bin_analysis', 'eyetrackingR'
    )
  temp_remove <- class(.data)[class(.data) %in% potential_classes]
  class(.data) <- class(.data)[!class(.data) %in% potential_classes]
  temp_attr <- attr(.data, "eyetrackingR") # also attributes
  
  out <- mutate_(.data, ...=..., .dots=.dots)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.time_window_data <- mutate_.time_sequence_data

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.bin_analysis <- mutate_.time_sequence_data

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.boot_splines_data <- mutate_.time_sequence_data

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.time_cluster_data <- mutate_.time_sequence_data

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.boot_splines_analysis <- mutate_.time_sequence_data

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.onset_data <- mutate_.time_sequence_data

#' @describeIn mutate_ eyetrackingR friendly mutate
#' @export
mutate_.eyetrackingR <- mutate_.time_sequence_data

# Friendly Dplyr Verbs: GroupBy ----------------------------------------------------------------------------------

#' EyetrackingR friendly group_by
#' @describeIn group_by_ eyetrackingR friendly group_by
#' @param .data An eyetrackingR dataframe
#' @param ... variables to group by. Duplicated groups will be silently dropped.
#' @param .dots Used to work around non-standard evaluation. See vignette("nse") for details.
#' @param add By default, when add = FALSE, group_by will override existing groups. To instead add
#'   to the existing groups, use add = TRUE
#' @export
#' @return A dataframe
group_by_.time_sequence_data <- function(.data, ..., .dots, add= FALSE) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 
      'boot_splines_analysis', "time_cluster_data", 'bin_analysis', 'eyetrackingR'
    )
  temp_remove <- class(.data)[class(.data) %in% potential_classes]
  class(.data) <- class(.data)[!class(.data) %in% potential_classes]
  temp_attr <- attr(.data, "eyetrackingR") # also attributes
  
  out <- group_by_(.data, ...=..., .dots=.dots, add=add)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.time_window_data <- group_by_.time_sequence_data

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.bin_analysis <- group_by_.time_sequence_data

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.boot_splines_data <- group_by_.time_sequence_data

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.time_cluster_data <- group_by_.time_sequence_data

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.boot_splines_analysis <- group_by_.time_sequence_data

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.onset_data <- group_by_.time_sequence_data

#' @describeIn group_by_ eyetrackingR friendly group_by
#' @export
group_by_.eyetrackingR <- group_by_.time_sequence_data


# Friendly Dplyr Verbs: Filter ----------------------------------------------------------------------------------

#' EyetrackingR friendly filter
#' @describeIn filter_ eyetrackingR friendly filter
#' @param .data An eyetrackingR dataframe
#' @param ... Logical predicates. Multiple conditions are combined with &.
#' @param .dots Used to work around non-standard evaluation. See vignette("nse") for details.
#' @export
#' @return A dataframe
filter_.time_sequence_data <- function(.data, ..., .dots) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 
      'boot_splines_analysis', "time_cluster_data", 'bin_analysis', 'eyetrackingR'
    )
  temp_remove <- class(.data)[class(.data) %in% potential_classes]
  class(.data) <- class(.data)[!class(.data) %in% potential_classes]
  temp_attr <- attr(.data, "eyetrackingR") # also attributes
  
  out <- filter_(.data, ...=..., .dots=.dots)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.time_window_data <- filter_.time_sequence_data

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.bin_analysis <- filter_.time_sequence_data

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.boot_splines_data <- filter_.time_sequence_data

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.time_cluster_data <- filter_.time_sequence_data

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.boot_splines_analysis <- filter_.time_sequence_data

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.onset_data <- filter_.time_sequence_data

#' @describeIn filter_ eyetrackingR friendly filter
#' @export
filter_.eyetrackingR <- filter_.time_sequence_data


# Friendly Dplyr Verbs: Ungroup ----------------------------------------------------------------------------------

#' EyetrackingR friendly ungroup
#' @describeIn ungroup EyetrackingR friendly ungroup
#' @param x An eyetrackingR dataframe
#' @export
#' @return An eyetrackingR dataframe, without dplyr groups
ungroup.time_sequence_data <- function(x) {
  # remove class names (avoid infinite recursion):
  potential_classes <-
    c(
      'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 
      'boot_splines_analysis', "time_cluster_data", 'bin_analysis', 'eyetrackingR'
    )
  temp_remove <- class(x)[class(x) %in% potential_classes]
  class(x) <- class(x)[!class(x) %in% potential_classes]
  temp_attr <- attr(x, "eyetrackingR") # also attributes
  
  out <- ungroup(x)
  
  # reapply class/attributes
  class(out) = c(temp_remove, class(out))
  attr(out, "eyetrackingR") = temp_attr
  
  return(out)
}

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.time_window_data <- ungroup.time_sequence_data

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.bin_analysis <- ungroup.time_sequence_data

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.boot_splines_data <- ungroup.time_sequence_data

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.time_cluster_data <- ungroup.time_sequence_data

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.boot_splines_analysis <- ungroup.time_sequence_data

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.onset_data <- ungroup.time_sequence_data

#' @describeIn ungroup EyetrackingR friendly ungroup
#' @export
ungroup.eyetrackingR <- ungroup.time_sequence_data


# Friendly Dplyr Verbs: Left-Join ----------------------------------------------------------------------------------

#' EyetrackingR friendly Left-Join
#' @describeIn left_join  EyetrackingR friendly Left-Join
#' @param x Left dataframe
#' @param y Right dataframe
#' @param by a character vector of variables to join by
#' @param copy Copy to same src? Default FALSE
#' @param ... other parameters passed onto methods
#' @export
#' @return A dataframe
left_join.time_sequence_data <-
  function(x, y, by = NULL, copy = FALSE, ...) {
    # remove class names (avoid infinite recursion):
    potential_classes <-
      c(
        'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 
        'boot_splines_analysis', "time_cluster_data", 'bin_analysis',
        'eyetrackingR'
      )
    temp_remove <- class(x)[class(x) %in% potential_classes]
    class(x) <- class(x)[!class(x) %in% potential_classes]
    temp_attr <- attr(x, "eyetrackingR") # also attributes
    
    out <- left_join(
      x = x, y = y, by = by, copy = copy, ...
    )
    
    # reapply class/attributes
    class(out) = c(temp_remove, class(out))
    attr(out, "eyetrackingR") = temp_attr
    
    return(out)
  }

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.time_window_data <- left_join.time_sequence_data

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.bin_analysis <- left_join.time_sequence_data

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.boot_splines_data <- left_join.time_sequence_data

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.time_cluster_data <- left_join.time_sequence_data

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.boot_splines_analysis <- left_join.time_sequence_data

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.onset_data <- left_join.time_sequence_data

#' @describeIn left_join EyetrackingR friendly Left-Join
#' @export
left_join.eyetrackingR <- left_join.time_sequence_data


## Should NOT be necessary 
## (only needed for tbl_dfs, and if your eyetrackingR object has tbl_df class then this won't get caught anyways)
# EyetrackingR friendly extract
# #' @describeIn Extract
# #' @param data A dataframe
# #' @export
# #' @return A dataframe
# 
# `[.time_sequence_data` <- function(data, i, j, ...) {
#   # remove class names (avoid infinite recursion):
#   potential_classes <-
#     c(
#       'time_sequence_data', 'time_window_data', 'onset_data', 'boot_splines_data', 'boot_splines_analysis', "time_cluster_data", 'bin_analysis'
#     )
#   temp_remove <- class(data)[class(data) %in% potential_classes]
#   class(data) <- class(data)[!class(data) %in% potential_classes]
#   temp_attr <- attr(data, "eyetrackingR") # also attributes
#   
#   out <- data[i = i, j = j,... = ...]
#   
#   # reapply class/attributes
#   class(out) = c(temp_remove, class(out))
#   attr(out, "eyetrackingR") = temp_attr
#   
#   return(out)
# }
# 
# #' @describeIn Extract
# #' @export
# `[.time_window_data` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.bin_analysis` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.boot_splines_data` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.time_cluster_data` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.boot_splines_analysis` <- `[.time_sequence_data`
# 
# #' @describeIn Extract
# #' @export
# `[.onset_data` <- `[.time_sequence_data`