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
#' @param other_dv_columns Other columns to summarize, aside from AOI column.
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
                  Elog = if_else(SamplesTotal==0, NA_real_, log((SamplesInAOI + .5) / (SamplesTotal - SamplesInAOI + .5))),
                  Weights = if_else(SamplesTotal==0, NA_real_,1 / ( (1 / (SamplesInAOI + .5)) / (1 / (SamplesTotal - SamplesInAOI + .5)) ) ),
                  Prop = SamplesInAOI / SamplesTotal,
                  LogitAdjusted = .logit_adj(Prop),
                  ArcSin = asin(sqrt(Prop))
    )
    return(out)
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



#' Add the original class/attributes back onto result (usually of dplyr operation)
#'
#' @param x The original object, class inforamation you want to restore.
#' @param result Some transformation of \code{x}, which may have removed its class/attributes.
#' @param ... Ignored 
#'
#' @return The \code{result}, now with class/attribute information restored.
#' @export
reclass <- function(x, result, ...) {
  UseMethod('reclass')
}

reclass.eyetrackingR_df <- function(x, result, ...) {
  if (is.null(result)) return(result)
  if (length(list(...))>0) warning(call. = FALSE, "Further arguments that aren't `x` and `result` were ignored.")
  class_idx <- which(class(x) == "eyetrackingR_df")
  restore_classes <- class(x)[1:class_idx]
  class(result) <- unique(c(restore_classes, class(result)))
  attr(result, "eyetrackingR") <- attr(x, "eyetrackingR")
  return(result)
}



