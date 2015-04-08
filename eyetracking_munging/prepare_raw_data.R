prepare_raw_data = function(data, 
                               data_options,
                               trial_start_msg = NULL,
                               trial_stop_msg = NULL,
                               prompt = TRUE) {
  ##
  # Clean Sample Report
  #
  # [fill me in]
  ##
  
  require("dplyr")
  
  ## Helpers:
  mk_form = function(..., cond = TRUE) {
    if (cond) {
      return( as.formula(paste0(...)) )
    } else {
      return( "NA" )
    }
  }
  
  ## Main:

  df = ungroup(data)
  dopts = data_options

  # Make Row for unique TrialID
  cat("\n\nMaking unique TrialID column...")
  df[['TrialID']] = paste(df[[dopts$participant_factor]], df[[dopts$trial_factor]], sep = "_")
  
  # Remove any trials in which the start or stop message in missing:
  df = df %>%
    group_by(TrialID) %>%
    mutate_(.dots = list(NoStartMsg = mk_form( "~all(!grepl('", trial_start_msg, "',", dopts$message_factor, "))" ),
                         NoStopMsg  = mk_form( "~all(!grepl('", trial_stop_msg,  "',", dopts$message_factor, "))", cond = !is.null(trial_stop_msg) )
    ) )
  no_start_msg_trials = unique( df$TrialID[which(df$NoStartMsg)] )
  no_stop_msg_trials = unique( df$TrialID[which(df$NoStopMsg)] )
  
  if ( length(no_start_msg_trials) + length(no_stop_msg_trials) > 0) {
    cat("\nFound", length(no_start_msg_trials), "trials without a start msg, and ", length(no_stop_msg_trials),
        "without a stop message. These trials will be removed.")
    if (prompt) {
      readline("Press enter to continue. ")
    }
  }
    
  # Add TimeInTrial Column:
  cat("\n\nAdding TimeInTrial column...")
  ts_command = mk_form("~", dopts$time_factor, " - ", dopts$time_factor, "[grep('", trial_start_msg, "',", dopts$message_factor, ")][1]")
  df = df %>%
    group_by(TrialID) %>%
    mutate_(.dots = list(TimeInTrial = ts_command) )
  cat("\nAdded. You may want to set data_options$time_factor to 'TimeInTrial'")
  
  # Mark Post-Trial TSs:
  df = df %>%
    mutate_(.dots = list(PostTrial = 
                           mk_form("~TimeInTrial > TimeInTrial[grep('", trial_stop_msg, "',", dopts$message_factor, ")][1]",  
                                   cond = !is.null(trial_stop_msg) )
    ))
  
  # Add Sample Index (if needed):
  if ( is.null(dopts$sample_factor) | is.null(df[[dopts$sample_factor]]) ) {
    cat("\n\nNo sample index column found, adding...")
    df = df %>%
      group_by(TrialID) %>%
      mutate(SampleIndex = 1:n())
    cat("\nAdded. You may want to set data_options$sample_factor to 'SampleIndex'")
  }
  
  # Verify Types:
  df = ungroup(df)
  df = verify_dataset(data = df, data_options = dopts, silent = !prompt)
    
  return(df)
  
}
