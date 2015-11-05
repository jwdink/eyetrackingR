# Loading/Cleaning/Describing Data ------------------------------------------------------------------------

#' Treat gaze data outside of all AOIs as missing
#'
#' Should gaze outside of any AOI be considered trackloss? 
#'
#' @param data The data
#' @param data_options The data options
#' @return Data with correct AOIs

.convert_non_aoi_to_missing <- function(data, data_options) {
  
  # Create version of AOIs with no NAs:
  .narepl <- function(x) ifelse(is.na(x), 0, x)
  data_no_na <- mutate_each_(data, funs(.narepl), vars = sapply(data_options$aoi_columns, as.name))
  
  # Find all rows which have no AOIs in them:
  data_no_na[[".AOISum"]] <- 0
  for (aoi in data_options$aoi_columns) {
    data_no_na[[".AOISum"]] <- data_no_na[[".AOISum"]] + data_no_na[[aoi]]
  }

  # Set these rows as trackloss:
  data[[data_options$trackloss_column]] <- ifelse(data_no_na[[".AOISum"]] == 0, TRUE, data_no_na[[data_options$trackloss_column]])
  
  return(data)
}

#' Convert raw data for use in eyetrackingR
#' 
#' This should be the first function you use when using eyetrackingR for a project (potentially with
#' the exception of `add_aoi`, if you need to add AOIs). This function takes your raw dataframe, as 
#' well as information about your dataframe. It confirms that all the columns are the right format, 
#' based on this information. Further if \code{treat_non_aoi_looks_as_missing} is set to TRUE, it 
#' converts non-AOI looks to missing data (see the "Preparing your data" vignette for more 
#' information).
#' 
#' eyetrackingR is designed to deal with data in a (relatively) raw form,
#' where each row specifies a sample. Each row should represent an equally spaced unit of time
#' (e.g., if your eye-tracker's sample rate is 100hz, then each row corresponds to the
#' eye-position every 10ms). This is in contrast to the more parsed data that the software bundled
#' with eye-trackers can sometimes output (e.g., already parsed into saccades or fixations). For
#' eyetrackingR, the simplest data is the best. This also maximizes compatibility: eyetrackingR
#' will work with any eye-tracker's data (e.g., Eyelink, Tobii, etc.), since it requires the most
#' basic format.
#' 
#' @param data               Your original data. See details section below.  
#' @param participant_column Column name for participant identifier
#' @param trackloss_column   Column name indicating trackloss
#' @param time_column        Column name indicating time
#' @param trial_column       Column name indicating trial identifier
#' @param item_columns       Column names indicating items (optional)
#' @param aoi_columns        Names of AOIs
#' @param treat_non_aoi_looks_as_missing This is a logical indicating how you would like to perform
#'   "proportion-looking" calculations, which are critical to any eyetracking analysis. If set to
#'   TRUE, any samples that are not in any of the AOIs (defined with the \code{aoi_columns} 
#'   argument) are treated as missing data; when it comes time for eyetrackingR to calculate 
#'   proportion looking to an AOI, this will be calculated as "time looking to that AOI divided by 
#'   time looking to all other AOIs." In contrast, if this parameter is set to FALSE, proportion 
#'   looking to an AOI will be calculated as "time looking to that AOI divided by total time 
#'   looking."
#'   
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#'   
#' @export
#' @return Dataframe ready for use in eyetrackingR.

make_eyetrackingr_data <- function(data, 
                                   participant_column,
                                   trackloss_column,
                                   time_column,
                                   trial_column,
                                   aoi_columns,
                                   treat_non_aoi_looks_as_missing,
                                   item_columns = NULL) {
  
  ## Data Options:
  data_options = list(participant_column = participant_column,
                        trackloss_column = trackloss_column,
                        time_column = time_column,
                        trial_column = trial_column,
                        item_columns = item_columns,
                        aoi_columns = aoi_columns,
                        treat_non_aoi_looks_as_missing = treat_non_aoi_looks_as_missing)
  
  ## Check for Reserved Column Name:
  if (data_options$time_column == "Time") {
    stop("We apologize for the inconvenience, but your `time_column` cannot be called 'Time' ",
         "(this is a reserved name that eyetrackingR uses). Please rename.")
  } 
  if ("Time" %in% colnames(data)) {
    warning("Your dataset has a column called 'Time', but this column name is reserved for eyetrackingR. Will rename to 'TimeOriginal'...")
    data$TimeOriginal <- data$Time
    data$Time <- NULL
  }
  
  ## Verify Columns:
  out <- data
  as.numeric2 <- function(x) as.numeric(as.character(x))
  check_then_convert <- function(x, checkfunc, convertfunc, colname) {
    if (!checkfunc(x)) {
      message("Converting ", colname, " to proper type.")
      convertfunc(x)
    } else {
      x
    }
  }
  col_type_converter <- list(participant_column = function(x) check_then_convert(x, is.factor, as.factor, "Participants"),
                             time_column = function(x) check_then_convert(x, is.numeric, as.numeric2, "Time"),
                             trial_column = function(x) check_then_convert(x, is.factor, as.factor, "Trial"),
                             trackloss_column = function(x) check_then_convert(x, is.logical, as.logical, "Trackloss"),
                             item_columns = function(x) check_then_convert(x, is.factor, as.factor, "Item"),
                             aoi_columns = function(x) check_then_convert(x, is.logical, as.logical, "AOI"))
  
  for (col in names(col_type_converter)) {
    for (i in seq_along(data_options[[col]])) {
      if (is.null(out[[data_options[[col]][i]]]))
        stop("Data are missing: ", col)
      out[[data_options[[col]][i]]] <- col_type_converter[[col]](out[[data_options[[col]][i]]])
    }
  }
  
  ## Deal with Non-AOI looks:
  if (treat_non_aoi_looks_as_missing) {
    out <- .convert_non_aoi_to_missing(out, data_options)
  }
  
  ## Assign attribute:
  class(out) <- c("eyetrackingR", "data.frame")
  attr(out, "eyetrackingR") <- list(data_options = data_options)
  return(out)
  
}

#' Add an area-of-interest to your dataset, based on x-y coordinates and the AOI rectangle.
#' 
#' Eyetracking-R requires that there is a column for each area-of-interest, specifying whether the gaze is 
#' within that area for each sample. This function creates an AOI column if needed.
#' 
#' Many eyetracking software packages export your data with a column corresponding to each AOI; however, if
#' your software does not do this, or if you had to define or revise your AOIs after running the experiment,
#' then this function will add the necessary AOI columns for you. The function takes two dataframes: (1) your
#' original data, (2) a dataframe specifying the bounding box for the AOI. The latter can specify a different
#' bounding box for each trial, each subject, each image, or even each video-frame-- anything you like. The
#' two dataframes are simply joined by matching any columns they have in common (case sensitive!), so if
#' there's a unique AOI for each "Trial" in the \code{aoi_dataframe}, and there's a "Trial" column in the
#' \code{data} dataframe, then the unique AOI coordinates for each trial will be used.
#' 
#' @param data Your data
#' @param aoi_dataframe A dataframe specifying the bounding-box for the AOI
#' @param x_col,y_col What are the column names for the x and y coordinates in your dataset?
#' @param aoi_name What is the name of this AOI?
#' @param x_min_col,x_max_col What are the column names for the left and right edge of the AOI-bounding box?
#'   Default "L","R"
#' @param y_min_col,y_max_col What are the column names for the top and bottom edge of the AOI-bounding box?
#'   Default "T","B"
#' @export
#' @return Dataset with a new column indicating whether gaze is in the AOI
add_aoi <- function(data, aoi_dataframe,
                     x_col, y_col,
                     aoi_name,
                     x_min_col = "L", x_max_col = "R",
                     y_min_col = "T", y_max_col = "B"
) {
  
  ## Helper
  .inside_rect = function(pt, ltrb) {
    if (is.null(dim(pt))) {
      pt = as.matrix(pt)
      pt = t(pt)
    }
    if (is.null(dim(ltrb))) {
      ltrb = as.matrix(ltrb)
      ltrb = t(ltrb)
    }
    if (dim(pt)[2]   != 2) stop('First argument should be a vector of length 2 or an n-by-2 matrix')
    if (dim(ltrb)[2] != 4) stop('Second argument should be a vector of length 4 or an n-by-4 matrix')
    
    return(
      pt[,1] >= ltrb[,1] &
      pt[,1] <= ltrb[,3] &
      pt[,2] >= ltrb[,2] &
      pt[,2] <= ltrb[,4] 
    )
  }
  
  ## Join AOI info to dataset
  df_joined <- left_join(data, aoi_dataframe)
  
  ## Make AOI column
  message("Making ", aoi_name, " AOI...")
  data[[aoi_name]] <- .inside_rect(pt    = cbind(df_joined[[x_col]], df_joined[[y_col]]),
                                   ltrb  = cbind(df_joined[[x_min_col]], df_joined[[y_min_col]], df_joined[[x_max_col]], df_joined[[y_max_col]])
  )
  
  return(data)
}


#' Extract a subset of the dataset within a time-window in each trial.
#' 
#' One of the more annoying aspects of preparing raw eyetracking data is filtering data down into the relevant
#' window within the trial, since for many experiments the precise start and end time of this window can vary 
#' from trial to trial. This function allows for several approaches to subsetting data into the relevant time-
#' window-- see 'Details' below.
#' 
#' \enumerate{
#'   \item The trial start/end times can be indicated by a message that is sent (e.g., TRIAL_START) in a 
#' particular row for each trial. In this case, the timestamp of that row is used.
#'   \item The trial start/end times can be indicated in by a column that specifies trial start/end times for each
#' trial.
#'   \item The trial start/end times can be indicated by the actual start and stop time, the same across all
#' trials (the simplest case).
#' }
#' 
#' If you only have a start time but the end time doesn't need adjusting, then leave the end time argument blank;
#' and vice versa.
#' 
#' This function can either rezero your data (the trial start time you select is the new zero-time-point), or 
#' not. The former is useful when performing initial data-cleaning (e.g., different trial-starts on each 
#' trial, as indicated by a message), and the latter is useful if you want to "zoom in" on a particular 
#' portion of your data while keeping obvious the fact that there were other parts of the trial (e.g., an 
#' image always appears 5000ms-7000ms in the trial, so for one analysis you are only interested in this 
#' portion).
#' 
#' @param data               Your original dataset
#' @param rezero             Should the beginning of the window be considered the zero point of the timestamp?
#'   Default TRUE
#' @param remove             Should everything before the beginning and after the end of the window be removed? 
#'   Default TRUE. If set to FALSE and \code{rezero} is set to FALSE, an error is thrown (since in this case,
#'   the function would not do anything).
#' @param window_start_msg   For method (1). A message that is present only in the row whose time corresponds 
#'   to the trial start time. Common for eyetrackers that send a message at trial/stimuli start.
#' @param window_end_msg     For method (1). A message that is present only in the row whose time corresponds 
#'   to the trial end time. Common for eyetrackers that send a message at trial-end/keypress/lookaway/etc.
#' @param msg_col            For method (1). If you are indicating the trial start/end with a message column,
#'   this is the name of that column.
#' @param window_start_col   For method (2). A column that gives the start time for each trial.
#' @param window_end_col     For method (2). A column that gives the end time for each trial.
#' @param window_start_time  For method (3). Number indicating a start time that applies to all trials.
#' @param window_end_time    For method (3). Number indicating an end time that applies to all trials.
#' @param quiet              Suppress messages? Default FALSE
#' 
#' @examples
#' data("word_recognition")
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' 
#' # zoom in to 15500-21000ms
#' response_window <- subset_by_window(data,
#'                                     window_start_time = 15500,
#'                                     window_end_time = 21000, rezero = FALSE, remove = TRUE)
#' 
#' # zoom in to 15500-21000ms and re-zero so timestamps start at 0
#' response_window <- subset_by_window(data,
#'                                     window_start_time = 15500, 
#'                                     window_end_time = 21000, 
#'                                     rezero = TRUE, 
#'                                     remove = TRUE)
#' 
#' # keep all data, but re-zero it
#' response_window <- subset_by_window(data,
#'                                     window_start_time = 0, 
#'                                     rezero = TRUE, 
#'                                     remove = FALSE)
#' 
#' @export
#' @return Subsetted data

subset_by_window <- function(data, 
                             rezero = TRUE,
                             remove = TRUE,
                             window_start_msg = NULL, window_end_msg = NULL, msg_col = NULL,
                             window_start_col = NULL, window_end_col = NULL,
                             window_start_time= NULL, window_end_time= NULL,
                             quiet = FALSE
                             ) {
  
  ## Helper:
  .safe_msg_checker = function(msg_vec, msg, time_vec, ppt_vec, trial_vec) {
    bool = (msg_vec==msg)
    if (length(which(bool)) != 1) {
      warning("The message ", msg, 
              " does not appear *exactly* one time for participant '", ppt_vec[1], 
              "' on trial '", trial_vec[1], "'. Trial will be removed from dataset.")
      return(Inf) # not returning NA due to bug (at time of writing) in dplyr
    }
    return(time_vec[bool])
  }
  
  # Prelims:
  orig_classes <- class(data)
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  time_col <- as.name(data_options$time_column)
  ppt_col <- as.name(data_options$participant_column)
  trial_col <- as.name(data_options$trial_column)
  if (!(rezero | remove)) stop("If both 'rezero' and 'remove' are FALSE, then this function doesn't do anything!")
  
  # Which method?
  start_method_num <- !(sapply(list(window_start_msg, window_start_col, window_start_time), is.null))
  stop_method_num <- !(sapply(list(window_end_msg, window_end_col, window_end_time), is.null))
  if ( sum(start_method_num) > 1 | sum(stop_method_num) > 1 ) {
    stop("Please use exactly one of the methods for start/stop time (msg, column, or time).")
  }
  
  # Start Time:
  if (!any(start_method_num)) {
    if (rezero) stop("Rezero must be set to FALSE if no start time specified.")
    start_method_num <- c(FALSE, FALSE, TRUE)
    window_start_time <- -Inf
  }
  if (which(start_method_num) == 1) {
    # Message:
    data[[msg_col]] <- as.character(data[[msg_col]])
    if (!is.character(msg_col)) stop("Please enter a column name for the message column (in quotes).")
    data <- group_by_(.data = data,
                      .dots = list(data_options$participant_column, data_options$trial_column
                      ))
    data <- mutate_(data, 
                    .dots = list(.WindowStart = interp(~.safe_msg_checker(MSG_COL, START_MSG, TIME_COL, PPT_COL, TRIAL_COL), 
                                                       MSG_COL = as.name(msg_col), 
                                                       START_MSG = window_start_msg,
                                                       TIME_COL = time_col,
                                                       PPT_COL = ppt_col,
                                                       TRIAL_COL = trial_col)
                                                       
                    ))
    data <- ungroup(data)
    data$.WindowStart[which(is.infinite(data$.WindowStart))] <- NA # see note above
  } else if (which(start_method_num) == 2) {
    # Column:
    data$.WindowStart <- data[[window_start_col]]
  } else if (which(start_method_num) == 3) {
    # Single Number:
    data$.WindowStart <- window_start_time
  } 
  
  # Stop Time:
  if (!any(stop_method_num)) {
    stop_method_num <- c(FALSE, FALSE, TRUE)
    window_end_time <- Inf
  }
  if (which(stop_method_num) == 1) {
    # Message:
    data[[msg_col]] <- as.character(data[[msg_col]])
    if (!is.character(msg_col)) stop("Please enter a column name for the message column (in quotes).")
    data <- group_by_(.data = data,
                      .dots = list(data_options$participant_column, data_options$trial_column
                      ))
    data <- mutate_(data, 
                    .dots = list(.WindowEnd   = interp(~.safe_msg_checker(MSG_COL, STOP_MSG, TIME_COL, PPT_COL, TRIAL_COL), 
                                                       MSG_COL = as.name(msg_col), 
                                                       STOP_MSG = window_end_msg,
                                                       TIME_COL = time_col,
                                                       PPT_COL = ppt_col,
                                                       TRIAL_COL = trial_col)
                    ))
    data <- ungroup(data)
    data$.WindowEnd[which(is.infinite(data$.WindowEnd))] <- NA # see note above
  } else if (which(stop_method_num) == 2) {
    # Column:
    data$.WindowEnd <- data[[window_end_col]]
  } else if (which(stop_method_num) == 3) {
    # Single Number:
    data$.WindowEnd <- window_end_time
  } 
  
  #
  if (!quiet) {
    new_len <- round(mean(data$.WindowEnd - data$.WindowStart, na.rm=TRUE),2)
    if (!is.infinite(new_len)) message("Avg. window length in new data will be ", new_len) 
  }
  
  # Subset
  df_subsetted <- filter(.data = data,
                         !is.na(.WindowEnd),
                         !is.na(.WindowStart))
  if (remove) {
    df_subsetted <- filter_(.data = df_subsetted,
                          .dots = list(interp(~TIME_COL >= .WindowStart & TIME_COL < .WindowEnd, TIME_COL = time_col)))
  } 
  
  # Rezero
  if (rezero) {
    df_grouped <- group_by_(.data = df_subsetted,
                            .dots = list(data_options$participant_column, data_options$trial_column
                            ))
    df_rezeroed <- mutate_(.data = df_grouped,
                           .dots = list(.NewTimeStamp = interp(~TIME_COL - .WindowStart, TIME_COL = time_col))
                           )
    out <- ungroup(df_rezeroed)
    out[[data_options$time_column]] <- out[[".NewTimeStamp"]]
    out[[".NewTimeStamp"]] <- NULL
  } else {
    out <- df_subsetted
  }

  out[[".WindowStart"]] <- NULL
  out[[".WindowEnd"]] <- NULL
  
  attr(out, "eyetrackingR") <- list(data_options = data_options)
  class(out) <- orig_classes
  
  out
}

#' Analyze trackloss.
#'
#' Get information on trackloss in your data.
#'
#' @param data The output of \code{make_eyetrackingr_data}
#' 
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' 
#' tl_analysis <- trackloss_analysis(data)
#' 
#' @export
#' @return A dataframe describing trackloss by-trial and by-participant

trackloss_analysis <- function(data) {

  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  trackloss_col <- as.name(data_options$trackloss_column)

  # Get Trackloss-by-Trial:
  df_grouped_trial <- group_by_(data, .dots = list(data_options$participant_column, data_options$trial_column))
  df_trackloss_by_trial <- mutate_(df_grouped_trial,
                                   .dots = list(SumTracklossForTrial = interp(~sum(TRACKLOSS_COL, na.rm = TRUE), TRACKLOSS_COL = trackloss_col),
                                                TotalTrialLength = interp(~length(TRACKLOSS_COL), TRACKLOSS_COL = trackloss_col),
                                                TracklossForTrial = interp(~SumTracklossForTrial/TotalTrialLength)
                                   ))

  # Get Trackloss-by-Participant:
  df_grouped_ppt <- group_by_(df_trackloss_by_trial, .dots = list(data_options$participant_column))
  df_trackloss_by_ppt <- mutate_(df_grouped_ppt,
                                 .dots = list(SumTracklossForParticipant = interp(~sum(TRACKLOSS_COL, na.rm = TRUE), TRACKLOSS_COL = trackloss_col),
                                              TotalParticipantLength = interp(~length(TRACKLOSS_COL), TRACKLOSS_COL = trackloss_col),
                                              TracklossForParticipant = interp(~SumTracklossForParticipant/TotalParticipantLength)))

  # Get Z-Scores:
  df_grouped <- group_by_(df_trackloss_by_ppt, .dots = list(data_options$participant_column, data_options$trial_column))
  df_summarized <- summarize(df_grouped,
                             Samples = mean(TotalTrialLength, na.rm = TRUE),
                             TracklossSamples = mean(SumTracklossForTrial, na.rm = TRUE),
                             TracklossForTrial = mean(TracklossForTrial, na.rm = TRUE),
                             TracklossForParticipant = mean(TracklossForParticipant, na.rm = TRUE))
  df_summarized <- ungroup(df_summarized)

  return(df_summarized)
}


#' Clean data by removing high-trackloss trials/subjects.
#'
#' Remove trials/participants with too much trackloss, with a customizable threshold.
#' 
#' @param data Data already run through \code{make_eyetrackingr_data}
#' @param participant_prop_thresh            Maximum proportion of trackloss for participants
#' @param trial_prop_thresh                  Maximum proportion of trackloss for trials
#' @param window_start_time,window_end_time  Time-window within which you want trackloss analysis to
#'   be based. Allows you to keep the entire trial window for data, but clean based on the trackloss
#'   within a subset of it
#'   
#' @examples
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' 
#' # scrub all trials with greater than 25% trackloss, and all
#' # participants with greater than 25% trackloss on average
#' # during the timeperiod 15500-2100
#' data_clean <- clean_by_trackloss(data,
#'                                  participant_prop_thresh = .25, 
#'                                  trial_prop_thresh = .25,
#'                                  window_start_time = 15500, 
#'                                  window_end_time = 21000
#' )
#' 
#' # scrub all trials with greater than 25% trackloss, but leave participants with a high average
#' data_clean <- clean_by_trackloss(data,
#'                                  trial_prop_thresh = .25,
#'                                  window_start_time = 15500, 
#'                                  window_end_time = 21000
#' )
#'   
#' @export
#' @return Cleaned data
clean_by_trackloss <- function(data,
                              participant_prop_thresh = 1,
                              trial_prop_thresh = 1,
                              window_start_time = -Inf, window_end_time = Inf) {

  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  # Helpful Column:
  data$.TrialID <- paste(data[[data_options$participant_col]], data[[data_options$trial_col]], sep = "_")

  # Trackloss Analysis:
  message("Performing Trackloss Analysis...")
  data_tl <- subset_by_window(data = data, quiet=TRUE,
                              window_start_time = window_start_time, window_end_time = window_end_time )
  tl <- trackloss_analysis(data_tl)

  # Bad Trials:
  if (trial_prop_thresh < 1) {
    message("Will exclude trials whose trackloss proportion is greater than : ", trial_prop_thresh)
    exclude_trials_props <- paste(tl$Participant[tl$TracklossForTrial > trial_prop_thresh],
                                 tl$Trial[tl$TracklossForTrial > trial_prop_thresh],
                                 sep="_")
    message(paste("\t...removed ", length(exclude_trials_props), " trials."))
  } else {
    exclude_trials_props <- c()
  }

  # Bad Participants
  part_vec <- data[[data_options$participant_col]]
  if (participant_prop_thresh < 1) {
    message("Will exclude participants whose trackloss proportion is greater than : ", participant_prop_thresh)
    exclude_ppts_prop <- unique(tl$Participant[tl$TracklossForParticipant > participant_prop_thresh])
    message(paste("\t...removed ", length(exclude_ppts_prop), " participants."))
  } else {
    exclude_ppts_prop <- c()
  }

  exclude_trials <- c(exclude_trials_props, unique( data$.TrialID[part_vec %in% exclude_ppts_prop] ))

  # Remove:
  data_clean <- filter(data, ! .TrialID %in% exclude_trials)
  data_clean$.TrialID <- NULL

  return(data_clean)

}

#' Describe dataset
#' 
#' Returns descriptive statistics about a column of choice. A simple convenience function that wraps
#' \code{dplyr::group_by} and \code{dplyr::summarize}, allowing a quick glance at the data.
#' 
#' @param data Data already run through \code{make_eyetrackingr_data}
#' @param describe_column The column to return descriptive statistics about.
#' @param group_columns Any columns to group by when calculating descriptive statistics (e.g., participants,
#'  conditions, etc.)
#'  
#'
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE
#' )
#' describe_data(data, describe_column = "Animate", group_columns = "ParticipantName")
#'  
#' @export
#' @return A dataframe giving descriptive statistics for the \code{describe_column}, including mean, SD, var,
#' min, max, and number of trials
describe_data <- function(data, describe_column, group_columns) {

  # Data options:
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  # Build Summarize Expression
  summarize_expr <- list(Mean = interp( ~mean(DV_COL, na.rm=TRUE),  DV_COL = as.name(describe_column) ),
                       SD   = interp( ~sd(DV_COL, na.rm=TRUE),      DV_COL = as.name(describe_column) ),
                       Var  = interp( ~var(DV_COL, na.rm=TRUE),     DV_COL = as.name(describe_column) ),
                       Min  = interp( ~min(DV_COL, na.rm=TRUE)*1.0, DV_COL = as.name(describe_column) ),
                       Max  = interp( ~max(DV_COL, na.rm=TRUE)*1.0, DV_COL = as.name(describe_column) )
  )
  if (data_options$trial_column %in% colnames(data)) {
    summarize_expr$NumTrials = interp( ~n_distinct(TRIAL_COL), TRIAL_COL = as.name(data_options$trial_column))
  }

  # Group, Summarize
  df_grouped <- group_by_(data, .dots = as.list(group_columns))
  df_summarized <- summarize_(df_grouped, .dots =summarize_expr )
  class(df_summarized) <- c("eyetrackingR_data_summary", class(df_summarized))
  attr(df_summarized, "eyetrackingR") <- list(data_options = data_options, 
                                             describe_column = describe_column,
                                             group_columns = group_columns)
  return(df_summarized)

}

#' Plot some summarized data from eyetrackingR
#' 
#' Plots the data returned from \code{describe_data}. Like that function, this is a convenient 
#' wrapper good for sanity checks.
#' 
#' @param x The data returned by \code{make_time_window_data()}
#' @param ... Ignored
#' @export
#' @return A ggplot object
plot.eyetrackingR_data_summary <- function(x, ...) {
  attrs <- attr(x, "eyetrackingR")
  
  ggplot(x, aes_string(x=attrs$group_columns[1], y="Mean", group=attrs$group_columns[2])) +
    stat_summary(fun.y='mean', geom='point') +
    stat_summary(fun.y='mean', geom='line')
}
