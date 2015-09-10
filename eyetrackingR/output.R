# Loading/Cleaning/Describing Data
# ------------------------------------------------------------------------

#' Set data options.
#' 
#' Create a list which describes the important aspects of your data, to be used by most other functions in
#' this package.
#' 
#' @param participant_column Column name for participant identifier
#' @param trackloss_column   Column name indicating trackloss
#' @param time_column        Column name indicating time
#' @param trial_column       Column name indicating trial identifier
#' @param item_columns       Column names indicating items (can be same as trial_column)
#' @param aoi_columns        Names of AOIs
#' @return list of configuration options

set_data_options <- function(participant_column, trackloss_column, time_column, trial_column, 
    item_columns = NULL, aoi_columns) {
    list(participant_column = participant_column, trackloss_column = trackloss_column, 
        time_column = time_column, trial_column = trial_column, item_columns = item_columns, 
        aoi_columns = aoi_columns)
}


#' Verify the dataset
#' 
#' Use data_options to verify that the columns in your dataset are of the correct type
#' 
#' @param data Your data
#' @param data_options Created by \code{set_data_options}
#' @return Dataset with verified column types.

verify_dataset <- function(data, data_options) {
    out <- data
    
    if (!all(data_options$aoi_columns %in% colnames(data))) {
        stop("Not all of the AOI columns specified in data_options are in the data.")
    }
    
    as.numeric2 <- function(x) as.numeric(as.character(x))
    check_then_convert <- function(x, checkfunc, convertfunc, colname) {
        if (!checkfunc(x)) {
            message("Converting ", colname, " to proper type.")
            convertfunc(x)
        } else {
            x
        }
    }
    col_type_converter <- list(participant_column = function(x) check_then_convert(x, 
        is.factor, as.factor, "Participants"), time_column = function(x) check_then_convert(x, 
        is.numeric, as.numeric2, "Time"), trial_column = function(x) check_then_convert(x, 
        is.factor, as.factor, "Trial"), trackloss_column = function(x) check_then_convert(x, 
        is.logical, as.logical, "Trackloss"), item_columns = function(x) check_then_convert(x, 
        is.factor, as.factor, "Item"), aoi_columns = function(x) check_then_convert(x, 
        is.logical, as.logical, "AOI"))
    
    for (col in names(col_type_converter)) {
        for (i in seq_along(data_options[[col]])) {
            if (is.null(out[[data_options[[col]][i]]])) 
                stop("Data are missing: ", col)
            out[[data_options[[col]][i]]] <- col_type_converter[[col]](out[[data_options[[col]][i]]])
        }
    }
    
    return(out)
}

#' subset_by_window ()
#' 
#' Extract a subset of the dataset, where each trial falls inside a time-window.
#' Time-window can either be specifed by a number (for a timestamp across all trials) or
#' by a column which picks out a timestamp for each participant/trial
#' 
#' @param data
#' @param data_options
#' @param window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param window_end Number (for timestamp) or character (for column that specifies timestamp)
#' @param rezero Should the beginning of the window be considered the zero point of the timestamp? 
#'  Default TRUE when window_start is column, FALSE when window_start is number
#' @return Subsetted data

subset_by_window <- function(data, data_options, window_start = -Inf, window_end = Inf, 
    rezero = NULL) {
    require("dplyr", quietly = TRUE)
    require("lazyeval", quietly = TRUE)
    
    # Prelims:
    time_col <- as.name(data_options$time_column)
    
    # Window Start:
    if (is.character(window_start)) {
        data$.WindowStart <- data[[window_start]]
        if (is.null(rezero)) 
            rezero <- TRUE
    } else {
        data$.WindowStart <- window_start
        if (is.null(rezero)) 
            rezero <- FALSE
    }
    
    # Window End:
    if (is.character(window_end)) {
        data$.WindowEnd <- data[[window_end]]
    } else {
        data$.WindowEnd <- window_end
    }
    
    # Subset
    df_subsetted <- filter_(.data = data, .dots = list(interp(~TIME_COL >= .WindowStart & 
        TIME_COL <= .WindowEnd, TIME_COL = time_col)))
    
    # Rezero
    if (rezero) {
        df_grouped <- group_by_(.data = df_subsetted, .dots = list(data_options$participant_column, 
            data_options$trial_column))
        df_rezeroed <- mutate_(.data = df_grouped, .dots = list(.NewTimeStamp = interp(~TIME_COL - 
            .WindowStart, TIME_COL = time_col)))
        out <- ungroup(df_rezeroed)
        out[[data_options$time_column]] <- out[[".NewTimeStamp"]]
        out[[".NewTimeStamp"]] <- NULL
    } else {
        out <- df_subsetted
    }
    
    out[[".WindowStart"]] <- NULL
    out[[".WindowEnd"]] <- NULL
    
    out
}

#' trackloss_analysis ()
#' 
#' Get information on trackloss in your data. This can be for the entire dataset, or for a specific time
#' window within a trial
#' 
#' @param data
#' @param data_options
#' @param window_start Number (for timestamp) or character (for column that specifies timestamp)
#' @param window_end Number (for timestamp) or character (for column that specifies timestamp)
#' @return A dataframe describing trackloss by-trial and by-participant 

trackloss_analysis <- function(data, data_options, window_start = -Inf, window_end = Inf) {
    require("dplyr", quietly = TRUE)
    require("lazyeval", quietly = TRUE)
    
    trackloss_col <- as.name(data_options$trackloss_column)
    
    # Filter by Time-Window:
    df_subsetted <- subset_by_window(data, data_options, window_start, window_end)
    
    # Get Trackloss-by-Trial:
    df_grouped_trial <- group_by_(df_subsetted, .dots = list(data_options$participant_column, 
        data_options$trial_column))
    df_trackloss_by_trial <- mutate_(df_grouped_trial, .dots = list(SumTracklossForTrial = interp(~sum(TRACKLOSS_COL, 
        na.rm = TRUE), TRACKLOSS_COL = trackloss_col), TotalTrialLength = interp(~length(TRACKLOSS_COL), 
        TRACKLOSS_COL = trackloss_col), TracklossForTrial = interp(~SumTracklossForTrial/TotalTrialLength)))
    
    # Get Trackloss-by-Participant:
    df_grouped_ppt <- group_by_(df_trackloss_by_trial, .dots = list(data_options$participant_column))
    df_trackloss_by_ppt <- mutate_(df_grouped_ppt, .dots = list(SumTracklossForParticipant = interp(~sum(TRACKLOSS_COL, 
        na.rm = TRUE), TRACKLOSS_COL = trackloss_col), TotalParticipantLength = interp(~length(TRACKLOSS_COL), 
        TRACKLOSS_COL = trackloss_col), TracklossForParticipant = interp(~SumTracklossForParticipant/TotalParticipantLength)))
    
    # Get Z-Scores:
    df_grouped <- group_by_(df_trackloss_by_ppt, .dots = list(data_options$participant_column, 
        data_options$trial_column))
    df_summarized <- summarise(df_grouped, Samples = mean(TotalTrialLength, na.rm = TRUE), 
        TracklossSamples = mean(SumTracklossForTrial, na.rm = TRUE), TracklossForTrial = mean(TracklossForTrial, 
            na.rm = TRUE), TracklossForParticipant = mean(TracklossForParticipant, 
            na.rm = TRUE))
    df_summarized <- ungroup(df_summarized)
    
    return(df_summarized)
} 
