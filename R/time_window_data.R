#' Make a dataset collapsing over a time-window
#'
#' Collapse time across our entire window and return a dataframe ready for analyses
#' 
#' Aside from proportion looking (\code{Prop}), this function returns several columns useful for subsequent
#' analysis:
#' 
#' \itemize{
#'  \item \code{LogitAdjusted} - The logit is defined as \code{log( Prop / (1 - Prop) )}. This
#'  transformation attempts to map bounded \code{0,1} data to the real number line. Unfortunately,
#'  for data that is exactly 0 or 1, this is undefined. One solution is add a very small value to
#'  any datapoints that equal 0, and subtract a small value to any datapoints that equal 1 (we use
#'  1/2 the smallest nonzero value for this adjustment).
#'  \item \code{Elog} - Another way of calculating a corrected logit transformation is to
#' add a small value \code{epsilon} to both the numerator and denominator of the logit equation (we
#' use 0.5).
#'  \item \code{Weights} - These attempt to further correct the Elog transformation, since the
#'  variance of the logit depends on the mean. They can be used in a mixed effects model by setting
#'  the \code{weights=Weights} in \code{lmer} (note that this is the reciprocal of the
#' weights calculated in \href{http://talklab.psy.gla.ac.uk/tvw/elogit-wt.html}{this empirical logit
#' walkthrough}, so you do *not* set \code{weights = 1/Weights} as done there.) 
#'  \item \code{ArcSin} - The arcsine-root transformation of the raw proportions, defined as
#' \code{asin(sqrt(Prop))}
#' }
#'
#' @param data  The output of \code{make_eyetrackingr_data}
#' @param aois   Which AOI(s) is/are of interest? Defaults to all specified in
#'   \code{make_eyetracking_r_data}
#' @param predictor_columns  Which columns indicate predictor vars, and therefore should be preserved in
#'   grouping operations?
#' @param other_dv_columns  Within each participant/trial (or whatever is specified in \code{summarize_by}), 
#'    this function will calculate not only proportion-looking, but also the mean of any columns specified here. 
#' @param summarize_by  Should the data be summarized along, e.g., participants, items, etc.? If so, give
#'   column names here. If left blank, will leave trials distinct. The former is needed for more traditional
#'   analyses (\code{t.test}, \code{ANOVA}), while the latter is preferable for mixed-effects models (\code{lmer})
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
#' # generate a dataset summarizing an AOI (Animate) by ParticipantName
#' response_window_agg_by_sub <- make_time_window_data(data,
#'                                                     aois='Animate',
#'                                                     summarize_by = "ParticipantName"
#' )
#' 
#' # optionally included additional columns for use as predictors
#' # in later statistical models
#' response_window_agg_by_sub <- make_time_window_data(data,
#'                                                     aois='Animate',
#'                                                     predictor_columns=c('Age','MCDI_Total'),
#'                                                     summarize_by = "ParticipantName"
#' )
#' 
#' # plot the aggregated data for sanity check
#' plot(response_window_agg_by_sub, predictor_columns="Age", dv = "LogitAdjusted") 
#'
#' @export
#' @return Data with proportion-looking and transformations (logit, arc-sin, etc.)

make_time_window_data <- function(data,
                         aois = NULL,
                         predictor_columns = NULL,
                         other_dv_columns = NULL,
                         summarize_by = NULL
) {

  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally? ",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  if (is.null(aois)) aois = data_options$aoi_columns

  # For Multiple AOIs:
  if (length(aois) > 1) {
    list_of_dfs <- lapply(X = aois, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      make_time_window_data(data = data, aois = this_aoi, 
        predictor_columns=predictor_columns, other_dv_columns = other_dv_columns, summarize_by=summarize_by)
    })
    out <- bind_rows(list_of_dfs)
    out <- as.data.frame(out)
    class(out) <- c('time_window_data', class(out))
    attr(out,"eyetrackingR") <- list(summarized_by = summarize_by,
                                     data_options = data_options)
    return( out )
  }

  # Prelims:
  data <- ungroup(data)
  aoi_col <- as.name(aois)

  # Make Summary
  if (is.null(summarize_by)) {
    groups <- c(data_options$participant_column,
               data_options$item_columns,
               data_options$trial_column,
               predictor_columns)
  } else {
    groups <- c(summarize_by, predictor_columns)
  }
  out <- .make_proportion_looking_summary(data=data, groups = groups, aoi_col = aoi_col, other_dv_columns = other_dv_columns)

  out <- as.data.frame(out)
  class(out) <- c('time_window_data', class(out))
  attr(out,"eyetrackingR") <- list(summarized_by = summarize_by,
                                   data_options = data_options)
  return(out)

}

#' Plot a time-window dataset
#' 
#' Plots the data returned from \code{make_time_window_data}. Data can be mapped onto (up to two)
#' predictor columns. If no predictor columns are supplied, AOI is placed on the x-axis; otherwise,
#' data for each AOI is set in a separate facet.
#' 
#' Data are collapsed by-participants for plotting.
#' 
#' @param x The data returned by make_time_window_data()
#' @param predictor_columns Up to two columns indicating predictors. The first maps to the X-axis,
#'   the second to group/color. If the latter is numeric, a median split is performed.
#' @param dv  Which dv should be used in plotting? Raw proportion-looking ("Prop"), empirical logit
#'   ("Elog"), or "ArcSin"?
#' @param ... Ignored
#' @export
#' 
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE)
#' response_window_agg_by_sub <- make_time_window_data(data, 
#'                                                     aois='Animate',
#'                                                     predictor_columns=c('Age','MCDI_Total'))
#'                                                     
#' plot(response_window_agg_by_sub, predictor_columns="Age", dv = "LogitAdjusted") 
#' 
#' @return A ggplot object

plot.time_window_data <- function(x, predictor_columns = NULL, dv = "Prop", ...) {

  # Prelims:
  data <- x
  data_options = attr(data, "eyetrackingR")$data_options
  if (!dv %in% colnames(data)) stop("Selected 'dv' is not in data.")
  
  # Organize Vars:
  if (is.null(predictor_columns)) {
    predictor_columns <- "AOI"
  }
  x_axis_column = predictor_columns[1]
  if (length(predictor_columns)>1) {
    group_column = predictor_columns[2]
  } else {
    group_column = NULL
  }
  if ( !is.null(group_column) ) {
    if (is.numeric(data[[group_column]])) {
      message("The variable '", group_column, "' is continous, will perform median split for visualization.")
      split_col = paste0(group_column, "_Split")
      the_median = median(data[[group_column]], na.rm=TRUE)
      data[[split_col]] = ifelse(data[[group_column]] > the_median,
                                 paste0("High (>", round(the_median,2), ")"),
                                 'Low')
      group_column = split_col
    }
  }
  color_var = group_column

  # Summarize by Participants:
  if (is.null(attr(data, "eyetrackingR")$summarized_by)) {
    df_grouped = group_by_(data, .dots = c(data_options$participant_column, x_axis_column, group_column, "AOI"))
    summarize_arg <- list(interp(~mean(DV, na.rm=TRUE), DV = as.name(dv)))
    names(summarize_arg) <- dv
    df_plot <- summarize_(df_grouped, .dots = summarize_arg )
  } else {
    df_plot <- data
  }
  
  if (x_axis_column!="AOI") df_plot$AOI <- paste("AOI: ", df_plot$AOI)

  # Plot:
  if ( is.numeric(df_plot[[x_axis_column]]) ) {
    group_var = ifelse( is.null(group_column), 1, group_column)
    g <- ggplot(df_plot, aes_string(x = x_axis_column, y = dv, group= group_var, color= color_var)) +
      geom_point() +
      stat_smooth(method="lm")
    if (length(unique(df_plot$AOI))>1 & !all(predictor_columns=="AOI")) {
      g <- g + facet_wrap( ~ AOI) + ylab(paste0("Looking to AOI (",dv,")"))
    } else {
      g <- g + ylab(paste0("Looking to ", df_plot$AOI[1], " (",dv,")"))
    }
  } else {
    group_var = group_column
    g <- ggplot(df_plot, aes_string(x = x_axis_column, y = dv, group= group_var, color= color_var)) +
      geom_point(position = position_jitter(width = .025, height=0), alpha= .75) +
      geom_boxplot(alpha = .05)
    if (dv %in% c("Prop", "Elog", "LogitAdjusted", "ArcSin")) {
      if (length(unique(df_plot$AOI))>1) {
        g <- g + facet_wrap( ~ AOI) + ylab(paste0("Looking to AOI (",dv,")"))
      } else {
        g <- g + ylab(paste0("Looking to ", df_plot$AOI[1], " (",dv,")"))
      }
    } else {
      if (length(unique(df_plot$AOI))>1) {
        g <- g + facet_wrap( ~ AOI)
        warning("Facets for a DV that doesn't correspond to amount-of-looking may not be meaningful.")
      }
      g <- g + ylab(dv)
    }
  }
  return(g)

}
