#' Make a dataset collapsing over a time-window
#'
#' Collapse time across our entire window and return a dataframe ready for analyses
#'
#' @param data
#' @param data_options
#' @param aois  Which AOIs are of interest? Defaults to all in 'data_options'
#' @param predictor_columns  Which columns indicate predictor vars, and therefore should be preserved in
#'   grouping operations?
#' @param summarize_by  Should the data be summarized along, e.g., participants, items, etc.? If so, give
#'   column names here. If left blank, will leave trials distinct. The former is needed for more traditional
#'   analyses (t.tests, ANOVAs), while the latter is preferable for mixed-effects models (lmer)
#' @export
#' @return Data with proportion-looking and transformations (empirical logit, arc-sin)

make_time_window_data <- function(data,
                         data_options,
                         aois = NULL,
                         predictor_columns = NULL,
                         summarize_by = NULL
) {

  if (is.null(aois)) aois = data_options$aoi_columns

  # For Multiple AOIs:
  if (length(aois) > 1) {
    list_of_dfs <- lapply(X = aois, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      make_time_window_data(data, data_options, aois = this_aoi, predictor_columns, summarize_by)
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
  out <- .make_proportion_looking_summary(data=data, groups = groups, aoi_col)

  out <- as.data.frame(out)
  class(out) <- c('time_window_data', class(out))
  attr(out,"eyetrackingR") <- list(summarized_by = summarize_by,
                                   data_options = data_options)
  return(out)

}

#' Plot a time-window dataset
#'
#' Plots the data returned from make_time_window_data. Data are collapsed by-participants for plotting.
#'
#' @param data The data returned by make_time_window_data()
#' @param predictor_columns Up to two columns indicating predictors. The first maps to the X-axis, the second
#'  to group/color. If the latter is numeric, a median split is performed.
#' @param dv  Which dv should be used in plotting? Raw proportion-looking ("Prop"), empirical logit ("Elog"),
#'  or "ArcSin"?
#' @export
#' @return A ggplot object

plot.time_window_data <- function(data, predictor_columns, dv = "Prop") {

  # Prelims:
  data_options = attr(data, "eyetrackingR")$data_options
  dv = match.arg(dv, c("Elog","Prop","ArcSin"))

  # Organize Vars:
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
  group_var = ifelse( is.null(group_column), 1, group_column)

  # Summarize by Participants:
  df_grouped = group_by_(data, .dots = c(data_options$participant_column, x_axis_column, group_column, "AOI"))
  summarize_arg <- list(interp(~mean(DV, na.rm=TRUE), DV = as.name(dv)))
  names(summarize_arg) <- dv
  df_summarized = summarize_(df_grouped, .dots = summarize_arg )

  # Plot:
  if ( is.numeric(df_summarized[[x_axis_column]]) ) {
    ggplot(df_summarized, aes_string(x = x_axis_column, y = dv, group= group_var, color= color_var)) +
      geom_point() +
      stat_smooth(method="lm") +
      facet_wrap( ~ AOI)
  } else {
    ggplot(df_summarized, aes_string(x = x_axis_column, y = dv, group= group_var, color= color_var)) +
      geom_point(position = position_jitter(width = .025, height=0), alpha= .75) +
      stat_summary(fun.y = mean, geom='line') +
      stat_summary(fun.dat = mean_cl_boot) +
      facet_wrap( ~ AOI)
  }

}
