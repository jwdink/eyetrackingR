#' make_time_sequence_data()
#'
#' Creates time-bins and summarizes proportion-looking within each time-bin
#'
#' @param data
#' @param data_options
#' @param time_bin_size
#' @param aois              Which AOIs are of interest? Defaults to all in 'data_options'
#' @param predictor_columns Which columns indicate predictor vars, and therefore should be preserved in
#'   grouping operations?
#' @param summarize_by      Should the data be summarized along, e.g., participants, items, etc.? If so, give
#'   column name(s) here. If left blank, will leave trials distinct. The former is needed for more traditional
#'   analyses (t.tests, ANOVAs), while the latter is preferable for mixed-effects models (lmer)
#'
#' @export
#' @return Data binned into time-bins, with proportion-looking and transformations as well as orthogonal
#'   time-polynomials for growth curve analysis

make_time_sequence_data <- function (data,
                        data_options,
                        time_bin_size,
                        aois = NULL,
                        predictor_columns = NULL,
                        summarize_by = NULL) {

  if (is.null(aois)) aois = data_options$aoi_columns

  # For Multiple AOIs:
  if (length(aois) > 1) {
    list_of_dfs = lapply(X = aois, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      make_time_sequence_data(data, data_options, time_bin_size, this_aoi, predictor_columns, summarize_by)
    })
    out <- bind_rows(list_of_dfs)
    out <- as.data.frame(out)
    class(out) <- c('time_sequence_data', class(out))
    attr(out,"eyetrackingR") <- list(
      data_options = data_options,
      summarized_by = summarize_by,
      time_bin_size = time_bin_size)
    return( out )
  }

  # Prelims:
  data <- ungroup(data)
  aoi_col <- as.name(aois)

  # Make Time Bins:
  data[["TimeBin"]] <- floor(data[[data_options$time_column]] / time_bin_size)
  # check that the last time bin has an appropriate amount of data inside it
  # (e.g., it's not just a couple of samples at the end of the trial)
  df_grouped <- group_by_(data, .dots = c("TimeBin", data_options$trial_column))
  df_tb_sum <- summarize_(df_grouped, .dots = list(N = interp(~n_distinct(TIME), TIME = as.name(data_options$time_column))) )
  df_tb_sum <- summarize(df_tb_sum, N = mean(N))
  too_small_tb <- df_tb_sum$N[which.max(df_tb_sum$TimeBin)] < median(df_tb_sum$N, na.rm=TRUE) / 3
  if (too_small_tb) warning("With the current time-bin size, the final time-bin has a much smaller number of ",
                            "distinct samples than the other time-bins. ",
                            "Consider choosing a different time-bin size or using 'subset_by_window' ",
                            "to remove this portion of the trial.")

  # Make Summary
  if (is.null(summarize_by)) {
    groups <- c(data_options$participant_column,
               data_options$item_columns,
               data_options$trial_column,
               predictor_columns, "TimeBin")
  } else {
    groups <- c(summarize_by, predictor_columns, "TimeBin")
  }
  df_summarized <- .make_proportion_looking_summary(data=data, groups = groups, aoi_col)
  df_summarized[["Time"]] <- df_summarized[["TimeBin"]] * time_bin_size

  # Add orthogonal polynomials for Growth Curve Analyses
  time_bin_column <- df_summarized$TimeBin
  max_degree <- min( length(unique(time_bin_column))-1 , 7 )
  if (max_degree < 7) warning("Fewer time bins than polynomial degrees-- consider decreasing size of time bin.")
  orthogonal_polynomials <- poly(sort(as.vector(unique(time_bin_column))), max_degree)
  time_codes <- data.frame(
    sort(as.vector(unique(time_bin_column))),
    orthogonal_polynomials[, c(1:max_degree)]
  )
  colnames(time_codes) <- c('TimeBin',paste0("ot", 1:max_degree))

  out <- left_join(df_summarized, time_codes, by='TimeBin')

  out <- as.data.frame(out)
  class(out) <- c('time_sequence_data', class(out))
  attr(out,"eyetrackingR") <- list(
    data_options = data_options,
    summarized_by = summarize_by,
    time_bin_size = time_bin_size)
  return(out)

}

#' analyze_time_bins()
#'
#' Runs a test on each time-bin of \code{time_sequence_data}. Supports \code{t.test}, \code{wilcox.test}, \code{lm}, and
#' \code{lmer}. Results can be plotted to see how test-results or parameters estimates vary over time.
#' @export
analyze_time_bins = function(data, ...) {
  UseMethod("analyze_time_bins")
}
#' @describeIn analyze_time_bins
#'
#' @param data   The output of the 'make_time_sequence_data' function
#' @param data_options
#' @param predictor_column  The variable whose test statistic you are interested in. If you are not 
#'   interested in a predictor, but the intercept, you can enter "intercept" for this argument.
#'   Interaction terms are not currently supported.
#' @param test              What type of test should be performed in each time bin? Supports 
#'   \code{t.test}, \code{wilcox.test}, \code{lm}, and \code{lmer}.
#' @param threshold         Value of statistic used in determining significance
#' @param alpha             Alpha value for determining significance, ignored if threshold is given
#' @param formula           What formula should be used for the test? Optional for all but
#'   \code{lmer}, if unset will use \code{Prop ~ [predictor_column]}
#' @param return_model      In the returned dataframe, should a model be given for each time bin, or
#'   just the summary of those models?
#' @param ...               Any other arguments to be passed to the selected 'test' function (e.g.,
#'   paired, var.equal, etc.)
#' @export
#' @return A dataframe indicating the results of the test at each time-bin.
analyze_time_bins.time_sequence_data <- function(data,
                              predictor_column,
                              test,
                              threshold = NULL,
                              alpha = .05,
                              formula = NULL,
                              return_model = FALSE,
                              quiet = FALSE,
                              ...)
{

  ## Helper:
  .fix_unpaired = function(data, data_options, predictor_column) {
    lvl1 <- levels(data[[predictor_column]])[1]
    df_grouped <- group_by_(data, .dots = c(data_options$participant_column))
    df_mutated <- mutate_(df_grouped,
                          .dots = list(PairedObs = interp(~length(which(COND_COL == lvl1)) > 0 & length(which(COND_COL != lvl1)) > 0,
                                                          COND_COL = as.name(predictor_column)))
    )
    df_filtered <- filter(df_mutated, PairedObs)
    ungroup(df_filtered)
  }

  # Prelims:
  data_options <- attr(data, "eyetrackingR")$data_options
  if (grepl("intercept", predictor_column, ignore.case = TRUE)) {
    if (is.null(formula)) stop("If testing intercept, please manually specify formula")
    predictor_column <- "(Intercept)"
  }
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    pblapply <- lapply
    message("Install package 'pbapply' for a progress bar in this function.")
  } else {
    pblapply <- pbapply::pblapply
  }
  test <- match.arg(test, c("t.test","wilcox.test","lm","lmer"))
  if (!requireNamespace("lme4", quietly = TRUE)) {
    if (test == "lmer") stop("Please install and load the 'lme4' package to use this method.")
  }

  # For Multiple aois:
  if (!'AOI' %in% colnames(data)) stop("'AOI' column is missing from data.")
  aois <- unique(data[['AOI']])
  if ( length(aois) > 1 ) {
    list_of_dfs <- lapply(X = aois, FUN = function(this_aoi) {
      if (!quiet) message("Analyzing ", this_aoi, "...")
      this_df <- filter(data, AOI == this_aoi)
      class(this_df) = class(data)
      analyze_time_bins(data = this_df, predictor_column, test, threshold, alpha, formula, return_model, quiet, ...)
    })
    out <- bind_rows(list_of_dfs)
    out <- as.data.frame(out)
    class(out) = c('bin_analysis', class(out))
    attr(out,"eyetrackingR") <- list(formula= formula)
    return( out )
  }

  # Check that data is collapsed (by e.g. participants):
  if (test != "lmer") {
    attrs <- attr(data, "eyetrackingR")
    summarized_by <- attrs$summarized_by
    if (is.null(summarized_by)) stop(test, " requires summarized data. ",
                                     "When using the 'make_time_sequence_data' function, please select an argument for 'summarize_by'",
                                     " (e.g., the participant column).")
  }

  # auto-make a formula, unless they specified one
  if (is.null(formula)) {
    if (test=="lmer") stop("Must specify a formula if using lmer.")
    formula <- as.formula(paste("Prop ~", predictor_column))
  }

  # Run a model for each time-bin
  cc <- match.call(expand.dots = TRUE)
  paired <- cc[["paired"]]
  if (!quiet) message("Computing ", test, " for each time bin...")
  if (test=="lmer") {
    failsafe_test <- failwith(default = NA, f = lme4::lmer, quiet = quiet)
  } else {
    failsafe_test <- failwith(default = NA, f = get(test), quiet = quiet)
  }
  if (quiet) pblapply <- lapply
  models= pblapply(unique(data$Time), function(tb) {
    # get data:
    temp_dat <- filter(data, Time==tb)
    # Make paired test more robust to unpaired observations within a bin:
    if (identical(paired, TRUE)) temp_dat <- .fix_unpaired(temp_dat, data_options, predictor_column)
    # make model:
    model <- failsafe_test(formula = formula, data = temp_dat, ... = ...)
    # get N:
    if (test=="wilcox.test" | test=="lm") {
      if (predictor_column == "(Intercept)") {
        predictor_col_is_na <- FALSE
      } else {
        predictor_col_is_na <- is.na(temp_dat[[predictor_column]])
      }
      model$sample_size <- length(unique( temp_dat[[data_options$participant_column]][!predictor_col_is_na] ))
    }
    model
  })

  # Get Statistic:
  if (test=="lmer") {
    tidied_models <- lapply(models, broom::tidy, effects="fixed")
  } else {
    tidied_models <- lapply(models, broom::tidy)
  }
  if (test %in% c('t.test','wilcox.test')) {
    models_statistics <- sapply(tidied_models, function(x) ifelse('statistic' %in% names(x), x[,'statistic'], NA) )
    models_estimates  <- sapply(tidied_models, function(x) ifelse('estimate' %in% names(x), x[,'estimate'], NA) )
    
    # no std. error provided, so grab it from CI
    models_std_err  <- sapply(tidied_models, function(x) ifelse(sum(c('conf.low','conf.high') %in% names(x)) == 2, (x[,'conf.high']-x[,'conf.low'])/(1.96*2), NA) )
  } else {
    model_row <- lapply(tidied_models, function(x) {
      which_row <- grep(pattern = predictor_column, x = x[['term']], fixed = TRUE) # look for partially matching param (for treatment coding)
      if (length(which_row)==1) {
        return(x[which_row, ])
      } else {
        # too many matches? look for exact match (happens with continous predictor)
        which_row <- which(x[['term']] == predictor_column)
        if (length(which_row)==1) return(x[which_row, 'statistic'])
        warning("Could not find the parameter '",predictor_column,"' in your model. Found instead: ", paste(x[['term']], collapse=", ") )
        return(NA)
      }
    } )
    
    models_statistics <- sapply(model_row, function(x) x[,"statistic"])
    models_estimates  <- sapply(model_row, function(x) x[,"estimate"])
    models_std_err    <- sapply(model_row, function(x) x[,"std.error"])
  }


  # Find Critical Value:
  if (is.null(threshold)) {
    if (test == "lmer") {
      if (!quiet) message("Using the normal approximation for critical value on parameter in lmer.")
      crit_pos =  qnorm(p=1-alpha/2)
      crit_neg = -qnorm(p=1-alpha/2)
    } else if (test=="t.test") {
      dfs <- sapply(tidied_models, function(x) ifelse('parameter' %in% names(x), x[,'parameter'], NA))
      crit_pos <- qt(1-alpha/2, df = dfs)
      crit_neg <- -crit_pos
    } else if (test=="wilcox.test") {
      sample_sizes <- sapply(models, function(x) x$sample_size)
      crit_pos <- qsignrank(p = 1-alpha/2, n = sample_sizes )
      crit_neg <- -crit_pos
    } else if (test=="lm") {
      sample_sizes <- sapply(models, function(x) x$sample_size)
      crit_pos <- qt(1-alpha/2, df = sample_sizes-1)
      crit_neg <- -crit_pos
    }
  } else {
    crit_pos <- ifelse(sign(threshold)==1,  threshold, -threshold)
    crit_neg <- ifelse(sign(threshold)==1, -threshold,  threshold)
  }

  # Return DataFrame:
  out <- data.frame(stringsAsFactors = FALSE,
                    Estimate = models_estimates,
                    StdErr = models_std_err,
                    Statistic = models_statistics,
                    CritStatisticPos = crit_pos,
                    CritStatisticNeg = crit_neg,
                    Time = unique(data$Time)) # same order as for loop that built models
  out$AOI <- data$AOI[1]
  if (return_model) out$Model <- models
  
  # Compute Information about Runs:
  out$PositiveRuns <- .label_consecutive(out$Statistic>out$CritStatisticPos)
  out$NegativeRuns <- .label_consecutive(out$Statistic<out$CritStatisticNeg)
  
  positive_runs = lapply(unique(na.omit(out$PositiveRuns)), function(run) {
    list(start_time = with(out, min(Time[which(PositiveRuns==run)], na.rm=TRUE)),
         stop_time  = with(out, max(Time[which(PositiveRuns==run)], na.rm=TRUE)) + attr(data, "eyetrackingR")$time_bin_size
    )
  })
  negative_runs = lapply(unique(na.omit(out$NegativeRuns)), function(run) {
    list(start_time = with(out, min(Time[which(NegativeRuns==run)], na.rm=TRUE)),
         stop_time  = with(out, max(Time[which(NegativeRuns==run)], na.rm=TRUE)) + attr(data, "eyetrackingR")$time_bin_size
    )
  })

  # Rename Class, add attributes
  out <- as.data.frame(out)
  class(out) <- c('bin_analysis', class(out))
  attr(out,"eyetrackingR") <- c(
    attr(data, "eyetrackingR"),
    list(formula= formula,
         test = test,
         predictor = predictor_column,
         positive_runs = positive_runs,
         negative_runs = negative_runs)
  )
  out = out[order(out$Time),] # arrange chronologically if not already
  out
}

#' Summary Method for Time-bin Analysis
#' @param  data The output of the \code{analyze_time_bins} function
#' @export
#' @return Prints information about each run of statistically significant time-bins, separately for
#'   positive and negative
summary.bin_analysis <- function(df_timebins) {
  
  attrs <- attr(df_timebins, "eyetrackingR")
  positive_runs <- seq_along(attrs$positive_runs)
  negative_runs <- seq_along(attrs$negative_runs)
  df_pos <- bind_rows(lapply(attrs$positive_runs, as.data.frame))
  df_neg <- bind_rows(lapply(attrs$negative_runs, as.data.frame))
  
  p1 <- paste("Test Type:\t", attrs$test,
              "\nPredictor:\t", attrs$predictor_column,
              "\nFormula:\t", Reduce(paste, deparse(attrs$formula)),
              "\nRuns of Significant Time Bins:")
  if (length(positive_runs) > 0) {
    p2 <- paste(
      "\nPositive Run", positive_runs, " =====",
      "\n\tTime:\t\t", df_pos$start_time, "-", df_pos$stop_time
    )
  } else {
    p2 <- ""
  }
  if (length(negative_runs) > 0) {
    p3 <- paste(
      "\nNegative Run", negative_runs, " =====",
      "\n\tTime:\t\t", df_neg$start_time, "-", df_neg$stop_time
    )
  } else {
    p3 <- ""
  }
  cat(p1,p2,p3)
  invisible(df_timebins)
}

#' Plot time-sequence data
#' 
#' Plot the timecourse of looking. Each AOI will be plotted in a separate pane, and data can be 
#' split into groups by a predictor column. Data is collapsed by subject for plotting. Supports
#' overlaying the predictions of a growth-curve mixed effects model on the data
#' 
#' @param data              Your data from \code{make_time_sequence_data}. Will be collapsed by
#'   subject for plotting.
#' @param predictor_column  Data can be grouped by a predictor column (median split is performed if
#'   numeric)
#' @param dv                What measure of gaze do you want to use? (\code{Prop}, \code{Elog}, or
#'   \code{ArcSin})
#' @param model             (Optional) A growth-curve mixed effects model (from \code{lmer}) that
#'   was used on the \code{time_sequence_data}. If model is given, this function will overlay the
#'   predictions of that model on the data
#' @export
#' @return A ggplot object
plot.time_sequence_data <- function(data, predictor_column = NULL, dv='Prop', model = NULL) {

  # Prelims:
  data_options = attr(data, "eyetrackingR")$data_options
  dv = match.arg(dv, c("Prop", "Elog", "ArcSin"))
  
  # Add model predictions to dataframe:
  if (!is.null(model)) {
    formula_as_character <- Reduce(paste, deparse(formula(model)))
    if (!grepl(dv, formula_as_character, fixed = TRUE)) {
      stop("Your model appears to use a different DV than the one you are attempting to plot. Change the 'dv' arg in plot.")
    }
    data$.Predicted = predict(model, data, re.form = NA)
  } 

  ## Collapse by-subject for plotting
  df_plot <- group_by_(data, .dots = c(data_options$participant_column, "Time", "AOI", predictor_column))
  summarize_arg <- list(interp(~mean(DV, na.rm=TRUE), DV = as.name(dv)))
  names(summarize_arg) <- dv
  if (!is.null(model)) summarize_arg[[".Predicted"]] <- ~mean(.Predicted, na.rm=TRUE)
  df_plot <- summarize_(df_plot, .dots = summarize_arg)
  df_plot$AOI <- paste("AOI: ", df_plot$AOI) # for facetting

  ## Check condition predictor
  if ( length(predictor_column) > 1 ) {
    stop('Can only plot time-analysis for one predictor at a time.')
  }
  numeric_predictor_col = FALSE
  if (!is.null(predictor_column)) {
    if (is.numeric(df_plot[[predictor_column]])) numeric_predictor_col <- TRUE
  }

  ## Plot:
  if (numeric_predictor_col) {
    message("Condition factor is numeric, performing median split...")
    the_median <- median(df_plot[[predictor_column]], na.rm=TRUE)
    df_plot[["GroupFactor"]] <- ifelse(df_plot[[predictor_column]] > the_median,
                                      paste0("High (>", round(the_median,2), ")"),
                                      "Low")
    
    g <- ggplot(df_plot, aes_string(x = "Time", y=dv, group="GroupFactor", color="GroupFactor")) +
      guides(color= guide_legend(title= predictor_column)) +
      xlab('Time in Trial')
  } else {
    g <- ggplot(df_plot, aes_string(x = "Time", y=dv, group=predictor_column, color=predictor_column, fill=predictor_column)) +
      xlab('Time in Trial')
  }
  
  g <- g + 
    stat_summary(fun.y='mean', geom='line', linetype = 'F1') + 
    stat_summary(fun.dat=mean_se, geom='ribbon', alpha= .25, colour=NA)
  
  if (!is.null(model)) {
    g <- g +
      stat_summary(aes(y = .Predicted), fun.y = 'mean', geom="line", size= 1.2) 
  }
  if (length(unique(df_plot$AOI))>1) {
    g <- g + facet_wrap( ~ AOI) + ylab(paste0("Looking to AOI (",dv,")"))
  } else {
    g <- g + ylab(paste0("Looking to ", df_plot$AOI[1], " (",dv,")"))
  }
  
  return(g)
  
}

#' Plot test-statistic for each time-bin in a time-series
#' 
#' Plot the result from the \code{analyze_time_bins} function, with the statistic and threshold for each bin
#' 
#' @param data
#' @param type Plot the test-statistic at each time bin ("statistic"), or the parameter estimate at each time
#'   bin ("estimate")?
#'   
#' @export
#' @return A ggplot object
plot.bin_analysis <- function(data, type = "statistic") {
  
  type = match.arg(type, c("statistic", "estimate"))
  
  if (type == "statistic") {
    g <- ggplot(data = data) +
      geom_line(mapping = aes(x = Time, y= Statistic)) +
      geom_line(mapping = aes(x = Time, y= CritStatisticPos), linetype="dashed") +
      geom_line(mapping = aes(x = Time, y= CritStatisticNeg), linetype="dashed") +
      ylab("Statistic") +
      xlab("Time") 
    if (length(unique(data$AOI))>1) g <- g + facet_wrap( ~ AOI)
  } else {
    g <- ggplot(data = data, mapping = aes(x = Time, y= Estimate)) +
      geom_line() +
      geom_ribbon(mapping = aes(ymin = Estimate - StdErr, ymax = Estimate + StdErr), alpha=.33) + 
      geom_hline(yintercept = 0, linetype="dashed") +
      ylab("Parameter Estimate") +
      xlab("Time") 
    if (length(unique(data$AOI))>1) g <- g + facet_wrap( ~ AOI)
  }
  return(g)
}
