#' make_time_sequence_data()
#'
#' Creates time-bins and summarizes proportion-looking within each time-bin. 
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
#'  \item \code{ot} - These columns (ot1-ot7) represent (centered) orthogonal time polynomials,
#'  needed for growth curve analysis. See
#'  \href{http://www.eyetracking-r.com/vignettes/growth_curve_analysis}{the vignette on growth curve
#'  models} for more details.
#' }
#' 
#' @param data              The output of \code{make_eyetrackingr_data}
#' @param time_bin_size     How large should each time bin be? Units are whatever units your \code{time} column is in
#' @param aois              Which AOI(s) is/are of interest? Defaults to all specified in
#'   \code{make_eyetracking_r_data}
#' @param predictor_columns Which columns indicate predictor variables, and therefore should be
#'   preserved in grouping operations?
#' @param other_dv_columns  Within each time-bin, this function will calculate not only proportion-
#'    looking, but also the mean of any columns specified here. 
#' @param summarize_by      Should the data be summarized along, e.g., participants, items, etc.? If
#'   so, give column name(s) here. If left blank, will leave trials distinct. The former is needed
#'   for more traditional analyses (\code{t.test}, \code{ANOVA}), while the latter is preferable for
#'   mixed-effects models (\code{lmer})
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
#' # bin data in 250ms bins, and generate a dataframe
#' # with a single AOI (Animate) predicted by Sex, and summarized by ParticipantName
#' response_time <- make_time_sequence_data(data,
#'                                          time_bin_size = 250,
#'                                          predictor_columns = c("Sex"),
#'                                          aois = "Animate",
#'                                          summarize_by = "ParticipantName"
#' )
#' 
#' # optionally specify other columns in the data
#' # to be included in the generated dataframe
#' # (e.g., for use in statistical models)
#' # bin data in 250ms bins, and generate a dataframe
#' # with Animate and MCDI_Total summarized by ParticipantName
#' response_time <- make_time_sequence_data(data,
#'                                          time_bin_size = 250,
#'                                          predictor_columns = c("Sex","MCDI_Total"),
#'                                          aois = "Animate", 
#'                                          summarize_by = "ParticipantName"
#' )
#'
#' @export
#' @return Data binned into time-bins, with proportion-looking and transformations as well as orthogonal
#'   time-polynomials for growth curve analysis

make_time_sequence_data <- function (data,
                        time_bin_size,
                        aois = NULL,
                        predictor_columns = NULL,
                        other_dv_columns = NULL,
                        summarize_by = NULL) {

  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) {
    stop("It appears your dataframe doesn't have information that eyetrackingR needs. ",
         "Did you run `make_eyetracking_r` data on it originally?",
         "If so, this information has been removed. This can happen when using functions that ",
         "transform your data significantly, like dplyr::summarise or dplyr::select.")
  }
  
  if (is.null(aois)) aois = data_options$aoi_columns

  # For Multiple AOIs:
  if (length(aois) > 1) {
    list_of_dfs = lapply(X = aois, FUN = function(this_aoi) {
      message("Analyzing ", this_aoi, "...")
      make_time_sequence_data(data=data, time_bin_size = time_bin_size, aois = this_aoi, 
                              predictor_columns = predictor_columns, other_dv_columns= other_dv_columns,
                              summarize_by = summarize_by)
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
  df_summarized <- .make_proportion_looking_summary(data=data, groups = groups, aoi_col = aoi_col, other_dv_columns = other_dv_columns)
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
#' Runs a test on each time-bin of \code{time_sequence_data}. Supports \code{t.test},
#' \code{wilcox.test}, \code{(g)lm}, and \code{(g)lmer}. Also includes support for
#' the "bootstrapped-splines" test (see \code{?make_boot_splines_data} and 
#' \href{http://www.eyetracking-r.com/vignettes/divergence}{the divergence vignette} for more info). 
#' By default, this function uses 'proportion-looking' (\code{Prop}) as the DV, which can be changed
#' by manually specifying the formula. Results can be plotted to see how test-results or parameters
#' estimates vary over time. P-values can be adjusted for multiple comparisons with \code{p_adjust_method}.
#' 
#' @export
analyze_time_bins = function(data, ...) {
  UseMethod("analyze_time_bins")
}
#' @describeIn analyze_time_bins
#'
#' @param data   The output of the 'make_time_sequence_data' function
#' @param predictor_column  The variable whose test statistic you are interested in. If you are not 
#'   interested in a predictor, but the intercept, you can enter "intercept" for this argument.
#'   Interaction terms are not currently supported.
#' @param test              What type of test should be performed in each time bin? Supports 
#'   \code{t.test}, \code{wilcox.test}, \code{(g)lm}, and \code{(g)lmer}. Also includes support for
#' the "bootstrapped-splines" test (see \code{?make_boot_splines_data} and 
#' \href{http://www.eyetracking-r.com/vignettes/divergence}{the divergence vignette} for more info).
#' @param threshold         Value of statistic used in determining significance
#' @param alpha             Alpha value for determining significance, ignored if threshold is given
#' @param aoi               Which AOI should be analyzed? If not specified (and dataframe has multiple AOIs), 
#'                          then AOI should be a predictor/covariate in your model (so `formula` needs 
#'                          to be specified).
#' @param formula           What formula should be used for the test? Optional for all but
#'   \code{(g)lmer}, if unset will use \code{Prop ~ [predictor_column]}. Change this if you want to use a custom DV.
#' @param p_adjust_method   Method to adjust p.values for multiple corrections (default="none"). 
#'                          See \code{p.adjust.methods}.
#' @param quiet             Should messages and progress bars be suppressed? Default is to show
#' @param ...               Any other arguments to be passed to the selected 'test' function (e.g.,
#'   paired, var.equal, etc.)
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
#' response_time <- make_time_sequence_data(data, time_bin_size = 250, 
#'                                          predictor_columns = c("MCDI_Total"),
#'                                          aois = "Animate", summarize_by = "ParticipantName")
#' tb_analysis <- analyze_time_bins(response_time, predictor_column = "MCDI_Total", 
#'                                  test = "lm", threshold = 2)
#' summary(tb_analysis)
#'   
#' @export
#' @return A dataframe indicating the results of the test at each time-bin.
analyze_time_bins.time_sequence_data <- function(data,
                              predictor_column,
                              test,
                              threshold = NULL,
                              alpha = NULL,
                              aoi = NULL,
                              formula = NULL,
                              p_adjust_method = "none",
                              quiet = FALSE,
                              ...)
{
  
  ## Helper:
  .fix_unpaired = function(data, data_options, predictor_column, dv) {
    if (!is.factor(data[[predictor_column]])) stop("Your condition column should be a factor.")
    lvl1 <- levels(data[[predictor_column]])[1]
    df_no_na   <- filter_(data, interp(~!is.na(DV), DV = as.name(dv)))
    summarized_by <- c("Time", attr(data, "eyetrackingR")$summarized_by)
    df_grouped <- group_by_(df_no_na, .dots = summarized_by)
    df_mutated <- mutate_(df_grouped,
                          .dots = list(PairedObs = interp(
                             ~length(which(COND_COL == lvl1)) > 0 & 
                              length(which(COND_COL != lvl1)) > 0,
                            COND_COL = as.name(predictor_column)))
    )
    df_filtered <- filter(df_mutated, PairedObs)
    ungroup(df_filtered)
  }

  # Data info:
  data_options <- attr(data, "eyetrackingR")$data_options
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: more informative?
  dots <- lazyeval::lazy_dots(...)
  
  # Need either alpha or threshold:
  if (!is.null(threshold) & !is.null(alpha)) stop("Please only specify alpha or threshold, not both.")
  if (is.null(threshold) & is.null(alpha)) stop("Please specify either alpha or threshold.")
  if (is.null(alpha)) {
    if (test %in% c('boot_splines', 'wilcox.test')) stop("This test requires `alpha` rather than `threshold`.")
  }

  # Which Test?:
  test <- match.arg(test, c("t.test","wilcox.test","lm","lmer","glm","glmer","boot_splines"))
  if (!requireNamespace("lme4", quietly = TRUE)) {
    if (test %in% c("lmer","glmer")) stop("Please install the 'lme4' package to use this method.")
  }

  # Multiple aois:
  if (!'AOI' %in% colnames(data)) stop("'AOI' column is missing from data.")
  unique_aois <- unique(data[['AOI']])
  if ( length(unique_aois) > 1 ) {           # DF has more than one AOI...
    if (is.null(aoi)) {                      # they haven't specified which is of interest
      if (predictor_column != "AOI") {       # its not the main predictor....
        if (!is.null(formula)) {             # they did specify a formula
          if (!grepl(pattern = 'AOI', x = deparse(formula))) {
            warning("There are multiple AOIs in your data frame, but your model does not use AOI as a predictor/covariate!",
                    immediate. = TRUE)
          }                                  # the formula has AOI-- no problem
        } else {                             # they did NOT specify a formula
          stop("If multiple AOIs in data, and `aoi` argument not specified, then you must manually specify a formula.")
        } 
      }                                      # predictor column *was* AOI-- no problem.
    } else {                                 # they *did* specify which AOI is of interest
      data <- filter(data, AOI == aoi)
      if (nrow(data)==0) stop("AOI not found in data.")
    }
  } 

  # Check that data is collapsed (by e.g. participants):
  if (!test %in% c("lmer","glmer")) {
    attrs <- attr(data, "eyetrackingR")
    summarized_by <- attrs$summarized_by
    if (is.null(summarized_by)) stop(test, " requires summarized data. ",
                                     "When using the 'make_time_sequence_data' function, please select an argument for 'summarize_by'",
                                     " (e.g., the participant column).")
  }

  # Formula Checks:
  if (grepl("intercept", predictor_column, ignore.case = TRUE)) {
    if (is.null(formula)) stop("If testing intercept, please manually specify formula")
    predictor_column <- "(Intercept)"
  }
  if (is.null(formula)) { # auto-make a formula, unless they specified one
    if (test%in% c("lmer","glmer")) stop("Must specify a formula if using (g)lmer.")
    formula <- as.formula(paste("Prop ~", predictor_column))
    dv <- "Prop"
  } else {
    dv <- gsub(formula[2], pattern = "()", replacement = "", fixed = TRUE)
  }
  
  # Run a model for each time-bin
  if (test!="boot_splines") {
    if (!quiet) message("Computing ", test, " for each time bin...")
    
    # Fix unpaired (only t.test/wilcox):
    paired <- eval(dots[["paired"]]$expr)
    if (identical(paired, TRUE) & test %in% c("t.test", "wilcox.test")) {
      # for paired t-tests, need to remove unpaired observations for each TB
      data <- .fix_unpaired(data, data_options, predictor_column, dv)
    }
    
    # Get Testing Function
    if (test %in% c("lmer","glmer")) {
      the_func <- .make_function_fail_informatively(get(test, envir = getNamespace("lme4")))
    } else {
      the_func <- .make_function_fail_informatively(get(test))
    }
    
    # Create Test/Summarize function, which also catches errors:
    the_test <- function(...) {
      res_err_warn <- the_func(...)
      if (test %in% c("lmer","glmer")) {
        out <- broom::tidy(res_err_warn$res, effects = 'fixed')
      } else {
        out <- broom::tidy(res_err_warn$res)
      }
      
      if (length(res_err_warn$err)>0) {
        df_err <- as.data.frame( do.call(cbind, as.list(res_err_warn$err)) )
        colnames(df_err) <- paste0("ErrorMsg", seq_along(res_err_warn$err))
        out <- df_err
      } else {
        # need to manually extract degrees of freedom for some tests
        # (not needed if func resulted in error)
        if (test %in% c("lm","glm")) out$parameter <- df.residual(res_err_warn$res)
      }
      
      if (length(res_err_warn$warn)>0) {
        df_warn <- as.data.frame( do.call(cbind, as.list(res_err_warn$warn)) )
        colnames(df_warn) <- paste0("WarningMsg", seq_along(res_err_warn$warn))
        df_warn <- bind_rows( lapply(X = 1:nrow(out), FUN = function(x) df_warn) )
        out <- bind_cols(out, df_warn)
      }
      
      out
    }
    
    # Run models:
    df_models <- data %>%
      group_by(Time) %>%
      do(the_test(formula = formula, data = ., ... = ...)) %>%
      as.data.frame()
    
    # Warn about warnings
    warn_cols <- grep("WarningMsg", colnames(df_models))
    if ( length(warn_cols)>0 ) {
      if (!quiet) message("At least one time-bin produced warnings--be sure to check 'Warning' col in output.")
      for (col in warn_cols) {
        unique_msgs <- unique(as.character(df_models[,col]))
        if (length(unique_msgs)==1) warning("All time-bins produced the same warning: '", unique_msgs, "'")
      }
    }
    err_cols <- grep("ErrorMsg", colnames(df_models))
    if ( length(err_cols)>0 ) {
      if (!quiet) message("At least one time-bin produced errors--be sure to check 'Error' col in output.")
      for (col in err_cols) {
        unique_msgs <- unique(as.character(df_models[,col]))
        if (length(unique_msgs)==1) stop("All time-bins produced same error: '", unique_msgs, "'")
      }
    }
    
    # Make model specifications the same for all test-types:
    if (test == "t.test") {
      df_models$std.error <- with(df_models, (conf.high-conf.low)/(1.96*2))
      if (!identical(paired, TRUE)) df_models$estimate <- df_models$estimate1 - df_models$estimate2
    } else if (test %in% c('glmer', 'lmer')) {
      if (!quiet) message("Using the normal approximation for p-value on parameter in ", test,".") 
      df_models$p.value <- with(df_models, 2*pnorm(q = abs(statistic), lower.tail = FALSE))
      df_models$parameter <- NA
    } else if (test == "wilcox.test") {
      df_models$statistic <- NA
      df_models$std.error <- NA
      df_models$estimate <- NA
      df_models$parameter <- NA
    }

    # Find Critical Value:
    if (is.null(threshold)) {
      df_models$CritStatisticPos <- .get_threshold(alpha, test, df_models$parameter, quiet)
    } else {
      df_models$CritStatisticPos <- ifelse(sign(threshold)==1,  threshold, -threshold)
    }
    df_models$CritStatisticNeg <- -df_models$CritStatisticPos
    
    # Filter Param-of-Interest:
    if (test %in% c('t.test', 'wilcox.test')) {
      df_models_this_param <- df_models
    } else {
      df_models_this_param <- filter(df_models, term == predictor_column)
      if (nrow(df_models_this_param)==0) {
        terms <- paste0("'", unique(df_models$term), "'", collapse= ", ")
        msg <- paste0("\nThe term '", predictor_column, "' was not found in your model. \nFound instead: ", terms)
        stop(msg)
      }
    }
    
    # Generate Output:
    new_cols <- list(Estimate = quote(estimate), 
                     StdErr = quote(std.error), 
                     Statistic = quote(statistic), 
                     Prob = quote(p.value), 
                     DF = quote(parameter),
                     quote(Time), quote(CritStatisticPos), quote(CritStatisticNeg) 
    )
    new_cols <- append(new_cols, grep("WarningMsg", colnames(df_models), value = TRUE))
    new_cols <- append(new_cols, grep("ErrorMsg", colnames(df_models), value = TRUE))
    out <- select_(.data = df_models_this_param, .dots = new_cols)

  } else if (test=="boot_splines") {

    # arg-check:
    if (is.null( dots$within_subj$expr )) stop("Method 'boot_splines' requires you specify `within_subj`.")
    
    # Make Boot Splines:
    bs_samples <- eval(dots$bs_samples$expr)
    if (is.null(bs_samples)) {
      if (!is.null(dots$samples$expr)) {
        bs_samples <- eval(dots$samples$expr)
        warning("The 'samples' argument is deprecated in boot-splines, please use 'bs_samples' in the future.", 
                call. = FALSE)
      }
    }
    the_args <- list(data = data, predictor_column = predictor_column, aoi = aoi, alpha = alpha)
    for (this_arg in names(dots)) {
      the_args[[this_arg]] <- dots[[this_arg]]$expr
    }
    the_args[["bs_samples"]] <- ifelse(is.null(bs_samples), 1000, bs_samples)
    bs_dat <- do.call(make_boot_splines_data, the_args)
    
    # Get Estimates:
    bs_anal <- analyze_boot_splines(bs_dat)
    
    out <- select(bs_anal, 
                  Time, Estimate = MeanDiff, StdErr = SE, Statistic, Significant, ConfLow = CI_low, ConfHigh = CI_high)
    out <- mutate(out,
                  Prob = NA, 
                  DF = NA,
                  CritStatisticNeg =  NA, 
                  CritStatisticPos =  NA)
    
  }
  
  # Adjust P-val.
  if (p_adjust_method != "none") {
    if (test == "boot_splines") stop("The p_adjust_method must be 'none' for boot-splines test.")
    if (is.null(alpha)) stop("If specifying p-value adjustment, must give alpha, not threshold.")
    out$Prob <- p.adjust(p = out$Prob, method = p_adjust_method)
    out$CritStatisticNeg <- out$CritStatisticPos <- NA
  }

  # Compute Information about Runs:
  if (test == "boot_splines") {
    out$PositiveRuns <- .label_consecutive( out$Significant & out$Statistic>0 )
    out$NegativeRuns <- .label_consecutive( out$Significant & out$Statistic<0 )
  } else {
    if (is.null(alpha)) {
      out$PositiveRuns <- .label_consecutive(out$Statistic>out$CritStatisticPos)
      out$NegativeRuns <- .label_consecutive(out$Statistic<out$CritStatisticNeg)
    } else {
      out$PositiveRuns <- .label_consecutive((alpha>out$Prob) & out$Statistic>0)
      out$NegativeRuns <- .label_consecutive((alpha>out$Prob) & out$Statistic<0)
    }
  }
  
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
         p_adjust_method = p_adjust_method,
         alpha = alpha,
         threshold = threshold,
         test = test,
         predictor = predictor_column,
         positive_runs = positive_runs,
         negative_runs = negative_runs)
  )
  out = out[order(out$Time),] # arrange chronologically if not already
  out
}

#' Summary Method for Time-bin Analysis
#' @param  object The output of the \code{analyze_time_bins} function
#' @param ... Ignored
#' @export
#' @return Prints information about each run of statistically significant time-bins, separately for
#'   positive and negative
summary.bin_analysis <- function(object, ...) {
  
  attrs <- attr(object, "eyetrackingR")
  positive_runs <- seq_along(attrs$positive_runs)
  negative_runs <- seq_along(attrs$negative_runs)
  df_pos <- bind_rows(lapply(attrs$positive_runs, as.data.frame))
  df_neg <- bind_rows(lapply(attrs$negative_runs, as.data.frame))
  
  p1 <- paste("Test Type:\t", attrs$test,
              "\nPredictor:\t", attrs$predictor,
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
  invisible(object)
}

#' Plot time-sequence data
#' 
#' Plot the timecourse of looking. Each AOI will be plotted in a separate pane, and data can be 
#' split into groups by a predictor column. Data is collapsed by subject for plotting. Supports
#' overlaying the predictions of a growth-curve mixed effects model on the data
#' 
#' @param x              Your data from \code{make_time_sequence_data}. Will be collapsed by
#'   subject for plotting (unless already collapsed by some other factor).
#' @param predictor_column  Data can be grouped by a predictor column (median split is performed if
#'   numeric)
#' @param dv                What measure of gaze do you want to use? (\code{Prop}, \code{Elog}, or
#'   \code{ArcSin})
#' @param model             (Optional) A growth-curve mixed effects model (from \code{lmer}) that
#'   was used on the \code{time_sequence_data}. If model is given, this function will overlay the
#'   predictions of that model on the data
#' @param ... Ignored
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
#' response_time <- make_time_sequence_data(data, time_bin_size = 250, 
#'                                          predictor_columns = c("MCDI_Total"),
#'                                          aois = "Animate", summarize_by = "ParticipantName")
#' 
#' # visualize time results
#' plot(response_time, predictor_column = "MCDI_Total") 
#' 
#' 
#' @export
#' @return A ggplot object
plot.time_sequence_data <- function(x, predictor_column = NULL, dv='Prop', model = NULL,...) {

  # Prelims:
  data_options = attr(x, "eyetrackingR")$data_options
  if (!dv %in% colnames(x)) stop("Selected 'dv' is not in data.")

  # Add model predictions to dataframe:
  if (!is.null(model)) {
    formula_as_character <- Reduce(paste, deparse(formula(model)))
    if (!grepl(dv, formula_as_character, fixed = TRUE)) {
      stop("Your model appears to use a different DV than the one you are attempting to plot. Change the 'dv' arg in plot.")
    }
    x$.Predicted = predict(model, x, re.form = NA)
  } 

  ## Collapse by-subject for plotting
  if (is.null(attr(x, "eyetrackingR")$summarized_by)) {
    df_plot <- group_by_(x, .dots = c(data_options$participant_column, "Time", "AOI", predictor_column))
    summarize_arg <- list(interp(~mean(DV, na.rm=TRUE), DV = as.name(dv)))
    names(summarize_arg) <- dv
    if (!is.null(model)) summarize_arg[[".Predicted"]] <- ~mean(.Predicted, na.rm=TRUE)
    df_plot <- summarize_(df_plot, .dots = summarize_arg)
  } else {
    df_plot <- x
  }
  
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
    message("Predictor is continuous/numeric, performing median split for visualization...")
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
    stat_summary(fun.data=mean_se, geom='ribbon', alpha= .25, colour=NA)
  
  if (!is.null(model)) {
    g <- g +
      stat_summary(aes(y = .Predicted), fun.y = 'mean', geom="line", size= 1.2) 
  }
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
  
  
  return(g)
  
}

#' Plot test-statistic for each time-bin in a time-series
#' 
#' Plot the result from the \code{analyze_time_bins} function, with the statistic and threshold for each bin
#' 
#' @param x The output of \code{analyze_time_bins}
#' @param type This function can plot the test-statistic ("statistic"), the parameter estimate +/-
#'   std. error ("estimate"), the p-value ("pvalue") or the negative-log-pvalue ("neg_log_pvalue").
#'   When test gives critical-statistic, default is to plot the test-statistic. Otherwise, default
#'   is to plot the estimate. For wilcox, only p-values can be plotted.
#' @param ... Ignored
#' @export
#' @return A ggplot object
plot.bin_analysis <- function(x, type = NULL, ...) {
  
  p_adjust_method <- attr(x, "eyetrackingR")$p_adjust_method
  test <- attr(x, "eyetrackingR")$test
  
  # if crit-statistic is valid, default plot statistic
  # if not (boot-splines, wilcox, p-adjust), then default plot estimate
  
  if (p_adjust_method != "none") {
    if (is.null(type)) type <- "estimate"
    if (type == "statistic") message("Cannot compute critical value when p-value adjustment is used.")
  } else if (test == "boot_splines") {
    if (is.null(type)) type <- "estimate"
    if (type %in% c('pvalue','neg_log_pvalue')) stop("Boot-splines test does not produce p-values.")
  } else if (test == "wilcox.test") {
    if (is.null(type)) type <- "neg_log_pvalue"
    if (type %in% c("statistic", "estimate")) stop("Can only plot p-values for wilcox test.")
  } else {
    if (is.null(type)) type <- "statistic"
  }
  
  type <- match.arg(type, c("statistic", "estimate", "pvalue", "neg_log_pvalue"))

  if (type == "statistic") {
    g <- ggplot(data = x) +
      geom_line(mapping = aes(x = Time, y= Statistic)) +
      ylab("Statistic") 
    if (p_adjust_method == "none" & test != "boot_splines") {
      g <- g + 
        geom_line(mapping = aes(x = Time, y= CritStatisticPos), linetype="dashed") +
        geom_line(mapping = aes(x = Time, y= CritStatisticNeg), linetype="dashed")
    } 
  } else if (type == "estimate") {
    g <- ggplot(data = x) +
      geom_line(mapping = aes(x = Time, y= Estimate)) +
      geom_ribbon(mapping = aes(x = Time, ymin = Estimate - StdErr, ymax = Estimate + StdErr), alpha=.33) + 
      geom_hline(yintercept = 0, linetype="dashed") +
      ylab("Parameter Estimate (+/-Std.Err)") 
  } else if (type == "pvalue") {
    alpha <- attr(x, "eyetrackingR")$alpha
    g <- ggplot(data = x) + 
      geom_line(mapping = aes(x = Time, y = Prob)) +
      coord_cartesian(ylim=c(0,1)) + ylab("P Value") 
    if (!is.null(alpha)) {
      g <- g + geom_hline(yintercept = alpha, linetype="dashed")
    }
  } else if (type == "neg_log_pvalue") {
    alpha <- attr(x, "eyetrackingR")$alpha
    g <- ggplot(data = x) + 
      geom_line(mapping = aes(x = Time, y = -log(Prob)) ) +
      ylab("-Log(P Value)") 
    if (!is.null(alpha)) {
      g <- g + geom_hline(yintercept = -log(alpha), linetype="dashed")
    }
  }
  if (length(unique(x$AOI))>1) g <- g + facet_grid(. ~ AOI, labeller = "label_both")
  
  # Shade significant runs:
  negative_runs <- data.frame(
    Start = sapply(attr(x, "eyetrackingR")$negative_runs, function(run) run$start_time),
    Stop  = sapply(attr(x, "eyetrackingR")$negative_runs, function(run) run$stop_time)
  )
  if (nrow(negative_runs)>0) {
    g <- g + geom_rect(data = negative_runs, fill= "blue", alpha = .20, 
                       mapping = aes(xmin = Start, xmax = Stop, 
                                     ymin = -Inf, ymax= Inf))
  }
  positive_runs <- data.frame(
    Start = sapply(attr(x, "eyetrackingR")$positive_runs, function(run) run$start_time),
    Stop  = sapply(attr(x, "eyetrackingR")$positive_runs, function(run) run$stop_time)
  )
  if (nrow(positive_runs)>0) {
    g <- g +
      geom_rect(data = positive_runs, fill= "blue", alpha = .20, 
                mapping = aes(xmin = Start, xmax = Stop, 
                              ymin = -Inf, ymax= Inf))
  }

  return(g+xlab("Time"))
}
