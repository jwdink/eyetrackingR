#' Bootstrap analysis of time-clusters.
#'
#' Takes data whose time bins have been clustered by test-statistic (using the \code{make_time_cluster_data}
#' function) and performs a permutation test (Maris & Oostenveld, 2007). This analysis takes a summed
#' statistic for each cluster, and compares it to the "null" distribution of sum statistics obtained by
#' shuffling/resampling the data and extracting the largest cluster from each resample.
#' @export
analyze_time_clusters <-function(data, ...) {
  UseMethod("analyze_time_clusters")
}
#' @describeIn analyze_time_clusters
#'
#' @param data          The output of the \code{make_time_cluster_data} function
#' @param within_subj   Logical indicating whether to perform within-subjects bootstrap resampling.
#' @param samples       How many iterations should be performed in the bootstrap resampling procedure?
#' @param formula       Formula for test. Should be identical to that passed to make_time_cluster_data fxn (if
#'   arg ignored there, can be ignored here)
#' @param shuffle_by    Along which attribute should the data be resampled? Default is the predictor
#'   column. But if the predictor_column is numeric *and* within-subjects, then observations with the 
#'   same predictor value could nevertheless correspond to distinct conditions/categories that
#'   should be shuffled separately. For example, when using vocabulary scores to predict looking
#'   behavior, a participant might get identical vocab scores for verbs and nouns; these are
#'   nevertheless distinct categories that should be re-assigned separately when
#'   bootstrap-resampling data. The 'shuffle_by' argument allows you to specify a column which
#'   indicates these kinds of distinct categories that should be resampled separately--
#'   but it's only needed if you've specified a numeric *and* within-subjects predictor column.
#' @param parallel      Use foreach for speed boost? By default off. May not work on Windows.
#' @param quiet         Display progress bar/messages? No progress bar when parallel=TRUE.
#' @param ...           Other args for to selected 'test' function; should be identical to those passed to
#'   \code{make_time_cluster_data} function
#'   
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE )
#' response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
#'                                     rezero = FALSE)
#' response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
#'                                          predictor_columns = "Sex")
#' 
#' time_cluster_data <- make_time_cluster_data(data = response_time, predictor_column = "SexM", 
#'                          aoi = "Animate", test = "lmer", 
#'                          threshold = 1.5, 
#'                          formula = LogitAdjusted ~ Sex + (1|Trial) + (1|ParticipantName))
#' summary(time_cluster_data)
#' plot(time_cluster_data)
#' 
#' # analyze time clusters in a non-parametric analysis
#' \dontrun{
#' tc_analysis <- analyze_time_clusters(time_cluster_data, within_subj = FALSE,
#'                                      samples = 2000)
#' plot(tc_analysis)
#' summary(tc_analysis)
#' }
#'   
#' @export
#' @return A cluster-analysis object, which can be plotted and summarized to examine which temporal periods
#'   show a significant effect of the predictor variable
analyze_time_clusters.time_cluster_data <-function(data,
                                                   within_subj,
                                                   samples = 2000,
                                                   formula = NULL,
                                                   shuffle_by = NULL,
                                                   parallel = FALSE,
                                                   quiet = FALSE,
                                                   ...) {

  # Get important information about type of data/analyses, input when running make_time_cluster_data
  attrs <-attr(data, "eyetrackingR")
  data_options <-attrs$data_options
  dots <- lazyeval::lazy_dots(...)
  
  # Check dots:
  if (is.null(attrs$the_dots)) attrs$the_dots <- c()
  dot_names <- names(dots)
  if (attrs$test=="boot_splines") dot_names <- c(dot_names, "alpha", "within_subj")

  dots_found <- attrs$the_dots %in% dot_names
  if (!all(dots_found)) {
    warning(immediate. = TRUE,
            "Not all extra args supplied to `make_time_cluster_data` were supplied here. Missing: ",
            paste(attrs$the_dots[!dots_found], collapse=", "))
  }
  dots_extra <- setdiff(names(dots), attrs$the_dots)
  if (length(dots_extra)>0) {
    warning(immediate. = TRUE,
             "Not all extra args given here were supplied to `make_time_cluster_data`. Missing previously: ",
             paste(dots_extra, collapse=", "))
  }

  # Progress Bar / Parallel:
  if (parallel) {
    success <- c(requireNamespace("foreach", quietly = TRUE),
                 requireNamespace("parallel", quietly = TRUE),
                 requireNamespace("doMC", quietly = TRUE))
    if (!all(success)) stop("For parallel = TRUE, the 'doMC' package must be installed.")
    doMC::registerDoMC()
  } else {
    if (!requireNamespace("pbapply", quietly = TRUE)) {
      pbsapply <- sapply
      message("Install package 'pbapply' for a progress bar in this function.")
    } else {
      pbsapply <- pbapply::pbsapply
    }
  }
  if (quiet) pbsapply <- sapply
  
  # Arg check:
  if (is.null(formula)) {
    formula <-attrs$formula
  } else {
    if (attrs$formula != formula) stop("Formula given in 'make_time_cluster_data' does not match formula given here.")
  }
  if (attrs$test %in% c("t.test", "wilcox.test")) {
    paired <- eval(dots[["paired"]]$expr)
    if (within_subj==TRUE) {
      # if within_subj is true, we need to confirm they overrode default
      if (!identical(paired, TRUE)) stop("For ", attrs$test, ", if 'within_subj' is TRUE, then 'paired' should also be TRUE.")
    } else {
      # if within_subj is false, we just need to confirm they didn't set paired = TRUE
      if (identical(paired, TRUE)) stop("For ", attrs$test, ", if 'within_subj' is FALSE, then 'paired' should also be FALSE.")
    }
  } else if (attrs$test %in% c("lm","glm")) {
    if (within_subj == TRUE) warning("For lm, 'within_subj' should probably be FALSE.")
  }
  
  # what are we grouping our resampling by? default is by participant:
  summarized_by <- attrs$summarized_by
  if (is.null(summarized_by)) summarized_by <- data_options$participant_column
  
  # Resample this data and get sum statistic each time, creating null distribution
  if (within_subj) {

    # Within-Subjects
    
    # Shuffle data by
    if (is.null(shuffle_by)) {
      shuffle_by <- attrs$predictor_column
      
      if ( attrs$test %in% c("lm", "lmer") & is.numeric(data[[attrs$predictor_column]]) & within_subj) {
        stop("If predictor column is numeric and within-subjects, you should specify a 'shuffle_by' argument. ",
             "See the documentation for more details.")
      }
    }
    if(! shuffle_by %in% colnames(data) ) stop("The column '", shuffle_by, "' not found in your data.")
    
    # Get PPTs (or whatever unit-of-analysis is being used):
    participants <-unique(data[[summarized_by]])

    # get a list of list of rows. outer list corresponds to participants/items, inner to conditions
    list_of_list_of_rows <-lapply(X = participants, FUN = function(ppt) {
      ppt_logical <-(data[[summarized_by]] == ppt)

      # for each participant, get the rows for each of level of the shuffle_by col
      this_ppt_levels <-unique(data[[shuffle_by]][ppt_logical])
      out <- lapply(X = this_ppt_levels, FUN = function(lev) {
        which(ppt_logical & data[[shuffle_by]] == lev)
      })
      names(out) <- this_ppt_levels
      return(out)
    })
    
    get_resampled_sum_stat_within <- function(data, list_of_list_of_rows, attrs, formula, ...) {
      df_resampled <- data
      dots <- lazyeval::lazy_dots(...)
      
      # for each participant, randomly resample rows to be assigned to each possible level of the predictor
      # TO DO: keep this in mind as a performance bottleneck. if so, refactor code so that df_resampled only
      # gets reassigned once per condition across all participants/items (rather than once per condition per participant)
      for (list_of_rows in list_of_list_of_rows) {
        resampled <-sample(x = list_of_rows, size = length(list_of_rows), replace = FALSE)
        for (i in seq_along(resampled)) {
          rows_orig <-list_of_rows[[i]]
          rows_new <-resampled[[i]]
          df_resampled[rows_new,attrs$predictor_column] <- data[first(rows_orig),attrs$predictor_column]
        }
      }
      
      # this gives a dataframe where the "predictor" label has been resampled within each participant
      # perform the clustering procedure on this
      df_resampled$Cluster <- NULL
      attr(df_resampled, "eyetrackingR")$clusters <- NULL
      attr(df_resampled, "eyetrackingR")$time_bin_summary <- NULL
      the_args = list(
        data = df_resampled,
        predictor_column = attrs$predictor_column,
        test = attrs$test,
        threshold = attrs$threshold,
        formula = formula,
        treatment_level = attrs$treatment_level
      )
      if (attrs$test=="boot_splines") {
        the_args$within_subj <- TRUE
        the_args$samples <- dots$boot_samples$expr
        the_args$alpha <- attrs$alpha
      }
      for (this_arg in names(dots)) {
        the_args[[this_arg]] <- dots[[this_arg]]$expr
      }
      the_args$boot_samples <- NULL
      shuffled_cluster_data <- do.call(make_time_cluster_data, the_args)
      clusters <- get_time_clusters(shuffled_cluster_data)
      if ( nrow(clusters)>0 ) {
        ss <- clusters$SumStatistic
        return( ss[which.max(abs(ss))] )
      } else {
        return(0)
      }

   }
    
    if (parallel) {
      null_distribution <- foreach::`%dopar%`(foreach::foreach(iter = 1:samples, .combine = 'c', .inorder = FALSE),
                                              get_resampled_sum_stat_within(data, list_of_list_of_rows, attrs, formula, ... = ...))
    } else {
      null_distribution <- pbsapply(1:samples, 
                                    FUN = function(iter) get_resampled_sum_stat_within(data, list_of_list_of_rows, 
                                                                                attrs, formula, ... = ...) )
    }
    

  } else {

    # Between Subjects

    # get rows for each participant
    participants <-unique(data[[summarized_by]])
    rows_of_participants <- lapply(participants, FUN = function(ppt) which(data[[summarized_by]] == ppt))

    get_resampled_sum_stat_btwn <- function(data, rows_of_participants, attrs, formula, ...) {
      df_resampled <- data
      dots <- lazyeval::lazy_dots(...)
      
      # randomly re-assign each participant/item to a condition:
      rows_of_participants_resampled <- sample(rows_of_participants, size=length(rows_of_participants), replace=FALSE)
      for (i in seq_along(rows_of_participants_resampled)) {
        # TO DO: keep this in mind as a performance bottleneck. if so, refactor code so that df_resampled only
        # gets reassigned once per condition across all participants/items (rather than once per participant)
        df_resampled[rows_of_participants_resampled[[i]], attrs$predictor_column] <-
          data[first(rows_of_participants[[i]]),attrs$predictor_column]
      }
      
      # this gives a dataframe where the "condition" label has been shuffled for participants/items
      # run analyze time bins on it to get sum statistic for cluster
      df_resampled$Cluster <- NULL
      attr(df_resampled, "eyetrackingR")$clusters <- NULL
      attr(df_resampled, "eyetrackingR")$time_bin_summary <- NULL
      the_args = list(
        data = df_resampled,
        predictor_column = attrs$predictor_column,
        test = attrs$test,
        threshold = attrs$threshold,
        formula = formula
      )
      if (attrs$test=="boot_splines") {
        the_args$within_subj <- FALSE
        the_args$samples <- dots$boot_samples$expr
        the_args$alpha <- attrs$alpha
      }
      for (this_arg in names(dots)) {
        the_args[[this_arg]] <- dots[[this_arg]]$expr
      }
      the_args$boot_samples <- NULL
      shuffled_cluster_data <- do.call(make_time_cluster_data, the_args)
      clusters <- get_time_clusters(shuffled_cluster_data)
      if ( nrow(clusters)>0 ) {
        ss <- clusters$SumStatistic
        return( ss[which.max(abs(ss))] )
      } else {
        return(0)
      }
      
    }

    if (parallel) {
      null_distribution <- foreach::`%dopar%`(foreach::foreach(iter = 1:samples, .combine = 'c', .inorder = FALSE),
                                              get_resampled_sum_stat_btwn(data, rows_of_participants, attrs, formula, ... = ...))
    } else {
      null_distribution <- pbsapply(1:samples, 
                                    FUN = function(iter) get_resampled_sum_stat_btwn(data, rows_of_participants, 
                                                                                attrs, formula, ... = ...) )
    }
    
  }

  # Get p-values (two-tailed):
  out <-c(list(null_distribution = null_distribution), attr(data, "eyetrackingR"))
  probs <-sapply(out$clusters$SumStatistic,
                 FUN= function(ss) { sum( 
                   sum(out$null_distribution <= min(ss, -ss)) / length(out$null_distribution),  # left-tail
                   sum(out$null_distribution >= max(ss, -ss)) / length(out$null_distribution)  # right-tail
                 )
            })
  
  out$clusters$Probability <- probs
  
  #
  class(out)  <- "cluster_analysis"
  return(out)

}

#' Get information about the clusters in a cluster-analysis
#' @param  object The output of the \code{analyze_time_clusters} function
#' @export
#' @return A dataframe with information about the clusters
get_time_clusters <- function(object) UseMethod("get_time_clusters")

#' @describeIn get_time_clusters Get time clusters dataframe
#' @export
get_time_clusters.time_cluster_data<- function(object) {
  attr(object, "eyetrackingR")$clusters
}

#' @describeIn get_time_clusters Get time clusters dataframe
#' @export
get_time_clusters.cluster_analysis<- function(object){
  object$clusters
}

#' Summary Method for Cluster Analysis
#' @param  object The output of the \code{analyze_clusters} function
#' @param ... Ignored
#' @export
#' @return Prints information about the bootstrapped null distribution, as well as information about each cluster.
summary.cluster_analysis <- function(object, ...) {
  print(object)
}

#' Print Method for Cluster Analysis
#' @param  x The output of the \code{analyze_clusters} function
#' @param ... Ignored
#' @export
#' @return Prints information about the bootstrapped null distribution, as well as information about each cluster.
print.cluster_analysis <- function(x, ...) {
  object <- x
  clusters <- object$clusters
  cat(
    "Test Type:\t", object$test,
    "\nPredictor:\t", object$predictor_column,
    "\nFormula:\t", Reduce(paste, deparse(object$formula)),
    "\nNull Distribution   ======",
    "\n Mean:\t\t", round(mean(object$null_distribution, na.rm=TRUE), digits = 4),
    "\n 2.5%:\t\t", round(quantile(object$null_distribution, probs = .025, na.rm=TRUE), digits = 4),
    "\n97.5%:\t\t", round(quantile(object$null_distribution, probs = .975, na.rm=TRUE), digits = 4),
    "\nSummary of Clusters ======\n"
  )
  print(clusters)
  invisible(object)
}

#' Make data for cluster analysis.
#'
#' Takes data that has been summarized into time-bins by \code{make_time_sequence_data()}, finds adjacent time
#' bins that pass some test-statistic threshold, and assigns these adjacent bins into groups (clusters).
#' Output is ready for a cluster permutation-based analyses (Maris & Oostenveld, 2007). Supports \code{t.test},
#' \code{wilcox.test}, \code{(g)lm}, and \code{(g)lmer}. Also includes support for
#' the "bootstrapped-splines" test (see \code{?make_boot_splines_data} and 
#' \href{http://www.eyetracking-r.com/vignettes/divergence}{the divergence vignette} for more info). 
#' By default, this function uses 'proportion-looking' (\code{Prop}) as the DV, which can be changed
#' by manually specifying the formula. 
#' 
#' @export
make_time_cluster_data <-function(data, ...) {
  UseMethod("make_time_cluster_data")
}
#' @describeIn make_time_cluster_data Make data for time cluster analysis
#' @param data   The output of the \code{make_time_sequence_data} function
#' @param predictor_column  The column name containing the variable whose test statistic you are interested in. 
#' @param aoi               Which AOI should be analyzed? If not specified (and dataframe has multiple AOIs), 
#'                          then AOI should be a predictor/covariate in your model (so `formula` needs 
#'                          to be specified).
#' @param test              What type of test should be performed in each time bin? Supports
#'                          \code{t.test}, \code{(g)lm}, or \code{(g)lmer}. Also includes experimental support for
#'                          the "bootstrapped-splines" test (see \code{?make_boot_splines_data} and 
#'                          \href{http://www.eyetracking-r.com/vignettes/divergence}{the divergence vignette} 
#'                          for more info). Does not support \code{wilcox.test}. 
#' @param threshold         Time-bins with test-statistics greater than this amount will be grouped into clusters. 
#' @param formula           What formula should be used for test? Optional (for all but \code{(g)lmer}), if unset
#'   uses \code{Prop ~ [predictor_column]}
#' @param treatment_level   If your predictor is a factor, regression functions like `lm` and `lmer` by default will 
#'                          treatment-code it. One option is to sum-code this predictor yourself before entering it 
#'                          into this function. Another is to use the `treatment_level` argument, which specifies the 
#'                          level of the predictor. For example, you are testing a model where `Target` is a predictor, 
#'                          which has two levels, 'Animate' and 'Inanimate'. R will code 'Animate' as the reference 
#'                          level, and code 'Inanimate' as the treatment level. You'd therefore want to set 
#'                          `treatment_level = Inanimate`.
#' @param ...               Any other arguments to be passed to the selected 'test' function (e.g., paired,
#'   var.equal, etc.)
#'   
#' @examples 
#' data(word_recognition)
#' data <- make_eyetrackingr_data(word_recognition, 
#'                                participant_column = "ParticipantName",
#'                                trial_column = "Trial",
#'                                time_column = "TimeFromTrialOnset",
#'                                trackloss_column = "TrackLoss",
#'                                aoi_columns = c('Animate','Inanimate'),
#'                                treat_non_aoi_looks_as_missing = TRUE )
#' response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
#'                                     rezero = FALSE)
#' 
#' # identify clusters in the sequence data using a t-test with
#' # threshold t-value of 2
#' 
#' # (note: t-tests require a summarized dataset)
#' response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
#'                                          predictor_columns = "Sex",
#'                                          summarize_by = "ParticipantName")
#'                                          
#' time_cluster_data <- make_time_cluster_data(data = response_time,
#'                                             predictor_column = "Sex",
#'                                             aoi = "Animate",
#'                                             test = "t.test",
#'                                             threshold = 2
#' )
#' 
#' # identify clusters in the sequence data using an lmer() random-effects
#' # model with a threshold t-value of 1.5.
#' 
#' # random-effects models don't require us to summarize
#' response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
#'                                          predictor_columns = "Sex")
#'    
#' # but they do require a formula to be specified
#' time_cluster_data <- make_time_cluster_data(data = response_time,
#'                            predictor_column = "SexM",
#'                            aoi = "Animate",
#'                            test = "lmer",
#'                            threshold = 1.5,
#'                            formula = LogitAdjusted ~ Sex + (1|Trial) + (1|ParticipantName)
#' )
#'   
#' @export
#' @return The original data, augmented with information about clusters. Calling summary on this data will
#'   describe these clusters. The dataset is ready for the \code{\link{analyze_time_clusters}} method.
make_time_cluster_data.time_sequence_data <- function(data,
                                                     predictor_column,
                                                     aoi = NULL,
                                                     test,
                                                     threshold = NULL,
                                                     formula = NULL,
                                                     treatment_level = NULL,
                                                     ...) {
  
  
  attrs <- attr(data, "eyetrackingR")
  data_options <- attrs$data_options
  dots <- lazyeval::lazy_dots(...)
  if (is.null(data_options)) stop("Dataframe has been corrupted.") # <----- TO DO: fix later
  if (test == "wilcox.test") stop("The wilcox.test is not supported for cluster-analysis.")
  if (test == "boot_splines") {
    if (is.null(dots$alpha$expr)) stop("For boot_splines test, please specify alpha instead of threshold.")
  } else {
    if (is.null(threshold)) stop("Please specify threshold for this test.")
  }
  
  # Filter Data:
  if (is.null(aoi)) {
    if (length(unique(data$AOI)) == 1) {
      aoi <- unique(data$AOI)
    } 
  } else {
    data <- filter(data, AOI == aoi)
    if (nrow(data)==0) stop("AOI not found in data.")
  }

  # Compute Time Bins:
  the_args <- list(data = data, predictor_column = predictor_column, test = test, formula = formula, 
                   aoi = aoi, treatment_level = treatment_level, quiet = TRUE)
  the_args[["threshold"]] <- threshold
  for (this_arg in names(dots)) {
    the_args[[this_arg]] <- dots[[this_arg]]$expr
  }
  time_bin_summary <- do.call(analyze_time_bins, the_args) 
  time_bin_summary <- rename(time_bin_summary, ClusterPos = PositiveRuns, ClusterNeg = NegativeRuns)
  formula <- attr(time_bin_summary, "eyetrackingR")$formula

  # Compute Sum Statistic for each Positive Cluster
  sum_stat_pos <- c()
  for (clust in na.omit(unique(time_bin_summary$ClusterPos))) {
    sum_stat_pos[clust] <- sum(time_bin_summary$Statistic[which(time_bin_summary$ClusterPos==clust)])
  }
  sum_stat_neg <- c()
  for (clust in na.omit(unique(time_bin_summary$ClusterNeg))) {
    sum_stat_neg[clust] <- sum(time_bin_summary$Statistic[which(time_bin_summary$ClusterNeg==clust)])
  }

  # Merge cluster info into original data
  df_timeclust <- left_join(data, time_bin_summary[,c('Time','ClusterNeg','ClusterPos')], by=c('Time'))
  
  # Collect info about each cluster:
  clusters <-
    data.frame(Cluster = seq_along(c(sum_stat_pos, sum_stat_neg)),
               Direction = unlist(c(rep("Positive", length.out = length(sum_stat_pos)), 
                                    rep("Negative", length.out = length(sum_stat_neg)))),
               SumStatistic = c(sum_stat_pos, sum_stat_neg),
               StartTime = unlist(c(sapply(seq_along(sum_stat_pos),
                                           FUN = function(clust) time_bin_summary$Time[first(which(time_bin_summary$ClusterPos==clust))] ),
                                    sapply(seq_along(sum_stat_neg),
                                           FUN = function(clust) time_bin_summary$Time[first(which(time_bin_summary$ClusterNeg==clust))] ) )),
               EndTime = unlist(c(sapply(seq_along(sum_stat_pos),
                                         FUN = function(clust) time_bin_summary$Time[last(which(time_bin_summary$ClusterPos==clust))] ),
                                  sapply(seq_along(sum_stat_neg),
                                         FUN = function(clust) time_bin_summary$Time[last(which(time_bin_summary$ClusterNeg==clust))] ) ))
    ) 
  clusters$EndTime <-clusters$EndTime + attrs$time_bin_size

  # Output data, add attributes w/ relevant info
  df_timeclust <- as.data.frame(df_timeclust)
  class(df_timeclust) <- unique(c("time_cluster_data", "time_sequence_data", "eyetrackingR_df", class(df_timeclust)))
  attr(df_timeclust, "eyetrackingR") <- c(attrs,
                                         list(clusters = clusters,
                                              predictor_column = predictor_column,
                                              test = test,
                                              threshold = threshold,
                                              alpha = dots$alpha$expr,
                                              formula = formula,
                                              time_bin_summary = time_bin_summary,
                                              the_dots = names(dots),
                                              treatment_level = treatment_level)
  )
  df_timeclust

}

#' Summary Method for Cluster Analysis
#' @param  object The output of the \code{analyze_clusters} function
#' @param ... Ignored
#' @export
#' @return Prints information about the bootstrapped null distribution, as well as information about each cluster.
summary.time_cluster_data <- function(object, ...) {
  clusters <- attr(object, "eyetrackingR")$clusters
  if (length(clusters)==0) {
    cat(
      "Test Type:\t", attr(object, "eyetrackingR")$test,
      "\nPredictor:\t", attr(object, "eyetrackingR")$predictor_column,
      "\nFormula:\t", Reduce(paste, deparse(attr(object, "eyetrackingR")$formula)),
      "\nNo Clusters"
    )
  } else {
    cat(
      "Test Type:\t", attr(object, "eyetrackingR")$test,
      "\nPredictor:\t", attr(object, "eyetrackingR")$predictor_column,
      "\nFormula:\t", Reduce(paste, deparse(attr(object, "eyetrackingR")$formula)),
      "\nSummary of Clusters ======\n"
      )
    print(clusters)
  }
  invisible(object)
}


#' Visualize the results of a cluster analysis.
#'
#' Plots the result of the bootstrapping cluster analysis. A histogram of the sum statistics for the
#' shuffled (null) distribution, with the sum statisics for each of the clusters indicated by dashed lines.
#'
#' @param x object returned by cluster_analysis()
#' @param ... Ignored
#' @export
#' @return A ggplot object
plot.cluster_analysis <- function(x, ...) {
  
  df_plot1 <- data.frame(NullDistribution = x$null_distribution)
  
  g <- ggplot(data = df_plot1, aes(x = NullDistribution)) +
    geom_density() +
    geom_histogram(aes(y=..density..), binwidth = sd(x$null_distribution)/5, alpha=.75 ) +
    xlab(paste("Distribution of summed statistics from", x$test )) + ylab("Density")
  
  if (nrow(x$clusters)>0) {
    dat <- c(x$clusters$SumStatistic, x$null_distribution)
    x_min <- min(dat, na.rm=TRUE) - sd(dat)
    x_max <- max(dat, na.rm=TRUE) + sd(dat)
    cluster_names <- as.factor(x$clusters$Cluster)
    df_plot2 <- data.frame(SumStat = x$clusters$SumStatistic,
                           Cluster = cluster_names)
    g <- g + 
      coord_cartesian(xlim = c(x_min, x_max)) +
      geom_vline(data = df_plot2, aes(xintercept = SumStat, color = Cluster), linetype="dashed", size=1, show.legend = TRUE) 
  }
  g

}

#' Plot test-statistic for each time-bin in a time-series, highlight clusters.
#
#' Plot time_cluster_data, highlights clusters of above-threshold time-bins.
#'
#' @param x The output of \code{make_time_cluster_data}
#' @param type This function can plot the test-statistic ("statistic"), the parameter estimate +/-
#'   std. error ("estimate"), the p-value ("pvalue") or the negative-log-pvalue ("neg_log_pvalue").
#'   When test gives critical-statistic, default is to plot the test-statistic. Otherwise, default
#'   is to plot the estimate. For wilcox, only p-values can be plotted; for boot-splines, p-values 
#'   cannot be plotted.
#' @param ... Ignored
#'
#' @export
#' @return A ggplot object
plot.time_cluster_data <- function(x, type = NULL, ...) {
  plot(attr(x, "eyetrackingR")$time_bin_summary, type= type)
}
