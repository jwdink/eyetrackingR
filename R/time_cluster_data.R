#' Bootstrap analysis of time-clusters.
#'
#' Takes data whose time bins have been clustered by significance (using the \code{make_time_cluster_data}
#' function) and performs a bootstrapping analyses (Maris & Oostenveld, 2007). This analysis takes a summed
#' statistic for each cluster, and compares it to the "null" distribution of sum statistics obtained by
#' resampling data within the largest of the clusters.
#'
#' @param data          The output of the \code{make_time_cluster_data} function
#' @param within_subj   Logical indicating whether to perform within-subjects bootstrap resampling. (Defaults
#'   to FALSE: between-subjects resampling)
#' @param samples       How many iterations should be performed in the bootstrap resampling procedure?
#' @param formula       Formula for test. Should be identical to that passed to make_time_cluster_data fxn (if
#'   arg ignored there, can be ignored here)
#' @param shuffle_by  If the predictor_column is numeric *and* within-subjects, then observations with the
#'   same predictor value could nevertheless correspond to distinct conditions/categories that should be
#'   shuffled separately. For example, when using vocabulary scores to predict looking behavior, a participant
#'   might get identical vocab scores for verbs and nouns; these are nevertheless distinct categories that
#'   should be re-assigned separately when bootstrap-resampling data. The 'shuffle_by' argument allows you to
#'   specify a column which indicates these kinds of distinct categories that should be resampled separately--
#'   but it's only needed if you've specified a numeric *and* within-subjects predictor column.
#' @param ...            Other args for to selected 'test' function; should be identical to those passed to
#'   \code{make_time_cluster_data} function
#' @return A cluster-analysis object, which can be plotted and summarized to examine which temporal periods
#'   show a significant effect of the predictor variable
analyze_time_clusters <-function(data, data_options, ...) {
  UseMethod("analyze_time_clusters")
}
analyze_time_clusters.time_cluster_data <-function(data,
                                                   within_subj = FALSE,
                                                   samples = 1000,
                                                   formula = NULL,
                                                   shuffle_by = NULL,
                                                   ...) {

  # Get important information about type of data/analyses, input when running make_time_cluster_data
  attrs <-attr(data, "eyetrackingR")
  data_options <-attrs$data_options

  if (is.null(shuffle_by)) {
    shuffle_by <-attrs$predictor_column
    if ( attrs$test %in% c("lm", "lmer") & is.numeric(data[[attrs$predictor_column]]) & within_subj) {
      stop("If predictor column is numeric and within-subjects, you should specify a 'shuffle_by' argument. ",
           "See the documentation for more details.")
    }
  }


  # Arg check:
  if (is.null(formula)) {
    formula <-attrs$formula
  } else {
    if (attrs$formula != formula) stop("Formula given in 'make_time_cluster_data' does not match formula given here.")
  }
  if (attrs$test %in% c("t.test", "wilcox.test")) {
    paired = list(...)[['paired']]
    if (within_subj==TRUE) {
      # if within_subj is true, we need to confirm they overrode default
      if (!identical(paired, TRUE)) stop("For ", attrs$test, ", if 'within_subj' is TRUE, then 'paired' should also be TRUE.")
    } else {
      # if within_subj is false, we just need to confirm they didn't set paired = TRUE
      if (identical(paired, TRUE)) stop("For ", attrs$test, ", if 'within_subj' is FALSE, then 'paired' should also be FALSE.")
    }
  } else if (attrs$test == "lm") {
    if (within_subj == TRUE) stop("For lm, 'within_subj' must be FALSE.")
  }

  # get data for biggest cluster
  df_biggclust <-data[!is.na(data[[attrs$predictor_column]]),]
  if (sign(attrs$threshold)==1) {
    df_biggclust <- filter(df_biggclust, Cluster == which.max(attrs$clusters["SumStat",]))
  } else {
    df_biggclust <- filter(df_biggclust, Cluster == which.min(attrs$clusters["SumStat",]))
  }

  # Resample this data and get sum statistic each time, creating null distribution
  if (within_subj) {

    # Within-Subjects

    participants <-unique(df_biggclust[[data_options$participant_column]])

    # get a list of list of rows. outer list corresponds to participants, inner to conditions
    list_of_list_of_rows <-lapply(X = participants, FUN = function(ppt) {
      ppt_logical <-(df_biggclust[[data_options$participant_column]] == ppt)

      # for each participant, get the rows for each of level of the shuffle_by col
      this_ppt_levels <-unique(df_biggclust[[shuffle_by]][ppt_logical])
      out <- lapply(X = this_ppt_levels, FUN = function(lev) {
        which(ppt_logical & df_biggclust[[shuffle_by]] == lev)
      })
      names(out) <- this_ppt_levels
      return(out)
    })

    null_distribution <-pbsapply(1:samples, FUN = function(iter) {
      df_resampled <- df_biggclust

      # for each participant, randomly resample rows to be assigned to each possible level of the predictor
      # TO DO: keep this in mind as a performance bottleneck. if so, refactor code so that df_resampled only
      # gets reassigned once per condition across all participants (rather than once per condition per participant)
      for (list_of_rows in list_of_list_of_rows) {
        resampled <-sample(x = list_of_rows, size = length(list_of_rows), replace = FALSE)
        for (i in seq_along(resampled)) {
          rows_orig <-list_of_rows[[i]]
          rows_new <-resampled[[i]]
          df_resampled[rows_new,attrs$predictor_column] <- df_biggclust[first(rows_orig),attrs$predictor_column]
        }
      }

      # this gives a dataframe where the "condition" label has been resampled within each participant
      # run analyze time bins on it to get sum statistic for cluster
      time_bin_summary_resampled <-analyze_time_bins(df_resampled, data_options,
                                                     predictor_column = attrs$predictor_column,
                                                     test = attrs$test,
                                                     threshold = attrs$threshold,
                                                     alpha = attrs$alpha,
                                                     formula = formula,
                                                     return_model = FALSE,
                                                     quiet = TRUE,
                                                     ...)

      return( sum(time_bin_summary_resampled$Statistic, na.rm=TRUE) )
    })


  } else {

    # Between Subjects

    # get rows for each participant
    participants <-unique(df_biggclust[[data_options$participant_column]])
    rows_of_participants <- lapply(participants, FUN = function(ppt) which(df_biggclust[[data_options$participant_column]] == ppt))

    null_distribution <- pbsapply(1:samples, FUN = function(iter) {
      df_resampled <- df_biggclust

      # randomly re-assign each participant to a condition:
      rows_of_participants_resampled <- sample(rows_of_participants, size=length(rows_of_participants), replace=FALSE)
      for (i in seq_along(rows_of_participants_resampled)) {
        # TO DO: keep this in mind as a performance bottleneck. if so, refactor code so that df_resampled only
        # gets reassigned once per condition across all participants (rather than once per participant)
        df_resampled[rows_of_participants_resampled[[i]], attrs$predictor_column] <-
          df_biggclust[first(rows_of_participants[[i]]),attrs$predictor_column]
      }

      # this gives a dataframe where the "condition" label has been resampled for participants
      # run analyze time bins on it to get sum statistic for cluster
      time_bin_summary_resampled <-analyze_time_bins(df_resampled, data_options,
                                                     predictor_column = attrs$predictor_column,
                                                     test = attrs$test,
                                                     threshold = attrs$threshold,
                                                     alpha = NULL,
                                                     formula = formula,
                                                     return_model = FALSE,
                                                     quiet = TRUE,
                                                     ...)

      return( sum(time_bin_summary_resampled$Statistic, na.rm=TRUE) )

    })

  }

  # Get p-values:
  out <-c(list(null_distribution = null_distribution), attr(data, "eyetrackingR"))
  probs <-sapply(out$clusters["SumStat",],
                 FUN= function(ss) ifelse(sign(out$threshold)==1,
                   mean(ss<out$null_distribution, na.rm=TRUE),
                   mean(ss>out$null_distribution, na.rm=TRUE)
                 )
  )
  out$clusters <-rbind(out$clusters, matrix(probs, nrow = 1))
  row.names(out$clusters)[nrow(out$clusters)] <- "Prob"

  #
  class(out)  <- "cluster_analysis"
  return(out)

}

summary.cluster_analysis <-print.cluster_analysis <- function(cl_analysis) {
  clusters <- cl_analysis$clusters
  cat(
    "Test Type:\t", cl_analysis$test,
    "\nPredictor:\t", cl_analysis$predictor_column,
    "\nFormula:\t", Reduce(paste, deparse(cl_analysis$formula)),
    "\nNull Distribution =====",
    "\n\tMean:\t", round(mean(cl_analysis$null_distribution, na.rm=TRUE), digits = 5),
    "\n\tSD:\t", round(sd(cl_analysis$null_distribution, na.rm=TRUE), digits = 5),
    paste(
      "\nCluster", clusters["Cluster",], " =====",
      "\n\tTime:\t\t", clusters["StartTime",], "-", clusters["EndTime",],
      "\n\tSum Statistic:\t", round(clusters["SumStat",], digits = 5),
      "\n\tProbability:\t", round(clusters["Prob",], digits = 5)
    )
  )
  invisible(cl_analysis)
}

#' Make data for cluster analysis.
#'
#' Takes data that has been summarized into time-bins by \code{make_time_sequence_data()}, finds adjacent time
#' bins that pass some threshold of significance, and assigns these adjacent bins into groups (clusters).
#' Output is ready for a bootstrapping analyses (Maris & Oostenveld, 2007)
#'
#' @param data   The output of the \code{make_time_sequence_data} function
#' @param predictor_column  The predictor variable whose test statistic you are interested in
#' @param aoi               If this dataframe has multiple AOIs, you must specify which to analyze
#' @param test              What type of test should be performed in each time bin? Supports \code{t.test},
#'   \code{wilcox}, \code{lm}, or \code{lmer}.
#' @param threshold         Value of statistic used in determining significance. Note the sign! This test is
#'  directional.
#' @param alpha             Alpha value for determining significance, ignored if threshold is given
#' @param formula           What formula should be used for test? Optional (for all but \code{lmer}), if unset
#'   uses \code{Prop ~ [predictor_column]}
#' @param ...               Any other arguments to be passed to the selected 'test' function (e.g., paired,
#'   var.equal, etc.)
#'
#' @return dataframe
make_time_cluster_data <-function(data, ...) {
  UseMethod("make_time_cluster_data")
}
make_time_cluster_data.time_sequence_data <- function(data, data_options,
                                                     predictor_column,
                                                     aoi = NULL,
                                                     test,
                                                     threshold = NULL,
                                                     formula = NULL,
                                                     ...) {
  ## Helper:
  .label_clusters <- function(vec) {
    vec = c(0,vec)
    vec[is.na(vec)] = 0
    out = c(cumsum(diff(vec)==1))
    vec = vec[-1]
    out[!vec] = NA
    out
  }

  data_options <- attr(data, "eyetrackingR")$data_options

  # Check Arg:
  if (is.null(threshold)) stop("This function requires a 'threshold' for each test. ",
                               "Since this method is directional, take care when choosing the sign of the threshold.")

  # Filter Data:
  if (is.null(aoi)) {
    if (length(unique(data$AOI)) == 1) {
      aoi <- unique(data$AOI)
      data <- filter(data, AOI == aoi)
    } else {
      stop("Please specify the AOI of interest.")
    }
  } else {
    data <- filter(data, AOI == aoi)
    if (nrow(data)==0) stop("AOI not found in data.")
  }

  # Compute Time Bins:
  time_bin_summary <- analyze_time_bins(data, data_options,
                                       predictor_column = predictor_column,
                                       test = test,
                                       threshold = threshold,
                                       alpha = NULL,
                                       formula = formula,
                                       return_model = FALSE,
                                       ...)
  formula <- attr(time_bin_summary, "eyetrackingR")$formula

  # Label Adjacent Clusters:
  if (sign(threshold)==1) {
    time_bin_summary$CritStatistic <- time_bin_summary$CritStatisticPos
    time_bin_summary$CritStatisticNeg <- time_bin_summary$CritStatistic
    time_bin_summary$Significant <- time_bin_summary$Statistic > time_bin_summary$CritStatistic
  } else {
    time_bin_summary$CritStatistic <- time_bin_summary$CritStatisticNeg
    time_bin_summary$CritStatisticPos <- time_bin_summary$CritStatistic
    time_bin_summary$Significant <- time_bin_summary$Statistic < time_bin_summary$CritStatistic
  }
  time_bin_summary$Cluster = .label_clusters(time_bin_summary$Significant)

  # Compute Sum Statistic for each Cluster
  sum_stat <- c()
  for (clust in na.omit(unique(time_bin_summary$Cluster))) {
    sum_stat[clust] = sum(time_bin_summary$Statistic[which(time_bin_summary$Cluster==clust)])
  }

  # Merge cluster info into original data
  df_timeclust <- left_join(data, time_bin_summary[,c('Time','AOI','Cluster')], by=c('Time','AOI'))

  # Collect info about each cluster:
  start_times <- sapply(seq_along(sum_stat),
                       FUN = function(clust) time_bin_summary$Time[first(which(time_bin_summary$Cluster==clust))]
  )
  end_times <- sapply(seq_along(sum_stat),
                     FUN = function(clust) time_bin_summary$Time[last(which(time_bin_summary$Cluster==clust))]
  )
  attrs <- attr(df_timeclust, "eyetrackingR")
  clusters <- sapply(seq_along(sum_stat), function(i) {
    c(Cluster = i, SumStat = sum_stat[i], StartTime = start_times[i], EndTime = end_times[i]+attrs$time_bin_size)
  })

  # Output data, add attributes w/ relevant info
  df_timeclust <- as.data.frame(df_timeclust)
  class(df_timeclust) <-c("time_cluster_data", "time_sequence_data", class(df_timeclust))
  attr(df_timeclust, "eyetrackingR") <- c(attrs,
                                         list(clusters = clusters,
                                              predictor_column = predictor_column,
                                              test = test,
                                              threshold = threshold,
                                              formula = formula,
                                              time_bin_summary = time_bin_summary)
  )
  df_timeclust

}

summary.time_cluster_data <- function(data) {
  clusters <- attr(data, "eyetrackingR")$clusters
  if (length(clusters)==0) {
    cat(
      "Test Type:\t", attr(data, "eyetrackingR")$test,
      "\nPredictor:\t", attr(data, "eyetrackingR")$predictor_column,
      "\nFormula:\t", Reduce(paste, deparse(attr(data, "eyetrackingR")$formula)),
      "\nNo Clusters"
    )
  } else {
    cat(
      "Test Type:\t", attr(data, "eyetrackingR")$test,
      "\nPredictor:\t", attr(data, "eyetrackingR")$predictor_column,
      "\nFormula:\t", Reduce(paste, deparse(attr(data, "eyetrackingR")$formula)),
      paste(
        "\nCluster", clusters["Cluster",], " =====",
        "\n\tTime:\t\t", clusters["StartTime",], "-", clusters["EndTime",],
        "\n\tSum Statistic:\t", round(clusters["SumStat",], digits = 5)
      )
    )
  }
  invisible(data)
}


#' Visualize the results of a cluster analysis.
#'
#' Plots the result of the bootstrapping cluster analysis. A histogram of the sum statistics for the
#' shuffled (null) distribution, with the sum statisics for each of the clusters indicated by dashed lines.
#'
#' @param cluster_analysis object returned by cluster_analysis()
#'
#' @return A ggplot object
plot.cluster_analysis <- function(cl_analysis) {
  dat <- c(cl_analysis$clusters['SumStat',], cl_analysis$null_distribution)
  x_min <- min(dat, na.rm=TRUE) - sd(dat)
  x_max <- max(dat, na.rm=TRUE) + sd(dat)
  cluster_names <- as.factor(cl_analysis$clusters['Cluster',])
  df_plot1 <- data.frame(NullDistribution = cl_analysis$null_distribution)
  df_plot2 <- data.frame(SumStat = cl_analysis$clusters['SumStat',],
                         Cluster = as.factor(cl_analysis$clusters['Cluster',]))
  ggplot(data = df_plot1, aes(x = NullDistribution)) +
    geom_density() +
    geom_histogram(aes(y=..density..), binwidth = sd(cl_analysis$null_distribution)/5, alpha=.75 ) +
    coord_cartesian(xlim = c(x_min, x_max)) +
    geom_vline(data = df_plot2, aes(xintercept = SumStat, color = Cluster), linetype="dashed", size=1, show_guide = TRUE) +
    xlab(paste("Distribution of summed statistics from", cl_analysis$test )) + ylab("Density")

}

#' Plot test-statistic for each time-bin in a time-series, highlight clusters.
#
#' Plot time_cluster_data, highlights clusters of above-threshold time-bins.
#'
#' @param data
#' @param clusters A vector of the clusters you'd like highlighted. If left blank, all are highlighted
#'
#' @return A ggplot object
plot.time_cluster_data <- function(data, clusters = NULL) {
  attrs <- attr(data, "eyetrackingR")
  g <- plot(attrs$time_bin_summary)

  if (is.null(clusters)) clusters <- unique(na.omit(attrs$time_bin_summary$Cluster))

  if (any(!is.na(attrs$time_bin_summary$Cluster))) {
    # Make data for shaded region
    time_vec <- seq(from = min(attrs$time_bin_summary$Time, na.rm=TRUE),
                   to = max(attrs$time_bin_summary$Time, na.rm=TRUE),
                   length.out = length(attrs$time_bin_summary$Time)*50)
    cluster_vec <- sapply(X = time_vec, function(x) with(attrs$time_bin_summary[!is.na(attrs$time_bin_summary$Cluster),], Cluster[which.min(abs(x - Time))]))
    stat_vec <- with(attrs$time_bin_summary, approxfun(x = Time, y = Statistic)(time_vec))
    if (sign(attrs$threshold)==1) {
      crit_stat_vec <- with(attrs$time_bin_summary, approxfun(x = Time, y = CritStatisticPos)(time_vec))
      ribbon_max <- ifelse(test = stat_vec>crit_stat_vec & cluster_vec %in% clusters, stat_vec, NA)
      ribbon_min <- ifelse(test = stat_vec>crit_stat_vec & cluster_vec %in% clusters, crit_stat_vec, NA)
    } else {
      crit_stat_vec <- with(attrs$time_bin_summary, approxfun(x = Time, y = CritStatisticNeg)(time_vec))
      ribbon_min <- ifelse(test = stat_vec<crit_stat_vec & cluster_vec %in% clusters, stat_vec, NA)
      ribbon_max <- ifelse(test = stat_vec<crit_stat_vec & cluster_vec %in% clusters, crit_stat_vec, NA)
    }

    ribbons <- data.frame(
      Time = time_vec,
      ribbon_max = ribbon_max,
      ribbon_min = ribbon_min
    )

    # add to plot:
    g <- g + geom_ribbon(data = ribbons, aes(x= Time, ymin= ribbon_min, ymax= ribbon_max), fill= "gray", alpha= .75, colour= NA)

  }
  g
}
