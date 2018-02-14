#' eyetrackingR: A package for cleaning, analyzing, and visualizing eye-tracking datasets
#'
#' This package addresses tasks along the pipeline from raw eye-tracking data to analysis and
#' visualization. It offers several popular types of analyses, including linear and
#' growth curve time analyses, onset-contingent reaction time analyses, and cluster mass
#' analyses, as well as novel non-parametric approaches to time-series data.
#'
#' For more information and tutorials, visit \href{http://www.eyetrackingr.com}{eyetrackingR.com}.
#'
#' @importFrom lazyeval interp
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr  gather
#' @importFrom tidyr  spread
#' @importFrom stats  plogis
#' @importFrom stats  qlogis
#' @importFrom stats  rnorm
#' @importFrom stats  qgamma
#' @importFrom stats  rbinom
#' @importFrom stats  rexp
#' @importFrom stats  rpois
#' @importFrom stats  as.formula
#' @importFrom stats  formula
#' @importFrom stats  median
#' @importFrom stats  na.omit
#' @importFrom stats  poly
#' @importFrom stats  predict
#' @importFrom stats  qnorm
#' @importFrom stats  qsignrank
#' @importFrom stats  qt
#' @importFrom stats  quantile
#' @importFrom stats  sd
#' @importFrom stats  rgamma
#' @importFrom stats  df.residual
#' @importFrom stats  p.adjust
#' @importFrom tidyr  gather
#' @importFrom tidyr  spread
#' @importFrom graphics plot
#' @importFrom purrr map map_lgl
#'
#' @docType package
#' @name eyetrackingR
NULL
#> NULL

# Suppress unbound global variable NOTES in R CMD CHECK:
utils::globalVariables(c('Prop',
                         'SumTracklossForParticipant',
                         'TotalParticipantLength',
                         ':=',
                         'AOI',
                         'Time',
                         'SamplesInAOI',
                         'SamplesTotal',
                         'PairedObs',
                         '.TrialID',
                         '.Target',
                         '.ClosestTime',
                         '.Distractor',
                         'WhichAOI',
                         'FirstAOI',
                         'N',
                         'Statistic',
                         'CritStatisticPos',
                         'CritStatisticNeg',
                         'Estimate',
                         'StdErr',
                         'MeanDiff',
                         'CI_high',
                         'CI_low',
                         'NullDistribution',
                         '..density..',
                         'SumStat',
                         'Cluster',
                         'SwitchAOI',
                         'Min',
                         'Max',
                         "term",
                         ".",
                         "SE",
                         "Significant",
                         "Start",
                         "Stop",
                         ".Time",
                         'FirstSwitch',
                         '.Predicted',
                         '.WindowEnd',
                         '.WindowStart',
                         'TotalTrialLength',
                         'SumTracklossForTrial',
                         'TracklossForTrial',
                         'TracklossForParticipant',
                         '.SpeedOffset',
                         'RT',
                         '.PrefOnset',
                         '.NumSwitches',
                         'AOI1',
                         'Condition',
                         'NegativeRuns', 
                         'ParticipantLogOdds',
                         'ItemLogOdds',
                         'Item',
                         'Participant',
                         'PositiveRuns',
                         'Trial',
                         'Lvl1',
                         'Lvl2',
                         'Prob',
                         'Val'))