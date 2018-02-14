# Filter
#' @export
filter.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
filter_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
filter_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
filter_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())

# mutate
#' @export
mutate.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
mutate_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
mutate_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
mutate_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())

# arrange
#' @export
arrange.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
arrange_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
arrange_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
arrange_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())

# group_by
#' @export
group_by.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
group_by_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
group_by_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
group_by_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
group_by_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())


# joins
# drawing a line in the sand: left and inner are OK, but others risk stripping too much
#' @export
left_join.eyetrackingR_df <- function(x, ...) reclass(x, NextMethod())
#' @export
inner_join.eyetrackingR_df <- function(x, ...) reclass(x, NextMethod())

# extract
#' @export
`[.eyetrackingR_df` <- function(x, ...) reclass(x, NextMethod())



