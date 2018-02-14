# Filter

#' @export
#' @method filter eyetrackingR_df
filter.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method filter_ eyetrackingR_df
filter_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method filter_at eyetrackingR_df
filter_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method filter_all eyetrackingR_df
filter_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method filter_if eyetrackingR_df
filter_if.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())

# mutate
#' @export
#' @method mutate eyetrackingR_df
mutate.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method mutate_ eyetrackingR_df
mutate_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method mutate_at eyetrackingR_df
mutate_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method mutate_all eyetrackingR_df
mutate_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method mutate_if eyetrackingR_df
mutate_if.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())

# arrange
#' @export
#' @method arrange eyetrackingR_df
arrange.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method arrange_ eyetrackingR_df
arrange_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method arrange_at eyetrackingR_df
arrange_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method arrange_all eyetrackingR_df
arrange_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method arrange_if eyetrackingR_df
arrange_if.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())

# group_by
#' @export
#' @method group_by eyetrackingR_df
group_by.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method group_by_ eyetrackingR_df
group_by_.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method group_by_at eyetrackingR_df
group_by_at.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method group_by_if eyetrackingR_df
group_by_if.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())
#' @export
#' @method group_by_all eyetrackingR_df
group_by_all.eyetrackingR_df <- function(.data, ...) reclass(.data, NextMethod())


# joins
# drawing a line in the sand: left and inner are OK, but others risk stripping too much
#' @export
#' @method left_join eyetrackingR_df
left_join.eyetrackingR_df <- function(x, ...) reclass(x, NextMethod())
#' @export
#' @method inner_join eyetrackingR_df
inner_join.eyetrackingR_df <- function(x, ...) reclass(x, NextMethod())

# extract
#' @export
`[.eyetrackingR_df` <- function(x, ...) reclass(x, NextMethod())



