#' @title Filter list
#'
#' @description Filter a dataset given filters in a list
#'
#' @param data data.frame to apply filters
#' @param ... sequence of filters to apply
#'
#' @return flist list with rows excluded by each filter
#' @return data data.frame after filtering
#' @return N original sample size
#'
#' @examples df <- data.frame(a = rnorm(100),
#'                           b = rnorm(100),
#'                           c = sample(letters[1:10], size = 100, replace = TRUE))
#'
#' filt_df <- filter_list(data = df, a < 1.96,
#'                        b < 1.96,
#'                        c %in% letters[1:5] | abs(b) > 0.2)
#' lapply(filt_df$flist, nrow)
#'
#' @export

filter_list <- function(data, ...){
  if (missing(data)) stop("You must provide data!")

  lfilters <- as.list(substitute(...()))
  lfilters <- lapply(lfilters, deparse)

  n_origin <- nrow(data)
  data$row_number <- 1:nrow(data)

  flist <- lapply(X = lfilters, FUN = function(x) subset(x = data, subset = !eval(expr = parse(text = x))))
  names(flist) <- paste0("!(", unlist(lfilters), ")")

  rows_filt <- sapply(X = flist, FUN = "[[", "row_number")
  rows_filt <- do.call(args = rows_filt, what = "c")
  rows_filt <- unique(rows_filt)

  data <- subset(data, !(row_number %in% rows_filt))
  data$row_number <- NULL

  return(list(data = data, N = n_origin, flist = flist))
}
