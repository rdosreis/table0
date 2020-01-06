#' @title Create data
#'
#' @description Extract filtered data from filt_df object
#'
#' @param filt_df list with the data and flist objetcs
#'
#' @return data data.frame after filtering
#'
#' @examples df <- data.frame(a = rnorm(100),
#'                           b = rnorm(100),
#'                           c = sample(letters[1:10], size = 100, replace = TRUE))
#'
#' lf <- list("a < 1.96",
#'            "b < 1.96",
#'            "c %in% letters[1:5] | abs(b) > 0.2")
#'
#' filt_df <- filter_list(data = df, lfilters = lf)
#' create_data(filt_df)
#'
#' @export

create_data <- function(filt_df){
  data <- filt_df$data
  return(data)
}

#' @title Create table zero
#'
#' @description Extract from filt_df object the number of study observations excluded with respect to each exclusion criteria
#'
#' @param filt_df list with the data and flist objetcs
#'
#' @return table0 data.frame with the number of study observations excluded with respect to each exclusion criteria
#'
#' @examples df <- data.frame(a = rnorm(100),
#'                           b = rnorm(100),
#'                           c = sample(letters[1:10], size = 100, replace = TRUE))
#'
#' lf <- list("a < 1.96",
#'            "b < 1.96",
#'            "c %in% letters[1:5] | abs(b) > 0.2")
#'
#' filt_df <- filter_list(data = df, lfilters = lf)
#' create_table0(filt_df)
#'
#' @export

create_table0 <- function(filt_df){
  aux <- lapply(filt_df$flist, nrow)
  fnames <- names(aux)
  fnames <- c("Origin", fnames)
  nvec <- unlist(aux)
  names(nvec) <- NULL
  nvec <- c(0, nvec)
  nsample <- filt_df$N - cumsum(nvec)
  table0 <- data.frame(Criteria = fnames, N_excluded = nvec, N_sample = nsample)
  return(table0)
}
