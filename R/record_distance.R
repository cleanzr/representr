#' The distance between two records
#' @param a Record a
#' @param b Record b
#' @return \code{binary_dist} returns a numeric value indicating how many discrepancies there are between two records.
#'
#' @examples
#' data("rl_reg1")
#' dist_binary(rl_reg1[1,], rl_reg1[2,])
#'
#' @export
dist_binary <- function(a, b) {
  ## error handling
  if(length(a) != length(b))
    stop("Records must be the same number of columns.")

  ## count number of times record fields don't match
  sum(a != b)
}

#' @param col_type A vector encoding the column type for each column in the dataset. Can take values
#'                 in "categorical", "ordinal", "string", or "numeric"
#' @param string_dist String distance function. Default is edit distance. Function must take at least
#'                    two arguments (strings)
#' @param weights A vector of weights for each column for making some column distances more important. Must sum to 1.
#'                Defaults to equal weights.
#' @param orders A named list containing the order of the levels in each ordinal column. Defaults to NULL,
#'               which corresponds to no ordinal variables.
#' @param ... Additional parameters passed to string distance function.
#'
#' @return \code{col_type_dist} return a numeric value of the weighted column type specific distance between two records.
#'
#' @rdname dist_binary
#'
#' @examples
#' type <- c("string", "string", "numeric", "numeric",
#'     "numeric", "categorical", "ordinal", "numeric", "numeric")
#' order <- list(education = c("Less than a high school diploma",
#'     "High school graduates, no college", "Some college or associate degree",
#'     "Bachelor's degree only", "Advanced degree"))
#'
#' dist_col_type(rl_reg1[1,], rl_reg1[2,], col_type = type, order = order)
#'
#' @importFrom utils adist
#' @export
dist_col_type <- function(a, b, col_type, string_dist = utils::adist, weights = rep(1/length(a), length(a)), orders = NULL, ...) {
  ## error handling
  if(length(a) != length(b))
    stop("Records must be the same number of columns.")
  if(length(col_type) != length(a))
    stop("Must have column type for each column.")
  if(!all(names(table(col_type)) %in% c("categorical", "ordinal", "string", "numeric")))
    stop("Column type must be 'categorical', 'ordinal', 'string', or 'numeric'")
  if(class(string_dist) != "function")
    stop("string_dist must be a function.")
  if(length(weights) != length(a)) {
    stop("Weights must be of same length as number of columns")
  } else if(!identical(round(sum(weights), 15), 1)) {
    stop("Weights must sum to 1.")
  }
  if("ordinal" %in% col_type) {
    if(sum(col_type == "ordinal") != length(orders)) stop("Please provide a named list containing the order of the levels in each ordinal column.")

    if(names(a)[col_type == "ordinal"] != names(orders)) stop("Orders must be a named list corresponding to the levels in each ordinal column.")

    for(idx in which(col_type == "ordinal")) {
      if(!all(c(a[,idx], b[,idx]) %in% orders[[names(a)[idx]]])) stop("Orders must be a named list corresponding to the levels in each ordinal column.")
    }
  }


  ## combine all column type distances
  p <- length(a)
  dist <- 0
  for(i in seq_len(p)) {
    if(col_type[i] == "numeric")
      # numeric: weighted absolute difference in values
      dist <- dist + weights[i]*abs(a[[i]] - b[[i]])
    else if(col_type[i] == "categorical")
      # categorical: weighted binary distance
      dist <- dist + weights[i]*(a[[i]] != b[[i]])
    else if(col_type[i] == "ordinal") {
      # ordinal: look up absolute level distance
      ord <- orders[[names(a)[i]]]
      dist <- dist + weights[i]*abs(which(ord == a[[i]]) - which(ord == b[[i]]))
    } else
      # string: weighted string distance
      dist <- dist + weights[i]*string_dist(as.character(a[[i]]), as.character(b[[i]]), ...)
  }
  return(dist)
}
