#' Create a representative dataset post record-linkage.
#'
#' @param data A data frame of records to be represented.
#' @param linkage A numeric vector indicating the cluster ids post-record linkage for each record in \code{data}.
#' @param rep_method Which method to use for representation. Valid options include "proto_minimax", "proto_random",
#'     and "composite".
#' @param parallel Logical flag if to use parallel computation or not (via \code{foreach}).
#' @param cores If specified, the number of cores to use with \code{foreach}.
#' @param ... Additional parameters sent to cluster representation function. See
#'     \link[=clust_proto_minimax]{prototyping} or \link[=clust_composite]{composite} methods.
#' @param scale If "proto_minimax" method is specified, logical flag to indicate if the column-type
#'     distance function should be scaled so that each distance takes value in [0, 1]. Defaults to
#'     FALSE.
#'
#' @examples
#'
#' data("rl_reg1")
#'
#' ## random prototyping
#' rep_dat_random <- represent(rl_reg1, identity.rl_reg1, "proto_random", id = FALSE, parallel = FALSE)
#' head(rep_dat_random)
#'
#' ## minimax prototyping
#' col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal",
#'     "numeric", "numeric")
#' orders <- list(education = c("Less than a high school diploma", "High school graduates, no college",
#'     "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))
#' weights <- c(.25, .25, .05, .05, .1, .15, .05, .05, .05)
#' rep_dat_minimax <- represent(rl_reg1, identity.rl_reg1, "proto_minimax", id = FALSE,
#'     distance = dist_col_type, col_type = col_type, weights = weights, orders = orders,
#'     scale = TRUE, parallel = FALSE)
#' head(rep_dat_minimax)
#'
#' ## composite prototyping
#' rep_dat_composite <- represent(rl_reg1, identity.rl_reg1, "composite",
#'                                col_type = col_type, parallel = FALSE)
#' head(rep_dat_composite)
#'
#' @importFrom stats complete.cases
#'
#' @export
represent <- function(data, linkage, rep_method, parallel = TRUE, cores = NULL, ..., scale = FALSE) {
  ## error handling
  if(!("data.frame" %in% class(data)))
    stop("data must be a data frame.")
  if(length(linkage) != nrow(data))
    stop("linkage must have one entry for every record.")
  if(!is.numeric(linkage))
    stop("linkage must be numeric.")
  if(!(rep_method %in% c("proto_minimax", "proto_random", "composite")))
    stop("Valid options for rep_method include 'proto_minimax', 'proto_random', and 'composite'.")
  if(sum(complete.cases(data)) != nrow(data))
    stop("representr has no support for missing values at this time.")

  ## check that ... options match method chosen
  args <- list(...)
  arg_names <- names(args)
  if(rep_method == "proto_minimax") {
    if(!("distance" %in% arg_names))
      stop("Must supply distance function for proto_minimax method. See help('clust_proto_minimax') for more options.")
    else if(class(args[["distance"]]) != "function")
      stop("Must supply distance function for proto_minimax.")
  } else if(rep_method == "composite") {
    if(!("col_type" %in% arg_names))
      stop("Must supply column types for composite method. See help('clust_composite') for more options.")
    if("weights" %in% arg_names) {
      ## must be list of length equal to number of clusters and total number of records
      weights <- args[["weights"]]
      if(length(weights) != length(unique(linkage))) stop("Weights must be list of length equal to number of clusters.")
      if(length(do.call(c, weights)) != nrow(data)) stop("Total number of Weights must equal total number of records.")
      if(class(do.call(c, weights)) != "numeric") stop("Weights must be numeric.")

      ## be sure everything sums to one
      weights <- lapply(weights, function(p) p/sum(p))

      ## remove prob from arg list
      args <- args[arg_names != "weights"]
    }
  } else if(rep_method == "proto_random") {
    if("prob" %in% arg_names) {
      ## must be list of length equal to number of clusters and total number of records
      prob <- args[["prob"]]
      if(length(prob) != length(unique(linkage))) stop("Probabilities must be list of length equal to number of clusters.")
      if(length(do.call(c, prob)) != nrow(data)) stop("Total number of probabilities must equal total number of records.")
      if(class(do.call(c, prob)) != "numeric") stop("Probabilities must be numeric.")

      ## be sure everything sums to one
      prob <- lapply(prob, function(p) p/sum(p))

      ## remove prob from arg list
      args <- args[arg_names != "prob"]
    }
  }

  ## scale option for minimax column-type distance
  if(rep_method == "proto_minimax" & scale) {
    # default weights
    if(!("weights" %in% arg_names)) args$weights <- rep(1/ncol(data), ncol(data))
    if(!("col_type" %in% arg_names))
      stop("If you wish to scale the column-type distance, provide a col_type vector. Otherwise, set scale = FALSE.")
    col_type <- args$col_type

    sca <- rep(1, ncol(data)) # need to keep things on the same scale
    if(sum(col_type == "numeric") > 1) {
      sca[col_type == "numeric"] <- 1/apply(data[, col_type == "numeric"], 2, function(col) diff(range(col)))
    } else if(sum(col_type == "numeric") == 1) {
      sca[col_type == "numeric"] <- 1/diff(range(data[, col_type == "numeric"]))
    }
    if(sum(col_type == "ordinal") > 1) {
      sca[col_type == "ordinal"] <- 1/(apply(data[, col_type == "ordinal"], 2, function(col) length(unique(col))) - 1)
    } else if(sum(col_type == "ordinal") == 1) {
      sca[col_type == "ordinal"] <- 1/(length(unique(data[, col_type == "ordinal"])) - 1)
    }
    args$weights <- args$weights*sca/sum(args$weights*sca)
  }

  ## apply representative method to each cluster
  clusters <- split(data, linkage)
  k <- length(clusters)

  ## make dummy prob in case not specified
  if(!("prob" %in% arg_names) & rep_method == "proto_random") prob <- lapply(seq_len(k), function(i) rep(1/nrow(clusters[[i]]), nrow(clusters[[i]])))
  if(!("weights" %in% arg_names) & rep_method == "composite") weights <- lapply(seq_len(k), function(i) rep(1/nrow(clusters[[i]]), nrow(clusters[[i]])))

  rep_fun <- switch(rep_method,
                    "proto_minimax" = clust_proto_minimax,
                    "proto_random" = clust_proto_random,
                    "composite" = clust_composite)

  `%doit%` <- ifelse(parallel, foreach::`%dopar%`, foreach::`%do%`)

  if(parallel) doParallel::registerDoParallel(cores = cores)

  ## register i so that check won't complain
  i <- NULL

  if(rep_method == "proto_random") {
    rep_dat <- foreach::foreach(i = 1:k, .combine = rbind) %doit%
      do.call("rep_fun", c(list(cluster = clusters[[i]], prob = prob[[i]]), args)) # complicated because args = ...
  } else if(rep_method == "composite"){
    rep_dat <- foreach::foreach(i = 1:k, .combine = rbind) %doit%
      do.call("rep_fun", c(list(cluster = clusters[[i]], weights = weights[[i]]), args)) # complicated because args = ...
  } else {
    rep_dat <- foreach::foreach(i = 1:k, .combine = rbind) %doit%
      do.call("rep_fun", c(list(cluster = clusters[[i]]), args)) # complicated because args = ...
  }

  if(parallel) doParallel::stopImplicitCluster()

  return(rep_dat)

}
