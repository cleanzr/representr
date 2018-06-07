#' Create a representative dataset post record-linkage.
#'
#' @param data A data frame of records to be represented.
#' @param linkage A numeric vector indication the cluster ids post-record linkage for each record in \code{data}.
#' @param method Which method to use for representation. Valid options include "proto_minimax", "proto_random",
#'     and "composite".
#' @param parallel Logical flag if to use parallel computation or not (via \code{foreach}).
#' @param cores If specified, the number of cores to use with \code{foreach}.
#' @param ... Additional parameters sent to cluster representation function. See
#'     \link[=clust_proto_minimax]{prototyping} or \link[=clust_composite]{composite} methods.
#'
#' @examples
#'
#' data("rl_reg1")
#' rep_dat_random <- represent(rl_reg1, identity.rl_reg1, "proto_random", id = FALSE)
#' head(rep_dat_random)
#'
#' @export
represent <- function(data, linkage, method, parallel = TRUE, cores = NULL, ...) {
  ## error handling
  if(!("data.frame" %in% class(data)))
    stop("data must be a data frame.")
  if(length(linkage) != nrow(data))
    stop("linkage must have one entry for every record.")
  if(!is.numeric(linkage))
    stop("linkage must be numeric.")
  if(!(method %in% c("proto_minimax", "proto_random", "composite")))
    stop("Valid options for method include 'proto_minimax', 'proto_random', and 'composite'.")

  ## TODO:
  ## check that ... options match method chosen
  ## don't forget to add scale option for minimax
  ## add examples for not random

  ## apply representative method to each cluster
  clusters <- split(data, linkage)
  k <- length(clusters)

  rep_fun <- switch(method,
                    "proto_minimax" = clust_proto_minimax,
                    "proto_random" = clust_proto_random,
                    "composite" = clust_composite)

  `%doit%` <- ifelse(parallel, foreach::`%dopar%`, foreach::`%do%`)

  if(parallel) doParallel::registerDoParallel(cores = cores)

  ##register i so that check won't complain
  i <- NULL

  rep_dat <- foreach::foreach(i = 1:k, .combine = rbind) %doit%
    rep_fun(clusters[[i]], ...)

  if(parallel) doParallel::stopImplicitCluster()

  return(rep_dat)

}
