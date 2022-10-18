#' Get posterior weights for each record post record-linkage using posterior prototyping.
#'
#' @param data A data frame of records to be represented.
#' @param posterior_linkage A matrix of size m x n, indicating the posterior cluster ids post-record linkage,
#'     each row represents the cluster assignment for each record in \code{data} for 1 iteration of the sampler.
#' @param rep_method Which method to use for representation. Valid options include "proto_minimax" and "proto_random".
#' @param parallel Logical flag if to use parallel computation or not (via \code{foreach}).
#' @param cores If specified, the number of cores to use with \code{foreach}.
#' @param ... Additional parameters sent to cluster representation function. See
#'     \link[=clust_proto_minimax]{minimax} or \link[=clust_proto_random]{random} methods. If passing a probability to
#'     the random method, must be list of the same length as the number of iterations in lambda and within each must be
#'     a list of the same length as the number of clusters. Within each should be a vector of probabilities, the same length
#'     as the number of rows in the cluster \code{prob[[iteration][[cluster]]}.
#' @param scale If "proto_minimax" method is specified, logical flag to indicate if the column-type
#'     distance function should be scaled so that each distance takes value in [0, 1]. Defaults to
#'     FALSE.
#' @param verbose Flag for progress messages.
#' @param save_loc Location to save intermediate progress. If NULL, no intermediate progress is saved.
#' @importFrom utils write.csv
#' @examples
#'
#' data(rl_reg1)
#'
#' # make a fake posterior distribution for the linkage
#' m <- 10
#' n <- nrow(rl_reg1)
#' post_link <- matrix(sample(seq_len(n), n*m, replace = TRUE), nrow = m)
#'
#' # get the posterior prototyping weights
#' col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal",
#'     "numeric", "numeric")
#' orders <- list(education = c("Less than a high school diploma", "High school graduates, no college",
#'     "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))
#' weights <- c(.25, .25, .05, .05, .1, .15, .05, .05, .05)
#'
#' \donttest{
#' pp_weight <- pp_weights(rl_reg1, post_link, "proto_minimax", distance = dist_col_type,
#'     col_type = col_type, weights = weights, orders = orders, scale = TRUE, parallel = FALSE)
#'
#' # threshold by posterior prototyping weights
#' head(rl_reg1[pp_weight > 0.5, ])
#' }
#'
#' @export
pp_weights <- function(data, posterior_linkage, rep_method, parallel = TRUE, cores = NULL, ..., scale = FALSE, save_loc = NULL, verbose = FALSE) {
  ## error handling
  if(!inherits(data, "data.frame"))
    stop("data must be a data frame.")
  if(ncol(posterior_linkage) != nrow(data))
    stop("posterior_linkage must have one column for every record.")
  if(!is.matrix(posterior_linkage))
    stop("posterior_linkage must be a matrix.")
  if(!(rep_method %in% c("proto_minimax", "proto_random")))
    stop("Valid options for rep_method include 'proto_minimax' and 'proto_random'.")

  ## check that ... options match method chosen
  args <- list(...)
  arg_names <- names(args)
  if(rep_method == "proto_minimax") {
    if(!("distance" %in% arg_names))
      stop("Must supply distance function for proto_minimax method. See help('clust_proto_minimax') for more options.")
    else if(!is.function(args[["distance"]]))
      stop("Must supply distance function for proto_minimax.")
  } else if(rep_method == "proto_random") {
    if("prob" %in% arg_names) {
      ## if probabilities are passed through
      ## must be a function to calculate prob list for each iteration
      if(!is.list(args[["prob"]])) {
        stop("prob must be a list of length equal to the number of iterations in lambda and within each must be a list of the same length as the number of clusters. Within each should be a vector of probabilities, the same length as the number of rows in the cluster.")
      } else {
        prob <- args[["prob"]]
        if(length(prob) != nrow(posterior_linkage)) {
          stop("prob must be a list of length equal to the number of iterations in lambda and within each must be a list of the same length as the number of clusters. Within each should be a vector of probabilities, the same length as the number of rows in the cluster.")
        }
        check_level_1 <- rep(NA, nrow(posterior_linkage))
        for(i in seq_len(nrow(posterior_linkage))) {
          # check that prob is the same length as the number of clusters within each iteration
          check_level_1[i] <- length(unique(posterior_linkage[i,])) != length(prob[[i]])

          # check that within each cluster we have the right number of probabilities
          check_level_2 <- table(posterior_linkage[i,]) != unlist(lapply(prob[[i]], length))
          if(sum(check_level_2) > 0) stop("prob must be a list of length equal to the number of iterations in lambda and within each must be a list of the same length as the number of clusters. Within each should be a vector of probabilities, the same length as the number of rows in the cluster.")
        }
        if(sum(check_level_1) > 0) stop("prob must be a list of length equal to the number of iterations in lambda and within each must be a list of the same length as the number of clusters. Within each should be a vector of probabilities, the same length as the number of rows in the cluster.")
      }
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

  # `%doit%` <- ifelse(
  `%doit%` <- foreach::`%do%`

  # if(parallel) doParallel::registerDoParallel(cores = cores)

  ## register i so that check won't complain
  i <- NULL

  m <- nrow(posterior_linkage)

  # posterior_rep <- foreach::foreach(i = 1:m, .combine = cbind) %doit% {
  #   if(rep_method == "proto_random" & "prob" %in% arg_names) args[["prob"]] <- prob[[i]]
  #   idx <- do.call("represent", c(list(data = data, linkage = posterior_linkage[i,], rep_method = rep_method, scale = scale, id = TRUE, parallel = parallel, cores = cores), args))
  #   seq_len(nrow(data)) %in% idx
  # }


  posterior_rep <- matrix(nrow = nrow(data), ncol = 0)

  for(i in seq_len(m)) {
    if(verbose & (i %% (m/10) == 0)) cat(paste0("iteration: ", i, " / ", m, "\n"))
    if(rep_method == "proto_random" & "prob" %in% arg_names) args[["prob"]] <- prob[[i]]

    idx <- do.call("represent", c(list(data = data, linkage = posterior_linkage[i,], rep_method = rep_method, scale = scale, id = TRUE, parallel = parallel, cores = cores), args))

    posterior_rep <- cbind(posterior_rep, seq_len(nrow(data)) %in% idx)
    if(!is.null(save_loc)) write.csv(posterior_rep, file = save_loc, row.names = FALSE)
  }


  # if(parallel) doParallel::stopImplicitCluster()

  rowSums(posterior_rep)/nrow(posterior_linkage)
}
