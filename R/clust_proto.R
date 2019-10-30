#' Prototype record from a cluster.
#' @param cluster A data frame of the clustered records.
#' @param prob A vector of length \code{nrow(cluster)} that sums to 1, giving the probability of selection.
#' @param id Logical indicator to return id of record selected (TRUE) or actual record (FALSE). Note,
#'     if returning id, must have original row numbers as rownames in each cluster.
#'
#' @return If \code{id = FALSE}, returns the prototype record from an individual cluster. Otherwise,
#'     returns the record id of the prototype record for that cluster. If there is a tie in the minimax
#'     prototype method, then random selection is used to break the tie.
#'
#' @examples
#' data("rl_reg1")
#'
#' clusters <- split(rl_reg1, identity.rl_reg1)
#' clust_proto_random(clusters[[1]])
#'
#' @export
clust_proto_random <- function(cluster, prob = rep(1/nrow(cluster), nrow(cluster)), id = TRUE) {
  # error handling
  if(any(is.na(as.numeric(rownames(cluster)))) & id)
    stop("If returning id, must have original row numbers as rownames.")
  if(length(prob) != nrow(cluster))
    stop("prob must be the same length as nrow(cluster).")
  if(class(prob) != "numeric")
    stop("prob must be a numeric vector.")
  if(round(sum(prob), 15) != 1)
    stop("prob must sum to 1.")

  n <- nrow(cluster)

  ## if cluster is of size 1, nothing to do
  if(n == 1) {
    if(!id) {
      return(cluster)
    } else {
      return(as.numeric(rownames(cluster)[1]))
    }
  }

  ## randomly sample row to return
  idx <- sample(seq_len(nrow(cluster)), 1, prob = prob)

  if(!id) {
    cluster[idx, ]
  } else {
    as.numeric(rownames(cluster)[idx])
  }
}

#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#' @importFrom utils combn
#'
#' @rdname clust_proto_random
#'
#' @examples
#'
#' clust_proto_minimax(clusters[[1]], dist_binary)
#'
#' @export
clust_proto_minimax <- function(cluster, distance, id = TRUE, ...) {
  if(any(is.na(as.numeric(rownames(cluster)))) & id)
    stop("If returning id, must have original row numbers as rownames.")

  n <- nrow(cluster)

  # if cluster is of size 1, nothing to do
  if(n == 1) {
    if(!id) {
      return(cluster)
    } else {
      return(as.numeric(rownames(cluster)[1]))
    }
  }

  # if cluster is of size > 1, need to compare records
  compare <- data.frame(t(combn(n, 2)))

  compare$dist <- apply(compare, 1, function(comp) {
    distance(cluster[comp[1],], cluster[comp[2],], ...)
  })

  # select the record whose farthest neighbor within the cluster is closest
  max_dist <- rep(NA, n)
  for(i in seq_len(n)) {
    max_dist[i] <- max(unlist(c(compare[compare[, 1] == i, "dist"], compare[compare[, 2] == i, "dist"])))
  }
  idx <- which(max_dist == min(max_dist))

  # in case of a tie, randomly break unless exactly equal
  # if rows are exactly equal, pick the first one
  if(length(idx) > 1) {
    if(nrow(unique(cluster[idx,])) == 1) {
      idx <- which.min(as.numeric(rownames(cluster)[idx]))
    } else {
      idx <- sample(idx, 1)
    }
  }

  if(!id) {
    cluster[idx, ]
  } else {
    as.numeric(rownames(cluster)[idx])
  }
}
