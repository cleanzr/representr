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

#' @param not_cluster A data frame of the records outside the cluster
#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#' @importFrom utils combn
#'
#' @rdname clust_proto_random
#'
#' @examples
#'
#'not_clusters <- lapply(seq_along(clusters), function(x){
#' if(nrow(clusters[[x]]) > 1)
#'   do.call(rbind, clusters[-x])
#' })
#' clust_proto_minimax(clusters[[1]], not_clusters[[1]], dist_binary)
#'
#' @export
clust_proto_minimax <- function(cluster, not_cluster, distance, id = TRUE, ...) {
  if(any(is.na(as.numeric(rownames(cluster)))) & id)
    stop("If returning id, must have original row numbers as rownames.")

  ## check that ... options match method chosen
  args <- list(...)
  arg_names <- names(args)

  if("ties_fn" %in% arg_names) {
    ties_fn <- ifelse(class(args[["ties_fn"]]) == "character", get(args[["ties_fn"]]), args[["ties_fn"]])
    args <- args[-which(arg_names == "ties_fn")]
    arg_names <- names(args)
  } else {
    ties_fn <- random_compare
  }
  n <- nrow(cluster)

  # if cluster is of size 1, nothing to do
  if(n == 1) {
    if(!id) {
      return(cluster)
    } else {
      return(as.numeric(rownames(cluster)[1]))
    }
  } else if(n == 2) {
    ## always will be a tie
    idx <- do.call(handle_ties, c(list(ties = cluster, not_cluster = not_cluster, distance = distance, ties_fn = ties_fn), args))
  } else {
    # if cluster is of size > 2, need to compare records
    compare <- do.call(distance_compare, c(list(recs1 = cluster, recs2 = cluster, compare_idx = data.frame(t(combn(n, 2))), distance = distance), args))

    # select the record whose farthest neighbor within the cluster is closest
    idx <- which.min(unlist(lapply(seq_len(nrow(cluster)), function(row_id) { max(unlist(c(compare[compare[, 1] == row_id, "dist"], compare[compare[, 2] == row_id, "dist"]))) })))

    # in case of a tie, randomly break unless exactly equal
    # or if compare ties function is specified
    # if rows are exactly equal, pick the first one
    if(length(idx) > 1) {
      all_idx <- idx
      idx <- all_idx[do.call(handle_ties, c(list(ties = cluster[idx,], not_cluster = not_cluster, distance = distance, ties_fn = ties_fn), args))]
    }
  }

  if(!id) {
    cluster[idx, ]
  } else {
    as.numeric(rownames(cluster)[idx])
  }
}

#' @param ties A data frame of the records that are tied
#' @param not_cluster A data frame of the records outside the cluster
#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#' @rdname clust_proto_random
maxmin_compare <- function(ties, not_cluster, distance, ...) {
  n <- nrow(ties)
  m <- nrow(not_cluster)

  compare <- distance_compare(ties, not_cluster, data.frame(expand.grid(seq_len(n), seq_len(m))), distance, ...)

  # select the record whose closest non-neighbor is farthest
  which.max(unlist(lapply(seq_len(nrow(ties)), function(row_id) {min(compare[compare[, 1] == row_id,"dist"])})))
}

#' @param ties A data frame of the records that are tied
#' @param not_cluster A data frame of the records outside the cluster
#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#'
#' @rdname clust_proto_random
within_category_compare <- function(ties, not_cluster, distance, ...) {
  args <- list(...)
  col_type <- args[["col_type"]]

  # for each tied record, compare to non cluster records that match categorical values
  # get the record with min max dist within category to non cluster records
  which.min(unlist({
    lapply(seq_len(nrow(ties)), function(i) {
      rec <- ties[i,]
      # get categorical values
      not_clust_cats <- not_cluster[, col_type %in% c("categorical", "ordinal")]
      rec_cats <- rec[, col_type %in% c("categorical", "ordinal"),]

      # records not in the cluster that have categorical matches
      not_cluster_cat_match <- not_cluster[rowMeans(not_clust_cats == rec_cats[rep(1, nrow(not_clust_cats)),]) == 1,]

      compare <- do.call(distance_compare,
                         c(list(recs1 = rec, recs2 = not_cluster_cat_match, compare_idx = data.frame(expand.grid(seq_len(1), seq_len(nrow(not_cluster_cat_match)))), distance = distance), args))
      # compare <- distance_compare(rec, not_cluster_cat_match, data.frame(expand.grid(seq_len(1), seq_len(nrow(not_cluster_cat_match)))), distance, col_type = col_type, weights = weights, orders = orders)

      # if there were no matches in category, return infinity to that record isn't chosen
      ifelse(nrow(compare) > 0, max(compare[,"dist"]), Inf)
    })
  }))
}

#' @param ties A data frame of the records that are tied
#' @param not_cluster A data frame of the records outside the cluster
#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#' @rdname clust_proto_random
random_compare <- function(ties, not_cluster, distance, ...) {
  n <- nrow(ties)
  sample(seq_len(n), 1)
}

#' handle_ties
#' Internal function to handle ties in a consistent way.
#'
#' @param ties A data frame of the records that are tied
#' @param not_cluster A data frame of the records outside the cluster
#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#' @noRd
handle_ties <- function(ties, not_cluster, distance, ties_fn, ...) {
  if(nrow(unique(ties)) == 1) {
    ## identical rows
    idx <- which.min(as.numeric(rownames(ties)))
  } else {
    idx <- ties_fn(ties = ties, not_cluster = not_cluster, distance = distance, ...)
  }
  ## if there are still ties
  if(length(idx) > 1) idx <- sample(idx, 1)
}

#' distance_compare
#' Internal convenience function to compute distances for specified records.
#'
#' @param rec1 A data frame of records to compare to recs2
#' @param recs2 A data frame of records to compare to recs1
#' @param compare_idx A data frame with two columns containing indices of recs1 and recs2 to compare, respectively
#' @param distance A distance function for comparing records
#' @param ... Additional arguments passed to the comparison function
#'
#' @noRd
distance_compare <- function(recs1, recs2, compare_idx, distance, ...) {
  args <- list(...)
  compare_idx$dist <- apply(compare_idx, 1, function(comp) {
    do.call(distance, c(list(a = recs1[comp[1],], b = recs2[comp[2],]), args))
    # distance(recs1[comp[1],], recs2[comp[2],], col_type = col_type, weights = weights, orders = orders)
  })
  compare_idx
}
