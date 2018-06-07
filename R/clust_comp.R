#' Composite record from a cluster using a weighted average of each column values.
#' @param cluster A data frame of the clustered records
#' @param col_type A vector encoding the column type for each column in the dataset. Can take values
#'                 in "categorical", "ordinal", "string", or "numeric"
#' @param weights A vector of length equal to the number of records in the cluster indicating the
#'                weight for each. Defaults to equal weight.
#'
#' @return Returns the composite record from an individual cluster.
#'
#' #' @examples
#' data("rl_reg1")
#'
#' clusters <- split(rl_reg1, identity.rl_reg1)
#' type <- c("string", "string", "numeric", "numeric",
#'     "numeric", "categorical", "ordinal", "numeric", "numeric")
#'
#' clust_composite(clusters[[1]], type)
#'
#' @export
clust_composite <- function(cluster, col_type, weights = rep(1/nrow(cluster), nrow(cluster))) {

  n <- nrow(cluster)
  p <- ncol(cluster)

  if(!all(names(table(col_type)) %in% c("categorical", "ordinal", "string", "numeric")))
    stop("Column type must be 'categorical', 'ordinal', 'string', or 'numeric'")
  if(length(weights) != n)
    stop("Weights must be of same length as number of rows")
  if(!identical(round(sum(weights), 15), 1))
    stop("Weights must sum to 1.")


  # if cluster is of size 1, nothing to do
  if(n == 1) return(cluster)

  avg_cluster <- cluster[1,]

  for(i in seq_len(p)) {
    if(col_type[i] == "numeric")
      # numeric: weighted avg values
      avg_cluster[i] <- sum(weights*cluster[, i])/sum(weights)
    else if(col_type[i] %in% c("categorical", "ordinal")) {
      # categorical | ordinal: weighted voting
      # mke weights freq table
      w_table <- data.frame(cat = character(), w_freq = numeric())
      col <- cluster[, i]
      for(cat in unique(col)) {
        #sum weights for occurence of each value
        w_table <- rbind(w_table, data.frame(cat = cat, w_freq = sum((col == cat)*weights)))
      }
      # get the max category
      val <- w_table[w_table$w_freq == max(w_table$w_freq), "cat"]
      if(length(val) > 1) val <- sample(val, 1) #ties
      avg_cluster[i] <- val
    }  else {
      # string: weighted voting for characters that don't match
      splits <- strsplit(cluster[, i], NULL)
      max_charlen <- max(unlist(lapply(splits, length)))
      composite_str <- rep("", max_charlen)

      #add blank string spaces for uneven length strings
      splits <- lapply(splits, function(charvec) c(charvec, rep("",max_charlen - length(charvec))))

      for(s in seq_len(max_charlen)) {
        compare <- unlist(lapply(splits, function(str) str[s]))
        w_table <- data.frame(cat = character(), w_freq = numeric())
        for(cat in unique(compare)) {
          #sum weights for occurence of each value
          w_table <- rbind(w_table, data.frame(cat = cat, w_freq = sum((compare == cat)*weights)))
        }
        # get the max category
        val <- as.character(w_table[w_table$w_freq == max(w_table$w_freq), "cat"])
        if(length(val) > 1) val <- sample(val, 1) #ties
        composite_str[s] <- val
      }
      avg_cluster[i] <- paste(composite_str, collapse = "")
    }
  }
  return(avg_cluster)
}
