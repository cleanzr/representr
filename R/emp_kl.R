# Histogram-Based Estimation for the Divergence from
# https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=1499042
# https://sail.usc.edu/publications/files/silvaisit2007.pdf
# https://sail.usc.edu/publications/files/silvaisit2009.pdf

# equal binning function
# @param numeric_vector Vector to bin
# @param T_m Number of bins
bins_by_num <- function(numeric_vector, T_m) {
  # returns cut points for numeric variable
  if(T_m == 0) return(c(-Inf, Inf))
  quants <- as.numeric(unique(stats::quantile(numeric_vector, probs = seq(0, 1, by = 1/T_m))))

  # replace first and last with Inf, -Inf for labels
  quants[1] <- -Inf
  quants[length(quants)] <- Inf

  return(quants)
}

# make nested cuts
# @param dat Data to make bins based on (the reference dataset)
# @param numeric_vars A vector of column positions or column names for the numeric variables.
# @param T_m Number of bins
nest_cuts <- function(dat, numeric_vars, T_m) {
  res <- list()
  if(length(numeric_vars) > 0) {
    col <- numeric_vars[1]
    cut_pts <- bins_by_num(dat[, col], T_m)
    cuts <- as.character(cut(dat[, col], breaks = cut_pts, right = TRUE, include.lowest = TRUE))

    res[[col]][["cut_pts"]] <- cut_pts

    if(length(numeric_vars) > 1) {
      for(i in unique(cuts)) {
        res[[col]][[i]] <- nest_cuts(dat[cuts == i,], numeric_vars[-1], T_m)
      }
    }
  }
  res
}

# make nested bins
# @param dat Data to make bins based on (the reference dataset)
# @param cuts_list List of cuts, from nest_cuts()
nest_bins <- function(dat, cuts_list) {
  #condition based on the cuts_list
  cuts <- data.frame()
  if(length(cuts_list) > 0 & nrow(dat) > 0) {
    col <- names(cuts_list)
    cut_pts <- cuts_list[[col]]$cut_pts
    subcut_list <- cuts_list[[col]][-1]

    cuts <- data.frame(as.character(cut(dat[, col], cut_pts, right = TRUE, include.lowest = TRUE)))
    names(cuts) <- col

    if(length(subcut_list) > 0) {
      #storage
      col2 <- names(subcut_list[[1]])
      cuts[, col2] <- NA

      for(inner in names(subcut_list)) {
        # need to subset data here by cuts above
        cuts[cuts[, col] == inner, col2] <- nest_bins(dat[cuts[, col] == inner, ], subcut_list[[inner]])
      }
    }

  }
  cuts
}


# get freqs for dataset
# @param data_set data to get freqs for
# @param categoric_vars A vector of column positions or column names for the categoric variables.
# @param numeric_vars A vector of column positions or column names for the numeric variables.
# @param true_cut_list List of cuts based on reference dataset, result from running nest_cuts() for each variable
# @param weights If weighted frequencies are desired, pass a vector weights of the same length as representative data points.
get_freqs <- function(data_set, categoric_vars, numeric_vars, true_cut_list, weights = rep(1, nrow(data_set))) {
  data_split <- split(data.frame(data_set[, c(categoric_vars, numeric_vars)], weights = weights), data_set[, categoric_vars])

  bins_list <- lapply(names(data_split), function(name) cbind(nest_bins(data_split[[name]], cuts_list = true_cut_list[[name]]), weights = data_split[[name]]$weights))
  bins_df <- do.call(rbind, bins_list)
  bins_df$cat_vars <- rep(names(data_split), unlist(lapply(data_split, nrow)))
  bins_df[, categoric_vars] <- do.call(rbind, strsplit(bins_df$cat_vars, split = "[.]"))

  data.frame(dplyr::summarise(dplyr::group_by_(bins_df, .dots = c(categoric_vars, numeric_vars)), freq = sum(weights)))
}

#' Calculate the empirical KL divergence for a representative dataset as compared to the true dataset
#'
#' @param true_dat The true dataset
#' @param rep_dat A representative dataset
#' @param categoric_vars A vector of column positions or column names for the categoric variables.
#' @param numeric_vars A vector of column positions or column names for the numeric variables.
#' @param l_m Approximate number of true data points to be in each bin for numeric variables. Default is 10.
#' @param weights If weighted frequencies are desired, pass a vector weights of the same length as representative data points.
#'
#' @details
#'
#' This function computes the estimated the KL divergence of two samples of data
#' using the empirical distribution functions for the representative data set and true data set
#' with continuous variables transformed to categorical using a histogram approach with
#' statistically equivalent data-dependent bins, as detailed in
#'
#' Wang, Qing, Sanjeev R. Kulkarni, and Sergio Verdú. "Divergence estimation of continuous distributions based on data-dependent partitions." IEEE Transactions on Information Theory 51.9 (2005): 3064-3074.
#'
#' @examples
#'
#' data("rl_reg1")
#'
#' ## random prototyping
#' rep_dat_random <- represent(rl_reg1, identity.rl_reg1, "proto_random", id = FALSE, parallel = FALSE)
#'
#' ## empirical KL divergence
#' cat_vars <- c("sex")
#' num_vars <- c("income", "bp")
#' emp_kl_div(rl_reg1[unique(identity.rl_reg1), c(cat_vars, num_vars)],
#'            rep_dat_random[, c(cat_vars, num_vars)],
#'            cat_vars, num_vars)
#'
#' @export
emp_kl_div <- function(true_dat, rep_dat, categoric_vars, numeric_vars, l_m = 10, weights = rep(1, nrow(rep_dat))) {
  stopifnot(nrow(rep_dat) == length(weights))

  ## universal bins
  true_cut_list <- lapply(split(true_dat, true_dat[, categoric_vars]), function(dat) {
    T_m <- floor((nrow(dat)/l_m)^(1/length(numeric_vars)))
    nest_cuts(dat, numeric_vars, T_m)
  })

  true_freq <- get_freqs(true_dat, categoric_vars, numeric_vars, true_cut_list)
  rep_freq <- get_freqs(rep_dat, categoric_vars, numeric_vars, true_cut_list, weights = weights)
  freqs <- merge(true_freq, rep_freq, by = c(numeric_vars, categoric_vars))
  freqs <- freqs[freqs$freq.x > 0, ] # true freqs > 0

  freqs$prob <- freqs$freq.y/sum(freqs$freq.y) # get probability distributions that are abs. cont wrt truth
  freqs$true_prob <- freqs$freq.x/sum(freqs$freq.x)

  sum(ifelse(freqs$prob == 0, 0, freqs$prob*log(freqs$prob/freqs$true_prob)))
}
