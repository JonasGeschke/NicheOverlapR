#' RA4 niche overlap randomization algorithm
#' 
#' Randomizes the monitoring data with applying a modification of the RA4 function from the "EcosimR" package to avoid upcoming error messages.
#' @details This function is part of the "nicheoverlap" function.

ra4_mod <- function(speciesData) {
  NonZeroRowShuffle <- function(vec = runif(10)) {
    nonzero <- which(vec > 0)
    shuffledvec <- vec
    shuffledvec[nonzero] <- vec[sample(nonzero, 
                                       length(nonzero))] #mod: added length() to avoid error msg
    return(shuffledvec)
  }
  #split the dataset into rows which have more than one observation and rows that have only one
  data_mult_obs <- speciesData[rowSums(speciesData != 0) > 1,]
  data_solo_obs <- speciesData[rowSums(speciesData != 0) == 1,]
  RM <- t(apply(data_mult_obs, 1, NonZeroRowShuffle))
  rownames(RM) <- rownames(data_mult_obs)
  colnames(RM) <- colnames(data_solo_obs)
  RM <- rbind(RM, data_solo_obs)
  return(RM)
}