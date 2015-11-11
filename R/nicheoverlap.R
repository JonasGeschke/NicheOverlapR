#' Niche overlap analysis
#' 
#' This function calculates the temporal niche overlap probabilities for each possible species pair within an animal community.
#' @param data Input data in cast format
#' @param method pianka or czekanowski, niche overlap index you want to use
#' @param ra4 TRUE (standard) or FALSE, if FALSE no RA4 randomization of the input data
#' @param iterations 1 (standard), numeric value of how many iterations you want the funtion to run
#' @param txt TRUE or FALSE (standard), if TRUE automaticly saves organized txt output files
#' @param site EBN (El Bosque Nuevo) or PP (Peru Panguana)
#' @return Temporal niche overlap probabilities for species pairs
#' @details Further, the processing time of the function is displayed: start / end / duration
#' @export

nicheoverlap <- function(data, method = "missing", ra4 = TRUE, iterations = 1, txt = FALSE, site = "missing"){
  time1 <- Sys.time()
  print(time1)
  if(site %in% c("PP", "EBN")){
    data_spp <- data.frame(Spp1 = factor(rep(1, choose(nrow(data), 2))),
                           Spp2 = factor(rep(1, choose(nrow(data), 2))))
    repeat{
      if(ra4 == TRUE){
        #ra4 randomization
        data_rand <- ra4_mod(data)
        #convert absolute to relative abundance data
        rel_data <- data_rand/rowSums(data_rand)}
      else {
        #convert absolute to relative abundance data
        rel_data <- data/rowSums(data)}
      if(method == "pianka"){
        # Pianka
        data_p <- cbind(t(combn(rownames(rel_data), 2)), 0)
        for (i in 1:nrow(data_p)){
          data_p[i, 3] <- sum(rel_data[data_p[i,1],] * rel_data[data_p[i,2],])/sqrt(sum(rel_data[data_p[i,1],]^2) * sum(rel_data[data_p[i, 2],]^2))
        }}
      else
        if(method == "czekanowski"){
          # Czekanowski
          data_p <- cbind(t(combn(rownames(rel_data), 2)), 0)
          for (i in 1:nrow(data_p)){
            data_p[i, 3] <- 1 - 0.5 * sum(abs((rel_data[data_p[i, 1], ] - rel_data[data_p[i, 2], ])))
          }}
      else
      {return("Error: Please set method as pianka or czekanowski")}
      data_spp[,c(1,2)] <- data_p[,c(1,2)]
      data_spp <- data.frame(data_spp, data_p[,3])
      if (ncol(data_spp)-2 == iterations[1]){break}
    }
    # write txt cut here
    if(txt == FALSE)
    {return(data_spp)}
    else
    {organized_txt(data_spp, method = method, site = site)}
  } else
  {return("Error: Please set site to EBN or PP")}
  time2 <- Sys.time()
  print(time2)
  print(time2 - time1)
}