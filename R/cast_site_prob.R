#' Subset cast dataset per probability threshold
#' 
#' Takes the input cast dataset and subsets into dataset containing only those species that were part of at least one species pair occurring with a probability >= a certain threshold value.
#' @param data Input cast dataset
#' @param prob Numeric probability threshold value, from which on (>=) species are subset - e.g. 0.5 (standard)
#' @param site EBN (El Bosque Nuevo) or PP (Peru Panguana)
#' @return By probability threshold reduced dataset
#' @export

# get the species of the species pairs that show >=X prob overlap in the observations datasets
cast_site_prob <- function(data, prob = 0.50, site = "missing"){
  if(site %in% c("PP", "EBN")){
    #extracting the species from spp pairs prob >=X prob into single datasets
    sub_data_1 <- data$Spp1[data$TempOverlapProb >= prob]
    sub_data_2 <- data$Spp2[data$TempOverlapProb >= prob]
    #create one dataset with all the species just extracted
    if(site == "PP")
    {cast_prob <- cast_PP[rownames(cast_PP) %in% sub_data_1 | rownames(cast_PP) %in% sub_data_2,]
    return(cast_prob)}
    else
    {cast_prob <- cast_EBN[rownames(cast_EBN) %in% sub_data_1 | rownames(cast_EBN) %in% sub_data_2,]
    return(cast_prob)}}
  else
  {return("Error: Please set site to EBN or PP")}
}