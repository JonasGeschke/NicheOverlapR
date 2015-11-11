#' Read in output files
#' 
#' Reads in the automaticly saved txt output files saved by the "organized_txt" function. Compiles the results into one dataframe per method and site.
#' @param method pianka or czekanowski
#' @param site EBN (El Bosque Nuevo) or PP (Peru Panguana)
#' @export

readin_txt <- function(method, site){
  file_list <- list.files(paste(getwd(), "/", 
                                method, "/", 
                                site, 
                                sep=""))
  #initialize empty dataframe
  data <- read.table(paste(getwd(), "/", 
                           method, "/", 
                           site, "/", 
                           file_list[1], 
                           sep=""), 
                     header = TRUE)
  for (i in file_list[-1]) {
    #read in the seperate files each indicated by the id number
    readin <- read.table(paste(getwd(), "/", 
                               method, "/", 
                               site, "/", 
                               i, 
                               sep=""), 
                         header = TRUE)
    #cbind the readin data into the dataframe data
    data <- cbind(data, readin[,3:ncol(readin)])
  }
  colnames(data) <- c("Spp1", "Spp2", 
                      (paste("Prob", 1:(ncol(data)-2), sep = "_")))
  return(data)
}