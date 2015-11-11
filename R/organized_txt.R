#' Automaticly save txt output files
#' 
#' Automaticly creates folders within the current working directory and saves organized txt output files.
#' @details This function is part of the "nicheoverlap" function.

organized_txt <- function(data_spp, method, site){
  if ("pianka" %in% list.files()|"czekanowski" %in% list.files())
  {}
  else
  {dir_list <- c("pianka", "czekanowski", "pianka/EBN", "pianka/PP", "czekanowski/EBN", "czekanowski/PP")
  for(i in dir_list){dir.create(i)}}
  colnames(data_spp) <- c("Spp1", "Spp2", (paste("Prob", 1:(ncol(data_spp)-2), sep = "_")))
  write.table(data_spp,
              file = paste(getwd(), "/", 
                           method, "/", 
                           site, "/", 
                           method,"_", 
                           site,"_", 
                           Sys.Date(), "_",
                           format(Sys.time(), "%H-%M"), 
                           ".txt", 
                           sep=""),
              sep = "\t",
              row.names = F)
  return(data_spp)
}