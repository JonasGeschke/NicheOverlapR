#' Transform data into cast format
#' 
#' Takes the input dataset, wich is one row per individual, and transforms it into the cast format, which will be an overview of the monitoring success per date.
#' @param data Input dataset
#' @param site EBN (El Bosque Nuevo) or PP (Peru Panguana)
#' @return Monitoring success per date
#' @export

cast_site <- function(data, site = "missing"){
  # Here, the package "reshape" is needed for the cast transformation
  if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
  library(reshape)
  # Subsetting the dataset to only contain rows of the requested site
  if (site == "PP"){
    data_sub <- subset(data, site == "PP")}
  else
    if (site == "EBN"){
      data_sub <- subset(data, site == "EBN")}
  else
  {return("Error: Please set site to EBN or PP")}
  # Transforming the dataset into the described cast-format
  cast <- cast(aggregate(data_sub$value,
                         by = list(data_sub$spp, data_sub$date),
                         FUN = "sum",
                         na.rm = T),
               Group.1 ~ Group.2,
               value = "x",
               "sum")
  # Changing the species identification information to the front column and
  rownames(cast) <- cast$Group.1
  # deleting the species column inside the table
  cast <- cast[,-1]
  return(cast)
}