## Functions for eca\&d station files
## -- introduced by C. Photiadou (KNMI), November 2015
# Secondary function for eca.input
# 
# Typical ECA\&D files contain a header and this functions finds the line number where the data starts.
#
find.start.of.data.index = function(fname) {
  matched.indices = which(grepl('^SOUID', gsub(" ", "", readLines(fname, n = 50))))
  if (length(matched.indices) > 1) stop('ECA fileformat error: cannot determine start of data, multiple header lines')
  if (length(matched.indices) == 0) stop('ECA fileformat error: cannot find start of data: cannot find header line')
  return(matched.indices - 1)
}

## Functions for eca\&d station files
#' ECA &D station data files 
#' 
#' This function reads and prepares the station data files from the European Climate Assessment and Dataset (ECA&D) for use in climdex
#' 
#' @param filename File name and path of station data file.
#' @param var.name A varialbe name from the ECA\&D variavle list: prec (RR), tavg (TG), tmax (TX), tmin(TN), sun (SS),
#'        wind_gust (FX), wind (FG).
#' @param data.name Always DATE
#' @return A data frame containing two columns: DATE with dates in PCICt format and "var.name" the varialbe name.
#' @template get_generic_example
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/}
#' @export
eca.input <- function(filename, var.name, date.name){
  
  ifile.eca <- read.table(filename, skip=find.start.of.data.index(filename), sep=",", header=T)[,c(date.name,var.name)] 
  ifile.eca[[date.name]] <- as.PCICt(strptime(as.character(ifile.eca[[date.name]]),"%Y%m%d"), cal="gregorian")
  ifile.eca[[var.name]][ifile.eca[[var.name]]==-9999]<- NA
  if(!(var.name %in% c("TG", "TX" ,"TN", "RR", "SS", "FX", "FG"))){
    return(ifile.eca)
  } else
    ifile.eca[[var.name]] <- ifile.eca[[var.name]]*0.1
  
  return(ifile.eca)
}
