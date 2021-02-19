#' @title A function to read in a csv file
#'
#' @param csv A file to read data from
#'
#' @return A data frame from the file
#' @export
#'
#' @examples
#' \dontrun{ddt <- DDT.csv; myread(csv = ddt)}
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
