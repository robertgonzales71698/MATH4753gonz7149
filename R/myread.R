#' Read csv function
#'
#' Takes a csv file and reads in the data
#'
#' @param csv
#'
#' @return a Object holding all of the data read from the csv file
#' @export
#'
#' @examples
#' File.csv; myread(File.csv)
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
