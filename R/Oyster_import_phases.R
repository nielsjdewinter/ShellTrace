#' @export
Oyster_import_phases <-
function(file_name){
    phase<-read.csv(file_name, sep=";")
    phase[,as.numeric(which(sapply(phase,class)=="factor"))]<-data.frame(lapply(phase[,as.numeric(which(sapply(phase,class)=="factor"))], as.character), stringsAsFactors=FALSE)
    return(phase)
}
