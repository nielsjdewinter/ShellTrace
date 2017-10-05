#' @export
Oyster_subincr_av_thickness <-
function(subincr_matrix){
    print("Calculating subincremental shell thickness")
    av_thickness<-subincr_matrix[,5]/subincr_matrix[,8]
    av_thickness[which(is.na(av_thickness))]<-0
    av_thickness[which(av_thickness=="Inf")]<-0
    subincr_matrix<-cbind(subincr_matrix, av_thickness)
    dev.new(); plot(subincr_matrix[,c(1,11)], type = "l")
    return(subincr_matrix)
}
