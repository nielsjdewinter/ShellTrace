#' @export
Oyster_subincr_av_thickness_X <-
function(IncG,subincr_matrix){
    print("Calculating subincremental shell thickness")
    IncT<-abs(IncG-IncG[,1])
    IncT[IncT==0]<-NA
    av_thickness<-colMeans(IncT,na.rm=TRUE)
    av_thickness[is.na(av_thickness)]<-0
    subincr_matrix<-cbind(subincr_matrix, av_thickness)
    dev.new(); plot(subincr_matrix[,c(1,11)], type = "l")
    return(subincr_matrix)
}