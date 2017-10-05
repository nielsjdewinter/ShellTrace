#' @export
Oyster_Shell_thickness <-
function(cross_section, incr_matrix){
    av_thickness<-0
    for(i in 4:length(cross_section[1,])){
        av_thickness<-append(av_thickness, mean(cross_section[,3]-cross_section[,i]))
    }
    incr_matrix<-cbind(incr_matrix, av_thickness)
    dev.new(); plot(incr_matrix[,c(2,6)], type = "l")
    return(incr_matrix)
}
