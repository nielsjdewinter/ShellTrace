#' @export
Oyster_incr_area <-
function(cross_section, incr_matrix){
    incr_area<-vector()
    for(i in 4:length(cross_section[1,])){
        area<-0
        for (j in 1:length(cross_section[,1])){
            area<-area + 0.1*(cross_section[j,i-1] - cross_section[j,i])
        }
        incr_area<-append(incr_area,area)
    }
    incr_area<-append(0,incr_area)
    incr_cumarea<-cumsum(incr_area)
    incr_matrix<-cbind(incr_matrix, incr_area, incr_cumarea)
    dev.new(); plot(incr_matrix[,2],incr_matrix[,4], type = "l")
    return(incr_matrix)
}
