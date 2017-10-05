#' @export
Oyster_subincr_area <-
function(IncG, subincr_matrix, Xstep=0.1){
    areaY<-0
    areaC<-0
    # Area between two increments and cumulative area of cross section is calculated
    for(t in 1:(length(IncG[1,])-1)){
        cat(paste("Calculating area for increment: ",t),"\r")
        areaY<-append(areaY,sum(IncG[,t]-IncG[,t+1])*Xstep)
    }
    areaC<-cumsum(areaY)
    subincr_matrix<-cbind(subincr_matrix,areaY,areaC)
    dev.new(); plot(subincr_matrix[,c(1,5)], type = "l")
    return(subincr_matrix)
}
