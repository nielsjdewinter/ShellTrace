#' @export
Oyster_Mass_gain <-
function(subincr_matrix, phase_mat, phases){
    # Set abundance of phases with density of 0 (resin is not considered) to 0
    phase_mat2<-as.data.frame(phase_mat)
    rownames(phase_mat2)<-phases[,2]
    phase_mat2[which(phases[,6]==0),]<-rep(0,length(phase_mat2[1,]))
    # Calculate relative contributions of phases in eachincrement
    phase_mat2<-as.data.frame(t(t(phase_mat2)/colSums(phase_mat2)))
    # Calculate average density of subincrements
    density<-as.vector(colSums(phase_mat2*phases[,6]))
    density[is.na(density)]<-0
    # Calculate mass of subincrement in g (daily growth rate if Tstep=1)
    WeightI<-subincr_matrix[,16]*density*0.001
    # Calculate daily growth rate in g/day (same as WeightI if Tstep=1)
    Growth_rate<-WeightI/(subincr_matrix[2,1]-subincr_matrix[1,1])
    # Calculate cumulative weight increase in g
    WeightC<-cumsum(WeightI)
    subincr_matrix<-cbind(subincr_matrix,WeightI,Growth_rate,WeightC)
    rownames(subincr_matrix)<-subincr_matrix[,1]
    dev.new();plot(subincr_matrix[,c(1,19)])
    dev.new();plot(subincr_matrix[,c(1,20)])
    return(subincr_matrix)
}
