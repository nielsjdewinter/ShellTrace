#' @export
Oyster_phase_stats <-
function(phasemat,phases){
    fraction<-vector()
    pixels<-vector()
    for(i in 1:length(phases[,2])){
        pixels<-append(pixels,length(which(phasemat==phases[i,2])))
        fraction<-append(fraction,length(which(phasemat==phases[i,2]))/length(phasemat))
    }
    phase_stat<-cbind(phases[,2],pixels,fraction,phases[,7:length(phases[1,])])
    
    phase_stat[,2:length(phase_stat[1,])]<-lapply(phase_stat[,2:length(phase_stat[1,])],as.numeric)
    phase_stat[,1]<-levels(phase_stat[,1])
    Bulkcomp<-as.vector(c("Bulk composition",as.numeric(sum(phase_stat[,2])),as.numeric(sum(phase_stat[,3])),as.numeric(colSums(phase_stat[,3]*phase_stat[,4:length(phase_stat[1,])]))))
    phase_stat<-rbind(phase_stat, Bulkcomp)

    return(phase_stat)
}
