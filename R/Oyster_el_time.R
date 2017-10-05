#' @export
Oyster_el_time <-
function(phase_mat, phases){
    # Exclude non-relevant phases
    phase_mat_f<-phase_mat[which(phases[,6]!=0),]
    phases_f<-phases[which(phases[,6]!=0),]
    # Calculate relative phase contributions
    phase_mat_f<-t(t(phase_mat_f)/as.vector(colSums(phase_mat_f)))
    # Remove zero-entries
    phase_mat_f[,which(colSums(phase_mat_f)==0)]<-rep(0,length(phases_f[,1]))
    # Calculate concentrations from relative abundances of elements in phases
    elmat<-phases_f[,7:length(phases_f[1,])]
    el_time<-vector()

    for(t in 1:length(phase_mat_f[1,])){
        cat(paste("Calculating increment: ",t),"\r")
        els<-as.numeric(colSums(phase_mat_f[,t]*elmat))
        el_time<-cbind(el_time,els)
    }
    el_time[is.na(el_time)]<-0
    colnames(el_time)<-colnames(phase_mat)
    rownames(el_time)<-colnames(elmat)
    return(el_time)
}
