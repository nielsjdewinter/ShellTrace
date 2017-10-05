#' @export
Oyster_subincr_phases <-
function(IncG, phasemat, pixelsize, phases){
    phase_mat<-vector()
    for(t in 1:(length(IncG[1,])-1)){
        tph<-as.vector(rep(0,length(phases[,1])))
        cat(paste("Calculating increment: ",t),"\r")
        for(x in 1:length(IncG[,1])){
            xph<-vector()
            y2<-IncG[x,t]/pixelsize
            y1<-IncG[x,t+1]/pixelsize
            x1<-as.numeric(rownames(IncG)[x])/pixelsize
            x2<-as.numeric(rownames(IncG)[x])/pixelsize
            if(x2>length(phasemat[1,])){
                xph<-rep(0,length(phases[,1]))
            }
            else{
                for(p in 1:length(phases[,1])){
                    xph<-append(xph,length(which(phasemat[round(y1-0.5,0):round(y2+0.5,0),round(x1-0.5,0):round(x2+0.5,0)]==phases[p,2])))
                }
            }
            tph<-tph+xph
        }
        phase_mat<-cbind(phase_mat,tph)
    }
    phase_mat<-cbind(rep(0,length(phases[,1])),phase_mat)
    colnames(phase_mat)<-colnames(IncG)
    return(phase_mat)
}
