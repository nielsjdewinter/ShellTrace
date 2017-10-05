#' @export
Oyster_phase_matrix_TIF <-
function(TIF,phases){
    TIF<-round(TIF*256,0)
    phasemat<-matrix(F, nrow=nrow(TIF), ncol=ncol(TIF))
    # Assign matching phases
    for(a in 1:nrow(TIF)){
        cat(paste("Converting row ",a, " of ",nrow(TIF)),"\r")
        for(b in 1:ncol(TIF)){
            # Use pythagorean theorem to find closest match of phase (prevents bug in case of distorted pixels)
            phasemat[a,b]<-phases[which.min(sqrt((TIF[a,b,1]-phases[,3])^2+(TIF[a,b,2]-phases[,4])^2+(TIF[a,b,3]-phases[,5])^2)),2]
        }
    }
    return(phasemat)
}
