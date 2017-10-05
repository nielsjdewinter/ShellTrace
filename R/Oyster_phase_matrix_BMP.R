#' @export
Oyster_phase_matrix_BMP <-
function(BMP,phases){
    phasemat<-matrix(ncol=ncol(BMP),nrow=nrow(BMP))
    for(a in 1:nrow(BMP)){
        cat(paste("Converting row ",a, " of ",nrow(BMP)),"\r")
        for(b in 1:ncol(BMP)){
            # Use pythagorean theorem to find closest match of phase (prevents bug in case of distorted pixels)
            phasemat[a,b]<-phases[which.min(sqrt((BMP[a,b,1]-phases[,3])^2+(BMP[a,b,2]-phases[,4])^2+(BMP[a,b,3]-phases[,5])^2)),2]
        }
    }
    # Get rid of factor columns in phasemat
    return(phasemat)
}
