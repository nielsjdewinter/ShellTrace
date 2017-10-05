#' @export
Oyster_run_sec4 <-
function(IncG, subincr_matrix, Xstep = 0.1){

    # Calculate Z-Values for volume modeling
    print("Calculating Z-values for volume model")
    Z_mat<-Oyster_Z_matrices(IncG, subincr_matrix)

    # Calculate in subyearly matrix the volumes and mass gain
    print("Calculating volume and growth rate through time")
    diagL<-Oyster_Volumes(subincr_matrix, Z_mat, IncG, Xstep)
    subincr_matrix<-as.data.frame(diagL[1])
    IncGAnet<-as.data.frame(diagL[2])

    List4<-list(subincr_matrix, IncGAnet)
    return(List4)
}
