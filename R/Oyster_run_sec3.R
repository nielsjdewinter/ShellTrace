#' @export
Oyster_run_sec3 <-
function(cross_section, incr_matrix, season_length=250, Xstep=0.1, Tstep=1, Oyster_height, Oyster_length){

    # New increment table through interpolation
    print("Interpolating cross section data to calculate subincrements at resolution of Tstep")
    Lsub<-Oyster_incr_cross_section(incr_matrix, cross_section, season_length, Tstep, Xstep)
    IncG<-data.frame(Lsub[1])
    subincr_matrix<-data.frame(Lsub[2])
    subincr_matrix<-data.frame(lapply(subincr_matrix, as.character), stringsAsFactors=FALSE)
    subincr_matrix<-data.frame(lapply(subincr_matrix, as.numeric))
    colnames(IncG)<-subincr_matrix[,1]

    # Plot increment cross section
    print("Plotting incremental growth cross section")
    Oyster_plot_incr_CS(IncG, incr_matrix, Tstep)
    Oyster_plot_incr_fill(IncG)

    # Calculate subincremental matrix of parameters
    print("Calculating subincremental values for shell cross section area, height, thickness and base ellipse parameters for total shell volume")
    subincr_matrix<-Oyster_subincr_area(IncG, subincr_matrix, Xstep)
    subincr_matrix<-Oyster_subincr_shell_height(subincr_matrix, IncG, Xstep)
    subincr_matrix<-Oyster_subincr_av_thickness(subincr_matrix)
    subincr_matrix<-Oyster_ellipse_parameters(subincr_matrix, IncG, Oyster_height, Oyster_length)

    List3<-list(IncG, subincr_matrix)
    return(List3)
}
