#' @export
Oyster_run_sec6 <-
function(phasemat, IncG, pixelsize, phases, subincr_matrix, npma, name_file){
    
    print("Calculating phase statistics per subyearly growth increment")
    phase_mat<-Oyster_subincr_phases(IncG, phasemat, pixelsize, phases)

    print("Calculating mass gain per subyearly growth increment")
    subincr_matrix<-Oyster_Mass_gain(subincr_matrix, phase_mat, phases)
    Oyster_Export(subincr_matrix, name_file)

    print("Calculating concentrations of elements through time using phase data")
    el_time<-Oyster_el_time(phase_mat, phases)

    print("Calculating accumulation rates based on concentrations through time and mass accumulation of growth model")
    AccL<-Oyster_accumulation(el_time, subincr_matrix, npma) 
    M_el_mat_c<-as.data.frame(AccL[1])
    M_el_mat<-as.data.frame(AccL[2])

    print("Exporting data as list")
    List6<-list(el_time, M_el_mat, M_el_mat_c, subincr_matrix)
    return(List6)
}
