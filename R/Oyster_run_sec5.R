#' @export
Oyster_run_sec5 <-
function(phases_name, image_name, image_ext){

    phases<-Oyster_import_phases(phases_name)

    if(image_ext=="bmp"|image_ext=="BMP"){
        BMP<-Oyster_import_BMP(paste(image_name,".",image_ext, sep=""))
        print("Constructing matrix of pixels and their phases from BMP")
        phasemat<-Oyster_phase_matrix_BMP(BMP, phases)
    }
    else if(image_ext=="tif"|image_ext=="tiff"|image_ext=="TIF"|image_ext=="TIFF"){
        TIF<-Oyster_import_TIF(paste(image_name,".",image_ext, sep=""))
        print("Constructing matrix of pixels and their phases from TIF")
        phasemat<-Oyster_phase_matrix_TIF(TIF, phases)
    }

    print("Calculating phase statistics of the entire map")
    phase_stat<-Oyster_phase_stats(phasemat, phases)

    print("Exporting data as list")
    List5<-list(phasemat, phase_stat, phases)
    return(List5)
}
