#' @export
Oyster_phase_run <-
function(LOG=T, phases_name, image_name, image_ext, IncG, pixelsize, subincr_matrix, npma=10, name_shell, name_file){
    if(LOG){
        # Print LOG
        name<-as.list(match.call()[-c(1,2)])
        lst<-list(paste("LOG of parameters of shell phase model ran on ", date(),"\nRoutine : ", as.character(match.call()[[1]]),"\n"))
        for(i in 1:length(name)){
            lst<-append(lst,paste(names(name[i])," : ",name[i]))
        }
        write.table(lst, paste("LOG_",as.character(match.call()[[1]]),format(Sys.time(),format="%Y%m%d_%Hh%M"),".txt"), sep="\n", col.names=F, row.names=F, quote=F)
    }
    
    List5<-Oyster_run_sec5(phases_name, image_name, image_ext)
    phasemat<-as.matrix(as.data.frame(List5[1]))
    phase_stat<-as.data.frame(List5[2])
    phases<-as.data.frame(List5[3])
    phases<-data.frame(lapply(phases, as.character), stringsAsFactors=FALSE)
    phases[,3:length(phases[1,])]<-data.frame(lapply(phases[,3:length(phases[1,])], as.numeric))
    rm(List5)

    List6<-Oyster_run_sec6(phasemat, IncG, pixelsize, phases, subincr_matrix, npma, name_file)
    el_time<-as.data.frame(List6[1])
    M_el_mat<-as.data.frame(List6[2])
    M_el_mat_c<-as.data.frame(List6[3])
    subincr_matrix<-as.data.frame(List6[4])
    rm(List6)

    Oyster_phase_export(phase_stat, el_time, M_el_mat, M_el_mat_c, name_shell)

    print("Exporting model results into XLSX files")
    Plist<-list(phase_stat, el_time, M_el_mat, M_el_mat_c, subincr_matrix)
    return(Plist)
}
