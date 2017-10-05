#' @export
Oyster_combined_run <-
function(raw_data, image_length, season_length=250, Xstep=0.1, Tstep=1, Oyster_height, Oyster_length, name_file="Oyster_growth_model", phases_name, image_name, image_ext, npma=10, name_shell){
    # Print LOG
    name<-as.list(match.call()[-c(1,2)])
    lst<-list(paste("LOG of parameters of combined shell model ran on ", date(),"\nRoutine : ", as.character(match.call()[[1]]),"\n"))
    for(i in 1:length(name)){
        lst<-append(lst,paste(names(name[i])," : ",name[i]))
    }
    write.table(lst, paste("LOG_",as.character(match.call()[[1]]),format(Sys.time(),format="%Y%m%d_%Hh%M"),".txt"), sep="\n", col.names=F, row.names=F, quote=F)
        
    # Run growth model
    Glist<-Oyster_growth_run(LOG=F, raw_data, image_length, season_length=250, Xstep=0.1, Tstep=1, Oyster_height, Oyster_length, name_file)
    
    # Export data that is needed for the phase model
    subincr_matrix<-as.data.frame(Glist[1])
    IncG<-as.data.frame(Glist[2])
    lengthfactor<-as.numeric(Glist[[3]])
    pixelsize<-ceiling(lengthfactor*1000)/1000
    
    # Run phase model
    Plist<-Oyster_phase_run(LOG=F, phases_name, image_name, image_ext, IncG, pixelsize, subincr_matrix, npma=10, name_shell, name_file)

    # Export data that is needed from phase model
    phase_stat<-as.data.frame(Plist[1])
    el_time<-as.data.frame(Plist[2])
    M_el_mat<-as.data.frame(Plist[3])
    M_el_mat_c<-as.data.frame(Plist[4])
    subinc_matrix<-as.data.frame(Plist[5])

    output<-list(subincr_matrix, IncG, phase_stat, el_time, M_el_mat, M_el_mat_c)
    return(output)
}