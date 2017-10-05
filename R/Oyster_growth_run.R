#' @export
Oyster_growth_run <-
function(LOG=T, raw_data, image_length, season_length=250, Xstep=0.1, Tstep=1, Oyster_height, Oyster_length, name_file="Oyster_growth_model"){
    if(LOG){
        # Print LOG
        name<-as.list(match.call()[-c(1,2)])
        lst<-list(paste("LOG of parameters of shell growth model ran on ", date(),"\nRoutine : ", as.character(match.call()[[1]]),"\n"))
        for(i in 1:length(name)){
            lst<-append(lst,paste(names(name[i])," : ",name[i]))
        }
        write.table(lst, paste("LOG_",as.character(match.call()[[1]]),format(Sys.time(),format="%Y%m%d_%Hh%M"),".txt"), sep="\n", col.names=F, row.names=F, quote=F)
    }

    List2<-Oyster_run_sec2(raw_data, image_length, Xstep)
    cross_section<-as.data.frame(List2[1])
    incr_matrix<-as.data.frame(List2[2])
    lengthfactor<-List2[3]
    rm(List2)

    List3<-Oyster_run_sec3(cross_section, incr_matrix, season_length, Xstep, Tstep, Oyster_height, Oyster_length)
    IncG<-as.data.frame(List3[1])
    subincr_matrix<-as.data.frame(List3[2])
    rm(List3)

    List4<-Oyster_run_sec4(IncG, subincr_matrix, Xstep)
    subincr_matrix<-as.data.frame(List4[1])
    IncGAnet<-as.data.frame(List4[2])
    rm(List4)

    # Export data
    print("Exporting results from volume model into XLSX file")
    Oyster_Export(subincr_matrix, name_file)

    GList<-list(subincr_matrix, IncG, lengthfactor)
    return(GList)
}