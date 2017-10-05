#' @export
Oyster_run_sec2 <-
function(raw_data, image_length, Xstep=0.1){

    #Modify data to fit XY-coordinates and to create cross section
    print("Converting traced shell data into cross section")
    Llist<-Oyster_Convert_cross_section(raw_data, image_length, Xstep)
    cross_section<-Llist[[1]]
    incr_trace<-Llist[[2]]
    lengthfactor<-Llist[[3]]
    incr_matrix<-Llist[[4]]
    rm(Llist)

    # Plot results of cross section
    print("Plotting cross section")
    Oyster_plot_cross_section(cross_section)

    # Create increment area matrix of layers
    print("Calculating increment values for shell area, length and thickness")
    incr_matrix<-Oyster_incr_area(cross_section, incr_matrix)
    incr_matrix<-Oyster_Shell_thickness(cross_section, incr_matrix)
    incr_matrix<-Oyster_Shell_height(cross_section, incr_matrix)

    List2<-list(cross_section, incr_matrix, lengthfactor)
    return(List2)
}
