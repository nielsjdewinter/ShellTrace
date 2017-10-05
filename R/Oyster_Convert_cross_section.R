#' @export
Oyster_Convert_cross_section <-
function(raw_data, image_length, Xstep=0.1){
    # Convert data to numeric data and remove NA-values for future calculation
    raw_data<-data.frame(lapply(raw_data, as.character), stringsAsFactors=FALSE)
    raw_data<-data.frame(lapply(raw_data, as.numeric))
    raw_data[is.na(raw_data)]<-0
    baseline<-raw_data[,c(1,2)]
    T_death<-raw_data[2,2]
    raw_data<-rbind(raw_data,rep(0,length(raw_data[1,])))

    # Adjust the coordinate system to actual mm scale by setting the length of the reference baseline to the actual length of the shell (measured independently)
    lengthfactor<-(image_length / (baseline[which(baseline[,1]==max(baseline[,1]))-1,1]-baseline[which(baseline[,1]==-1)+1,1]))
    lines_mm<-raw_data*lengthfactor
    lines_mm[2,]<-0

    # Data cleaning
    # In order to set all data to a common x-scale, returning x-values have to be removed to facilitate linear interpolation
    # Data cleaning is done by looping through the X-values and only allowing X-values that are equal to 0 or bigger than the previous value
    # This is done for all XY-records in the input file
    # A new data frame is built with the cleaned data
    # In the process, timing data from the raw_data file is put in the correct format (incr_matrix) for later use
    lines_mm1<-lines_mm
    incr_matrix<-data.frame(t(c(0,0)))
    colnames(incr_matrix)<-c("growth band","age (days)")
    for(i in seq(1,ncol(lines_mm1),3)){
        x<-vector()
        y<-vector()
        for(j in 1:(length(lines_mm1[,1]))){
            if(lines_mm1[j,i]==0){
                x<-append(x,lines_mm1[j,i])
                y<-append(y,lines_mm1[j,i+1])
            }
            else if(lines_mm1[j-1,i]==0 | is.na(lines_mm1[j-1,i])){
                x<-append(x,lines_mm1[j,i])
                y<-append(y,lines_mm1[j,i+1])
            }
            else if(lines_mm1[j,i]>lines_mm1[j-1,i]){
                x<-append(x,lines_mm1[j,i])
                y<-append(y,lines_mm1[j,i+1])
            }
        }
        x<-append(x,rep(0,length(lines_mm1[,i])-length(x)))
        y<-append(y,rep(0,length(lines_mm1[,i])-length(y)))
        lines_mm1[,i]<-x
        lines_mm1[,i+1]<-y

        incr_matrix<-rbind(incr_matrix,c(raw_data[2,i],raw_data[2,i+1]))
    }
    # Calibrate ages of increments to seasonal scale
    T_or<-incr_matrix[length(incr_matrix[,2]),2]%%365
    T_add<-T_death - T_or
    if(T_add<0){T_add<-T_add+365}
    T_cal<-incr_matrix[,2]+T_add
    incr_matrix<-cbind(incr_matrix,T_cal)
    colnames(incr_matrix)[3]<-"age_cal (days)"

    # Linear interpolation
    # Data is assigned a common X-axis by linear interpolation between the data points in the raw data
    # Make sure all x-columns are sorted in ascending order!
    
    # Maximum X-value of the reference baseline is found and rounded up to the next 100 mm
    Xmax<-max(lines_mm[,seq(1,ncol(lines_mm),3)])
    X<-round(Xmax/100+0.5,0)*100
    # A new X-axis is created with Xstep in mm (recommended default: 0.1 mm) 
    lines_lin<-as.data.frame(seq(0,X,Xstep))
    lines_mm2<-lines_mm1
    # All XY-records are linearly interpolated to find values for every X of the new scale 
    for(i in seq(1,ncol(lines_mm2),3)){
        yvect<-vector()
        for(j in lines_lin[,1]){
            close <- which.min(abs(lines_mm2[,i]-j))
            if(lines_mm2[close,i]==j){maxp<-close; minp<-close}
            else {if(lines_mm2[close,i]>j){maxp<-close; minp<-close-1}
                else {minp<-close; maxp<-close+1}
            }
            if(lines_mm2[minp,i+1]==0 | lines_mm2[maxp,i+1]==0) {
                y <- 0
                }
            else {y = lines_mm2[minp,i+1] + ((lines_mm2[maxp,i+1]-lines_mm2[minp,i+1]) / (lines_mm2[maxp,i]-lines_mm2[minp,i])) * (j - lines_mm2[minp,i])
                }
            yvect<-append(yvect,y)
        }
        yvect<-append(yvect,rep(0,length(lines_lin[,1])-length(yvect)))
        # New data frame is created for the interpolated data
        lines_lin<-cbind(lines_lin,yvect)
    }
    colnames(lines_lin)<-c("X",colnames(lines_mm2[,seq(1,ncol(lines_mm2),3)]))

    # New Y-scale with 0 on baseline and positive values upward
    # Subtract Y-values from baseline value to make y-axis point upward (positive values towards higher points in the shell)

    lines_lin2<-lines_lin
    for(i in 1:length(lines_lin[,1])){
        for(j in 2:length(lines_lin[1,])){
            if(is.nan(lines_lin[i,j])){
                lines_lin[i,j]<-lines_lin[i-1,j]
                }
            if(lines_lin[i,j]==0){
                lines_lin2[i,j]<-0
                }
            else {lines_lin2[i,j]<-lines_lin[i,2]-lines_lin[i,j]}
        }
    }

    # Extend growth lines to maximum extend of shell
    # Add values from top and bottom to layer if (x-)extend of shell is less than total reach (in x direction) of the shell to make all layers equally long
    # This equals the length of all growth lines and allows direct addition/subtraction of values

    # Create new data frame
    lines_lin3<-lines_lin2
    # Find first and last values of top and bottom lines
    firstb<-which(lines_lin3[,3]!=0)[1]
    lastb<-which(lines_lin3[,3]!=0)[length(which(lines_lin3[,3]!=0))]
    fistt<-which(lines_lin3[,length(lines_lin3[1,])]!=0)[1]
    lastt<-which(lines_lin3[,length(lines_lin3[1,])]!=0)[length(which(lines_lin3[,length(lines_lin3[1,])]!=0))]
    
    for(i in 3:length(lines_lin3[1,])){
        cat(paste("Converting increment number ",(i-3)),"\r")
        # Find first and last values of yearly growth increments
        first<-which(lines_lin3[,i]!=0)[1]
        last<-which(lines_lin3[,i]!=0)[length(which(lines_lin3[,i]!=0))]
        # Find out if edges of growth lines end in top or bottom of shell and add values from top or bottom lines accordingly 
        for(j in 1:length(lines_lin3[,i])){
            if(j < first) {
                if (abs(lines_lin3[first,i]-lines_lin3[first,3]) < abs(lines_lin3[first,i]-lines_lin3[first,length(lines_lin3[1,])])){
                    lines_lin3[j,i]<-lines_lin3[j,3]
                    }
                else {lines_lin3[j,i]<-lines_lin3[j,length(lines_lin3[1,])]}
                }
            else if (j > last) {
                if (abs(lines_lin3[last,i]-lines_lin3[last,3]) < abs(lines_lin3[last,i]-lines_lin3[last,length(lines_lin3[1,])])){
                    lines_lin3[j,i]<-lines_lin3[j,3]
                    }
                else {lines_lin3[j,i]<-lines_lin3[j,length(lines_lin3[1,])]}
                }
            else {lines_lin3[j,i]<-lines_lin3[j,i]}
        }
    }
    # Give timing in days as headers of cross section columns
    colnames(lines_lin3)<-incr_matrix[,2]
    incr_matrix<-incr_matrix[-c(1,2),]
    # Export raw growth line data (year trace), growth line data with top and bottom values added (cross section) as well as
    # lengthfactor and timing matrix for future use
    Llist<-list(lines_lin3, lines_lin2, lengthfactor, incr_matrix)
    return(Llist)
}