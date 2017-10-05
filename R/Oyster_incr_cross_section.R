#' @export
Oyster_incr_cross_section <-
function(incr_matrix, cross_section, season_length, Tstep=1, Xstep=0.1){
    # Calculate increment layers through interpolation of X and Y values between yearly growth lines
    # Create new data frame for incremental cross section
    IncG<-cross_section[,3]
    # Create storage vectors for start and endpoints of subincrements
    p1xs<-incr_matrix[1,7]
    p2xs<-incr_matrix[1,9]
    for(i in 1:(length(incr_matrix[,1])-1)){
        cat(paste("Interpolating increment number ",i),"\r")
        # Determine start and end days of section to be interpolated
        T_in1<-incr_matrix[i,3]
        T_in2<-incr_matrix[i+1,3]
        # Create t-axis between endpoints
        steps<-seq(T_in1+Tstep,T_in2,Tstep)
        # Reduce to day of the year
        steps<-cbind(steps,steps%%365)
        # Calculate g_thres based on season length
        T1<-365/2-0.5*season_length
        g_thres = 0.5 - 0.5 * cos((2*pi) / 365 * T1)
        # Calculate the relative amount of growth in each time step
        steps<-cbind(steps,0.5 - 0.5 * cos((2*pi) / 365 * steps[,2]) - g_thres)
        steps[which(steps[,3]<0),3]<-0
        # If entire range between increments falls below the threshold (then the growth season length is wrongly chosen!),
        # then linear growth is assumed in order to allow the model to continue:
        if(sum(steps[,3])==0){
            steps<-cbind(steps,rep(1/length(steps[,1])))
            print("WARNING: Length of growth season specified is too short, linear growth interpolation applied")
        }else{steps<-cbind(steps,steps[,3]/sum(steps[,3]))}
        steps<-cbind(steps,cumsum(steps[,4]))

        for(tin in 1:length(steps[,1])){
            # Find the X value of the first and last point on the subyearly inremental line
            p1xtin<-incr_matrix[i,7]+(incr_matrix[i+1,7]-incr_matrix[i,7])*steps[tin,5]
            p2xtin<-incr_matrix[i,9]+(incr_matrix[i+1,9]-incr_matrix[i,9])*steps[tin,5]
            p1xs<-append(p1xs,p1xtin)
            p2xs<-append(p2xs,p2xtin)

            # Calculate the distance between the subincrement and the top of the shell
            Dinc<-(cross_section[,i+2]-cross_section[,i+3])*steps[tin,5]
            Dinc<-c(rep(0,round(p1xtin/Xstep+0.5,0)),Dinc[(round(p1xtin/Xstep+0.5,0)+1):(round(p2xtin/Xstep-0.5,0)+1)],rep(0,length(Dinc)-(round(p2xtin/Xstep-0.5,0)+1)))
            if(length(Dinc)>length(cross_section[,1])){
                Dinc<-Dinc[1:length(cross_section[,1])]
            }

            # Calculate Y values of the subincrement and add to matrix
            Inc<-cross_section[,i+2]-Dinc
            IncG<-cbind(IncG,Inc)
            # Name column names after time steps that were interpolated
        }
    }
    # Store in IncG
    rownames(IncG)<-cross_section[,1]
    colnames(IncG)<-seq(incr_matrix[1,3],incr_matrix[length(incr_matrix[,1]),3],Tstep)
    # Store subyearly data in matrix
    subincr_matrix<-cbind(colnames(IncG),p1xs,p2xs)
    Lsub<-list(IncG, subincr_matrix)
    return(Lsub)
}
