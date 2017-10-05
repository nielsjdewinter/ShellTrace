#' @export
Oyster_Shell_height <-
function(cross_section, incr_matrix){
    # Create vectors of coordinates of first (leftmost) and last (rightmost) points on each increment and of shell length
    p1x<-0
    p1y<-0
    p2x<-0
    p2y<-0
    for(i in 4:length(cross_section[1,])){
        # Find left and right points of shell at the time of the increment
        D<-which(c(FALSE,(cross_section[,i]-cross_section[,3])==0)!=c((cross_section[,i]-cross_section[,3])==0,FALSE))
        first<-D[2]
        last<-D[3]-1
        firstX<-cross_section[first,1]
        firstY<-cross_section[first,i]
        lastX<-cross_section[last,1]
        lastY<-cross_section[last,i]
        
        # Correct when points from earlier increments result in greater shell heights (material of earlier increments is of course not removed from the shell) 
        if(p1x[length(p1x)]==0 | firstX<=p1x[length(p1x)]){
            p1x<-append(p1x,firstX)
            p1y<-append(p1y,firstY)
        } else {
            p1x<-append(p1x,p1x[length(p1x)])
            p1y<-append(p1y,p1y[length(p1x)])
        }
        if(p2x[length(p2x)]==0 | lastX>=p2x[length(p2x)]){
            p2x<-append(p2x,lastX)
            p2y<-append(p2y,lastY)
        } else {
            p2x<-append(p2x,p2x[length(p2x)])
            p2y<-append(p2y,p2y[length(p2x)])
        }
        # Set point of growth onset to leftmost edge of increment 1 (instead of point [0,0])
        p1x[1]<-p1x[2]
        p2x[1]<-p1x[2]
        p1y[1]<-p1y[2]
        p2y[1]<-p1y[2]
    }
    # Find shell height by Pythogorian Theorem between the two extreme points on the shell edges (P1 and P2)
    shell_height<-((p2x-p1x)^2+(p2y-p1y)^2)^0.5
    # Add all data to the increment matrix
    incr_matrix<-cbind(incr_matrix,p1x,p1y,p2x,p2y,shell_height)
    dev.new(); plot(incr_matrix[,c(2,11)], type="l")
    return(incr_matrix)
}
