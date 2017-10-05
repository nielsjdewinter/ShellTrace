#' @export
Oyster_subincr_shell_height <-
function(subincr_matrix, IncG, Xstep=0.1){
    print("Calculating subincremental shell height")
    # Create matrices for first (leftmost) and last (rightmost) values of increment lines
    p1x<-subincr_matrix[,2]
    p2x<-subincr_matrix[,3]
    firstl<-round(p1x/Xstep+0.5,0)+1
    lastl<-round(p2x/Xstep-0.5,0)+1
    p1y<-0
    p2y<-0
    shell_height<-0
    for(t in 1:(length(IncG[1,])-1)){
        p1y<-append(p1y,IncG[firstl[t],t])
        p2y<-append(p2y,IncG[lastl[t],t])
        # Find shell height by Pythogorian Theorem between the two extreme points on the shell edges (P1 and P2)
        L<-sqrt((p2x[t]-p1x[t])^2+(p2y[t]-p1y[t])^2)
        if(L<shell_height[length(shell_height)]){
            L<-shell_height[length(shell_height)]
        }
        shell_height<-append(shell_height,L)
    }
    subincr_matrix<-cbind(subincr_matrix,p1y,p2y,shell_height,firstl,lastl)
    dev.new(); plot(subincr_matrix[,c(1,8)], type = "l")
    return(subincr_matrix)
}