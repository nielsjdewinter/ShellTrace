#' @export
Oyster_ellipse_parameters <-
function(subincr_matrix, IncG, Oyster_height, Oyster_length){
    # r_ellipse is the ratio between length and width of the ellipse, which is fixed for the shell based on the measured values of shell length and maximum width
    r_ellipse<-Oyster_length/Oyster_height
    L_ellipse_acc<-vector()
    W_ellipse<-vector()
    a_ellipse<-vector()
    b_ellipse<-vector()
    for(t in 1:(length(IncG[1,]))){
        # L_ellipse = maximum X-extent of the shell
        #        = p2x - p1x
        # W_ellipse = maximum Y-extent of the shell
        #        = r_ellipse * L_ellipse
        # a = 0.5 * L_ellipse
        # b = 0.5 * W_ellipse
        L_ellipse_acc<-append(L_ellipse_acc,(subincr_matrix[t,3]-subincr_matrix[t,2]))
        W_ellipse<-append(W_ellipse,r_ellipse*(subincr_matrix[t,3]-subincr_matrix[t,2]))
        a_ellipse<-append(a_ellipse,0.5*r_ellipse*(subincr_matrix[t,3]-subincr_matrix[t,2]))
        b_ellipse<-append(b_ellipse,0.5*(subincr_matrix[t,3]-subincr_matrix[t,2]))
    }
    subincr_matrix<-cbind(subincr_matrix, W_ellipse, L_ellipse_acc, a_ellipse, b_ellipse)
    return(subincr_matrix)
}
