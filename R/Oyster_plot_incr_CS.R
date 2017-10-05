#' @export
Oyster_plot_incr_CS <-
function(IncG, incr_matrix, Tstep=1){
    dev.new(); plot(rownames(IncG),IncG[,1], type = "l");
    for(i in 2:length(incr_matrix[,2])){
        lines(rownames(IncG),IncG[,(incr_matrix[i,2]/Tstep)])
        lines(rownames(IncG),IncG[,incr_matrix[i-1,2]/Tstep+round((incr_matrix[i,2]/Tstep-incr_matrix[i-1,2]/Tstep)/2)],col="100")
    }
}