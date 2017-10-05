#' @export
Oyster_plot_cross_section <-
function(cross_section){
    dev.new();
    plot(cross_section[,c(1,3)], type = "l")
    text(x=10,y=cross_section[100,3],pos=2,label=colnames(cross_section)[3])
    for(i in 3:(length(cross_section[1,])-1)){
        lines(cross_section[,c(1,i)])
        text(x=10,y=cross_section[100,i],pos=1,label=colnames(cross_section)[i])
    }
}
