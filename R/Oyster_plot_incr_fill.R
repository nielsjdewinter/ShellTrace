#' @export
Oyster_plot_incr_fill <-
function(IncG){
    IncGt<-IncG
    IncGt<-IncGt[rowSums(IncGt[,-1])>0,]
    incr<-round(length(as.numeric(colnames(IncG)))/10,0)
    dev.new(width=10,height=10)
    plot(rownames(IncGt),IncGt[,1], type = "l", col=NA, ylim = c(0,max(IncGt[,1])+1))
    lines(rownames(IncGt),IncGt[,1], col="black",lwd=3)
    palette<-heat.colors(length(colnames(IncGt)))
    for(i in 2:length(IncGt[1,])){
        polygon(c(rownames(IncGt),rev(rownames(IncGt))),c(IncGt[,i],rev(IncGt[,i-1])),border=NA,col=palette[i])
        if(i%%incr==0){
            lines(rownames(IncGt),IncGt[,i], col="black",lwd=1)
        }
    }
    lines(rownames(IncGt),IncGt[,length(IncGt[1,])], col="black",lwd=3)
}