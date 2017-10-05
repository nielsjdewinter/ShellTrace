#' @export
pma <-
function(x,i,n){
	ts<-cbind(x[,1],x[,i])
	ts<-cbind(ts,rep(NA,length(x[,1])))
	for (a in ((n/2+0.5):(length(x[,1])+0.5-(n/2)))) {
		ts[a,3] = mean(ts[(a-(n/2)+1):(a+(n/2)),2],na.rm=T)
		}		
	return(ts)
}
