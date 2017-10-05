#` @export
Oyster_accumulation <-
function(el_time, subincr_matrix, npma=10){
    Growth_rate<-pma(subincr_matrix,19,npma)
    Growth_rate[is.na(Growth_rate)]<-0
    # Mass accumulation in grams/inc
    M_el_mat<-t(el_time)/100*Growth_rate[,3]
    # Cumulative mass of elements in grams
    M_el_mat_c<-apply(t(M_el_mat),1,cumsum)
    rownames(M_el_mat)<-rownames(subincr_matrix)
    rownames(M_el_mat_c)<-rownames(subincr_matrix)
    # Plot result
    dev.new()
    par(mfrow=c(2,2))
    plot(M_el_mat[,which(colnames(M_el_mat)=="Sr")]/M_el_mat[,which(colnames(M_el_mat)=="Ca")],type="l",col="blue",xlab="days",ylab="Sr/Ca (g/g)", main="Accumulation rate")
    plot(M_el_mat[,which(colnames(M_el_mat)=="Fe")]/M_el_mat[,which(colnames(M_el_mat)=="Ca")],type="l",col="red",xlab="days",ylab="Fe/Ca (g/g)", main="Accumulation rate")
    plot(M_el_mat_c[,which(colnames(M_el_mat_c)=="Sr")],type="l",col="blue",xlab="days",ylab="Sr (g)", main="Accumulation")
    plot(M_el_mat_c[,which(colnames(M_el_mat_c)=="Fe")],type="l",col="red",xlab="days",ylab="Fe (g)", main="Accumulation")
    return(list(M_el_mat,M_el_mat_c))
}