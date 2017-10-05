#' @export
Oyster_Z_matrices <-
function(IncG, subincr_matrix){
    Z_mat<-as.numeric(rownames(IncG))
    for(t in 1:length(IncG[1,])){
        cat(paste("Calculating Z-values for increment: ",t),"\r")
        a<-subincr_matrix[t,14]
        b<-subincr_matrix[t,15]
        x1<-subincr_matrix[t,2]
        Z<-(a / b) * (b^2 - (as.numeric(rownames(IncG))-b-x1)^2)^(0.5)
        Z_mat<-cbind(Z_mat,Z)        
    }
    Z_mat<-Z_mat[,-1]
    colnames(Z_mat)<-colnames(IncG)
    rownames(Z_mat)<-rownames(IncG)
    Z_mat[is.na(Z_mat)]<-0
    dev.new();heatmap(Z_mat,main = "Visualization of Z matrix",Rowv = NA,Colv = NA,col=heat.colors(256), margins=c(0,0), labRow = F, labCol = F)
    return(Z_mat)
}
