#' @export
Oyster_import_BMP <-
function(file_name){
    BMP<-bmp::read.bmp(file_name)
    return(BMP)
}
