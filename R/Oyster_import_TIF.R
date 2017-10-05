#' @export
Oyster_import_TIF <-
function(file_name){
    TIF<-tiff::readTIFF(file_name)
    return(TIF)
}
