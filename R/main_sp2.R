#' @export
write_tiff <- function(r, file) {
    rgdal::writeGDAL(r, file)
}
