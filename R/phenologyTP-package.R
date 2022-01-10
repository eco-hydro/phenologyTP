#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


#' @import lattice.layers
.onLoad <- function(libname, pkgname) {
    # suppressMessages
    # suppressWarnings
    # suppressMessages({
    #     library(magrittr)
    #     # library(lattice)
    #     library(devtools)
    # })
    library(lattice.layers)

    lattice.layers::init_lattice()
    lattice.layers::set_font()
    # init_lattice()
    invisible()
}
