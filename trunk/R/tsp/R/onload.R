
## export executable in default location to
## environment if available. (C)
.onLoad <- function(libname, pkgname) {
    export <- c(system.file("exec", "concorde", package = pkgname))
    if (export != "")
        Sys.putenv("R_CONCORDE" = export)
}
##
