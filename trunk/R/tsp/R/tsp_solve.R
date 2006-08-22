tsp_solve <- function(dist, options = "", exe = Sys.getenv("R_CONCORDE"),  wd = "/tmp/") {

    # file name needs to be unique
    tmp_file_in <- paste(wd, "/tsp.dat", sep = "")
    tmp_file_out <- paste(wd, "/tsp.sol", sep = "")
    
    # prepare data
    TSPLIB_write(dist, tmp_file_in)

    dir <- getwd()
    setwd(wd)
    # do the call and read back result
    system(paste(exe, "-x", "-o", tmp_file_out , options, tmp_file_in))
    order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
    # remove number of nodes and add one (result starts with 0)
    order <- order[-1]+1 

    # tidy up
    unlink(c(tmp_file_in, tmp_file_out))
    setwd(dir)
    
    order
}

tsp_help <- function(exe = Sys.getenv("R_CONCORDE")) {
      system(paste(exe, "-h"))
}
  
