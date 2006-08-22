tsp_concorde <- function(x, options = "", exe = Sys.getenv("R_CONCORDE")) {

    if(!inherits(x, "dist")) stop(paste(sQuote("x"), "is not of class dist"))
    
    wd <- tempdir()
    temp_file <- tempfile(tmpdir = wd) 
    
    # file name needs to be unique
    tmp_file_in <- paste(temp_file, ".dat", sep = "")
    tmp_file_out <- paste(temp_file, ".sol", sep = "")
    
    # prepare data
    TSPLIB_write(x, tmp_file_in)

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

tsp_concorde_help <- function(exe = Sys.getenv("R_CONCORDE")) {
      system(paste(exe, "-h"))
}
  
