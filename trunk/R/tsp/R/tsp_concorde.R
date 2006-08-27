tsp_concorde <- function(x, options = NULL){

    # get parameters
    clo         <- options$clo
    precision   <- options$precision
    exe         <- options$exe
    if(is.null(clo))    clo     <- ""
    if(is.null(precision))  precision   <- 6
    
    # see if the environment is set, otherwise we hope it can be found in PATH
    if(is.null(exe))        exe         <- Sys.getenv("R_CONCORDE")
    if(exe == "")           exe         <- "concorde"   
    
    # check parameters
    if(!inherits(x, "TSP")) x <- TSP(x)
    
    # get temp files
    wd <- tempdir()
    temp_file <- tempfile(tmpdir = wd) 
    
    # file name needs to be unique
    tmp_file_in <- paste(temp_file, ".dat", sep = "")
    tmp_file_out <- paste(temp_file, ".sol", sep = "")
    
    # prepare data
    write_TSPLIB(x, file = tmp_file_in, precision = precision)

    dir <- getwd()
    setwd(wd)
    on.exit(setwd(dir))
    
    # do the call and read back result
    system(paste(exe, "-x", "-o", tmp_file_out , clo, tmp_file_in))
    order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
    # remove number of nodes and add one (result starts with 0)
    order <- order[-1]+1 

    # tidy up
    unlink(c(tmp_file_in, tmp_file_out))
    
    order
}

tsp_concorde_help <- function(exe = Sys.getenv("R_CONCORDE")) {
      system(paste(exe, "-h"))
}
  
