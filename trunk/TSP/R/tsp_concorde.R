## interface to the Concorde algorithm (can only handle TSP)

tsp_concorde <- function(x, control = NULL){

    ## get parameters
    clo         <- if(!is.null(control$clo))        control$clo         else ""
    precision   <- if(!is.null(control$precision))  control$precision   else 6
    exe         <- .find_exe(control$exe, "concorde")
   
    
    ## check x
    if(!inherits(x, "TSP")) stop("Concorde only solves symmetric TSPs.")
    
    ##cat("\nrunning Concorde:\n")
    

    ## get max (excluding inf) to check for possible integer overflows
    max_x <- x
    max_x[is.infinite(x)] <- NA
    max_x <- max(max_x, na.rm = TRUE)
    
    if(n_of_cities(x) < 10){
        ## <10 cities: concorde can only handle max 2^15
        MAX <- 2^15
        if(max_x > MAX) stop("Concorde can only handle distances < 2^15 for less than 10 cities")
        
        prec <- floor(log10(MAX / max_x))
        if(prec < precision) {
            precision <- prec
            warning(paste("Concorde can only handle distances < 2^15 for",
                    "less than 10 cities. Reducing precision to", 
                    precision), immediate. = TRUE)
        }
    }else{
        ## regular constraint on integer is 2^31 - 1    
        MAX <- 2^31 - 1

        prec <- floor(log10(MAX / max_x / n_of_cities(x)))
        if(prec < precision) {
            precision <- prec
            warning(paste("Reducing precision for Concorde to",
                    precision), immediate. = TRUE)
        }
    }
    
    ## get temp files
    wd <- tempdir()
    temp_file <- tempfile(tmpdir = wd) 
    
    ## file name needs to be unique
    tmp_file_in  <- paste(temp_file, ".dat", sep = "")
    tmp_file_out <- paste(temp_file, ".sol", sep = "")
    
    ## prepare data
    write_TSPLIB(x, file = tmp_file_in, precision = precision)

    ## change working directory
    dir <- getwd()
    setwd(wd)
    on.exit(setwd(dir))
    
    ## do the call and read back result
    ## we do not check return values of concorde since they are not
    ## very consistent
    system(paste(exe, "-x", "-o", tmp_file_out , clo, tmp_file_in))
    
    if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading Concorde's output. Is Concorde properly installed?")
    ##else cat("Concorde done.\n")
    
    order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
    ## remove number of nodes and add one (result starts with 0)
    order <- order[-1] + as.integer(1) 

    ## tidy up
    unlink(c(tmp_file_in, tmp_file_out))
    
    order
}

## interface to the Concorde's Chained Lin-Kernighan algorithm 
## (can only handle TSP)

tsp_linkern <- function(x, control = NULL){

    ## get parameters
    clo         <- if(!is.null(control$clo))        control$clo         else ""
    precision   <- if(!is.null(control$precision))  control$precision   else 6
    exe         <- .find_exe(control$exe, "linkern")
    verbatim    <- if(!is.null(control$verbatim))   control$verbatim    
                            else FALSE 
   
    verbatim    <- if(!verbatim) "-Q" else "" 
    
    ## check x
    if(!inherits(x, "TSP")) stop("Concorde's LK only solves symmetric TSPs.")
    
    ## get max (excluding inf) to check for possible integer overflows
    max_x <- x
    max_x[is.infinite(x)] <- NA
    max_x <- max(max_x, na.rm = TRUE)

    MAX <- 2^31 - 1

    prec <- floor(log10(MAX / max_x / n_of_cities(x)))
    if(prec < precision) {
        precision <- prec
        warning(paste("Reducing precision for Concorde to",
                precision), immediate. = TRUE)
    }

    ## get temp files
    wd <- tempdir()
    temp_file <- tempfile(tmpdir = wd) 
    
    ## file name needs to be unique
    tmp_file_in  <- paste(temp_file, ".dat", sep = "")
    tmp_file_out <- paste(temp_file, ".sol", sep = "")
    
    ## prepare data
    write_TSPLIB(x, file = tmp_file_in, precision = precision)

    ## change working directory
    dir <- getwd()
    setwd(wd)
    on.exit(setwd(dir))
    
    ## do the call and read back result
    ## we do not check return values of concorde since they are not
    ## very consistent
    system(paste(exe, verbatim, "-o", tmp_file_out , clo, tmp_file_in))
    
    if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading Linkern's output. Is Linkern properly installed?")
    ##else cat("Concorde done.\n")
    
    order <- read.table(tmp_file_out)[,1]
    ## remove number of nodes and add one (result starts with 0)
    order <- order + as.integer(1) 

    ## tidy up
    unlink(c(tmp_file_in, tmp_file_out))
    
    order
}

## get help page
help_concorde <- function(exe = NULL) {
      system(paste(.find_exe(exe, "concorde"), ""))
}

help_linkern <- function(exe = NULL) {
      system(paste(.find_exe(exe, "linkern"), ""))
}
  
## helper to find the concorde executable
.find_exe <- function(exe = NULL, prog) {
    ## use environment variable?
    #if(is.null(exe))    exe <- Sys.getenv("R_CONCORDE")
    ## last resort (hopefully it is in the PATH)
    #if(exe == "")       exe <- prog
    if(is.null(exe))       exe <- prog
    
    exe
}

