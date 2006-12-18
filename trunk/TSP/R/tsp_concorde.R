## interface to the Concorde algorithm (can only handle TSP)
   

## helper to shift the values all between [0, max]
.concorde_fix_inf <- function(x){
    shift <- 0

    ## find infinite values
    x_inf <- is.infinite(x)
    x_wo_inf <- x
    if(any(x_inf)) x_wo_inf[x_inf] <- NA

    min_x <- min(x_wo_inf, na.rm = TRUE)
    max_x <- max(x_wo_inf, na.rm = TRUE)

    ## remove neg. values (we just add shift)
    shift <- if(min_x < 0) -min_x else 0

    ## make space for -Inf (we add max_x)
    if(any(x[x_inf] == -Inf)) shift <- shift + max_x 

    max_x <- max_x + shift

    if(shift > 0) cat("Adding", shift, 
        "to the distances to avoid neg. values.\n")

    list(shift = shift, max_x = max_x)
}
    
tsp_concorde <- function(x, control = NULL){

    ## get parameters
    clo         <- if(!is.null(control$clo))        control$clo         else ""
    precision   <- if(!is.null(control$precision))  control$precision   else 6
    exe         <- .find_exe(control$exe, "concorde")
   
    ## check x
    if(!inherits(x, "TSP")) stop("Concorde only solves symmetric TSPs.")


    ## fix inf and neg. values
    fix <- .concorde_fix_inf(x)

    shift <- fix$shift
    max_x <- fix$max_x
    x <- x + shift
    
    ## get max (excluding inf) to check for possible integer overflows
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
    
    ## prepare data (neg_inf = 0 so everything is > 0)
    write_TSPLIB(x, file = tmp_file_in, 
        precision = precision, neg_inf = 0)

    ## change working directory
    dir <- getwd()
    setwd(wd)
    on.exit(setwd(dir))
    
    ## do the call and read back result
    ## we do not check return values of concorde since they are not
    ## very consistent
    system(paste(exe, "-x", "-o", tmp_file_out , clo, tmp_file_in))
    
    if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading Concorde's output. Is concorde properly installed?")
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
    
    
    ## fix inf and neg. values
    fix <- .concorde_fix_inf(x)

    shift <- fix$shift
    max_x <- fix$max_x
    x <- x + shift
    

    ## check for possible overflows
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
    
    ## prepare data (neg_inf = 0 so everything is > 0)
    write_TSPLIB(x, file = tmp_file_in, 
        precision = precision, neg_inf = 0)

    ## change working directory
    dir <- getwd()
    setwd(wd)
    on.exit(setwd(dir))
    
    ## do the call and read back result
    ## we do not check return values of concorde since they are not
    ## very consistent
    system(paste(exe, verbatim, "-o", tmp_file_out , clo, tmp_file_in))
    
    if(!file.access(tmp_file_out) == 0) 
    stop("Problems with reading linkern's output. Is linkern properly installed?")
    ##else cat("Concorde done.\n")
    
    order <- read.table(tmp_file_out)[,1]
    ## remove number of nodes and add one (result starts with 0)
    order <- order + as.integer(1) 

    ## tidy up
    unlink(c(tmp_file_in, tmp_file_out))
    
    order
}

## get help page
concorde_help <- function(exe = NULL) {
      system(paste(.find_exe(exe, "concorde"), ""))
}

linkern_help <- function(exe = NULL) {
      system(paste(.find_exe(exe, "linkern"), ""))
}

## path
concorde_path <- local({
    .path <- NULL
    function(path){
        if(missing(path)) .path else {
            .path <<- path
            if(!is.null(path)) {
                ex <- c(list.files(path, pattern = "concorde"),
                    list.files(path, pattern = "linkern"))
                if(length(ex) < 1)
                warning(paste("no executable (concorde, linkern) found in", 
                        path))
                cat("found:", ex, "\n")
            }
            invisible(.path)

        }
    }
})


## helper to find the concorde executable
.find_exe <- function(exe = NULL, prog) {
    ## if not specified
    if(is.null(exe)) {
        ## was the path set ?
        if(!is.null(concorde_path())) 
        exe <- paste(concorde_path(), .Platform$file.sep, prog, sep ="")
        ## no, so it must be in the systems execution path
        else exe <- prog
    }
    exe
}

