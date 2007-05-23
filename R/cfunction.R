
cfunction <- function(sig=character(), body=character(), includes=character(), cpp=TRUE, verbose=FALSE) {
  f <- basename(tempfile())
  ## GENERATE THE CODE
  ## include R includes, also error
  code <- paste("#include <R.h>\n#include <Rdefines.h>\n",
                  "#include <R_ext/Error.h>\n", sep="");
  ## include further includes
  if ( length(includes) > 0 ) 
    code <- paste( code, includes, sep="\n")
  ## generate C-function sig from the original sig
  if ( length(sig) > 0 )
    funCsig <- paste("SEXP", names(sig),collapse=", " )
  else funCsig <- ""
  funCsig <- paste("SEXP", f, "(", funCsig, ")", sep=" ")
  ## add C export of the function
  if ( cpp ) code <- paste( code, "extern \"C\" {\n  ", funCsig, ";\n}\n\n", sep="")
  ## OPEN function 
  code <- paste( code, funCsig, " {\n", sep="")
  ## add code, split lines
  code <- paste( code, paste(body, collapse="\n"), sep="")
  ## CLOSE function, add return and warning in case the user forgot it
  code <- paste( code, "\n  warning(\"your C program does not return anything!\");\n  return R_NilValue;\n}\n", sep="");
  
  ## WRITE AND COMPILE THE CODE
  if ( .Platform$OS.type == "windows" ) {
    ## windows files
    dir <- gsub("\\\\", "/", tempdir())
    libCFile  <- paste(dir, "/", f, ".cpp", sep="")
    libLFile  <- paste(dir, "/", f, ".dll", sep="")
    libLFile2 <- paste(dir, "/", f, ".dll", sep="")
  } else {
    ## UNIX-alike build
    libCFile  <- paste(tempdir(), "/", f, ".cpp", sep="")
    libLFile  <- paste(tempdir(), "/", f, ".so", sep="")
    libLFile2 <- paste(tempdir(), "/", f, ".sl", sep="")
  }
  if ( !cpp ) libCFile <- sub(".cpp", ".c", libCFile)
  ## write the code
  write(code, libCFile)
  ## compile the code
  if ( file.exists(libLFile) ) file.remove( libLFile )
  if ( file.exists(libLFile2) ) file.remove( libLFile2 )
  compiled <- system(paste("R CMD SHLIB", libCFile), intern=!verbose)
  if ( !file.exists(libLFile) && file.exists(libLFile2) ) libLFile <- libLFile2
  if ( !file.exists(libLFile) ) {
    cat("\nERROR(s) in C/C++ code! Your program output looks as follows:\n")
    code <- strsplit(code, "\n")
    for (i in 1:length(code[[1]])) cat(format(i,width=3), ": ", code[[1]][i], "\n", sep="")
    stop( "Compilation ERROR, method(s) not created!" )
  }

  res <- function(...) {
    vals <- list(...)
    if ( any(names(vals) != names(sig)) ) 
      stop("argument list does not match earlier definition")
    if ( !is.loaded(libLFile) ) dyn.load(libLFile)
    .Call(f, ...)
  }
  if ( verbose ) {
    cat("C/C++ program source:\n")
    code <- strsplit(code, "\n")
    for (i in 1:length(code[[1]])) cat(format(i,width=3), ": ", code[[1]][i], "\n", sep="")
    print(res)
  }
  return( res )
}

