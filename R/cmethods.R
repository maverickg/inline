# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric( "setCMethod", function(f, sig, body, ...) standardGeneric("setCMethod") )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod( "setCMethod", signature(f="character", sig="list", body="list"),
  function(f, sig, body, includes="", cpp=TRUE, verbose=FALSE, where=topenv(.GlobalEnv), ...) {
    if ( length(f) != length(sig) || length(sig) != length(body) )
      stop("number of signatures does not correspond to the number of code chunks")
    ## GENERATE IDs
    ## c-functions ids
#    fIDs <- sub("file", "fun_", sapply(1:length(sig), function(x) basename(tempfile())) )
    fIDs <- f
    fSigs <- sapply(sig, function(x) ifelse( is.na(x)||is.null(x)||nchar(x)<1, "void", 
      paste("SEXP", names(x), collapse=", ")) )
    fSigs <- paste( "SEXP ", fIDs, "(", fSigs, ")", sep="")
    ## library c/cpp and so file ids
    libID <- basename(tempfile())
    if ( .Platform$OS.type == "windows" ) {
      ## windows files
      dir <- gsub("\\\\", "/", tempdir())
      libCFile  <- paste(dir, "/", libID, ".cpp", sep="")
      libLFile  <- paste(dir, "/", libID, ".dll", sep="")
      libLFile2 <- paste(dir, "/", libID, ".dll", sep="")
    } else {
      ## UNIX-alike build
      libCFile  <- paste(tempdir(), "/", libID, ".cpp", sep="")
      libLFile  <- paste(tempdir(), "/", libID, ".so", sep="")
      libLFile2 <- paste(tempdir(), "/", libID, ".sl", sep="")
    }
    if ( !cpp ) libCFile <- sub(".cpp", ".c", libCFile)
    ## GENERATE CODE
    ## includes
    code <- paste("#include <R.h>", "#include <Rdefines.h>", "#include <R_ext/Error.h>\n", sep="\n")
    if ( nchar(includes) > 0 ) code <- paste(code, includes, sep="\n")
    ## C exports if C++ code
    if ( cpp ) {
      code <- paste(code, "extern \"C\" {\n", sep="\n")
      for ( s in fSigs ) code <- paste(code, "  ", s, ";\n", sep="")
      code <- paste(code, "}\n", sep="")
    }
    ## functions
    for ( i in seq_along(sig) ) {
      code <- paste(code, fSigs[i], " {", sep="")
      chunk <- unlist(strsplit(body[[i]], "\n"))
      chunk <- paste("  ", chunk, sep="")
      chunk <- paste(chunk, collapse="\n")
      code <- paste(code, chunk, sep="\n")
      code <- paste(code, "\n  warning(\"your C program does not return anything!\");\n  return R_NilValue;\n}\n\n", sep="")
    }
    ## WRITE CODE AND COMPILE
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
    ## Let's try to create generics
    for ( i in 1:length(f) ) {
      generic <- paste( "setGeneric(\"", f[i], "\", function(", paste(names(sig[[i]]),collapse=", "), 
        ",...) standardGeneric(\"", f[i], "\"),where=where )", sep="")
      eval(parse(text=generic))
    }
    ## Let's try to define methods
    for ( i in 1:length(f) ) {
      fun <- paste( "function(", paste(names(sig[[i]]), collapse=",", sep=""), ",...)",
        "{ if ( !is.loaded(libLFile) ) dyn.load(libLFile); .Call(f[i],", 
        paste(names(sig[[i]]), collapse=",", sep=""), ", ...) }", sep="")
     setMethod(f[i], sig[[i]], eval(parse(text=fun)), where=where)
    }
    if ( verbose ) {
      cat("C/C++ program source:\n")
      code <- strsplit(code, "\n")
      for (i in 1:length(code[[1]])) cat(format(i,width=3), ": ", code[[1]][i], "\n", sep="")
      cat("\nThe following methods are now defined:\n")
      for ( i in 1:length(f) )
        showMethods(f[i])
    }
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod( "setCMethod", signature(f="character", sig="character", body="character"),
  function(f, sig, body, includes="", cpp=TRUE, verbose=FALSE,...)
    setCMethod(f, list(sig), list(body), includes, cpp, verbose, where=topenv(.GlobalEnv), ...)
)

