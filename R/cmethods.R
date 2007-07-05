# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric( "setCMethod", function(f, sig, body, ...) standardGeneric("setCMethod") )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod( "setCMethod", signature(f="character", sig="list", body="list"),
  function(f, sig, body, includes="", language=c("C++", "C", "Fortran", "F95", "ObjectiveC", "ObjectiveC++"),
                      verbose=FALSE, convention=c(".Call", ".C", ".Fortran"), where=topenv(.GlobalEnv), ...) {
    if ( length(f) != length(sig) || length(sig) != length(body) )
      stop("number of signatures does not correspond to the number of code chunks")
    
    names(sig) <- f
    fns <- cfunction(sig, body, includes, language, verbose, convention)
    
    ## Let's try to create generics
    for ( i in 1:length(f) ) {
      generic <- paste( "setGeneric(\"", f[i], "\", function(", paste(names(sig[[i]]),collapse=", "), 
        ",...) standardGeneric(\"", f[i], "\"),where=where )", sep="")
      eval(parse(text=generic))
    }
    ## Let's try to define methods
    for ( i in 1:length(f) ) {
      setMethod(f[i], sig[[i]], fns[[i]], where=where)
    }
    if ( verbose ) {
      cat("\nThe following methods are now defined:\n")
      for ( i in 1:length(f) )
        showMethods(f[i])
    }
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod( "setCMethod", signature(f="character", sig="character", body="character"),
  function(f, sig, body, includes="", language=c("C++", "C", "Fortran", "F95", "ObjectiveC", "ObjectiveC++"),
                      verbose=FALSE, convention=c(".Call", ".C", ".Fortran"), where=topenv(.GlobalEnv), ...)
    setCMethod(f, list(sig), list(body), includes, language, verbose, convention, where=topenv(.GlobalEnv), ...)
)

