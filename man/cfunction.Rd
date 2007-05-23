\name{cfunction}

\alias{cfunction}
\alias{setCMethod}
\alias{inline}

\concept{inline function call}

\title{ Inline C/C++ function calls from R }

\description{
  The function allows to dynamically define an R-function with inline C or C++ code
  using the \code{\link{.Call}} call mechanism of R.
}

\usage{
  cfunction(sig=character(), body=character(), includes=character(), cpp=TRUE, verbose=FALSE)

  ## S4 methods for signatures
  #  f='character', sig='list', body='list'
  #  f='character', sig='character', body='character'
  
  setCMethod(f, sig, body, ...)

  ## Further arguments:
  #  setCMethod(f, sig, body, includes="", cpp=TRUE, verbose=FALSE, where=topenv(.GlobalEnv), ...)
}}

\arguments{
  \item{f}{A character value if \code{sig} and \code{body} are character vectors
    or a character vector of the same length and the length of \code{sig} or
    \code{body} with the name(s) of methods to create.}
    
  \item{sig}{A match of formal argument names for the function with the
    character-string names of corresponding classes. In case of \code{setCMethod}
    with signature \code{list} -- a list of such character vectors. }

  \item{body}{ A character vector of C/C++ code starting after the C-function 
    opening curly bracket and ending before the C-function closing curly bracket, 
    brackets should not be inlcuded. In case of \code{setCMethod} with signature
    \code{list} -- a list of such character vectors. } 

  \item{includes}{ A character vector of additional includes and preprocessor
    statements etc that will be put between the R includes and the user function.}

  \item{cpp}{ A logical value that specifies if C++ code (and compiler)
    should be used over C. In this case all functions are additionally
    listed in the \code{extern "C"} clause.}
    
  \item{verbose}{ If \code{TRUE} outputs the full compilation output, the source
    code of the resulting C/C++ program and the definitions of all declared
    methods. If \code{FALSE}, the function is silent, but outputs compiler
    warning and error messages, as well as it outputs the source code of 
    the C/C++ program in case if the compilation fails. }

  \item{...}{ Reserved, passed to \code{\link{.Call}}.}
}

\value{
  \code{cfunction} returns an object of class \code{\link{function}}.
  
  \code{setCMethod} declares new methods with given names and signatures and
  returns invisible \code{NULL}.
}

\details{
  
  To declare multiple functions in the same library one can use \code{setCMethod}
  supplying lists of signatures and implementations. In this case, provide as
  many method names in \code{f} as you define methods. Avoid clashes when selecting
  names of the methods to declare, i.e. if you provide the same name several times
  you must ensure that signatures are different but can share the same generic!

  The C/C++ code in the \code{body} must start after the C-function opening curly 
  bracket and end before the C-function closing curly bracket, brackets should not 
  be inlcuded. The C/C++ function definition and signature will be automatically
  generated from the R-\code{signature} argument. C/C++ variables will carry the
  same name, therefore please avoid variables with dots.
  
  The \code{\link{.Call}} mechanism is used in this implementation and its
  result is returned directly as the result of the call of the generated function.
  
  As the last line of the generated C/C++ code a \code{return R_NilValue;}
  is added and a warning is generated in case the user has forgotten to provide a
  return value. To suppress the warning ans still return NULL, add the same
  line in your code.
  
  Special care is needed with types, memory allocation and protection -- exactly
  the same like if the C/C++ code were not inline: see the Writing R Extension
  manual for information on \code{\link{.Call}}.
  
  Attached R includes include \code{R.h, Rdefines.h} and 
  \code{R_ext\\Error.h}.
  
}

\seealso{ \code{
    \link{.Call}
}}

\examples{
code <- readLines(textConnection("
SEXP res;
int nprotect = 0, nx, ny, nz, x, y;
PROTECT(res = Rf_duplicate(a)); nprotect++;
nx = INTEGER(GET_DIM(a))[0];
ny = INTEGER(GET_DIM(a))[1];
nz = INTEGER(GET_DIM(a))[2];
double sigma2 = REAL(s)[0] * REAL(s)[0], d2 ;
double cx = REAL(centre)[0], cy = REAL(centre)[1], *data, *rdata;
for (int im = 0; im < nz; im++) {
  data = &(REAL(a)[im*nx*ny]); rdata = &(REAL(res)[im*nx*ny]);
  for (x = 0; x < nx; x++)
    for (y = 0; y < ny; y++) {
      d2 = (x-cx)*(x-cx) + (y-cy)*(y-cy);
      rdata[x + y*nx] = data[x + y*nx] * exp(-d2/sigma2);
    }
}
UNPROTECT(nprotect);
return res;
"))

x <- array(runif(50*50), c(50,50,1))

funx <- cfunction(signature(a="array", s="numeric", centre="numeric"), code)
res <- funx(a=x, s=10, centre=c(25,15))
image(res[,,1])

setCMethod("funy", signature(a="array", s="numeric", centre="numeric"), code, verbose=TRUE)
res <- funy(x, 10, c(35,35))
x11()
image(res[,,1])

}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

