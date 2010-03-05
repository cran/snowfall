\name{snowfall-package}
\alias{snowfall-package}
\alias{snowfall}
\docType{package}
\title{Toplevel useability wrapper for snow to make parallel programming even
  more easy and comfortable.
  All functions are able to run without cluster in sequential mode.
  Also snowfall works as connector to the cluster management
  program sfCluster, but can also run without it.}
\description{
  \pkg{snowfall} is designed to make setup and usage of \pkg{snow} more
  easier. It also is made ready to work together with \code{sfCluster},
  a ressource management and runtime observation tool for
  R-cluster usage.
}
\details{
\tabular{ll}{
Package: \tab snowfall\cr
Type: \tab Package\cr
Version: \tab 1.61\cr
Date: \tab 2008-11-01\cr
License: \tab GPL\cr
}
}
\section{Initialisation}{Initalisation via \code{sfInit} must be called
  before the usage
  of any of the \pkg{snowfall} internal functions. \code{sfStop} stopps
  the current cluster. Some additional functions give access to build-in
  functions (like \code{sfParallel}, \code{sfCpus} etc.).
}
\section{Calculations}{The are plenty of function to execute parallel
  calculations via \pkg{snowfall}. Most of them are wrappers to the
  according \pkg{snow} functions, but there are additional functions as
  well. Most likely the parallel versions of the R-buildin applies are
  interesting: \code{sfLapply}, \code{sfSapply} and \code{sfApply}. For
  better cluster take a look at the load balanced
  \code{sfClusterApplyLB} and the function with restore possibilities:
  \code{sfClusterApplySR}.
}
\section{Tools}{Various tools allow an easier access to parallel
  computing: \code{sfLibrary} and \code{sfSource} for loading code on
  the cluster, \code{sfExport}, \code{sfExportAll}, \code{sfRemoveAll}
  and \code{sfRemoveAll} for variable sperading on the cluster. And some
  more.
}
\section{sfCluster}{\pkg{snowfall} is also the R-connector to the
  cluster management program \code{sfCluster}. Mostly all of the
  communication to this tool is done implicit and directly affecting the
  initialisation via \code{sfInit}. Using \code{sfCluster} makes the
  parallel programming with \pkg{snowfall} even more practicable in real
  life environments.

  For futher informations about the usage of \code{sfCluster} look at
  its documentation.
}
\author{
Jochen Knaus

Maintainer:
Jochen Knaus <jo@imbi.uni-freiburg.de>,
}
\references{
  \pkg{snow} (Simple Network of Workstations):\cr
  http://cran.r-project.org/src/contrib/Descriptions/snow.html\cr\cr

  \code{sfCluster} (Unix management tool for \pkg{snowfall} clusters):\cr
  http://www.imbi.uni-freiburg.de/parallel\cr
}
\keyword{package}
\seealso{
  Snowfall Initialisation: \code{\link{snowfall-init}}\cr
  Snowfall Calculation: \code{\link{snowfall-calculation}}\cr
  Snowfall Tools: \code{\link{snowfall-tools}}\cr

  Optional links to other man pages, e.g. \code{\link[snow]{snow-cluster}}
}
\examples{
\dontrun{
  # Init Snowfall with settings from sfCluster
  ##sfInit()

  # Init Snowfall with explicit settings.
  sfInit( parallel=TRUE, cpus=2 )

  if( sfParallel() )
    cat( "Running in parallel mode on", sfCpus(), "nodes.\n" )
  else
    cat( "Running in sequential mode.\n" )

  # Define some global objects.
  globalVar1 <- c( "a", "b", "c" )
  globalVar2 <- c( "d", "e" )
  globalVar3 <- c( 1:10 )
  globalNoExport <- "dummy"

  # Define stupid little function.
  calculate <- function( x ) {
    cat( x )
    return( 2 ^ x )
  }

  # Export all global objects except globalNoExport
  # List of exported objects is listed.
  # Work both parallel and sequential.
  sfExportAll( except=c( "globalNoExport" ) )

  # List objects on each node.
  sfClusterEvalQ( ls() )

  # Calc something with parallel sfLappy
  cat( unlist( sfLapply( globalVar3, calculate ) ) )

  # Remove all variables from object.
  sfRemoveAll( except=c( "calculate" ) )
}
}
