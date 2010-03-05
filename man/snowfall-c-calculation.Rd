\name{snowfall-calculation}

%% Separate alias for cross-references.
\alias{snowfall-calculation}

\alias{sfClusterMap}

\alias{sfClusterApply}
\alias{sfClusterApplyLB}
\alias{sfClusterApplySR}

\alias{sfLapply}
\alias{sfSapply}
\alias{sfApply}
\alias{sfRapply}
\alias{sfCapply}
\alias{sfMM}

\alias{sfRestore}

\title{Parallel calculation functions}
\usage{
sfClusterApply( x, fun, ... )
sfClusterApplyLB( x, fun, ... )
sfClusterApplySR( x, fun, ..., name="default", perUpdate=NULL, restore=sfRestore() )

sfClusterMap( fun, ..., MoreArgs = NULL, RECYCLE = TRUE )

sfLapply( x, fun, ... )
sfSapply( x, fun, ..., simplify = TRUE, USE.NAMES = TRUE )
sfApply( x, margin, fun, ... )
sfRapply( x, fun, ... )
sfCapply( x, fun, ... )

sfMM( a, b )

sfRestore()
}
\arguments{
  \item{x}{vary depending on function. See function details below.}
  \item{fun}{function to call}
  \item{margin}{vector speficying the dimension to use}
  \item{...}{additional arguments to pass to standard function}
  \item{simplify}{logical; see \code{sapply}}
  \item{USE.NAMES}{logical; see \code{sapply}}
  \item{a}{matrix}
  \item{b}{matrix}
  \item{RECYCLE}{see snow documentation}
  \item{MoreArgs}{see snow documentation}
  \item{name}{a character string indicating the name of this parallel
    execution. Naming is only needed if there are more than one call to
    \code{sfClusterApplySR} in a program.}
  \item{perUpdate}{a numerical value indicating the progress
    printing. Values range from 1 to 100 (no printing). Value means: any
    X percent of progress status is printed. Default (on given value \sQuote{NULL}) is 5).}
  \item{restore}{logical indicating whether results from previous runs
    should be restored or not. Default is coming from sfCluster. If
    running without sfCluster, default is FALSE, if yes, it is set to
    the value coming from the external program.}
}
\description{
  Parallel calculation functions. Execution is distributed automatically
  over the cluster.\cr
  Most of this functions are wrappers for \pkg{snow} functions, but all
  can be used directly in sequential mode.
}
\details{
  \code{sfClusterApply} calls each index of a given list on a seperate
  node, so length of given list must be smaller than nodes. Wrapper for
  \pkg{snow} function \code{clusterApply}.

  \code{sfClusterApplyLB} is a load balanced version of
  \code{sfClusterApply}. If a node finished it's list segment it
  immidiately starts with the next segment. Use this function in
  infrastructures with machines with different speed. Wrapper for
  \pkg{snow} function \code{clusterApplyLB}.

  \code{sfClusterApplySR} saves intermediate results and is able to
  restore them on a restart. Use this function on very long calculations
  or it is (however) foreseeable that cluster will not be able to finish
  it's calculations (e.g. because of a shutdown of a node machine). If
  your program use more than one parallised part, argument \code{name}
  must be given with a unique name for each loop. Intermediate data is
  saved depending on R-filename, so restore of data must be explicit
  given for not confusing changes on your R-file (it is recommended to
  only restore on fully tested programs). If restores,
  \code{sfClusterApplySR} continues calculation after the first non-null
  value in the saved list. If your parallized function can return null
  values, you probably want to change this.

  \code{sfLapply}, \code{sfSapply} and \code{sfApply} are parallel
  versions of \code{lapply}, \code{sapply} and \code{apply}. The first
  two use an list or vector as argument, the latter an array.

  \code{parMM} is a parallel matrix multiplication.  Wrapper for
  \pkg{snow} function \code{parMM}.

  \emph{\code{sfRapply} and \code{sfCapply} are not implemented atm.}
}
\keyword{package}
\seealso{
See snow documentation for details on commands:
\code{\link[snow]{snow-parallel}}
}
\examples{
\dontrun{
  restoreResults <- TRUE

  sfInit(parallel=FALSE)

  ## Execute in cluster or sequential.
  sfLapply(1:10, exp)

  ## Execute with intermediate result saving and restore on wish.
  sfClusterApplySR(1:100, exp, name="CALC_EXP", restore=restoreResults)
  sfClusterApplySR(1:100, sum, name="CALC_SUM", restore=restoreResults)

  sfStop()

  ##
  ## Small bootstrap example.
  ##
  sfInit(parallel=TRUE, cpus=2)

  require(mvna)
  data(sir.adm)

  sfExport("sir.adm", local=FALSE)
  sfLibrary(cmprsk)

  wrapper <- function(a) {
    index <- sample(1:nrow(sir.adm), replace=TRUE)
    temp <- sir.adm[index, ]
    fit <- crr(temp$time, temp$status, temp$pneu, failcode=1, cencode=0)
    return(fit$coef)
  }

  result <- sfLapply(1:100, wrapper)

  mean( unlist( rbind( result ) ) )
  sfStop()
}
}