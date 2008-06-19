\name{snowfall-init}
\alias{sfInit}
\alias{sfStop}

\alias{sfParallel}
\alias{sfCpus}
\alias{sfNodes}
\alias{sfGetCluster}
\alias{sfSession}
\alias{sfSetMaxCPUs}

\title{Initialisation of cluster usage}
\usage{
sfInit( parallel=NULL, cpus=NULL, nostart=FALSE )
sfStop( nostop=FALSE )

sfParallel()
sfCpus()
sfNodes()
sfGetCluster()
sfSession()
sfSetMaxCPUs( number=32 )
}
\arguments{
  \item{parallel}{Logical determinating parallel or sequential
    execution. If not set values from commandline are taken.}
  \item{cpus}{Numerical amount of CPUs requested for the cluster. If
    not set, values from the commandline are taken.}
  \item{nostart}{Logical determinating if the basic cluster setup should
    be skipped. Needed for nested use of \pkg{snowfall} and usage in
    packages.}
  \item{nostop}{Same as noStart for ending.}
  \item{number}{Amount of maximum CPUs useable.}
}
\description{
  Initialisation and organisation code to use \pkg{snowfall}.
}
\details{
  \code{sfInit} initialisise the usage of the \pkg{snowfall} functions
  and - if running in parallel mode - setup the cluster and
  \pkg{snow}. If using
  \code{sfCluster} management tool, call this without arguments. If
  \code{sfInit} is called with arguments, these overwrite
  \code{sfCluster} settings. If running parallel, \code{sfInit}
  set up the
  cluster by calling \code{makeCluster} from \pkg{snow}. If using with
  \code{sfCluster}, the initialisation also contains management of
  lockfiles. If this function is called more than once and current
  cluster is yet running, \code{sfStop} is called automatically.

  Note that you MUST call \code{sfInit} before using any other function
  from \pkg{snowfall}, with the only exception \code{sfSetMaxCPUs}.

  If you use \pkg{snowfall} in a package argument \code{nostart} is very
  handy if mainprogram uses \pkg{snowfall} as well. If set, cluster
  setup will be skipped and both parts (package and main program) use
  the same cluster.

  If you call \code{sfInit} more than one time in a program without
  explicit calling \code{sfStop}, stopping of the cluster will be
  executed automatically. If your R-environment does not cover required
  libraries, \code{sfInit} automatically switches to sequential mode
  (with a warning). Required libraries for parallel usage are \pkg{snow}
  and \pkg{Rmpi}.

  Note there is limit on CPUs used in one program (which can be
  configured on package installation). The current limit are 32 CPUs. If
  you need a higher amount of CPUs, call \code{sfSetMaxCPUs}
  \emph{before} the first call to \code{sfInit}. The limit is set to
  prevent inadvertently request by single users affecting the cluster as
  a whole. 

  \code{sfStop} stop cluster. If running in parallel mode, the LAM/MPI
  cluster is shut down.
  
  \code{sfParallel}, \code{sfCpus} and \code{sfSession} grant access to
  the internal state of the currently used cluster.
  All three can be configured via commandline and especially with
  \code{sfCluster} as well, but given
  arguments in \code{sfInit} always overwrite values on commandline.
  The commandline options are \option{--parallel} (empty option. If missing,
  sequential mode is forced), \option{--cpus=X} (for nodes, where X is a
  numerical value) and \option{--session=X} (with X a string).

  \code{sfParallel} returns a
  logical if program is running in parallel/cluster-mode or sequential
  on a single processor.

  \code{sfCpus} returns the size of the cluster in CPUs
  (equals the CPUs which are useable). In sequential mode \code{sfCpus}
  returns one. \code{sfNodes} is a deprecated similar to \code{sfCpus}.

  \code{sfSession} returns a string with the
  session-identification. It is mainly important if used with the
  \code{sfCluster} tool.
 
  \code{sfGetCluster} gets the \pkg{snow}-cluster handler. Use for
  direct calling of \pkg{snow} functions.

  \code{sfSetMaxCPUs} enables to set a higher maximum CPU-count for this
  program. If you need higher limits, call \code{sfSetMaxCPUs} before
  \code{sfInit} with the new maximum amount.
}
\keyword{package}
\seealso{
See snow documentation for details on commands:
\code{\link[snow:snow-package]{snow}}
}
\examples{
\dontrun{
  # Run program in plain sequential mode.
  sfInit( parallel=FALSE )
  stopifnot( sfParallel() == FALSE )
  sfStop()

  # Run in parallel mode overwriting probably given values on
  # commandline. Hooks in running LAM cluster, if none is running,
  # snow is spawning one on localhost.
  sfInit( parallel=TRUE, nodes=10 )
  stopifnot( sfCpus() == 10 )
  stopifnot( sfParallel() == TRUE )
  sfStop()

  # Run in sfCluster-mode: settings are taken from commandline.
  sfInit()

  # Session-ID from sfCluster (or XXXXXXXX as default)
  session <- sfSession()

  # Calling a snow function: cluster handler needed.
  parLapply( sfGetCluster(), 1:10, exp )

  # Same using snowfall wrapper, no handler needed.
  sfLapply( 1:10, exp )

  sfStop()
}
}

