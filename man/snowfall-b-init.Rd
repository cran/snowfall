\name{snowfall-init}
\alias{snowfall-init}

\alias{sfInit}
\alias{sfStop}

\alias{sfParallel}
\alias{sfCpus}
\alias{sfNodes}
\alias{sfType}
\alias{sfIsRunning}
\alias{sfSocketHosts}
\alias{sfGetCluster}
\alias{sfSession}
\alias{sfSetMaxCPUs}

\title{Initialisation of cluster usage}
\usage{
sfInit( parallel=NULL, cpus=NULL, type=NULL, socketHosts=NULL, restore=NULL,
        slaveOutfile=NULL, nostart=FALSE, useRscript=FALSE )
sfStop( nostop=FALSE )

sfParallel()
sfIsRunning()
sfCpus()
sfNodes()
sfGetCluster()
sfType()
sfSession()
sfSocketHosts()
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
  \item{type}{Type of cluster. Can be 'SOCK', 'MPI', 'PVM' or 'NWS'. Default is 'SOCK'.}
  \item{socketHosts}{Host list for socket clusters. Only needed for
    socketmode (SOCK) and
    if using more than one machines (if using only your local machine
    (localhost) no list is needed).}
  \item{restore}{Globally set the restore behavior in the call
    \code{sfClusterApplySR} to the given value.}
  \item{slaveOutfile}{Write R slave output to this file. Default: no
    output (Unix: \code{/dev/null}, Windows: \code{:nul}). If
    using sfCluster this argument has no function, as slave logs are
    defined using sfCluster.}
  \item{useRscript}{Change startup behavior (snow>0.3 needed): use shell scripts or R-script for startup (R-scripts beeing the new variant, but not working with sfCluster.}
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

  Note that you should call \code{sfInit} before using any other function
  from \pkg{snowfall}, with the only exception \code{sfSetMaxCPUs}.
  If you do not call \code{sfInit} first, on calling any \pkg{snowfall}
  function \code{sfInit} is called without any parameters, which is
  equal to sequential mode in \pkg{snowfall} only mode or the settings from
  sfCluster if used with sfCluster.

  This also means, you cannot check if \code{sfInit} was called from
  within your own program, as any call to a function will initialize
  again. Therefore the function \code{sfIsRunning} gives you a logical
  if a cluster is running. Please note: this will not call \code{sfInit}
  and it also returns true if a previous running cluster was stopped via
  \code{sfStop} in the meantime.

  If you use \pkg{snowfall} in a package argument \code{nostart} is very
  handy if mainprogram uses \pkg{snowfall} as well. If set, cluster
  setup will be skipped and both parts (package and main program) use
  the same cluster.

  If you call \code{sfInit} more than one time in a program without
  explicit calling \code{sfStop}, stopping of the cluster will be
  executed automatically. If your R-environment does not cover required
  libraries, \code{sfInit} automatically switches to sequential mode
  (with a warning). Required libraries for parallel usage are \pkg{snow}
  and depending on argument \code{type} the libraries for the
  cluster mode (none for
  socket clusters, \pkg{Rmpi} for MPI clusters, \pkg{rpvm} for
  PVM clusters and \pkg{nws} for NetWorkSpaces).

  If using Socket or NetWorkSpaces, \code{socketHosts} can be used to
  specify the hosts you want to have your workers running.
  Basically this is a list, where any entry can be a plain character
  string with IP or hostname (depending on your DNS settings). Also
  for real heterogenous clusters for any host pathes are setable. Please
  look to the acccording \pkg{snow} documentation for details.
  If you are not giving an socketlist, a list with the required amount
  of CPUs on your local machine (localhost) is used. This would be the
  easiest way to use parallel computing on a single machine, like a
  laptop.

  Note there is limit on CPUs used in one program (which can be
  configured on package installation). The current limit are 32 CPUs. If
  you need a higher amount of CPUs, call \code{sfSetMaxCPUs}
  \emph{before} the first call to \code{sfInit}. The limit is set to
  prevent inadvertently request by single users affecting the cluster as
  a whole.

  Use \code{slaveOutfile} to define a file where to write the log
  files. The file location must be available on all nodes. Beware of
  taking a location on a shared network drive! Under *nix systems, most
  likely the directories \code{/tmp} and \code{/var/tmp} are not shared
  between the different machines. The default is no output file.
  If you are using \code{sfCluster} this
  argument have no meaning as the slave logs are always created in a
  location of \code{sfClusters} choice (depending on it's configuration).

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

  \code{sfType} returns the type of the current cluster backend (if
  used any). The value can be SOCK, MPI, PVM or NWS for parallel
  modes or "- sequential -" for sequential execution.

  \code{sfSocketHosts} gives the list with currently used hosts for
  socket clusters. Returns empty list if not used in socket mode (means:
  \code{sfType() != 'SOCK'}).
  
  \code{sfSetMaxCPUs} enables to set a higher maximum CPU-count for this
  program. If you need higher limits, call \code{sfSetMaxCPUs} before
  \code{sfInit} with the new maximum amount.
}
\keyword{package}
\seealso{
See snow documentation for details on commands:
\code{link[snow]{snow-cluster}}
}
\examples{
\dontrun{
  # Run program in plain sequential mode.
  sfInit( parallel=FALSE )
  stopifnot( sfParallel() == FALSE )
  sfStop()

  # Run in parallel mode overwriting probably given values on
  # commandline.
  # Executes via Socket-cluster with 4 worker processes on
  # localhost.
  # This is probably the best way to use parallel computing
  # on a single machine, like a notebook, if you are not
  # using sfCluster.
  # Uses Socketcluster (Default) - which can also be stated
  # using type="SOCK".
  sfInit( parallel=TRUE, cpus=4 )
  stopifnot( sfCpus() == 4 )
  stopifnot( sfParallel() == TRUE )
  sfStop()

  # Run parallel mode (socket) with 4 workers on 3 specific machines.
  sfInit( parallel=TRUE, cpus=4, type="SOCK",
          socketHosts=c( "biom7", "biom7", "biom11", "biom12" ) )
  stopifnot( sfCpus() == 4 )
  stopifnot( sfParallel() == TRUE )
  sfStop()

  # Hook into MPI cluster.
  # Note: you can use any kind MPI cluster Rmpi supports.
  sfInit( parallel=TRUE, cpus=4, type="MPI" )
  sfStop()

  # Hook into PVM cluster.
  sfInit( parallel=TRUE, cpus=4, type="PVM" )
  sfStop()

  # Run in sfCluster-mode: settings are taken from commandline:
  # Runmode (sequential or parallel), amount of nodes and hosts which
  # are used.
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
