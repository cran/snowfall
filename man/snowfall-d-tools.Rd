\name{snowfall-tools}

\alias{snowfall-tools}

\alias{sfLibrary}
\alias{sfSource}
\alias{sfExport}
\alias{sfExportAll}
\alias{sfRemove}
\alias{sfRemoveAll}
\alias{sfCat}

\alias{sfClusterSplit}
\alias{sfClusterCall}
\alias{sfClusterEval}
\alias{sfClusterEvalQ}

\alias{sfClusterSetupRNG}
\alias{sfClusterSetupRNGstream}
\alias{sfClusterSetupSPRNG}

\alias{sfTest}

\title{Cluster tools}
\usage{
sfLibrary( package, pos=2,
           lib.loc=NULL, character.only=FALSE,
           warn.conflicts=TRUE,
           keep.source=NULL,
           verbose=getOption("verbose"), version,
           stopOnError=TRUE )
sfSource( file, encoding = getOption("encoding"), stopOnError = TRUE )
sfExport( ..., list=NULL, local=TRUE, namespace=NULL, debug=FALSE, stopOnError = TRUE )
sfExportAll( except=NULL, debug=FALSE )

sfRemove( ..., list=NULL, master=FALSE, debug=FALSE )
sfRemoveAll( except=NULL, debug=FALSE, hidden=TRUE )

sfCat( ..., sep=" ", master=TRUE )

sfClusterSplit( seq )
sfClusterCall( fun, ..., stopOnError=TRUE )
sfClusterEval( expr, stopOnError=TRUE )

sfClusterSetupRNG( type="RNGstream", ... )
sfClusterSetupRNGstream( seed=rep(12345,6), ... )
sfClusterSetupSPRNG( seed=round(2^32*runif(1)), prngkind="default", para=0, ... )

sfTest()
}
\arguments{
  \item{expr}{expression to evaluate}
  \item{seq}{vector to split}
  \item{fun}{function to call}
  \item{list}{character vector with names of objects to export}
  \item{local}{a logical indicating if variables should taken from
    local scope(s) or only from global.}
  \item{namespace}{a character given a namespace where to search for the
    object.}
  \item{debug}{a logical indicating extended information is given upon
    action to be done (e.g. print exported variables, print context of
    local variables etc.).}
  \item{except}{character vector with names of objects not to
    export/remove}
  \item{hidden}{also remove hidden names (starting with a dot)?}
  \item{sep}{a character string separating elements in x}
  \item{master}{a logical indicating if executed on master as well}
  \item{...}{additional arguments to pass to standard function}
  \item{package}{name of the package. Check \code{library} for details.}
  \item{pos}{position in search path to load library.}
  \item{warn.conflicts}{warn on conflicts (see "library").}
  \item{keep.source}{see "library". Please note: this argument has only
    effect on R-2.x, starting with R-3.0 it will only be a placeholder
    for backward compatibility.}
  \item{verbose}{enable verbose messages.}
  \item{version}{version of library to load (see "library").}
  \item{encoding}{encoding of library to load (see "library").}
  \item{lib.loc}{a character vector describing the location of the R
    library trees to search through, or 'NULL'. Check \code{library} for
    details.}
  \item{character.only}{a logical indicating package can be assumed to
    be a character string. Check \code{library} for details.}
  \item{file}{filename of file to read. Check \code{source} for
    details}
  \item{stopOnError}{a logical indicating if function stops on
    failure or still returns. Default is \code{TRUE}.}
  \item{type}{a character determine which random number generator should
    be used for clusters. Allowed values are "RNGstream" for L'Ecuyer's
    RNG or "SPRNG" for Scalable Parallel Random Number Generators.}
  \item{para}{additional parameters for the RNGs.}
  \item{seed}{Seed for the RNG.}
  \item{prngkind}{type of RNG, see snow documentation.}
  
}
\description{
  Tools for cluster usage. Allow easier handling of cluster programming.
}
\details{
The current functions are little helpers to make cluster programming
easier.
All of these functions also work in sequential mode without any
further code changes.

\code{sfLibrary} loads an R-package on all nodes, including
master. Use this function if slaves need this library,
too. Parameters are identically to the R-build in funtion
\code{\link{library}}. If a relative path is given in \code{lib.loc},
it is converted to an absolute path.\\
As default \code{sfLibrary} stops on any error, but this can be
prevented by setting \code{stopOnError=FALSE}, the function is returning
\code{FALSE} then. On success \code{TRUE} is returned.

\code{sfSource} loads a sourcefile on all nodes, including master. Use
this function if the slaves need the code as well. Make sure the file
is accessible on all nodes under the same path. The loading is done
on slaves using \code{source} with fixes parameters:
\code{local=FALSE, chdir=FALSE, echo=FALSE}, so the files is loaded
global without changing of directory.\\
As default \code{sfSource} stops on any error, but this can be
prevented by setting \code{stopOnError=FALSE}, the function is returning
\code{FALSE} then. On success \code{TRUE} is returned.

\code{sfExport} exports variables from the master to all
slaves. Use this function if slaves need acccess to these variables as
well. \code{sfExport} features two execution modes: local and global.
If using local mode (default), variables for export are searched
backwards from current environment to \code{globalenv()}. Use this mode
if you want to export local variables from functions or other
scopes to the slaves. In global mode only global variables from master
are exported.\\
\emph{Note: all exported variables are \emph{global} on the slaves!}\\
If you have many identical named variables in different scopes, use
argument \code{debug=TRUE} to view the context the exported variable
is coming from.\\
Variables are given as their names or as a
character vector with their names using argument \code{list}.

\code{sfExportAll} exports all global variables from the master to all
slaves with exception of the
given list. Use this functions if you want to export mostly all
variables to all slaves.\\Argument \code{list} is a character vector
with names of the variables \emph{not} to export.

\code{sfRemove} removes a list of global (previous exported or
generated) variables from slaves and (optional) master.
Use this function if there are large further unused variables
left on slave. Basically this is only interesting if you have more than
one explicit parallel task in your program - where the danger is slaves
memory usage exceed.\\
If argument \code{master} is given, the variables are removed from
master as well (default is FALSE).\\
Give names of variables as arguments, or use argument \code{list}
as a character vector with the names. For deep cleaning of slave
memory use \code{sfRemoveAll}.

\code{sfRemoveAll} removes all global variables from the slaves. Use
this functions if you want to remove mostly all
variables on the slaves.  Argument \code{list} is a character vector
with names of the variables \emph{not} to remove.

\code{sfCat} is a debugging function printing a message on all slaves
(which appear in the logfiles).

\code{sfClusterSplit} splits a vector into one consecutive piece for
each cluster and returns as a list with length equal to the number of
cluster nodes. Wrapper for \pkg{snow} function \code{clusterSplit}.

\code{sfClusterCall} calls a function on each node and returns list of
results. Wrapper for \pkg{snow} function \code{clusterCall}.

\code{sfClusterEvalQ} evaluates a literal expression on all
nodes. Wrapper for \pkg{snow} function \code{clusterEvalQ}.

\code{sfTest} is a simple unit-test for most of the build in functions.
It runs tests and compares the results for the correct behavior. Note
there are some warnings if using, this is intended (as behavior for
some errors is tested, too). use this if you are not sure all nodes are
running your R-code correctly (but mainly it is implemented for
development).
}
\keyword{package}
\seealso{
See \pkg{snow} documentation for details on wrapper-commands:
\code{\link[snow]{snow-parallel}}
}
\examples{
\dontrun{
    sfInit( parallel=FALSE )

    ## Now works both in parallel as in sequential mode without
    ## explicit cluster handler.
    sfClusterEval( cat( "yummie\n" ) );

    ## Load a library on all slaves. Stop if fails.
    sfLibrary( tools )
    sfLibrary( "tools", character.only=TRUE )  ## Alternative.

    ## Execute in cluster or sequential.
    sfLapply( 1:10, exp )

    ## Export global Var
    gVar <- 99
    sfExport( "gVar" )

    ## If there are local variables with same name which shall not
    ## be exported.
    sfExport( "gVar", local=FALSE )

    ## Export local variables
    var1 <- 1    ## Define global
    var2 <- "a"

    f1 <- function() {
      var1 <- 2
      var3 <- "x"

      f2 <- function() {
        var1 <- 3

        sfExport( "var1", "var2", "var3", local=TRUE )
        sfClusterCall( var1 )    ## 3
        sfClusterCall( var2 )    ## "a"
        sfClusterCall( var3 )    ## "x"
      }

      f2()
    }

    f1()

    ## Init random number streams (snows functions, build upon
    ## packages rlecuyer/rsprng).
    sfClusterCall( runif, 4 )

    sfClusterSetupRNG()         ## L'Ecuyer is default.
    sfClusterCall( runif, 4 )

    sfClusterSetupRNG( type="SPRNG", seed = 9876)
    sfClusterCall( runif, 4 )

    ## Run unit-test on main functions.
    sfTest()
}
}