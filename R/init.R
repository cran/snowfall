##*****************************************************************************
## Functions for initialisizing the Cluster.
##
## Attention: this package does nasty things with explicit sideeffects (not
## only using "require" and "options" etc.)...
##
## "Nodes" and "CPUs" are used identical (unbeautiful).
##
## PARAMETER:  [Boolean parallel - overwrite CMDline settings],
##             [Int nodes        - overwrites commandline settings / DEPRECATED]
##             [Int cpus         - overwrites commandline settings]
##             [Boolean nostart  - If set, no cluster start will be run.
##                                 Needed for nested usage of snowfall]
## RETURN:     Boolean TRUE
##*****************************************************************************
sfInit <- function( parallel=NULL,
##                    nodes=NULL,
                    cpus=NULL,
                    nostart=FALSE ) {
  ## Flag for detection of reconnect (means: non-first calls to sfInit())
  reconnect <- FALSE

  ## Saves users from many own if-clauses probably.
  if( nostart ) return( TRUE );
  
  ## Global Vars made global. Prototype is sfOption from sysdata.rda
  if( !exists( ".sfOption", env=globalenv() ) ) {
    if( !exists( "sfOption" ) )
      warning( "Preconfigured variable sfOption is missing?" )

    ## Reassign to user space .sfOption
    assign( ".sfOption", sfOption, pos=globalenv() )

    .sfOption$stopped <<- FALSE
    .sfOption$init    <<- FALSE
  }
  ## If .sfOption exists, sfInit() was called before: restart.
  ## (sfCluster should be able to handle this - although slaves are iterated and
  ## slave killing is only done through Snow).
  else {
    reconnect <- TRUE

    if( .sfOption$stopped ) {
      ## If initialised yet, stop cluster first.
      if( .sfOption$init ) {
        message( "Explicit sfStop() is missing: stop now." );
        sfStop();
      }
    }
    else {
      message( "Explicit sfStop() is missing: stop now." );
      sfStop();
    }
  }

  ## Load server configuration file.
  data( "config", package="snowfall" )
  configM <- as.matrix( t( config ) )
  config  <- as.list( configM )
  names( config ) <- dimnames( configM )[[2]]

  ## Pump Server Configuration in global Vars.
  ## Currently unused...
  .sfOption$SERVER   <<- as.character( config[["SERVER"]] )  ## Unused...
  .sfOption$PORT     <<- as.numeric( config[["PORT"]] )      ## Unused...

  ## Node count are limited in snowfall as well (as it is useable without
  ## sfCluster) and you probably don't want an arbitrary amount of CPUs
  ## requested by a DAU.
  .sfOption$MAXNODES <<- as.numeric( config[["MAXNODES"]] )  ## Max. nodes

  ## If changed preset exists, take this number.
  if( exists( ".sfPresetCPUs", env=globalenv() ) )
    .sfOption$MAXNODES <<- .sfPresetCPUs;

  .sfOption$LOCKFILE <<- ""                                  ## Startup lockfile

  ## Temporary directory (for logfiles, esp. on the slaves)
  .sfOption$TMPDIR   <<- path.expand( as.character( config[["TMPDIR"]] ) )

  ## Addition variables for save/restore (only used in sfClusterApplySR).
  .sfOption$RESTOREFILES <<- NULL     ## List with restore files (for cleanup)
  .sfOption$RESTOREUPDATE <<- 5       ## Updates percent output any 5%
  .sfOption$RESTORE      <<- FALSE    ## Restore previous results?
  .sfOption$CURRENT      <<- NULL     ## Currently executed R-File

  ## Restore file directory (for saved intermediate results) - not neccessary
  ## under/in TMPDIR.
  ## (As log files prob woul be set in global dir, restore files should be
  ## stored under users home - as they don't contain a session-ID or something
  ## generic unique thing to differ them.
  .sfOption$RESTDIR  <<- path.expand( as.character( config[["RESTDIR"]] ) )
 
  ## Remove config (as data() writes it as global variable).
  rm( config, pos=globalenv() )

  ## Values for parallel/session can be in the commandline or the enviroment.
  ## Function parameters overwrite commandline.
  searchCommandline( parallel, cpus=cpus )

  if( getOption( 'verbose' ) && !reconnect )
    print( .sfOption )

  ## If given restore-directory does not exist, create it.
  if( !file.exists( .sfOption$RESTDIR ) ) {
    .sfOption$RESTDIR = path.expand( "~/.sfCluster/restore" )

    dirCreateStop( .sfOption$RESTDIR )
  }
  
  ## Running in parallel mode? That means: Cluster setup.
  ## Will be blocked if argument "nostart" is set (for usage of snowfall
  ## in packages).
  if( .sfOption$parallel && !nostart ) {
    ## Internal stopper. Running in parallel mode a session-ID is needed.
    ## For testing purposes can be anything (mainly used for pathnames
    ## of logfiles).
    if( is.null( .sfOption$session ) )
      stop( "no session-ID but running parallel..." )

    ## If amount of nodes not set via commandline, then it will be 2
    if( is.null( .sfOption$nodes ) || is.na( as.numeric( .sfOption$nodes ) ) )
      .sfOption$nodes <<- 2
    else
      .sfOption$nodes <<- as.numeric( .sfOption$nodes )
    
    ## Load snow (and Rmpi).
    ## Not loaded at first as then the Rmpi package loaded from snow tries
    ## to start a cluster - which can result in troubles in sequentially
    ## testing mode on a machine without LAM/MPI.
    if( !library( Rmpi, logical.return=TRUE ) ) {
      message( paste( "Failed to load library 'Rmpi' required for parallel mode.\n",
                      "Switching to sequential mode (1 cpu only)!." ) );

      ## Re-Init in sequential mode.
      return( sfInit( parallel=FALSE ) )
##      stop( "failed to load library 'Rmpi' required for parallel mode" )
    }
      
    if( !library( snow, logical.return=TRUE ) ) {
      message( paste( "Failed to load library 'snow' required for parallel mode.\n",
                      "Switching to sequential mode (1 cpu only)!." ) );

      ## Re-Init in sequential mode.
      return( sfInit( parallel=FALSE ) )
##      stop( "failed to load library 'snow' required for parallel mode" )
    }
      
    ## Temporary file for output.
    ## Fixed name as this is accessed by starter.
    ## Ending ".txt" is removed as the last thing in session number is the
    ## R-Version.
    tmp <- file.path( .sfOption$TMPDIR,
                      paste( "rout_", .sfOption$session, sep="" ) )

    ## Only create outfile once.
    if( !reconnect ) {
      ## If needed create temporary path. Problem: this is executed only on
      ## master, not on slaves.
      ## The clusterstarter needs to manage this.
      dirCreateStop( .sfOption$TMPDIR )
    }

    ## Exception Handler fehlt...
    ## Ebenso: Timeout - das ist extrem hässlich, wenn das Cluster nicht
    ## korrekt startet und hängen bleibt (z.B. wenn zuviele CPUs für das
    ## Cluster angefordert werden - was PVM schluckt, macht MPI anscheinend
    ## Kopfzerbrechen).
    setDefaultClusterOptions( type = "MPI" )
    setDefaultClusterOptions( homogenous = FALSE )

    .sfOption$cluster <<- try( makeCluster(  .sfOption$nodes,
#                                       type = "MPI",
#                                       homogenous = TRUE,
#                                       homogenous = FALSE,
                                             outfile = tmp
                                          ) )

    ## Startup successfull? If not: stop.
    if( is.null( .sfOption$cluster ) ||
        inherits( .sfOption$cluster, "try-error" ) )
      stop( paste( "Starting of snow cluster failed!",
                   geterrmessage() ) )
    
    ## Cluster setup finished. Set flag (used in error handlers and stop).
    ## Also: no function can be called if init is not set.
    .sfOption$init <<- TRUE;

    if( !reconnect ) {
      ## As Snow Init spawn all the requires R-processes, the proprietary
      ## lockfile can be deleted now (if it exists).
      ## Problem: now all R procs are spawned, but the observer most
      ## likely didn't catch them until the next time of his observing
      ## loop.
      if( !is.null( .sfOption$LOCKFILE ) &&
         file.exists( .sfOption$LOCKFILE ) ) {
        if( unlink( .sfOption$LOCKFILE ) != 0 )
          warning( "Unable to remove startup lockfile: ", .sfOption$LOCKFILE )
        else
          message( "Startup Lockfile removed: ", .sfOption$LOCKFILE )
      }
    
      if( getOption( 'verbose' ) )
        message( paste( "Temporary log for STDOUT/STDERR (on each node): ", tmp, "\n",
                        "Cluster started with", .sfOption$nodes, "CPUs.", "\n" ) )
   
      ## Write R-Version and Time in all logfiles.
      .startInfo = strsplit( Sys.info(), "\n" );
      .startMsg <- paste( sep="",
                          "JOB STARTED AT ", date(),      # Global Var!
                          " ON ", .startInfo$nodename, " (OS", .startInfo$sysname,
                          ") ", .startInfo$release, "\n" )

      sfExport( ".sfOption", ".startMsg", local=TRUE )
      sfCat( .startMsg, "\n" )
      sfCat( paste( "R Version: ", R.version$version.string, "\n" ) )

      ## Remove starting message.
      sfRemove( ".startMsg" )
    }
    else
      sfExport( ".sfOption", local=FALSE )
  }
  ## Sequential mode or no start.
  ## Init will be set. If someone calls sfInit with nostart and aims
  ## it to be started, it's his problem.
  else {
    ## Cluster setup finished. Set flag (used in error handlers and stop).
    ## Also: no function can be called if init is not set.
    .sfOption$init <<- TRUE;

    .sfOption$cluster <<- NULL
  }
    
  ## Print init Message (esp. print State of parallel and snowfall
  ## version.
  message( paste( "snowfall ", packageDescription( "snowfall" )$Version,
                  " initialized (",
                  "parallel=", sfParallel(),
                  ", CPUs=", sfCpus(), ")\n",
                  sep="" ) );

  return( invisible( TRUE ) )
}

sfCheck <- function() {
  if( !exists( ".sfOption" ) || !.sfOption$init )
    stop( "Calling to snowfall function is only allowed AFTER calling sfInit()!" )

  return( invisible( TRUE ) )
}

##*****************************************************************************
## Stop the (snow)-Cluster. Just calls Snows stopCluster.
##
## PARAMETER: [Boolean nostop: don't stop]
##*****************************************************************************
sfStop <- function( nostop=FALSE ) {
  ## Saves users from many own if-clauses probably.
  if( nostop ) return( TRUE );

  if( exists( ".sfOption" ) ) {
    ## Only stop if initialisized and running parallel.
    if( .sfOption$init && sfParallel() ) {
      message( "\nStopping cluster\n" )

      ## Stopping snow cluster.
      stopCluster( .sfOption$cluster )
    }

    ## Delete probably stored resultfiles (can also be used in sequential mode!)
    deleteRestoreFiles()

    .sfOption$init    <<- FALSE
    .sfOption$stopped <<- TRUE
  }

  invisible( NULL )
}

##*****************************************************************************
## Is programm running parallel? Wrapper for internal Optionblock (therefore
## exported of course).
## Also: get cluster Handler (prob. not exported in the final).
##
## RETURN: Boolean Running in parallel mode
##*****************************************************************************
sfParallel <- function() {
  sfCheck();

  if( exists( ".sfOption" ) )
    return( .sfOption$parallel )
  else
    return( FALSE )
}

##*****************************************************************************
## Shall sfClusterApplySR restore results?
##*****************************************************************************
sfRestore <- function() {
  sfCheck();

  if( exists( ".sfOption" ) )
    return( .sfOption$RESTORE )
  else
    return( FALSE )
}

##*****************************************************************************
## Receive snow cluster handler (for direct calls to snow functions).
##*****************************************************************************
sfGetCluster <- function() {
  sfCheck();

  if( exists( ".sfOption" ) )
    return( .sfOption$cluster )
  else
    return( NULL )
}

##*****************************************************************************
## Receive amount of currently used CPUs (sequential: 1).
##*****************************************************************************
sfCpus <- function() {
  sfCheck();

  if( exists( ".sfOption" ) )
    return( .sfOption$nodes )
  else
    return( NULL )
}

## getter for amount of nodes. Wrapper for sfCPUs.
sfNodes <- function() return( sfCpus() )

##*****************************************************************************
## getter for session-ID.
##*****************************************************************************
sfSession <- function() {
  sfCheck();

  if( exists( ".sfOption" ) )
    return( .sfOption$session )
  else
    return( NULL )
}

##*****************************************************************************
## Increase max. numbers of CPUs used per process.
## No check for sensefull values (if user wants 1000, you get 1000 :)).
##*****************************************************************************
sfSetMaxCPUs <- function( number=32 ) {
  .sfPresetCPUs <<- number;
}

##*****************************************************************************
## Internal function.
##
## Search commandline arguments for Parallel and Session values.
## If there are arguments on commandline, these overwrites the values on the
## commandline.
##
## Commandline arguments: --parallel(=[01])*
##                        --session=\d{8}
##                        --nodes=\d{1,2}
##                        --tmpdir=\/[a-z].*
## Results will be saved in options .parallel (bool) and .session (8 chars)
##*****************************************************************************
searchCommandline <- function( parallel=NULL, cpus=NULL ) {
  if( !exists( ".sfOption", envir=globalenv() ) )
    stop( "Global options missing. Internal error." )

  ## Defaults come from calling arguments on sfInitCluster.
  if( !is.null( parallel ) ) {
    .sfOption$parallel <<- parallel

    if( parallel ) {
      ## There is a slightly problem: as many users can use sfCluster without
      ## session-ID, the session number "XXXXXXXX" is not good enough.
      ## Problem: we need the filename on clusterinit so we cannot use cluster
      ## here. And no SSH session is available here.
      try( uname <- system( "whoami", intern=TRUE, ignore.stderr=TRUE ) )

      ## Add R for RunSnowMode heterogenous mode.
      ## XXX Check R version and fill in correct version.
      .sfOption$session <<- paste( sep="_",
                                   "XXXXXXXXR",
                                   uname,
                                   format( Sys.time(), "%H%M%S_%m%d%y" ) )

      message( "Forced parallel. Using session: ", .sfOption$session, " \n" )
    }
    else
      message( "Forced to sequential mode.\n" )
  }

  ## If set, copy to sfCluster data structure.
  if( !is.null( cpus ) )
    .sfOption$nodes <<- cpus

  arguments <- commandArgs()

  ## Search for currently executed R-file (if there is any). Detected by
  ## argument followed to option "-f" ("R CMD BATCH" adds -f implicitely).
  ## Save filename for options (for save/restore)
  ## @todo Find a better way to detect R-file (is there any?)
  ## Last argument to be ignored (as no follow-up exists).
  if( length( arguments ) >= 2 ) {
    for( entry in seq( 1, length( arguments ) - 1 ) ) {
      if( !is.null( arguments[entry] ) && ( arguments[entry] == '-f' ) ) {
        ## Switch to next entry and check if this is valid.
        entry <- entry + 1;

        ## If yes, take it as filename.
        if( !is.null( arguments[entry] ) && ( arguments[entry] != "" ) ) {
          .sfOption$CURRENT <<- arguments[entry]
          break
        }
      }
    }
  }

  ## No R-file given: set to DEFAULT filename (always occurs in interactive
  ## mode).
  if( is.null( .sfOption$CURRENT ) )
    .sfOption$CURRENT <- 'DEFAULT'
  
  ## Go through all arguments from commandline.
  for( arg in arguments ) {
    ## Non sfCluster-like argument? Skip.
    ## (Only empty argument are '--parallel' and '--restore')
    if( ( length( grep( "=", arg ) ) == 0 ) &&
        !( ( arg == "--parallel" ) || ( arg == "--restoreSR" ) ) )
      next;

    ## Arguments in form "--name=value"
    args <- strsplit( arg, "=" )

    ## Marker for parallel execution.
    ## If parallel was set via function arguments, commandline is ignored.
    if( ( args[[1]][1] == "--parallel" ) && ( is.null( parallel ) ) ) {
      if( !is.null( args[[1]][2] ) && !is.na( as.numeric( args[[1]][2] ) ) ) {
        .sfOption$parallel <<- ifelse( ( as.numeric( args[[1]][2] ) > 0 ), TRUE, FALSE )
      }
      ## --parallel is allowed to use without value (means: true).
      else {
        .sfOption$parallel <<- TRUE
      }
    }
    ## Marker for restore (only used in sfClusterApplySR).
    else if( args[[1]][1] == "--restoreSR" ) {
      .sfOption$RESTORE <<- TRUE
    }
    ## Marker for Session-ID.
    else if( args[[1]][1] == "--session" ) {
      ## Session-ID is allways 8 Chars long.
      ## Not anymore since sfCluster >=0.23
      if( !is.null( args[[1]][2] ) ) { ##&& ( nchar( args[[1]][2] ) == 8 ) ) {
        .sfOption$session <<- args[[1]][2]
      }
      else
        warning( paste( "Empty or irregular Session-ID: '", args[[1]][2], "'\n" ) )
    }
    ## Amount of Nodes.
    ## If set via function arguments, commandline is ignored.  
    else if( ( args[[1]][1] == "--nodes" ) && ( is.null( cpus ) ) ) {
      nodes <- as.numeric( args[[1]][2] )

      if( !is.null( nodes ) && !is.na( nodes ) ) {
        if( nodes > .sfOption$MAXNODES )
          stop( paste( "Too much CPUs allocated: ", nodes, "Max.:",
                       .sfOption$MAXNODES,
                       "\n - Call sfSetMaxCPUs() before sfInit() if you need more." ) )

        if( nodes < 0 )
          nodes <- 1
        
        .sfOption$nodes <<- nodes
      }
      else
        warning( paste( "Empty or irregular nodes amount: '", nodes, "'\n" ) )
    }
    ## Temporary directory.
    else if( args[[1]][1] == "--tmpdir" ) {
      if( !is.null( args[[1]][2] ) && ( nchar( args[[1]][2] ) > 0 ) )
        .sfOption$TMPDIR <<- args[[1]][2]
      else
        warning( "No temporary directory given as value for --tmpdir" )
    }
    ## Startup lock.
    else if( args[[1]][1] == "--lockfile" ) {
      if( !is.null( args[[1]][2] ) && ( nchar( args[[1]][2] ) > 0 ) )
        .sfOption$LOCKFILE <<- args[[1]][2]
      else
        warning( "No lockfile given as value for --lockfile" )
    }
    ## Unknown option
##    else
##      warning( paste( "Unknown option on commandline:", args[[1]][1] ) )
  }

  invisible( NULL )
}

