##*****************************************************************************
## Unordered internal helper functions.
##*****************************************************************************

##*****************************************************************************
## Replaces the tilde operator in file/directory names with the system
## depending counterpart.
## Used for configuration files mainly.
##
## PARAMETER: String directory
## RETURN:    String directory replaced
##*****************************************************************************
fetchDirName <- function( dir ) {
  return( gsub( "~", Sys.getenv( "HOME" ), dir ) )
}

##*****************************************************************************
## Is this snowfall session started through sfCluster?
## As a backward compatible solution there is only the LOCKFILE option open
## (as there is no default for it and setable through commandline).
##
## PARAMETER: -
## RETURN:    Boolean True (running with sfCluster), False
##*****************************************************************************
startedWithSfCluster <- function() {
  if( !exists( ".sfOption" ) )
    return( FALSE )
  else
    return( !is.null( .sfOption$LOCKFILE ) && ( .sfOption$LOCKFILE != '' ) )
}

##*****************************************************************************
## Creates a directory (recursive) if needed and stops on failure.
##
## PARAMETER: String directory
## RETURN:    Boolean success (true, on fail, execution stops)
##*****************************************************************************
dirCreateStop <- function( dir=NULL ) {
  if( !is.null( dir ) && !file.exists( dir ) ) {
    if( dir.create( dir, recursive=TRUE ) ) {
      message( "Created directory: ", dir )
      return( invisible( TRUE ) );
    }
    else
      stop( "UNABLE to create directory: ", dir )
  }

  ## Never reached.
  return( invisible( FALSE ) );
}

##***************************************************************************
## Add a file (with absolute path) to remove list after sfStop().
## Used for save/restore-files.
##
## PARAMETER: file String abs. filepath
##***************************************************************************
addRestoreFile <- function( file=NULL ) {
  if( !is.null( file ) )
    if( is.vector( .sfOption$RESTOREFILES ) )
      ## Check if file is already in the list. If yes: no add.
      if( length( grep( file, .sfOption$RESTOREFILES ) ) == 0 )
        .sfOption$RESTOREFILES <<- c( .sfOption$RESTOREFILES, file )
    else
      .sfOption$RESTOREFILES <<- c( file )

  debug( paste( "Added file for delete: ", file, "\n" ) )

  return( invisible( length( .sfOption$RESTOREFILES ) ) )
}

##***************************************************************************
## Clean up save/restore files after successfull cluster shutdown.
##***************************************************************************
deleteRestoreFiles <- function() {
  if( !is.null( .sfOption$RESTOREFILES ) ) {
    ## File names are absolute: just unlink all.
##    lapply( .sfOption$RESTOREFILES, unlink )
    for( file in .sfOption$RESTOREFILES ) {
      ## Does file exist?
      if( file.exists( file ) ) {
        if( unlink( file ) != 0 )
          cat( "Unable to delete save/restore file:", file, "\n" )
        else
          cat( "Deleted save/restore file:", file, "\n" )
      }
    }

    .sfOption$RESTOREFILES <- NULL
  }
}

##***************************************************************************
## Check if any element of a given list produced a stop or try-error.
## RETURN: Vector of logicals (true: ok, false: try error caught).
##***************************************************************************
checkTryErrorAny <- function( res ) {
  return( sapply( res,
                  function( x ) {                   
                    if( inherits( x, "try-error" ) )
                      return( FALSE )
                    else
                      return( TRUE )
                  }
                 ) )
}

##***************************************************************************
## Check if given argument is a function.
##***************************************************************************
checkFunction <- function( fun, stopOnError=TRUE ) {
  return( TRUE )

  state <- FALSE

  try( if( !exists( as.character( substitute( fun ) ), inherit=TRUE ) ||
          !is.function( fun ) ||
          is.null( get( as.character( substitute( fun ) ), inherit=TRUE ) ) ||
          !is.function( fun ) ) state <- TRUE )
  
  if( !state ) {
##    if( !is.function( fun ) ) cat( "FAIL SYMBOL\n" )
##    if( !exists( as.character( substitute( fun ) ), inherit=TRUE ) ) cat( "FAIL EXIST\n" )
##    if( is.null( get( as.character( substitute( fun ) ), inherit=TRUE ) ) ) cat( "FAIL GET\n" )
##    if( !is.function( fun ) ) cat( "FAIL FUNCTION\n" )

    if( stopOnError )
      stop( paste( "Not a function in sfCluster function call: '", fun, "'" ) )
  }

  return( state )
}

errHandler <- function( ... ) {
  print( "ERROR IN HANDLING STUFF!\n" )
}

##***************************************************************************
## Treat given three dot arguments as strings (for names listings
## like in sfExport).
## Ripped from buildin R function rm (by XXX).
## Returns list with names, stops on errors.
##***************************************************************************
fetchNames <- function( ... ) {
  ## Dot argument to list of characters: ripped from rm()...
  dots <- match.call(expand.dots = FALSE)$...

  if( length(dots) &&
      !all( sapply( dots, function(x) is.symbol(x) || is.character(x) ) ) )
    stop( "... must contain names or character strings in function ",
          as.character( sys.call( -1 ) ) )

  names <- sapply(dots, as.character)
  ## End ripp.

  return( names )
}

##***************************************************************************
## Create named list with all parameters from an function call.
## Idea somewhere from R-help (not tracked).
## This does not work if above env is not global env!
##***************************************************************************
getNamedArguments <- function( ... ) {
  pars <- as.list( substitute( {...} )[-1] )

##  pars <- as.list( substitute( {...} )[-1] )
##  pars <- lapply( pars, function( x ) {
##                                        if( is.atomic( x ) )
##                                          return( x )
##                                        else
##                                          return( deparse( x ) )
##                                      } )

  return( pars )
}

##***************************************************************************
## Ensure a given filename contains an absolute path.
## Kind of silly and lame. But works in most cases.
##***************************************************************************
absFilePath <- function( file ) {
  ## If not starting with separator, path is most likely relative.
  ## Make it absolute then.
  if( substr( file, 0, 1 ) != .Platform$file.sep )
    file <- file.path( getwd(), file )

  return( file )
}

.onLoad <- function( lib, pkg ) {
##  options( "error"=errHandler )
}
