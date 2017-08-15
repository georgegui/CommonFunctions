#' @importFrom Rcpp evalCpp
#' @useDynLib CommonFunctions
#' @import data.table
#' @import data.tree
#' @import yaml
#' @import parallel
#' @import lfe
#' @import matrixStats
NULL

#' check if a global variable is correctly set
#'
#' This function will be used to check if a variable exists in the .GlobalEnv.
#'  and whether it is a logical value.
#'
#' @param the name of the variable
#'
GlobalVariableIsCorrectlySet <- function(var_name){
  if(!exists(var_name, envir = .GlobalEnv)) return(FALSE)
  var <- get(var_name, .GlobalEnv)
  if(is.null(var))     return(FALSE)
  if(is.na(var))       return(FALSE)
  if(!is.logical(var)) return(FALSE)
  return(var)
}

#' Get the value of a Global Variable
#'
#' This function gets the variable in the .GlobalEnv. If it does not exist,
#'  it will return a default value specified by the user.
#'
#' @export
#' @param var the name of the variable
#' @param default_val_if_unset the default value of the variable if the variable
#'  is not correctly set in the global environment.
GetGlobalVariable <- function(var, default_val_if_unset){
  var_set <-GlobalVariableIsCorrectlySet(var)
  if(var_set){
    get(var, envir = .GlobalEnv)
  } else {
    default_val_if_unset
  }
}

#' @describeIn GetGlobalVariable Get 'SAMPLE_TEST' in the global environment,
#'  with default value being FALSE
#' @export
IsTestMode <- function(var = 'SAMPLE_TEST'){
  GetGlobalVariable(var, default_val_if_unset = FALSE)
}

#' @describeIn GetGlobalVariable Get 'VERBOSE' in the global environment, with
#'  default value being FALSE
#' @export
IsVerboseMode <- function(var = 'VERBOSE'){
  GetGlobalVariable(var, default_val_if_unset = FALSE)
}

#' @describeIn GetGlobalVariable Get 'PARALLEL' in the global environment, with
#'  default value being FALSE
#' @export
IsParallelMode <- function(var = 'PARALLEL'){
  GetGlobalVariable(var, default_val_if_unset = FALSE)
}

#' print message in verbose mode.
#'
#' Similar to original R fucntion cat, but it will only print if is in Verbose
#'  Mode
#' @export
VerboseWarning <- function(...){
  if(IsVerboseMode()){
    cat(paste(..., '\n'), sep = '')
  }
}

#' Count the number of unique elements
#'
#' @param values a list of values
#' @return the number of unique elements of the list, excluding NA.
#' @export
CountUnique <- function(values){
  unique_list <- unique(values)
  length(setdiff(unique_list, NA))
}

#' Get the mode of a list
#'
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Convert a string to a legal and readable file name.
#'
#' This function will get rid of the illegal file-name characters of a string.
#'  It will also get rid of space and underscore capitalize letter
#'  after each space or underscore instead.
#'
#' @param X string or a list of strings.
#' @return updated string or a list of strings.
#'
#' @export
ConvertName <- function(X){
  X <- tolower(gsub('-|&|/|%|\\.|\\\\', '_', X))
  X <- gsub('_{1,}', '_', X)
  X <- gsub('(^| )([a-z])', '\\U\\2', X, perl = TRUE)
  X <- gsub(' ', '', X)
  return(X)
}

#' This function takes in a directory tree and makes it into full directories.
#'
#' For readability the yaml path setting files are written of relative paths.
#'  This function converts those relative paths into the absolute paths.
#'
#' @param tree data.tree object, with the 'dir' value being its path.
FormatTreeToDirectory <- function(tree){
  if(isRoot(tree)){
    tree$dir = tree$dir
  } else {
    tree$dir = paste0(tree$parent$dir, tree$dir)
  }
}

#' this function generates the path and folder structure based on a yaml file
#'
#' this function takes in a yaml file that specifies directories and generates
#'  a data.tree that has a 'dir' leaf in each node that specifies the full
#'  path of the folder.
#'
#' @export
#' @param path_file the path of the yaml file.
#' @return a data.tree object, with the 'dir' value being its path.
#'
GeneratePaths <- function(path_file){
  PATHS <- as.Node(yaml.load_file(path_file))
  cat('Directory Structure\n')
  if(IsVerboseMode()){
    print(PATHS, 'dir')
  } else {
    print(PATHS)
  }
  PATHS$Do(FormatTreeToDirectory, traversal = 'level')
  return(PATHS)
}

# # this function converts a readable list, which is ordered by
# # field, value, field, value, field, value, to a table whose first column
# # is the field name and the rests are values.
# ListToTable <- function(l, nrow = 2){
#   t(matrix(l, nrow = nrow))
# }


#' Generate a line to separate messages
#'
#' This function generates a hyphen lines to separate printed messages.
#'
#' @param n the number of hyphens in a line
#' @return a string of hyphens
#'
#' @export
Separator <- function(n = 80){
  tmp <- paste(rep('-', n), collapse = '')
  paste0(tmp, '\n')
}

#' Remove rows of a data.table that has infinite values
#'
#' @export
#' @param dt a data.table
#' @return updated ata.table without any infinite_row
#'
RemoveInfinite <- function(dt){
  infinite_row <- rowSums(dt[, lapply(.SD, is.infinite)])
  dt[infinite_row == 0]
}

#' Demean a list of columns
#'
#' This function centers a list of columns by certain groups
#' @export
#' @param dt a data.table
#' @param relevant_cols list of columns that needed to be centered
#' @param by_cols by what columns the relevant_cols should be centered
#' @return updated data.table where all relevant columns are centered by groups
DemeanCols <- function(dt, relevant_cols, by_cols){
  for(v in relevant_cols){
    dt[, (v) := get(v) - mean(get(v), na.rm = TRUE),
         by = by_cols]
  }
  return(dt)
}



#' mclapply if ParallelMode
#'
#' This function will run single core lapply if it is not in parallel mode;
#'  it will run multi-core mclapply if is in PARALLEL = TRUE
#' @export
ParallelApply <- function(..., n_cores = 10){
  if(IsParallelMode()){
    mclapply(..., mc.cores = n_cores)
  } else {
    lapply(...)
  }
}


#' Rsquare of fixed effect
#'
#' This function calculates the rsquare of fixed effect
#'
#' @export
#' @param form formula
#' @param data.table a data.table to be regressed on
Rfe <- function(form, dt, vars){
  fereg = felm(form, data = dt)
  FE = data.table(getfe(fereg))
  FE[,min_fe:=min(effect, na.rm=TRUE), by = "fe"]
  FE[,`:=`(effect = effect - min_fe, min_fe = NULL,
           obs=NULL, comp=NULL)]
  setnames(FE, "effect", vars)
  setkeyv(FE, c("fe", "idx"))
  setnames(dt, all.vars(form)[1], "diff_var")
  r2 = 1 - sum((fereg$residuals)^2, na.rm=TRUE)/sum((dt$diff_var-mean(dt$diff_var, na.rm=TRUE))^2, na.rm=TRUE)
  setnames(dt, "diff_var", all.vars(form)[1])
  return(list(r.squared = r2, fe = FE))
}

#' source a package even if it is uninstalled
#'
#' This function can source a package even if it is not installed.
#'  If a function is installed, the package will load the installed one, if it
#'  is not, then it will source the R code directly.
#' @export
#' @param package_name the name of the package that we want to load
#' @param package_folder_name the path in which the package can be built, if
#'  it is unspecified, it will equal the package_name.
SourcePackage <- function(package_name, package_folder_name = NULL){
  if(is.null(package_folder_name)){
    package_folder_name <- package_name
  }
  package_loaded <- require(package_name, character.only = TRUE)
  if(!package_loaded){
    require(yaml)
    library_list <- yaml.load_file(paste0(package_folder_name, '/DESCRIPTION'))$Imports
    if(!is.null(library_list)){
      library_list <- strsplit(library_list, ',| ')
      library_list <- setdiff(unlist(library_list), '')
      for(lib in library_list) require(lib, character.only = TRUE)
    }
    cur_script_directory <- paste0(package_folder_name , '/R/')
    parent_directory     <- getwd()
    setwd(cur_script_directory)
    function_files       <- list.files(pattern = '*.R$',ignore.case = TRUE)
    sapply(function_files, source) #, local = WholeSales
    if('src' %in% dir('../')){
      cur_src_directory <- '../src/'
      setwd(cur_src_directory)
      function_files       <- list.files(pattern = '*.cpp$',ignore.case = TRUE)
      sapply(function_files, sourceCpp)
    }
    setwd(parent_directory)
  }
}




