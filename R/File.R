#' @import data.table
NULL

#' Check if the module file exists
#'
#' This function checks if a list of module files exists.
#'  These module files are often prefixed with their module number.
#'
#' @export
#' @param path_list a list of paths that you want to search for
#' @param pattern_list, a list of regex patterns of the module files that you
#' want to search for within those paths.
#' @param pattern_names, a list of names you want to have for each pattern.
#' @return a data.table that includes list of modules for which the files exist
#' and their corresponding file sizes.
CheckModuleFileExistence <- function(path_list, pattern_list, pattern_names = NULL){
  DT_list <- list()
  for(i in 1:length(path_list)){
    cur_path <- path_list[[i]]
    cur_pat  <- pattern_list[[i]]
    if(!is.null(pattern_names)) cur_name <- pattern_names[[i]]
    file_list          <- grep(cur_pat, dir(cur_path), value = TRUE)
    module_number_list <- sub(cur_pat, '\\1', file_list)
    file_size_list     <- file.info(paste0(cur_path, file_list))$size
    DT_list[[i]]       <- data.table(module_number = module_number_list,
                                    file_size = file_size_list)
    if(!is.null(pattern_names)) colnames(DT_list[[i]]) <- c('module', cur_name)
  }
  merge_module <- function(x, y){
    merge(x, y, by = 'module', all = TRUE)
  }
  Reduce(merge_module, DT_list)
}

#' Make Directories
#'
#' This function makes a list of directories recursively.
#'
#' @export
#' @param folder_list a list of directories that you want to create
MakeDir <- function(folder_list){
  for(cur_dir in folder_list){
    if(grepl('\\.', cur_dir)){
      VerboseWarning(cur_dir, ' could be a file instead of a directory')
      next
    }
    if(!dir.exists(cur_dir)){
      dir.create(cur_dir, recursive = TRUE)
    }
  }
  return(NULL)
}


#' load a data.table from a file
#'
#' This function loads a data.table from a file, it will check if the data.table
#' is empty and valid. If not this function will return NULL. This should be
#' deprecated if the data is saved in the format of RDS.
#'
#' @export
#' @param file the path of the file
#' @param dt_name the name of the data.table variable
#' @return the data.table with name dt_name
LoadData <- function(file, dt_name  = 'move'){
  assign(dt_name, NULL)
  VerboseWarning('loading', dt_name, 'from', file, '\n')
  tryCatch(load(file),
    error = function(e){}
  )
  dt <- get(dt_name)
  if(IsEmpty(dt)){
    cat('failure loading', dt_name, 'from', file, '\n')
    return(NULL)
  } else {
    return(dt)
  }
}

#' test if an object is empty
#' @export
IsEmpty <- function(object, ...){
  UseMethod('IsEmpty')
}

#' @describeIn IsEmpty NULL
#' @export
IsEmpty.NULL <- function(object){
  return(TRUE)
}

#' @describeIn IsEmpty data.table
#' @export
IsEmpty.data.table <- function(dt){
  if(is.null(dt)) return(TRUE)
  if(nrow(dt) == 0) return(TRUE)
  return(FALSE)
}


#' @describeIn IsEmpty list
#' @export
IsEmpty.list <- function(list){
  if(is.null(list)) return(TRUE)
  if(length(list) == 0) return(TRUE)
  return(FALSE)
}


#' Copy necessary script files to new folder
#'
CopyToNewFolder <- function(
  from, to,
  folder_list = folder_list <- c(
    'DESCRIPTION', 'R/', 'src/',
    'NAMESPACE', 'inst/', 'vignettes/', 'Readme.md', 'readme')
){
  folder_list <- paste0(from, '/', folder_list)
  MakeDir(to)
  for(cur_file in folder_list){
    if(file.exists(cur_file)){
      file.copy(cur_file, paste0(to, '/'), recursive = TRUE)
    }
  }
}

#' Create a new git repository with only the necessary function files
#'
#' @export
UpdateToGitHub <- function(from, to,
                           user_name, repos_name, commit_message = 'First Commit'){
  folder_list <- c(
    'DESCRIPTION', 'R/', 'src/',
    'NAMESPACE', 'inst/', 'vignettes/', 'Readme.md', 'readme')
  folder_list <- paste0(from, '/', folder_list)
  MakeDir(to)
  for(cur_file in folder_list){
    if(file.exists(cur_file)){
      file.copy(cur_file, paste0(to, '/'), recursive = TRUE)
    }
  }
  repos_name_full <- sprintf('git@github.com:%s/%s.git', user_name, repos_name)
  Sys.setenv(new_repository = repos_name_full)
  write(commit_message, '/tmp/git_commit_message.txt')
  Sys.setenv(commit_message = commit_message)
  git_upload_script = system.file("create_git.sh", package = 'CommonFunctions')
  setwd(to)
  system(paste('sh', git_upload_script))
  return(NULL)

}
