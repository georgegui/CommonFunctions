

ClSource <- function(file_to_source, parameter_value = NULL, parameter_name = NULL,
                     variables_to_collect = NULL){
  if(is.null(parameter_name)) parameter_name = 'tmp'
  if(is.null(parameter_value)) parameter_value = 1
  clusterExport(CL, 'variables_to_collect')
  if(exists('CL')){
    if(length(parameter_value) == 1){
      assign(parameter_name, parameter_value)
      clusterExport(CL, parameter_name)
    } else {
      for(i in 1:length(parameter_value)){
        assign(parameter_name, cur_param[[i]])
        clusterExport(CL[i], parameter_name)
      }
    }
    clusterExport(CL, 'file_to_source', envir = environment())
    clusterEvalQ(CL, {
      source(file_to_source)
    })
    if(!is.null(variables_to_collect)){
      tmp_list <- clusterEvalQ(CL, {
        tmp <- list()
        for(cur_var in variables_to_collect){
          tmp[[cur_var]] <- get(cur_var)
        }
        tmp
      })
      return(tmp_list)
    } else {
      return(1)
    }
  } else {
    if(is.null(variables_to_collect))
      tmp_list <- list()
    lockBinding('parameter_value', globalenv())
    for(cur_param_id in 1:length(parameter_value)){
      lockBinding('cur_param_id', globalenv())
      assign(parameter_name, parameter_value[[cur_param_id]])
      source(file_to_source)
      tmp <- list()
      for(cur_var in variables_to_collect){
        tmp[[cur_var]] <- get(cur_var)
      }
      tmp_list[[cur_param_id]] <- tmp
      unlockBinding('cur_param_id', globalenv())
    }
    unlockBinding('parameter_value', globalenv())
    return(tmp_list)
  }
}
