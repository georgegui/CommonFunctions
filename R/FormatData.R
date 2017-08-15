#' @import data.table
NULL

#' this function select relevant columns of DT for estimation
#'  it also renames variable to make them shorter but readable
#'
#' @export
#' @param DT the data.table that we want to format
#' @param old_names the column names that we want to change
#' @param new_names the column names that we want to change to
#' @param cols_to_keep the columns that we want to keep
#' @param cols_to_factor the columns that we want to convert to as factor
#' @return the updated data.table. Note because 'set' functions are used, the
#' original data.table will also be changed when running this function.
SetTableFormat <- function(DT, old_names, new_names, cols_to_keep, cols_to_factor){
    for(cur_col in cols_to_factor){
      DT[, (cur_col) := as.character(get(cur_col))]
    }
    cols_to_keep   <- union(cols_to_keep, old_names)
    cols_to_delete <- setdiff(names(DT), cols_to_keep)
    for(cur_col in cols_to_delete){
      DT[, (cur_col) := NULL]
    }
    if(!is.null(old_names)) setnames(DT, old_names, new_names)
    return(DT)
}

#' @describeIn SetTableFormat Format the store table
#' @export
SetStoreTableFormat <- function(DT){
  SetTableFormat(DT,
    old_names      = c('fips_state_code', 'dma_code', 'store_zip3', 'parent_code', 'revenue_RMS'),
    new_names      = c('State', 'DMA', 'ZIP3', 'Chain', 'store_revenue'),
    cols_to_keep   = 'store_code_uc',
    cols_to_factor = c('fips_state_code', 'dma_code', 'store_zip3', 'parent_code', 'store_code_uc')
    )
  setattr(DT, 'class', c('StoreBasket.DemandEstimation', class(DT)))
  return(DT)
}


#' Add store table to price movement table
#'
#' This function merges store table and movement table together.
#' @export
#' @param move the price movement data.table
#' @param store_dt the store data.table
#' @return the updated movement data.table with store information.
AddStoreToMovement <- function(move, store_dt){
  if(!('StoreBasket.DemandEstimation' %in% class(store_dt))){
    stop('invalid store input')
    }
  if(!('BrandPrice' %in% class(move))){
    stop('invalid brand price movement input')
  }
  necessary_cols  <- setdiff(names(move), names(store_dt))
  necessary_cols  <- c('store_code_uc', necessary_cols)
  classes_to_keep <- class(move)
  move <- merge(move[, .SD, .SDcols = necessary_cols],
    store_dt, by = 'store_code_uc')
  setattr(move, 'class', classes_to_keep)
  return(move)
}

#' Add Time Factors to price movement table
#'
#'  Add Time Factors such as YearQuarter, YearMonth and YearWeeks to the
#'    movement tables.
#' @export
#' @param move the price movement data.table
#' @param time_dt the time factor data.table
#' @return updated price movement 'BrandPrice' data.table
AddTimeFactorToMovement <- function(move, time_dt){
  if(!('TimeFactor.DemandEstimation' %in% class(time_dt))){
    stop('invalid store input')
    }
  if(!('BrandPrice' %in% class(move))){
    stop('invalid brand price movement input')
  }
  necessary_cols  <- setdiff(names(move), names(time_dt))
  necessary_cols  <- c('week_end', necessary_cols)
  classes_to_keep <- class(move)
  move <- merge(move[, .SD, .SDcols = necessary_cols],
    time_dt, by = 'week_end')
  setattr(move, 'class', classes_to_keep)
  return(move)
}

#' this function generates factors by time
#'
#' This function generates a data.table of time factor with YearQuarter,
#'  YearMonth, and YearWeek.
#' @export
#' @return a time factor data.table
GenerateTimeFactorTable <- function(
  start_date = '2006-01-07',
  end_date   = '2014-12-25',
  factors    = c('YearMonth', 'YearWeek', 'YearQuarter')){
  start_date <- as.Date(start_date)
  start_date <- start_date + 7 - wday(start_date)
  DT <- data.table(week_end=seq(as.Date(start_date),as.Date(end_date), 7))
  if('YearMonth' %in% factors){
    DT[, YearMonth   := format(as.Date(week_end), '%Y-%m')]
  }
  if('YearWeek' %in% factors){
    DT[, YearWeek    := week_end]
  }
  if('YearQuarter' %in% factors){
    DT[, YearQuarter := as.factor(year(week_end) + 0.25 * floor((month(week_end)-1)/3))]
  }
  setkey(DT, week_end)
  setattr(DT, 'class', c('TimeFactor.DemandEstimation', class(DT)))
  return(DT)
}




# FormatBrandPriceTable <- function(DT){
#   SetTableFormat(DT, )
# }
