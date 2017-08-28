#' @title
#' Date Factor
#' 
#' @description
#' Map a vector of dates to a factor at one of these levels {"yearmonth", "yearquarter", "quarter", "month"}
#' 
#' @details
#' The resulting vector is an ordered factor of the specified \code{type} (e.g. yearmonth)
#' 
#' @param dateVec A vector of date values
#' @param type One of {"year", "yearquarter", "yearmonth", "quarter", "month"}
#' @param minDate (Default = min(dateVec)) When determining factor levels, use this date to set the min level, after coercing 
#' dates to the specified \code{type}. For example, if dateVec = (2016-01-15, 2016-02-15), type = "yearmonth", and minDate = 2016-02-01,
#' the result will be (NA, Feb 2016).
#' @param maxDate (Default = max(dateVec)) When determining factor levels, use this date to set the max level. (See minDate, above)
#'
#' @examples
#' library(data.table)
#' dts <- as.Date(c("2014-1-1", "2015-1-15", "2015-6-1"))
#' date_factor(dts, type = "yearmonth")
#' date_factor(dts, type = "yearquarter")
#' date_factor(
#'   dateVec = dts, 
#'   type = "yearquarter", 
#'   minDate = as.Date("2015-1-1"), 
#'   maxDate = as.Date("2015-12-31")
#' )
#' date_factor(
#'   dateVec = as.Date(character(0)), 
#'   type = "yearmonth", 
#'   minDate = as.Date("2016-1-1"), 
#'   as.Date("2016-12-31")
#' )
#'
#' @export
#' @import data.table

date_factor <- function(dateVec, type="yearmonth", minDate=min(dateVec, na.rm=TRUE), maxDate=max(dateVec, na.rm=TRUE)){
  # return an ordered factor whose values correspond to dateVec
  # type can be one of {"year", "yearquarter", "yearmonth", "quarter", "month"}
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  first_of_month <- NULL
  end_of_month <- NULL
  
  #--------------------------------------------------
  
  if(!type %in% c("year", "yearquarter", "yearmonth", "quarter", "month"))
    stop('type must be one of {"year", "yearquarter", "yearmonth", "quarter", "month"}')
  
  if(length(dateVec) == 0 & (is.na(minDate) | is.na(maxDate)))
     stop("dateVec is length 0. If this is expected, minDate and maxDate must be specified to determine factor levels.")
  
  if(minDate > maxDate)
    stop("minDate > maxDate")
  
  #--------------------------------------------------
  # Helpers
  
  # Helper method to get first-of-month of given dates
  first_of_month <- function(somedate, p=as.POSIXlt(somedate)){
    # Returns the first day in this month
    return(as.Date(utils::modifyList(p, list(mon=p$mon, mday=1))))
  }
  
  # Helper method to get end-of-month of given dates
  end_of_month <- function(somedate, p=as.POSIXlt(somedate)){
    # Returns the last day in this month
    return(as.Date(utils::modifyList(p, list(mon=p$mon + 1, mday=0))))
  }
  
  #--------------------------------------------------
  # Check that the dateVec values are within minDate, maxDate bounds
  
  if(length(dateVec) > 0){
    
    if(first_of_month(minDate) > first_of_month(min(dateVec, na.rm = T)))
      warning("minDate > min(dateVec). These cases will coerce to NA")
    
    if(end_of_month(maxDate) < end_of_month(max(dateVec, na.rm = T)))
      warning("maxDate < max(dateVec). These cases will coerce to NA")
  }
  
  #--------------------------------------------------
  
  if(type == "yearquarter"){
    minDate <- as.Date(paste0(year(minDate), "-", floor(month(minDate)/3)*3L + 1L, "-1"))
    maxDate <- as.Date(paste0(year(maxDate), "-", ceiling(month(maxDate)/3)*3L, "-1"))
  } else{
    minDate <- first_of_month(minDate)
    maxDate <- first_of_month(maxDate)
  }
  
  if(type == "yearmonth"){
    dts <- seq.Date(minDate, maxDate, by="month")
    result <- factor(format(dateVec, "%Y %b"), levels = format(dts, "%Y %b"), ordered=TRUE)
  } else if(type == "yearquarter"){
    year_quarter <- function(x) paste0(year(x), " Q", quarter(x))
    dts <- seq.Date(minDate, maxDate, by="3 months")
    result <- factor(year_quarter(dateVec), levels = year_quarter(dts), ordered=TRUE)
  } else if(type == "year"){
    dts <- seq.Date(minDate, maxDate, by="year")
    result <- factor(year(dateVec), levels = year(dts), ordered=TRUE)
  } else if(type == "quarter"){
    get_quarter <- function(x) paste0("Q", quarter(x))
    dts <- seq.Date(as.Date("2016-1-1"), as.Date("2016-12-1"), by="3 months")
    result <- factor(get_quarter(dateVec), levels = get_quarter(dts), ordered=TRUE)
  } else if(type == "month"){
    dts <- seq.Date(as.Date("2016-1-1"), as.Date("2016-12-1"), by="month")
    result <- factor(format(dateVec, "%b"), levels = unique(format(dts, "%b")), ordered=TRUE)
  }
  
  return(result)
}
