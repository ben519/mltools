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
#' @param type One of {"yearmonth", "yearquarter", "quarter", "month"}
#' @param fullyears Should levels will always start and end at the end of a year? 
#' For example \code{date_factor(as.Date(c("2016-4-1", "2016-7-1")), "yearquarter", fullyears=TRUE)} will return a factor with
#' four levels (Q1, Q2, Q3, Q4) even though only two date values were given.
#'
#' @examples
#' library(data.table)
#' dts <- as.Date(c("2014-1-1", "2015-1-1", "2015-6-1"))
#' date_factor(dts, type="yearmonth")
#' date_factor(dts, type="yearquarter")
#' date_factor(dts, type="yearquarter", fullyears=FALSE)
#'
#' @export
#' @import data.table

date_factor <- function(dateVec, type="yearmonth", fullyears=TRUE){
  # return an ordered factor whose values correspond to dateVec
  # type can be one of {"yearmonth", "yearquarter", "quarter", "month"}
  
  minDate <- min(dateVec, na.rm=TRUE)
  maxDate <- max(dateVec, na.rm=TRUE)
  
  if(fullyears){
    minDate <- as.Date(paste0(year(minDate), "-1-1"))
    maxDate <- as.Date(paste0(year(maxDate), "-12-1"))
  } else{
    minDate <- as.Date(paste0(year(minDate), "-", month(minDate), "-1"))
    maxDate <- as.Date(paste0(year(maxDate), "-", month(maxDate), "-1"))
  }
  
  if(type == "yearmonth"){
    dts <- seq.Date(minDate, maxDate, by="month")
    result <- factor(format(dateVec, "%Y %b"), levels = format(dts, "%Y %b"), ordered=TRUE)
  } else if(type == "yearquarter"){
    year_quarter <- function(x) paste0(year(x), " Q", quarter(x))
    dts <- seq.Date(minDate, maxDate, by="3 months")
    result <- factor(year_quarter(dateVec), levels = year_quarter(dts), ordered=TRUE)
  }else if(type == "year"){
    dts <- seq.Date(minDate, maxDate, by="year")
    result <- factor(year(dateVec), levels = year(dts), ordered=TRUE)
  }else if(type == "quarter"){
    get_quarter <- function(x) paste0("Q", quarter(x))
    dts <- seq.Date(as.Date("2016-1-1"), as.Date("2016-12-1"), by="3 months")
    result <- factor(get_quarter(dateVec), levels = get_quarter(dts), ordered=TRUE)
  }else if(type == "month"){
    dts <- seq.Date(as.Date("2016-1-1"), as.Date("2016-12-1"), by="month")
    result <- factor(format(dateVec, "%b"), levels = unique(format(dts, "%b")), ordered=TRUE)
  }
  
  return(result)
}