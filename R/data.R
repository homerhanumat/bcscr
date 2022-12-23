#' Speed and Fuel Efficiency (British Ford Escort)
#'
#' A British Ford Escort was driven along a prescribed course.  Each drive was
#' done at a different speed, and the fuel efficiency was recorded for each
#' drive.
#'
#'
#' @name fuel
#' @docType data
#' @format A data frame with 15 observations on the following 2 variables.
#' \describe{
#' \item{speed}{in kilometers per hour.}
#' \item{efficiency}{fuel efficiency, measured in liters of fuel required to travel 100 kilometers. }
#' }
#' @source The Basic Practice of Statistics, by Moore and McCabe.
#' @keywords datasets

NA

#' @title MAT 111 Survey
#'
#' @description Results of a survey of MAT 111 students at Georgetown College.
#'
#' \itemize{
#'       \item height.  How tall are you, in inches?
#'       \item ideal_ht.  A numeric vector  How tall would you LIKE to be, in inches?
#'       \item sleep.  How much sleep did you get last night?
#'       \item fastest.  What is the highest speed at which you have ever driven a car?
#'       \item weight_feel.  How do you feel about your weight?
#'       \item love_first.  Do you believe in love at first sight?
#'       \item extra_life.  Do you believe in extraterrestrial life?
#'       \item seat.  When you have a choice, where do you prefer to sit in a classroom?
#'       \item GPA.  What is your college GPA?
#'       \item enough_Sleep.  Do you think you get enough sleep?
#'       \item sex.  What sex are you?
#'       \item diff.  Your ideal height minus your actual height.
#' }
#'
#' @docType data
#' @keywords datasets
#' @source Georgetown College, MAT 111.
#' @format A data frame with 71 rows and 12 variables
#' @name m111survey


NA

#' @title Names and Phone Numbers
#'
#' @description Sample data for regular expression practice.
#'
#' \itemize{
#'       \item name.  Last name followed by first.
#'       \item phone. Phone number with area code, in several formats.
#' }
#'
#' @docType data
#' @keywords datasets.
#' @format A data frame with 50 rows and 2 variables
#' @name NamePhone


NA

#' Volume of Users of a Rail Trail
#'
#' This data table is modifed slightly from mosaicData::RailTrail,
#' (see http://cran.r-project.org/web/packages/mosaicData/mosaicData.pdf).
#' Description below is drawn from the mosaicData help file.
#'
#' The Pioneer Valley Planning Commission (PVPC) collected data north of Chestnut Street in
#' Florence, MA for ninety days from April 5, 2005 to November 15, 2005. Data collectors set up a
#' laser sensor, with breaks in the laser beam recording when a rail-trail user passed the
#' data collection station.
#'
#' @docType data
#' @name railtrail
#' @usage data(railtrail)
#' @format
#'   A data frame with 90 observations on the following variables.
#'   \itemize{
#'     \item{\code{hightemp}} {daily high temperature (in degrees Fahrenheit)}
#'     \item{\code{lowtemp}} {daily low temperature (in degrees Fahrenheit)}
#'     \item{\code{avgtemp}} {average of daily low and daily high temperature (in degrees Fahrenheit)}
#'     \item{\code{season}} {spring, summer or fall}
#'     \item{\code{cloudcover}} {measure of cloud cover (in oktas)}
#'     \item{\code{precip}} {measure of precipitation (in inches)}
#'     \item{\code{volume}} {estimated number of trail users that day (number of breaks recorded)}
#'     \item{\code{weekday}} {logical indicator of whether the day was a non-holiday weekday}
#'     \item{\code{dayType}} {one of "weekday" or "weekend"}
#'   }
#'
#' @details
#' There is a potential for error when two users trigger the infrared beam at exactly the same time since the counter
#' would only logs one of the crossings.  The collectors left the motion detector out during the winter, but because the
#' counter drops data when the temperature falls below 14 degrees Fahrenheit, there is no data for the cold winter months.
#'
#' @source
#' Pioneer Valley Planning Commission
#'
#' @references
#' http://www.fvgreenway.org/pdfs/Northampton-Bikepath-Volume-Counts%20_05_LTA.pdf
#'
#' @examples
#' data(railtrail)
#'

NA

