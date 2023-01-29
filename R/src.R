#' Check date type
#' @noRd
date_check <- function(date)
{
  if(!any(class(date) %in% c("POSIXct", "POSIXt", "Date")))
  {
    stop('date must be a valid `date` type. Try using as.Date(date) to convert the variable to date format')
  }
  TRUE
}

#' Put a string within parenthesis
#' @noRd

inpar <- function(x)
{
  paste0(' (', as.character(x), ')')
}


#' neat alias of the week day with reference based on current date
#' @param date a Date or POSIX time stamp
#' @param reference.alias a Boolean. If set to TRUE, a reference alias of week day is shown based
#' on current date such as Today/Yesterday/Tomorrow/Last/Coming.
#' @return week day of the date in a readable format with reference alias based on current date
#' @examples
#' # Get day of the week of current date without reference alias
#' x <- Sys.Date()
#' nday(x, reference.alias = FALSE)
#' # Get day of the week with reference alias
#' nday(x, reference.alias = TRUE)
#' @export

nday <- function(date, reference.alias = FALSE)
{
  date_check(date)
  out <- format(date, '%a')
  if(reference.alias)
  {
    today <- Sys.Date()
    day_delta <- today - as.Date(date)
    day_alias <- fcase(
      day_delta >= 2 & day_delta <= 6, 'Last ',
      day_delta == 1, 'Yesterday, ',
      day_delta == 0, 'Today, ',
      day_delta == -1, 'Tomorrow, ',
      day_delta >= -6 & day_delta <= -2, 'Coming ',
      default = ''
    )
    out <- paste0(day_alias, out)
  }
  return(out)
}


#' neat representation of dates
#' @param date a Date or POSIX time stamp
#' @param include.alias a Boolean. If the reference alias of date to be included or not such as Today/Yesterday/Tomorrow/Last/Coming. See examples below.
#' @param is.month a Boolean variable representing if the date represents month. If this set to TRUE,
#' the function returns 'MMMM'YY' as the output which is a neater representation of month.
#' @return String representation of the date
#' @examples
#' # Neat representation of current date
#' x <- Sys.Date()
#' ndate(x)
#' # Neat representation of current date with reference alias.
#' ndate(x, include.alias = FALSE)
#' # Neat representation of current date with only month and year
#' ndate(x, is.month = TRUE)
#' @export

ndate <- function(date, include.alias = TRUE, is.month = FALSE)
{
  date_check(date)
  if(is.month)
  {
    out <- format(date, "%b'%y")
  } else {
  if(include.alias)
  {
    date_alias <- inpar(nday(date, reference.alias = TRUE))
  } else {
    date_alias <- rep('', length(date))
  }
  out <- paste0(format(date, '%b %d, %Y'), date_alias)
  }
  return(out)
}

#' neat representation of time stamp
#' @param timestamp a POSIX time stamp
#' @param include.date a Boolean representing if the date of time stamp to be included. By default it is set to TRUE
#' @param include.minutes a Boolean representing if the minutes to be included. By default it is set to TRUE
#' @param include.seconds a Boolean representing if the seconds to be included. By default it is set to TRUE
#' @param include.timezone a Boolean variable representing if the timezone of the date variable
#' to be included. By default it is set to TRUE
#' @return String representation of time stamp
#' @examples
#' # Neat representation of time stamp
#' x <- Sys.time()
#' ntimestamp(x)
#' # Neat representation of time from a time stamp
#' ntimestamp(x, include.date = FALSE, include.seconds = FALSE, include.timezone = FALSE)
#' @export

ntimestamp <- function(timestamp, include.date = TRUE, include.minutes = TRUE, include.seconds = TRUE,
                       include.timezone = TRUE)
{
  if(!any(class(timestamp) %in% c("POSIXct", "POSIXt")))
  {
    stop('timestamp should be a valid POSIX timestamp. Try using as.POSIXct(timestamp)')
  }
  defaults <- rep('', length(timestamp))
  if(include.minutes)
  {
    mins <- format(timestamp, ' %MM')
  } else {
    mins <- defaults
  }
  if(include.seconds)
  {
    secs <- format(timestamp, ' %SS')
  } else {
    secs <- defaults
  }
  if(include.timezone)
  {
    tz <- format(timestamp, ' %Z')
  } else {
    tz <- defaults
  }
  if(include.date)
  {
    date <- format(timestamp, '%b %d, %Y ')
  } else {
    date <- defaults
  }
  hour <- format(timestamp, '%IH')
  am_pm <- format(timestamp, ' %p')
  paste0(date, hour, mins, secs, am_pm, tz)
}


#' neat representation of numbers
#' @param number an integer or double
#' @param digits number of digits to round-off. Default value is 1.
#' @return String representation of numbers with suffix denoting K for thousands,
#' Mn for millions, Bn for billions, Tn for trillions. A number lower than thousand is
#' represented as it is.
#' @examples
#' x <- c(10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
#' nnumber(x)
#' nnumber(123456789.123456, digits = 1)
#' @export

nnumber <- function(number, digits = 1) {
  if(!is.numeric(number)) {
    stop('number must be of numeric type.
    Try as.numeric(x) to convert to numeric type variable')
  }
  num_format <- function(n, ...) {
    suf <- c('', ' K', ' Mn', ' Bn', ' Tn')
    k <- ifelse(n == 0, 0, log10(abs(n))/3)
    mx <- max(0, min(length(suf) -1 ,
                     as.integer(floor(k))))
    sn <- suf[mx + 1]
    sx <- round(n / 10^(3 * mx), digits)
    paste0(sx, sn)
  }
  unlist(lapply(number, num_format))
}

#' Pretty printing of percentages
#' @noRd

pct <- function(x, is.decimal = TRUE) {
  if(is.decimal)
    {
    x <- x * 100
  }
  return(x)
}


#' Add + or - sign before the number
#' @noRd

add_sign <- function(x, plus.sign = TRUE) {
  ifelse(plus.sign & x > 0, paste0('+', x), x)
}

#' Add percentage symbol at the end of the number
#' @noRd

add_psym <- function(x) {
  paste0(x, '%')
}

#' neat representation of percentage
#' @param percent an integer or double representing percentage
#' @param is.decimal a Boolean variable. If the percent is raw, the value to set as TRUE. See examples below.
#' If the percent variable is already pre-multiplied by 100 then the value to be set as FALSE.
#' @param digits number of digits to round-off
#' @param factor.out an optional Boolean variable. If the percent exceeds |100%| then a string representing growth or drop as
#' readable factors. See examples below.
#' @param plus.sign a Boolean variable. If the percent is positive then setting plus_sign = TRUE, includes an explicit + sign before the percent
#' @return String representation of the percentages.
#' @examples
#' # Formatting 22.3%
#' npercent(0.223, is.decimal = TRUE, digits = 1)
#' npercent(22.3, is.decimal = FALSE, digits = 1)
#' # Formatting percentages with growth factors
#' npercent(c(-4.01, 2.56), is.decimal = TRUE, factor.out = TRUE)
#' @export


npercent <- function(percent, is.decimal = TRUE, digits = 1,
                     plus.sign = TRUE, factor.out = FALSE) {
  if(!is.numeric(percent)) {
    stop('percent must be of numeric type representing a percentage.
         Try as.numeric(x) to convert to numeric type')
  }
  out <- percent %>%
    pct(is.decimal)

  if(factor.out)
  {
    gtemp <- round(out/100,1)
    gtemp_abs <- abs(gtemp)
    gfactor <- ifelse(gtemp >= 1, inpar(paste0(gtemp_abs, 'x growth')),
                      ifelse(gtemp <= -1, inpar(paste0(gtemp_abs, 'x drop')), ''))
  } else {
    gfactor <- rep('', length(percent))
  }

  out <- paste0(out %>%
    round(digits = digits) %>%
    add_sign(plus.sign = plus.sign) %>%
    add_psym(), gfactor)

  return(out)
}

#' convert string to title case
#' @noRd
totitle <- function(x) {
  toTitleCase(tolower(x))
}

#' convert string to start case
#' @noRd
tostart <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse =" ")
}

#' case conversion
#' @noRd

convert_case <- function(x, case) {
  switch(case,
         lower = tolower(x),
         upper = toupper(x),
         title = totitle(x),
         start = tostart(x))
}

#' Removing punctuation and special characters from a string
#' @noRd

clean_text <- function(x)
{
  gsub("[^[:alnum:][:space:]]","", x)
}

#' White space cleaning
#' @noRd

clean_space <- function(x)
{
  trimws(gsub("[ ]+"," ",x))
}

#' neat representation of string
#' @param string a string / character
#' @param case an optional parameter to convert the string variable to specific case.
#' By default the case of the string is kept as it is. The available case conversions are lower, upper, title and start case.
#' @param remove.specials an optional parameter. set to TRUE if special characters including any punctuation
#' to be removed from the string
#' @return White space cleaned and optionally formatted by case conversion and removal of special characters of the input string.
#' @seealso Refer to \url{https://en.wikipedia.org/wiki/Letter_case#Stylistic_or_specialised_usage} for more information about the different cases of text/string.
#' @examples
#' nstring('   All MOdels are wrong.   some ARE useful!!! ', case = 'title', remove.specials = TRUE)
#' nstring("all Models are Wrong some are Useful", case = 'start', remove.specials = TRUE)
#' @export


nstring <- function(string, case = NULL, remove.specials = FALSE)
{
  if(!is.character(string))
  {
    stop('string must be of character type.
         Try as.character(x) to convert to character type variable')
  }
  if(!is.null(case) && !any(case %in% c('lower', 'upper', 'title', 'start')))
  {
    stop('To convert case of the string variable, select case = lower/upper/title/start')
  }
  string <- clean_space(string)
  if(!is.null(case))
  {
    string <- convert_case(string, case)
  }
  if(remove.specials)
  {
    string <- clean_text(string)
  }
  return(string)
}

