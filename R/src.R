#' Check date type
#' @noRd

date_check <- function(date)
{
  chk <- any(class(date) %in% c("POSIXct", "POSIXt", "Date"))
  if(!chk)
  {
    e <- paste0('`', deparse(substitute(date)), '` must be a date or timestamp variable')
    stop(e)
  }
}

#' Extended check for null or NA values
#' @noRd

is_na <- function(x)
{
  if(is.null(x) || length(x) == 0L)
  {
    out <- TRUE
  } else {
    out <- is.na(x)
  }
  return(out)
}

#' Check for singletons of numeric
#' @noRd

num_singleton_check <- function(x)
{
  if(!(is.numeric(x) && NROW(x) == 1))
  {
    e <- paste0('`', deparse(substitute(x)), '` must be a string (character type) of length one.')
    stop(e)
  }
}

#' Check for singletons of integer
#' @noRd

int_singleton_check <- function(x)
{
  if(!(any(is.integer(x),
            (is.numeric(x) && x == as.integer(x))) && NROW(x) == 1))
  {
    e <- paste0('`', deparse(substitute(x)), '` must be a integer of length one.')
    stop(e)
  }
}

#' Check for singletons of string
#' @noRd

str_singleton_check <- function(x, is_nullable = FALSE)
{
  na_chk <- ifelse(is_nullable, is.null(x), FALSE)
  chk <- any(na_chk, (is.character(x) && NROW(x) == 1))
  if(!chk)
  {
    e <- paste0('`', deparse(substitute(x)), '` must be a string (character type) of length one.')
    stop(e)
  }
}

#' Check for singletons of boolean
#' @noRd

bool_singleton_check <- function(x)
{
  if(!(is.logical(x) && NROW(x) == 1))
  {
    e <- paste0('`', deparse(substitute(x)), '` must be a boolean (TRUE / FALSE) of length one.')
    stop(e)
  }
}

#' Check timestamp type
#' @noRd

timestamp_check <- function(timestamp)
{
  chk <- any(class(timestamp) %in% c("POSIXct", "POSIXt"))
  if(!chk)
  {
    e <- paste0('`', deparse(substitute(timestamp)), '` must be a timestamp variable')
    stop(e)
  }
}

#' Check list of singleton strings
#' @noRd

lst_str_check <- function(x)
{
  if(!(all(is.list(x), str_singleton_check(x[[1]]))))
  {
    e <- paste0('`', deparse(substitute(x)), '` must be a list of string (character)')
    stop(e)
  }
}

#' Put a string within parenthesis
#' @noRd

inpar <- function(x)
{
  paste0(' (', as.character(x), ')')
}


#' neat alias of the week day with reference based on current date
#' @param date a Date or POSIX time stamp
#' @param reference_alias a Boolean. If set to TRUE, a reference alias of week day is shown based
#' on current date such as Today/Yesterday/Tomorrow/Last/Coming.
#' @return week day of the date in a readable format with reference alias based on current date
#' @examples
#' # Get day of the week of current date without reference alias
#' x <- Sys.Date()
#' nday(x, reference_alias = FALSE)
#' # Get day of the week with reference alias
#' nday(x, reference_alias = TRUE)
#' @export

nday <- function(date, reference_alias = FALSE)
{
  date_check(date)
  bool_singleton_check(reference_alias)
  out <- format(date, '%a')
  if(reference_alias)
  {
    today <- Sys.Date()
    day_delta <- today - as.Date(date)
    day_alias <- fcase(
      day_delta >= 2 & day_delta <= 8, 'Last ',
      day_delta == 1, 'Yesterday, ',
      day_delta == 0, 'Today, ',
      day_delta == -1, 'Tomorrow, ',
      day_delta >= -8 & day_delta <= -2, 'Coming ',
      default = ''
    )
    out <- paste0(day_alias, out)
  }
  return(out)
}


#' neat representation of dates
#' @param date a Date or POSIX time stamp
#' @param display_weekday a Boolean. Whether the weekday of the date
#' to be included.
#' @param is_month a Boolean variable representing if the date represents month.
#' If this set to TRUE,
#' the function returns 'MMMM'YY' as the output which is a neater
#' representation of month.
#' @return String representation of the date
#' @examples
#' # Neat representation of current date
#' x <- Sys.Date()
#' ndate(x)
#' # Neat representation of current date with day of week.
#' ndate(x, display_weekday = FALSE)
#' # Neat representation of current date with only month and year
#' ndate(x, display_weekday = FALSE, is_month = TRUE)
#' @export

ndate <- function(date, display_weekday = TRUE, is_month = FALSE)
{
  date_check(date)
  bool_singleton_check(display_weekday)
  bool_singleton_check(is_month)
  if(is_month)
  {
    out <- format(date, "%b'%y")
  } else {
  if(display_weekday)
  {
    wd <- inpar(nday(date, reference_alias = FALSE))
  } else {
    wd <- rep('', length(date))
  }
  out <- paste0(format(date, '%b %d, %Y'), wd)
  }
  return(out)
}

#' neat representation of time stamp
#' @param timestamp a POSIX time stamp
#' @param include_date a Boolean representing if the date of time stamp
#' to be included. By default it is set to TRUE.
#' @param display_weekday a Boolean representing if the weekday of the timestamp
#' to be included. By default it is set to TRUE
#' @param include_hours a Boolean representing if the hours to be included.
#' By default it is set to TRUE
#' @param include_minutes a Boolean representing if the minutes to be included.
#' By default it is set to TRUE
#' @param include_seconds a Boolean representing if the seconds to be included.
#' By default it is set to TRUE
#' @param include_timezone a Boolean variable representing if the
#' timezone of the date variable to be included. By default it is set to TRUE.
#' @return String representation of time stamp
#' @examples
#' # Neat representation of time stamp
#' x <- Sys.time()
#' ntimestamp(x)
#' # Neat representation of time from a time stamp
#' ntimestamp(x, include_date = FALSE, include_seconds = FALSE,
#' include_timezone = FALSE)
#' @export

ntimestamp <- function(timestamp, display_weekday = TRUE, include_date = TRUE,
    include_hours = TRUE, include_minutes = TRUE, include_seconds = TRUE,
    include_timezone = TRUE)
{
  timestamp_check(timestamp)
  bool_singleton_check(display_weekday)
  bool_singleton_check(include_date)
  bool_singleton_check(include_hours)
  bool_singleton_check(include_minutes)
  bool_singleton_check(include_seconds)
  bool_singleton_check(include_timezone)

  defaults <- rep('', length(timestamp))

  if(include_hours)
  {
    hour <- format(timestamp, '%IH')
  } else {
    hour <- defaults
  }
  if(include_minutes)
  {
    mins <- format(timestamp, ' %MM')
  } else {
    mins <- defaults
  }
  if(include_seconds)
  {
    secs <- format(timestamp, ' %SS')
  } else {
    secs <- defaults
  }
  if(include_timezone)
  {
    tz <- toupper(format(timestamp, ' %Z'))
  } else {
    tz <- defaults
  }
  if(include_date)
  {
    date <- format(timestamp, '%b %d, %Y ')
  } else {
    date <- defaults
  }

  am_pm <- toupper(format(timestamp, ' %p'))
  out <- paste0(date, hour, mins, secs, am_pm, tz)
  if (display_weekday) {
    wd <- inpar(nday(timestamp, reference_alias = FALSE))
    out <- paste0(out, wd)
  }
  return(out)
}

#' Sandwiching a variable with prefix and suffix, outputs a string of the input
#' with optional prefix and suffix
#' @noRd

sandwich <- function(x, prefix = "", suffix = "")
{
  clean_space(paste0(prefix, x, suffix))
}

#' Add comma or dot separation of thousands to chunk large numbers
#' @noRd

chunk_digits <- function(x, thousand_separator = ",")
{
  if(all(x == round(x, 1)))
  {
    nsmall <- 0
  } else {
    nsmall <- 1
  }
  decimal_separator <- ifelse(thousand_separator == ".", ",", ".")
  prettyNum(round(x, 1), nsmall = nsmall, big.mark = thousand_separator,
            decimal.mark = decimal_separator, scientific = FALSE)
}

#' Check for empty string
#' @noRd

is_empty <- function(x)
{
  if(!is.character(x))
  {
    stop('Input should be a character.')
  }
  trimws(x) == ''
}

#' Coalesce for null or na values in a vector
#' @noRd

coalesce <- function(x, replace_by)
{
  if(is.null(x))
    {
      x <- replace_by
    } else {
      x[is_na(x)] <- replace_by
  }
  if(is.character(x))
  {
    x[is_empty(x)] <- replace_by
  }
  return(x)
}

#' Custom number formatting based on the values
#' @noRd

num_format <- function(n, ul, digits)
{
  ul <- ifelse(ul == '', '', paste0(' ', ul))
  k <- ifelse(n == 0, 0, log10(abs(n))/3)
  mx <- max(0, min(length(ul) -1 , as.integer(floor(k))))
  sn <- ul[mx + 1]
  sx <- nround(n / 10^(3 * mx), digits)
  paste0(sx, sn)
}

#' neat representation of numbers
#' @param number an integer or double.
#' @param digits number of digits to round-off. Default value is 1.
#' @param unit unit to which the number to be converted. See examples below.
#' @param unit_labels a vector of strings (optional) that gives the unit label for
#' thousand, million, billion and trillion.
#' @param prefix a string (optional) that can be prepended to the formatted
#' number.
#' @param suffix a string (optional) that can be appended at the end of the
#' formatted number.
#' @param thousand_separator a character (optional) that can be used to chunk thousands
#' to display large numbers. Default is set as comma, dot, comma or underscore
#' can be used.
#' @return String representation of numbers with suffix denoting K for thousands,
#' Mn for millions, Bn for billions, Tn for trillions. A number lower than thousand is
#' represented as it is.
#' @examples
#' x <- c(10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
#' nnumber(x)
#' nnumber(123456789.123456, digits = 1)
#' nnumber(123456789.123456, digits = 1, unit = 'Mn', prefix = '$')
#' @export

nnumber <- function(number, digits = 1, unit = 'custom',
  unit_labels = list(thousand = 'K', million = 'Mn',
                  billion = 'Bn',trillion = 'Tn'),
  prefix = '', suffix = '', thousand_separator = ",")
{
  if(!is.numeric(number)) {
    stop('number must be a numeric type variable (vector).
    Try as.numeric(x) to convert to numeric type variable')
  }
  int_singleton_check(digits)
  str_singleton_check(unit)
  lst_str_check(unit_labels)
  str_singleton_check(prefix)
  str_singleton_check(suffix)

  if(!any(thousand_separator %in% c(",", ".", "_", "'", " ")))
  {
    stop('thousand_separator to distinguish thousands can take any of the below values
         `.`, `,`, `_` Default is set as comma`,`')
  }
  ul <- unname(unlist(c('',
                 coalesce(unit_labels[['thousand']], 'K'),
                 coalesce(unit_labels[['million']], 'Mn'),
                 coalesce(unit_labels[['billion']], 'Bn'),
                 coalesce(unit_labels[['trillion']], 'Tn'))))

  unit_factor <- c(1,1e-3,1e-6,1e-9,1e-12)

  y <- unlist(lapply(number, num_format, ul, digits))
  if(unit != 'custom')
  {
    if(unit == "auto")
    {
      fmt <- names(which.max(table(gsub("[^[:alpha:]]", "", y))))[1]
      unit <- which(fmt == ul)
    } else {
      unit <- match(unit, ul)
      fmt <- ul[unit]
    }
    if(is.na(unit))
    {
      stop("`unit` parameter must be one of the following,
         '', 'K', 'Mn', 'Bn', 'Tn' or 'auto' or 'custom' or a custom specified
           value in the `unit_labels` list.")
    }
    ytemp <- chunk_digits(round(number * unit_factor[unit], digits),
                          thousand_separator)
    ytemp <- ifelse(ytemp == '0', '<0.1', ytemp)
    y <- paste0(ytemp, ' ', fmt)
  }
  sandwich(y, prefix = prefix, suffix = suffix)
}

#' Pretty printing of percentages
#' @noRd

pct <- function(x, is_decimal = TRUE)
{
  if(is_decimal)
  {
    x <- x * 100
  }
  return(x)
}


#' Add + or - sign before the number
#' @noRd

add_sign <- function(x, plus_sign = TRUE)
{
  ifelse(plus_sign & x > 0, paste0('+', x), x)
}

#' Add percentage symbol at the end of the number
#' @noRd

add_psym <- function(x)
{
  paste0(x, '%')
}

#' Round a number and display 0 digit in decimals
#' @noRd

nround <- function(x, digits = 1)
{
  trimws(format(round(x, digits = digits), nsmall = digits))
}

#' Show percentage in basis points
#' @noRd

num_sign <- function(x)
{
  ifelse(x >= 0, "+", "-")
}

#' Growth label based on the sign of the value
#' @noRd

sign_label <- function(x)
{
  ifelse(x == 0, "Flat", ifelse(x > 0, "Growth", "Drop"))
}

#' Basis point calculation
#' @noRd

nbps <- function(x) {
  paste0(ifelse(x >= 0, "+", ""), x * 10000, " bps")
}


#' neat representation of percentage
#' @param percent an integer or double representing percentage
#' @param is_decimal a Boolean variable. If the percent is raw,
#' the value to set as TRUE. See examples below.
#' If the percent variable is already pre-multiplied by 100
#' then the value to be set as FALSE.
#' @param digits number of digits to round-off
#' @param factor_out an optional Boolean variable.
#' @param basis_points_out an optional parameter to get the percentage as basis points
#' If the percent exceeds |100%| then a string representing growth or drop as
#' readable factors. See examples below.
#' @param plus_sign a Boolean variable. If the percent is positive
#' then setting plus_sign = TRUE, includes an explicit + sign before the percent
#' @return String representation of the percentages.
#' @examples
#' # Formatting 22.3%
#' npercent(0.223, is_decimal = TRUE, digits = 1)
#' npercent(22.3, is_decimal = FALSE, digits = 1)
#' # Formatting percentages with growth factors
#' npercent(c(-4.01, 2.56), is_decimal = TRUE, factor_out = TRUE)
#' @export


npercent <- function(percent, is_decimal = TRUE, digits = 1,
  plus_sign = TRUE, factor_out = FALSE, basis_points_out = FALSE)
{
  if(!is.numeric(percent)) {
    stop('percent must be of numeric type representing a percentage.
         Try as.numeric(x) to convert to numeric type')
  }
  bool_singleton_check(is_decimal)
  int_singleton_check(digits)
  bool_singleton_check(plus_sign)
  bool_singleton_check(factor_out)

  out <- percent %>%
    pct(is_decimal)

  if(basis_points_out)
  {
    bp <- inpar(nbps(percent))
  } else {
    bp <- rep('', length(percent))
  }

  if(factor_out)
  {
    gtemp <- out / 100
    gtemp_abs <- abs(gtemp)
    gfactor <- ifelse(gtemp >= 1, inpar(paste0(round(gtemp_abs,1), 'x Growth')),
                  ifelse(gtemp <= -1, inpar(paste0(round(gtemp_abs,1), 'x Drop')),
                         inpar(sign_label(gtemp))))
  } else {
    gfactor <- rep('', length(percent))
  }

  out <- paste0(out %>%
    nround(digits = digits) %>%
    add_sign(plus_sign = plus_sign) %>%
    add_psym(), gfactor, bp)
  return(out)
}

#' convert string to title case
#' @noRd

totitle <- function(x)
{
  toTitleCase(tolower(x))
}

#' Get nth element from a list
#' @noRd

getnth <- function(l, n)
{
  sapply(l, '[[', n)
}

#' convert string to start case
#' @noRd

tostartTmp <- function(x)
{
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse =" ")
}

#' convert string to start case (for a vector)
#' @noRd

tostart <- function(x)
{
  res <- c()
  for(i in seq_along(x))
  {
    res[i] <- tostartTmp(x[i])
  }
  return(res)
}

#' convert string to lower case with initial letter in upper case
#' @noRd

toinitcapTmp <- function(x)
{
  tmp <- unlist(strsplit(x,""))
  paste0(c(toupper(tmp[1]), tolower(tmp[-1])), collapse = "")
}

#' convert string to lower case with initial letter in upper case (for vector)
#' @noRd

toinitcap <- function(x)
{
  res <- c()
  for(i in seq_along(x))
  {
    res[i] <- toinitcapTmp(x[i])
  }
  return(res)
}

#' case conversion
#' @noRd

convert_case <- function(x, case)
{
  switch(case,
         lower = tolower(x),
         upper = toupper(x),
         title = totitle(x),
         start = tostart(x),
         initcap = toinitcap(x))
}

#' Removing punctuation and special characters from a string
#' @noRd

clean_text <- function(x, whitelist_specials = "")
{
  ptmp <- paste0(whitelist_specials, collapse = "|")
  pattern <- paste0("[^", ptmp, "[:alnum:][:space:]]")
  gsub(pattern = pattern,"", x)
}


#' White space cleaning
#' @noRd

clean_space <- function(x)
{
  trimws(gsub("[ ]+"," ",x))
}

#' Removing non-english characters
#' @noRd

strip_non_english_tmp <- function(x)
{
  y <- unlist(strsplit(x, ""))
  pos <- utf8ToInt(x) <= 122
  trimws(paste0(y[pos], collapse = ""))
}

#' Vector version of strip_non_english
#' @noRd

strip_non_english <- function(x)
{
  unlist(lapply(x, strip_non_english_tmp))
}

#' neat representation of string
#' @param string a string / character
#' @param case an optional parameter to convert the string variable
#' to specific case. By default the case of the string is kept as it is.
#' The available case conversions are lower, upper, title, start and initcap case.
#' @param remove_specials an optional boolean. To remove special characters
#' including any punctuation to be removed from the string, set this to TRUE.
#' @param whitelist_specials an optional vector of strings. If any
#' special characters to be retained while remove_specials is set to TRUE.
#' See examples below.
#' @param en_only an optional parameter taking boolean values, if set to TRUE,
#' only english alphabets (and numbers) are kept in the string.
#' Non english characters are removed.
#' @return White space cleaned and optionally formatted by case conversion
#' and removal of special characters of the input string.
#' @seealso Refer to \url{https://en.wikipedia.org/wiki/Letter_case#Stylistic_or_specialised_usage} for more information about the different cases of text/string.
#' @examples
#' nstring('   All MOdels are wrong.   some ARE useful!!! ', case = 'title', remove_specials = TRUE)
#' nstring("all Models are Wrong some are Useful", case = 'start', remove_specials = TRUE)
#' nstring('variable_123!!', remove_specials = TRUE, whitelist_specials = c('_'))
#' @export


nstring <- function(string, case = NULL, remove_specials = FALSE,
  whitelist_specials = '', en_only = FALSE)
{
  if(!is.character(string))
  {
    stop('string must be of character type.
         Try as.character(x) to convert to character type variable')
  }

  str_singleton_check(case, is_nullable = TRUE)
  bool_singleton_check(remove_specials)
  is.character(whitelist_specials)
  bool_singleton_check(en_only)

  if(!is.null(case) && !any(case %in% c('lower', 'upper', 'title', 'start', 'initcap')))
  {
    stop('To convert case of the string variable, select case = lower/upper/title/start')
  }
  if(!is.null(case))
  {
    string <- convert_case(string, case)
  }
  if(remove_specials)
  {
    string <- clean_text(string, whitelist_specials = whitelist_specials)
  }
  if(en_only)
  {
    string <- strip_non_english(string)
  }
  string <- clean_space(string)
  return(string)
}


