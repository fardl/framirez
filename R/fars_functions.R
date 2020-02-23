#'fars_read
#'
#'This is a function that loads the database using the function "read_csv"
#'  from the "readr" package.
#'
#'@param filename A valid .csv file with the information of interest.
#'
#'@import readr
#'
#'@import dplyr
#'
#'@note If the filename given does not exists,
#'      the function stops and prints "' does not exist".
#'      This function also suppresses additional information related
#'      with the csv database characteristics and potential errors.
#'
#'@return This function creates a table from the data frame object.
#'
#'@examples
#'fars_read("dataexample.csv")
#'
#'@export
#'fars_read
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'make_filename
#'
#'This function creates a filename wrapping a year given as input and "accident_%d.csv.bz2"
#'
#'@param year A valid year
#'
#'@note use functions "as.integer" and "sprintf"
#'
#'@inheritParams as.integer, sprintf
#'
#'@return This function returns a name for the file with the given year.
#'
#'@examples
#'make_filename(2010)
#'
#'@export
#'make_filename

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#'.fars_read_years
#'
#'This is a function that creates a set of tables with the MONTH and year from
#' the "accident_%d.csv.bz2" databases for a given set (vector) of years.
#'
#'@param years A vector with years of interest.
#'
#'@inheritParams lapply, make_filename, fars_read, error
#'
#@import dplyr
#'
#'@note If a year is not valid, the fuction prints a warning
#'      of "invalid year: ", and return a NULL result.
#'
#'@return This function creates a table from data frames objects with the MONTH and year.
#'
#'@examples
#'years =c(2000,2001,2002)
#'fars_read_years(years)
#'
#'@export
#'fars_read_years

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#'fars_summarize_years
#'
#'This function creates a table combining the tables generated from
#'  the vectors of years given to the function. The infomation showed
#'  is the main statistics by year of the columns in each database.
#'
#'@param years A vector with years of interest.
#'
#'@inheritParams fars_read_years
#'
#@import dplyr
#'
#'@import tidyr
#'
#'@note This function uses the function bind_rows, group_by and summarize
#'  from the dplyr package and the function spread from the tidyr.
#'
#'@return This function creates a table with the descriptive statistics
#'  by year.
#'
#'@examples
#'years =c(2000,2001,2002)
#'fars_summarize_years(years)
#'
#'@export
#'fars_summarize_years

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#'fars_map_state
#'
#'This function draw a geographical map for a given state and year
#'  containing the information of accidents.
#'
#'@param state.num The number (code) of the state of interest
#'
#'@param year A year of interest.
#'
#'@inheritParams fars_read, make_filename, as.integer
#'
#'@import maps
#'
#@import dplyr
#'
#'@import graphics
#'
#'@note This function provide messages for inexistent states numbers
#'  and the absent of accidents in a given year and/or state.
#'
#'@return This function creates a geographical map for a specific state
#'  and year, containing the information of accidents.
#'
#'@examples
#'state.num = 50
#'year = 2000
#'fars_map_state(state.num,year)
#'
#'@export
#'fars_map_state


fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
