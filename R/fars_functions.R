#' Reads data from a CSV file into a data frame tbl
#'
#' @param filename (string) Filename of the file to read
#'
#' @return The data from the given CSV file as a data frame tbl (see tibble-package)
#' Stops when the input file does not exist.
#'
#' @examples \dontrun{
#'   fars_read('accident_2013.csv.bz2')
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates the filename to load FARS data of a particular year
#'
#' @param year The year (integer) to get the filename for
#'
#' @return The filename (string) of the FARS data for a particular year
#'
#' @examples \dontrun{
#'   make_filename(2014)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("data/accident_%d.csv.bz2", year)
}

#' Reads FARS data and retrieves the month and year of each accident from a given list of years
#'
#' @param years A list of years (integers) to get the data for
#'
#' @return A list with a table (data frame) per year with the columns MONTH and year of each accident
#' Shows a warning when an invalid year is specified or a year for which no data is available
#'
#' @examples \dontrun{
#'   fars_read_years(2013:2014)
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate %>%
#'
#' @export
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

#' Reads FARS data and retrieves the number of accidents per month for each wanted year
#'
#' @param years A list of years (integers) to get the data for
#'
#' @return A table (data frame) with, for each asked year, the number of accidents per month
#' Shows a warning when an invalid year is specified or a year for which no data is available
#'
#' @examples \dontrun{
#'   fars_summarize_years(2013:2014)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Shows a map of a given state with a dot for every accident that happened in a given year
#'
#' @param state.num The number of a state (integer) to plot the data for
#' @param year The year to map the data for (integer)
#'
#' @return None
#' Shows a message when an invalid state number is given or when no accidents were found to plot
#'
#' @examples \dontrun{
#'   fars_map_state(1, 2014)
#' }
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @export
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
