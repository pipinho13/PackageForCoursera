
#' Data Input
#'
#' @description Reads a file in table format and creates a data frame table from it.
#' @param filename the name of the file which the data are to be read from.
#'
#' @return returns a data frame table if the file exists, throws an error if the file
#'   doesn't exist. If the filename does not contain an absolute path, the filename
#'   is relative to the current working directory, \code{getwd()}.
#' @export
#'
#' @importFrom dplyr tbl_df %>%
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#'   fars_read("accident_2013.csv.bz2")
#' }
#'
fars_read <- function(filename) {
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate file names for data files from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System
#' \href{http://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Analysis Reporting System (FARS)}.
#'
#' @param year An integer or string indicating the year that the data file generated
#'
#' @return an string as the name of the file
#' @export
#' @examples
#' \dontrun{
#'  make_filename(1992)
#'  make_filename("2012")
#'  }
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Load several year of FARS data files
#'
#' @param years a vector of integer or strings that indicated the year of the FARS data files
#'
#' @return a list of FARS data for the years in the parameter \code{years}
#' @export
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom stats setNames
#'
#' @examples
#'   \dontrun{
#'     fars_read_years(c(2013, 2014))
#'     fars_read_years(c("2013", "2014"))
#'   }
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, .dots = setNames(list(~year)), year) %>%
        dplyr::select_(~MONTH, ~year)
    },
    error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Monthly number of accidents in each year using the data from FARS data
#'
#' @param years a list of FARS data for the years in the parameter \code{years}
#'
#' @return A dataframe with monthly number of accidents in each row and years as
#' colunms. It uses \code{\link{fars_read_years}} function.
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @importFrom stats setNames
#' @examples
#'   \dontrun{
#'     fars_summarize_years(c(2013, 2014))
#'     fars_summarize_years(c("2013", "2014"))
#'   }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(.dots = setNames(~n(), "n")) %>%
    tidyr::spread_(~year, ~n)
}

#' Draws the location of accidents in given state for a given year
#'
#' @param state.num An integer that represents the state number
#' @param year An integer or string
#'
#' @return plots a map of the state with the accidents as dots on the map.
#' It throws an error if \code{"state.num"} is not invalid. If there hasn't been
#' any accident in that year, it produces a message.
#' @export
#'
#' @importFrom dplyr filter %>%
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'   \dontrun{
#'     fars_map_state(48, 2013)
#'     fars_map_state(48, 2014)
#'     fars_map_state(6, 2013)
#'   }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
  if (nrow(data.sub) == 0L) {
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

