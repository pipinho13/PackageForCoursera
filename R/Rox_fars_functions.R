#' read csv
#'
#' reads a filename from the working directory 
#' @param filename A string of the filename
#'
#' @return This function takes as an input the string of the filename which is in the working directory
#'		   and returns a tbl_df object using the dplyr package
#'
#' @importFrom readr, read_csv
#' @importFrom dplyr, tbl_df
#'
#' @details if there is no file such a filename in the working directory it returns a message that that filename does not exist
#'
#'
#' @examples fars_read("accident_2013.csv.bz2")
#' @export 

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}



#' make a file name
#' creates a file name of the form accident_%d.csv.bz2 where %d is an integer
#' @param year An integer value
#'
#' @return This returns a file name of the format accident_%d.csv.bz2 where %d is an integer
#'         thus this is a character object 
#'
#' @examples make_filename(2013)
#'
#'
#'
#' @export 


make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' It returns only the two columns which are Month and the year 
#' @param years A vector of integers
#' @return it takes as an input a vector of integers which are the years and it returns tbl_df object 
#' 
#' @importFrom dplyr, select, mutate
#' 
#' @details In case that there is not a file in the working directory of that particular year it returns an error of "invalid year"
#' 
#' @examples fars_read_years(c(2013,2014,2015))
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


#' It rbind the different tbl_df
#' @param years A vector of integers
#' @return it takes as an input a vector of integers which are the years and it returns tbl_df object 
#' 			which is a summary of the number of observations by year and month
#' 
#' @importFrom dplyr, bind_rows, group_by, summarize 
#' @importFrom tidyr, spread 
#' @details By using the tidyr package it spreads the years to columns so the rows are the Months and the columns the Years
#' 
#' @examples fars_summarize_years(c(2013,2014,2015))
#' @export 
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}




#' It creates a map of the LATITUDE and LONGITUD of the state
#' @param state.num an integer of the state code
#' @param year An integer of the year
#' @return it returns a map of the LATITUDE and LONGITUD of the state when an observation occurs in the dataset
#' 
#' @importFrom dplyr,filter 
#' @importFrom maps, map 
#' @details In case where there is not such state.num it returns a message of invalid State number. Also when LONGITUD > 900 then it is assign as NA. 
#'			The same when LATITUDE>90
#' 
#' @examples fars_map_state(1,2013)
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
