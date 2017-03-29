#' creates a dataframe reading data from a specified file
#'
#' This function loads a zipped csv file as a dataframe. Returns an error if the file doesn't exists
#' The file contains data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System,
#' which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @param filename A character string matching the complete filename
#' You can use 'make_filename()' function to create filename for a given year
#'
#' @return A dataframe from data readed from specified csv file
#'
#' @export
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' creates a full file name depending on the given year
#'
#' This function creates a full file name depending on the given year. Returns an error if the parameter is not a number.
#'
#' @param year A character string of the year for which to create the filename
#'
#' @return A character string matching the complete filename
#'
#' @export
#'
#' @examples
#' make_filename(2013)
#' #return "accident_2013.csv.bz2"
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Creates a list of dataframe (one for every specified year)
#'
#' This function loads a zipped csv file as a dataframe for every valid specified year. Returns a warning for every specified year for which
#' doesn't exist a matching data file.
#' Every created dataframe has 2 columns (MONTH, year)
#' This function needs 'readr' and 'dplyr' packages.
#'
#' @param years a vector containing the desired years, for example c(2013,2014,2015)
#' A vector of years
#'
#' @return A list of dataframe with accident data for the specified list of years.
#'
#' @export
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
#' fars_read_years(2013:2015)
#'
#' @importFrom dplyr mutate select

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


#' Creates a dataframe that summarize the number of fatal injuries for every month in a year
#'
#' This function loads a zipped csv file as a dataframe for every valid specified year. Returns a warning for every specified year for which
#' doesn't exist a matching data file.
#' The resulting dataframe has 12 rows (one for every month of the year) and n+1 columns where n is the number of valid years specified as argument
#'
#' @param years a vector containing the desired years, for example c(2013,2014,2015)
#'
#' @return a data frame containing the summary of accidents for each month and for each year requested (spreaded in columns)
#'
#' @export
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#' fars_summarize_years(2013:2015)
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' creates a chart with data of the specified year in the specified state
#'
#' This function geolocate the injuries occurred in a state. Returns an error if the file (for specified year) or state code doesn't
#' exists.
#'
#' @param state.num A character string of code number of the state for which the graphic has to be created
#' You can obtain all state code using
#' data <- fars_read_years(2014)
#' unique(data$STATE)
#'
#' @param year A character string of the year for data to plot
#'
#' @return This function creates a graphic but doesn't return an object.
#'
#' @export
#'
#' @examples
#' fars_map_state(1,2015)
#' fars_map_state(55,2013)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points


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
