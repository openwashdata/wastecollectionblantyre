# description -------------------------------------------------------------

# load packages -----------------------------------------------------------

library(tidyverse)

# read --------------------------------------------------------------------

data_in1 <- read_delim("data-raw/vehicles.csv", delim = ";") |>
  janitor::clean_names()

data_in2 <- read_delim("data-raw/skip_levels.csv", delim = ";") |>
  janitor::clean_names()


# data transformation -----------------------------------------------------

vehicles <- data_in1

skip_level <- data_in2 |>
  select(market_contact:skip_number) |>
  rename(name = norm_name,
         name_contact = market_contact,
         level = assessed_level) |>
  unite(col = "date_time", date_of_pic:time, sep = " ") |>
  mutate(date_time = lubridate::dmy_hms(date_time)) |>
  relocate(date_time) |>
  relocate(filename, .after = skip_number)

## code to prepare `DATASET` dataset goes here

usethis::use_data(vehicles, skip_level, overwrite = TRUE)


# dictionary --------------------------------------------------------------

# prepare dictionaires ----------------------------------------------------

library(tibble)

get_variable_info <- function(data, directory = "", file_name = "") {
  total_variables <- sum(sapply(data, function(df) length(names(df))))

  variable_info <- tibble(
    directory = character(total_variables),
    file_name = character(total_variables),
    variable_name = character(total_variables),
    variable_type = character(total_variables),
    description = character(total_variables)
  )

  index <- 1

  for (i in seq_along(data)) {
    dataframe <- data[[i]]
    variable_names <- names(dataframe)
    variable_types <- sapply(dataframe, typeof)

    num_variables <- length(variable_names)
    variable_info$variable_name[index:(index + num_variables - 1)] <- variable_names
    variable_info$variable_type[index:(index + num_variables - 1)] <- variable_types
    variable_info$file_name[index:(index + num_variables - 1)] <- rep(file_name[i], num_variables)
    variable_info$directory[index:(index + num_variables - 1)] <- rep(directory[i], num_variables)

    index <- index + num_variables
  }

  return(variable_info)
}


# Specify values for directory and file_name
directories <- c("data/", "data/")
file_names <- c("vehicles.rda", "skip_level.rda")

dictionary <- get_variable_info(data = list(vehicles, skip_level),
                                directory = directories,
                                file_name = file_names)

dictionary |>
  openxlsx::write.xlsx("data-raw/dictionary.xlsx")
