library(dplyr)
library(fs)
library(glue)
library(lubridate)
library(purrr)
library(readr)
library(rvest)
library(stringr)

# Get the latest year available on the snapshots page
get_latest_year <- function(type) {
  url <- glue("https://aact.ctti-clinicaltrials.org/downloads/snapshots?type={type}")
  page <- read_html(url)

  # Find the "year nav" div
  div.year_nav <- ( page
                   |> html_nodes(xpath = r'{//div[contains(@class, "year-navigation")]}') )

  if (length(div.year_nav) != 1) {
    stop("Could not find year navigation div")
  }

  # Get all links from the <div> and the text is a year (YYYY)
  links <- div.year_nav |> html_nodes("a")
  years <- ( links
            |> map_chr(~ html_text(.x) |> str_trim())
            |> str_extract(r"(^\d{4}$)")
            |> as.integer()
            |> na.omit() )

  max(years)
}

extract_data_from_html_table <- function(html_table) {
  if(is.null(html_table)) {
    return(tibble(date = character(), filename = character(), url = character()))
  }

  rows <- ( html_table
           |> html_nodes(xpath = r'{.//div[
                           contains(@class, "snapshots-grid-row")
                           and not(contains(@class, "snapshots-grid-header"))
                         ]}') )

  # Extract data from each row
  archives <- rows |> map_dfr(function(row) {
    data <- row |> html_nodes(xpath = './/div[@data-label]')
    href <- row |> html_nodes(xpath = './/a[1]') |> html_attr('href')

    if (length(data) == 0) {
      return(NULL)
    }

    # Extract data-label attributes and clean them to use as column names
    labels <- ( data
               |> html_attr('data-label')
               |> str_to_lower()
               |> str_remove(':$') )

    # Extract text values
    values <- data |> html_text() |> str_trim()

    # Create named list
    row_data <- as.list(setNames(values, labels))

    # Add URL from href
    if (length(href) > 0 && !is.na(href[1])) {
      row_data$url <- href[1]
    }

    as_tibble(row_data)
  })

  archives
}

# Get monthly archives metadata for a specific type and year
get_monthly_archives <- function(type, year) {
  url <- paste0("https://aact.ctti-clinicaltrials.org/downloads/snapshots?type=", type, "&year=", year)

  print(paste("Fetching", type, "archives for year", year))

  page <- read_html(url)

  # Find the "Monthly Archives" table
  tables <- ( page
             |> html_nodes(xpath = r'{//div[contains(@class, "snapshots-section")
                 and contains(., "Monthly Archives")]
                 //div[contains(@class, "snapshots-grid-table")]
                }') )

  if (length(tables) != 1) {
    warning(paste("No monthly archives table found for", type, year))
    return(extract_data_from_html_table(NULL))
  }

  # Get the monthly archives table
  monthly_table <- tables[[1]]
  archives <- extract_data_from_html_table(monthly_table)

  return(archives)
}

get_daily_archives_from_page <- function(type, page = 1) {
  url <- paste0("https://aact.ctti-clinicaltrials.org/downloads/snapshots?type=", type, "&page=", page)

  print(paste("Fetching", type, "daily archives on page", page))

  page <- read_html(url)

  # Find the "Recent Daily Snapshots" table
  tables <- ( page
             |> html_nodes(xpath = r'{//div[contains(@class, "snapshots-section")
                 and contains(., "Recent Daily Snapshots")]
                 //div[contains(@class, "snapshots-grid-table")]
                }') )

  if (length(tables) != 1) {
    warning(paste("No daily archives table found for", type, page))
    return(extract_data_from_html_table(NULL))
  }

  # Get the daily archives table
  daily_table <- tables[[1]]
  archives <- extract_data_from_html_table(daily_table)

  return(archives)
}

# Get years that already have metadata files
get_existing_years <- function(type) {
  metadata_dir <- fs::path("metadata/export-monthly", type)

  if (!fs::dir_exists(metadata_dir)) {
    return(integer(0))
  }

  files <- fs::dir_ls(metadata_dir, glob = "*.tsv")

  if (length(files) == 0) {
    return(integer(0))
  }

  return ( files
          |> fs::path_file()
          |> fs::path_ext_remove()
          |> as.integer()
          |> sort() )
}

# Determine which years to fetch
determine_years_to_fetch <- function(existing_years, latest_year) {
  all_years <- 2017:latest_year

  if (length(existing_years) == 0) {
    # No existing files, fetch all years from 2017
    return(all_years)
  }

  # Find missing years
  missing_years <- setdiff(all_years, existing_years)

  if (length(missing_years) == 0) {
    # No missing years, just refresh the last existing year through latest
    last_existing_year <- max(existing_years)
    return(last_existing_year:latest_year)
  }

  # Start from the earlier of first_missing or last_existing
  # This fills gaps and refreshes recent years
  first_missing <- min(missing_years)
  last_existing_year <- max(existing_years)
  start_year <- min(first_missing, last_existing_year)

  return(start_year:latest_year)
}

# Process a single type (pgdump or flatfiles)
process_type <- function(type) {
  print(paste("Processing type:", type))

  # Get latest year available
  latest_year <- get_latest_year(type)
  print(paste("Latest year available:", latest_year))

  # Get existing years
  existing_years <- get_existing_years(type)
  print(paste("Existing years:", paste(existing_years, collapse = ", ")))

  # Determine which years to fetch
  years_to_fetch <- determine_years_to_fetch(existing_years, latest_year)

  if (length(years_to_fetch) == 0) {
    print(paste("No years to fetch for", type))
    return()
  }

  print(paste("Years to fetch:", paste(years_to_fetch, collapse = ", ")))

  # Create output directory
  output_dir <- fs::dir_create(fs::path("metadata/export-monthly", type))

  # Get current year and month to filter out current month's entry
  current_date <- Sys.Date()
  current_year <- year(current_date)
  current_month <- month(current_date)

  # Fetch and save metadata for each year
  years_to_fetch |> walk(function(year) {
    archives <- get_monthly_archives(type, year)

    if (nrow(archives) > 0) {
      # Parse date column and filter out current month
      # Date format is MM-DD-YYYY
      archives <- archives |>
        mutate(
          parsed_date = mdy(date),
          archive_year = year(parsed_date),
          archive_month = month(parsed_date)
        ) |>
        filter(!(archive_year == current_year & archive_month == current_month)) |>
        select(-parsed_date, -archive_year, -archive_month)

      if (nrow(archives) > 0) {
        output_file <- fs::path(output_dir, paste0(year, ".tsv"))
        print(paste("Writing", nrow(archives), "archives to", output_file))
        write_tsv(archives, output_file)
      } else {
        print(paste("No archives found for", type, year, "(after filtering current month)"))
      }
    } else {
      print(paste("No archives found for", type, year))
    }
  })
}

