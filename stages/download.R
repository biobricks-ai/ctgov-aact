library(rvest)
library(purrr)
library(fs)
library(stringr)

options(timeout=1800) # download timeout

# read html from page and grab file to download
page   <- read_html("https://aact.ctti-clinicaltrials.org/downloads/snapshots?type=flatfiles")
nodes  <- page |> html_nodes("a")
recent <- detect(nodes, ~ grepl("*.zip", . |> html_text() ))

# download file to download directory
name     <- recent |> html_text() |> stringr::str_trim()
download <- fs::dir_create("download") |> fs::path(name)
url      <- recent |> html_attr("href")
print(paste("Downloading", url, "to", download))
download.file(url,download)
