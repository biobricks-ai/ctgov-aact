options(timeout=1800) # download timeout

source('stages/lib/download_meta.R')

# read find daily archive file to download
daily_archives <- get_daily_archives_from_page('flatfiles')
latest_row <- ( daily_archives
  |> arrange(mdy(date))
  |> tail(1)
)

# download file to download directory
name     <- latest_row$file[[1]]
download <- fs::dir_create("download") |> fs::path(name)
url      <- latest_row$url[[1]]
print(paste("Downloading", url, "to", download))
download.file(url,download)
