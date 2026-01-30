source('stages/lib/download_meta.R')
library(purrr)

# Main execution
message("Starting monthly metadata download")

# Process both types
c("pgdump", "flatfiles") |> walk(process_type)

message("Done!")
