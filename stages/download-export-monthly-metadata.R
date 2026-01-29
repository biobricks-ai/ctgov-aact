source('stages/lib/download_meta.R')
library(purrr)

# Main execution
print("Starting monthly metadata download")

# Process both types
c("pgdump", "flatfiles") |> walk(process_type)

print("Done!")
