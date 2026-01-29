#!/bin/bash

set -eo pipefail

# Get local path
localpath=$(pwd)
echo "Local path: $localpath"

metadatapath="$localpath/metadata/doc"
echo "Metadata path: $metadatapath"
mkdir -p $metadatapath

# From <https://aact.ctti-clinicaltrials.org/schema>
# It is really an Excel file.
wget -qO $metadatapath/definitions.xlsx 'https://aact.ctti-clinicaltrials.org/definitions.csv'
