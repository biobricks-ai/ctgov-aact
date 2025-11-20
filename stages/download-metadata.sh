#!/bin/bash

set -eo pipefail

# Get local path
localpath=$(pwd)
echo "Local path: $localpath"

metadatapath="$localpath/metadata/doc"
echo "Metadata path: $metadatapath"
mkdir -p $metadatapath

# From <https://aact.ctti-clinicaltrials.org/schema>
wget -qO - 'https://aact.ctti-clinicaltrials.org/definitions.csv' \
	| perl -MHTML::Entities -lpE '$_ = decode_entities($_)' \
	| sponge $metadatapath/definitions.csv
