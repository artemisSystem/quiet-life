#!/bin/sh

set -e

instance="$(cat instance_location.txt)"
resources="$instance/config/openloader/resources"
data="$instance/config/openloader/data"

rm -rf "$resources/core_assets"
rm -rf "$resources/core_assets_generated"
rm -rf "$data/core_data_generated"
rm -rf "$instance/content"

cp -r "core_assets" "$resources/core_assets"
cp -r "generated/resources/core_assets_generated" "$resources/core_assets_generated"
cp -r "generated/data/core_data_generated" "$data/core_data_generated"
cp -r "generated/content" "$instance/content"

echo "Succesfully deployed to $instance!"
