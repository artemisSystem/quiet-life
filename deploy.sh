#!/bin/sh

set -e

instance="$(cat instance_location.txt)"
resources="$instance/resources/resourcepacks"
data="$instance/resources/datapacks"

rm -rf "$resources/core_assets"
rm -rf "$resources/core_assets_generated"
rm -rf "$data/core_data_generated"
rm -rf "$instance/content"

cp -r "core_assets" "$resources/core_assets"
cp -r "generated/resourcepacks/core_assets_generated" "$resources/core_assets_generated"
cp -r "generated/datapacks/core_data_generated" "$data/core_data_generated"
cp -r "generated/content" "$instance/content"

echo "Succesfully deployed to $instance!"
