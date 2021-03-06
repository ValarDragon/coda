#!/bin/bash
set -euxo pipefail

# script to use in ci to pull down current proving and verificaiton keys
# uses exising circleci GC creds
# run as sudo

# Install jq
sudo apt install -y jq

# cloud sdk debian install
export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)" 

if [ ! -f /etc/apt/sources.list.d/google-cloud-sdk.list ]; then
    echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
fi
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - 
apt-get update -y && apt-get install google-cloud-sdk -y

# credentials (inside circleci)
echo $JSON_GCLOUD_CREDENTIALS > google_creds.json
/usr/bin/gcloud auth activate-service-account --key-file=google_creds.json
/usr/bin/gcloud config set project $(cat google_creds.json | jq -r .project_id)

PINNED_KEY_COMMIT=c92e84c97abc239feb54b758de6cde98b9a6ef58

# Download keys
/usr/bin/gsutil cp gs://proving-keys-stable/keys-$PINNED_KEY_COMMIT.tar.bz2 /tmp/.

mkdir -p /var/lib/coda
cd /var/lib/coda
tar --strip-components=2 -xvf /tmp/keys-$PINNED_KEY_COMMIT.tar.bz2
