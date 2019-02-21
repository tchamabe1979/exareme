#!/bin/bash

if [ -z ${NODE_NAME} ]; then echo "NODE_NAME is unset";exit;  fi
if [ -z ${CONSULURL} ]; then echo "CONSULURL is unset"; exit; fi

MADIS=$(pwd)"/lib/madis/src/mterm.py"
DATASETS_PATH="datasets"
MY_PATH=$(pwd)
PARENT_PATH="$(dirname "$MY_PATH")"
DATASETS=$(echo "select  distinct __val from (file header:t file:"$PARENT_PATH"/algorithms/input_tbl.csv) where __colname = 'dataset';" | $MADIS | \
		 sed '1d ; $d' | jq .[]  | sed 's/^\"//g ; s/\"$//g' | printf %s "$(cat)"| jq -R -c -s 'split("\n")')

curl -X PUT -d @- $CONSULURL/v1/kv/$DATASETS_PATH/$NODE_NAME <<< $DATASETS
