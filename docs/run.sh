#!/usr/bin/env bash

removalCriteria="protocol_filter/removalCriteria.json"
bucket="/lrlhps/data/c4r/common/data/c4r_all_protocols_pdf/"
python3 pipeline.py $bucket $removalCriteria
