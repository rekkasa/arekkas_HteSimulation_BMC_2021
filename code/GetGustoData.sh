#!/bin/bash

[[ ! -f "data/raw/gusto.rda" ]] && wget --output-document data/raw/gusto.rda -nc http://hbiostat.org/data/gusto.rda || echo -e "Nothing to be done"
