#!/bin/bash

[[ ! -f "submission/arxiv.sty" ]] && wget --output-document submission/arxiv.sty -nc https://raw.githubusercontent.com/kourgeorge/arxiv-style/master/arxiv.sty || echo -e "Nothing to be done"
