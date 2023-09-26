#!/bin/bash
chmod +x ~/Documents/Yale/projects/wi-coverage/workflow/workflow.sh

#-- parameters
wd=~/Documents/Yale/projects/wi-coverage
src=~/Documents/Yale/projects/wi-coverage/src

cd $wd

chmod 744 $src/poc/plot-data-summary.r #Use to make executable

$src/poc/plot-data-summary.r data/ analysis/
