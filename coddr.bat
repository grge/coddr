@echo off
R --no-save --no-restore --quiet -e "setwd('%~dp0'); source('run.R')"
