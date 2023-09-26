#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Template
Usage:
script_template <dat> <out> [--db=<db>] [-t]
script_template (-h | --help)
Control files:
  ctfs/individual.csv
Parameters:
  dat: path to input csv file. 
  out: path to output directory.
Options:
-h --help     Show this screen.
-v --version     Show version.
-d --db=<db> Path to movement database. Defaults to <wd>/data/move.db
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/Documents/Yale/projects/easy-breezy'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/dat.csv')
  .outPF <- file.path(.wd,'figs/myfig.png')
  .dbPF <- file.path(.wd,'data/move.db')
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
  .datPF <- makePath(ag$dat)
  .outPF <- makePath(ag$out)
  if(length(ag$db)==0) {
    .dbPF <- file.path(.wd,'data/move.db')
  } else {
    .dbPF <- makePath(ag$db)
  }
}

#---- Initialize Environment ----#
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
  }))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

theme_set(theme_eda)

#---- Local parameters ----#

#---- Load control files ----#

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

std <- tbl(db,'study')

#---- Load data ----#
message('Loading data...')

#====

#---- Perform analysis ----#

#---- Save output ---#
message(glue('Saving to {.outPF}'))

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

#---- Finalize script ----#

message(glue('Script complete in {diffmin(t0)} minutes'))