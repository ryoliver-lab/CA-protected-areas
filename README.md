# easy breezy workflow
### scripts and file structure for projects
modified based on the breezy philosophy: github.com/benscarlson/breezy

## file structure
**project** folder: 
* Rproj file
* README
* **data** - input folder
* **analysis** - output folder
* **doc** - documents
* **src** - source code
  * funs - functions
  * poc - proof of concept
  * workflow - final workflow

## general workflow
* Analysis starts in the poc folder. 
  * As scripts become final they are upgraded to the workflow folder.
  * breezy-script.R contains template of code
  * scripts should not perform analysis and generate figures
* All scripts are called from workflow.sh
* Analysis outputs are stored in the analysis folder.
* Any associated documents (drafts, notes) are stored in the docs folder


# README TEMPLATE
## goals:
description...
* goal 1

## to do:
* task 1

## activity log:

|date|activity|
|:-|:------------|
|2021-01-01|progress update|

## notes
* note 1