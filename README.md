GIMVS - Graphical Interface for MaxEnt Variable Selection
=========================================================

The [MaxentVariableSelection](https://github.com/alj1983/MaxentVariableSelection) package is useful to identify the most important set of uncorrelated environmental variables on a **MaxEnt** Model and also helps tune **MaxEnt** settings. The aim of this **RStudio** gadget is to simplify the tasks to select input directories and files, and MaxEnt parameters. It also helps with the creation of input files in SWD format.

## Default Parameters
At the beginning of the script are defined the default directories, files, and parameters. It is recommended that you modify them according to your OS and experiments. Please note that if you are using a computer with Windows OS, you should put the paths with the forward slash `"/"` or the double backwards slash `"\\"`.

## Installation

Just donwload the file `gimvs.R` from: [https://github.com/geoabi/gimvs](https://github.com/geoabi/gimvs)

Also install the following packages:

```R
install.packages("shiny")
install.packages("miniUI")
install.packages("raster")
install.packages("MaxentVariableSelection")
```
You can also install **MaxentVariableSelection** using:
```R
devtools::install_github("alj1983/MaxentVariableSelection")
```
You can get the last version of **MaxEnt** and documentation [here](http://biodiversityinformatics.amnh.org/open_source/maxent/). This gadget was tested using the version **3.3.3k** that you can get [here](https://github.com/mrmaxent/Maxent/tree/master/ArchivedReleases/3.3.3k).

## How to use
Open **RStudio** and set your working directory to the location where you downloaded the `gimvs.R` file. Later use the command `source` to execute the gadget. For example, if you donwloaded the file to `C:/myScripts` use:
```R
source("C:/myScripts/gimvs.R")
```
Alternatively you can just open the file on the file editor of **RStudio** and click on `Source`.

This gadget is an implementation of the instructions provided with the documentation of **MaxentVariableSelection**, you could test this gadget by following the tutorial in `MaxentVariableSelection.pdf` that is located in the doc directory of the package installation. You can also get it [here](https://github.com/alj1983/MaxentVariableSelection/blob/master/inst/doc/MaxentVariableSelection.pdf). Please note that running the variable selection could take a long period of time depending to the input data and your computer configuration.

## Problems and Contribution
If you have problems running the script or you have suggestions, please open an issue [here](https://github.com/geoabi/gimvs/issues) at  GitHub. Also if you are interested in collaborate, please make a pull request.

## Screenshots
<img src="https://github.com/geoabi/gimvs/blob/master/swd_convertion.png" width="60%">
<img src="https://github.com/geoabi/gimvs/blob/master/run_MaxentVariableSelection.png" width="60%">

