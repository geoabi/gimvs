# gimvs.R
# Graphical Interface for MaxEnt Variable Selection
# July 2017
# Geovany A. Ramirez
# Jornada Basin LTER 
# New Mexico State University
# Las Cruces, NM

library(shiny)
library(miniUI)
library(raster)
library(MaxentVariableSelection)

#************* Default directories and parameters ******************************

# SWD file convertion
d.name <- "name" 
# directory with ASCII files
d.ascDir <- "C:/gimvs/BioORACLEVariables"
# original occurrence and background files
d.occurrenceFile <- "C:/gimvs/extdata/Occurrencedata.csv" 
d.backgroundFile <- "C:/gimvs/extdata/Backgrounddata.csv"
# destination directory for SWD files
d.swdDir <- "C:/gimvs/swd"
# remove rows with NA
d.removeNA <- TRUE

# MaxEnt .jar file location
d.maxent <- "C:/maxent/maxent.jar"
# directory with ASCII files, it could be different than d.ascDir 
d.gridfolder <- "C:/gimvs/BioORACLEVariables"
# occurrence and background files in SWD format
d.occurrencelocations <- "C:/gimvs/swd/SWD_Occurrencedata.csv"
d.backgroundlocations <- "C:/gimvs/swd/SWD_Backgrounddata.csv"
d.outdir <- "C:/gimvs/output"
d.contributionthreshold <- 5
d.contributionthreshold.min <- 0
d.contributionthreshold.max <- 20
d.correlationthreshold <- 0.7
d.correlationthreshold.min <- 0
d.correlationthreshold.max <- 1
d.betamultiplier <- c(1,6,0.5)
d.betamultiplier.min <- 0
d.betamultiplier.max <- 10
d.additionalargs <- ""
d.saveLog <- TRUE

# log file, by default write log to terminal
d.logFile <- stderr()       

#************* helper functions ************************************************

# print messages to terminal
prt <- function(...) {
  cat(paste(...), "\n")
}

# write a log
wrLog <- function(...) {
	cat(paste(...), "\n", file = d.logFile , append = TRUE )
}

# default width for UI
colWidth <- 12
colOffset <- 0

# creates a button and text output
buttonAndText <- function(btId, btLabel, txId, mode = "f" ) {
  if (mode == "f") {
    iconName <- "file"
  } else {
    iconName <- "folder-open"
  }
  
  div( class="input-group",
    span(class="input-group-btn",
      actionButton(btId, btLabel, class = "btn-success", icon = icon(iconName))
    ),
    div(style = "padding-left:0.5em;margin-top:0.5em",
      verbatimTextOutput(txId)
    )
  )
}

# creates a minipanel based on a wellPanel
miniPanel <- function(..., title = NULL) {
  fluidRow(column(colWidth, offset = colOffset,
    wellPanel( 
      if(!is.null(title)) 
        h3(title, align = "center", style="margin-top:0em"),
      ...)
  ))
}

# custom CSS to modify progress bar
customCSS <- "
.shiny-progress .progress {
  height: 50px !important;
}
.shiny-progress .progress-text {
  position: absolute;
  right: 15%!important;
  top: 55px!important;
  width: 70%!important;
  height: 50px !important;
  background-color: #FFB900;
  border-radius: 60px!important; 
  text-align: center;
}
.shiny-progress .progress-text .progress-message {
  padding: 0px 10px!important;
  font-size: 200%!important;
}
.shiny-progress .progress-text .progress-detail {
  font-size: 180%!important;
}
"
# verify that the file or directory exists and fix the slash
checkAndFix <- function(f) {
  if (!dir.exists(f) && !file.exists(f)) {
    r <- ""
  } else {
    r <- gsub("\\\\", "/", f)
  }
  r
}

#************* main gadget function ********************************************
gimvs <- function(inputValue1, inputValue2) {

  maxentTx <- strong("MaxentVariableSelection")
  rstudioTx <- strong("RStudio")

  ui <- miniPage(
    tags$head(tags$style(HTML(customCSS))),
    gadgetTitleBar("Graphical Interface for MaxEnt Variable Selection"),
    
    miniTabstripPanel(
     
      #********* SWD file conversion *******************************************
      miniTabPanel(
        strong("SWD File covertion"),
        icon = icon("gears"),
        miniContentPanel(
          miniPanel( title = "Directories and files",
            buttonAndText("ascDirBt", "ASCII directory", "ascDirTx", "d"),
            buttonAndText("occFileBt", "Occurrence file", "occFileTx", "f"),
            buttonAndText("bacFileBt", "Background file", "bacFileTx", "f"),
            buttonAndText("swdDirBt", "Output directory", "swdDirTx", "d")
          ),
          miniPanel( title = "Parameters",
            fluidRow(
              column(6,
                textInput("swdNameTx", "Species Name:", width = "50%")
              ),
              column(6,
                br(),
                checkboxInput("noNACB", "Remove rows with NA", TRUE)
              )
            )
          ),
          miniPanel( 
            fluidRow(
              column(8,
                uiOutput("swdResultUI")
              ),
              column(4,
                actionButton("runSWDBt", h4("Create SWD files"), 
                  icon = icon("play"),
                  class = "pull-right btn-primary btn-lg",
                  onclick = paste0("document.getElementById('swdResult')", 
                                   ".style.display = 'none';"))
              )
            )
          )
        )
      ),
      #********* Variable selection ********************************************
      miniTabPanel(
        strong("Variable selection"),
        icon = icon("sort-amount-desc"),
        miniContentPanel(
          miniPanel( title = "Directories and Files",
            buttonAndText("maxFileBt", "Maxent file", "maxFileTx", "f"),
            buttonAndText("maxAscDirBt", "ASCII directory", "maxAscDirTx", "d"),
            buttonAndText("maxOccFileBt", "Occurrence file (SWD)", 
                          "maxOccFileTx", "f"),
            buttonAndText("maxBacFileBt", "Background file (SWD)", 
                          "maxBacFileTx", "f"),
            buttonAndText("maxOutDirBt", "Output directory", "maxOutDirTx", "d")
          ),
          miniPanel( title = "Parameters",
            fluidRow(
              column(8,
                fluidRow(
                  column(6, wellPanel(
                    sliderInput("maxContThSl", "Contribution Threshold", 
                                min = d.contributionthreshold.min, 
                                max = d.contributionthreshold.max, 
                                value = d.contributionthreshold) )
                  ),
                  column(6, wellPanel(
                    sliderInput("maxCorrThSl", "Correlation Threshold", 
                                min = d.correlationthreshold.min, 
                                max = d.correlationthreshold.max, 
                                value = d.correlationthreshold) )
                  )
                ),
                textInput("maxAddArgsTx", "Additional Args:", d.additionalargs)
              ),
              column(4, wellPanel(
                sliderInput("maxBetaMulSl", "Beta multiplier", 
                            min = d.betamultiplier.min, 
                            max = d.betamultiplier.max, 
                            value = d.betamultiplier[1:2]),
                numericInput("maxBetaStepNu", "Step", min = 0.1, max = 1,
                             value = d.betamultiplier[3], step = 0.1, 
                             width = "35%")
                )
              )
            )
          ),
          miniPanel(
            fluidRow(
              column(3,
                br(),
                checkboxInput("maxSaveLogCb", "Save log (gimvsLog.txt)", 
                              d.saveLog)
              ),
              column(6,
                uiOutput("maxResultUI")
              ),
              column(3,
                actionButton("maxRunBt", h4("Run MaxEnt"), icon = icon("play"),
                  class = "pull-right btn-primary btn-lg",
                  onclick = paste0("document.getElementById('maxResult')", 
                                   ".style.display = 'none';"))
              )
            )
          )
        )
      ),
      #********* Help **********************************************************
      miniTabPanel(
        strong("Help"),
        icon = icon("question"),
        miniContentPanel(
          fluidRow( 
            column(8, offset = 2,
              h3("GIMVS - Graphical Interface for MaxEnt Variable Selection"),
              br(),
              p("The ", maxentTx, "package is 
                 useful to identify the most important set of
                 uncorrelated environmental variables on a MaxEnt Model and 
                 also helps tune ", strong("MaxEnt"), " settings. The aim 
                 of this ", rstudioTx, " gadget is to simplify 
                 the tasks to select input directories and files, and ",
                 strong("MaxEnt"), " parameters. It also helps with the 
                 creation of input files in SWD format."),
              br(),
              h4("Default Parameters"),
              p("At the beginning of the script are defined the default 
                 directories, files, and parameters. It is recommended that 
                 you modify them according to your OS and experiments. Please 
                 note that if you are using a computer with Windows OS, you 
                 should put the paths with the forward  slash", 
                 code('"/"'), " or the double backwards slash", 
                 code('"\\\\"'), "."),
              br(),
              h4("Installation"),
              p("Just donwload the file ", code("gimvs.R"), " from: ", 
                span(
                  tags$a(href = "https://github.com/geoabi/gimvs", 
                         target = "_blank", "https://github.com/geoabi/gimvs")
                )
              ),
              p("Also install the following packages:"),
              pre(paste0(collapse = "\n", 
                  'install.packages("shiny")\n', 
                  'install.packages("miniUI")\n',
                  'install.packages("raster")\n',
                  'install.packages("MaxentVariableSelection")\n')),
              p("You can also install ", maxentTx, " using:"),
              pre(paste0('devtools::install_github(',
                          '"alj1983/MaxentVariableSelection")')),
              p("You can get the last version of ", strong("MaxEnt"), " and
                 documentation ", 
                 tags$a(href = paste0("http://biodiversityinformatics.amnh.org",
                                      "/open_source/maxent/"),
                        target = "_blank", "here."), 
                 " This gadget was tested using the version ", strong("3.3.3k"), 
                 "that you can get", 
                 tags$a(href = paste0("https://github.com/mrmaxent/Maxent/",
                                      "tree/master/ArchivedReleases/3.3.3k"),
                        target = "_blank", "here.")),
              br(),
              h4("How to use"),
              p("Open ", rstudioTx, "and set your working directory to the
                 location where you downloaded the ", code("gimvs.R"), "file.
                 Later use the command ", code("source"), "to execute the 
                 gadget. For example, if you donwloaded the file to ", 
                 code("c:/myScripts"), " use:"),
              pre('source("C:/myScripts/gimvs.R")'),
              p("Alternatively you can just open the file on the file editor
                of ", rstudioTx, "  and click on ", code("Source")),
              p("This gadget is an implementation of the instructions provided
                with the documentation of ", maxentTx, ", you could test this
                gadget by following the tutorial in ", 
                strong("'MaxentVariableSelection.pdf'"), " that is located in 
                the ", code("doc"), "directory of the package installation. 
                You can also get it ",
                   tags$a(href = paste0("https://github.com/alj1983/",
                      "MaxentVariableSelection/blob/master/inst/doc/",
                      "MaxentVariableSelection.pdf"),
                      target = "_blank", "here."), 
                " Please note that running the variable selection could
                take a long period of time depending to the input data and
                your computer configuration.")
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
  
    # try an expression and if it fails catch the error
    tryThis <- function(..., showError = TRUE, wait = 0) {
    # ... expression
    # showError: write error to the terminal
    # wait: time in second to wait if thw show has to be showed
    #
    # the output of the exprsion is stored in r$result
    # if there is an error, the call and the message are returned 

      rVar <- list(ok = TRUE, result = NULL, call = NULL, message = NULL)

      result <- tryCatch(
        ...
        ,
        error = function(cond) {
          rVar$ok <<- FALSE
          return(cond)
        },
        finally = { 
        }
      )
      
      if (rVar$ok) {
        rVar$result <- result
      } else {
        rVar$call <- result$call
        rVar$message <- result$message
        if (showError) {
          prt("Error executing expression:\n", rVar$message)
          if (wait > 0) 
            Sys.sleep(wait)
        }
      }
      rVar
    } 
  
    processSWDFile <- function(inFileName, outFileName, experimentName, 
                               swdDir, Grids, removeNA) {
      # Load the occurrence records
      occurrencelocations <- read.csv(inFileName, header=TRUE)

      LonLatData <- occurrencelocations[ , c(2, 3)]
      VariablesAtOccurrencelocations <- raster::extract(Grids, LonLatData)

      Outfile <- as.data.frame(cbind(experimentName, LonLatData, 
                               VariablesAtOccurrencelocations))
                                
      # Combining the extracted values with the longitude and latitude values
      colnames(Outfile) <- c("species", "longitude", "latitude",
                             colnames(VariablesAtOccurrencelocations))
      if (removeNA) {                       
        prt("Removing rows with NA")
        okRows <- complete.cases(Outfile)
        prt("Number of rows with NA:", sum(!okRows))
        Outfile <- Outfile[okRows, ]
      }               
                             
      # writing this table to a csv file:
      if (!dir.exists(swdDir))
        stop(paste("Cannot open output directory'", swdDir, "'"))
        
      write.csv(Outfile, file = outFileName, eol = "\n", na = "NA", 
                row.names = FALSE)
    }
  
    convertSWD <- function(experimentName, ascDir, occurrenceFile, 
                           backgroundFile, swdDir, removeNA) {
      n <- 4
      incProgress(1/n, detail = "Load the environmental variables into R..." )                   
      # output files, they will have the same names as csv but wiht the prefix 
      # "SWD_"
      SWDoccurrenceFile <- file.path(swdDir, 
                                      paste0("SWD_", basename(occurrenceFile)))
      SWDbackgroundFile <- file.path(swdDir, 
                                      paste0("SWD_", basename(backgroundFile)))

      #************* common for occurrence and background **********************
      files <- list.files(ascDir, pattern='.asc', full.names=TRUE)
      
      if (length(files) == 0)
        stop(paste("No .asc files where found on '", ascDir, "'"))
      
      Grids <- raster::stack(files)

      #************* for occurrence file ***************************************
      prt(SWDoccurrenceFile)
      incProgress(1/n, detail = "Occurrence file" ) 
      processSWDFile(occurrenceFile, SWDoccurrenceFile, experimentName, 
                               swdDir, Grids, removeNA)
             
      #*************** for background file *************************************
      prt(SWDbackgroundFile)
      incProgress(1/n, detail = "Background file" ) 
      processSWDFile(backgroundFile, SWDbackgroundFile, experimentName, 
                               swdDir, Grids, removeNA)
    }

    #********* reavtive variables **********************************************
    vars <- reactiveValues(
      # SWD convertion
      name = d.name, 
      ascDir = checkAndFix(d.ascDir), 
      occurrenceFile = checkAndFix(d.occurrenceFile), 
      backgroundFile = checkAndFix(d.backgroundFile), 
      swdDir = checkAndFix(d.swdDir), 
      removeNA = d.removeNA,
      swdErrMsg = "",
      swdOk = FALSE,
      # maxent
      maxent = checkAndFix(d.maxent),
      gridfolder = checkAndFix(d.gridfolder),
      # in SWD format
      occurrencelocations = checkAndFix(d.occurrencelocations),
      backgroundlocations = checkAndFix(d.backgroundlocations),
      outdir = checkAndFix(d.outdir),
      maxErrMsg = "",
      maxOk = FALSE
    )
    
    #********* SWD file conversion *********************************************
    
    observe({
      updateTextInput(session, "swdNameTx", value = vars$name)
    })
    
    observeEvent(input$ascDirBt, {
      v <- choose.dir(vars$ascDir, "Select the folder with the .asc files")
      if (!is.na(v))
        vars$ascDir <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$occFileBt, {
      v <- choose.files(vars$occurrenceFile, "Select the .csv Occurrence file",
                        multi = FALSE)
      if (length(v) > 0)
        vars$occurrenceFile <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$bacFileBt, {
      v <- choose.files(vars$backgroundFile, "Select the .csv Background file",
                        multi = FALSE)
      if (length(v) > 0)
        vars$backgroundFile <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$swdDirBt, {
      v <- choose.dir(vars$swdDir, "Select the folder to save the .swd files")
      if (!is.na(v))
        vars$swdDir <-  gsub("\\\\", "/", v)
    })
    
    output$ascDirTx <- renderPrint({
     vars$ascDir
    })
    
    output$occFileTx <- renderPrint({
      vars$occurrenceFile
    })
    
    output$bacFileTx <- renderPrint({
      vars$backgroundFile
    })
    
    output$swdDirTx <- renderPrint({
     vars$swdDir
    })
    
    observeEvent(input$noNACB, {
     vars$removeNA <- input$noNACB
    })
    
    observeEvent(input$runSWDBt, {
      vars$swdOk <- TRUE
      vars$swdOk <- FALSE
      
      if (!nchar(vars$ascDir) && !nchar(vars$occurrenceFile) && 
          !nchar(vars$backgroundFile) && !nchar(vars$swdDir)) {
         vars$swdErrMsg <- "Error: Missing directories or files."
      } else {
        vars$swdErrMsg <- ""
      }
      prt("vars$swdErrMsg", vars$swdErrMsg)

      withProgress(message = "Processing:", value = 0, style = "old", {
        
        r <- tryThis({
          if (!dir.exists(vars$ascDir))
            stop(paste('Cannot open ASCII directory: "',  vars$ascDir, '"'))
          if (!file.exists(vars$occurrenceFile))
            stop(paste('Cannot open Occurrence file: "', 
                 vars$occurrenceFile, '"')) 
          if (!file.exists(vars$backgroundFile))
            stop(paste('Cannot open Background file: "', 
                 vars$backgroundFile, '"'))
          
          convertSWD(vars$name, vars$ascDir, vars$occurrenceFile, 
                vars$backgroundFile, vars$swdDir, vars$removeNA)
        })
        
        if (!r$ok) {
          incProgress(1, detail = "Done!" )
          vars$swdErrMsg <- paste("Error:", r$message)
          prt("vars$swdErrMsg", vars$swdErrMsg)
        } else {
          vars$swdOk <- TRUE
        }
      })
    })
    
    output$swdErrorUI <- renderUI({
      if (nchar(vars$swdErrMsg))
        p(class="text-danger", align = "center", vars$swdErrMsg)
    })
    
    output$swdResultUI <- renderUI({
      if (vars$swdOk) {
        div(id = "swdResult",
          h3("Done!", align = "center", style="margin-top:0em"),
          div(
            span(strong("Results at:")), 
            tags$pre(isolate(vars$swdDir), style = "display:inline;")
          )
        )
      } else if (nchar(vars$swdErrMsg)) {
        div(id = "swdResult",
          p(class="text-danger", align = "center", vars$swdErrMsg)
        )
      }
     
    })
    
    #********* Maxent Variable selection ***************************************
    observeEvent(input$maxFileBt, {
      v <- choose.files(vars$maxent, "Select the maxent.jar file",
                        multi = FALSE)
      if (length(v) > 0)
        vars$maxent <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$maxAscDirBt, {
      v <- choose.dir(vars$gridfolder, "Select the folder with the .asc files")
      if (!is.na(v))
        vars$gridfolder <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$maxOccFileBt, {
      v <- choose.files(vars$occurrencelocations, 
                        "Select the occurrence file in SWD format",
                        multi = FALSE)
      if (length(v) > 0)
        vars$occurrencelocations <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$maxBacFileBt, {
      v <- choose.files(vars$backgroundlocations, 
                        "Select the occurrence file in SWD format",
                        multi = FALSE)
      if (length(v) > 0)
        vars$backgroundlocations <-  gsub("\\\\", "/", v)
    })
    
    observeEvent(input$maxOutDirBt, {
      v <- choose.dir(vars$outdir, "Select the output folder")
      if (!is.na(v))
        vars$outdir <-  gsub("\\\\", "/", v)
    })
    
    output$maxFileTx <- renderPrint({
     vars$maxent
    })
    
    output$maxAscDirTx <- renderPrint({
     vars$gridfolder
    })
    
    output$maxOccFileTx <- renderPrint({
     vars$occurrencelocations
    })
    
    output$maxBacFileTx <- renderPrint({
     vars$backgroundlocations
    })
    
    output$maxOutDirTx <- renderPrint({
     vars$outdir
    })
    
    # parameters
    
    observeEvent(input$maxContThSl, {
      d.contributionthreshold <<- input$maxContThSl
    })
    
    observeEvent(input$maxCorrThSl, {
      d.correlationthreshold <<- input$maxCorrThSl
    })
    
    observeEvent(input$maxBetaMulSl, {
      d.betamultiplier[1:2] <<- input$maxBetaMulSl
    })
    
    observeEvent(input$maxBetaStepNu, {
      d.betamultiplier[3] <<- input$maxBetaStepNu
    })
    
    observeEvent(input$maxAddArgsTx, {
      d.additionalargs <<- input$maxAddArgsTx
    })
    
    observeEvent(input$maxSaveLogCb, {
      d.saveLog <<- input$maxSaveLogCb
    })
    
    # run MaxEnt Variable Selection
    observeEvent(input$maxRunBt, {
      vars$maxOk <- TRUE
      vars$maxOk <- FALSE

      prt("Running VariableSelection...")
      start.time <- format(Sys.time(), 
                    "============= Starts:  %b %d, %Y. %H:%M:%S ==============")
      prt(start.time)
      # creates the betamultiplier sequence
      betamultiplier <- seq(d.betamultiplier[1], d.betamultiplier[2], 
                            d.betamultiplier[3])
      
      withProgress(message = "Running Variable Selection...", value = 0, 
                   style = "old", {
        
        incProgress(0.5, detail = "" ) 
        
        r <- tryThis({
          if (!file.exists(vars$maxent))
            stop(paste('Cannot open MaxEnt .jar file: "', 
                 vars$maxent, '"')) 
          if (!dir.exists(vars$gridfolder))
            stop(paste('Cannot open ASCII directory: "', vars$gridfolder, '"'))
          if (!file.exists(vars$occurrencelocations))
            stop(paste('Cannot open Occurrence file: "', 
                 vars$occurrencelocations, '"')) 
          if (!file.exists(vars$backgroundlocations))
            stop(paste('Cannot open Background file: "', 
                 vars$backgroundlocations, '"'))
          if (!dir.exists(vars$outdir))
            stop(paste('Output directory does not exist: "', vars$outdir, '"'))       
          VariableSelection(
            vars$maxent,
            vars$outdir,
            vars$gridfolder,
            vars$occurrencelocations,
            vars$backgroundlocations,
            d.additionalargs,
            d.contributionthreshold,
            d.correlationthreshold,
            betamultiplier
          )
        })
        
        if (!r$ok) {
          incProgress(1, detail = "Done!" )
          vars$maxErrMsg <- paste("Error:", r$message)
        } else {
          vars$maxOk <- TRUE
        }
      })
      
      end.time <- format(Sys.time(), 
              "============= Ends:  %b %d, %Y. %H:%M:%S ==============")
      prt(end.time)
      
      if (d.saveLog && dir.exists(vars$outdir)) {
        # define log file to outdir
        d.logFile <<- file.path(vars$outdir, 'gimvsLog.txt')

        # creates outdir
        dir.create(vars$outdir)

        # save parameters to log 
        wrLog(start.time)
        wrLog("outdir:", vars$outdir)
        wrLog("gridfolder:", vars$gridfolder)
        wrLog("occurrencelocations:", vars$occurrencelocations)
        wrLog("backgroundlocations:", vars$backgroundlocations)
        wrLog("additionalargs:", d.additionalargs)
        wrLog("contributionthreshold:", d.contributionthreshold)
        wrLog("correlationthreshold:", d.correlationthreshold)
        wrLog("betamultiplier:", paste(d.betamultiplier, collapse=", "))
        wrLog("")
        if (vars$maxOk) {
          wrLog("MaxEnt Variable Selection was executed correctly.")
        } else {
          wrLog(vars$maxErrMsg)
        }
        wrLog(end.time)
        wrLog("")
      }
      prt("d.logFile", d.logFile)
    })
    
    output$maxResultUI <- renderUI({
      if (vars$maxOk) {
        div(id = "maxResult",
          h3("Done!", align = "center", style="margin-top:0em"),
          div(
            span(strong("Results at:")), 
            tags$pre(isolate(vars$outdir), style = "display:inline;")
          )
        )
      } else if (nchar(vars$maxErrMsg)) {
        div(id = "maxResult",
          p(class="text-danger", align = "center", vars$maxErrMsg)
        )
      }
    })
    
    #***************************************************************************
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- TRUE
      stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}

gimvs()
