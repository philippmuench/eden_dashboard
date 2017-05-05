# library(shinydashboard)
library(shiny)
library(shinyjs)
library(tools)
library(ggplot2)
library(shinydashboard)
library(data.table)
library(DT)
library(pander)

source("functions.R")

# set path variable
if (file.exists("/home/eden/eden.sh")) {
  # we are inside the docker container
  # packrat::on()
  csv.path <<- "/home/eden/data/csv" # folder where processed .tar files are located in csv format (one file for a sample)
  tar.path <<- "/home/eden/data/tar" # folder where .tar files are located (eden output)
  raw.path <<- "/home/eden/data/raw" # folder where unpacked .tar files are located
  annotation.path <<- "/home/eden/tigr_data" # folder werhere TIGR_ROLE_NAMES and TIGRFAMS_ROLE_LINK are located
  dir.create(csv.path)
  dir.create(raw.path)
  dir.create(tar.path)
} else {
  # we are online hosted
  csv.path <<- "csv"
  tar.path <<- "tar"
  raw.path <<- "raw"
  dir.create(csv.path)
  dir.create(raw.path)
  annotation.path <<- "annotation"
}

# check if .csv files are located
# TODO(pmuench): if no_csv == TRUE dont show tabs
dirs <- list.dirs(csv.path,recursive=FALSE)
files.list <- list.files(dirs, ".*\\.csv", recursive=TRUE, full.names=TRUE)
if (length(files.list) > 0){
  no_csv <<- FALSE
} else {
  no_csv <<- TRUE
}

# check if .tar files are located
files.list <- list.files(tar.path, ".*\\.tar", recursive=TRUE, full.names=TRUE)
if (length(files.list) > 0){
  no_tar <<- FALSE
} else {
  no_tar <<- TRUE
}

# check if there are new tar files
tar.list <- basename(list.files(tar.path, ".*\\.tar", recursive=TRUE, full.names=TRUE))
csv.list <- basename(list.dirs(csv.path,recursive=FALSE))
if (any(is.na(match(tar.list, csv.list)))) { # true if the match is not complete (one tar file name dont match to csv file name)
  new_files <<- TRUE
} else {
  new_files <<- FALSE
}

# set path variable
if (file.exists("/home/eden/eden.sh")) {
  # we are inside the docker
  packrat::on()
  # folders to store input faa and ffn files
  faa.path <<- "/home/eden/data/faa"
  ffn.path <<- "/home/eden/data/ffn"
  sample.path <<- "/home/eden/data/samples.txt"
  hmm.path <<- "/home/eden/data/model.hmm"
  tar.path <<- "/home/eden/data/tar"
  fasta.path <<- "/home/eden/data/fasta"
  folder.path <<- "data"
  log.path <<- "/home/eden/shinylog.txt"
  lock.file <<- "/home/eden/lock.txt"
} else {
  # we are online/local hosted
  # folders to store input faa and ffn files
  log.path <<- "log.txt"
  faa.path <<- "faa"
  ffn.path <<- "ffn"
  fasta.path <<- "fasta"
  tar.path <<- "tar"
  folder.path <<- NULL
  sample.path <<- "samples.txt"
  hmm.path <<- "model.hmm"
  log.path <<- "log.txt"
  lock.file <<- "lock.txt"
  
}

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


if (!file.exists(fasta.path)) {
  dir.create(fasta.path)
}
if (!file.exists(faa.path)) {
  dir.create(faa.path)
}
if (!file.exists(ffn.path)) {
  dir.create(ffn.path)
}
Sys.chmod(fasta.path, mode = "0777", use_umask = TRUE)
Sys.chmod(ffn.path, mode = "0777", use_umask = TRUE)
Sys.chmod(faa.path, mode = "0777", use_umask = TRUE)

system2("echo", paste('";;server ready" >> ', log.path, sep = ""))



dbHeader <- dashboardHeader(title = "EDEN",
                            tags$li(tableOutput("log"), class="dropdown",  style = "padding-top:15px; padding-bottom:10px;"),
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"))



dashboardPage(
  dbHeader, 
  #  dashboardHeader(
  #    title = "Eden",
  #    tableOutput("log"),
  #    dropdownMenu(type = "notifications", 
  #                 notificationItem(text = "this is an example error",
  #                                  icon("warning")))
  
  #  ),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(id = "sid",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Start new job", tabName = "newjob", icon = icon("road"),  badgeLabel = textOutput("text3"), badgeColor = "green"),
    menuItem("Show job", tabName = "showjob", icon = icon("pie-chart"),  badgeLabel = textOutput("samplesnum"), badgeColor = "green"),
    conditionalPanel(
      condition = "input.sid == 'showjob'",
      uiOutput("main_ui")
      
    ),
    
        
    
    menuItem("View log", tabName = "log", icon = icon("binoculars", lib = "font-awesome" )),  
    menuItem("Developer", tabName = "developer", icon = icon("cogs",  lib = "font-awesome")),
    menuItem("Bug Reports", icon = icon("bug"),
             href = "https://github.com/philippmuench/eden/issues"),
    menuItem("Support", icon = icon("envelope", lib = "font-awesome"),
             href = "mailto:philipp.muench@helmholtz-hzi.de")
  )
    
  ),
  
  ## Body content
  dashboardBody( useShinyjs(),tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            # infoBoxes with fill=FALSE
            fluidRow(column(width=12,
              htmlOutput("welcome")),
              # A static infoBox
              htmlOutput("statusfinished"),
              htmlOutput("statuscheck"), 
              uiOutput("TableBody"),
              htmlOutput("statusrunning")
              # htmlOutput("timeline")
              
            )),
    
    # Second tab content
    tabItem(tabName = "newjob",
            h2("Start a new job"),
            fluidRow(
              column(width=6, 
                     
                     box( title =  uiOutput("box1status"), #"File upload",
                          uiOutput("upload_ui_head"),
                          tableOutput("fasta_uploaded"),
                          tableOutput("faa_uploaded"),
                          tableOutput("ffn_uploaded"), status = "primary", solidHeader=T, width = NULL
                          #htmlOutput("check_response1")
                     ),
                     box( title =  uiOutput("box2status"),  uiOutput("upload_ui_mid"),status = "primary", solidHeader=T, width = NULL),
                     box( title =  "Reset",  uiOutput("reset_ui_buttons"),status = "primary", solidHeader=T, width = NULL),
                     uiOutput("startEDEN")
                     
              ),
              column(width=6,
                     box( title = uiOutput("box3status"),  uiOutput("upload_ui_hmm"),tableOutput("hmm_uploaded"), width = NULL, status = "primary", solidHeader=T),
                     box( title = uiOutput("box4status"), uiOutput("upload_ui_bottom"),  status = "primary", solidHeader=T, width = NULL)))),
    
    
    
    
    
    # Second tab content
    tabItem(tabName = "showjob",
            h2("inspect a job"),
            
            fluidRow(
              useShinyjs(),
         
              
              box( title = "Figures and tables", status = "primary", solidHeader=  T, width = 12,
                tabsetPanel(
          #        tabPanel(
           #         "Start",
          #          textOutput("reloadmsg"),
                 #  htmlOutput("welcome"),
                    # htmlOutput("tar_check"), 
                    # htmlOutput("newtar"), 
                    # htmlOutput("csv_check"), 
                    # htmlOutput("selected_dataset"),
                    # htmlOutput("selected_samples"),
         #           value = "start"
        #          ),
                  
                  tabPanel(
                    "Overview",
                #    htmlOutput("overview_hint"),
                    div(DT::dataTableOutput("table"), style = "font-size:80%"),
                    htmlOutput("overview_table"),
                    htmlOutput("summary2"),
                    value = "overview"
                  ),
                  
                  tabPanel(
                    "Annotation",
                    #htmlOutput("annotation_hint", inline = FALSE),
                    plotOutput("annotationplotglobal", width = "100%", height =
                                 "auto"),
                    htmlOutput("annotation_figure"),
                    value = "annotation"
                  ),
                  
                  tabPanel(
                    "Alignment Plot",
                    htmlOutput("alignment_hint"),
                    plotOutput("alignmentplot", width = "100%", height =
                                 "auto"),
                    htmlOutput("alignment_figure"),
                    value = "alignment"
                  ),
                  
                  tabPanel(
                    "Categories",
                    plotOutput("annotationplot", width = "100%", height =
                                 "auto"),
                    div(DT::dataTableOutput("table_annotaion"), style = "font-size:80%"),
                    value = "categories"
                  ),
                  
                  tabPanel(
                    "Histogram",
                    h4(""),
                    plotOutput("plot1", width = "100%", height = "auto"),
                    value = "histogram"
                  ),
                  
                  tabPanel(
                    "Boxplot",
                    h4(""),
                   # htmlOutput("boxplot_hint"),
                    plotOutput("plot4", width = "100%", height = "auto"),
                    
                    div(DT::dataTableOutput("table_sample"), style = "font-size:80%"),
                    
                    value = "box"
                  ),
                  id = "tsp"
                )
              ),
              
              box( title = "Export & configure", status = "primary", solidHeader=T, width = 12,
         
                   
                   
                     
                     conditionalPanel(
                       condition = "input.tsp=='map'",
                       tags$button(
                         id = 'close',
                         type = "button",
                         class = "btn action-button",
                         onclick = "setTimeout(function(){window.close();},500);",
                         "Close Application"
                       )
                     ),
                    # conditionalPanel(condition = "input.tsp=='start' || input.tsp=='log'",
                    #                  uiOutput("start_UI")
                    #                  ),
                     
#                     conditionalPanel(
#                       condition = "input.tsp=='start'"#,
                       #htmlOutput("reloadstatus"), 
                   #    actionButton('reloadButton', label = "Reload/Import files")
#                     ),
                     
                     conditionalPanel(
                       condition = "input.tsp=='overview'",
                       
                       ##s
                   
                       actionButton('resetSelection', label = "Reset row selection"),#,   class =    "btn-block btn-primary"),
                       downloadButton("dlTable", "Download filtered table"),
                       actionButton('reloadButton', label = "Reload/Import files"),
                       textOutput("reloadmsg")
                     ),
                     
                     conditionalPanel(
                       condition = "input.tsp=='annotation'",
                       downloadButton("dlAnnotationPlot", "Download barplot")
                     ),
                     
                     conditionalPanel(
                       condition = "input.tsp=='alignment'",
                       checkboxInput('points', 'show points', value =
                                       TRUE),
                       uiOutput('colorpoints'),
                       downloadButton("dlCurSequenceplot", "Download sequenceplot")
                     ),
                     
                     conditionalPanel(
                       condition = "input.tsp=='histogram'",
                       sliderInput(
                         'binSize',
                         'Number of bins',
                         min = 10,
                         max = 500,
                         value = min(10, 500),
                         step = 10,
                         round = 0
                       ),
                       checkboxInput('facet', 'Facet by sample'),
                       downloadButton("dlCurPlot", "Download histogram")
                     ),
                     #   conditionalPanel(condition = "input.tsp=='start'", uiOutput("reload_ui")), 
                     
                     conditionalPanel(
                       condition = "input.tsp=='categories'",
                       checkboxInput('navalues', 'remove NA', value =
                                       TRUE),
                       checkboxInput('showmean', 'plot mean value', value =
                                       TRUE),
                       checkboxInput('bysamplefacet', 'facet by sample'),
                       checkboxInput('bysamplecolor', 'color by sample'),
                       checkboxInput('showmeanselected', 'plot mean of selected families'),
                       selectInput(
                         "sortannotation",
                         label = "Order by",
                         choices = list("ratio" = "ratio", "p-value (not implemented)" = "pvalue"),
                         selected = "ratio"
                       ),
                       downloadButton("dlCurAnnotationplot", "Download boxplot")
                     ),
                     conditionalPanel(
                       condition = "input.tsp=='box'",
                       selectInput(
                         "oderchoice",
                         label = "Order by",
                         choices = list("Dataset name" = "default", "Mean ratio" = "mean"),
                         selected = "default"
                       ),
                       checkboxInput('highlightbox', 'Highlight mean of selected elements'),
                       downloadButton("dlCurBoxPlot", "Download boxplot")
                     )
                   
              )
            ) 
            
           ),
    
    
    
    
    
    
    
    
    # Third tab content
    # Second tab content
    tabItem(tabName = "log",
            h2("Log"),
            fluidRow(column(width=12,dataTableOutput("logtable")))),
    
    # Third tab content
    
    tabItem(tabName = "developer",
            h2("Developer informations"),
            fluidRow(
              column(width=6, 
                     box( title ="URL components",
                          verbatimTextOutput("urlText"), width = NULL),
                     box( title ="Parsed query string",
                          verbatimTextOutput("queryText"), width = NULL),
                     
                     box( title ="statusinformation",
                          verbatimTextOutput("sessioninfo"), width = NULL),
                     box( title ="command on button",
                          verbatimTextOutput("cmdinfo"), width = NULL),
                     box( title ="system state",
                          verbatimTextOutput("stateinfo"), width = NULL)
              ),
              column(width=6, 
                     box( title ="clientData values",
                          verbatimTextOutput("clientdataText"), width = NULL),
                     box( title ="session information",
                          verbatimTextOutput("statusinfo"), width = NULL),
                     box( title ="file information",
                          verbatimTextOutput("fileinfo"), width = NULL),
                     box( title ="viz information",
                          verbatimTextOutput("vizinfo"), width = NULL)
              )
              
            )
    ))
  ))