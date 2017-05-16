# library(shinydashboard)
library(shiny)
library(shinyjs)
library(tools)
library(ggplot2)
library(shinydashboard)
library(data.table)
library(DT)
library(zoo)
library(pander)
library(Biostrings)
source("functions.R")

########## startup settings ##########
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


########## dashboard header ##########
dbHeader <- dashboardHeader( titleWidth=200,
                            
                            # Set height of dashboardHeader
                            tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 60px}"),
                                    tags$style(".main-header .logo {height: 60px;}"),
                                    tags$style(".sidebar-toggle {height: 60px; padding-top: 1px !important;}"),
                                    tags$style(".navbar {min-height:60px !important}")
                            )
                            )

dbHeader$children[[2]]$children <-  tags$a(href='',
                                           tags$img(src='logo.png',height='47',width='115'))


dashboardPage(skin = "black",
  dbHeader, 

  ########## dashboard sidebar ##########
  dashboardSidebar(width = 200,  tags$style(".left-side, .main-sidebar {padding-top: 60px}"), sidebarMenu(id = "sid",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Start new job", tabName = "newjob", icon = icon("road"),  badgeLabel = textOutput("text3"), badgeColor = "green"),
    menuItem("Visualize job", tabName = "showjob", icon = icon("pie-chart"),  badgeLabel = textOutput("samplesnum"), badgeColor = "green"),
    conditionalPanel(
      condition = "input.sid == 'showjob'",
      uiOutput("main_ui")
    ),
    menuItem("View log", tabName = "log", icon = icon("binoculars", lib = "font-awesome" )),  
    menuItem("Get example files", icon = icon("file-archive-o", lib =  "font-awesome"),
             href = 'https://www.dropbox.com/s/pd7tj6ztua2s3ma/sample_files.zip?dl=1'),
    menuItem("Developer", tabName = "developer", icon = icon("cogs",  lib = "font-awesome")),
    menuItem("Bug Reports", icon = icon("bug"),
             href = "https://github.com/philippmuench/eden/issues"),
    menuItem("Support", icon = icon("envelope", lib = "font-awesome"),
             href = "mailto:philipp.muench@helmholtz-hzi.de"),
    htmlOutput("log")
  )
  ),
  
  ########## dashboard body ##########
  dashboardBody( useShinyjs(),
                 
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "edentheme.css")
                 ),
                 
                 tabItems(
    tabItem(tabName = "dashboard",
   #         fluidRow(
  #            infoBoxOutput("lockfileBox")
  #          ),
            fluidRow(column(width=12,
              htmlOutput("welcome")),
              htmlOutput("statusfinished"),
              htmlOutput("statuscheck"), 
              htmlOutput("statusrunning")
            )),
    
    ### body: newjob
    tabItem(tabName = "newjob",
            h2("Start a new job"),
            fluidRow(
              column(width=6, 
                     
                     box( title =  htmlOutput("box1statustest"), #"File upload",
                          uiOutput("upload_ui_head"),
                          tableOutput("fasta_uploaded"),
                          tableOutput("faa_uploaded"),
                          tableOutput("ffn_uploaded"), status = "primary", solidHeader=T, width = NULL
                     ),
                     box( title =  htmlOutput("box2status"),  uiOutput("upload_ui_mid"),status = "primary", solidHeader=T, width = NULL),
                     box( title =  "Reset",  uiOutput("reset_ui_buttons"),status = "primary", solidHeader=T, width = NULL),
                     uiOutput("startEDEN")
                     
              ),
              column(width=6,
                     box( title = htmlOutput("box3status"),  uiOutput("upload_ui_hmm"),tableOutput("hmm_uploaded"), width = NULL, status = "primary", solidHeader=T),
                     box( title = uiOutput("box4status"), uiOutput("upload_ui_bottom"),  status = "primary", solidHeader=T, width = NULL)))),
    
    
    # body: inspect job
    tabItem(tabName = "showjob",
            h2("inspect a job"),
            
            fluidRow(
              useShinyjs(),
      
              box( id ="figurebox", title = "Figures and tables", status = "primary", solidHeader=  T, width = 12,
                tabsetPanel(id = "tsp",
                  tabPanel(
                    "Overview",
                    div(DT::dataTableOutput("table"), style = "font-size:80%"),
                    htmlOutput("overview_table"),
                    htmlOutput("summary2"),
                    value = "overview"
                  ),
                  
                  tabPanel(
                    "Annotation",
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
                    plotOutput("plot4", width = "100%", height = "auto"),
                    div(DT::dataTableOutput("table_sample"), style = "font-size:80%"),
                    
                    value = "box"
                  )
                 
                )
              ),
          
              box( title = "Plot settings", status = "primary", solidHeader=T, width = 12,
                     
                   # barplot download
                     conditionalPanel(
                       condition = "input.tsp=='overview'",
                       downloadButton("dlTable", "Download filtered table")
                     ),
                   
                  # barplot download
                   conditionalPanel(
                     condition = "input.tsp=='overview'",
                     renderText("dlRaw")
                   ),
                     
                  # barplot download
                   conditionalPanel(
                     condition = "input.tsp=='annotation'",
                     downloadButton("dlAnnotationPlot", "Download barplot")
                   ),
                   
                   # sequenceplot options
                     conditionalPanel(
                       condition = "input.tsp=='alignment'",
                       checkboxInput('points', 'show points', value =
                                       TRUE),
                       uiOutput('colorpoints'),
                       downloadButton("dlCurSequenceplot", "Download sequenceplot")
                     ),
                     
                   # histogram options
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
                    
                   # categorie options
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
                   
                   
              ), 
              
              box( title = "Export & configure", status = "primary", solidHeader=T, width = 12,
                   condition = "input.tsp=='overview'",
                   actionButton('resetSelection', label = "Reset row selection"),
                 
                   actionButton('reloadButton', label = "Reload/Import files")
                   #textOutput("reloadmsg")
              )
            ) 
           ),
  
    # body: logtable
    tabItem(tabName = "log",
            h2("Log"),
            fluidRow(column(width=12,htmlOutput("logtable")))),
    
    # body: developer 
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
    )
    )
  )
  )