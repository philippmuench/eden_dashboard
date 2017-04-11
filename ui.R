library(shinydashboard)
library(shiny)
library(shinyjs)
library(tools)
library(ggplot2)
library(data.table)
library(DT)

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
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Start new job", tabName = "newjob", icon = icon("th")),
  
    menuItem("Developer", tabName = "developer", icon = icon("tasks"))
    
  )),
  
  ## Body content
  dashboardBody( useShinyjs(),tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            # infoBoxes with fill=FALSE
            fluidRow(
              # A static infoBox
              infoBox(
                "Fasta", uiOutput("dashboard1"), icon = icon("users"), color = "purple"
              ),
              uiOutput("TableBody")

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
                   verbatimTextOutput("sessioninfo"), width = NULL)
              ),
              column(width=6, 
              box( title ="clientData values",
                   verbatimTextOutput("clientdataText"), width = NULL),
              box( title ="session information",
                   verbatimTextOutput("statusinfo"), width = NULL)
              )
              
              )
  ))
))