



instance_id <- 1000
options(shiny.maxRequestSize = 2024 * 1024 ^ 2) # set max file size for file upload
options(shiny.deprecation.messages = FALSE)
options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {
  instance_id <<- instance_id + 1
  this_instance <- instance_id

  vals = reactiveValues()
  vals$Data = data.table(
    job = "example job",
    gap_proportion = 80,
    groups =2,
    hmm_mode = 1
 )
  
  output$TableBody <- renderUI({
    if(exists("vals")){
    fluidPage(box(
      width = 12,
      column(
        6,
        offset = 6,
        HTML(
          '<div class="btn-group" role="group" aria-label="Basic example">'
        ),
       
        HTML('</div>')
      ),
      
      column(12, dataTableOutput("Main_table")),
      tags$script(
        HTML(
          '$(document).on("click", "input", function () {
          var checkboxes = document.getElementsByName("row_selected");
          var checkboxesChecked = [];
          for (var i=0; i<checkboxes.length; i++) {
          if (checkboxes[i].checked) {
          checkboxesChecked.push(checkboxes[i].value);
          }
          }
          Shiny.onInputChange("checked_rows",checkboxesChecked);
  })'
)
      ),
tags$script(
  "$(document).on('click', '#Main_table button', function () {
  Shiny.onInputChange('lastClickId',this.id);
  Shiny.onInputChange('lastClick', Math.random())
});"
)

    ))
    } else {
      NULL
    }
    })
  
  output$Main_table <- renderDataTable({
   if(exists("vals")){
     
     
    DT = vals$Data
   
    
    DT[["Actions"]] <-
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <button type="button" class="btn btn-secondary delete" id=delete_',
        1:nrow(vals$Data),
        '>Delete</button>
        <button type="button" class="btn btn-secondary modify"id=modify_',
        1:nrow(vals$Data),
        '>Show results</button>
        </div>
        
        '
      )

      datatable(DT,
                escape = F)
   } else {
     NULL
   }

    
  })
  
  
  
  fake_sales_modal <- modalDialog(fluidPage(h3(
    strong("Monthly sales of selected brands"), align = "center"
  ),
  plotOutput('sales_plot')),
  size = "l")
  
  output$sales_plot <- renderPlot({
    require(ggplot2)
    ggplot(vals$fake_sales, aes(x = month, y = sales, color = Brands)) +
      geom_line()
  })
  
  ##Managing in row deletion
  modal_modify <- modalDialog(fluidPage(
    h3(strong("Row modification"), align = "center"),
    hr(),
    dataTableOutput('row_modif'),
    actionButton("save_changes", "Save changes"),
    
    tags$script(
      HTML(
        "$(document).on('click', '#save_changes', function () {
        var list_value=[]
        for (i = 0; i < $( '.new_input' ).length; i++)
        {
        list_value.push($( '.new_input' )[i].value)
        }
        Shiny.onInputChange('newValue', list_value)
        });"
)
    )
    ),
size = "l")
  
  
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId %like% "delete")
                 {
                   row_to_del = as.numeric(gsub("delete_", "", input$lastClickId))
                   vals$Data = vals$Data[-row_to_del]
                 }
                 else if (input$lastClickId %like% "modify")
                 {
                   showModal(modal_modify)
                 }
               })
  
  output$row_modif <- renderDataTable({
    selected_row = as.numeric(gsub("modify_", "", input$lastClickId))
    old_row = vals$Data[selected_row]
    row_change = list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals$Data[[i]]))
      {
        row_change[[i]] <-
          paste0('<input class="new_input" type="number" id=new_',
                 i,
                 '><br>')
      }
      else
        row_change[[i]] <-
          paste0('<input class="new_input" type="text" id=new_',
                 i,
                 '><br>')
    }
    row_change = as.data.table(row_change)
    setnames(row_change, colnames(old_row))
    DT = rbind(old_row, row_change)
    rownames(DT) <- c("Current values", "New values")
    DT
    
  }, escape = F, options = list(dom = 't', ordering = F), selection = "none")
  
  
  observeEvent(input$newValue,
               {
                 newValue = lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(
                     as.character(col)
                   ))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF = data.frame(lapply(newValue, function(x)
                   t(data.frame(x))))
                 colnames(DF) = colnames(vals$Data)
                 vals$Data[as.numeric(gsub("modify_", "", input$lastClickId))] <-
                   DF
                 
               })
  
  # Return the components of the URL in a string:
  output$urlText <- renderText({
    paste(
      sep = "",
      "protocol: ",
      session$clientData$url_protocol,
      "\n",
      "hostname: ",
      session$clientData$url_hostname,
      "\n",
      "pathname: ",
      session$clientData$url_pathname,
      "\n",
      "port: ",
      session$clientData$url_port,
      "\n",
      "search: ",
      session$clientData$url_search,
      "\n"
    )
  })
  
  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse = ", ")
  })
  
  # Store in a convenience variable
  cdata <- session$clientData
  
  # Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)
    
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
  
  ###### END TABLE
  
  # which fields are mandatory
  fieldsMandatory <- c("eden_run_name")
  
  # add an asterisk to an input label
  labelMandatory <- function(label) {
    tagList(label,
            span("*", class = "mandatory_star"))
  }
  
  observeEvent(input$Add_row_head, {
    new_row = data.frame(
      job = "NewRun",
      gap_proportion = sample(1:20, 1),
      groups = status$num_groups,
      hmm_mode = round(rnorm(1, 1000, 1000) ^ 2)
    )
    vals$Data = rbind(vals$Data, new_row)
  })

  # get current Epoch time
  epochTime <- function() {
    return(as.integer(Sys.time()))
  }
  
  # get a formatted string of the timestamp (exclude colons as they are invalid
  # characters in Windows filenames)
  humanTime <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
  }
  
  fileInput3 <-
    function (inputId,
              label,
              multiple = FALSE,
              accept = NULL,
              width = NULL)
    {
      restoredValue <- restoreInput(id = inputId, default = NULL)
      if (!is.null(restoredValue) &&
          !is.data.frame(restoredValue)) {
        warning("Restored value for ", inputId, " has incorrect format.")
        restoredValue <- NULL
      }
      if (!is.null(restoredValue)) {
        restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
      }
      inputTag <-
        tags$input(
          id = inputId,
          name = inputId,
          type = "file",
          style = "display: none;",
          `data-restore` = restoredValue
        )
      if (multiple)
        inputTag$attribs$multiple <- "multiple"
      if (length(accept) > 0)
        inputTag$attribs$accept <- paste(accept, collapse = ",")
      div(
        class = "form-group shiny-input-container",
        style = if (!is.null(width))
          paste0("width: ", validateCssUnit(width), ";"),
        tags$label(label),
        div(class = "input-group", tags$label(
          class = "input-group-btn",
          span(class = "btn btn-default btn-file", "Browse...",
               inputTag)
        ))
      )
    }
  
  # ==============================================================================================
  # initialize status
  # ==============================================================================================
  status <-
    reactiveValues() # save the run status as a reactive object
  status$orf_finished <- FALSE # TRUE after start_orf.sh is executed
  status$eden_finished <-
    FALSE # TRUE if a finished signal detected in the log file
  status$eden_failed <-
    FALSE # is TRUE if a error signal detected in the log file
  status$samples_provided <-
    FALSE # TRUE if a samples.txt is provided
  status$faa <- FALSE
  status$ffn <- FALSE
  status$fasta <- FALSE
  status$isready <- TRUE
  status$num_hmm <- 0
  status$num_fasta <- 0
  status$num_faa <- 0
  status$num_ffn <- 0
  status$num_groups <- 0
  status$filespassed <- FALSE
  status$grouppassed <- TRUE
  status$settingspassed <- FALSE
  status$hmmpassed <- TRUE
  status$groupnum <- 1
  status$showstart <- FALSE
  

 
  # ==============================================================================================
  # check procedure for the detection which files are provided by the user
  # ==============================================================================================
  
  ### running status
  # function to evaluate if a process is running in the background
  update_running_status <- function() {
    my_running_status <<-
      get_running_status() # get_new_status returns the html-alert box visible on the first page
  }
  output$statusrunning = renderText({
    invalidateLater(millis = 1000, session) # update every 1 seconds
    update_running_status() # get a new status line
  })
  
  ### finished status
  # function to evaluate if a process is running in the background
  update_finished_status <- function() {
    my_finished_status <<-
      get_finished_status() # get_new_status returns the html-alert box visible on the first page
  }
  output$statusfinished = renderText({
    invalidateLater(millis = 1000, session) # update every 1 seconds
    update_finished_status() # get a new status line
  })
  
  
  # ==============================================================================================
  # procedure to update log file / progress message
  # ==============================================================================================
  # loads the log file and extract the informations for the check process bar
  get_new_log <- function() {
    cat(".")
    msg <- ""
    data <- read.table(log.path, header = F, sep = ";")
    colnames(data) <- c("step", "steps", "message")
    
    last_event <- data[nrow(data),]$message # current message
    steps <- data[nrow(data),]$steps # current step
    step <-
      data[nrow(data),]$step # total number of steps till finished
    # print(last_event) # for debugging, print the last event
    if (last_event == "error") {
      # error signal
      status$check_failed <<- TRUE
      msg <-
        paste("<span class='label label-danger'>check success</span>")
    } else if (last_event == "finished") {
      # finished signal for eden
      msg <-
        paste("<span class='label label-success'>eden finished</span>")
      status$eden_finished <- TRUE
    } else {
      msg <-
        paste("<span class='label label-success'>",
              last_event,
              "</span>")
    }
    
    return(msg)
  }
  
  output$dashboard1 <- renderUI({
    paste0(status$num_fasta, " Files")
  })
  
  # is running informations
  output$dashboard2 <- renderUI({
    paste0(status$num_fasta, " Files")
  })
  
  output$box1status <- renderUI({
    if (status$filespassed) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    p("File upload ", icon(iconName, lib = "glyphicon"))
  })
  
  output$box2status <- renderUI({
    if (status$grouppassed) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    p("Sample grouping ", icon(iconName, lib = "glyphicon"))
  })
  
  output$startEDEN <- renderUI({
    if (status$filespassed && status$hmmpassed && status$grouppassed) {
      box(title =  uiOutput("box5status"), uiOutput("upload_ui_buttons"), status = "success", solidHeader=T, width = NULL)
    } else {
      box(title =  uiOutput("box5status"), uiOutput("startError"), status = "warning", solidHeader=T, width = NULL)
    }
  })
  
  output$startError <- renderUI({
    iconName <- "remove"
    p("Please check files ", icon(iconName, lib = "glyphicon"))
  })
  
  
  output$box3status <- renderUI({
    if (status$hmmpassed) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    p("HMM upload ", icon(iconName, lib = "glyphicon"))
  })
  
  output$statusinfo <- renderPrint({
    cat(paste("status$filespassed: ",status$filespassed, " \n"))
    cat(paste("status$grouppassed: ", status$grouppassed, " \n"))
    cat(paste("status$hmmpassed: ", status$hmmpassed, " \n"))
    cat(paste("status$groupnum: ", status$groupnum, " \n"))
    cat(paste("status$num_hmm: ", status$num_hmm, " \n"))
    cat(paste("status$num_fasta: ", status$num_fasta, " \n"))
    cat(paste("status$num_faa: ", status$num_faa, " \n"))
    cat(paste("status$num_ffn: ", status$num_ffn, " \n"))
  })
  

  output$sessioninfo <- renderPrint({
    cat(paste("Session ID: ", Sys.getpid(), " \n"))
    cat(paste("Global Instance ID: ", instance_id, " \n"))
    cat(paste("This Instance ID: ", this_instance, " \n"))
  })
  
  output$box4status <- renderUI({
    if (TRUE) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    p("Settings ", icon(iconName, lib = "glyphicon"))
  })
  
  
  output$box5status <- renderUI({
    if (status$filespassed & status$hmmpassed ) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    p("Start ", icon(iconName, lib = "glyphicon"))
  })
  
  output$upload_ui_head_a <- renderUI({
    if (input$intype == 'orf') {
      if (status$num_fasta == 0) {
        conditionalPanel(condition = "status$isready==TRUE",
                         fluidRow(column(
                           5,
                           fileInput3(
                             'files_faa',
                             labelMandatory('upload .faa files'),
                             accept = c('.faa'),
                             multiple = TRUE
                           )
                         ), column(
                           5,
                           fileInput3(
                             'files_ffn',
                             labelMandatory('upload .ffn files'),
                             accept = c('.ffn'),
                             multiple = TRUE
                           )
                         )),
                         
                         fluidRow(column(
                           5,
                           htmlOutput("upload_response_faa")
                         ), column(
                           5,
                           htmlOutput("upload_response_ffn")
                         )))
      } else {
        conditionalPanel(condition = "input.tsp=='tab1'",
                      #   htmlOutput("warning2"), 
                         
                         
                         showModal(modalDialog(
                           title = "Warning",
                           "You already have fasta files uploaded. If you want to use ORFs instead please click the 'reset button' first.",
                           easyClose = TRUE,
                           footer = NULL
                         )),
                      updateRadioButtons(session, "intype",
                                         selected = "fasta"
                      )
                      
                      
        
                         )
        
        
      }
      
    } else {
      if (status$num_faa == 0 & status$num_ffn == 0) {
        conditionalPanel(
          condition = "input.tsp=='tab1'",
          
          fileInput3(
            'files_fasta',
            labelMandatory('upload .fasta files'),
            accept = c('.fasta'),
            multiple = TRUE,
            width = '100%'
          ),
          htmlOutput("upload_response")
          
          
          
        )
      } else {
        conditionalPanel(condition = "input.tsp=='tab1'",
                         #htmlOutput("warning3"),
                         
                         showModal(modalDialog(
                           title = "Warning",
                           "You already have ORF files uploaded. If you want to use fasta instead please click the 'reset button' first.",
                           easyClose = TRUE,
                           footer = NULL
                         )),
                         updateRadioButtons(session, "intype",
                                            selected = "orf"
                         )
                         )
        
        
      }
      
      
    }
  })
  
  ### ui head part
  output$upload_ui_head <- renderUI({
    conditionalPanel(
      condition = "status.isready==TRUE",
      helpText(
        "Specify input file format. You can either upload .faa and .ffn files of open reading frames (ORF) or the nucleotide .fasta file (in this case the ORFs will be predicted inside the pipeline). Fields marked with * have to be provided."
      ),
      radioButtons(
        "intype",
        labelMandatory("Input type:"),
        c(
          "fasta files with open reading frame information" = "orf",
          "multiple fasta file" = "fasta"
        )
      ),
      uiOutput("upload_ui_head_a")
      
    )
  })
  
  output$fasta_uploaded <- renderTable({
    if (is.null(input$files_fasta))
      return(NULL)
    else {
      status$filespassed <- TRUE
      infiles_fasta1 <- as.data.frame(input$files_fasta)
      infiles_fasta1$dest <-
        paste(fasta.path, infiles_fasta1$name, sep = "/")
      for (i in 1:nrow(infiles_fasta1)) {
        cmd <-
          paste("mv ",
                infiles_fasta1$datapath[i],
                " ",
                infiles_fasta1$dest[i],
                sep = "")
        err <- system(cmd,  intern = TRUE)
      }
      out <- paste(err)
      system2("echo",
              paste('";;fasta files added" >> ', log.path, sep = ""))
      status$fasta <- TRUE
      status$files <- list.files(path =  fasta.path,
                                 full.names = FALSE,
                                 recursive = FALSE)
      status$num_fasta <- length(status$files)
      
    }
    return(NULL)
  })
  
  output$faa_uploaded <- renderTable({
    if (is.null(input$files_faa))
      return(NULL)
    else {
      infiles_faa <- as.data.frame(input$files_faa)
      
      infiles_faa$dest <-
        paste(faa.path, infiles_faa$name, sep = "/")
      for (i in 1:nrow(infiles_faa)) {
        cmd <-
          paste("mv ",
                infiles_faa$datapath[i],
                " ",
                infiles_faa$dest[i],
                sep = "")
        err <- system(cmd,  intern = TRUE)
      }
      out <- paste(err)
      system2("echo", paste('";;faa files added" >> ', log.path, sep = ""))
      status$faa <- TRUE
      status$files <- list.files(path =  faa.path,
                                 full.names = FALSE,
                                 recursive = FALSE)
      status$faa_files <- list.files(path =  faa.path,
                                     full.names = FALSE,
                                     recursive = FALSE)
      
      status$num_faa <- length(status$faa_files)
      if (length(status$ffn_files) > 0) {
        #compare faa and ffn names
        status$filespassed <- TRUE
      }
      return(NULL)
    }
    
    
    
  })
  
  
  output$hmm_uploaded <- renderTable({
    if (is.null(input$hmmfile))
      return(NULL)
    else{
      # process ffn
      status$hmmpassed <- TRUE
      cat("setting hmmpassed to true")
      hmmfile <- as.data.frame(input$hmmfile)
      hmmfile$dest <- hmm.path
      cmd <-
        paste("mv ", hmmfile$datapath, " ", hmmfile$dest, sep = "")
      err <- system(cmd,  intern = TRUE)
      out <- paste(err)
      system2("echo", paste('";;hmmfile added" >> ', log.path, sep = ""))
      hmmfile_success <<- TRUE
      status$num_hmm <- 1
      
      return(NULL)
    }
  })
  
  
  
  output$ffn_uploaded <- renderTable({
    if (is.null(input$files_ffn))
      return(NULL)
    else {
      infiles_ffn <- as.data.frame(input$files_ffn)
      
      infiles_ffn$dest <-
        paste(ffn.path, infiles_ffn$name, sep = "/")
      for (i in 1:nrow(infiles_ffn)) {
        cmd <-
          paste("mv ",
                infiles_ffn$datapath[i],
                " ",
                infiles_ffn$dest[i],
                sep = "")
        err <- system(cmd,  intern = TRUE)
      }
      out <- paste(err)
      system2("echo", paste('";;ffn files added" >> ', log.path, sep = ""))
      status$ffn <- TRUE
      status$files <- list.files(path =  ffn.path,
                                 full.names = FALSE,
                                 recursive = FALSE)
      status$ffn_files <- list.files(path =  ffn.path,
                                     full.names = FALSE,
                                     recursive = FALSE)
      status$num_ffn <- length(status$ffn_files)
      if (length(status$faa_files) > 0) {
        status$filespassed <- TRUE
      }
      return(NULL)
    }
    
  })
  
  
  ### ui mid part
  output$upload_ui_mid <- renderUI({
    #   if (status$filespassed) {
    conditionalPanel(
      condition = "input.tsp=='tab1'",
      helpText(
        "Specify file handling: If you want to perform a comparative analysis you have to specify which samples are get pooled together. On default all samples will be pooled together."
      ),
      uiOutput('uploadsamplestxt')
    )
    
    
    #  } else {
    #  conditionalPanel(condition = "input.tsp=='tab1'",
    
    #                  htmlOutput("warning1"))
    # }
  })
  
  output$upload_ui_bottom <- renderUI({
    conditionalPanel(
      condition = "input.tsp=='tab4'",
      helpText("Specify name and thresholds"),
      textInput(
        "eden_run_name",
        labelMandatory("give your analysis a unique name"),
        value = "eden_run_1"
      ),
      #  textInput("eden_run_cpus", label = "number of CPUs used for analysis", value = "4"),
      numericInput("eden_run_cpus", label = "number of CPUs", value = 4),
      helpText(
        "Low-confidence postions can be automatically filtered for the dN/dS analysis. On deault, positions with more than 80% gaps in the alignment will not be used for dN/dS caluclation."
      ),
      sliderInput(
        "eden_run_gap",
        label = "gap proportion to filter out low confidence positions",
        min = 0,
        max = 100,
        value = 80
      )
    )
  })
  
  output$upload_hmm <- renderUI({
    if (input$radio == 1) {
  
      conditionalPanel(
        condition = "input.tsp=='tab3'",
        
        fileInput3(
          'hmmfile',
          'upload a hidden markov model file',
          accept = c('.HMM', '.hmm'),
          multiple = FALSE
        ),
        htmlOutput("upload_response_hmm")
      )
    } 
  })
  
  output$upload_ui_hmm <- renderUI({
    conditionalPanel(
      condition = "input.tsp=='tab3'",
      
      helpText(
        "Select group definition. You can select precalculated hidden markov models (HMM) or upload a .HMM file which may contain multiple hmm models for the gene families of interest."
      ),
      
      radioButtons(
        "radio",
        label = labelMandatory("Group definition"),
        choices = list(
          "upload HMM model" = 1,
          "use full TIGRFAM database" = 2
        ),
        selected = 2
      ),
      uiOutput("upload_hmm")
    )
    # uiOutput("hmmui"))
  })
  
  output$upload_ui_buttons <- renderUI({
    conditionalPanel(
      condition = "input.tsp=='start",
    # htmlOutput("warning4"),
      actionButton('checkButton', label = "start analysis")#,
  #    shinyjs::hidden(
  #      span(id = "checkButton", "start analysis"),
#       div(id = "error",
#            div(
#             br(), tags$b("Error: "), span(id = "error_msg")
#            ))
#      )
      
      #  actionButton('deletefiles', label = "reset files")
    )
  })
  
  
  output$reset_ui_buttons <- renderUI({
    conditionalPanel(condition = "input.tsp=='start",
                     actionButton('deletefiles', label = "reset files"))
  })
  
  # show upload section for samples.txt
  output$uploadsamplestxt <- renderUI({
    conditionalPanel(
      condition = "input.tsp=='tab1'",
      radioButtons(
        "grouptype", labelMandatory(
        "select kind of analysis"),
        c(
          "pool samples together" = "pooling",
          "comparative analysis" = "comparative"
        )
      ),
      uiOutput('uploadsamplestxt_a'),
      htmlOutput("upload_response_grouping")
      # htmlOutput("grouping_response")
    )
  })
  
  # show upload section for samples.txt
  output$uploadsamplestxt_a <- renderUI({
    status$grouppassed
    if (input$grouptype == 'comparative') {
        if (status$filespassed) {
        # comparative mode and we have files uploaded
        conditionalPanel(
          condition = "input.tsp=='tab1'",
          helpText("Please define which samples are pooled together:"),
          numericInput(
            "groupnum",
            label = "number of groups",
            value = 2,
            width = '100px'
          ),
          uiOutput("groupboxes"),
          uiOutput("updategroups")
        )
      } else {
        # comparative mode but no files uploaded
        htmlOutput("warning1")
      }
      
    }
    else {
      # we are in the pooling mode, there is no user input needed
      #   status$grouppassed <- TRUE
    }
    
    
    
  })
  
  
  output$groupboxes <- renderUI({
    members <<- as.integer(input$groupnum) # default 2
    max_pred <- as.integer(20)
    lapply(1:members, function(i) {
      fluidRow(column(
        5,
        textInput(
          inputId = paste0("name", i),
          label = paste("name", i),
          width = "100%",
          value = paste("group_", i, sep = "")
        )
      ), column(
        5,
        selectInput(
          inputId = paste0("ind", i),
          label = paste("group", i),
          choices = status$files,
          multiple = TRUE,
          width = "100%"
        )
      ))
    })
    
  })
  
  output$updategroups <- renderUI({
    actionButton('updategrouping', label = "update grouping")
  })
  
  # ==============================================================================================
  # button press events
  # ==============================================================================================
  
  # delete files on button press
  observeEvent(input$deletefiles, {
    # reset number of uploaded files to zero
    status$filespassed <- FALSE
 #   status$grouppassed <- FALSE
    status$settingspassed <- FALSE
    status$hmmpassed <- FALSE
    status$num_fasta <- 0
    status$num_hmm <- 0
    status$num_faa <- 0
    status$num_ffn <- 0
    status$num_groups <- 0
    
    updateRadioButtons(session, "radio",
                       selected = 2
    )
    updateRadioButtons(session, "intype",
                       selected = "orf"
    )
    updateRadioButtons(session, "grouptype", selected = "pooling")
    updateTextInput(session, "eden_run_name", value="eden_run_1")
    updateSliderInput(session, "eden_run_gap", value="80")
    updateNumericInput(session, "eden_run_cpus", value=4)
    
    isolate({
      unlink(faa.path, recursive = T, force = T)
      unlink(ffn.path, recursive = T, force = T)
      unlink(fasta.path, recursive = T, force = T)
      unlink(hmm.path, recursive = T, force = T)
      unlink(sample.path, recursive = T, force = T)
      unlink("/home/eden/data/groups.txt",
             recursive = T,
             force = T)
      unlink(log.path, recursive = T, force = T)
      system2("echo", paste('";;server ready" >> ', log.path, sep = ""))
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
      
      status <-
        reactiveValues() # save the run status as a reactive object
      input <-
        reactiveValues() # save the run status as a reactive object
      
    })
  })
  
  
  
  # iterate over input$name_ and input$ind_ and create a samples table
  observeEvent(input$updategrouping, {
  #  xgrouing <<- TRUE
    sam <-  rep("unknown", members)
    nam <- rep("unknown", members)
    for (i in 1:members) {
      temp <- paste0("name", i)
      temp2 <<- paste0("ind", i)
      nam[i] <- input[[temp]]
      print(paste(basename(file_path_sans_ext(input[[temp2]])),  collapse =
                    "+"))
      #  sam[i] <- paste(substr(input[[temp2]], 1, nchar(input[[temp2]])-4), collapse="+")
      
      #  sam[i] <- paste(substr(input[[temp2]], 1, nchar(input[[temp2]])-4), collapse="+")
      sam[i] <-
        paste(basename(file_path_sans_ext(input[[temp2]])),  collapse = "+")
    }
    dat <- data.frame(name = nam, sample = sam)
    write.table(
      dat,
      file = sample.path,
      row.names = F,
      quote = F,
      col.names = F,
      sep = ";"
    )
    status$samples_provided <- TRUE
    status$num_groups <- members
    system2("echo",
            paste('";;grouping file updated" >> ', log.path, sep = ""))
    
  })
  
  # ==============================================================================================
  # function that updates message based on files prsovided
  # ==============================================================================================
  
  get_running_status <- function() {
    if (file.exists(lock.file)) {
      msg <-
        "</br><div class='alert alert-dismissible alert-info'>
      <button type='button' class='close' data-dismiss='alert'>&times;</button>
      <p><strong>A process is running in the background! Please wait</strong> until the process is finished.</p>"
      # print ok when criteria are met
      return(msg)
    } else {
      return(NULL)
    }
  }
  
  get_finished_status <- function() {
    if (isolate(status$eden_finished)) {
      msg <-
        "</br><div class='alert alert-dismissible alert-info'>
      <button type='button' class='close' data-dismiss='alert'>&times;</button>
      <p><strong>Eden finished!</stong>  <a href='../eden-visualizer' class='alert-link'> download and visualize the results</p>"
      # clean up
      unlink(faa.path, recursive = T, force = T)
      unlink(ffn.path, recursive = T, force = T)
      unlink(fasta.path, recursive = T, force = T)
      unlink(hmm.path, recursive = T, force = T)
      unlink(sample.path, recursive = T, force = T)
      unlink("/home/eden/data/groups.txt",
             recursive = T,
             force = T)
      unlink(log.path, recursive = T, force = T)
      system2("echo", paste('";;server ready" >> ', log.path, sep = ""))
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
      status <-
        reactiveValues() # save the run status as a reactive object
      input <-
        reactiveValues() # save the run status as a reactive object
      return(msg)
    } else {
      return(NULL)
    }
  }
  
  # ==============================================================================================
  # execution of shell scripts and update of statusmsg print
  # ==============================================================================================
  
  # use observe event to check if button pressed
  observeEvent(input$checkButton, {
    #  file.create(lock.file)
    shinyjs::disable("checkButton")
    shinyjs::disable("deletefiles")
    
    system(
      paste(
#        "/home/eden/start_check.sh",
          "./start_check.sh",
        
        faa.path,
        ffn.path,
        input$eden_run_cpus,
        input$eden_run_name,
        input$eden_run_gap / 100,
        hmm.path,
        sample.path
      ),
      wait = FALSE
    )
  })
  
  # Initialize log
  my_log <<- get_new_log()
  
  # Function to update my_data
  update_log <- function() {
    my_log <<- get_new_log()
  }
  
  output$log = renderText({
    invalidateLater(millis = 1000, session)
    update_log()
    
  })
  
  output$text1 <-  renderText({
    "</br>"
  })
  
  output$text2 <- renderText({
    paste(status$files)
  })
  
  #
  # ==============================================================================================
  # clean up on close
  # ==============================================================================================
  
  ### clean up routine
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    unlink(faa.path, recursive = T, force = T)
    unlink(ffn.path, recursive = T, force = T)
    unlink(fasta.path, recursive = T, force = T)
    unlink(hmm.path, recursive = T, force = T)
    unlink(sample.path, recursive = T, force = T)
    unlink("/home/eden/data/groups.txt",
           recursive = T,
           force = T)
    unlink(log.path, recursive = T, force = T)
    system2("echo", paste('";;server ready" >> ', log.path, sep = ""))
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
    status <-
      reactiveValues() # save the run status as a reactive object
    input <-
      reactiveValues() # save the run status as a reactive object
    #    js$reset()
  })
  
  ### new
  
  output$warning1 = renderText({
    input$deletefiles

    msg <- "<div class='alert alert-dismissible alert-info'>
    <button type='button' class='close' data-dismiss='alert'>&times;</button>
    Please provide input files first.
    </div>"

    msg
  })
  
  output$warning2 = renderText({
    input$deletefiles
    msg <- "<div class='alert alert-dismissible alert-info'>
    <button type='button' class='close' data-dismiss='alert'>&times;</button>
    <strong>Warning:</strong> You already have fasta files uploaded. If you want to use ORFs instead please click the 'reset button' first.
    </div>"
    msg
  })
  
  output$warning3 = renderText({
    input$deletefiles
    msg <- "<div class='alert alert-dismissible alert-info'>
    <button type='button' class='close' data-dismiss='alert'>&times;</button>
    <strong>Warning:</strong> You already have ORF files uploaded. If you want to use fasta files instead please click the 'reset button' first.
    </div>"
    msg
  })
  
  output$warning4 = renderText({
    input$deletefiles
    if (!status$hmmpassed) {
      msg <- "<div class='alert alert-dismissible alert-info'>
      <button type='button' class='close' data-dismiss='alert'>&times;</button>
      <strong>User input needed!</strong> Please upload a hmm file.  <a href='#' class='alert-link' data-vivaldi-spatnav-clickable='1'>Or download example input files</a>.
      </div>"
    } else {
      msg <- ""
    }
    msg
  })
  

  output$check_response1 = renderText({
    input$deletefiles
    if (status$filespassed) {
      check1 <- ""
    } else {
      check1 <- "<div class='alert alert-dismissible alert-info'>
      <button type='button' class='close' data-dismiss='alert'>&times;</button>
      <strong>User input needed!</strong> Please provide input files <a href='#' class='alert-link' data-vivaldi-spatnav-clickable='1'>or download example input files</a>.
      </div>"
      
    }
    msg <- paste(check1, sep = "</br>")
    msg
  })
  
  
  
  output$upload_response = renderText({
    input$deletefiles
    
    msg <-
      paste("<span class='badge'>",
            status$num_fasta,
            "</span> fasta files uploaded",
            sep = "")
    
    msg
  })
  
  ### new
  output$upload_response_faa = renderText({
    input$deletefiles
    msg <-
      paste("<span class='badge'>",
            status$num_faa,
            "</span> faa files uploaded",
            sep = "")
    msg
  })
  
  output$upload_response_hmm = renderText({
    input$deletefiles
    msg <-
      paste("<span class='badge'>",
            status$num_hmm,
            "</span> hmm model uploaded",
            sep = "")
    if (status$num_hmm > 1) {
      status$hmmpassed <- TRUE
    }
    msg
  })
  
  
  
  
  output$upload_response_grouping = renderText({
    status$grouppassed
    input$deletefiles # reevaluate if delete button is pressed
    
    if (input$grouptype == "comparative") {
      if (status$num_groups > 1) {
        msg <-
          paste("<span class='badge'>",
                status$num_groups,
                "</span> groups defined",
                sep = "")
      } else {
       # msg <- paste("no groups defined", sep = "")
        msg <- NULL
      }
    } else {
    
 msg <- NULL
    }
    msg
  })

  
  
  
  output$box1 <- renderUI({
    box(
      title = "File upload",
      uiOutput("upload_ui_head"),
      tableOutput("fasta_uploaded"),
      tableOutput("faa_uploaded"),
      tableOutput("ffn_uploaded"),
      status = "warning",
      solidHeader = T
      #htmlOutput("check_response1")
    )
    
  })
  
  
  output$box2 <- renderUI({
    if (status$grouppassed) {
      box(
        title = "Gene family",
        uiOutput("upload_ui_mid"),
        status = "warning",
        solidHeader = T
      )
    } else {
      box(
        title = "Gene family",
        uiOutput("upload_ui_mid"),
        status = "success",
        solidHeader = T
      )
    }
  })
  
  
  


  
  ### new
  output$upload_response_ffn = renderText({
    input$deletefiles
    msg <-
      paste("<span class='badge'>",
            status$num_ffn,
            "</span> ffn files uploaded",
            sep = "")
    msg
  })
  
  output$grouping_response = renderText({
    # msg <- paste("<span class='badge'>", status$groupnum  ,"</span> grouping", sep='')
    msg <- ""
    msg
  })
  
  ### new
  output$timeline = renderText({
    input$deletefiles
    
    
    msg <- "  <ul class='timeline'>
    <li>
    <div class='timeline-badge success'><i class='glyphicon glyphicon-ok-sign'></i></div>
    <div class='timeline-panel'>
    <div class='timeline-heading'>
    <h4 class='timeline-title'>process 1</h4>
    <p><small class='text-muted'><i class='glyphicon glyphicon-time'></i>started 11 hours ago</small></p>
    </div>
    <div class='timeline-body'>
    <p>cmd
    </div>
    </div>
    </li>
    <li class='timeline'>
    <div class='timeline-badge warning'><i class='glyphicon glyphicon-ok-sign'></i></div>
    <div class='timeline-panel'>
    <div class='timeline-heading'>
    <h4 class='timeline-title'>process2</h4>
    </div>
    <div class='timeline-body'>
    
    </div>
    </div>
    </li>
    <li>
    <div class='timeline-badge danger'><i class='glyphicon glyphicon-ok-sign'></i></div>
    <div class='timeline-panel'>
    <div class='timeline-heading'>
    <h4 class='timeline-title'>process3</h4>
    </div>
    <div class='timeline-body'>
    
    </div>
    </div>
    </li>
    
    <li>
    <div class='timeline-badge success'><i class='glyphicon glyphicon-ok-sign'></i></div>
    <div class='timeline-panel'>
    <div class='timeline-heading'>
    <h4 class='timeline-title'>process5</h4>
    </div>
    <div class='timeline-body'>
    
    <hr>
    <div class='btn-group'>
    <button type='button' class='btn btn-primary btn-sm dropdown-toggle' data-toggle='dropdown'>
    <i class='glyphicon glyphicon-cog'></i> <span class='caret'></span>
    </button>
    <ul class='dropdown-menu' role='menu'>
    <li><a href='#'>Action</a></li>
    <li><a href='#'>Another action</a></li>
    <li><a href='#'>Something else here</a></li>
    <li class='divider'></li>
    <li><a href='#'>Separated link</a></li>
    </ul>
    </div>
    </div>
    </div>
    </li>
    <li>
    <div class='timeline-panel'>
    <div class='timeline-heading'>
    <h4 class='timeline-title'>process6</h4>
    </div>
    <div class='timeline-body'>
    
    </div>
    </div>
    </li>
    <li class='timeline'>
    <div class='timeline-badge success'><i class='glyphicon glyphicon-repeat'></i></div>
    <div class='timeline-panel'>
    <div class='timeline-heading'>
    <h4 class='timeline-title'>process7</h4>
    </div>
    <div class='timeline-body'>
    
    </div>
    </div>
    </li>
    </ul>
    "

    msg
  })
  
  # sample grouping status
  observeEvent(input$grouptype, {
    if (input$grouptype == "pooling"){
      status$grouppassed <- TRUE
    } else {
      if (status$num_groups > 1){
        status$grouppassed <- TRUE
      } else {
        status$grouppassed <- FALSE
      }
    }
  })
  
  observe({
    num <- status$num_groups
    if (num>1){
      status$grouppassed <- TRUE
    } else {
      status$grouppassed <- FALSE
    }
  })
  
  # sample hmm status
  observeEvent(input$radio, {
    if (input$radio == 2){
      status$hmmpassed <- TRUE
    } else {
      if (status$num_hmm > 0){
        status$hmmpassed <- TRUE
      } else {
        status$hmmpassed <- FALSE
      }
    }
  })
  
  observe({
    num <- status$num_hmm
    if (num>0){
      status$hmmpassed <- TRUE
    } else {
      status$hmmpassed <- FALSE
    }
  })
  
  # enable buttons if eden finished and cleanup
  observe({
    if (status$eden_finished){
      
      showModal(modalDialog(
        title = "Eden run finished!",
        "To inspect the results go to the Dashboard tab",
        easyClose = TRUE,
        footer = NULL
      ))
      
      shinyjs::enable("checkButton")
      shinyjs::enable("deletefiles")
      status$filespassed <- FALSE
      #   status$grouppassed <- FALSE
      status$settingspassed <- FALSE
      status$hmmpassed <- TRUE
      status$num_fasta <- 0
      status$num_hmm <- 0
      status$num_faa <- 0
      status$num_ffn <- 0
      status$num_groups <- 0
      
      updateRadioButtons(session, "radio",
                         selected = 2
      )
      updateRadioButtons(session, "intype",
                         selected = "orf"
      )
      updateRadioButtons(session, "grouptype", selected = "pooling")
      updateTextInput(session, "eden_run_name", value="eden_run_1")
      updateSliderInput(session, "eden_run_gap", value="80")
      updateNumericInput(session, "eden_run_cpus", value=4)
  
        unlink(faa.path, recursive = T, force = T)
        unlink(ffn.path, recursive = T, force = T)
        unlink(fasta.path, recursive = T, force = T)
        unlink(hmm.path, recursive = T, force = T)
        unlink(sample.path, recursive = T, force = T)
        unlink("/home/eden/data/groups.txt",
               recursive = T,
               force = T)
        unlink(log.path, recursive = T, force = T)
        system2("echo", paste('";;server ready" >> ', log.path, sep = ""))
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
        
        status <-
          reactiveValues() # save the run status as a reactive object
        input <-
          reactiveValues() # save the run status as a reactive object
    }
  })
  
  # add col to job table
  observeEvent(input$checkButton, {
    new_row = data.frame(
      job = input$eden_run_name,
      gap_proportion = input$eden_run_gap,
      groups = status$num_groups,
      hmm_mode = input$radio
    )
    vals$Data = rbind(vals$Data, new_row)
  })
  
  

  })