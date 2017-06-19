
instance_id <- 1000
options(shiny.maxRequestSize = 2024 * 1024 ^ 2) # set max file size for file upload
options(shiny.deprecation.messages = FALSE)
options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {
  instance_id <<- instance_id + 1
  this_instance <- instance_id
  
  vals = reactiveValues()
  vals$Data = data.table(
    job = list.files(
      path = tar.path,
      full.names = FALSE,
      recursive = FALSE
    )
  )
  
  output$samplesnum <- renderText({ 
    status$samplesnumber <- length(list.dirs(csv.path))
    as.character(status$samplesnumber -1)
  })
  
  # get the query string
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['text']])) {
      updateTextInput(session, "text", value = query[['text']])
    }
  })
  
  
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
  status$dataset <- "oligo.tar"
  status$newfiles <- FALSE
  status$serverstatus <- "ready"
  status$eden_error <- FALSE

  
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
    if(demomode){
      
      invalidateLater(millis = 60000, session) # update every 1 seconds
      update_running_status() # get a new status line
    NULL
    } else {
      invalidateLater(millis = 1000, session) # update every 1 seconds
      update_running_status() # get a new status line
      
    }
  })
  
  ### finished status
  # function to evaluate if a process is running in the background
#  update_finished_status <- function() {
 #   my_finished_status <<-
#      get_finished_status() # get_new_status returns the html-alert box visible on the first page
#  }
#  output$statusfinished = renderText({
#    invalidateLater(millis = 1000, session) # update every 1 seconds
#    update_finished_status() # get a new status line
#  })
  # 
  # ==============================================================================================
  # procedure to update log file / progress message
  # ==============================================================================================

  # function that checks if there are new tar files in the tar.path that have not folder in csv folder
  check_new_file <- function(){
    raws <- list.dirs(raw.path, recursive = F, full.names = F)
    tars <- list.files(tar.path, recursive = F, full.names = F)
    if (!identical(raws, tars)){
      status$newfiles <- TRUE
    } else {
      status$newfiles <- FALSE
    }
  }
  
  # loads the log file and extract the informations for the check process bar
  get_new_log <- function() {
#    cat(".")
   msg <- ""
    
    if(file.exists(log.path)){
     if( file.info(log.path)$size >0){
       data <- read.table(log.path, header = F, sep = ";")
       colnames(data) <- c("type", "time", "message")
       last_event <- data[nrow(data),]$message # current message
       if (last_event == "error") {
         status$eden_error <- TRUE
         last_error <- last_event
         status$serverstatus <- "error"
       } else if (last_event == "finished") {
         status$eden_finished <- TRUE
       }
     }
      output$logtable = renderTable({
        if(file.exists(log.path) & file.info(log.path)$size >0){
          logtable <- read.csv2(log.path, header=F)
          colnames(logtable) <- c("type", "time", "message")
          logtable.rev <- apply(as.data.frame(logtable), 2, rev)  # reverse
          last_event <- as.character( data[nrow(data),]$message) # current message
       print(logtable.rev)
          #  print(last_event) 
        } else {
          # either there is no log file at all or the log file is without content
          return(NULL)
        }
      }
      )
      
      return(msg)
     }

    
  }
  
  output$dashboard1 <- renderUI({
    paste0(status$num_fasta, " Files")
  })
  
  # is running informations
  output$dashboard2 <- renderUI({
    paste0(status$num_fasta, " Files")
  })
  
  output$box1statustest <- renderText({
    #i <- icon("ok", lib = "glyphicon")
    #
    if (status$filespassed) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    paste0("Sequence files <i class='glyphicon glyphicon-",iconName,"'></i>")

  })
  
  output$box2status <- renderText({
    if (status$grouppassed) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    paste0("Define grouping <i class='glyphicon glyphicon-",iconName,"'></i>")
  })
  
  output$startEDEN <- renderUI({
    if (status$filespassed && status$hmmpassed && status$grouppassed) {
      box(title =  uiOutput("box5status"), uiOutput("upload_ui_buttons"), status = "success", solidHeader=T, width = NULL)
    } else {
      box(title =  uiOutput("box5status"), uiOutput("startError"), status = "warning", solidHeader=T, width = NULL)
    }
  })
  
  output$startError <- renderUI({
 #   iconName <- "remove"
#    icon(iconName, lib = "glyphicon")
    paste0("Missing input data! Please check boxes with a 'x' sign and provide necessary informations")
  })
  
  
  output$box3status <- renderText({
    if (status$hmmpassed) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    paste0("Define HMM file <i class='glyphicon glyphicon-",iconName,"'></i>")
  })
  
  output$statusinfo <- renderPrint({
    cat(paste("input$dataset == '' : ",input$dataset == "", " \n"))
    
    cat(paste("status$filespassed: ",status$filespassed, " \n"))
    cat(paste("status$grouppassed: ", status$grouppassed, " \n"))
    cat(paste("status$hmmpassed: ", status$hmmpassed, " \n"))
    cat(paste("status$groupnum: ", status$groupnum, " \n"))
    cat(paste("status$num_hmm: ", status$num_hmm, " \n"))
    cat(paste("status$num_fasta: ", status$num_fasta, " \n"))
    cat(paste("status$num_faa: ", status$num_faa, " \n"))
    cat(paste("status$num_ffn: ", status$num_ffn, " \n"))
  })
  
  
  output$fileinfo <- renderPrint({
    cat(paste("faa.path: ",faa.path, " \n"))
    cat(paste("ffn.path: ",ffn.path,  " \n"))
    cat(paste("sample.path: ",sample.path,  " \n"))
    cat(paste("folder.path: ",folder.path,  " \n"))
    cat(paste("log.path: ",log.path,  " \n"))
    cat(paste("lock.file: ",lock.file,  " \n"))
  })
  
  output$vizinfo <- renderPrint({
    cat(paste("csv.path: ",csv.path, " \n"))
    cat(paste("raw.path: ",raw.path, " \n"))
    cat(paste("tar.path: ",tar.path, " \n"))
    cat(paste("status$dataset: ",status$dataset,  " \n"))
  })
  
  output$stateinfo <- renderPrint({
    cat(paste("status$eden_finished: ", status$eden_finished,  " \n"))
    cat(paste("status$eden_failed : ",status$eden_failed ,  " \n"))
    cat(paste("status$orf_finished : ",status$orf_finished ,  " \n"))
  })
  
  output$cmdinfo <- renderPrint({
    cat(  paste(
      "/home/eden/start_check.sh",
      
      #      "./start_check.sh",
      faa.path,
      ffn.path,
      input$eden_run_cpus,
      input$eden_run_name,
      input$eden_run_gap / 100,
      hmm.path,
      sample.path))
  })
  
  output$sessioninfo <- renderPrint({
    cat(paste("Session ID: ", Sys.getpid(), " \n"))
    cat(paste("Global Instance ID: ", instance_id, " \n"))
    cat(paste("This Instance ID: ", this_instance, " \n"))
  })
  
  output$box4status <- renderText({
    if (TRUE) {
      iconName <- "ok"
    } else {
      iconName <- "remove"
    }
    paste0("Settings <i class='glyphicon glyphicon-",iconName,"'></i>")
  })
  
  
  output$box5status <- renderText({
    if (status$filespassed & status$hmmpassed ) {
      text <- "start anaylysis"
    } else {
      text <- "Start analysis"
    }
    text
  })
  
  output$upload_ui_head_a <- renderUI({
    if (input$intype == 'orf') {
      if (status$num_fasta == 0) {
    #    conditionalPanel(condition = "status.isready==TRUE",

        tagList(
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
        conditionalPanel(condition = "status.isready==TRUE",
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
          condition = "status.isready==TRUE",
          
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
        conditionalPanel(condition = "status.isready==TRUE",
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
   # conditionalPanel(
    #  condition = "status.isready==TRUE",
    tagList(
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
      # check if the files have the right ending
      files <- input$files_fasta
     if(all(file_ext(files$name) =="fasta")){
    
        # check if we can read the fasta files
        # load all as read.fasta and raise error if they have no sequences
        passed <- TRUE
        withProgress(message = 'Checking files, please wait', value = 0,  {
          
       for (i in 1:nrow(files)){
           
           
         testload <- 
         if(length(read.fasta(files$datapath[1])) < 1){
          passed <- FALSE
          
         }
         incProgress(1/nrow(files), message = paste("checking", files$name[i]) )
         }
       })
        
        if (passed){
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
          #    system2("echo",
          #            paste('";;fasta files added" >> ', log.path, sep = ""))
          status$fasta <- TRUE
          status$files <- list.files(path =  fasta.path,
                                     full.names = FALSE,
                                     recursive = FALSE)
          status$num_fasta <- length(status$files)
          
          
        } else {
          showModal(modalDialog(
            title = "Please check fasta files",
            "Seems that a fasta files is not in the FASTA format (or empty). Please check input files and try again.",
            easyClose = TRUE,
            footer = NULL
          ))
          
        }

                
      } else {
        # not all files have "fasta" ending
        showModal(modalDialog(
          title = "Please check file ending",
          "Currenlty only files with the '.fasta' ending are supported.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    
      
    }
    return(NULL)
  })
  
  output$faa_uploaded <- renderTable({
    if (is.null(input$files_faa))
      return(NULL)
    else {
      
      files <- input$files_faa
      if(all(file_ext(files$name) =="faa")){
        
        # check if we can read the fasta files
        # load all as read.fasta and raise error if they have no sequences
        passed <- TRUE
        withProgress(message = 'Checking files, please wait', value = 0,  {
        for (i in 1:nrow(files)){
         # testload <- 
            if(length(read.fasta(files$datapath[i], seqtype="AA")) < 1){
              passed <- FALSE
            }
          incProgress(1/nrow(files), message = paste("checking", files$name[i]) )
        }
        })
        
        if(passed){
          
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
      #    out <- paste(err)
          #    system2("echo", paste('";;faa files added" >> ', log.path, sep = ""))
          status$faa <- TRUE
       #   status$files <- list.files(path =  faa.path,
        #                             full.names = FALSE,
         #                            recursive = FALSE)
     #     status$ffn_files <- list.files(path =  ffn.path,
       #                                  full.names = FALSE,
      #                                   recursive = FALSE)
          
          status$num_faa <- nrow(infiles_faa)
         if(isolate(status$faa) & isolate(status$ffn)) {
            #compare faa and ffn names
            status$filespassed <- TRUE
          }
         return(NULL)
          
        } else {
          
          showModal(modalDialog(
            title = "Please check input files",
            "Seems that one of the files is not in the FASTA format or empty.",
            easyClose = TRUE,
            footer = NULL
          ))
          
        }
     
    
    } else {
      showModal(modalDialog(
        title = "Please check file ending",
        "Currenlty only files with the '.faa' ending are supported.",
        easyClose = TRUE,
        footer = NULL
      ))
      
    }
    }
    
    
  })
  
  
  output$hmm_uploaded <- renderTable({
    if (is.null(input$hmmfile))
      return(NULL)
    else{
      # process ffn
      status$hmmpassed <- TRUE
      files <- input$files_hmm
      if(all(file_ext(files$name) =="HMM")){
        
      
      hmmfile <- as.data.frame(input$hmmfile)
      hmmfile$dest <- hmm.path
      cmd <-
        paste("mv ", hmmfile$datapath, " ", hmmfile$dest, sep = "")
      err <- system(cmd,  intern = TRUE)
      out <- paste(err)
      hmmfile_success <<- TRUE
      status$num_hmm <- 1
      
      return(NULL)
      } else {
        
        showModal(modalDialog(
          title = "Please check file ending",
          "Currenlty only files with the '.HMM' ending are supported.",
          easyClose = TRUE,
          footer = NULL
        ))
        
      }
    }
  })
  
  
  output$ffn_uploaded <- renderTable({
    if (is.null(input$files_ffn))
      return(NULL)
    else {
      
      files <- input$files_ffn
      if(all(file_ext(files$name) =="ffn")){
        
        # check if we can read the fasta files
        # load all as read.fasta and raise error if they have no sequences
        passed <- TRUE
        withProgress(message = 'Checking files, please wait', value = 0,  {
          for (i in 1:nrow(files)){
            #testload <- 
              if(length(read.fasta(files$datapath[i], seqtype = "DNA")) < 1){
                passed <- FALSE
              }
            incProgress(1/nrow(files), message = paste("checking", files$name[i]) )
         }
        })
        
        if(passed){
          
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
       
        #  out <- paste(err)
          #    system2("echo", paste('";;faa files added" >> ', log.path, sep = ""))
     
            status$ffn <- TRUE
     #     status$files <- list.files(path =  ffn.path,
      #                               full.names = FALSE,
       #                              recursive = FALSE)
      #    status$faa_files <- list.files(path =  faa.path,
      #                                   full.names = FALSE,
      #                                   recursive = FALSE)
    
     #     status$num_ffn <- length(status$files)
            status$num_ffn <- nrow(infiles_ffn)
      if(isolate(status$faa) & isolate(status$ffn)) {
            #compare faa and ffn names
            status$filespassed <- TRUE
         }
 
         return(NULL)
        } else {
          
          showModal(modalDialog(
            title = "Please check input files",
            "Seems that one of the files is not in the FASTA format or empty.",
            easyClose = TRUE,
            footer = NULL
          ))
          
        }
        
        
      } else {
        showModal(modalDialog(
          title = "Please check file ending",
          "Currenlty only files with the '.faa' ending are supported.",
          easyClose = TRUE,
          footer = NULL
        ))
        
      }
    }
    
    
  })
  
  ### ui mid part
  output$upload_ui_mid <- renderUI({
    #   if (status$filespassed) {
   # conditionalPanel(
  #    condition = "input.tsp=='tab1'",
    tagList(
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
   # conditionalPanel(
  #    condition = "input.tsp=='tab4'",
    tagList(
      helpText("Specify name and thresholds"),
      textInput(
        "eden_run_name",
        labelMandatory("give your analysis a unique name"),
        value = paste0("run_", Sys.Date())
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
      
  #    conditionalPanel(
  #      condition = "input.tsp=='tab3'",
   tagList(     
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
   # conditionalPanel(
  #    condition = "input.tsp=='tab3'",
   tagList(   
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
  #  conditionalPanel(
   #  condition = "input.tsp=='overview",
    tagList(
      # htmlOutput("warning4"),
      actionButton('checkButton', label = "run analysis")#,
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
  #  conditionalPanel(condition = "input.tsp=='start",
    tagList(
                     actionButton('deletefiles', label = "reset files"))
  })
  
  # show upload section for samples.txt
  output$uploadsamplestxt <- renderUI({
 #   conditionalPanel(
#      condition = "input.tsp=='tab1'",
    tagList(
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
   #     conditionalPanel(
  #        condition = "input.tsp=='tab1'",
        tagList(
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
  
    status$serverstatus <- "ready"
    status$samplesnumber <- 0
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
    status$eden_error <- FALSE
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

      dir.create(fasta.path, showWarnings = FALSE)
      dir.create(faa.path, showWarnings = FALSE)
      dir.create(ffn.path, showWarnings = FALSE)
  
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
   #   print(paste(basename(file_path_sans_ext(input[[temp2]])),  collapse =
   #                 "+"))
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
 #  system2("echo",
  #          paste('";;grouping file updated" >> ', log.path, sep = ""))
    
  })
  
  # ==============================================================================================
  # function that updates message based on files prsovided
  # ==============================================================================================
  
  get_running_status <- function() {
    if (file.exists(lock.file)) {
      status$serverstatus <- "running"
      return(NULL)
    } else {
      return(NULL)
    }
  }
  
  
  # ==============================================================================================
  # execution of shell scripts and update of statusmsg print
  # ==============================================================================================
  
  # use observe event to check if button pressed
  observeEvent(input$checkButton, {
    status$serverstatus <- "running"
    #  file.create(lock.file)
    shinyjs::disable("checkButton")
    shinyjs::disable("deletefiles")
    
    
    showModal(modalDialog(
      title = "Job submitted",
      "You can now go to the log tab to obtain status information of the job",
      easyClose = TRUE,
      footer = NULL
    ))
    
    system(
      paste(
        "/home/eden/start_check.sh",
        #          "./start_check.sh",
        
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
    internal_check <<- check_new_file()
  }
  
  output$log = renderText({
if(demomode){
  invalidateLater(millis = 60000, session)
  update_log()
} else {
  invalidateLater(millis = 3000, session)
  update_log()
  
  
}
    NULL
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
    status$eden_error <- TRUE
    unlink(hmm.path, recursive = T, force = T)
    unlink(sample.path, recursive = T, force = T)
    if(file.exists("/home/eden/data/groups.txt")){
      unlink("/home/eden/data/groups.txt",
             recursive = T,
             force = T)
    }
  #  unlink(log.path, recursive = T, force = T)
    dir.create(fasta.path)
    dir.create(faa.path)
    dir.create(ffn.path)
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
  
  
  
  observe({
    output$text3 <- renderText(status$serverstatus)
  })
  
  
  
  #output$text3 <- renderText({ 
  #  status$serverstatus
  #})
  
  
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
  
  # update status$dataset if input variable changes
  observe({
    input$dataset
status$dataset <- input$dataset
  })
  
  observe({
    if(status$eden_error){
      
      showModal(modalDialog(
        title = "An error occured",
        "Please go to the log tab to get informations about the error",
        easyClose = TRUE,
        footer = NULL
      ))
      
      
      
    }
    
    
  })
  
  # enable buttons if eden finished and cleanup
  observe({
    if (status$eden_finished){
#      Sys.sleep(1)
        # extract and update boxes
#          withProgress(message = 'Extract files, please wait', value = 0, {
#            extractTar(tar.path, raw.path, csv.path, progress=TRUE)
#          })
#         updateSelectInput(session,
 #                           "dataset",  label = "Select run", choices = list.files(
#                              path = csv.path,
#                              full.names = FALSE,
#                              recursive = FALSE
#                            )
#          )
#         updateSelectInput(session, "samples")
      
      # inform user
      showModal(modalDialog(
        title = "Eden run finished!",
        "You can now inspect the results on the 'Visualize job' tab.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      system2("echo", paste('"internal;;update ui\n" >> ', log.path, sep = ""))
      # reset variables
      status$eden_finished <- FALSE
   #   last_log <- "eden updated"
      status$serverstatus <- "finished"
      shinyjs::enable("checkButton")
     shinyjs::enable("deletefiles")
      status$filespassed <- FALSE
      ##   status$grouppassed <- FALSE
      status$settingspassed <- FALSE
      status$hmmpassed <- TRUE
      status$num_fasta <- 0
      status$num_hmm <- 0
      status$num_faa <- 0
      status$num_ffn <- 0
      status$num_groups <- 0
      status$eden_finished <- FALSE
  
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
      status$eden_error <- FALSE
      unlink(faa.path, recursive = T, force = T)
      unlink(ffn.path, recursive = T, force = T)
     unlink(fasta.path, recursive = T, force = T)
     unlink(hmm.path, recursive = T, force = T)
      unlink(sample.path, recursive = T, force = T)
      unlink("/home/eden/data/groups.txt",
             recursive = T,
            force = T)
   #unlink(log.path, recursive = T, force = T)
    #  system2("echo", paste('"status;;initialize\n" >> ', log.path, sep = ""))
     # system2("echo", paste('"status;;server ready\n" >> ', log.path, sep = ""))
  
        dir.create(fasta.path)
        dir.create(faa.path)
        dir.create(ffn.path)
     
      Sys.chmod(fasta.path, mode = "0777", use_umask = TRUE)
      Sys.chmod(ffn.path, mode = "0777", use_umask = TRUE)
      Sys.chmod(faa.path, mode = "0777", use_umask = TRUE)
      
      status <-
        reactiveValues() # save the run status as a reactive object
      input <-
        reactiveValues() # save the run status as a reactive object
    }
  })
  

  # enable buttons if eden finished and cleanup
  observe({
    if (status$eden_error){
      showModal(modalDialog(
        title = "An error occured!",
        paste0("Error: ", last_error, " Please repeat the analysis or contact pmu15@helmholtz-hzi.de"),
        easyClose = TRUE,
        footer = NULL
      ))
      
      shinyjs::enable("checkButton")
      shinyjs::enable("deletefiles")
      status$filespassed <- FALSE
      ##   status$grouppassed <- FALSE
      status$settingspassed <- FALSE
      status$hmmpassed <- TRUE
      status$num_fasta <- 0
      status$num_hmm <- 0
      status$num_faa <- 0
      status$num_ffn <- 0
      status$num_groups <- 0
      status$eden_finished <- FALSE
      
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
      status$eden_error <- FALSE
      unlink(faa.path, recursive = T, force = T)
      unlink(ffn.path, recursive = T, force = T)
      unlink(fasta.path, recursive = T, force = T)
      unlink(hmm.path, recursive = T, force = T)
      unlink(sample.path, recursive = T, force = T)
      unlink("/home/eden/data/groups.txt",
             recursive = T,
             force = T)
      #unlink(log.path, recursive = T, force = T)
      #  system2("echo", paste('"status;;initialize\n" >> ', log.path, sep = ""))
      # system2("echo", paste('"status;;server ready\n" >> ', log.path, sep = ""))
      
      dir.create(fasta.path)
      dir.create(faa.path)
      dir.create(ffn.path)
      
      Sys.chmod(fasta.path, mode = "0777", use_umask = TRUE)
      Sys.chmod(ffn.path, mode = "0777", use_umask = TRUE)
      Sys.chmod(faa.path, mode = "0777", use_umask = TRUE)
      
      status <-
        reactiveValues() # save the run status as a reactive object
      input <-
        reactiveValues() # save the run status as a reactive object
    }
  })
  
  
  ############ start eden-visualizer ###########

  dataset <- reactive({
    if (status$dataset[1] != "") {
      readCsv(paste(csv.path, status$dataset, sep = "/"))
    }
  })
  
  output$table_filtered <- DT::renderDataTable(DT::datatable(dataset, options = list(paging = 25)))
  output$table_annotaion <- DT::renderDataTable(DT::datatable({
    require(pander)
    data <-   readCsv(paste(csv.path, status$dataset, sep = "/"))
    if (length(input$samples) > 1) {
      subset <- NULL
      data_pool <- NULL
      for (i in 1:length(input$samples)) {
        subset <- data[which(data$sample == input$samples[i]), ]
        data_pool <- rbind(subset, data_pool)
      }
      data <- data_pool
    } else {
      data <- data[which(data$sample == input$samples), ]
    }
    
    df <- NULL
    df <-
      data.frame(
        term = unique(data$term),
        pval = rep(-1, length(unique(data$term))),
        elements = rep(0, length(unique(data$term)))
      )
    df <- df[which(!is.na(df$term)), ]
    i <- 1
    for (term in df$term) {
      data.term <- data[which(data$term == term), ]
      data.nonterm <- data[which(data$term != term), ]
      test.mat <-
        matrix(c(
          round(sum(data.term$sum_pN)),
          round(sum(data.term$sum_pS)),
          round(sum(data.nonterm$sum_pN)),
          round(sum(data.nonterm$sum_pS))
        ),
        nrow = 2,
        dimnames =
          list(c("background", "selected"),
               c("dN", "dS")))
      df[i, ]$pval <-
        fisher.test(test.mat, alternative = "greater",  workspace=1e9)$p.value
      df[i, ]$elements <- df[i, ]$elements  + nrow(data.term)
      i <- i + 1
    }
    df$fdr <- p.adjust(df$pval, method = "fdr")
    df$fdr <- round(df$fdr, digits = 6)
    df$star <- add.significance.stars(df$fdr)
    df$pval <- NULL
    df
  }))
  
  output$table_sample <- DT::renderDataTable(DT::datatable({
    require(pander)
    data <-   readCsv(paste(csv.path, status$dataset, sep = "/"))
    data <- data[which(data$fdr <= input$pval), ]
    if (length(input$samples) > 1) {
      subset <- NULL
      data_pool <- NULL
      for (i in 1:length(input$samples)) {
        subset <- data[which(data$sample == input$samples[i]), ]
        data_pool <- rbind(subset, data_pool)
      }
      data <- data_pool
    } else {
      data <- data[which(data$sample == input$samples), ]
    }
    
    df <- NULL
    df <-
      data.frame(sample = unique(data$sample),
                 pval = rep(-1, length(unique(data$sample))))
    df <- df[which(!is.na(df$sample)), ]
    i <- 1
    for (sample in df$sample) {
      data.sample <- data[which(data$sample == sample), ]
      data.nonsample <- data[which(data$sample != sample), ]
      test.mat <-
        matrix(c(
          round(sum(data.sample$sum_pN)),
          round(sum(data.sample$sum_pS)),
          round(sum(data.nonsample$sum_pN)),
          round(sum(data.nonsample$sum_pS))
        ),
        nrow = 2,
        dimnames =
          list(c("dN", "dS"),
               c("selected", "background")))
      df[i, ]$pval <-
        fisher.test(test.mat, alternative = "greater", workspace=1e9)$p.value
      i <- i + 1
    }
    
    df$fdr <- p.adjust(df$pval, method = "fdr")
    df$pval <- NULL
    df$fdr <- round(df$fdr, digits = 6)
    df$star <- add.significance.stars(df$fdr)
    df
  }))
  
  output$table <- DT::renderDataTable(
    if(input$dataset != ""){
      DT::datatable(dataset, options = list(pageLength = 25))
    } 
    )
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    
    if(input$dataset != ""){
      
      require(pander)
      data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
      
      if (input$dofiltering == "pvalue") {
        
        data <- data[which(data$fdr <= input$pval), ] # sort based on pvalue
      } else {
        data <- data[which(data$ratio >= input$ratio), ] # sort based on ratio
        
      }
      
      if (length(input$samples) > 1) {
        subset <- NULL
        data_pool <- NULL
        for (i in 1:length(input$samples)) {
          subset <- data[which(data$sample == input$samples[i]), ]
          data_pool <- rbind(subset, data_pool)
        }
        data <- data_pool
      } else {
        data <- data[which(data$sample == input$samples), ]
      }
      data$stars <- add.significance.stars(data$fdr)
      data$sum_pN <- NULL
      data$sum_pS <- NULL
      data$role <- NULL
      data$pvalue <- NULL
      data$ratio <- round(data$ratio, digits = 3)
      data$fdr <- round(data$fdr, digits = 5)
      num.name <<- nrow(data)
      num.meanratio <<- round(mean(data$ratio, na.rm = T), digits = 2)
      num.sd <<- round(sd(data$ratio, na.rm = T), digits = 3)
      downloadObj <<- data # generate downloadable table
      input$resetSelection
      data
      
    }
  }))
  
  #################
  # Render UI
  #################
  
  
  
  # render again if input$datase changes
  output$filters_UI <- renderUI({
if(input$dataset != ""){
  # if (!no_csv){
  dataset <- readCsv(paste(csv.path, status$dataset, sep = "/"))
  
  
  selectInput(
    "samples",
    "Choose one or more samples:",
    choices = levels(factor(dataset$sample)),
    selected = c(levels(factor(
      dataset()$sample
    ))[1]),
    multiple = T,
    width = "100%"
  ) 

}
    #}
  })
  
  # render again if input$dofiltering changes
  output$dependentselection <- renderUI({
    if (input$dofiltering == "pvalue") {
      sliderInput(
        "pval",
        label = "adjusted P value threshold",
        min = .001,
        max = 1,
        value = 1
      )
    } else {
      sliderInput(
        "ratio",
        label = "select minimal ratio to display",
        min = 0,
        max = 1,
        value =1)
  
    }
  })
  
  ### start main ui
  output$main_ui <- renderUI({
    conditionalPanel(
      condition = "input.tabset=='overview' ||
      input.tabset=='annotation' ||
      input.tabset=='alignment' ||
      input.tabset=='histogram' ||
      input.tabset=='box' ||
      input.tabset=='start' ||
input.tabset=='overview' ||
      input.tabset=='categories' ",
  #    helpText("Select which analysis run you want to show"),
      selectInput(
        "dataset",
        "Select run:",
        choices = list.dirs(
          path = csv.path,
          full.names = FALSE,
          recursive = FALSE
        ),
        selected = list.dirs(
          path = csv.path,
          full.names = FALSE,
          recursive = FALSE
        )[1],
        multiple = F,
        width = "100%"
      ),
      
      uiOutput('filters_UI'),
      
      selectInput(
        'dofiltering',
        label = 'Filter results',
        choices = c("by P value" = "pvalue", "by dN/dS ratio" = "ratio"),
        selected = "no filtering"
      ),
  uiOutput("dependentselection")
    )
  })
  
  ### end main ui
  
  
  # render again if input$player_name changes
  output$start_UI_samples <- renderUI({
    if (input$analysistype == "comparative") {
      conditionalPanel(
        condition = "input.tabset=='start' || input.tabset=='log'",
        helpText(
          "For a comparative analysis please provide information which samples should be pooled together"
        ),
        fileInput(
          'file_sample',
          'upload a sample description file',
          accept = c('.txt'),
          multiple = FALSE
        )#,
        #   checkboxInput("eden_use_mgm", "find ORFs with MetaGeneMark", FALSE)
      )
    }
  })
  
  
  # render again if input$dofiltering changes
  output$colorpoints <- renderUI({
    if (input$points) {
      checkboxInput('gap', 'color by gap proportion', value = TRUE)
    }
  })
  
  
  #################
  # ggplot functions
  #################
  
  # generates a histogram
  doPlotHistogram <- function(dataset, input) {
    p <- ggplot(dataset, aes(ratio, fill = sample)) +
      geom_histogram(bins = input$binSize) + theme_classic()
    p <-
      p + labs(x = "dN/dS ratio", y = "protein families") + ggtitle("Histogram")
    if (input$facet)
      p <- p + facet_grid(sample ~ .)
    
    if (length(input$table_rows_selected)) {
      # get the ratio of selected rows
      mark.ratio <- dataset[input$table_rows_selected, ]$ratio
      mark.name <- dataset[input$table_rows_selected, ]$name
      p <- p + geom_vline(xintercept = mark.ratio)
    }
    return(p)
  }
  
  # this functions calls the create_msa_plot() function multiple times based
  # on selected protein families
  doAlignmentPlot <- function(data, input) {
    require(ggplot2)
    require(grid)
    require(gridExtra)
    fam_ids <- data$name
    dnds <-
      paste(
        raw.path,
        "/",
        status$dataset[1],
        "/",
        input$samples,
        "/dnds/",
        fam_ids,
        ".txt.DnDsRatio.txt",
        sep = ""
      )
    gap <-
      paste(
        raw.path,
        "/",
        status$dataset[1],
        "/",
        input$samples,
        "/gap/",
        fam_ids,
        ".gap.txt",
        sep = ""
      )
    if (input$points) {
      if (input$gap) {
        # get list of ggplot obj with gap color
        p <- list()
        for (i in 1:length(dnds)) {
          p[[i]] <- create_msa_plot(
            dnds_path = dnds[i],
            gap_path = gap[i],
            gapcolor = T
          )
        }
      } else {
        # get list of ggplot obj without gap color
        p <- list()
        for (i in 1:length(dnds)) {
          p[[i]] <- create_msa_plot(
            dnds_path = dnds[i],
            gap_path = gap[i],
            gapcolor = F
          )
        }
      }
    } else {
      p <- list()
      for (i in 1:length(dnds)) {
        p[[i]] <- create_msa_plot(
          dnds_path = dnds[i],
          gap_path = gap[i],
          gapcolor = F,
          points = F
        )
      }
    }
    do.call(grid.arrange, p)
    return(p)
  }
  
  # show TIGRFAM annotation for selected samples
  doPlotAnnotationGlobal <- function(data, input) {
    if (substring(data$name, 1, 4)[1] == "TIGR") {
      require(gridExtra)
      num <- as.data.frame(table(data$term))
      num <- num[which(num$Freq > 0), ]
      p <-
        ggplot(num, aes(reorder(Var1, Freq), Freq)) + coord_flip()
      p <-
        p + geom_bar(stat = "identity",
                     fill = "grey80",
                     width = 0.8) + theme_classic()
      #p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      p <-
        p + labs(x = "", y = "number of protein families annotated")
      if (input$pval < 1) {
        p <-
          p + ggtitle(
            paste(
              "Number of protein families (p-value less than: ",
              input$pval,
              ")",
              sep = ""
            )
          )
      } else {
        p <- p + ggtitle("Number of protein families in dataset")
      }
      return(p)
    }
  }
  
  doPlotSample <- function(data, input) {
    require(ggplot2)
    num <- as.data.frame(table(data$sample))
    num$selected <- FALSE
    for (i in 1:length(input$samples)) {
      num[which(num$Var1 == input$samples[i]), ]$selected <- TRUE
    }
    p <- ggplot(num, aes(Var1, Freq, fill = selected))
    p <-
      p + geom_bar(stat = "identity", width = 0.8) + theme_classic()
    p <-
      p + scale_fill_manual(breaks = c(TRUE, FALSE),
                            values = c("grey80", "black")) + guides(fill = FALSE)
    p <-
      p + labs(x = "Samples", y = "# protein families") + ggtitle("Selected samples")
    p <-
      p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(p)
  }
  
  doAnnotationplot <- function(data, input) {
    require(ggplot2)
    if (input$navalues) {
      # remove NA values
      data <- data[which(!is.na(data$term)), ]
    }
    if (input$sortannotation == "ratio") {
      if (input$bysamplecolor) {
        p <- ggplot(data, aes(
          x = reorder(term, ratio),
          y = ratio,
          fill = sample
        ))
        p <-
          p + geom_boxplot(width = 0.3) + theme_classic() + coord_flip()
      } else {
        p <- ggplot(data, aes(x = reorder(term, ratio), y = ratio))
        p <-
          p + geom_boxplot(width = 0.3, fill = "grey80") + theme_classic() + coord_flip()
      }
    } else {
      p <- ggplot(data, aes(x = reorder(term,-fdr), y = ratio))
    }
    p <- p + ylab("dN/dS ratio") + xlab("functional group")
    if (input$showmean) {
      p <- p + geom_hline(yintercept = mean(data$ratio, na.rm = T))
    }
    if (input$showmeanselected) {
      p <-
        p + geom_hline(yintercept = mean(data[input$table_rows_selected, ]$ratio, na.rm =
                                           T),
                       color = "red")
    }
    if (input$bysamplefacet) {
      p <- p + facet_wrap( ~ sample)
    }
    return(p)
  }
  
  doPlotBox <- function(data, input) {
    require(ggplot2)
    if (input$oderchoice == "mean") {
      p <- ggplot(data, aes(x = reorder(sample, ratio), y = ratio))
    }
    # if(input$oderchoice == "pvalue"){
    #    p <- ggplot(data, aes(x=reorder(sample, -fdr),y=ratio))
    #  }
    if (input$oderchoice == "default") {
      p <- ggplot(data, aes(x = sample, y = ratio))
    }
    p <- p + ylab("dN/dS ratio") + xlab("sample")
    p <-
      p + geom_boxplot(fill = "grey80", width = 0.8) + theme_classic() + coord_flip()
    if (input$highlightbox) {
      mark.ratio <- data[input$table_rows_selected, ]$ratio
      p <- p + geom_hline(yintercept = mean(mark.ratio, na.rm = T))
    }
    return(p)
  }
  
  
  output$plot1 <- renderPlot({
    data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
    data <- data[which(data$fdr <= input$pval), ]
    data <- data[which(data$sample == input$samples), ]
    p <- doPlotHistogram(data, input)
    histogram_obj <<- p
    print(p)
  }, height = 700)
  
  # Density plot (unused)
  output$plot2 <- renderPlot({
    p <- ggplot(dataset(), aes(ratio, fill = sample)) +
      geom_density(adjust = input$densityBw)
    print(p)
  }, height = 700)
  
  
  # boxplot
  output$sampleplot <- renderPlot({
    data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
    p <- doPlotSample(data, input)
    downloadableSamplePlot <<- p
    print(p)
  }, height = 200)
  
  output$alignmentplot <- renderPlot({
    
    if (input$dataset != ""){
      data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
      data <- data[which(data$fdr <= input$pval), ]
      data <- data[which(data$sample == input$samples), ]
      data <- data[input$table_rows_selected, ]
      if (length(input$table_rows_selected) > 0) {
        # get the dnds an gap paths
        p <- doAlignmentPlot(data, input)
        downloadableAlignmentPlot <<- p
      } else {
        df <- data.frame()
        p <- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
        downloadableAlignmentPlot <<- p
      }
      # else write an error msg that the user have to select some rowss
      
      
    }
  }, height = 700)
  ####
  #### BOXPLOT TAB
  ####
  
  # boxplot
  output$plot4 <- renderPlot({
    data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
    data <- data[which(data$fdr <= input$pval), ]
    if (length(input$samples) > 1) {
      subset <- NULL
      data_pool <- NULL
      for (i in 1:length(input$samples)) {
        subset <- data[which(data$sample == input$samples[i]), ]
        data_pool <- rbind(subset, data_pool)
      }
      data <- data_pool
    } else {
      data <- data[which(data$sample == input$samples), ]
    }
    
    p <- doPlotBox(data, input)
    downloadableBoxplot <<- p
    print(p)
  }, height = 300)
  
  # boxplot
  output$annotationplot <- renderPlot({
    data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
    data <- data[which(data$fdr <= input$pval), ]
    #  data <- data[which(data$sample == input$samples),]
    
    if (length(input$samples) > 1) {
      subset <- NULL
      data_pool <- NULL
      for (i in 1:length(input$samples)) {
        subset <- data[which(data$sample == input$samples[i]), ]
        data_pool <- rbind(subset, data_pool)
      }
      data <- data_pool
    } else {
      data <- data[which(data$sample == input$samples), ]
    }
    p <- doAnnotationplot(data, input)
    catplot_obj <<- p
    print(p)
  }, height = 400)
  
  # annotation plot
  output$annotationplotglobal <- renderPlot({
    if(input$dataset != ""){
      
      data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
      data <- data[which(data$fdr <= input$pval), ]
      if (length(input$samples) > 1) {
        subset <- NULL
        data_pool <- NULL
        for (i in 1:length(input$samples)) {
          subset <- data[which(data$sample == input$samples[i]), ]
          data_pool <- rbind(subset, data_pool)
        }
        data <- data_pool
      } else {
        data <- data[which(data$sample == input$samples), ]
      }
      p <- doPlotAnnotationGlobal(data, input)
      downloadableAnnotaionplot <<- p
      print(p)
    } 
    

  }, height = 400)
  
  
  # print the selected indices
  output$selected = renderPrint({
    s = input$table_rows_selected
    if (length(s)) {
      cat('These rows are selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  
  output$summary2 <-
    renderText({
      if(input$dataset != ""){
        
              
      data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
      data <- data[which(data$fdr <= input$pval), ]
      data <- data[which(data$sample == input$samples), ]
      
      s = input$table_rows_selected
      if (length(s)) {
        data.selection <<- data[input$table_rows_selected,]
    
       fams <- print(data.selection$name)
       for(fam in fams){
         input$sample 
        
      # move selected families to tmp folder that they can downloaded on button press
      dnds_path <- paste0(raw.path, "/",input$dataset,"/", input$samples[1] ,"/dnds/",fams,".txt.DnDsRatio.txt")
      tree_path <- paste0(raw.path, "/",input$dataset,"/", input$samples[1] ,"/tree/",fams,".ffn.txt")
      codon_path <- paste0(raw.path, "/",input$dataset,"/", input$samples[1] ,"/codon/",fams,".msa")
      print("copy files")
      do.call(file.remove, list(list.files(down.path, full.names = TRUE)))
      file.copy(dnds_path, paste0(down.path, "/", input$dataset,"-", input$samples[1],"-",fams, ".dnds.txt"))
      file.copy(tree_path, paste0(down.path, "/", input$dataset,"-", input$samples[1],"-",fams, ".tree.phy"))
      file.copy(codon_path, paste0(down.path, "/", input$dataset,"-", input$samples[1],"-",fams, ".codon.msa"))
      
       }
   
        
        dnds_files <- data[input$table_rows_selected,]
        test.mat <-
          matrix(c(
            round(sum(data.selection$sum_pN)),
            round(sum(data.selection$sum_pS)),
            round(sum(data$sum_pN)),
            round(sum(data$sum_pS))
          ),
          nrow = 2,
          dimnames =
            list(c("dN", "dS"),
                 c("selected", "background")))
        
        pval <- fisher.test(test.mat, alternative = "greater", workspace = 1e9)$p.value
        ## selected
        paste(
          "</br><div class='panel panel-default'>
          <div class='panel-heading'>Fisher's test</div>
          <div class='panel-body'>",
          "mean ratio of selected datasets: <span class='badge'>",
          
          round(mean(data$ratio, na.rm=TRUE), digits = 3),
          "+-",
          round(sd(data$ratio, na.rm=TRUE), digits = 3) ,
          "(SD) </span></br> compared to  mean ratio of selected families: <span class='badge'>",
          round(mean(data.selection$ratio, na.rm=TRUE), digits = 3),
          "+-",
          round(sd(data.selection$ratio, na.rm=TRUE), digits = 3) ,
          "(SD) </span></br> p-value (one-sided):  <span class='badge'>", round(pval, digits = 5),"</span>"
  
        
                )
      } else {
        
        paste(
          "</br><div class='panel panel-default'>
          <div class='panel-heading'>Fisher's test</div>
          <div class='panel-body'>",
          "please select rows from the table above to perform a fisher test")
      }
      
      }
      
      
    })
  
  # print summary and selected rows
  output$alignment <- renderPrint({
    data <-  readCsv(paste(csv.path, status$dataset, sep = "/"))
    data <- data[which(data$fdr <= input$pval), ]
    data <- data[which(data$sample == input$samples), ]
    
    if (length(input$samples) > 1) {
      cat(
        paste(
          "Error: More than one dataset selected! A plot can only be created for one dataset. Please go back to the Data Table tab and deselect the dataset\n"
        )
      )
    }
    s = input$table_rows_selected
    if (length(s)) {
      cat("\n")
      cat(paste(length(s), ' protein families were selected:\n'))
    } else {
      cat(
        "Error: No gene families selected! Please go to the Data Table tab and select one or more rows in the data table."
      )
    }
    
    cat(length(input$samples))
  })
  
  
  observeEvent(input$reloadButton, {
    withProgress(message = 'Extract files, please wait', value = 0, {
      extractTar(tar.path, raw.path, csv.path, progress=TRUE)
    })
    status$serverstatus <- "ready"
    status$samplesnumber <- 0
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
    status$eden_error <- FALSE
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
      
      dir.create(fasta.path, showWarnings = FALSE)
      dir.create(faa.path, showWarnings = FALSE)
      dir.create(ffn.path, showWarnings = FALSE)
      
      Sys.chmod(fasta.path, mode = "0777", use_umask = TRUE)
      Sys.chmod(ffn.path, mode = "0777", use_umask = TRUE)
      Sys.chmod(faa.path, mode = "0777", use_umask = TRUE)
      
      status <-
        reactiveValues() # save the run status as a reactive object
      input <-
        reactiveValues() # save the run status as a reactive object
      
    })
    
    updateSelectInput(session,
                      "dataset",  label = "Select run", choices = list.files(
                        path = csv.path,
                        full.names = FALSE,
                        recursive = FALSE
                      )
    )
    updateSelectInput(session, "samples")

  })
  
  

  
  #################
  # RENDER html
  #################
  
  output$welcome <-
    renderText({
      
      paste("</br><font color=\"#4db898\"><b>Welcome to eden!</b></br></font>",
            "Eden is a fast implementation of the widely used method for the detection of protein families that are under positive selection based on the ratio of amino acid replacement versus silent substitution rates (dN/dS) that can applied an large metagenomic samples.",
            "</br></br>", "<font color=\"#4db898\"><b>About the examples</b></br></font>", 
            "On the left panel you can visualize pre-computed dataset under the 'Visualize job' panel. In the 'bodysites.tar' example we used >60 metagenomic samples from healthy individuals from the Human Microbiome Project. In a second example named 'bmi.tar' we analyzed over 50 metagenomic samples from the gut of lean, overwight and obese individuals.</br></br>"
            
      )
      
    })
  
  output$lockfileBox <- renderInfoBox({
    if(file.exists(lock.file)){
      infoBox(
        "Server busy","we detected that a eden process in running in the background.", icon = icon("list"),
        color = "red"
      )    
    } else {
      infoBox(
        "Server ready","You can submit a new job.", icon = icon("list"),
        color = "green"
      )
    }
  
  })
  
#  observe({
#    observeEvent(status$eden_error ,{
#      if(length(list.files(tar.path)) == 0) return(NULL)
#      hide(id = "figurebox", anim = TRUE)
#    })
    
#  })
  
  

  observe({
    if (status$newfiles){
      #showModal(modalDialog(
      #  title = "Preparing the viewer for new samples!","loading...",
      #  loadtar(),
      
        # automatically imported here
    #    actionButton('reloadButton2', label = "import files"), 
    #   easyClose = TRUE,
    #    footer = NULL
    #  ))
      Sys.sleep(1) # make sure the tar file is loading complete by waiting
      withProgress(message = 'Extract files, please wait', value = 0, {
        extractTar(tar.path, raw.path, csv.path, progress=TRUE)
      })
      
      
      updateSelectInput(session,
                        "dataset",  label = "Select run", choices = list.files(
                          path = csv.path,
                          full.names = FALSE,
                          recursive = FALSE
                        )
      )
      
      updateSelectInput(session, "samples")
      
    }
    })
 
  output$reloadstatus <-
    renderText({
      if (status$newfiles){
        paste("<font color=\"#4db898\"><b>New samples detected! You have to import these samples to make them visible",
              "</b></font>")
        
      } else 
        " "
    })
  
  # print if new files found 
  output$newtar <-
    renderText({
      paste("<font color=\"#4db898\"><b>New tar::",status$newfiles,
            "</b></font>")
    })
  
  
  
  # print if csv file is found or not
  output$selected_dataset <-
    renderText({
      paste("<font color=\"#4db898\"><b>Dataset selected:",status$dataset,
            "</b></font>")
    })
  
  output$selected_samples <-
    renderText({
      paste("<font color=\"#4db898\"><b>Samples selected: ",input$samples,
            "</b></font>")
    })
  
  
  # print if csv file is found or not
  output$csv_check <-
    renderText({
      if(no_csv){
        paste("<font color=\"#4db898\"><b>",
              "No csv files found!</b></font>")
      } else {
        paste("<font color=\"#4db898\"><b>",
              "Csv files found!</b></font>")
      }
    })
  
  # print if .tar files are found or not
  output$tar_check <-
    renderText({
      if(no_tar){
        paste("<font color=\"#4db898\"><b>",
              "No tar files found!</b></font>")
      } else {
        paste("<font color=\"#4db898\"><b>",
              "Tar files found!</b></font>")
      }
    })
  
  output$start_hint_online <-
    renderText({
      paste("</br><font color=\"#4db898\"><b>",
            "Welcome text here</b></font></br></br>")
    })
  
  # tab 1
  # overview  tab
  output$overview_hint <-
    renderText({
      paste(
        
        "<div class='alert alert-dismissible alert-warning'>
        <button type='button' class='close' data-dismiss='alert'>&times;</button>
        <h4>Hint!</h4>
        <p>You can include or remove samples from your analysis. For this use the <strong>Choose one or more samples</strong> form on the widget on the left side.</p>
        </div>"
        
        
      )
    })
  
  output$overview_hint2 <-
    renderText({
      paste(
        
        "<div class='alert alert-dismissible alert-warning'>
        <button type='button' class='close' data-dismiss='alert'>&times;</button>
        <p>You can perform a one-sided Fisher's exact test by selecting protein families by clicking on the table</p>
        </div>"
        
      )
    })
  
  output$overview_table <-
   
          
    renderText({
      if (input$dataset != ""){
      paste(
        "</br><div class='panel panel-default'>
        <div class='panel-heading'>Table description</div>
        <div class='panel-body'>",
        "<span class='badge'>",
        num.name,
        "</span> protein families found in <span class='badge'>",
        length(input$samples),
        "</span> samples with a mean dN/dS ratio of <span class='badge'>",
        num.meanratio,
        " +- ",
        num.sd,
        "(SD) </span>. Categories based on HMM match with E-value 0.01. Only protein families with a FDR adjusted p-value of less than ",
        input$pval,
        " are shown. p-value(s) as: one star for value below 0.05, two for 0.01 and three for 0.001. Table generated with eden <span class='label label-default'>v. 0.1.0</span>",
        "</div></div>"
      )
      } 
 
    })
  
  
  # tab 2
  # annotation tab
  output$annotation_hint <-
    renderText({
      paste(
        
        "<div class='alert alert-dismissible alert-warning'>
        <h4>Just want to show gene families that are significant?</h4>
        <button type='button' class='close' data-dismiss='alert'>&times;</button>
        <p>You can specify a filter to show protein families that have a significant or high dn/ds ratio. For this use the slider <strong>p-value threshold</strong> on the widget on the left side.</p>
        </div>"
        
      )
    })
  
  output$annotation_figure <-
    renderText({
      
      if (input$dataset != ""){
        paste(
          
          "</br><div class='panel panel-default'>
        <div class='panel-heading'>Figure description</div>
        <div class='panel-body'>",
          
          "Number of protein families found in <span class='badge'>",
          length(input$samples),
          "</span> samples for each category. Only protein families with a FDR adjusted p-value of less than <span class='badge'>",
          input$pval,
          "</span> are shown. Figure generated with eden <span class='label label-default'>v. 0.1.0</span></div>"
        )
        
      }
     
    })
  
  # tab 3
  # overview  tab
  output$alignment_hint <-
    renderText({
      if (length(input$table_rows_selected) < 1) {
        paste( "<div class='alert alert-dismissible alert-danger'>
               <h4>You need to select protein families first</h4>
               <button type='button' class='close' data-dismiss='alert'>&times;</button>
               <p>Just go back to the <strong>Overview</strong> tab and select rows you want to show here</p>
               </div>")
      }
      
      })
  # overview  tab
  output$alignment_figure <-
    renderText({
      if(length(input$table_rows_selected)>0){
        paste(
          "</br><div class='panel panel-default'>
          <div class='panel-heading'>Figure description</div>
          <div class='panel-body'>",
          
          "Sequence clusters of residues under positive selection in selected protein families. Dots indicate dN/dS ratio for a given position in the protein sequence, and their color corresponds to the proportion of gaps in the multiple sequence alignment (MSA). Gray-shaded areas indicate significant clusters of residues under positive selection.</div>"
        )
        
        
      }
    })
  
  
  # boxplot tab
  output$boxplot_hint <-
    renderText({
      paste(
        "<div class='alert alert-dismissible alert-warning'>
        <button type='button' class='close' data-dismiss='alert'>&times;</button>
        <h4>Hint!</h4>
        <p>You can include or remove samples from your analysis. For this use the <strong>Choose one or more samples</strong> form on the widget on the left side.</p>
        </div>"
      )
    })
  
  
  #################
  # download handlers
  #################
  
  # download main table on the first tab
  output$dlTable <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
      write.csv(downloadObj, file)
    }
  )
  
  # download raw files
  output$dlRaw <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
   content <- function(file) {
     files2zip <- dir(down.path, full.names = TRUE)
     zip(zipfile = 'out', files = files2zip)
    file.copy("out.zip", file)
    },
    contentType = "application/zip"
  )
  
  
  
  # download histogram
  output$dlCurPlot <- downloadHandler(
    filename = 'histogram.pdf',
    content = function(file) {
      pdf(file = file,
          width = 11,
          height = 8.5)
      
      print(histogram_obj)
      dev.off()
    }
  )
  
  # download sequenceplot
  output$dlCurSequenceplot <- downloadHandler(
    filename = 'sequenceplot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 11,
          height = 8.5)
      print(downloadableAlignmentPlot)
      dev.off()
    }
  )
  
  # download boxplot
  output$dlCurBoxPlot <- downloadHandler(
    filename = 'boxplot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 11,
          height = 8.5)
      print(downloadableBoxplot)
      dev.off()
    }
  )
  
  
  # download categorie plot
  output$dlCurAnnotationplot <- downloadHandler(
    filename = 'boxplot_categories.pdf',
    content = function(file) {
      pdf(file = file,
          width = 11,
          height = 8.5)
      print(catplot_obj)
      dev.off()
    }
  )
  
  # download sequenceplot
  output$dlAnnotationPlot <- downloadHandler(
    filename = 'annotationplot.pdf',
    content = function(file) {
      pdf(file = file,
          width = 11,
          height = 8.5)
      print(downloadableAnnotaionplot)
      dev.off()
    }
  )
  
  ############ end eden-visualizer #############
  
  })